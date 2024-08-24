# °º¤ø,¸¸,ø¤º°`°º¤ø,¸,ø¤°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤°º¤ø,¸¸,ø¤:>
#
#              ASPECTS OF HIGHER CONSCIOUSNESS
#
# °º¤ø,¸¸,ø¤º°`°º¤ø,¸,ø¤°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤°º¤ø,¸¸,ø¤:>

# Load libraries and utility scripts ----

library(boot)
library(caret)
library(dplyr)
library(semTools)
library(ggplot2)
library(lavaan)
library(lavaanPlot)
library(likert) # https://github.com/jbryer/likert
library(ltm)
library(MVN)
library(plyr)
library(psych)
library(regsem)

source("code/ses-utility.R")
# ETL (respondent data and variable mappings) ----
ses.data <- ses.loaddatafile()
ses.vars <- ses.loadvarfile()

# Create Split Data Sets ----

set.seed(1234567) # For reproducibility

percent <- .5
efa_idx <- sort(sample(x = nrow(ses.data), size = floor(percent*nrow(ses.data)), replace = F))
cfa_idx <- sort(setdiff(1:nrow(ses.data), efa_idx))
sum(match(efa_idx, cfa_idx, nomatch = 0) > 0) # If positive, split was not clean
length(efa_idx) + length(cfa_idx) # Check total length
# set.seed(NULL) # In case we need true randomization later


# # # # # # # # # #
# Data Summary ----
# # # # # # # # # #

# Total number of participants
print(paste("Number of records is " , nrow(ses.data)))

# Number of participants by sex
table(ses.data$sex)
round(prop.table(table(ses.data$sex)), 4) * 100

# Number of participants by sex (graphically)
ggplot(data=ses.data, aes(x = sex, fill = sex)) +
  guides(fill = FALSE) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..)) +
  labs(x="Sex of Participant", y="Number of participants", fill="Sex")

# Age descriptive stats
describe(ses.data$age)

# Age histogram
# Credit: https://www.datacamp.com/community/tutorials/make-histogram-ggplot2#gs.ko0NeIE
ggplot(data=ses.data[ses.data$age > 10, ]
, aes(x = age, width = .4)) +
  geom_histogram(binwidth = 5, col = "white", aes(fill =..count..), alpha = .8) +
  labs(x="Age of Participant", y="Count") +
  scale_x_continuous(breaks = seq(10, 100, by = 5))


# Country data
ses.data[,'country']
clipr::write_clip(ses.data[,c('id','country')])


## Likert Visualizations ----

likert_viz <- function (data, regex = "", max_length = 30) {
  results <- ses.get.questiontext(
    data[,grepl(regex, names(data))], # Grab by category
    max_length
  )
  plot(likert(results), centered = FALSE)
}

likert_viz(ses.data, "mystical\\d+")
likert_viz(ses.data, "spiritual\\d+")
likert_viz(ses.data, "psyphys\\d+")
likert_viz(ses.data, "psychic\\d+")
likert_viz(ses.data, "talents\\d+")
likert_viz(ses.data, "invmov\\d+")
likert_viz(ses.data, "sensation\\d+")
likert_viz(ses.data, "negphysical\\d+")
likert_viz(ses.data, "otherphysical\\d+")
likert_viz(ses.data, "negpsych\\d+")
likert_viz(ses.data, "psybliss\\d+", max_length = 40)
likert_viz(ses.data, "psygrowth\\d+", max_length = 40)


# # # # # # # # # # # # # # # # # # # # # # # # # #
# Transcendent Experience Item Factor Analysis ----  
# # # # # # # # # # # # # # # # # # # # # # # # # #

## 0. Data load ----

grepmatch = "mystical\\d+|spiritual\\d+|psyphys\\d+|psychic\\d+"
data.num <- extract.numeric.columns.by.regex(ses.data, grepmatch)
nrow(data.num)

## Non-normality (univariate and multivariate) ----
MVN::mvn(data.num, mvnTest = "hz")
MVN::mvn(data.num, mvnTest = "mardia")

describe(data.num)
describe(describe(data.num)$skew)
describe(describe(data.num)$kurtosis)

# Dropping redundant or hard to interpret items
drop <- c("spiritual5", "spiritual4", "spiritual19", "psyphys7", "mystical24", "mystical2", "mystical11", "mystical12")
data.num <- data.num %>% dplyr::select(-one_of(drop))
length(names(data.num))

#
## Correlation matrices ----
#

grepmatch = "mystical\\d+"
corPlot(extract.numeric.columns.by.regex(ses.data[], grepmatch))

grepmatch = "spiritual\\d+"
corPlot(extract.numeric.columns.by.regex(ses.data[], grepmatch))

grepmatch = "psyphys\\d+"
corPlot(extract.numeric.columns.by.regex(ses.data[], grepmatch))


#
## Validity and Reliability Tests ----
#

# Kaiser-Meyer-Olkin measure of sampling adequacy
psych::KMO(data.num[efa_idx,])[["MSA"]]

# Bartlett’s Test of Sphericity
bartlett.test(data.num[efa_idx,]) # result is greater than the critical value for chi2

# Cronbach's Alpha
psych::alpha(data.num[efa_idx,])


#
## Select # of Factors ----
#

# Cattell's scree test
# Raymond B. Cattell (1966) The Scree Test For The Number Of Factors, Multivariate Behavioral Research, 1:2, 245-276, DOI: 10.1207/s15327906mbr0102_10
scree(data.num[efa_idx,])

# Horn’s parallel analysis
# Horn, J.L. A rationale and test for the number of factors in factor analysis. Psychometrika 30, 179–185 (1965). https://doi.org/10.1007/BF02289447
# Dinno, A. (2009). Exploring the sensitivity of Horn's parallel analysis to the distributional form of random data. Multivariate behavioral research, 44(3), 362-388.
par.fa <- fa.parallel(x = data.num[efa_idx,], fa = "both", fm = "pa", n.iter = 1000, SMC = TRUE)
par.fa <- fa.parallel(x = data.num, fa = "both", fm = "pa", cor = "poly", SMC = TRUE)
# saveRDS(par.fa, file = "outputs/priexp.fa.parallel.rds") # 1000 iter, pearson
par.fa <- readRDS(file = "outputs/priexp.fa.parallel.rds")
par.fa # Indicates 5 factors

CD(data.num[efa_idx,], max_iter = 1000) # 5 factors

# Revelle’s Very Simple Structure
# Revelle, W., & Rocklin, T. (1979). Very simple structure: An alternative procedure for estimating the optimal number of interpretable factors. Multivariate behavioral research, 14(4), 403-414.
vss(data.num[efa_idx,], cor = "poly", fm="pa") # VSS1 = 1, VSS2 = 2, MAP = 7

## Experience Item ICLUST ----
# Revelle, W. (1978). ICLUST: A cluster analytic approach to exploratory and confirmatory scale construction. Behavior Research Methods & Instrumentation, 10(5), 739-742.
pchor <- polychoric(ses.get.questiontext(data.num[efa_idx,])) # Polychoric question text
pchor <- polychoric(data.num[efa_idx,]) # Polychoric question code
iclust <- iclust(pchor$rho) # polychoric

iclust <- iclust(data.num[efa_idx,]) # Question code
iclust <- iclust(ses.get.questiontext(data.num[efa_idx,])) # Question text

iclust.diagram(iclust) # Hard to read diagram (but native to R environment)
ICLUST.graph(iclust) # For external "dot" language application, e.g., https://dreampuf.github.io/GraphvizOnline/

#
## Factor Extraction ----
#

# fa.res <- fa(r = data.num[efa_idx,],
fa.res <- fa(r = data.num,
  nfactors = 5,
  fm = "pa",
  rotate = "oblimin",
  cor = "poly",
  n.rotations = 1,
  max.iter = 1000
)
fa.res
clipr::write_clip(fa.res[["communality"]])
clipr::write_clip(fa.res[["Phi"]])
clipr::write_clip(t(fa.res[["Vaccounted"]]))

fa.diagram(fa.res)

# Factor loadings (ouput to CSV)
friendly.loadings <- ses.format.loadings(fa.res$loadings)
clipr::write_clip(friendly.loadings)
write.csv(friendly.loadings, file = paste("outputs/", "primaryexp-loadings.csv", sep = ''))
# Factor correlations (ouput to CSV)
write.csv(fa.res$Phi, file = paste("outputs/", "primaryexp-factorcorrelations.csv", sep = ''))

## Harman's Single Factor (CMV Detection) ----
out <- principal(data.num)
out

fa.res <- fa(r = data.num,
             nfactors = 1,
             fm = "ml",
             n.rotations = 1
)
fa.res
fa.res <- fa(r = data.num,
             nfactors = 1,
             fm = "uls",
             n.rotations = 1
)
fa.res


hc18_ind <- c("mystical6", "mystical22", "mystical25", "mystical15", "mystical8", "mystical13", "mystical10", "mystical5", "mystical7", "mystical4", "spiritual3", "spiritual2", "spiritual26", "psyphys5", "psyphys3", "psyphys9", "psyphys11", "psyphys1")
hsf_ind <- data.num[,hc18_ind]

hc18_ind_minus_uc <- c("mystical5", "mystical7", "mystical4", "spiritual3", "spiritual2", "spiritual26", "psyphys5", "psyphys3", "psyphys9", "psyphys11", "psyphys1")
hsf_ind <- data.num[,hc18_ind_minus_uc]

names(hsf_ind)

out <- principal(hsf_ind)
out

fa.res <- fa(r = hsf_ind,
             nfactors = 1,
             fm = "ml",
             n.rotations = 1
)
fa.res

fa.res <- fa(r = hsf_ind,
             nfactors = 1,
             fm = "uls",
             n.rotations = 1
)
fa.res



## TE Schmid-Leiman ----

library(psych)
poly <- polychoric(data.num[efa_idx,])
poly <- polychoric(data.num)
library(fungible)
sl <- SchmidLeiman(
  poly$rho,
  c(5,1), # 5 first-order factors, 1 higher-order factor
  facMethod = "fapa",
  rotate = "oblimin"
)

clipr::write_clip(sl$B)
clipr::write_clip(names(data.num))


## Omega ----
SL(fa.res)
# McDonald's Omega(h and t)
# Revelle, W., & Condon, D. M. (2019). Reliability from α to ω: A tutorial. Psychological assessment, 31(12), 1395.
# omega(m = data.num[efa_idx,], nfactors = 6, fm="pa", rotate="oblimin", poly = TRUE)
omega.res <- omega(m = fa.res$loadings, Phi = fa.res$Phi)
omega.res
clipr::write_clip(t(omega.res$omega.group))


## TE ESEM Model ----

te.target <- matrix(c(   1, 0, 0, 0, 0, 0, 0,
                         1, 0, 0, 0, 0, 0, 0,
                         1, 0, 0, 0, 0, 0, 0,
                         1, 0, 0, 0, 0, 0, 0,
                         1, 0, 0, 0, 0, 0, 0,
                         1, 0, 0, 0, 0, 0, 0,
                         1, 0, 0, 0, 0, 0, 0,
                         0, 1, 0, 0, 0, 0, 0,
                         0, 1, 0, 0, 0, 0, 0,
                         0, 1, 0, 0, 0, 0, 0,
                         0, 0, 1, 0, 0, 0, 0,
                         0, 0, 1, 0, 0, 0, 0,
                         0, 0, 1, 0, 0, 0, 0,
                         0, 0, 0, 1, 0, 0, 0,
                         0, 0, 0, 1, 0, 0, 0,
                         0, 0, 0, 1, 0, 0, 0,
                         0, 0, 0, 0, 1, 0, 0,
                         0, 0, 0, 0, 1, 0, 0,
                         0, 0, 0, 0, 0, 1, 0,                         
                         0, 0, 0, 0, 0, 1, 0,
                         0, 0, 0, 0, 0, 1, 0,
                         0, 0, 0, 0, 0, 0, 1,
                         0, 0, 0, 0, 0, 0, 1,
                         0, 0, 0, 0, 0, 0, 1,
                         0, 0, 0, 0, 0, 0, 1), nrow = 25, ncol = 7, byrow = TRUE)


mod <- '
efa("efa1")*f1 +
efa("efa1")*f2 +
efa("efa1")*f3 +
efa("efa1")*f4 +
efa("efa1")*f5 +
efa("efa1")*f6 +
efa("efa1")*f7
=~ mystical6 + mystical22 + mystical25 + mystical15 + mystical8 + mystical13 + mystical10 + mystical5 + mystical7 + mystical4 + spiritual3 + spiritual2 + spiritual26 + psyphys5 + psyphys3 + psyphys9 + psyphys11 + psyphys1 + psychic1 + psychic9 + psychic7 + mystical1 + spiritual13 + spiritual14 + spiritual16
'
# Convergence issues
cfa <- sem(mod, data = data.num, estimator = "MLR",
           rotation = "target",
           rotation.args = list(target = te.target))

mod <- '
efa("efa1")*g =~ mystical6 + mystical22 + mystical25 + mystical15 + mystical8 + mystical13 + mystical10 + mystical5 + mystical7 + mystical4 + spiritual3 + spiritual2 + spiritual26 + psyphys5 + psyphys3 + psyphys9 + psyphys11 + psyphys1 + psychic1 + psychic9 + psychic7 + mystical1 + spiritual13 + spiritual14 + spiritual16
efa("efa1")*f1 +
efa("efa1")*f2 +
efa("efa1")*f3 +
efa("efa1")*f4 +
efa("efa1")*f5 +
efa("efa1")*f6 +
efa("efa1")*f7
=~ mystical6 + mystical22 + mystical25 + mystical15 + mystical8 + mystical13 + mystical10 + mystical5 + mystical7 + mystical4 + spiritual3 + spiritual2 + spiritual26 + psyphys5 + psyphys3 + psyphys9 + psyphys11 + psyphys1 + psychic1 + psychic9 + psychic7 + mystical1 + spiritual13 + spiritual14 + spiritual16
'
cfa <- sem(mod, data = data.num, estimator = "MLR",
           rotation = "bigeomin")

## TE CFA Model ----
# te.mod <- NULL
te.mod <- hc.mod
te.mod <- paste0(te.mod, "\n", 'psychic =~ psychic1 + psychic9 + psychic7')
te.mod <- paste0(te.mod, "\n", 'nonlocal =~ mystical1 + spiritual13 + spiritual14 + spiritual16')
te.mod <- paste0(te.mod, "\n")

cfa <- cfa(te.mod, data=data.num[cfa_idx,], ordered = T, estimator = "WLSMV")
cfa <- cfa(te.mod, data=data.num, ordered = F, estimator = "MLR")
summary(cfa, fit.measures = TRUE, standardized = TRUE)
fitMeasures(cfa, c("cfi.scaled", "tli.scaled",	"rmsea.scaled",	"cfi.robust",	"tli.robust",	"rmsea.robust", "srmr"))
limit.output(summary(cfa, fit.measures = TRUE, standardized = TRUE), 50) # Fit indices only
lavaanPlot(model = cfa, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stand = TRUE)


## TE Bifactor Model ----
mod <- '
g =~ mystical6 + mystical22 + mystical25 + mystical15 + mystical8 + mystical13 + mystical10 + mystical5 + mystical7 + mystical4 + spiritual3 + spiritual2 + spiritual26 + psyphys5 + psyphys3 + psyphys9 + psyphys11 + psyphys1 + psychic1 + psychic9 + psychic7 + mystical1 + spiritual13 + spiritual14 + spiritual16
# unityconsc =~ mystical6 + mystical22 + mystical25 + mystical15 + mystical8 + mystical13 + mystical10
bliss =~ mystical5 + mystical7 + mystical4
insight =~ spiritual3 + spiritual2 + spiritual26
energy =~ psyphys5 + psyphys3 + psyphys9
light =~ psyphys11 + psyphys1
psychic =~ psychic1 + psychic9 + psychic7
nonlocal =~ mystical1 + spiritual13 + spiritual14 + spiritual16
g ~~ 0*bliss + 0*insight + 0*energy + 0*light + 0*psychic + 0*nonlocal
'

cfa <- cfa(mod, std.lv = F, data=data.num, ordered = F, estimator = "MLR")
clipr::write_clip(lavInspect(cfa, what = "std")$lambda)
BifactorIndicesCalculator::bifactorIndices(cfa)
EFAtools::OMEGA(cfa)

## TE Higher-order model ----

mod <- te.mod
mod <- paste0(te.mod, "\n", 'g =~ unityconsc + bliss + insight + energy + light + psychic + nonlocal')

cfa <- cfa(mod, data=data.num, std.lv = T, ordered = T, estimator = "WLSMV") 
# cfa <- cfa(mod, data=data.num, std.lv = T, ordered = F, estimator = "MLR") 

# For parameter estimate comparison
clipr::write_clip(parameterestimates(cfa, standardized = T))

# Table
clipr::write_clip(fitMeasures(cfa, c("cfi",	"tli", "rmsea", "cfi.scaled",	"tli.scaled",	"rmsea.scaled", "cfi.robust",	"tli.robust",	"rmsea.robust", "srmr", "chisq.scaled", "df.scaled")))

# Inline
stats <- fitMeasures(cfa, c("cfi.robust",	"tli.robust",	"rmsea.robust", "srmr"))
clipr::write_clip(paste(c("CFI = ", gsub("^0", "", as.character(round(stats[1], 3))),
                          ", TLI = ", gsub("^0", "", as.character(round(stats[2], 3))),
                          ", RMSEA = ", gsub("^0", "", as.character(round(stats[3], 3))),
                          ", SRMR = ", gsub("^0", "", as.character(round(stats[4], 3)))
), collapse = ""))

### Schmid-Leiman ----

cfa <- cfa(mod, data=data.num, ordered = T, estimator = "WLSMV") # WLSMV - Confirmatory Factor Analysis p. 354
library(EFAtools)
SL(cfa) # Must have higher order factor 'g', aka Higher Consciousness
clipr::write_clip(SL(cfa)$sl)

### McDonald's Omega of HC ----
library(EFAtools)
EFAtools::OMEGA(cfa) # Bifactor approach
library(lavaan)
library(semTools)
# devtools::install_github("simsem/semTools/semTools")
semTools::compRelSEM(cfa, higher = "g", ord.scale=T) # As of 12/7/23 requires development version of semTools, e.g., devtools::install_github("simsem/semTools/semTools")

### TE CFA Schmid-Leiman ----

Bpattern <- matrix(c( 1, 	1, 	0, 	0, 	0, 	0, 	0, 	0, 
                      1, 	1, 	0, 	0, 	0, 	0, 	0, 	0, 
                      1, 	1, 	0, 	0, 	0, 	0, 	0, 	0, 
                      1, 	1, 	0, 	0, 	0, 	0, 	0, 	0, 
                      1, 	1, 	0, 	0, 	0, 	0, 	0, 	0, 
                      1, 	1, 	0, 	0, 	0, 	0, 	0, 	0, 
                      1, 	1, 	0, 	0, 	0, 	0, 	0, 	0, 
                      1, 	0, 	1, 	0, 	0, 	0, 	0, 	0, 
                      1, 	0, 	1, 	0, 	0, 	0, 	0, 	0, 
                      1, 	0, 	1, 	0, 	0, 	0, 	0, 	0, 
                      1, 	0, 	0, 	1, 	0, 	0, 	0, 	0, 
                      1, 	0, 	0, 	1, 	0, 	0, 	0, 	0, 
                      1, 	0, 	0, 	1, 	0, 	0, 	0, 	0, 
                      1, 	0, 	0, 	0, 	1, 	0, 	0, 	0, 
                      1, 	0, 	0, 	0, 	1, 	0, 	0, 	0, 
                      1, 	0, 	0, 	0, 	1, 	0, 	0, 	0, 
                      1, 	0, 	0, 	0, 	0, 	1, 	0, 	0, 
                      1, 	0, 	0, 	0, 	0, 	1, 	0, 	0, 
                      1, 	0, 	0, 	0, 	0, 	0, 	1, 	0, 
                      1, 	0, 	0, 	0, 	0, 	0, 	1, 	0, 
                      1, 	0, 	0, 	0, 	0, 	0, 	1, 	0, 
                      1, 	0, 	0, 	0, 	0, 	0, 	0, 	1, 
                      1, 	0, 	0, 	0, 	0, 	0, 	0, 	1, 
                      1, 	0, 	0, 	0, 	0, 	0, 	0, 	1,
                      1, 	0, 	0, 	0, 	0, 	0, 	0, 	1), nrow = 25, ncol = 8, byrow = TRUE)


# Variable names
clipr::write_clip(names(as.data.frame(lavInspect(cfa, "data"))))

m <- as.matrix(lavInspect(cfa, "observed")$cov)
# m <- as.matrix(lavInspect(cfa, "cov.ov"))
# clipr::write_clip(names(data.num))
# m <- as.matrix(lavInspect(cfa, "observed")$cov)
# m[upper.tri(m)] <- t(m)[upper.tri(m)]
# m

out <- BiFAD(
  m,
  Bpattern,
  7
)
clipr::write_clip(out$BstarSL)


out <- SLi(
  m,
  NULL,
  numFactors = c(7, 1),
  facMethod = "fapa",
  rotate = "oblimin",
)
clipr::write_clip(out$loadings)

# # # # # # # # # # # # # # # #
# Higher Consciousness CFA ----
# # # # # # # # # # # # # # # #

## 0. Data load ----
grepmatch <- 'id|mystical\\d+|spiritual\\d+|psyphys\\d+|psychic\\d+'
data.num <- extract.numeric.columns.by.regex(ses.data, grepmatch = grepmatch)
nrow(data.num)
# data.num <- ses.data[,grepl('mystical\\d+|spiritual\\d+|psyphys\\d+|pe.gate|sex', names(ses.data))]
# data.num$pe.gate <- ifelse(data.num$pe.gate == 2, 0, data.num$pe.gate)

## All item model (baseline) ----
mod.all <- 'f =~ psyphys11 + psyphys1 + psyphys12 + psyphys2 + mystical13 + spiritual11 + mystical4 + psyphys8 + mystical12 + spiritual25 + spiritual12 + mystical9 + psyphys6 + mystical10 + spiritual15 + psyphys9 + mystical22 + mystical23 + psyphys3 + mystical26 + spiritual14 + spiritual13 + mystical5 + mystical24 + spiritual24 + mystical7 + spiritual18 + spiritual9 + mystical2 + mystical3 + spiritual22 + mystical25 + spiritual1 + spiritual6 + mystical6 + mystical15 + spiritual10 + spiritual20 + spiritual23 + mystical14 + mystical8 + spiritual27 + mystical11 + spiritual16 + mystical27 + spiritual2 + mystical17 + mystical18 + mystical21 + mystical1 + spiritual8 + psyphys5 + spiritual3 + spiritual21'
cfa <- cfa(mod.all, data=data.num[cfa_idx,], ordered = F, estimator = "MLR") # std.lv = fix factor variances

cfa <- cfa(mod.all, data=data.num, ordered = T, estimator = "WLSMV")
cfa <- cfa(mod.all, data=data.num, ordered = F, estimator = "MLR")
summary(cfa, fit.measures = TRUE, standardized = TRUE)

# Table
clipr::write_clip(fitMeasures(cfa, c("cfi",	"tli", "rmsea", "cfi.scaled",	"tli.scaled",	"rmsea.scaled", "cfi.robust",	"tli.robust",	"rmsea.robust", "srmr", "chisq.scaled", "df.scaled")))

# Inline
stats <- fitMeasures(cfa, c("cfi.robust",	"tli.robust",	"rmsea.robust", "srmr"))
clipr::write_clip(paste(c("CFI = ", gsub("^0", "", as.character(round(stats[1], 3))),
                          ", TLI = ", gsub("^0", "", as.character(round(stats[2], 3))),
                          ", RMSEA = ", gsub("^0", "", as.character(round(stats[3], 3))),
                          ", SRMR = ", gsub("^0", "", as.character(round(stats[4], 3)))
  ), collapse = ""))

## Higher Consciousness Model ----

# hc.mod <- '
# singlehc =~ mystical6 + mystical22 + mystical25 + mystical15 + mystical8 + mystical13 + mystical10 + mystical5 + mystical7 + mystical4 + spiritual3 + spiritual2 + spiritual26 + psyphys5 + psyphys3 + psyphys9 + psyphys11 + psyphys1
# '

hc.mod <- '
unityconsc =~ mystical6 + mystical22 + mystical25 + mystical15 + mystical8 + mystical13 + mystical10
bliss =~ mystical5 + mystical7 + mystical4
insight =~ spiritual3 + spiritual2 + spiritual26
energy =~ psyphys5 + psyphys3 + psyphys9
light =~ psyphys11 + psyphys1
'

hc.HO <- '
unityconsc =~ mystical6 + mystical22 + mystical25 + mystical15 + mystical8 + mystical13 + mystical10
bliss =~ mystical5 + mystical7 + mystical4
insight =~ spiritual3 + spiritual2 + spiritual26
energy =~ psyphys5 + psyphys3 + psyphys9
light =~ psyphys11 + psyphys1
g =~ unityconsc + bliss + insight + energy + light
'

# Generate and evaluate model
cfa <- cfa(hc.mod, data=data.num[cfa_idx,], std.lv = TRUE, ordered = F, estimator = "MLR")
cfa <- cfa(hc.mod, data=data.num[efa_idx,], ordered = F, estimator = "MLR")
cfa <- cfa(hc.mod, data=data.num, ordered = T, estimator = "WLSMV")
cfa <- cfa(hc.mod, std.lv = T, data=data.num, ordered = F, estimator = "MLR")
summary(cfa, fit.measures = TRUE, standardized = TRUE)
fitMeasures(cfa, c("aic", "bic", "cfi.robust",	"tli.robust",	"rmsea.robust", "srmr", "chisq.scaled", "df.scaled"))
limit.output(summary(cfa, fit.measures = TRUE, standardized = TRUE), 50) # Fit indices only
lavaanPlot(model = cfa, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stand = TRUE)

ses.kfold(data.num, n.folds = 2, rep = 10, mod = hc.mod)

library(semPlot)
semPlot::semPaths(cfa, whatLabels = "std", edge.label.cex = .9, bifactor = "g",
                  layout = "tree2", exoCov = F)

# Output for table
clipr::write_clip(fitMeasures(cfa, c("cfi",	"tli", "rmsea", "cfi.scaled",	"tli.scaled",	"rmsea.scaled", "cfi.robust",	"tli.robust",	"rmsea.robust", "srmr", "chisq.scaled", "df.scaled")))

# Output for inline stats
stats <- fitMeasures(cfa, c("cfi.robust",	"tli.robust",	"rmsea.robust", "srmr"))
clipr::write_clip(paste(c("CFI = ", gsub("^0", "", as.character(round(stats[1], 3))),
                          ", TLI = ", gsub("^0", "", as.character(round(stats[2], 3))),
                          ", RMSEA = ", gsub("^0", "", as.character(round(stats[3], 3))),
                          ", SRMR = ", gsub("^0", "", as.character(round(stats[4], 3)))
), collapse = ""))


### Direct Schmid Leiman ----

sl.target <- matrix(c( 1, 1, 0, 0, 0, 0,
                       1, 1, 0, 0, 0, 0,
                       1, 1, 0, 0, 0, 0,
                       1, 1, 0, 0, 0, 0,
                       1, 1, 0, 0, 0, 0,
                       1, 1, 0, 0, 0, 0,
                       1, 1, 0, 0, 0, 0,
                       1, 0, 1, 0, 0, 0,
                       1, 0, 1, 0, 0, 0,
                       1, 0, 1, 0, 0, 0,
                       1, 0, 0, 1, 0, 0,
                       1, 0, 0, 1, 0, 0,
                       1, 0, 0, 1, 0, 0,
                       1, 0, 0, 0, 1, 0,
                       1, 0, 0, 0, 1, 0,
                       1, 0, 0, 0, 1, 0,
                       1, 0, 0, 0, 0, 1,
                       1, 0, 0, 0, 0, 1), nrow = 18, ncol = 6, byrow = TRUE)

clipr::write_clip(names(data.num[,c("mystical6", "mystical22", "mystical25", "mystical15", "mystical8", "mystical13", "mystical10", "mystical5", "mystical7", "mystical4", "spiritual3", "spiritual2", "spiritual26", "psyphys5", "psyphys3", "psyphys9", "psyphys11", "psyphys1")]))
out <- BiFAD(
  R = as.matrix(
        polychoric(data.num[,c("mystical6", "mystical22", "mystical25", "mystical15", "mystical8", "mystical13", "mystical10", "mystical5", "mystical7", "mystical4", "spiritual3", "spiritual2", "spiritual26", "psyphys5", "psyphys3", "psyphys9", "psyphys11", "psyphys1")])
        $rho
      ),
  B = sl.target,
  numFactors = NULL
)
clipr::write_clip(out$B)
clipr::write_clip(out$BstarFR)


### Mystical 8 Mock Model ----
hc.mod <- '
# allind =~ mystical6 + mystical22 + mystical25 + mystical15 + mystical8 + mystical13 + mystical10 + mystical5 + mystical7 + mystical4 + spiritual3 + spiritual2 + spiritual26 + psyphys5 + psyphys3 + psyphys9 + psyphys11 + psyphys1
unityconsc =~ mystical6 + mystical22 + mystical25 + mystical15 + mystical13 + mystical10
bliss =~ mystical5 + mystical7 + mystical4
insight =~ spiritual3 + spiritual2 + spiritual26
energy =~ psyphys5 + psyphys3 + psyphys9
light =~ psyphys11 + psyphys1
lfd =~ mystical8
mystical8 ~~ .8*mystical8

g =~ unityconsc + bliss + insight + energy + light + lfd
'


### ESEM ----

cong.target <- matrix(c( 1, 0, 0, 0, 0,
                         1, 0, 0, 0, 0,
                         1, 0, 0, 0, 0,
                         1, 0, 0, 0, 0,
                         1, 0, 0, 0, 0,
                         1, 0, 0, 0, 0,
                         1, 0, 0, 0, 0,
                         0, 1, 0, 0, 0,
                         0, 1, 0, 0, 0,
                         0, 1, 0, 0, 0,
                         0, 0, 1, 0, 0,
                         0, 0, 1, 0, 0,
                         0, 0, 1, 0, 0,
                         0, 0, 0, 1, 0,
                         0, 0, 0, 1, 0,
                         0, 0, 0, 1, 0,
                         0, 0, 0, 0, 1,
                         0, 0, 0, 0, 1), nrow = 18, ncol = 5, byrow = TRUE)

mod <- ' # Higher Order S*(I-1)
efa("efa1")*f1 +
efa("efa1")*f2 +
efa("efa1")*f3 +
efa("efa1")*f4 +
efa("efa1")*f5
=~ mystical22 + mystical25 + mystical15 + mystical8 + mystical13 + mystical10 + mystical5 + mystical7 + mystical4 + spiritual3 + spiritual2 + spiritual26 + psyphys5 + psyphys3 + psyphys9 + psyphys11 + psyphys1
g=~ mystical6 + f1 + f2 + f3 + f4 + f5
'
esem <- sem(mod, data = data.num, std.lv = T, ordered = F, estimator = "MLR",
            rotation="geomin",
            rotation.args = list(orthogonal = F))

mod <- ' # Target Rotation ESEM
efa("efa1")*f1 +
efa("efa1")*f2 +
efa("efa1")*f3 +
efa("efa1")*f4 +
efa("efa1")*f5
=~ mystical6 + mystical22 + mystical25 + mystical15 + mystical8 + mystical13 + mystical10 + mystical5 + mystical7 + mystical4 + spiritual3 + spiritual2 + spiritual26 + psyphys5 + psyphys3 + psyphys9 + psyphys11 + psyphys1
'
esem <- sem(mod, data = data.num, std.lv = F, ordered = F, estimator = "MLR",
            rotation="target",
            rotation.args = list(target = cong.target))
esem <- sem(mod, data = data.num, std.lv = T, ordered = T, estimator = "WLSMV",
            rotation="target",
            rotation.args = list(target = cong.target))
esem <- sem(mod, data = data.num, std.lv = T, ordered = F, estimator = "MLR",
            rotation="geomin")
summary(esem, fit.measures = TRUE, standardized = TRUE)
fitMeasures(esem, c("bic", "cfi.robust",	"tli.robust",	"rmsea.robust", "srmr", "chisq.scaled", "df.scaled"))
clipr::write_clip(lavInspect(esem, what = "std")$lambda)

lavTestLRT(esem, esem2)


Bpattern <- matrix(c( 1, 	1, 	0, 	0, 	0, 	0, 	0, 	0, 
                      1, 	1, 	0, 	0, 	0, 	0, 	0, 	0, 
                      1, 	1, 	0, 	0, 	0, 	0, 	0, 	0, 
                      1, 	1, 	0, 	0, 	0, 	0, 	0, 	0, 
                      1, 	1, 	0, 	0, 	0, 	0, 	0, 	0, 
                      1, 	1, 	0, 	0, 	0, 	0, 	0, 	0, 
                      1, 	1, 	0, 	0, 	0, 	0, 	0, 	0, 
                      1, 	0, 	1, 	0, 	0, 	0, 	0, 	0, 
                      1, 	0, 	1, 	0, 	0, 	0, 	0, 	0, 
                      1, 	0, 	1, 	0, 	0, 	0, 	0, 	0, 
                      1, 	0, 	0, 	1, 	0, 	0, 	0, 	0, 
                      1, 	0, 	0, 	1, 	0, 	0, 	0, 	0, 
                      1, 	0, 	0, 	1, 	0, 	0, 	0, 	0, 
                      1, 	0, 	0, 	0, 	1, 	0, 	0, 	0, 
                      1, 	0, 	0, 	0, 	1, 	0, 	0, 	0, 
                      1, 	0, 	0, 	0, 	1, 	0, 	0, 	0, 
                      1, 	0, 	0, 	0, 	0, 	1, 	0, 	0, 
                      1, 	0, 	0, 	0, 	0, 	1, 	0, 	0, 
                      1, 	0, 	0, 	0, 	0, 	0, 	1, 	0, 
                      1, 	0, 	0, 	0, 	0, 	0, 	1, 	0, 
                      1, 	0, 	0, 	0, 	0, 	0, 	1, 	0, 
                      1, 	0, 	0, 	0, 	0, 	0, 	0, 	1, 
                      1, 	0, 	0, 	0, 	0, 	0, 	0, 	1, 
                      1, 	0, 	0, 	0, 	0, 	0, 	0, 	1), nrow = 24, ncol = 8, byrow = TRUE)


### Bifactor ESEM ----
mod <- '
efa("efa1")*g =~ mystical6 + mystical22 + mystical25 + mystical15 + mystical8 + mystical13 + mystical10 + mystical5 + mystical7 + mystical4 + spiritual3 + spiritual2 + spiritual26 + psyphys5 + psyphys3 + psyphys9 + psyphys11 + psyphys1
efa("efa1")*f1 +
efa("efa1")*f2 +
efa("efa1")*f3 +
efa("efa1")*f4 +
efa("efa1")*f5
=~ mystical6 + mystical22 + mystical25 + mystical15 + mystical8 + mystical13 + mystical10 + mystical5 + mystical7 + mystical4 + spiritual3 + spiritual2 + spiritual26 + psyphys5 + psyphys3 + psyphys9 + psyphys11 + psyphys1
'
# Orthogonal
esem <- sem(mod, data = data.num, std.lv = F, ordered = F,
            estimator = "MLR",
            rotation="bigeomin",
            orthogonal = T,
            rotation.args = list(orthogonal = T))
esem <- sem(mod, data = data.num, std.lv = F, ordered = T,
            estimator = "WLSMV",
            rotation="bigeomin",
            orthogonal = T,
            rotation.args = list(orthogonal = T))
# Oblique
esem <- sem(mod, data = data.num, std.lv = F, ordered = F,
            estimator = "MLR",
            rotation="bigeomin",
            orthogonal = F,
            rotation.args = list(orthogonal = F))
esem <- sem(mod, data = data.num, std.lv = F, ordered = T,
            estimator = "WLSMV",
            rotation="bigeomin",
            orthogonal = F,
            rotation.args = list(orthogonal = F))
summary(esem, fit.measures = TRUE, standardized = TRUE)
fitMeasures(esem, c("bic", "cfi.robust",	"tli.robust",	"rmsea.robust", "srmr", "chisq.scaled", "df.scaled"))
clipr::write_clip(lavInspect(esem, what = "std")$lambda)
lavInspect(esem, what = "fit")
clipr::write_clip(parameterestimates(esem))
lavTestLRT(esem, esem2)

### HC S-1 Bifactor Model ----
hc.bifactor <- ' # S-1 UC bifactor with covaring energy and light
g =~ mystical6 + mystical22 + mystical25 + mystical15 + mystical8 + mystical13 + mystical10 + mystical5 + mystical7 + mystical4 + spiritual3 + spiritual2 + spiritual26 + psyphys5 + psyphys3 + psyphys9 + psyphys11 + psyphys1
# unityconsc =~ mystical22 + mystical25 + mystical15 + mystical8 + mystical13 + mystical10
bliss =~ mystical5 + mystical7 + mystical4
insight =~ spiritual3 + spiritual2 + spiritual26
energy =~ psyphys5 + psyphys3 + psyphys9
light =~ psyphys11 + psyphys1
g ~~ 0*bliss + 0*insight + 0*energy + 0*light
'
cfa <- cfa(hc.bifactor, std.lv = F, data=data.num, ordered = F,
           estimator = "MLR", orthogonal = F)
cfa <- cfa(hc.bifactor, std.lv = F, data=data.num, ordered = T,
           estimator = "WLSMV", orthogonal = F)
fitMeasures(cfa, c("aic", "bic", "cfi.robust",	"tli.robust",	"rmsea.robust", "srmr", "chisq.scaled", "df.scaled"))
summary(cfa, fit.measures = TRUE, standardized = TRUE)

clipr::write_clip(inspect(cfa,what="std")$lambda)
clipr::write_clip(inspect(cfa,what="std")$psi)

lavTestLRT(cfa1, cfa2)

EFAtools::SL(cfa1)

library(EFAtools)
EFAtools::OMEGA(cfa, g_name = "g") # Bifactor approach
library(BifactorIndicesCalculator)
bifactorIndices(cfa)

stats <- fitMeasures(cfa1, c("cfi.robust",	"tli.robust",	"rmsea.robust", "srmr"))
clipr::write_clip(paste(c("CFI = ", gsub("^0", "", as.character(round(stats[1], 3))),
                          ", TLI = ", gsub("^0", "", as.character(round(stats[2], 3))),
                          ", RMSEA = ", gsub("^0", "", as.character(round(stats[3], 3))),
                          ", SRMR = ", gsub("^0", "", as.character(round(stats[4], 3)))
), collapse = ""))


### K-Fold ----

results <- ses.kfold(data.num, n.folds = 2, rep = 100, mod = mod)
ggplot(results, aes(x=model, y=cfi, fill=factor(model)), environment = environment()) +
  geom_boxplot(fill = "grey", aes(group = factor(model))) + 
  geom_jitter(width = 0.05, height = 0, colour = rgb(0,0,0,.3)) + 
  xlab("Higher Concsiousness") + ylab("Robust CFI") + 
  theme(legend.position="none", axis.title.x=element_blank(), axis.text.x=element_blank()) +
  scale_fill_grey(start=.3,end=.7)

ggplot(results, aes(x=model, y=tli, fill=factor(model)), environment = environment()) +
  geom_boxplot(fill = "grey", aes(group = factor(model))) + 
  geom_jitter(width = 0.05, height = 0, colour = rgb(0,0,0,.3)) + 
  xlab("Higher Concsiousness") + ylab("Robust TLI") + 
  theme(legend.position="none", axis.title.x=element_blank(), axis.text.x=element_blank()) +
  scale_fill_grey(start=.3,end=.7)

ggplot(results, aes(x=model, y=rmsea, fill=factor(model))) +
  geom_boxplot(fill = "grey", aes(group = factor(model))) + 
  geom_jitter(width = 0.05, colour = rgb(0,0,0,.3)) +
  xlab("Higher Concsiousness") + ylab("Robust RMSEA") + 
  theme(legend.position="none", axis.title.x=element_blank(), axis.text.x=element_blank()) +
  scale_fill_grey(start=.3,end=.7)

ggplot(results, aes(x=model, y=srmr, fill=factor(model))) +
  geom_boxplot(fill = "grey", aes(group = factor(model))) + 
  geom_jitter(width = 0.05, colour = rgb(0,0,0,.3)) +
  xlab("Higher Concsiousness") + ylab("SRMR") + 
  theme(legend.position="none", axis.title.x=element_blank(), axis.text.x=element_blank()) +
  scale_fill_grey(start=.3,end=.7)  


# Generate and evaluate model
# cfa <- cfa(mod, data=data.num[cfa_idx,], std.lv = TRUE, ordered = F, estimator = "MLR")
# cfa <- cfa(mod, data=data.num[efa_idx,], ordered = F, estimator = "MLR")
# cfa <- cfa(mod, data=data.num, ordered = T, estimator = "WLSMV")
cfa <- cfa(mod, std.lv = T, data=data.num, ordered = F, estimator = "MLR")
# cfa <- cfa(mod, std.lv = T, data=data.num, ordered = F, estimator = "MLR", orthogonal = T)
summary(cfa, fit.measures = TRUE, standardized = TRUE)
fitMeasures(cfa, c("cfi.robust",	"tli.robust",	"rmsea.robust", "srmr", "chisq.scaled", "df.scaled"))
limit.output(summary(cfa, fit.measures = TRUE, standardized = TRUE), 50) # Fit indices only
lavaanPlot(model = cfa, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stand = TRUE)

ses.kfold(data.num, n.folds = 2, rep = 10, mod = hc.mod)

library(semPlot)
semPlot::semPaths(cfa1, whatLabels = "std", edge.label.cex = .9, bifactor = "g",
                  layout = "tree2", exoCov = F)



### Data set fit indices ----

cfa <- cfa(hc.mod, data=data.num[cfa_idx,], ordered = F, estimator = "MLR")
summary(cfa, fit.measures = TRUE, standardized = TRUE)
cfa <- cfa(hc.mod, data=data.num[efa_idx,], ordered = F, estimator = "MLR")
summary(cfa, fit.measures = TRUE, standardized = TRUE)
cfa <- cfa(hc.mod, data=data.num, ordered = F, estimator = "MLR")
summary(cfa, fit.measures = TRUE, standardized = TRUE)

### Local areas of strain ----
lavResiduals(cfa)
modindices(cfa, sort = TRUE)

### Factor Covariance matrix ----
lavInspect(cfa, "cov.lv")
cov2cor(lavInspect(cfa, what = "est")$psi)


### Marker analysis ----
barplot(table(data.num[,'mystical6'])) # Visualization
shapiro.test(data.num$mystical22) # Test for normality
describe(data.num[,'spiritual26']) # Descriptive stats


### Bootstrapped factor loadings ----

getParamEstimates <- function(data, mod, idx, n) {
  # Get "default" coefficient vector in case of non-convergence
  cfa <- cfa(mod, ordered = TRUE, estimator = "WLSMV", std.lv = TRUE, warn = FALSE)
  blank_coef <- coef(cfa)
  cfa <- cfa(mod, data = data[idx,], ordered = TRUE, estimator = "WLSMV", std.lv = TRUE, warn = FALSE)
  # Check for non-convergence
  if (!lavInspect(cfa, "converged")) {
    cat("Non-convergence detected. Filling return vector with NA.\n")
    return(rep(NA, length(blank_coef[1:n]))) # Range to ensure same-length vectors are returned in the case of no values for ordinals
  } else {
    coef <- standardizedsolution(cfa)$est.std[1:n]
    return(coef) # Range to ensure same-length vectors are returned in the case of no values for ordinals
  }
}

set.seed(1234567)
nrow(data.num)
# Run initial bootstrap
n <- 18 # Number of rows from the lavaan parameter estimates table
casesNeeded <- 1000
casesFound <- 0
boot.hc <- boot(data.num, getParamEstimates, R = casesNeeded, n = n, mod = hc.mod)

# Update boot object with completed cases and counts
casesFound <- sum(complete.cases(boot.hc$t))
boot.hc$t <- boot.hc$t[complete.cases(boot.hc$t) , ]
boot.hc$R <- casesFound
# Fill with number of cases needed in case of a non-convergence
while(casesNeeded > casesFound) {
  newboot <- boot(data.num, getParamEstimates, R = 1, n = n, mod = hc.mod)
  if(sum(complete.cases(newboot$t)) == 1) {
    boot.hc$t <- rbind(boot.hc$t, newboot$t)
    casesFound <- sum(complete.cases(boot.hc$t))
    boot.hc$R <- casesFound
  }
}
set.seed(NULL)

# saveRDS(boot.hc, file = "outputs/boot_hc_params.rds")
boot.hc <- readRDS(file = "outputs/boot_hc_params.rds")

# Format empirical values and CI's, e.g., ".90 [.89, .91]"
formatLoading <- function(num) { sub("^0+", "", sprintf("%.2f", num)) } # No leading spaces, 2 decimal places
for(i in 1:18) {
  bootci <- boot.ci(boot.hc, type = "bca", index=i)
  print(paste0(
    parameterestimates(cfa)$lhs[i],
    parameterestimates(cfa)$op[i],
    parameterestimates(cfa)$rhs[i], ": ",
    formatLoading(boot.hc$t0[i]),
    " [", formatLoading(bootci$bca[4]), ", ", formatLoading(bootci$bca[5]), "]"
  ))
}


## Unmeasured Latent Variable (ULMC) Common Method Variance (CMV) Detection ----
umlv <- '
unityconsc =~ mystical6 + mystical22 + mystical25 + mystical15 + mystical8 + mystical13 + mystical10
bliss =~ mystical5 + mystical7 + mystical4
insight =~ spiritual3 + spiritual2 + spiritual26
energy =~ psyphys5 + psyphys3 + psyphys9
light =~ psyphys11 + psyphys1

unityconsc ~~ bliss + insight + energy + light
bliss ~~ insight + energy + light
insight ~~ energy + light
energy ~~ light

# g =~  cfl*mystical6 + cfl*mystical22 + cfl*mystical25 + cfl*mystical15 + cfl*mystical8 + cfl*mystical13 + cfl*mystical10 + cfl*mystical5 + cfl*mystical7 + cfl*mystical4 + cfl*spiritual3 + cfl*spiritual2 + cfl*spiritual26 + cfl*psyphys5 + cfl*psyphys3 + cfl*psyphys9 + cfl*psyphys11 + cfl*psyphys1
# g ~~ 0*unityconsc + 0*bliss + 0*insight + 0*energy + 0*light
'

umlv2 <- '
unityconsc =~ mystical6 + mystical22 + mystical25 + mystical15 + mystical8 + mystical13 + mystical10
bliss =~ mystical5 + mystical7 + mystical4
insight =~ spiritual3 + spiritual2 + spiritual26
energy =~ psyphys5 + psyphys3 + psyphys9
light =~ psyphys11 + psyphys1

unityconsc ~~ bliss + insight + energy + light
bliss ~~ insight + energy + light
insight ~~ energy + light
energy ~~ light

# g =~  mystical6 + mystical22 + mystical25 + mystical15 + mystical8 + mystical13 + mystical10 + mystical5 + mystical7 + mystical4 + spiritual3 + spiritual2 + spiritual26 + psyphys5 + psyphys3 + psyphys9 + psyphys11 + psyphys1
g =~  cfl*mystical6 + cfl*mystical22 + cfl*mystical25 + cfl*mystical15 + cfl*mystical8 + cfl*mystical13 + cfl*mystical10 + cfl*mystical5 + cfl*mystical7 + cfl*mystical4 + cfl*spiritual3 + cfl*spiritual2 + cfl*spiritual26 + cfl*psyphys5 + cfl*psyphys3 + cfl*psyphys9 + cfl*psyphys11 + cfl*psyphys1
g ~~ 0*unityconsc + 0*bliss + 0*insight + 0*energy + 0*light
'

cfa <- cfa(umlv, std.lv = T, data=data.num, ordered = F, estimator = "MLR")
cfa2 <- cfa(umlv2, std.lv = T, data=data.num, ordered = F, estimator = "MLR")
fitMeasures(cfa, c("cfi.robust",	"tli.robust",	"rmsea.robust", "srmr", "chisq.scaled", "df.scaled"))
fitMeasures(cfa2, c("cfi.robust",	"tli.robust",	"rmsea.robust", "srmr", "chisq.scaled", "df.scaled"))
summary(cfa, fit.measures = TRUE, standardized = TRUE)
summary(cfa2, fit.measures = TRUE, standardized = TRUE)
lavTestLRT(cfa, cfa2)


## Factor reliability ----

cfa <- cfa(hc.mod, data=data.num, ordered = T, estimator = "WLSMV")

semTools::compRelSEM(cfa, tau.eq = T, ord.scale = T) # Ordinal Alpha
semTools::compRelSEM(cfa, ord.scale = T) # Omega

# Ordinal Alpha and Omega
get.reliability <- function(data, indices) {
  fit <- cfa(hc.mod, data=data[indices,], ordered = T, estimator = "WLSMV")
  omega <- as.vector.data.frame(semTools::compRelSEM(fit))
  names(omega) <- paste0("o-", names(omega))
  ord_alpha <- as.vector.data.frame(semTools::compRelSEM(fit, tau.eq = T))
  names(ord_alpha) <- paste0("a-", names(ord_alpha))
  return(c(omega,ord_alpha))
}

boot.reliability <- boot(data.num, R = 1000, get.reliability) # R repetitions must be larger than number of rows!

# saveRDS(boot.reliability, file = "outputs/boot.reliability.rds")
boot.reliability <- readRDS(file = "outputs/boot.reliability.rds")

# Format empirical values and CI's, e.g., ".90 [.89, .91]"
for(i in 1:10) {
  bootci <- boot.ci(boot.reliability, type = "bca", index=i)
  if(i == 1) {
    print('Omega values')
  } else if(i == 6) {
    print('Alpha values')
  }
  print(paste0(round(boot.reliability$t0[i], 2), " [",round(bootci$bca[4], 2), ", ", round(bootci$bca[5], 2), "]"))
}

# Cronbach's Alpha "manual" calculations (should match the above)
get.alpha <- function(data, indices) {
  # Unity-Consciousness
  grepmatch <- '\\bmystical6\\b|\\bmystical22\\b|\\bmystical25\\b|\\bmystical15\\b|\\bmystical8\\b|\\bmystical13\\b|\\bmystical10\\b'
  tmp <- extract.numeric.columns.by.regex(data, grepmatch = grepmatch)
  uc_a <- alpha(tmp[indices,])
  
  #  Bliss
  grepmatch <- '\\bmystical5\\b|\\bmystical7\\b|\\bmystical4\\b'
  tmp <- extract.numeric.columns.by.regex(data, grepmatch = grepmatch)
  b_a <- alpha(tmp[indices,])
  
  # Insight
  grepmatch <- '\\bspiritual3\\b|\\bspiritual2\\b|\\bspiritual26\\b'
  tmp <- extract.numeric.columns.by.regex(data, grepmatch = grepmatch)
  i_a <- alpha(tmp[indices,])
  
  # Energy
  grepmatch <- '\\bpsyphys5\\b|\\bpsyphys3\\b|\\bpsyphys9\\b'
  tmp <- extract.numeric.columns.by.regex(data, grepmatch = grepmatch)
  e_a <- alpha(tmp[indices,])
  
  # Light
  grepmatch <- '\\bpsyphys11\\b|\\bpsyphys1\\b'
  tmp <- extract.numeric.columns.by.regex(data, grepmatch = grepmatch)
  l_a <- alpha(tmp[indices,])
  
  alpha <- as.vector(c(
    uc_a$total$raw_alpha,
    b_a$total$raw_alpha,
    i_a$total$raw_alpha,
    e_a$total$raw_alpha,
    l_a$total$raw_alpha
  ))
  names(alpha) <- c('unityconsc', 'bliss', 'insight', 'energy', 'light')
  return(alpha)
}

boot.alpha <- boot(data.num, R = 1000, get.alpha) # R repetitions must be larger than number of rows!

for(i in 1:5) {
  bootci <- boot.ci(boot.alpha, type = "bca", index=i)  
  print(paste0(round(boot.alpha$t0[i], 2), " [",round(bootci$bca[4], 2), ", ", round(bootci$bca[5], 2), "]"))
}

# Inspect Unity-Consciousness
grepmatch <- '\\bmystical6\\b|\\bmystical22\\b|\\bmystical25\\b|\\bmystical15\\b|\\bmystical8\\b|\\bmystical13\\b|\\bmystical10\\b' # uc
grepmatch <- '\\bmystical4\\b|\\bmystical5\\b|\\bmystical7\\b' # bliss
grepmatch <- '\\bspiritual2\\b|\\bspiritual3\\b|\\bspiritual26\\b' # insight
grepmatch <- '\\bpsyphys3\\b|\\bpsyphys5|\\bpsyphys9\\b' # energy
grepmatch <- '\\bpsyphys1\\b|\\bpsyphys11' # light

uc <- extract.numeric.columns.by.regex(ses.data, grepmatch = grepmatch)
polychoric(uc) # Highest correlation .84
cor(uc) # Highest correlation .79

### Holdout Cross-Validation ----
cfa <- cfa(hc.mod, data=data.num[cfa_idx,], ordered = F, estimator = "MLR")
fitMeasures(cfa, c("cfi.robust",	"tli.robust",	"rmsea.robust", "srmr"))
cfa <- cfa(hc.mod, data=data.num[efa_idx,], ordered = F, estimator = "MLR")
fitMeasures(cfa, c("cfi.robust",	"tli.robust",	"rmsea.robust", "srmr"))
cfa <- cfa(hc.mod, data=data.num, ordered = F, estimator = "MLR")
fitMeasures(cfa, c("cfi.robust",	"tli.robust",	"rmsea.robust", "srmr"))
summary(cfa_efa_ml, fit.measures = TRUE, standardized = TRUE)

## Model K-fold Cross-Validation ----

set.seed(1234567)
mod <- hc.mod
hc.kfold.results <- ses.kfold(data.num, 2, 100, mod)
nrow(hc.kfold.results)
set.seed(NULL)

ggplot(hc.kfold.results, aes(x=model, y=cfi, fill=factor(model))) +
  geom_boxplot(fill = "grey", aes(group = factor(model))) + 
  geom_jitter(width = 0.05, height = 0, colour = rgb(0,0,0,.3)) + 
  xlab("Higher Concsiousness") + ylab("Robust CFI") + 
  theme(legend.position="none", axis.title.x=element_blank(), axis.text.x=element_blank()) +
  scale_fill_grey(start=.3,end=.7)

round(quantile(hc.kfold.results[,"cfi"], c(0.025, .5, 0.975)), 2)
round(sd(hc.kfold.results[,"cfi"]), 3)

ggplot(hc.kfold.results, aes(x=model, y=tli, fill=factor(model))) +
  geom_boxplot(fill = "grey", aes(group = factor(model))) + 
  geom_jitter(width = 0.05, height = 0, colour = rgb(0,0,0,.3)) + 
  xlab("Higher Concsiousness") + ylab("Robust TLI") + 
  theme(legend.position="none", axis.title.x=element_blank(), axis.text.x=element_blank()) +
  scale_fill_grey(start=.3,end=.7)

round(quantile(hc.kfold.results[,"tli"], c(0.025, .5, 0.975)), 2)
round(sd(hc.kfold.results[,"tli"]), 3)

ggplot(hc.kfold.results, aes(x=model, y=rmsea, fill=factor(model))) +
  geom_boxplot(fill = "grey", aes(group = factor(model))) + 
  geom_jitter(width = 0.05, colour = rgb(0,0,0,.3)) +
  xlab("Higher Concsiousness") + ylab("Robust RMSEA") + 
  theme(legend.position="none", axis.title.x=element_blank(), axis.text.x=element_blank()) +
  scale_fill_grey(start=.3,end=.7)

round(quantile(hc.kfold.results[,"rmsea"], c(0.025, .5, 0.975)), 3)
round(sd(hc.kfold.results[,"rmsea"]), 3)

ggplot(hc.kfold.results, aes(x=model, y=srmr, fill=factor(model))) +
  geom_boxplot(fill = "grey", aes(group = factor(model))) + 
  geom_jitter(width = 0.05, colour = rgb(0,0,0,.3)) +
  xlab("Higher Concsiousness") + ylab("SRMR") + 
  theme(legend.position="none", axis.title.x=element_blank(), axis.text.x=element_blank()) +
  scale_fill_grey(start=.3,end=.7)

round(quantile(hc.kfold.results[,"srmr"], c(0.025, .5, 0.975)), 3)
round(sd(hc.kfold.results[,"srmr"]), 3)



## Measurement invariance ----

# Age MI
data.num <- extract.numeric.columns.by.regex(ses.data, 'mystical\\d+|spiritual\\d+|psyphys\\d+|sex|age')
data.age <- data.num
age_breaks <- c(18, 35, 50, 100)
# age_labels <- c("0-35 years old", "35-50 years old", "51-105 years old")
# Create the new factor variable, then convert to numeric. 
data.age$age_group <- as.factor(cut(data.age$age, breaks = age_breaks, right = FALSE))
table(data.age$age_group)
# sum(table(data.age$age_group))
data.age <- na.omit(data.age)
nrow(data.age)
fit1 <- cfa(hc.mod, data=data.age, group = "age_group", ordered = T)
fit2 <- cfa(hc.mod, data=data.age, group = "age_group", ordered = T, group.equal = "loadings")
fit3 <- cfa(hc.mod, data=data.age, group = "age_group", ordered = T, group.equal = c("intercepts", "loadings"))
# summary(fit3, fit.measures = TRUE, standardized = TRUE)
lavTestLRT(fit1, fit2, fit3)


# Sex MI
remove.intersex <- data.num[!(data.num$sex == 3),] # Justification: Only one repondent indicated they are intersex - not enough data to run a model of them as a separate group
nrow(remove.intersex)
table(remove.intersex$sex)
fit1 <- cfa(hc.mod, data=remove.intersex, group = "sex", ordered = T)
fit2 <- cfa(hc.mod, data=remove.intersex, group = "sex", ordered = T, group.equal = "loadings")
fit3 <- cfa(hc.mod, data=remove.intersex, group = "sex", ordered = T, group.equal = c("intercepts", "loadings"))
# summary(fit3, fit.measures = TRUE, standardized = TRUE)
lavTestLRT(fit1, fit2, fit3)


# NDE MI - Exploratory only
# data.nde <- data.num
# data.nde$group <- ifelse(data.num$spiritual16 %in% c(1), "No NDE", "NDE")
# nrow(data.nde)
# table(data.nde$group)
# fit1 <- cfa(hc.mod, data=data.nde, group = "group", ordered = T)
# fit2 <- cfa(hc.mod, data=data.nde, group = "group", ordered = T, group.equal = "loadings")
# fit3 <- cfa(hc.mod, data=data.nde, group = "group", ordered = T, group.equal = c("intercepts", "loadings"))
# # summary(fit1, fit.measures = TRUE, standardized = TRUE)
# lavTestLRT(fit1, fit2, fit3)



## Unidimensionality ----

### Examine factor loading differences between covarying and higher-order structures ----

cfa <- cfa(hc.mod, data=data.num, std.lv = T, ordered = T, estimator = "WLSMV")
clipr::write_clip(parameterEstimates(cfa))
summary(cfa, fit.measures = T, standardized = T)

mod <- paste0(hc.mod, "\n", 'g =~ unityconsc + bliss + insight + energy + light')
# mod <- paste0(hc.mod, "\n", 'g =~ unityconsc + bliss + insight + energy + light + intuition + oob')
cfa <- cfa(hc.HO, data=data.num, std.lv = T, ordered = T, estimator = "WLSMV")
clipr::write_clip(parameterEstimates(cfa, standardized = T))

cfa <- cfa(mod, data=data.num, ordered = F, estimator = "MLR")
cfa <- cfa(mod, data=data.num, ordered = T, estimator = "WLSMV")
fitMeasures(cfa, c("cfi.robust",	"tli.robust",	"rmsea.robust", "srmr"))

clipr::write_clip(fitMeasures(cfa, c("cfi",	"tli", "rmsea", "cfi.scaled",	"tli.scaled",	"rmsea.scaled", "cfi.robust",	"tli.robust",	"rmsea.robust", "srmr", "chisq.scaled", "df.scaled")))
stats <- fitMeasures(cfa, c("cfi.robust",	"tli.robust",	"rmsea.robust", "srmr"))
clipr::write_clip(paste(c("CFI = ", gsub("^0", "", as.character(round(stats[1], 3))),
                          ", TLI = ", gsub("^0", "", as.character(round(stats[2], 3))),
                          ", RMSEA = ", gsub("^0", "", as.character(round(stats[3], 3))),
                          ", SRMR = ", gsub("^0", "", as.character(round(stats[4], 3)))
), collapse = ""))

summary(cfa, fit.measures = T, standardized = T)
lavaanPlot(model = cfa, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stand = TRUE)


### Factor Covariance matrix ----
lavInspect(cfa, "cov.lv")
cov2cor(lavInspect(cfa, what = "est")$psi)

### Schmid-Leiman transformation for higher order factor ----
cfa <- cfa(hc.HO, data=data.num, ordered = T, estimator = "WLSMV") # WLSMV - Confirmatory Factor Analysis p. 354
EFAtools::SL(cfa) # Must have higher order factor 'g', aka Higher Consciousness

### McDonald's Omega of HC ----
library(EFAtools)
EFAtools::OMEGA(cfa, g_name = "g") # Bifactor approach

library(semTools)
compRelSEM(cfa, higher = "g", ord.scale = T) # As of 12/7/23 requires development version of semTools, e.g., devtools::install_github("simsem/semTools/semTools")


# ~~~~~~~~~~~~~~~~~~~~~~ ----
# # # # # # # #
# talents  ----
# # # # # # # #

grepmatch = "talents\\d+"
talents.num <- extract.numeric.columns.by.regex(ses.data, grepmatch)
corPlot(talents.num)

fa.res <- ses.qgroup("talents", grepmatch, nfactors = 1, omit.na = F) # parallel = T

# Talents CFA Model

data.num <- extract.numeric.columns.by.regex(ses.data, 'mystical\\d+|spiritual\\d+|psyphys\\d+|psygrowth\\d+|talents\\d+')
data.num <- na.omit(data.num)

# Factor MIMIC
# mod <- NULL # Start the model here - to check local fit, etc.
mod <- hc.mod
mod <- paste0(mod, "\n", 'talents =~ talents4 + talents5 + talents6')
mod <- paste0(mod, "\n", 'g ~ talents')
mod <- paste0(mod, "\n", 'g =~ unityconsc + bliss + insight + energy + light')
cfa <- cfa(mod, data=data.num, ordered = TRUE, std.lv = TRUE, likelihood = "WLSMV") # WLSMV - Confirmatory Factor Analysis p. 354
fitMeasures(cfa, c("cfi.robust",	"tli.robust",	"rmsea.robust", "srmr"))
limit.output(summary(cfa, fit.measures = TRUE, standardized = TRUE), 150) # Fit indices only
summary(cfa, fit.measures = TRUE, standardized = TRUE)
lavaanPlot(model = cfa, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stand = TRUE)


# Regularized MIMIC
reg <- ses.regsem("talents\\d+", hc.mod = hc.mod, data = data.num, n.lambda=10 , jump=0.01)
clipr::write_clip(reg)

# Standard MIMIC
mod <- hc.mod # Start with the HC model
all.vars <- paste('g ~', colnames(data.num[,grepl(grepmatch, names(data.num))]),collapse="\n")
mod <- paste(mod, all.vars)
mod <- mod
mod <- paste0(mod, "\n", 'g =~ unityconsc + bliss + insight + energy + light')
cfa <- cfa(mod, data=data.num, ordered = TRUE, std.lv = TRUE, likelihood = "WLSMV") # WLSMV - Confirmatory Factor Analysis p. 354
limit.output(summary(cfa, fit.measures = TRUE, standardized = TRUE), 150) # Fit indices only
# summary(cfa, fit.measures = TRUE, standardized = TRUE)  
lavaanPlot(model = cfa, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stand = TRUE)



# # # # # # # # # # # # # #
# psybliss ----
# # # # # # # # # # # # # #

grepmatch = "psybliss\\d+"
data.num <- extract.numeric.columns.by.regex(ses.data, grepmatch)

corPlot(data.num)

fa.res <- ses.qgroup("psybliss", grepmatch, nfactors = 5, omit.na = F)
clipr::write_clip(fa.res$fa$loadings)

# CFA Model

data.num <- extract.numeric.columns.by.regex(ses.data, 'mystical\\d+|spiritual\\d+|psyphys\\d+|psybliss\\d+')
data.num <- na.omit(data.num)

# Regularized MIMIC
mod <- hc.mod
reg <- ses.regsem("psybliss\\d+", hc.mod = mod, data = data.num, n.lambda=10 , jump=0.01)
# reg <- ses.regsem("psybliss\\d+", hc.mod = hc.mod, data = as.data.frame(scale(data.num)), n.lambda=20 , jump=0.01)
clipr::write_clip(reg)

# Factor MIMIC
# mod <- NULL # Start the model here - to check local fit, etc.
mod <- hc.mod
mod <- paste0(mod, "\n", 'oneness =~ psybliss21 + psybliss18 + psybliss22 + psybliss19')
mod <- paste0(mod, "\n", 'peaks =~ psybliss4 + psybliss3 + psybliss5')
# mod <- paste0(mod, "\n", 'overcome =~ psybliss10 + psybliss9 + psybliss29')

# MIMIC (formative)
mod <- paste0(mod, "\n", 'g ~ oneness')
mod <- paste0(mod, "\n", 'g ~ peaks')
mod <- paste0(mod, "\n", 'g =~ unityconsc + bliss + insight + energy + light')
# non.reg <- mod
cfa <- cfa(mod, data=data.num, ordered = TRUE, std.lv = T, likelihood = "WLSMV") # Fix factor variances with std.lv = TRUE
limit.output(summary(cfa, fit.measures = TRUE, standardized = TRUE), 140)
summary(cfa, fit.measures = TRUE, standardized = TRUE)
modindices(cfa, sort = TRUE)
# round(cor(data.num[,c("mystical2", "mystical3")]), 2)
lavaanPlot(model = cfa, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stand = TRUE)
lavInspect(cfa, "cov.lv")
cov2cor(lavInspect(cfa, what = "est")$psi)



# # # # # # # # #
# psygrowth ----
# # # # # # # # #

grepmatch = "psygrowth\\d+"
data.num <- extract.numeric.columns.by.regex(ses.data, grepmatch)

corPlot(data.num)

# fa.res <- ses.qgroup("psygrowth", grepmatch, parallel = T, omit.na = T)
fa.res <- ses.qgroup("psygrowth", grepmatch, parallel = F, nfactors = 7, omit.na = T)
clipr::write_clip(fa.res$fa$loadings)

# CFA Model

data.num <- extract.numeric.columns.by.regex(ses.data, 'mystical\\d+|spiritual\\d+|psyphys\\d+|psygrowth\\d+')
data.num <- na.omit(data.num)


# Regularized MIMIC
# reg <- ses.regsem("psygrowth\\d+", hc.mod = hc.mod, data = data.num, n.lambda=20 , jump=0.01)
reg <- ses.regsem("psygrowth\\d+", hc.mod = hc.mod, data = data.num, n.lambda=5 , jump=0.01)
clipr::write_clip(reg)

# mod <- NULL # Start the model here - to check local fit, etc.
mod <- hc.mod
mod <- paste0(mod, "\n", 'desire =~ psygrowth18 + psygrowth19 + psygrowth20 + psygrowth15 + psygrowth17')
mod <- paste0(mod, "\n", 'psytal =~ psygrowth6 + psygrowth5 + psygrowth14')
mod <- paste0(mod, "\n", 'altruism =~ psygrowth30 + psygrowth10 + psygrowth45 + psygrowth33 + psygrowth34 + psygrowth35')
mod <- paste0(mod, "\n", 'physcare =~ psygrowth24 + psygrowth22 + psygrowth23')
mod <- paste0(mod, "\n", 'discon =~ psygrowth3 + psygrowth2')
mod <- paste0(mod, "\n", 'concen =~ psygrowth41 + psygrowth37 + psygrowth39')
mod <- paste0(mod, "\n", 'issues =~ psygrowth43 + psygrowth42')

# MIMIC (formative)
mod <- paste0(mod, "\n", 'g ~ desire')
mod <- paste0(mod, "\n", 'g ~ psytal')
mod <- paste0(mod, "\n", 'g ~ altruism')
mod <- paste0(mod, "\n", 'g ~ physcare')
mod <- paste0(mod, "\n", 'g ~ discon')
mod <- paste0(mod, "\n", 'g ~ concen')
mod <- paste0(mod, "\n", 'g ~ issues')

mod <- paste0(mod, "\n", 'g =~ unityconsc + bliss + insight + energy + light')
# non.reg <- mod
cfa <- cfa(mod, data=data.num, ordered = TRUE, likelihood = "WLSMV")
limit.output(summary(cfa, fit.measures = TRUE, standardized = TRUE), 140)
summary(cfa, fit.measures = TRUE, standardized = TRUE)
modindices(cfa, sort = TRUE)
# round(cor(data.num[,c("mystical2", "mystical3")]), 2)
lavaanPlot(model = cfa, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stand = TRUE)
lavInspect(cfa, "cov.lv")
cov2cor(lavInspect(cfa, what = "est")$psi)



# # # # # # # # #
# negpsych ----
# # # # # # # # #

grepmatch = "negpsych\\d+"
data.num <- extract.numeric.columns.by.regex(ses.data, grepmatch)

corPlot(data.num)

# fa.res <- ses.qgroup("negpsych", grepmatch, parallel = T, omit.na = T)
fa.res <- ses.qgroup("negpsych", grepmatch, parallel = F, nfactors = 6, omit.na = T)
clipr::write_clip(fa.res$fa$loadings)

# CFA Model

data.num <- extract.numeric.columns.by.regex(ses.data, 'mystical\\d+|spiritual\\d+|psyphys\\d+|negpsych\\d+')
data.num <- na.omit(data.num)

# Regularized MIMIC
mod <- hc.mod
# reg <- ses.regsem("negpsych\\d+", hc.mod = hc.mod, data = data.num, n.lambda=20 , jump=0.01)
library(regsem)
reg <- ses.regsem("negpsych\\d+", hc.mod = hc.mod, data = data.num, n.lambda=8 , jump=0.01)
clipr::write_clip(reg)


# mod <- NULL # Start the model here - to check local fit, etc.
mod <- hc.mod
mod <- paste0(mod, "\n", 'dysreg =~ negpsych16 + negpsych17 + negpsych15 + negpsych19 + negpsych20')
mod <- paste0(mod, "\n", 'detach =~ negpsych29 + negpsych30')
mod <- paste0(mod, "\n", 'negviz =~ negpsych1 + negpsych2 + negpsych3 + negpsych4')
mod <- paste0(mod, "\n", 'cogimp =~ negpsych35 + negpsych22 + negpsych23')
mod <- paste0(mod, "\n", 'rigid =~ negpsych33 + negpsych32 + negpsych31')
mod <- paste0(mod, "\n", 'fearcrazy =~ negpsych13 + negpsych26')

# MIMIC (formative)
mod <- paste0(mod, "\n", 'g ~ dysreg')
mod <- paste0(mod, "\n", 'g ~ detach')
mod <- paste0(mod, "\n", 'g ~ negviz')
mod <- paste0(mod, "\n", 'g ~ cogimp')
mod <- paste0(mod, "\n", 'g ~ rigid')
mod <- paste0(mod, "\n", 'g ~ fearcrazy')

mod <- paste0(mod, "\n", 'g =~ unityconsc + bliss + insight + energy + light')
# non.reg <- mod
cfa <- cfa(mod, data=data.num, ordered = TRUE, std.lv = T, estimator = "WLSMV") # Fix factor variances with std.lv = TRUE
cfa <- cfa(mod, data=data.num, std.lv = T, estimator = "MLR") # Fix factor variances with std.lv = TRUE
limit.output(summary(cfa, fit.measures = TRUE, standardized = TRUE), 140)
summary(cfa, fit.measures = TRUE, standardized = TRUE)
modindices(cfa, sort = TRUE)
# round(cor(data.num[,c("mystical2", "mystical3")]), 2)
lavaanPlot(model = cfa, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stand = TRUE)
lavInspect(cfa, "cov.lv")
cov2cor(lavInspect(cfa, what = "est")$psi)



# # # # # # # # # # # # # # # # #
# Final Psychological MIMIC ----
# # # # # # # # # # # # # # # # #

# CFA Model

data.num <- extract.numeric.columns.by.regex(ses.data, 'mystical\\d+|spiritual\\d+|psyphys\\d+|psybliss\\d+|psygrowth\\d+')
data.num <- na.omit(data.num)

# Regularized MIMIC
mod <- hc.mod
scaled.num <- as.data.frame(scale(data.num))
reg <- ses.regsem("psybliss\\d+|psygrowth\\d+", hc.mod = hc.mod, data = scaled.num, n.lambda=10 , jump=0.1)


# mod <- NULL # Start the model here - to check local fit, etc.
mod <- hc.mod
mod <- paste0(mod, "\n", 'oneness =~ psybliss21 + psybliss18 + psybliss22 + psybliss19')
mod <- paste0(mod, "\n", 'peaks =~ psybliss4 + psybliss3 + psybliss5')
mod <- paste0(mod, "\n", 'desire =~ psygrowth18 + psygrowth19 + psygrowth20 + psygrowth15 + psygrowth17')
mod <- paste0(mod, "\n", 'psytal =~ psygrowth6 + psygrowth5 + psygrowth14')
mod <- paste0(mod, "\n", 'altruism =~ psygrowth30 + psygrowth10 + psygrowth45 + psygrowth33 + psygrowth34')
mod <- paste0(mod, "\n", 'concen =~ psygrowth41 + psygrowth37')

# MIMIC (formative)
mod <- paste0(mod, "\n", 'g ~ oneness')
mod <- paste0(mod, "\n", 'g ~ peaks')
mod <- paste0(mod, "\n", 'g ~ desire')
mod <- paste0(mod, "\n", 'g ~ psytal')
mod <- paste0(mod, "\n", 'g ~ altruism')
mod <- paste0(mod, "\n", 'g ~ concen')


non.reg <- paste0(mod, "\n", 'g =~ unityconsc + bliss + insight + energy + light')
# non.reg <- mod
cfa <- cfa(non.reg, data=data.num, ordered = TRUE, std.lv = T, likelihood = "WLSMV") # Fix factor variances with std.lv = TRUE
limit.output(summary(cfa, fit.measures = TRUE, standardized = TRUE), 140)
# summary(cfa, fit.measures = TRUE, standardized = TRUE)
modindices(cfa, sort = TRUE)
# round(cor(data.num[,c("mystical2", "mystical3")]), 2)
lavaanPlot(model = cfa, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stand = TRUE)
lavInspect(cfa, "cov.lv")
cov2cor(lavInspect(cfa, what = "est")$psi)

reg.mod <- mod
reg.mod <- paste0(reg.mod, "\n", 'g =~ NA*unityconsc + bliss + insight + energy + light')
reg.mod <- paste0(reg.mod, "\n", 'g ~~ 1*g')
cfa <- cfa(reg.mod, data=data.num)
summary(cfa, fit.measures = TRUE, standardized = TRUE)
reg.out <- cv_regsem(cfa,n.lambda=10,jump=.01,type="lasso",pars_pen="regressions")
plot(reg.out)
summary(reg.out)
est <- as.data.frame(reg.out$final_pars)



# ~~~~~~~~~~~~~~~~~~~~~~ ----
# # # # # # # # #
# invmov ----
# # # # # # # # #

grepmatch = "invmov\\d+"
data.num <- extract.numeric.columns.by.regex(ses.data, grepmatch)
data.num <- na.omit(data.num)
nrow(data.num)

corPlot(data.num)

# fa.res <- ses.qgroup("invmov", grepmatch, parallel = T, omit.na = T)
fa.res <- ses.qgroup(fname = "invmov", grepmatch = grepmatch, parallel = F, nfactors = 5, omit.na = T)
clipr::write_clip(fa.res$fa$loadings)

# CFA Model

data.num <- extract.numeric.columns.by.regex(ses.data, 'mystical\\d+|spiritual\\d+|psyphys\\d+|invmov\\d+')
data.num <- na.omit(data.num)

# Regularized MIMIC
mod <- hc.mod
# reg <- ses.regsem("invmov\\d+", hc.mod = hc.mod, data = data.num, n.lambda=20 , jump=0.01)
reg <- ses.regsem("invmov\\d+", hc.mod = hc.mod, data = data.num, n.lambda=5 , jump=0.01)
clipr::write_clip(reg)

# mod <- NULL # Start the model here - to check local fit, etc.
mod <- hc.mod
mod <- paste0(mod, "\n", 'kriyas =~ invmov3 + invmov2 + invmov1')
mod <- paste0(mod, "\n", 'spasms =~ invmov14 + invmov15')
mod <- paste0(mod, "\n", 'dance =~ invmov7 + invmov9 + invmov6')
mod <- paste0(mod, "\n", 'breathgaze =~ invmov4 + invmov13')

# MIMIC (formative)
mod <- paste0(mod, "\n", 'g ~ kriyas')
mod <- paste0(mod, "\n", 'g ~ spasms')
mod <- paste0(mod, "\n", 'g ~ dance')
mod <- paste0(mod, "\n", 'g ~ breathgaze')

mod <- paste0(mod, "\n", 'g =~ unityconsc + bliss + insight + energy + light')
cfa <- cfa(mod, data=data.num, ordered = TRUE, std.lv = T, likelihood = "WLSMV") # Fix factor variances with std.lv = TRUE
limit.output(summary(cfa, fit.measures = TRUE, standardized = TRUE), 140)
summary(cfa, fit.measures = TRUE, standardized = TRUE)
modindices(cfa, sort = TRUE)
# round(cor(data.num[,c("mystical2", "mystical3")]), 2)
lavaanPlot(model = cfa, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stand = TRUE)
lavInspect(cfa, "cov.lv")
cov2cor(lavInspect(cfa, what = "est")$psi)




# # # # # # # # #
# sensation ----
# # # # # # # # #

grepmatch = "sensation\\d+"
data.num <- extract.numeric.columns.by.regex(ses.data, grepmatch)

corPlot(data.num)

# fa.res <- ses.qgroup("sensation", grepmatch, parallel = T, omit.na = T)
fa.res <- ses.qgroup("sensation", grepmatch, parallel = F, nfactors = 4, omit.na = T)
clipr::write_clip(fa.res$fa$loadings)

# CFA Model

data.num <- extract.numeric.columns.by.regex(ses.data, 'mystical\\d+|spiritual\\d+|psyphys\\d+|sensation\\d+')
data.num <- na.omit(data.num)

# Regularized MIMIC
mod <- hc.mod
# reg <- ses.regsem("sensation\\d+", hc.mod = hc.mod, data = data.num, n.lambda=20 , jump=0.01)
reg <- ses.regsem("sensation\\d+", hc.mod = mod, data = data.num, n.lambda=10 , jump=0.01)
clipr::write_clip(reg)

# mod <- NULL # Start the model here - to check local fit, etc.
mod <- hc.mod
mod <- paste0(mod, "\n", 'energytingle =~ sensation5 + sensation4 + sensation10')
mod <- paste0(mod, "\n", 'nerves =~ sensation8 + sensation9')
mod <- paste0(mod, "\n", 'sexual =~ sensation2 + sensation6')
mod <- paste0(mod, "\n", 'itch =~ sensation1 + sensation3')

# MIMIC (formative)
mod <- paste0(mod, "\n", 'g ~ energytingle')
mod <- paste0(mod, "\n", 'g ~ nerves')
mod <- paste0(mod, "\n", 'g ~ sexual')
mod <- paste0(mod, "\n", 'g ~ itch')

mod <- paste0(mod, "\n", 'g =~ unityconsc + bliss + insight + energy + light')
# non.reg <- mod
cfa <- cfa(mod, data=data.num, ordered = TRUE, std.lv = T, estimator = "WLSMV") # Fix factor variances with std.lv = TRUE
limit.output(summary(cfa, fit.measures = TRUE, standardized = TRUE), 140)
summary(cfa, fit.measures = TRUE, standardized = TRUE)
modindices(cfa, sort = TRUE)
# round(cor(data.num[,c("mystical2", "mystical3")]), 2)
lavaanPlot(model = cfa, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stand = TRUE)
lavInspect(cfa, "cov.lv")
cov2cor(lavInspect(cfa, what = "est")$psi)



# # # # # # # # #
# negphysical ----
# # # # # # # # #

grepmatch = "negphysical\\d+"
data.num <- extract.numeric.columns.by.regex(ses.data, grepmatch)

corPlot(data.num)

# fa.res <- ses.qgroup("negphysical", grepmatch, parallel = T, omit.na = T)
fa.res <- ses.qgroup("negphysical", grepmatch, parallel = F, nfactors = 5, omit.na = T)
clipr::write_clip(fa.res$fa$loadings)

# CFA Model
data.num <- extract.numeric.columns.by.regex(ses.data, 'mystical\\d+|spiritual\\d+|psyphys\\d+|negphysical\\d+')
data.num <- na.omit(data.num)

# Regularized MIMIC
mod <- hc.mod
# reg <- ses.regsem("negphysical\\d+", hc.mod = hc.mod, data = data.num, n.lambda=20 , jump=0.01)
reg <- ses.regsem("negphysical\\d+", hc.mod = hc.mod, data = data.num, n.lambda=10 , jump=0.01)
clipr::write_clip(reg)

# mod <- NULL # Start the model here - to check local fit, etc.
mod <- hc.mod
# mod <- paste0(mod, "\n", 'lightdif =~ negphysical12 + negphysical11 + negphysical9')
# mod <- paste0(mod, "\n", 'pains =~ negphysical6 + negphysical7 + negphysical5')
mod <- paste0(mod, "\n", 'heartprob =~ negphysical4 + negphysical3')
mod <- paste0(mod, "\n", 'sicksens =~ negphysical2 + negphysical1')
# mod <- paste0(mod, "\n", 'rashanex =~ negphysical14 + negphysical13')

# MIMIC (formative)
# mod <- paste0(mod, "\n", 'g ~ lightdif')
# mod <- paste0(mod, "\n", 'g ~ pains')
mod <- paste0(mod, "\n", 'g ~ heartprob')
mod <- paste0(mod, "\n", 'g ~ sicksens')
# mod <- paste0(mod, "\n", 'g ~ rashanex')

non.reg <- paste0(mod, "\n", 'g =~ unityconsc + bliss + insight + energy + light')
# non.reg <- mod
cfa <- cfa(non.reg, data=data.num, ordered = TRUE, std.lv = T, likelihood = "WLSMV") # Fix factor variances with std.lv = TRUE
limit.output(summary(cfa, fit.measures = TRUE, standardized = TRUE), 140)
# summary(cfa, fit.measures = TRUE, standardized = TRUE)
modindices(cfa, sort = TRUE)
# round(cor(data.num[,c("mystical2", "mystical3")]), 2)
lavaanPlot(model = cfa, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stand = TRUE)
lavInspect(cfa, "cov.lv")
cov2cor(lavInspect(cfa, what = "est")$psi)

xnames <- c("negphysical6", "negphysical7")
ynames <- c("mystical6", "mystical22", "mystical25", "mystical15", "mystical8", "mystical13", "mystical10", "mystical5", "mystical7", "mystical4", "spiritual3", "spiritual2", "spiritual26", "psyphys5", "psyphys3", "psyphys9", "psyphys11", "psyphys1")
set.seed(1234567)
kfold.res <- ses.pred_kfold(data.num, non.reg, 2, 10, xnames, ynames)
set.seed(NULL)

ggplot(kfold.res, aes(x = model, y = rmsep, fill = factor(model))) +
  geom_boxplot(fill = "grey", aes(group = factor(model))) +
  geom_jitter(width = 0.05, height = 0, colour = rgb(0, 0, 0, 0.3)) +
  xlab("Data set") +
  ylab("RMSEp") +
  # theme(legend.position="none") +
  theme(legend.position="none", axis.title.x=element_blank(), axis.text.x=element_blank()) +
  scale_fill_grey(start = 0.3, end = 0.7)

round(quantile(ypred_results[,"rmsep"], c(0.025, .5, 0.975)), 3)

# # # # # # # # # # #
# otherphysical ----
# # # # # # # # # # #

grepmatch = "otherphysical\\d+"
data.num <- extract.numeric.columns.by.regex(ses.data, grepmatch)

corPlot(data.num)

fa.res <- ses.qgroup("otherphysical", grepmatch, parallel = F, nfactors = 7, omit.na = T)
clipr::write_clip(fa.res$fa$loadings)

# CFA Model

data.num <- extract.numeric.columns.by.regex(ses.data, 'mystical\\d+|spiritual\\d+|psyphys\\d+|otherphysical\\d+')
data.num <- na.omit(data.num)

# Regularized MIMIC
mod <- hc.mod
# reg <- ses.regsem("otherphysical\\d+", hc.mod = hc.mod, data = data.num, n.lambda=20 , jump=0.01)
reg <- ses.regsem("otherphysical\\d+", hc.mod = hc.mod, data = data.num, n.lambda=10 , jump=0.01)
clipr::write_clip(reg)

# mod <- NULL # Start the model here - to check local fit, etc.
mod <- hc.mod
mod <- paste0(mod, "\n", 'sleep =~ otherphysical24 + otherphysical21 + otherphysical4')
mod <- paste0(mod, "\n", 'digestive =~ otherphysical20 + otherphysical14 + otherphysical13 + otherphysical19')
mod <- paste0(mod, "\n", 'altered =~ otherphysical28 + otherphysical8 + otherphysical29')
mod <- paste0(mod, "\n", 'tempchange =~ otherphysical16 + otherphysical15')
mod <- paste0(mod, "\n", 'shining =~ otherphysical2 + otherphysical1')
mod <- paste0(mod, "\n", 'vitality =~ otherphysical11 + otherphysical12')
mod <- paste0(mod, "\n", 'sexchange =~ otherphysical22 + otherphysical30')

# MIMIC (formative)
mod <- paste0(mod, "\n", 'g ~ sleep')
mod <- paste0(mod, "\n", 'g ~ digestive')
mod <- paste0(mod, "\n", 'g ~ altered')
mod <- paste0(mod, "\n", 'g ~ tempchange')
mod <- paste0(mod, "\n", 'g ~ shining')
mod <- paste0(mod, "\n", 'g ~ vitality')
mod <- paste0(mod, "\n", 'g ~ sexchange')

non.reg <- paste0(mod, "\n", 'g =~ unityconsc + bliss + insight + energy + light')
# non.reg <- mod
cfa <- cfa(non.reg, data=data.num, ordered = TRUE, std.lv = T, likelihood = "WLSMV") # Fix factor variances with std.lv = TRUE
limit.output(summary(cfa, fit.measures = TRUE, standardized = TRUE), 140)
summary(cfa, fit.measures = TRUE, standardized = TRUE)
modindices(cfa, sort = TRUE)
# round(cor(data.num[,c("mystical2", "mystical3")]), 2)
lavaanPlot(model = cfa, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stand = TRUE)
lavInspect(cfa, "cov.lv")
cov2cor(lavInspect(cfa, what = "est")$psi)





# ~~~~~~~~~~~~~~~~~~~~~~ ----
# # # # # # # # # # # # # # #
# Gate Question Analysis ----
# # # # # # # # # # # # # # #


# Extract all primary experience and gate questions
# data.num <- extract.numeric.columns.by.regex(ses.data, paste0('mystical\\d+|spiritual\\d+|psyphys\\d+|psygrowth\\d+|psybliss\\d+|pe.gate|pe.invmov.gate|pe.sensation.gate|pe.negphysical.gate|negpsych.gate'))

# Specify a gate question to analyze
# pe.gate
# pe.invmov.gate
# pe.sensation.gate
# pe.negphysical.gate
# negpsych.gate

gatequestions <- names(ses.data[ , grepl("*.gate", names(ses.data))])
for(gateq in gatequestions) {
  data.num <- extract.numeric.columns.by.regex(ses.data, paste0('mystical\\d+|spiritual\\d+|psyphys\\d+|psygrowth\\d+|psybliss\\d+|',gateq))
  data.num <- na.omit(data.num)
  print(paste("Rows:",nrow(data.num)))
  mod <- hc.mod
  mod <- paste0(mod, "\n", 'g =~ unityconsc + bliss + insight + energy + light')
  mod <- paste0(mod, "\n", 'g ~ ', gateq)
  cfa <- cfa(mod, data=data.num, ordered = TRUE, estimator = "WLSMV", std.lv = T)
  # summary(cfa, fit.measures = TRUE, standardized = TRUE)
  limit.output(summary(cfa, fit.measures = TRUE, standardized = TRUE), 100) # Fit indices only
}

data.num <- extract.numeric.columns.by.regex(ses.data, paste0('mystical\\d+|spiritual\\d+|psyphys\\d+|psygrowth\\d+|psybliss\\d+|*.gate'))
df <- data.frame(Question=character(), Yes=character(), No=character(), Total=character())
for(gateq in gatequestions) {
  data.num <- extract.numeric.columns.by.regex(ses.data, paste0('mystical\\d+|spiritual\\d+|psyphys\\d+|psygrowth\\d+|psybliss\\d+|',gateq))
  data.num <- na.omit(data.num)
  # print("Counts")
  # print(table(data.num[,gateq]))
  
  df <- rbind(df,
    data.frame(
      Question = gateq,
      Yes = paste0( # Format e.g., "10% (40)"
        (round(prop.table(table(data.num[ , gateq])), 2) * 100)[2],
        "% (", table(data.num[,gateq])[2], ")"
      ),
      No = paste0( # Format e.g., "10% (40)"
        (round(prop.table(table(data.num[ , gateq])), 2) * 100)[1],
        "% (", table(data.num[,gateq])[1], ")"
      ),
      Total = paste0( # Format e.g., "10% (40)"
        table(data.num[,gateq])[1] + table(data.num[,gateq])[2]
      )
    )
  )
}

clipr::write_clip(df)

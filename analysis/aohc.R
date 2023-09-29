# °º¤ø,¸¸,ø¤º°`°º¤ø,¸,ø¤°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤°º¤ø,¸¸,ø¤:>
#
#              ASPECTS OF HIGHER CONSCIOUSNESS
#
#            All figures and quantifiable values
#
# °º¤ø,¸¸,ø¤º°`°º¤ø,¸,ø¤°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤°º¤ø,¸¸,ø¤:>

# Load libraries and utility scripts ----

library(ggplot2)
library(likert) # https://github.com/jbryer/likert
library(psych)
library(plyr) # For mapvalues
library(ltm)
# CFA Libraries
library(lavaan)
library(lavaanPlot)
source("code/ses-utility.R")
# ETL (respondent data and variable mappings) ----
ses.data <- ses.loaddatafile()
ses.vars <- ses.loadvarfile()

# ~~~~~~~~~~~~~~~~~~~~~~ ----

# Data Summary ----

# Total number of participants
print(paste("Number of records is " , nrow(ses.data)))

# Number of participants by sex
table(ses.data$sex)

# Number of participants by sex (graphically)
ggplot(data=ses.data, aes(x = sex, fill = sex)) +
  guides(fill = FALSE) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..)) +
  labs(x="Sex of Participant", y="Number of participants", fill="Sex")

# Age histogram
# Credit: https://www.datacamp.com/community/tutorials/make-histogram-ggplot2#gs.ko0NeIE
ggplot(data=ses.data[ses.data$age > 10, ]
, aes(x = age, width = .4)) +
  geom_histogram(binwidth = 5, col = "white", aes(fill =..count..), alpha = .8) +
  labs(x="Age of Participant", y="Count") +
  scale_x_continuous(breaks = seq(10, 100, by = 5))

#
## Likert Visualizations ----
#
likert_viz <- function (data, regex = "") {
  results <- ses.get.questiontext(
    data[,grepl(regex, names(data))], # Grab by category
    max_length = 30 # Truncate text
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
likert_viz(ses.data, "psybliss\\d+")
likert_viz(ses.data, "psygrowth\\d+")

# ~~~~~~~~~~~~~~~~~~~~~~ ----



# # # # # # # # # # # # # # # # # # # # #
# Experience Item Factor Analysis ----  
# # # # # # # # # # # # # # # # # # # # #

# In order to explore the underlying latent constructs of Higher Consciousness,
# exploratory factor analysis (EFA) was conducted first on the ungated survey items
# related to spiritual experiences (mystical, spiritual, and psychophysiological categories).
grepmatch = "mystical\\d+|spiritual\\d+|psyphys\\d+"
data.num <- extract.numeric.columns.by.regex(ses.data, grepmatch)

#
## Correlation matrices ----
#

grepmatch = "mystical\\d+"
corPlot(extract.numeric.columns.by.regex(ses.data, grepmatch))

grepmatch = "spiritual\\d+"
corPlot(extract.numeric.columns.by.regex(ses.data, grepmatch))

grepmatch = "psyphys\\d+"
corPlot(extract.numeric.columns.by.regex(ses.data, grepmatch))


#
## Validity and Reliability Tests ----
#

# Kaiser-Meyer-Olkin measure of sampling adequacy
KMO(data.num)[["MSA"]]

# Bartlett’s Test of Sphericity
bartlett.test(data.num) # result is greater than the critical value for chi2

# Cronbach's Alpha
psych::alpha(data.num)


#
## Select # of Factors ----
#

# Cattell's scree test
# Raymond B. Cattell (1966) The Scree Test For The Number Of Factors, Multivariate Behavioral Research, 1:2, 245-276, DOI: 10.1207/s15327906mbr0102_10
scree(data.num)

# Horn’s parallel analysis
# Horn, J.L. A rationale and test for the number of factors in factor analysis. Psychometrika 30, 179–185 (1965). https://doi.org/10.1007/BF02289447
# Dinno, A. (2009). Exploring the sensitivity of Horn's parallel analysis to the distributional form of random data. Multivariate behavioral research, 44(3), 362-388.
fa.parallel(x = data.num, cor = "poly", fa = "both", sim = TRUE, n.iter=100)
fa.parallel(x = data.num, cor = "poly", fa = "pc", n.iter=20)

# Revelle’s Very Simple Structure
# Revelle, W., & Rocklin, T. (1979). Very simple structure: An alternative procedure for estimating the optimal number of interpretable factors. Multivariate behavioral research, 14(4), 403-414.
vss(data.num)

## Experience Item ICLUST ----
# Revelle, W. (1978). ICLUST: A cluster analytic approach to exploratory and confirmatory scale construction. Behavior Research Methods & Instrumentation, 10(5), 739-742.
pchor <- polychoric(data.num)
iclust <- iclust(pchor$rho, beta.min = 0.99, n.iterations = 20, purify = TRUE, nclusters = 8)
iclust <- iclust(data.num)
ICLUST.graph(iclust)

#
## Factor Extraction ----
#
fa.res <- fa(r = data.num, nfactors = 8, fm = "pa", rotate = "promax", cor = "poly")
pca.res <- principal(r = data.num, nfactors = 4, cor = "poly")
pca.loadings <- as.data.frame.matrix(pca.res$loadings)

# McDonald's Omega(h and t)
# Revelle, W., & Condon, D. M. (2019). Reliability from α to ω: A tutorial. Psychological assessment, 31(12), 1395.
omega(m = data.num, nfactors = 8, fm="pa", rotate="promax", poly = TRUE)

fa.diagram(fa.res)
omega(m = data.num, 8)

# Factor loadings (ouput to CSV)
friendly.loadings <- ses.format.loadings(fa.res$loadings)
write.csv(friendly.loadings, file = paste("outputs/", "initial", "-loadings.csv", sep = ''))

# Factor correlations (ouput to CSV)
write.csv(fa.res$Phi, file = paste("outputs/", "initial", "-factorcorrelations.csv", sep = ''))


# # # # # # # # # # # # # # # #
# Mystical Factor Analysis ----
# # # # # # # # # # # # # # # #

grepmatch = "mystical\\d+"
data.num <- extract.numeric.columns.by.regex(ses.data, grepmatch)

#
# RELIABILITY TESTS (PRE-FA)
#

# Kaiser-Meyer-Olkin measure of sampling adequacy
KMO(data.num)[["MSA"]]

# Bartlett’s Test of Sphericity
bartlett.test(data.num) # result is greater than the critical value for chi2

# Cronbach's Alpha
psych::alpha(data.num)

#
# N factor selection
#

# Cattell's scree test
# Raymond B. Cattell (1966) The Scree Test For The Number Of Factors, Multivariate Behavioral Research, 1:2, 245-276, DOI: 10.1207/s15327906mbr0102_10
scree(data.num)

# Horn’s parallel analysis
# Horn, J.L. A rationale and test for the number of factors in factor analysis. Psychometrika 30, 179–185 (1965). https://doi.org/10.1007/BF02289447
# Dinno, A. (2009). Exploring the sensitivity of Horn's parallel analysis to the distributional form of random data. Multivariate behavioral research, 44(3), 362-388.
fa.parallel(x = data.num, cor = "poly", fa = "both", sim = TRUE, n.iter=20)

# Revelle’s Very Simple Structure
# Revelle, W., & Rocklin, T. (1979). Very simple structure: An alternative procedure for estimating the optimal number of interpretable factors. Multivariate behavioral research, 14(4), 403-414.
vss(data.num)

# Revelle, W. (1978). ICLUST: A cluster analytic approach to exploratory and confirmatory scale construction. Behavior Research Methods & Instrumentation, 10(5), 739-742.
iclust(data.num)

# Factor analysis (first iteration)
fa.res <- fa(r = data.num, nfactors = 4, fm = "pa", rotate = "promax", cor = "poly")
fa.diagram(fa.res)

# Factor loadings (ouput to CSV)
friendly.loadings <- ses.format.loadings(fa.res$loadings)
write.csv(friendly.loadings, file = paste("outputs/", "mystical", "-loadings.csv", sep = ''))

# Factor correlations (ouput to CSV)
write.csv(fa.res$Phi, file = paste("outputs/", "mystical", "-factorcorrelations.csv", sep = ''))



# # # # # # # # # # # # # #
# Experience Item CFA ----
# # # # # # # # # # # # # #

data.num <- extract.numeric.columns.by.regex(ses.data, 'mystical\\d+|spiritual\\d+|psyphys\\d+|pe.gate')
data.num <- na.omit(data.num)

## CFA model ----

### All item model (baseline) ----
mod.all <- 'f =~ psyphys11 + psyphys1 + psyphys12 + psyphys2 + mystical13 + spiritual11 + mystical4 + psyphys8 + mystical12 + spiritual25 + spiritual12 + mystical9 + psyphys6 + mystical10 + spiritual15 + psyphys9 + mystical22 + mystical23 + psyphys3 + mystical26 + spiritual14 + spiritual13 + mystical5 + mystical24 + spiritual24 + mystical7 + spiritual18 + spiritual9 + mystical2 + mystical3 + spiritual22 + mystical25 + spiritual1 + spiritual6 + mystical6 + mystical15 + spiritual10 + spiritual20 + spiritual23 + mystical14 + mystical8 + spiritual27 + mystical11 + spiritual16 + mystical27 + spiritual2 + mystical17 + mystical18 + mystical21 + mystical1 + spiritual8 + psyphys5 + spiritual3 + spiritual21'
cfa.all <- cfa(mod.all, data=data.num, ordered = TRUE, std.lv = TRUE) # std.lv = fix factor variances
summary(cfa.all, fit.measures = TRUE, standardized = TRUE)

### Unity-Consciousness Model of Higher Consciousness ----
# For regression and to reduce noise, contains consc + unity ONLY
hc.mod <- NULL
hc.mod <- paste0(hc.mod, "\n", 'hc =~ mystical6 + mystical2 + mystical25 + spiritual26 + mystical15 + mystical8 + mystical13 + mystical10')
cfa <- cfa(hc.mod, data=data.num, ordered = T) # WLSMV - Confirmatory Factor Analysis p. 354
summary(cfa, fit.measures = TRUE, standardized = TRUE)

### Composite Experience Model of Higher Consciousness ----
hc.mod <- NULL
# hc.mod <- paste0(hc.mod, "\n", 'allind =~ mystical6 + mystical2 + mystical25 + spiritual26 + mystical15 + mystical8 + mystical13 + mystical10 + mystical5 + mystical7 + mystical4 + spiritual3 + spiritual2 + psyphys5 + psyphys3 + psyphys9 + psyphys11 + psyphys1')
# hc.mod <- paste0(hc.mod, "\n", 'hc =~ consc + unity + bliss + insight + energy + light')
# hc.mod <- paste0(hc.mod, "\n", 'hc =~ consc + unity + bliss + insight + f2_psysom')
# hc.mod <- paste0(hc.mod, "\n", 'f2_psysom =~ energy + light')
# hc.mod <- paste0(hc.mod, "\n", 'hc =~ consc + unity')
hc.mod <- paste0(hc.mod, "\n", 'consc =~ mystical6 + mystical2 + mystical25 + spiritual26 + mystical15')
hc.mod <- paste0(hc.mod, "\n", 'unity =~ mystical8 + mystical13 + mystical10')
hc.mod <- paste0(hc.mod, "\n", 'bliss =~ mystical5 + mystical7 + mystical4')
hc.mod <- paste0(hc.mod, "\n", 'insight =~ spiritual3 + spiritual2')
hc.mod <- paste0(hc.mod, "\n", 'energy =~ psyphys5 + psyphys3 + psyphys9')
hc.mod <- paste0(hc.mod, "\n", 'light = ~ psyphys11 + psyphys1')
cfa <- cfa(hc.mod, data=data.num, ordered = T, estimator = "WLSMV") # WLSMV - Confirmatory Factor Analysis p. 354
# summary(cfa, fit.measures = TRUE, standardized = TRUE)
limit.output(summary(cfa, fit.measures = TRUE, standardized = TRUE), 50) # Fit indices only
lavResiduals(cfa)
modindices(cfa, sort = TRUE)
lavaanPlot(model = cfa, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stand = TRUE)

# Analyze markers
# barplot(table(data.num[,'psyphys3'])) # Visualization
# shapiro.test(data.num$spiritual3) # Test for normality
# describe(data.num[,'mystical5']) # Descriptive stats

# Primary experience MIMIC
hc.mod <- paste0(hc.mod, "\n", 'bliss ~ hc')
hc.mod <- paste0(hc.mod, "\n", 'insight ~ hc')
hc.mod <- paste0(hc.mod, "\n", 'hc ~ energy')
hc.mod <- paste0(hc.mod, "\n", 'hc ~ light')
cfa <- cfa(hc.mod, data=data.num, ordered = T) # WLSMV - Confirmatory Factor Analysis p. 354
summary(cfa, fit.measures = TRUE, standardized = TRUE)
lavaanPlot(model = cfa, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stand = TRUE)



## Internal reliability of factors ----

# Cronbach's Alpha > 0.7 is acceptable per Nunnally and Bernstein, 1994

# Higher Consciousness (Unity Consciousness)
psych::alpha(data.num[,c("mystical6", "mystical2", "mystical25", "spiritual26", "mystical15", "mystical8", "mystical13", "mystical10")])

# Consciousness
psych::alpha(data.num[,c("mystical6", "mystical2", "mystical25", "spiritual26", "mystical15")])

# Unity
psych::alpha(data.num[,c("mystical8", "mystical13", "mystical10")])

# Bliss
psych::alpha(data.num[,c("mystical5", "mystical7", "mystical4")])

# Insight
psych::alpha(data.num[,c("spiritual3", "spiritual2")])

# Energy
psych::alpha(data.num[,c("psyphys5", "psyphys3", "psyphys9")])

# Light
psych::alpha(data.num[,c("psyphys11", "psyphys1")])



# ~~~~~~~~~~~~~~~~~~~~~~ ----
# # # # # # # # # # # # # # # #
# Psychic Factor Analysis ----
# # # # # # # # # # # # # # # #

grepmatch = "psychic\\d+"
psychic.num <- extract.numeric.columns.by.regex(ses.data, grepmatch)

describe(psychic.num)

corPlot(ses.get.questiontext(psychic.num, max_length = 8))

# Likert viz
psychic.likert <- ses.data[,grepl(x = names(ses.data), pattern = grepmatch)]
psychic.likert <- ses.get.questiontext(psychic.likert, max_length = 10)
plot(likert(psychic.likert))


#
## FA Reliability Tests (pre-FA) ----
#

# Kaiser-Meyer-Olkin measure of sampling adequacy
KMO(psychic.num)[["MSA"]]

# Bartlett’s Test of Sphericity
bartlett.test(psychic.num) # result is greater than the critical value for chi2

# Cronbach's Alpha
psych::alpha(psychic.num)



#
## Select # of Factors ----
#

# Cattell's scree test
# Raymond B. Cattell (1966) The Scree Test For The Number Of Factors, Multivariate Behavioral Research, 1:2, 245-276, DOI: 10.1207/s15327906mbr0102_10
scree(psychic.num)

# Horn’s parallel analysis
# Horn, J.L. A rationale and test for the number of factors in factor analysis. Psychometrika 30, 179–185 (1965). https://doi.org/10.1007/BF02289447
# Dinno, A. (2009). Exploring the sensitivity of Horn's parallel analysis to the distributional form of random data. Multivariate behavioral research, 44(3), 362-388.
fa.parallel(x = psychic.num, cor = "poly", fa = "both", sim = TRUE, n.iter=20)

# Revelle’s Very Simple Structure
# Revelle, W., & Rocklin, T. (1979). Very simple structure: An alternative procedure for estimating the optimal number of interpretable factors. Multivariate behavioral research, 14(4), 403-414.
vss(psychic.num)

## Experience Item ICLUST ----
# Revelle, W. (1978). ICLUST: A cluster analytic approach to exploratory and confirmatory scale construction. Behavior Research Methods & Instrumentation, 10(5), 739-742.
pchor <- polychoric(psychic.num)
iclust <- iclust(pchor$rho, beta.min = 0.99, n.iterations = 20, purify = TRUE, nclusters = 1)
iclust <- iclust(psychic.num)


#
## Factor Extraction ----
#
psychic.fa.res <- fa(r = psychic.num, nfactors = 3, fm = "pa", rotate = "promax", cor = "poly")
pca.res <- principal(r = psychic.num, nfactors = 1, cor = "poly")
pca.loadings <- as.data.frame.matrix(pca.res$loadings)

# McDonald's Omega(h and t)
# Revelle, W., & Condon, D. M. (2019). Reliability from α to ω: A tutorial. Psychological assessment, 31(12), 1395.
omega(m = psychic.num, nfactors = 3, fm="pa", rotate="promax", poly = TRUE)

fa.diagram(psychic.fa.res)
omega(m = psychic.num, 8)

# Factor loadings (ouput to CSV)
friendly.loadings <- ses.format.loadings(psychic.fa.res$loadings)
write.csv(friendly.loadings, file = paste("outputs/", "psychic", "-loadings.csv", sep = ''))

# Factor correlations (ouput to CSV)
write.csv(psychic.fa.res$Phi, file = paste("outputs/", "psychic", "-factorcorrelations.csv", sep = ''))



# # # # # # # # # # # # # # # # # # # # #
# Psychic Correlates CFA Model ----
# # # # # # # # # # # # # # # # # # # # #

data.num <- extract.numeric.columns.by.regex(ses.data, 'mystical\\d+|spiritual\\d+|psyphys\\d+|psychic\\d+')

## CFA model ----

# Psychic MIMIC model (formative)
mod <- hc.mod
mod <- paste0(mod, "\n", 'hc ~ psychic1')
mod <- paste0(mod, "\n", 'hc ~ psychic2')
mod <- paste0(mod, "\n", 'hc ~ psychic3')
mod <- paste0(mod, "\n", 'hc ~ psychic4')
mod <- paste0(mod, "\n", 'hc ~ psychic5')
mod <- paste0(mod, "\n", 'hc ~ psychic6')
mod <- paste0(mod, "\n", 'hc ~ psychic7')
mod <- paste0(mod, "\n", 'hc ~ psychic8')
mod <- paste0(mod, "\n", 'hc ~ psychic9')

cfa <- cfa(mod, data=data.num, ordered = T, std.lv = T, likelihood = "WLSMV") # Fix factor variances
summary(cfa, fit.measures = T, standardized = T) # rsquare above .8 are problematic (1/(1-rsquare) > 5)

modindices(cfa, sort = TRUE)
# round(cor(data.num[,c("mystical2", "mystical3")]), 2)
lavaanPlot(model = cfa, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stand = TRUE)
lavInspect(cfa, "cov.lv")
cov2cor(lavInspect(cfa, what = "est")$psi)



# ~~~~~~~~~~~~~~~~~~~~~~ ----
# # # # # # # # # # # # # # #
# Talents Factor Analysis ----
# # # # # # # # # # # # # # #

grepmatch = "talents\\d+"
talents.num <- extract.numeric.columns.by.regex(ses.data, grepmatch)
corPlot(talents.num)


#
## FA Reliability Tests (pre-FA) ----
#

# Kaiser-Meyer-Olkin measure of sampling adequacy
KMO(talents.num)[["MSA"]]

# Bartlett’s Test of Sphericity
bartlett.test(talents.num) # result is greater than the critical value for chi2

# Cronbach's Alpha
psych::alpha(talents.num)



#
## Select # of Factors ----
#

# Cattell's scree test
# Raymond B. Cattell (1966) The Scree Test For The Number Of Factors, Multivariate Behavioral Research, 1:2, 245-276, DOI: 10.1207/s15327906mbr0102_10
scree(talents.num)

# Horn’s parallel analysis
# Horn, J.L. A rationale and test for the number of factors in factor analysis. Psychometrika 30, 179–185 (1965). https://doi.org/10.1007/BF02289447
# Dinno, A. (2009). Exploring the sensitivity of Horn's parallel analysis to the distributional form of random data. Multivariate behavioral research, 44(3), 362-388.
fa.parallel(x = talents.num, cor = "poly", fa = "both", sim = TRUE, n.iter=20)
fa.parallel(x = talents.num, cor = "poly", fa = "pc", n.iter=20)

# Revelle’s Very Simple Structure
# Revelle, W., & Rocklin, T. (1979). Very simple structure: An alternative procedure for estimating the optimal number of interpretable factors. Multivariate behavioral research, 14(4), 403-414.
vss(talents.num)

## Experience Item ICLUST ----
# Revelle, W. (1978). ICLUST: A cluster analytic approach to exploratory and confirmatory scale construction. Behavior Research Methods & Instrumentation, 10(5), 739-742.
pchor <- polychoric(talents.num)
iclust <- iclust(pchor$rho, beta.min = 0.99, n.iterations = 20, purify = TRUE, nclusters = 1)
iclust <- iclust(talents.num)


#
## Factor Extraction ----
#
talents.fa.res <- fa(r = talents.num, nfactors = 2, fm = "pa", rotate = "promax", cor = "poly")
pca.res <- principal(r = talents.num, nfactors = 1, cor = "poly")
pca.loadings <- as.data.frame.matrix(pca.res$loadings)

# McDonald's Omega(h and t)
# Revelle, W., & Condon, D. M. (2019). Reliability from α to ω: A tutorial. Psychological assessment, 31(12), 1395.
omega(m = talents.num, nfactors = 7, fm="pa", rotate="promax", poly = TRUE)

fa.diagram(talents.fa.res)
omega(m = talents.num, 8)

# Factor loadings (ouput to CSV)
friendly.loadings <- ses.format.loadings(talents.fa.res$loadings)
write.csv(friendly.loadings, file = paste("outputs/", "psygrowth", "-loadings.csv", sep = ''))

# Factor correlations (ouput to CSV)
write.csv(psygrowth.fa.res$Phi, file = paste("outputs/", "psygrowth", "-factorcorrelations.csv", sep = ''))



# # # # # # # # # # # # # # # # # # # # #
# Talents Correlates CFA Model ----
# # # # # # # # # # # # # # # # # # # # #

data.num <- extract.numeric.columns.by.regex(ses.data, 'mystical\\d+|spiritual\\d+|psyphys\\d+|psygrowth\\d+|talents\\d+')
data.num <- na.omit(data.num)

## CFA model ----
mod <- hc.mod

# MIMIC (formative)
mod <- paste0(mod, "\n", 'hc ~ talents1')
mod <- paste0(mod, "\n", 'hc ~ talents2')
mod <- paste0(mod, "\n", 'hc ~ talents3')
mod <- paste0(mod, "\n", 'hc ~ talents4')
mod <- paste0(mod, "\n", 'hc ~ talents5')
mod <- paste0(mod, "\n", 'hc ~ talents6')
mod <- paste0(mod, "\n", 'hc ~ talents7')
mod <- paste0(mod, "\n", 'hc ~ talents8')

cfa <- cfa(mod, data=data.num, ordered = TRUE, std.lv = TRUE, likelihood = "WLSMV") # WLSMV - Confirmatory Factor Analysis p. 354
limit.output(summary(cfa, fit.measures = TRUE, standardized = TRUE), 150) # Fit indices only
# summary(cfa, fit.measures = TRUE, standardized = TRUE)

modindices(cfa, sort = TRUE)
# round(cor(data.num[,c("mystical2", "mystical3")]), 2)
lavaanPlot(model = cfa, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stand = TRUE)
lavInspect(cfa, "cov.lv")
cov2cor(lavInspect(cfa, what = "est")$psi)


# ~~~~~~~~~~~~~~~~~~~~~~ ----
# # # # # # # # # # # # # # # # # # # # # #
# Psychological Growth Factor Analysis ----
# # # # # # # # # # # # # # # # # # # # # #

grepmatch = "psygrowth\\d+"
psygrowth.num <- extract.numeric.columns.by.regex(ses.data, grepmatch)


#
## FA Reliability Tests (pre-FA) ----
#

# Kaiser-Meyer-Olkin measure of sampling adequacy
KMO(psygrowth.num)[["MSA"]]

# Bartlett’s Test of Sphericity
bartlett.test(psygrowth.num) # result is greater than the critical value for chi2

# Cronbach's Alpha
psych::alpha(psygrowth.num)



#
## Select # of Factors ----
#

# Cattell's scree test
# Raymond B. Cattell (1966) The Scree Test For The Number Of Factors, Multivariate Behavioral Research, 1:2, 245-276, DOI: 10.1207/s15327906mbr0102_10
scree(psygrowth.num)

# Horn’s parallel analysis
# Horn, J.L. A rationale and test for the number of factors in factor analysis. Psychometrika 30, 179–185 (1965). https://doi.org/10.1007/BF02289447
# Dinno, A. (2009). Exploring the sensitivity of Horn's parallel analysis to the distributional form of random data. Multivariate behavioral research, 44(3), 362-388.
fa.parallel(x = psygrowth.num, cor = "poly", fa = "both", sim = TRUE, n.iter=20)
fa.parallel(x = psygrowth.num, cor = "poly", fa = "pc", n.iter=20)

# Revelle’s Very Simple Structure
# Revelle, W., & Rocklin, T. (1979). Very simple structure: An alternative procedure for estimating the optimal number of interpretable factors. Multivariate behavioral research, 14(4), 403-414.
vss(psygrowth.num)

## Experience Item ICLUST ----
# Revelle, W. (1978). ICLUST: A cluster analytic approach to exploratory and confirmatory scale construction. Behavior Research Methods & Instrumentation, 10(5), 739-742.
pchor <- polychoric(psygrowth.num)
iclust <- iclust(pchor$rho, beta.min = 0.99, n.iterations = 20, purify = TRUE, nclusters = 8)
iclust <- iclust(psygrowth.num)


#
## Factor Extraction ----
#
psygrowth.fa.res <- fa(r = psygrowth.num, nfactors = 7, fm = "pa", rotate = "promax", cor = "poly")
pca.res <- principal(r = psygrowth.num, nfactors = 4, cor = "poly")
pca.loadings <- as.data.frame.matrix(pca.res$loadings)

# McDonald's Omega(h and t)
# Revelle, W., & Condon, D. M. (2019). Reliability from α to ω: A tutorial. Psychological assessment, 31(12), 1395.
omega(m = psygrowth.num, nfactors = 7, fm="pa", rotate="promax", poly = TRUE)

fa.diagram(psygrowth.fa.res)
omega(m = psygrowth.num, 8)

# Factor loadings (ouput to CSV)
friendly.loadings <- ses.format.loadings(psygrowth.fa.res$loadings)
write.csv(friendly.loadings, file = paste("outputs/", "psygrowth", "-loadings.csv", sep = ''))

# Factor correlations (ouput to CSV)
write.csv(psygrowth.fa.res$Phi, file = paste("outputs/", "psygrowth", "-factorcorrelations.csv", sep = ''))


# # # # # # # # # # # # # # # # # # #
# Psychological Growth CFA Model ----
# # # # # # # # # # # # # # # # # # #

data.num <- extract.numeric.columns.by.regex(ses.data, 'mystical\\d+|spiritual\\d+|psyphys\\d+|psygrowth\\d+')

## Psychological Growth Model ----
mod <- hc.mod
# mod <- NULL # Start the model here - to check local fit, etc.
mod <- paste0(mod, "\n", 'altruism =~ psygrowth30 + psygrowth10 + psygrowth45')
mod <- paste0(mod, "\n", 'talent =~ psygrowth6 + psygrowth5 + psygrowth14')
mod <- paste0(mod, "\n", 'desire =~ psygrowth18 + psygrowth19 + psygrowth20 + psygrowth15 + psygrowth17')
mod <- paste0(mod, "\n", 'physcare =~ psygrowth24 + psygrowth22 + psygrowth23')
mod <- paste0(mod, "\n", 'healing =~ psygrowth43 + psygrowth42 + psygrowth36')
mod <- paste0(mod, "\n", 'concen =~ psygrowth41 + psygrowth39 + psygrowth29 + psygrowth37')
mod <- paste0(mod, "\n", 'empty =~ psygrowth3 + psygrowth2')

# MIMIC (formative)
mod <- paste0(mod, "\n", 'hc ~ altruism')
mod <- paste0(mod, "\n", 'hc ~ talent')
mod <- paste0(mod, "\n", 'hc ~ concen')
mod <- paste0(mod, "\n", 'hc ~ desire')
mod <- paste0(mod, "\n", 'hc ~ physcare')
mod <- paste0(mod, "\n", 'hc ~ healing')
mod <- paste0(mod, "\n", 'hc ~ empty')

cfa <- cfa(mod, data=data.num, ordered = TRUE) # Fix factor variances with std.lv = TRUE
limit.output(summary(cfa, fit.measures = TRUE, standardized = TRUE), 150) # Fit indices only
# summary(cfa, fit.measures = TRUE, standardized = TRUE)

modindices(cfa, sort = TRUE)
# round(cor(data.num[,c("mystical2", "mystical3")]), 2)
lavaanPlot(model = cfa, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stand = TRUE)
lavInspect(cfa, "cov.lv")
cov2cor(lavInspect(cfa, what = "est")$psi)



# ~~~~~~~~~~~~~~~~~~~~~~ ----
# # # # # # # # # # # # # # # # # # # # # #
# Psychological Bliss Factor Analysis ----
# # # # # # # # # # # # # # # # # # # # # #

grepmatch = "psybliss\\d+"
psybliss.num <- extract.numeric.columns.by.regex(ses.data, grepmatch)


#
## FA Reliability Tests (pre-FA) ----
#

# Kaiser-Meyer-Olkin measure of sampling adequacy
KMO(psybliss.num)[["MSA"]]

# Bartlett’s Test of Sphericity
bartlett.test(psybliss.num) # result is greater than the critical value for chi2

# Cronbach's Alpha
psych::alpha(psybliss.num)



#
## Select # of Factors ----
#

# Cattell's scree test
# Raymond B. Cattell (1966) The Scree Test For The Number Of Factors, Multivariate Behavioral Research, 1:2, 245-276, DOI: 10.1207/s15327906mbr0102_10
scree(psybliss.num)

# Horn’s parallel analysis
# Horn, J.L. A rationale and test for the number of factors in factor analysis. Psychometrika 30, 179–185 (1965). https://doi.org/10.1007/BF02289447
# Dinno, A. (2009). Exploring the sensitivity of Horn's parallel analysis to the distributional form of random data. Multivariate behavioral research, 44(3), 362-388.
fa.parallel(x = psybliss.num, cor = "poly", fa = "both", sim = TRUE, n.iter=20)
fa.parallel(x = psybliss.num, cor = "poly", fa = "pc", n.iter=20)

# Revelle’s Very Simple Structure
# Revelle, W., & Rocklin, T. (1979). Very simple structure: An alternative procedure for estimating the optimal number of interpretable factors. Multivariate behavioral research, 14(4), 403-414.
vss(psybliss.num)

## Experience Item ICLUST ----
# Revelle, W. (1978). ICLUST: A cluster analytic approach to exploratory and confirmatory scale construction. Behavior Research Methods & Instrumentation, 10(5), 739-742.
pchor <- polychoric(psybliss.num)
iclust <- iclust(pchor$rho, beta.min = 0.99, n.iterations = 20, purify = TRUE, nclusters = 8)
iclust <- iclust(psybliss.num)


#
## Factor Extraction ----
#
psybliss.fa.res <- fa(r = psybliss.num, nfactors = 5, fm = "pa", rotate = "promax", cor = "poly")
pca.res <- principal(r = psybliss.num, nfactors = 2, cor = "poly")
pca.loadings <- as.data.frame.matrix(pca.res$loadings)

# McDonald's Omega(h and t)
# Revelle, W., & Condon, D. M. (2019). Reliability from α to ω: A tutorial. Psychological assessment, 31(12), 1395.
omega(m = psybliss.num, nfactors = 7, fm="pa", rotate="promax", poly = TRUE)

fa.diagram(psybliss.fa.res)
omega(m = psybliss.num, 8)

# Factor loadings (ouput to CSV)
friendly.loadings <- ses.format.loadings(psybliss.fa.res$loadings)
write.csv(friendly.loadings, file = paste("outputs/", "psybliss", "-loadings.csv", sep = ''))

# Factor correlations (ouput to CSV)
write.csv(psybliss.fa.res$Phi, file = paste("outputs/", "psybliss", "-factorcorrelations.csv", sep = ''))


# # # # # # # # # # # # # # # # # # #
# Psychological Bliss CFA Model ----
# # # # # # # # # # # # # # # # # # #

data.num <- extract.numeric.columns.by.regex(ses.data, 'mystical\\d+|spiritual\\d+|psyphys\\d+|psybliss\\d+')

## Psychological Bliss Model ----
mod <- hc.mod
# mod <- NULL # Start the model here - to check local fit, etc.
mod <- paste0(mod, "\n", 'oneness =~ psybliss21 + psybliss18 + psybliss22 + psybliss23 + psybliss19')
mod <- paste0(mod, "\n", 'creative =~ psybliss6 + psybliss24 + psybliss16')
mod <- paste0(mod, "\n", 'peaks =~ psybliss4 + psybliss3 + psybliss5')
mod <- paste0(mod, "\n", 'overcome =~ psybliss10 + psybliss9 + psybliss29')

# MIMIC (formative)
mod <- paste0(mod, "\n", 'hc ~ oneness')
mod <- paste0(mod, "\n", 'hc ~ creative')
mod <- paste0(mod, "\n", 'hc ~ peaks')
mod <- paste0(mod, "\n", 'hc ~ overcome')

cfa <- cfa(mod, data=data.num, ordered = TRUE) # Fix factor variances with std.lv = TRUE
limit.output(summary(cfa, fit.measures = TRUE, standardized = TRUE), 140)
# summary(cfa, fit.measures = TRUE, standardized = TRUE)


modindices(cfa, sort = TRUE)
# round(cor(data.num[,c("mystical2", "mystical3")]), 2)
lavaanPlot(model = cfa, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stand = TRUE)
lavInspect(cfa, "cov.lv")
cov2cor(lavInspect(cfa, what = "est")$psi)


# Measurement invariance ----
mod <- hc.mod 
data.num <- extract.numeric.columns.by.regex(ses.data, 'mystical\\d+|spiritual\\d+|psyphys\\d+')
nrow(data.num)
data.num <- cbind(data.num, ses.data['pe.gate'])
nrow(data.num)
data.num$pe.gate[is.na(data.num$pe.gate)] <- 'N'
ses.data$pe.gate <- as.factor(ses.data$pe.gate)
# ses.data$pe.negphysical.gate <- as.factor(ses.data$pe.negphysical.gate)
str(data.num)
data.num <- na.omit(data.num)

nrow(data.num)

fit1 <- cfa(mod, data=data.num, group = "pe.gate")
fit2 <- cfa(mod, data=data.num, group = "pe.gate", group.equal = "loadings")
fit3 <- cfa(mod, data=data.num, group = "pe.gate", group.equal = c("intercepts", "loadings"))

lavTestLRT(fit1, fit2, fit3)



# CFA Groups

data.num <- extract.numeric.columns.by.regex(ses.data, 'mystical\\d+|spiritual\\d+|psyphys\\d+|sex|pe.gate')
nrow(data.num)
data.num <- cbind(data.num, ses.data['sex'])
nrow(data.num)
data.num <- data.num[!data.num$sex == 3, ]
nrow(data.num)
ses.data$sex <- as.factor(ses.data$sex)
# ses.data$pe.negphysical.gate <- as.factor(ses.data$pe.negphysical.gate)
str(data.num)
data.num <- na.omit(data.num)

nrow(data.num)

fit1 <- cfa(mod, data=data.num, group = "sex")
fit2 <- cfa(mod, data=data.num, group = "sex", group.equal = "loadings")
fit3 <- cfa(mod, data=data.num, group = "sex", group.equal = c("intercepts", "loadings"))
# summary(cfa, fit.measures = TRUE, standardized = TRUE)
lavTestLRT(fit1, fit2, fit3)



pred <- lavPredict(cfa, append.data = TRUE)
tmp <- cbind(pred, data.num$sex)
tmp <- as.data.frame(tmp)

# Example in R
boxplot(conscunity ~ V47, data = tmp, col = c("red", "blue"))
t.test(conscunity ~ V47, data = tmp)
wilcox.test(conscunity ~ V47, data = tmp)



pred <- lavPredict(cfa, append.data = TRUE, transform = TRUE)
data.num <- na.omit(data.num)
tmp <- cbind(pred, data.num$pe.gate)
tmp <- as.data.frame(tmp)
tmp$V47 <- factor(tmp$V47)

# Example in R
boxplot(conscunity ~ V47, data = tmp, col = c("red", "blue"))
library(ggplot2)
ggplot(tmp, aes(x = V47, y = conscunity)) +
  geom_violin()
tmp$V47 <- factor(tmp$V47)
ggplot(tmp, aes(x = V47, y = conscunity, fill = V47)) +
  geom_violin() +
  ylab("CONSCUNITY") +
  xlab("Physical Symptoms")
t.test(conscunity ~ V47, data = tmp)
wilcox.test(conscunity ~ V47, data = tmp)







summary(ses.data)

#
# DATA CLEANSING
#
# kps.data <- kps.data[!is.na(kps.data['mystical16']),]


# CREATION OF NUMERIC DATA SET

kps.numeric <- kps.data
# Ensure all labels are used in the subset
likert.names <- grepl('mystical\\d+|spiritual\\d+|psyphys\\d+|psychic\\d+|talents\\d+|invmov\\d+|sensation\\d+|negphysical\\d+|otherphysical\\d+|negpsych\\d+|psybliss\\d+|psygrowth\\d+',
                      names(kps.numeric))
kps.numeric[,likert.names] <- lapply(kps.numeric[,likert.names], function(x) {
  x <- mapvalues(x,
                 from=c('Not at all', 'Very Weak/low intensity', 'Weak', 'Moderate', 'Strong', 'Very strong/high intensity'),
                 to=c("1", "2", "3", "4", "5", "6")
                 , warn_missing = FALSE)
  x <- as.numeric(x)
  return(x)
})
kps.numeric[is.na(kps.numeric)] <- 1

kps.numeric <- kps.numeric[,grepl('mystical\\d+|spiritual\\d+|psyphys\\d+|talents\\d+|invmov\\d+|sensation\\d+|negphysical\\d+|otherphysical\\d+|negpsych\\d+|psybliss\\d+|psygrowth\\d+', names(kps.numeric))]




#
# RESPONDENT BREAKDOWN BY SEX
#

ggplot(data=kps.data, aes(x = kps.data$sex, fill = kps.data$sex)) +
  guides(fill = FALSE) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..), vjust=-1) +
  labs(x="Sex of Participant", y="Number of participants", fill="Sex")

summary(kps.data$sex)
summary(kps.data$age)




#
# PRIMARY EXPERIENCE QUESTION BREAKDOWN
#

# Mystical likert visualization
q <- kps.data[,grepl("mystical\\d+", names(kps.data))]
q.questiontext <- ses.get.questiontext(q)
plot(likert(q.questiontext), centered = FALSE)

# Spiritual likert visualization
q <- kps.data[,grepl("spiritual\\d+", names(kps.data))]
q.questiontext <- ses.get.questiontext(q)
plot(likert(q.questiontext), centered = FALSE)

# PsychoPhysical likert visualization
q <- kps.data[,grepl("psyphys\\d+", names(kps.data))]
q.questiontext <- ses.get.questiontext(q)
plot(likert(q.questiontext), centered = FALSE)

# Psychic likert visualization
q <- kps.data[,grepl("psychic\\d+", names(kps.data))]
q.questiontext <- ses.get.questiontext(q)
plot(likert(q.questiontext), centered = FALSE)

# Talents likert visualization
q <- kps.data[,grepl("talents\\d+", names(kps.data))]
q.questiontext <- ses.get.questiontext(q)
plot(likert(q.questiontext), centered = FALSE)



# Load survey response and variable information
# ses.data <- ses.loaddatafile() # question codes, or...
# ses.data <- ses.get.questiontext() # question text
# ses.vars <- ses.loadvarfile()
=
# Data integrity checks
# tmp <- as.data.frame(apply(kps.numeric, 2, function(x) any(is.na(x))))
# tmp <- as.data.frame(apply(factor.scores, 2, function(x) any(is.na(x))))



library(mclust)
clusterdata <- factor.scores
bic <- mclustBIC(clusterdata)
# tmp <- as.data.frame(apply(clusterdata, 2, function(x) any(is.na(x))))

bic <- mclustBIC(clusterdata)
mod <- Mclust(clusterdata, x = bic)
summary(mod)
plot(mod)


data.num <- na.omit(data.num)
f.scores <- psych::factor.scores(x = data.num, f = fa.res)
f.scores <- f.scores$scores
scores <- f.scores
# scores <- f.scores[, c("PA1", "PA3")]
bic <- mclustBIC(na.omit(scores))
plot(bic)
mod <- Mclust(na.omit(scores), x = bic)
plot(mod, what = "classification")



library(mclust)
# pred <- lavPredict(cfa, append.data = TRUE)
pred <- lavPredict(cfa, transform = F)
tmp <- pred
# clusterdata <- pred[,c("consc", "altruism")]
pred <- as.data.frame(pred)
# clusterdata <- pred[,grepl( "hc|energy|light" , names( pred ) )]
clusterdata <- pred
bic <- mclustBIC(clusterdata)
mod <- Mclust(clusterdata, x = bic)
# mod <- Mclust(clusterdata, G = 4) # Set # of clusters
summary(mod)
plot(mod, what = "classification")
plot(mod, what = "uncertainty")
plot(mod, what = "density")

describe(pred)
corPlot(pred)

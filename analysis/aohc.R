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
library(caret)
library(plyr) # For mapvalues
library(ltm)
# CFA Libraries
library(lavaan)
library(lavaanPlot)
library(EFAtools)
library(regsem)
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

ses.data[order(ses.data$age),"age"]
max(ses.data$age)

#
## Likert Visualizations ----
#
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

# ~~~~~~~~~~~~~~~~~~~~~~ ----



# # # # # # # # # # # # # # # # # # # # #
# Experience Item Factor Analysis ----  
# # # # # # # # # # # # # # # # # # # # #

# In order to explore the underlying latent constructs of Higher Consciousness,
# exploratory factor analysis (EFA) was conducted first on the ungated survey items
# related to spiritual experiences (mystical, spiritual, and psychophysiological categories).
grepmatch = "mystical\\d+|spiritual\\d+|psyphys\\d+"
data.num <- extract.numeric.columns.by.regex(ses.data, grepmatch)

grepmatch = "mystical\\d+"
ses.qgroup("primary", grepmatch, parallel = T)

#
## Correlation matrices ----
#

grepmatch = "mystical\\d+|spiritual\\d+"
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
par.fa <- fa.parallel(x = data.num, cor = "poly", fa = "fa", fm = "pa", sim = TRUE, n.iter=20)
par.pc <- fa.parallel(x = data.num, cor = "poly", fa = "pc", n.iter=20)

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
# Factor loadings (ouput to CSV)
friendly.loadings <- ses.format.loadings(fa.res$loadings)
write.csv(friendly.loadings, file = paste("outputs/", "initial", "-loadings.csv", sep = ''))
# Factor correlations (ouput to CSV)
write.csv(fa.res$Phi, file = paste("outputs/", "initial", "-factorcorrelations.csv", sep = ''))

pca.res <- principal(r = data.num, nfactors = 4, cor = "poly")
pca.loadings <- as.data.frame.matrix(pca.res$loadings)

# McDonald's Omega(h and t)
# Revelle, W., & Condon, D. M. (2019). Reliability from α to ω: A tutorial. Psychological assessment, 31(12), 1395.
omega(m = data.num, nfactors = 8, fm="pa", rotate="promax", poly = TRUE)
fa.diagram(fa.res)


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

# grepmatch <- 'id|mystical\\d+|spiritual\\d+|psyphys\\d+|pe.gate|pe.negphysical.gate|sex'
# data.num <- extract.numeric.columns.by.regex(ses.data, grepmatch = grepmatch)
data.num <- ses.data[,grepl('id|mystical\\d+|spiritual\\d+|psyphys\\d+|pe.gate|pe.negphysical.gate|sex', names(ses.data))]

## All item model (baseline) ----
mod.all <- 'f =~ psyphys11 + psyphys1 + psyphys12 + psyphys2 + mystical13 + spiritual11 + mystical4 + psyphys8 + mystical12 + spiritual25 + spiritual12 + mystical9 + psyphys6 + mystical10 + spiritual15 + psyphys9 + mystical22 + mystical23 + psyphys3 + mystical26 + spiritual14 + spiritual13 + mystical5 + mystical24 + spiritual24 + mystical7 + spiritual18 + spiritual9 + mystical2 + mystical3 + spiritual22 + mystical25 + spiritual1 + spiritual6 + mystical6 + mystical15 + spiritual10 + spiritual20 + spiritual23 + mystical14 + mystical8 + spiritual27 + mystical11 + spiritual16 + mystical27 + spiritual2 + mystical17 + mystical18 + mystical21 + mystical1 + spiritual8 + psyphys5 + spiritual3 + spiritual21'
cfa.all <- cfa(mod.all, data=data.num, ordered = TRUE, std.lv = TRUE) # std.lv = fix factor variances
summary(cfa.all, fit.measures = TRUE, standardized = TRUE)

## Higher Consciousness ----
hc.mod <- NULL
# hc.mod <- paste0(hc.mod, "\n", 'allind =~ mystical6 + mystical2 + mystical25 + spiritual26 + mystical15 + mystical8 + mystical13 + mystical10 + mystical5 + mystical7 + mystical4 + spiritual3 + spiritual2 + psyphys5 + psyphys3 + psyphys9 + psyphys11 + psyphys1')
# hc.mod <- paste0(hc.mod, "\n", 'hc =~ mystical6 + mystical2 + mystical25 + spiritual26 + mystical15 + mystical8 + mystical13 + mystical10')
# hc.mod <- paste0(hc.mod, "\n", 'g =~ unityconsc + bliss + insight + energy + light')
hc.mod <- paste0(hc.mod, "\n", 'unityconsc =~ mystical6 + mystical2 + mystical25 + spiritual26 + mystical15 + mystical8 + mystical13 + mystical10')
# hc.mod <- paste0(hc.mod, "\n", 'consc =~ mystical6 + mystical2 + mystical25 + spiritual26 + mystical15')
# hc.mod <- paste0(hc.mod, "\n", 'unity =~ mystical8 + mystical13 + mystical10')
hc.mod <- paste0(hc.mod, "\n", 'bliss =~ mystical5 + mystical7 + mystical4')
hc.mod <- paste0(hc.mod, "\n", 'insight =~ spiritual3 + spiritual2')
hc.mod <- paste0(hc.mod, "\n", 'energy =~ psyphys5 + psyphys3 + psyphys9')
hc.mod <- paste0(hc.mod, "\n", 'light =~ psyphys11 + psyphys1')
hc.mod <- paste0(hc.mod, "\n")

# Data partitioning
table(data.num$sex)
# partition <- createDataPartition(data.num$sex, p = .5, list = F)
percent <- 1
partition <- sample(x = nrow(data.num), size = floor(percent*nrow(data.num)), replace = F)
length(partition)
table(data.num[partition,c("sex")])

# nrow(data.num)
# remove.intersex <- data.num[!(data.num$sex == 1),]
# nrow(remove.intersex)

# cfa <- cfa(hc.mod, data=data.num[partition,], ordered = T, estimator = "WLSMV") # WLSMV - Confirmatory Factor Analysis p. 354
cfa <- cfa(hc.mod, data=data.num[partition,], ordered = T, estimator = "WLSMV") # WLSMV - Confirmatory Factor Analysis p. 354
limit.output(summary(cfa, fit.measures = TRUE, standardized = TRUE), 150) # Fit indices only
summary(cfa, fit.measures = TRUE, standardized = TRUE, rsq = T)
lavaanPlot(model = cfa, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stand = TRUE)

pred <- lavPredict(cfa, transform = T)
nrow(pred)
# oneway.test(unityconsc ~ sex,
#     data.num = , # 
#     var.equal = FALSE # assuming equal variances
# )
sex.anova <- data.frame(data.num$sex, pred)

test <- aov(unityconsc ~ data.num.sex, data = sex.anova)
test <- aov(bliss ~ data.num.sex, data = sex.anova)
test <- aov(insight ~ data.num.sex, data = sex.anova)
test <- aov(energy ~ data.num.sex, data = sex.anova)
test <- aov(light ~ data.num.sex, data = sex.anova)
summary(test)


# Schmid-Leiman transformation for higher order factor
SL(cfa) # Must have higher order factor 'g', aka Higher Consciousness
EFAtools::OMEGA(cfa) # Must have higher order factor 'g', aka Higher Consciousness

# Bootstrap solution
boot <- bootstrapLavaan(cfa, R = 500, FUN=fitMeasures)
quantile(boot[,"rmsea.robust"], c(0.025, 0.975))
quantile(boot[,"cfi.robust"], c(0.025, 0.975))
quantile(boot[,"tli.robust"], c(0.025, 0.975))

# Local areas of strain
lavResiduals(cfa)
modindices(cfa, sort = TRUE)

# Covariance matrix
lavInspect(cfa, "cov.lv")
cov2cor(lavInspect(cfa, what = "est")$psi)

# Analyze markers
# barplot(table(data.num[,'psyphys3'])) # Visualization
# shapiro.test(data.num$spiritual3) # Test for normality
# describe(data.num[,'mystical5']) # Descriptive stats

# Primary experience MIMIC
# hc.mod <- paste0(hc.mod, "\n", 'bliss ~ hc')
# hc.mod <- paste0(hc.mod, "\n", 'insight ~ hc')
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


## Measurement invariance between ages and sexes ----

data.age <- extract.numeric.columns.by.regex(ses.data, 'mystical\\d+|spiritual\\d+|psyphys\\d+|sex|age')
age_breaks <- c(16, 35, 50, 100)
# age_labels <- c("0-35 years old", "35-50 years old", "51-105 years old")
# Create the new factor variable, then convert to numeric. 
data.age$age_group <- as.factor(cut(data.age$age, breaks = age_breaks, right = FALSE))
table(data.age$age_group)
data.age <- na.omit(data.age)
nrow(data.age)
fit1 <- cfa(hc.mod, data=data.age, group = "age_group", ordered = T)
summary(fit1, fit.measures = TRUE, standardized = TRUE)
fit2 <- cfa(hc.mod, data=data.age, group = "age_group", ordered = T, group.equal = "loadings")
summary(fit2, fit.measures = TRUE, standardized = TRUE)
fit3 <- cfa(hc.mod, data=data.age, group = "age_group", ordered = T, group.equal = c("intercepts", "loadings"))
summary(fit3, fit.measures = TRUE, standardized = TRUE)
lavTestLRT(fit1, fit2, fit3)


# set.seed(123)
# Separate the data based on the 'sex' variable
# group_1 <- data.num[data.num$sex == 1, ]
# group_2 <- data.num[data.num$sex == 2, ]

# Calculate the number of records in the smaller group
# min_records <- min(nrow(group_1), nrow(group_2))

# Sample equal number of records from each group
# sampled_group_1 <- group_1[sample(1:nrow(group_1), min_records), ]
# sampled_group_2 <- group_2[sample(1:nrow(group_2), min_records), ]

# Combine the two groups to create the final data frame
# remove.intersex <- rbind(sampled_group_1, sampled_group_2)

# Shuffle the rows of the final data frame
# remove.intersex <- equal_data[sample(1:nrow(equal_data)), ]


nrow(data.num)
remove.intersex[,"sex"] <- as.numeric(data.num[,"sex"])
remove.intersex <- remove.intersex[!(remove.intersex$sex == 3),] # Justification: Only one repondent indicated they are intersex
nrow(remove.intersex)
table(remove.intersex$sex)

fit1 <- cfa(hc.mod, data=remove.intersex, group = "sex", ordered = T)
summary(fit1, fit.measures = TRUE, standardized = TRUE)
fit2 <- cfa(hc.mod, data=remove.intersex, group = "sex", ordered = T, group.equal = "loadings")
summary(fit2, fit.measures = TRUE, standardized = TRUE)
fit3 <- cfa(hc.mod, data=remove.intersex, group = "sex", ordered = T, group.equal = c("intercepts", "loadings"))
summary(fit3, fit.measures = TRUE, standardized = TRUE)
lavTestLRT(fit1, fit2, fit3)



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
# # # # # # # #
# Talents  ----
# # # # # # # #

grepmatch = "talents\\d+"
talents.num <- extract.numeric.columns.by.regex(ses.data, grepmatch)
corPlot(talents.num)

ses.qgroup("talents", grepmatch, parallel = T, omit.na = F)

## Talents CFA Model ----

data.num <- extract.numeric.columns.by.regex(ses.data, 'mystical\\d+|spiritual\\d+|psyphys\\d+|psygrowth\\d+|talents\\d+')
data.num <- na.omit(data.num)

mod <- hc.mod

# Regularized MIMIC
reg <- ses.regsem("talents\\d+", hc.mod = hc.mod, data = data.num, n.lambda=20 , jump=0.01)
est <- as.data.frame(reg$reg.out$final_pars)

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



# ~~~~~~~~~~~~~~~~~~~~~~ ----
# # # # # # # # # # # # # # # # # # # #
# Negative Physical Factor Analysis ----
# # # # # # # # # # # # # # # # # # # #

grepmatch = "negphysical\\d+"
negphysical.num <- extract.numeric.columns.by.regex(ses.data, grepmatch)


#
## FA Reliability Tests (pre-FA) ----
#

# Kaiser-Meyer-Olkin measure of sampling adequacy
KMO(negphysical.num)[["MSA"]]

# Bartlett’s Test of Sphericity
bartlett.test(negphysical.num) # result is greater than the critical value for chi2

# Cronbach's Alpha
psych::alpha(negphysical.num)



#
## Select # of Factors ----
#

# Cattell's scree test
# Raymond B. Cattell (1966) The Scree Test For The Number Of Factors, Multivariate Behavioral Research, 1:2, 245-276, DOI: 10.1207/s15327906mbr0102_10
scree(negphysical.num)

# Horn’s parallel analysis
# Horn, J.L. A rationale and test for the number of factors in factor analysis. Psychometrika 30, 179–185 (1965). https://doi.org/10.1007/BF02289447
# Dinno, A. (2009). Exploring the sensitivity of Horn's parallel analysis to the distributional form of random data. Multivariate behavioral research, 44(3), 362-388.
fa.parallel(x = negphysical.num, cor = "poly", fa = "both", sim = TRUE, n.iter=20)
fa.parallel(x = negphysical.num, cor = "poly", fa = "pc", n.iter=20)

# Revelle’s Very Simple Structure
# Revelle, W., & Rocklin, T. (1979). Very simple structure: An alternative procedure for estimating the optimal number of interpretable factors. Multivariate behavioral research, 14(4), 403-414.
vss(negphysical.num)

## Experience Item ICLUST ----
# Revelle, W. (1978). ICLUST: A cluster analytic approach to exploratory and confirmatory scale construction. Behavior Research Methods & Instrumentation, 10(5), 739-742.
pchor <- polychoric(negphysical.num)
iclust <- iclust(pchor$rho, beta.min = 0.99, n.iterations = 20, purify = TRUE, nclusters = 8)
iclust <- iclust(negphysical.num)


#
## Factor Extraction ----
#
negphysical.fa.res <- fa(r = negphysical.num, nfactors = 5, fm = "pa", rotate = "promax", cor = "poly")
pca.res <- principal(r = negphysical.num, nfactors = 1, cor = "poly")
pca.loadings <- as.data.frame.matrix(pca.res$loadings)

# McDonald's Omega(h and t)
# Revelle, W., & Condon, D. M. (2019). Reliability from α to ω: A tutorial. Psychological assessment, 31(12), 1395.
omega(m = negphysical.num, nfactors = 7, fm="pa", rotate="promax", poly = TRUE)

fa.diagram(negphysical.fa.res)
omega(m = negphysical.num, 8)

# Factor loadings (ouput to CSV)
friendly.loadings <- ses.format.loadings(negphysical.fa.res$loadings)
write.csv(friendly.loadings, file = paste("outputs/", "negphysical", "-loadings.csv", sep = ''))

# Factor correlations (ouput to CSV)
write.csv(negphysical.fa.res$Phi, file = paste("outputs/", "negphysical", "-factorcorrelations.csv", sep = ''))


# # # # # # # # # # # # # # # # #
# Negative Physical CFA Model ----
# # # # # # # # # # # # # # # # #

data.num <- extract.numeric.columns.by.regex(ses.data, 'mystical\\d+|spiritual\\d+|psyphys\\d+|negphysical\\d+|negphysical\\d+')

## Negative Physical Model ----
mod <- hc.mod
# mod <- NULL # Start the model here - to check local fit, etc.
mod <- paste0(mod, "\n", 'negvisual =~ negphysical12 + negphysical11 + negphysical9')
mod <- paste0(mod, "\n", 'negbodypains =~ negphysical6 + negphysical7 + negphysical5')
mod <- paste0(mod, "\n", 'negheart =~ negphysical4 + negphysical3')

# MIMIC (formative)
mod <- paste0(mod, "\n", 'hc ~ negvisual')
mod <- paste0(mod, "\n", 'hc ~ negbodypains')
mod <- paste0(mod, "\n", 'hc ~ negheart') # *

# mod <- paste0(mod, "\n", 'hc ~ negphysical14')
# mod <- paste0(mod, "\n", 'hc ~ negphysical13')
# mod <- paste0(mod, "\n", 'hc ~ negphysical1')
# mod <- paste0(mod, "\n", 'hc ~ negphysical11')
# mod <- paste0(mod, "\n", 'hc ~ negphysical10')
# mod <- paste0(mod, "\n", 'hc ~ negphysical4')
# mod <- paste0(mod, "\n", 'hc ~ negphysical7')
# mod <- paste0(mod, "\n", 'hc ~ negphysical6')
# mod <- paste0(mod, "\n", 'hc ~ negphysical3')
# mod <- paste0(mod, "\n", 'hc ~ negphysical9')
# mod <- paste0(mod, "\n", 'hc ~ negphysical8')
# mod <- paste0(mod, "\n", 'hc ~ negphysical2')
# mod <- paste0(mod, "\n", 'hc ~ negphysical12')
# mod <- paste0(mod, "\n", 'hc ~ negphysical5')


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
# Growth and Bliss Factor Analysis ----
# # # # # # # # # # # # # # # # # # # # # #

grepmatch = "psybliss\\d+|psygrowth\\d+"
psygrowth.num <- extract.numeric.columns.by.regex(ses.data, grepmatch)
nrow(psygrowth.num)
psygrowth.num <- na.omit(psygrowth.num)
nrow(psygrowth.num)

## Likert viz ----
likert_viz(ses.data, "psygrowth\\d+", max_length = 40)

## Correlation matrix ----
grepmatch = "psygrowth"
corPlot(extract.numeric.columns.by.regex(ses.data, grepmatch))

#
## FA Reliability Tests (pre-FA) ----
#

# Kaiser-Meyer-Olkin measure of sampling adequacy
psych::KMO(psygrowth.num)[["MSA"]]

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
fa.par <- fa.parallel(x = psygrowth.num, cor = "poly", fa = "fa", fm = "pa", n.iter=20)
pc.par <- fa.parallel(x = psygrowth.num, cor = "poly", fa = "pc", n.iter=20)

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
data.num.fa.res <- fa(r = psygrowth.num, nfactors = 9, fm = "pa", rotate = "promax", cor = "poly")
# pca.res <- principal(r = psygrowth.num, nfactors = 4, cor = "poly")
pca.loadings <- as.data.frame.matrix(pca.res$loadings)

# McDonald's Omega(h and t)
# Revelle, W., & Condon, D. M. (2019). Reliability from α to ω: A tutorial. Psychological assessment, 31(12), 1395.
omega(m = psygrowth.num, nfactors = 7, fm="pa", rotate="promax", poly = TRUE)

fa.diagram(data.num.fa.res)
omega(m = data.num, 9)

# Factor loadings (ouput to CSV)
friendly.loadings <- ses.format.loadings(data.num.fa.res$loadings)
write.csv(friendly.loadings, file = paste("outputs/", "psyblissgrowth", "-loadings.csv", sep = ''))

# Factor correlations (ouput to CSV)
write.csv(data.num.fa.res$Phi, file = paste("outputs/", "psyblissgrowth", "-factorcorrelations.csv", sep = ''))


# # # # # # # # #
## CFA Model ----
# # # # # # # # #

data.num <- extract.numeric.columns.by.regex(ses.data, 'mystical\\d+|spiritual\\d+|psyphys\\d+|psygrowth\\d+|psybliss\\d+')
nrow(data.num)
data.num <- na.omit(data.num)
nrow(data.num)

# Psychological Growth Model

mod <- hc.mod # Start with the HC model
# mod <- NULL # Start the model here to check local fit, etc.
mod <- paste0(mod, "\n", 'altruism =~ psygrowth30 + psygrowth10 + psygrowth45')
mod <- paste0(mod, "\n", 'talent =~ psygrowth6 + psygrowth5 + psygrowth14')
mod <- paste0(mod, "\n", 'desire =~ psygrowth18 + psygrowth19 + psygrowth20 + psygrowth15 + psygrowth17')
mod <- paste0(mod, "\n", 'physcare =~ psygrowth24 + psygrowth22 + psygrowth23')
mod <- paste0(mod, "\n", 'healing =~ psygrowth43 + psygrowth42 + psygrowth36')
mod <- paste0(mod, "\n", 'concen =~ psygrowth41 + psygrowth39 + psygrowth29 + psygrowth37')
mod <- paste0(mod, "\n", 'empty =~ psygrowth3 + psygrowth2')

# MIMIC (formative)
mod <- paste0(mod, "\n", 'g ~ altruism')
mod <- paste0(mod, "\n", 'g ~ talent')
mod <- paste0(mod, "\n", 'g ~ concen')
mod <- paste0(mod, "\n", 'g ~ desire')
mod <- paste0(mod, "\n", 'g ~ physcare')
mod <- paste0(mod, "\n", 'g ~ healing')
mod <- paste0(mod, "\n", 'g ~ empty')

#
### Non-regularized SEM regression ----
#
non.reg <- paste0(hc.mod, "\n", 'g =~ unityconsc + bliss + insight + energy + light')
cfa <- cfa(non.reg, data=data.num, ordered = TRUE) # Fix factor variances with std.lv = TRUE
# limit.output(summary(cfa, fit.measures = TRUE, standardized = TRUE), 150) # Fit indices only
summary(cfa, fit.measures = TRUE, standardized = TRUE)

modindices(cfa, sort = TRUE)
# round(cor(data.num[,c("mystical2", "mystical3")]), 2)
lavaanPlot(model = cfa, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stand = TRUE)
lavInspect(cfa, "cov.lv")
cov2cor(lavInspect(cfa, what = "est")$psi)

#
### Regularized SEM regression ----
#
mod <- hc.mod # Start with the HC model
all.vars <- paste('g ~', colnames(data.num[,grepl("psybliss\\d+|psygrowth\\d+", names(data.num))]),collapse="\n")
mod <- paste(hc.mod, all.vars)
reg.mod <- mod
reg.mod <- paste0(reg.mod, "\n", 'g =~ NA*unityconsc + bliss + insight + energy + light')
reg.mod <- paste0(reg.mod, "\n", 'g ~~ 1*g')
cfa <- cfa(reg.mod, data=data.num)
summary(cfa, fit.measures = TRUE, standardized = TRUE)
reg.out <- cv_regsem(cfa,n.lambda=5,jump=0.2,type="lasso",pars_pen="regressions")
plot(reg.out)
summary(reg.out)
tmp <- as.data.frame(reg.out$final_pars)





# ~~~~~~~~~~~~~~~~~~~~~~ ----
# # # # # # # # # # # # # # # # # # # # # #
# Psychological Growth Factor Analysis ----
# # # # # # # # # # # # # # # # # # # # # #

grepmatch = "d+|psygrowth\\d+"
psygrowth.num <- extract.numeric.columns.by.regex(ses.data, grepmatch)
nrow(psygrowth.num)
psygrowth.num <- na.omit(psygrowth.num)
nrow(psygrowth.num)

## Likert viz ----
likert_viz(ses.data, "psygrowth\\d+", max_length = 40)

## Correlation matrix ----
grepmatch = "psygrowth"
corPlot(extract.numeric.columns.by.regex(ses.data, grepmatch))

#
## FA Reliability Tests (pre-FA) ----
#

# Kaiser-Meyer-Olkin measure of sampling adequacy
psych::KMO(psygrowth.num)[["MSA"]]

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
fa.par <- fa.parallel(x = psygrowth.num, cor = "poly", fa = "fa", fm = "pa", n.iter=20)
pc.par <- fa.parallel(x = psygrowth.num, cor = "poly", fa = "pc", n.iter=20)

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


# # # # # # # # #
## CFA Model ----
# # # # # # # # #

data.num <- extract.numeric.columns.by.regex(ses.data, 'mystical\\d+|spiritual\\d+|psyphys\\d+|psygrowth\\d+|sex')

# Psychological Growth Model

mod <- hc.mod # Start with the HC model
# mod <- NULL # Start the model here to check local fit, etc.
mod <- paste0(mod, "\n", 'altruism =~ psygrowth30 + psygrowth10 + psygrowth45')
mod <- paste0(mod, "\n", 'talent =~ psygrowth6 + psygrowth5 + psygrowth14')
mod <- paste0(mod, "\n", 'desire =~ psygrowth18 + psygrowth19 + psygrowth20 + psygrowth15 + psygrowth17')
mod <- paste0(mod, "\n", 'physcare =~ psygrowth24 + psygrowth22 + psygrowth23')
mod <- paste0(mod, "\n", 'healing =~ psygrowth43 + psygrowth42 + psygrowth36')
mod <- paste0(mod, "\n", 'concen =~ psygrowth41 + psygrowth39 + psygrowth29 + psygrowth37')
mod <- paste0(mod, "\n", 'empty =~ psygrowth3 + psygrowth2')

# MIMIC (formative)
mod <- paste0(mod, "\n", 'g ~ altruism')
mod <- paste0(mod, "\n", 'g ~ talent')
mod <- paste0(mod, "\n", 'g ~ concen')
mod <- paste0(mod, "\n", 'g ~ desire')
mod <- paste0(mod, "\n", 'g ~ physcare')
mod <- paste0(mod, "\n", 'g ~ healing')
mod <- paste0(mod, "\n", 'g ~ empty')

# Regularized MIMIC
mod <- paste0(mod, "\n", 'g ~ psygrowth1')
mod <- paste0(mod, "\n", 'g ~ psygrowth2')
mod <- paste0(mod, "\n", 'g ~ psygrowth3')
mod <- paste0(mod, "\n", 'g ~ psygrowth4')
mod <- paste0(mod, "\n", 'g ~ psygrowth5')
mod <- paste0(mod, "\n", 'g ~ psygrowth6')
mod <- paste0(mod, "\n", 'g ~ psygrowth7')
mod <- paste0(mod, "\n", 'g ~ psygrowth8')
mod <- paste0(mod, "\n", 'g ~ psygrowth9')
mod <- paste0(mod, "\n", 'g ~ psygrowth10')
mod <- paste0(mod, "\n", 'g ~ psygrowth11')
mod <- paste0(mod, "\n", 'g ~ psygrowth12')
mod <- paste0(mod, "\n", 'g ~ psygrowth13')
mod <- paste0(mod, "\n", 'g ~ psygrowth14')
mod <- paste0(mod, "\n", 'g ~ psygrowth15')
mod <- paste0(mod, "\n", 'g ~ psygrowth16')
mod <- paste0(mod, "\n", 'g ~ psygrowth17')
mod <- paste0(mod, "\n", 'g ~ psygrowth18')
mod <- paste0(mod, "\n", 'g ~ psygrowth19')
mod <- paste0(mod, "\n", 'g ~ psygrowth20')
mod <- paste0(mod, "\n", 'g ~ psygrowth21')
mod <- paste0(mod, "\n", 'g ~ psygrowth22')
mod <- paste0(mod, "\n", 'g ~ psygrowth23')
mod <- paste0(mod, "\n", 'g ~ psygrowth24')
mod <- paste0(mod, "\n", 'g ~ psygrowth25')
mod <- paste0(mod, "\n", 'g ~ psygrowth26')
mod <- paste0(mod, "\n", 'g ~ psygrowth27')
mod <- paste0(mod, "\n", 'g ~ psygrowth28')
mod <- paste0(mod, "\n", 'g ~ psygrowth29')
mod <- paste0(mod, "\n", 'g ~ psygrowth30')
mod <- paste0(mod, "\n", 'g ~ psygrowth31')
mod <- paste0(mod, "\n", 'g ~ psygrowth32')
mod <- paste0(mod, "\n", 'g ~ psygrowth33')
mod <- paste0(mod, "\n", 'g ~ psygrowth34')
mod <- paste0(mod, "\n", 'g ~ psygrowth35')
mod <- paste0(mod, "\n", 'g ~ psygrowth36')
mod <- paste0(mod, "\n", 'g ~ psygrowth37')
mod <- paste0(mod, "\n", 'g ~ psygrowth38')
mod <- paste0(mod, "\n", 'g ~ psygrowth39')
mod <- paste0(mod, "\n", 'g ~ psygrowth40')
mod <- paste0(mod, "\n", 'g ~ psygrowth41')
mod <- paste0(mod, "\n", 'g ~ psygrowth42')
mod <- paste0(mod, "\n", 'g ~ psygrowth43')
mod <- paste0(mod, "\n", 'g ~ psygrowth44')
mod <- paste0(mod, "\n", 'g ~ psygrowth45')
mod <- paste0(mod, "\n", 'g ~ psygrowth46')
mod <- paste0(mod, "\n", 'g ~ psygrowth47')

#
### Non-regularized SEM regression ----
#
non.reg <- paste0(hc.mod, "\n", 'g =~ unityconsc + bliss + insight + energy + light')
cfa <- cfa(non.reg, data=data.num, ordered = TRUE) # Fix factor variances with std.lv = TRUE
# limit.output(summary(cfa, fit.measures = TRUE, standardized = TRUE), 150) # Fit indices only
summary(cfa, fit.measures = TRUE, standardized = TRUE)

modindices(cfa, sort = TRUE)
# round(cor(data.num[,c("mystical2", "mystical3")]), 2)
lavaanPlot(model = cfa, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stand = TRUE)
lavInspect(cfa, "cov.lv")
cov2cor(lavInspect(cfa, what = "est")$psi)

#
### Regularized SEM regression ----
#
reg.mod <- paste0(mod, "\n", 'g =~ NA*unityconsc + bliss + insight + energy + light')
reg.mod <- paste0(mod, "\n", 'g ~~ 1*g')
cfa <- cfa(reg.mod, data=data.num)
reg.out <- cv_regsem(cfa,n.lambda=5,jump=0.2,type="lasso",pars_pen="regressions")
plot(reg.out)
summary(reg.out)
tmp <- as.data.frame(reg.out$final_pars)



# ~~~~~~~~~~~~~~~~~~~~~~ ----
# # # # # # # # # # # # # # # # # # # # # # *
# Negative Psychological Factor Analysis ----
# # # # # # # # # # # # # # # # # # # # # #

grepmatch = "negpsych\\d+"
negpsych.num <- extract.numeric.columns.by.regex(ses.data, grepmatch)
corPlot(negpsych.num)
likert(ses.data, "negpsych\\d+")

#
## FA Reliability Tests (pre-FA) ----
#

# Kaiser-Meyer-Olkin measure of sampling adequacy
KMO(negpsych.num)[["MSA"]]

# Bartlett’s Test of Sphericity
bartlett.test(negpsych.num) # result is greater than the critical value for chi2

# Cronbach's Alpha
psych::alpha(negpsych.num)



#
## Select # of Factors ----
#

# Cattell's scree test
# Raymond B. Cattell (1966) The Scree Test For The Number Of Factors, Multivariate Behavioral Research, 1:2, 245-276, DOI: 10.1207/s15327906mbr0102_10
scree(negpsych.num)

# Horn’s parallel analysis
# Horn, J.L. A rationale and test for the number of factors in factor analysis. Psychometrika 30, 179–185 (1965). https://doi.org/10.1007/BF02289447
# Dinno, A. (2009). Exploring the sensitivity of Horn's parallel analysis to the distributional form of random data. Multivariate behavioral research, 44(3), 362-388.
fa.parallel(x = negpsych.num, cor = "poly", fa = "both", sim = TRUE, n.iter=20)
par_result <- fa.parallel(x = negpsych.num, cor = "poly", fa = "pc", n.iter=20)

# Revelle’s Very Simple Structure
# Revelle, W., & Rocklin, T. (1979). Very simple structure: An alternative procedure for estimating the optimal number of interpretable factors. Multivariate behavioral research, 14(4), 403-414.
vss(negpsych.num)

## Experience Item ICLUST ----
# Revelle, W. (1978). ICLUST: A cluster analytic approach to exploratory and confirmatory scale construction. Behavior Research Methods & Instrumentation, 10(5), 739-742.
pchor <- polychoric(negpsych.num)
iclust <- iclust(pchor$rho, beta.min = 0.99, n.iterations = 20, purify = TRUE, nclusters = 8)
iclust <- iclust(negpsych.num)


#
## Factor Extraction ----
#
negpsych.fa.res <- fa(r = negpsych.num, nfactors = 6, fm = "pa", rotate = "promax", cor = "poly")
pca.res <- principal(r = negpsych.num, nfactors = 1, cor = "poly")
pca.loadings <- as.data.frame.matrix(pca.res$loadings)

# McDonald's Omega(h and t)
# Revelle, W., & Condon, D. M. (2019). Reliability from α to ω: A tutorial. Psychological assessment, 31(12), 1395.
omega(m = negpsych.num, nfactors = 7, fm="pa", rotate="promax", poly = TRUE)

fa.diagram(negpsych.fa.res)
omega(m = negpsych.num, 8)

# Factor loadings (ouput to CSV)
friendly.loadings <- ses.format.loadings(negpsych.fa.res$loadings)
write.csv(friendly.loadings, file = paste("outputs/", "negpsych", "-loadings.csv", sep = ''))

# Factor correlations (ouput to CSV)
write.csv(negpsych.fa.res$Phi, file = paste("outputs/", "negpsych", "-factorcorrelations.csv", sep = ''))


# # # # # # # # # # # # # # # # # # #
# Negative Psychological CFA Model ----
# # # # # # # # # # # # # # # # # # #

data.num <- extract.numeric.columns.by.regex(ses.data, 'mystical\\d+|spiritual\\d+|psyphys\\d+|negpsych\\d+')

## Negative Psychological Model ----
mod <- hc.mod
# mod <- NULL # Start the model here - to check local fit, etc.
# mod <- paste0(mod, "\n", 'instability =~ negpsych1 + negpsych16 + negpsych15 + negpsych17 + negpsych19 + negpsych20')
# mod <- paste0(mod, "\n", 'detach =~ negpsych29 + negpsych30')
# mod <- paste0(mod, "\n", 'panic =~ negpsych3 + negpsych4 + negpsych2')

# MIMIC (formative)
# mod <- paste0(mod, "\n", 'hc ~ instability + detach + panic')

mod <- paste0(mod, "\n", 'hc ~ negpsych1')
mod <- paste0(mod, "\n", 'hc ~ negpsych3')
mod <- paste0(mod, "\n", 'hc ~ negpsych4')
mod <- paste0(mod, "\n", 'hc ~ negpsych2')
mod <- paste0(mod, "\n", 'hc ~ negpsych12')
mod <- paste0(mod, "\n", 'hc ~ negpsych7')
mod <- paste0(mod, "\n", 'hc ~ negpsych14')
mod <- paste0(mod, "\n", 'hc ~ negpsych13')
mod <- paste0(mod, "\n", 'hc ~ negpsych6')
mod <- paste0(mod, "\n", 'hc ~ negpsych5')
mod <- paste0(mod, "\n", 'hc ~ negpsych21')
mod <- paste0(mod, "\n", 'hc ~ negpsych11')
mod <- paste0(mod, "\n", 'hc ~ negpsych31')
mod <- paste0(mod, "\n", 'hc ~ negpsych19')
mod <- paste0(mod, "\n", 'hc ~ negpsych28')
mod <- paste0(mod, "\n", 'hc ~ negpsych26')
mod <- paste0(mod, "\n", 'hc ~ negpsych24')
mod <- paste0(mod, "\n", 'hc ~ negpsych22')
mod <- paste0(mod, "\n", 'hc ~ negpsych32')
mod <- paste0(mod, "\n", 'hc ~ negpsych10')
mod <- paste0(mod, "\n", 'hc ~ negpsych25')
mod <- paste0(mod, "\n", 'hc ~ negpsych20')
mod <- paste0(mod, "\n", 'hc ~ negpsych35')
mod <- paste0(mod, "\n", 'hc ~ negpsych33')
mod <- paste0(mod, "\n", 'hc ~ negpsych23')
mod <- paste0(mod, "\n", 'hc ~ negpsych18')
mod <- paste0(mod, "\n", 'hc ~ negpsych9')
mod <- paste0(mod, "\n", 'hc ~ negpsych8')
mod <- paste0(mod, "\n", 'hc ~ negpsych34')
mod <- paste0(mod, "\n", 'hc ~ negpsych15')
mod <- paste0(mod, "\n", 'hc ~ negpsych27')
mod <- paste0(mod, "\n", 'hc ~ negpsych30')
mod <- paste0(mod, "\n", 'hc ~ negpsych16')
mod <- paste0(mod, "\n", 'hc ~ negpsych29')
mod <- paste0(mod, "\n", 'hc ~ negpsych17')

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
# mod <- paste0(mod, "\n", 'oneness =~ psybliss21 + psybliss18 + psybliss22 + psybliss23 + psybliss19')
mod <- paste0(mod, "\n", 'virtue =~ psybliss20 + psybliss23 + psybliss27 + psybliss19 + psybliss26 + psybliss28 + psybliss21 + psybliss29 + psybliss15 + psybliss22 + psybliss18 + psybliss10 + psybliss14')
mod <- paste0(mod, "\n", 'virtue =~ psybliss23 + psybliss21 + psybliss18 + psybliss17')
mod <- paste0(mod, "\n", 'g ~ virtue')

# mod <- paste0(mod, "\n", 'creative =~ psybliss6 + psybliss24 + psybliss16')
# mod <- paste0(mod, "\n", 'peaks =~ psybliss4 + psybliss3 + psybliss5')
# mod <- paste0(mod, "\n", 'overcome =~ psybliss10 + psybliss9 + psybliss29')



# MIMIC (formative)
mod <- paste0(mod, "\n", 'g ~ oneness')
mod <- paste0(mod, "\n", 'g ~ creative')
mod <- paste0(mod, "\n", 'g ~ peaks')
mod <- paste0(mod, "\n", 'g ~ overcome')

non.reg <- paste0(mod, "\n", 'g =~ unityconsc + bliss + insight + energy + light')
cfa <- cfa(non.reg, data=data.num, ordered = TRUE) # Fix factor variances with std.lv = TRUE
limit.output(summary(cfa, fit.measures = TRUE, standardized = TRUE), 140)
# summary(cfa, fit.measures = TRUE, standardized = TRUE)



modindices(cfa, sort = TRUE)
# round(cor(data.num[,c("mystical2", "mystical3")]), 2)
lavaanPlot(model = cfa, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stand = TRUE)
lavInspect(cfa, "cov.lv")
cov2cor(lavInspect(cfa, what = "est")$psi)

mod <- hc.mod # Start with the HC model
all.vars <- paste('g ~', colnames(data.num[,grepl("psybliss\\d+", names(data.num))]),collapse="\n")
mod <- paste(mod, all.vars)
reg.mod <- mod
reg.mod <- paste0(reg.mod, "\n", 'g =~ NA*unityconsc + bliss + insight + energy + light')
reg.mod <- paste0(reg.mod, "\n", 'g ~~ 1*g')
cfa <- cfa(reg.mod, data=data.num)
summary(cfa, fit.measures = TRUE, standardized = TRUE)
reg.out <- cv_regsem(cfa,n.lambda=5,jump=0.2,type="enet",pars_pen="regressions")
plot(reg.out)
summary(reg.out)
tmp <- as.data.frame(reg.out$final_pars)








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

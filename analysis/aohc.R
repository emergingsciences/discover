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

source("code/ses-utility.R")


# 
# Full Data Load (respondent data and variable mappings) ----
#


ses.data <- ses.loaddatafile()
ses.vars <- ses.loadvarfile()


# TODO: Data Summary and Cleanup ----



# # # # # # # # # # # # # # # # # # # # #
# Experience Item Factor Analysis ----  
# # # # # # # # # # # # # # # # # # # # #

# In order to explore the underlying latent constructs of Higher Consciousness,
# exploratory factor analysis (EFA) was conducted first on the ungated survey items
# related to spiritual experiences (mystical, spiritual, and psychophysiological categories).
grepmatch = "mystical\\d+|spiritual\\d+|psyphys\\d+"
data.num <- extract.numeric.columns.by.regex(ses.data, grepmatch)


#
## FA Reliability Tests (pre-FA) ----
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

library(lavaan)
library(lavaanPlot)

data.num <- extract.numeric.columns.by.regex(ses.data, 'mystical\\d+|spiritual\\d+|psyphys\\d+')
data.num <- na.omit(data.num)
# data.num[] <- lapply(data.num, as.factor)

## All item model ----
mod.all <- 'f =~ psyphys11 + psyphys1 + psyphys12 + psyphys2 + mystical13 + spiritual11 + mystical4 + psyphys8 + mystical12 + spiritual25 + spiritual12 + mystical9 + psyphys6 + mystical10 + spiritual15 + psyphys9 + mystical22 + mystical23 + psyphys3 + mystical26 + spiritual14 + spiritual13 + mystical5 + mystical24 + spiritual24 + mystical7 + spiritual18 + spiritual9 + mystical2 + mystical3 + spiritual22 + mystical25 + spiritual1 + spiritual6 + mystical6 + mystical15 + spiritual10 + spiritual20 + spiritual23 + mystical14 + mystical8 + spiritual27 + mystical11 + spiritual16 + mystical27 + spiritual2 + mystical17 + mystical18 + mystical21 + mystical1 + spiritual8 + psyphys5 + spiritual3 + spiritual21'
cfa.all <- cfa(mod.all, data=data.num, std.lv = TRUE) # std.lv = fix factor variances
summary(cfa.all, fit.measures = TRUE, standardized = TRUE)

## CFA model ----
mod <- NULL
# mod <- paste0(mod, "\n", 'hc =~ mystical22 + mystical24 + mystical2 + mystical25 + mystical6 + mystical13 + mystical12 + mystical10 + mystical23 + mystical26 + mystical3 + mystical14 + mystical4 + mystical5 + mystical7 + mystical9')
mod <- paste0(mod, "\n", 'consc =~ mystical22 + mystical24 + mystical2 + mystical25 + mystical6')
mod <- paste0(mod, "\n", "mystical12 ~~ mystical13") # DUPLICATE: Experience of deep unity and expansive consciousness / All sense of separateness disappears
mod <- paste0(mod, "\n", "mystical22 ~~ mystical2") # DUPLICATE: Expansion of consciousness and Expansion / explosion of consciousness are highly correlated
mod <- paste0(mod, "\n", 'unity =~ mystical13 + mystical12 + mystical10 + mystical23 + mystical26 + mystical3 + mystical14')
mod <- paste0(mod, "\n", 'bliss =~ mystical4 + mystical5 + mystical7 + mystical9')
mod <- paste0(mod, "\n", 'rebirth =~ spiritual1 + spiritual2 + spiritual3')
mod <- paste0(mod, "\n", 'insight =~ spiritual21 + spiritual22 + spiritual26 + spiritual27 + spiritual8 + spiritual10 + spiritual12')
# cfa <- cfa(mod, data=data.num, ordered = TRUE, std.lv = TRUE)
# summary(cfa, fit.measures = TRUE, standardized = TRUE)

## Structural model ----

# mod <- paste0(mod, "\n", 'consc ~ bliss + unity')
# mod <- paste0(mod, "\n", 'bliss ~ unity + consc')
mod <- paste0(mod, "\n", 'unity ~ consc + bliss') # Best

# BCU Relationship 
# mod <- paste0(mod, "\n", 'consc ~ bliss')
# mod <- paste0(mod, "\n", 'unity ~ consc')
# mod <- paste0(mod, "\n", 'unity ~ bliss')

# CBU Relationship
# mod <- paste0(mod, "\n", 'bliss ~ consc')
# mod <- paste0(mod, "\n", 'unity ~ consc')
# mod <- paste0(mod, "\n", 'unity ~ bliss')

# Meditation (C -> B -> U)
# mod <- paste0(mod, "\n", 'unity ~ c*consc') # Direct effect
# mod <- paste0(mod, "\n", 'bliss ~ a*consc') # mediator
# mod <- paste0(mod, "\n", 'unity ~ b*bliss') # mediator
# mod <- paste0(mod, "\n", 'ab := a*b') # indirect effect
# mod <- paste0(mod, "\n", 'total := c + (a*b)') # total effect

# Meditation (B -> C -> U)
# mod <- paste0(mod, "\n", 'unity ~ c*bliss') # Direct effect
# mod <- paste0(mod, "\n", 'consc ~ a*bliss') # mediator
# mod <- paste0(mod, "\n", 'unity ~ b*consc') # mediator
# mod <- paste0(mod, "\n", 'ab := a*b') # indirect effect
# mod <- paste0(mod, "\n", 'total := c + (a*b)') # total effect

cfa <- cfa(mod, data=data.num, ordered = TRUE, std.lv = TRUE)
summary(cfa, fit.measures = TRUE, standardized = TRUE)

## Bifactor model (does not converge) ----
mod <- NULL
# mod <- paste0(mod, "\n", 'hc =~ mystical22 + mystical24 + mystical2 + mystical25 + mystical6 + mystical13 + mystical12 + mystical10 + mystical23 + mystical26 + mystical3 + mystical14 + mystical4 + mystical5 + mystical7 + mystical9')
mod <- paste0(mod, "\n", 'hc =~ mystical22 + 1*mystical24 + 1*mystical2 + 1*mystical25 + 1*mystical6 + 1*mystical13 + 1*mystical12 + 1*mystical10 + 1*mystical23 + 1*mystical26 + 1*mystical3 + 1*mystical14 + 1*mystical4 + 1*mystical5 + 1*mystical7 + 1*mystical9')
# mod <- paste0(mod, "\n", 'consc =~ mystical22 + mystical24 + mystical2 + mystical25 + mystical6')
# mod <- paste0(mod, "\n", 'unity =~ mystical13 + mystical12 + mystical10 + mystical23 + mystical26 + mystical3 + mystical14')
# mod <- paste0(mod, "\n", 'bliss =~ mystical4 + mystical5 + mystical7 + mystical9')
mod <- paste0(mod, "\n", 'consc =~ mystical22 + 1*mystical24 + 1*mystical2 + 1*mystical25 + 1*mystical6')
mod <- paste0(mod, "\n", 'unity =~ mystical13 + 1*mystical12 + 1*mystical10 + 1*mystical23 + 1*mystical26 + 1*mystical3 + 1*mystical14')
mod <- paste0(mod, "\n", 'bliss =~ mystical4 + 1*mystical5 + 1*mystical7 + 1*mystical9')
mod <- paste0(mod, "\n", "mystical12 ~~ mystical13") # DUPLICATE: Experience of deep unity and expansive consciousness / All sense of separateness disappears
mod <- paste0(mod, "\n", "mystical22 ~~ mystical2") # DUPLICATE: Expansion of consciousness and Expansion / explosion of consciousness are highly correlated
mod <- paste0(mod, "\n", 'rebirth =~ spiritual1 + spiritual2 + spiritual3')
mod <- paste0(mod, "\n", 'insight =~ spiritual21 + spiritual22 + spiritual26 + spiritual27 + spiritual8 + spiritual10 + spiritual12')
mod <- paste0(mod, "\n", 'consc ~~ 0*unity + bliss')
mod <- paste0(mod, "\n", 'bliss ~~ 0*unity')
cfa <- cfa(mod, data=data.num, ordered = TRUE) # std.lv = TRUE (omitted)
summary(cfa, fit.measures = TRUE, standardized = TRUE)

cfa <- cfa(mod, data=data.num, ordered = TRUE, std.lv = TRUE) # Fix factor variances with std.lv = TRUE
summary(cfa, fit.measures = TRUE, standardized = TRUE)
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

library(lavaan)
library(lavaanPlot)

data.num <- extract.numeric.columns.by.regex(ses.data, 'mystical\\d+|spiritual\\d+|psyphys\\d+|psygrowth\\d+|talents\\d+')
data.num <- na.omit(data.num)

## All item model ----
mod.all <- 'f =~ psyphys11 + psyphys1 + psyphys12 + psyphys2 + mystical13 + spiritual11 + mystical4 + psyphys8 + mystical12 + spiritual25 + spiritual12 + mystical9 + psyphys6 + mystical10 + spiritual15 + psyphys9 + mystical22 + mystical23 + psyphys3 + mystical26 + spiritual14 + spiritual13 + mystical5 + mystical24 + spiritual24 + mystical7 + spiritual18 + spiritual9 + mystical2 + mystical3 + spiritual22 + mystical25 + spiritual1 + spiritual6 + mystical6 + mystical15 + spiritual10 + spiritual20 + spiritual23 + mystical14 + mystical8 + spiritual27 + mystical11 + spiritual16 + mystical27 + spiritual2 + mystical17 + mystical18 + mystical21 + mystical1 + spiritual8 + psyphys5 + spiritual3 + spiritual21'
cfa.all <- cfa(mod.all, data=data.num, std.lv = TRUE) # std.lv = fix factor variances
summary(cfa.all, fit.measures = TRUE, standardized = TRUE)

## Refined experience model ----
mod <- NULL
mod <- paste0(mod, "\n", 'hc =~ mystical22 + mystical24 + mystical2 + mystical25 + mystical6 + mystical13 + mystical12 + mystical10 + mystical23 + mystical26 + mystical3 + mystical14 + mystical4 + mystical5 + mystical7 + mystical9')
# mod <- paste0(mod, "\n", 'consc =~ mystical22 + mystical24 + mystical2 + mystical25 + mystical6')
# mod <- paste0(mod, "\n", 'unity =~ mystical13 + mystical12 + mystical10 + mystical23 + mystical26 + mystical3 + mystical14')
# mod <- paste0(mod, "\n", 'bliss =~ mystical4 + mystical5 + mystical7 + mystical9')
mod <- paste0(mod, "\n", "mystical12 ~~ mystical13") # DUPLICATE: Experience of deep unity and expansive consciousness / All sense of separateness disappears
mod <- paste0(mod, "\n", "mystical22 ~~ mystical2") # DUPLICATE: Expansion of consciousness and Expansion / explosion of consciousness are highly correlated
mod <- paste0(mod, "\n", "mystical4 ~~ mystical9") # DUPLICATE: Intense feeling of peace / Overwhelming sense of bliss, joy and or contentment

# mod <- paste0(mod, "\n", 'rebirth =~ spiritual1 + spiritual2 + spiritual3')
# mod <- paste0(mod, "\n", 'insight =~ spiritual21 + spiritual22 + spiritual26 + spiritual27 + spiritual8 + spiritual10 + spiritual12')

# Talents path model
mod <- paste0(mod, "\n", 'hc ~ talents1')
mod <- paste0(mod, "\n", 'hc ~ talents2')
mod <- paste0(mod, "\n", 'hc ~ talents3')
mod <- paste0(mod, "\n", 'hc ~ talents4')
mod <- paste0(mod, "\n", 'hc ~ talents5')
mod <- paste0(mod, "\n", 'hc ~ talents6')
mod <- paste0(mod, "\n", 'hc ~ talents7')
mod <- paste0(mod, "\n", 'hc ~ talents8')

# Reverse model
# mod <- paste0(mod, "\n", 'talents1 ~ hc')
# mod <- paste0(mod, "\n", 'talents2 ~ hc')
# mod <- paste0(mod, "\n", 'talents3 ~ hc')
# mod <- paste0(mod, "\n", 'talents4 ~ hc')
# mod <- paste0(mod, "\n", 'talents5 ~ hc')
# mod <- paste0(mod, "\n", 'talents6 ~ hc')
# mod <- paste0(mod, "\n", 'talents7 ~ hc')
# mod <- paste0(mod, "\n", 'talents8 ~ hc')

# Structural model
# mod <- paste0(mod, "\n", '')

cfa <- cfa(mod, data=data.num, ordered = TRUE, std.lv = TRUE) # Fix factor variances
summary(cfa, fit.measures = TRUE, standardized = TRUE)

modindices(cfa, sort = TRUE)
# round(cor(data.num[,c("mystical2", "mystical3")]), 2)
lavaanPlot(model = cfa, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stand = TRUE)
lavInspect(cfa, "cov.lv")
cov2cor(lavInspect(cfa, what = "est")$psi)



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

library(lavaan)
library(lavaanPlot)

data.num <- extract.numeric.columns.by.regex(ses.data, 'mystical\\d+|spiritual\\d+|psyphys\\d+|psygrowth\\d+')
data.num <- na.omit(data.num)

## All item model ----
# TODO
# mod.all <- 'f =~ psyphys11 + psyphys1 + psyphys12 + psyphys2 + mystical13 + spiritual11 + mystical4 + psyphys8 + mystical12 + spiritual25 + spiritual12 + mystical9 + psyphys6 + mystical10 + spiritual15 + psyphys9 + mystical22 + mystical23 + psyphys3 + mystical26 + spiritual14 + spiritual13 + mystical5 + mystical24 + spiritual24 + mystical7 + spiritual18 + spiritual9 + mystical2 + mystical3 + spiritual22 + mystical25 + spiritual1 + spiritual6 + mystical6 + mystical15 + spiritual10 + spiritual20 + spiritual23 + mystical14 + mystical8 + spiritual27 + mystical11 + spiritual16 + mystical27 + spiritual2 + mystical17 + mystical18 + mystical21 + mystical1 + spiritual8 + psyphys5 + spiritual3 + spiritual21'
# cfa.all <- cfa(mod.all, data=data.num, std.lv = TRUE) # std.lv = fix factor variances
# summary(cfa.all, fit.measures = TRUE, standardized = TRUE)

## Psychological Growth Model ----
mod <- NULL
mod <- paste0(mod, "\n", 'hc =~ mystical22 + mystical24 + mystical2 + mystical25 + mystical6 + mystical13 + mystical12 + mystical10 + mystical23 + mystical26 + mystical3 + mystical14 + mystical4 + mystical5 + mystical7 + mystical9')
# mod <- paste0(mod, "\n", 'consc =~ mystical22 + mystical24 + mystical2 + mystical25 + mystical6')
# mod <- paste0(mod, "\n", 'unity =~ mystical13 + mystical12 + mystical10 + mystical23 + mystical26 + mystical3 + mystical14')
# mod <- paste0(mod, "\n", 'bliss =~ mystical4 + mystical5 + mystical7 + mystical9')
mod <- paste0(mod, "\n", "mystical12 ~~ mystical13") # DUPLICATE: Experience of deep unity and expansive consciousness / All sense of separateness disappears
mod <- paste0(mod, "\n", "mystical22 ~~ mystical2") # DUPLICATE: Expansion of consciousness and Expansion / explosion of consciousness are highly correlated
mod <- paste0(mod, "\n", "mystical4 ~~ mystical9") # DUPLICATE: Intense feeling of peace / Overwhelming sense of bliss, joy and or contentment

# mod <- paste0(mod, "\n", 'rebirth =~ spiritual1 + spiritual2 + spiritual3')
# mod <- paste0(mod, "\n", 'insight =~ spiritual21 + spiritual22 + spiritual26 + spiritual27 + spiritual8 + spiritual10 + spiritual12')

mod <- paste0(mod, "\n", 'altruism =~ psygrowth30 + psygrowth10 + psygrowth45')
mod <- paste0(mod, "\n", 'psytal =~ psygrowth6 + psygrowth5 + psygrowth14')
mod <- paste0(mod, "\n", 'desire =~ psygrowth18 + psygrowth19 + psygrowth20 + psygrowth15 + psygrowth17')
mod <- paste0(mod, "\n", 'physcare =~ psygrowth24 + psygrowth22 + psygrowth23')
mod <- paste0(mod, "\n", 'healing =~ psygrowth43 + psygrowth42 + psygrowth36')
mod <- paste0(mod, "\n", 'concen =~ psygrowth41 + psygrowth39 + psygrowth29')
mod <- paste0(mod, "\n", 'detach =~ psygrowth3 + psygrowth2 + psygrowth37
')

# Paths
mod <- paste0(mod, "\n", 'hc ~ altruism')
mod <- paste0(mod, "\n", 'hc ~ psytal')
mod <- paste0(mod, "\n", 'hc ~ concen')
mod <- paste0(mod, "\n", 'hc ~ desire')
mod <- paste0(mod, "\n", 'hc ~ physcare')
mod <- paste0(mod, "\n", 'hc ~ healing')
mod <- paste0(mod, "\n", 'hc ~ detach')

# Reverse Paths
# mod <- paste0(mod, "\n", 'altruism ~ hc')
# mod <- paste0(mod, "\n", 'psytal ~ hc')
# mod <- paste0(mod, "\n", 'desire ~ hc')
# mod <- paste0(mod, "\n", 'concen ~ hc')
# mod <- paste0(mod, "\n", 'physcare ~ hc')
# mod <- paste0(mod, "\n", 'healing ~ hc')
# mod <- paste0(mod, "\n", 'detach ~ hc')

# Meditation
# mod <- paste0(mod, "\n", 'consc ~ c*altruism') # Direct effect
# mod <- paste0(mod, "\n", 'unity ~ a*altruism') # mediator
# mod <- paste0(mod, "\n", 'consc ~ b*unity') # mediator
# mod <- paste0(mod, "\n", 'ab := a*b') # indirect effect
# mod <- paste0(mod, "\n", 'total := c + (a*b)') # total effect
cfa <- cfa(mod, data=data.num, ordered = TRUE, std.lv = TRUE) # Fix factor variances with std.lv = TRUE
summary(cfa, fit.measures = TRUE, standardized = TRUE)

modindices(cfa, sort = TRUE)
# round(cor(data.num[,c("mystical2", "mystical3")]), 2)
lavaanPlot(model = cfa, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stand = TRUE)
lavInspect(cfa, "cov.lv")
cov2cor(lavInspect(cfa, what = "est")$psi)






# Group invariance

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

fit1 <- cfa(mod, data=data.num, std.lv = TRUE, group = "pe.gate")
fit2 <- cfa(mod, data=data.num, std.lv = TRUE, group = "pe.gate", group.equal = "loadings")
fit3 <- cfa(mod, data=data.num, std.lv = TRUE, group = "pe.gate", group.equal = c("intercepts", "loadings"))
summary(cfa, fit.measures = TRUE, standardized = TRUE)
lavTestLRT(fit1, fit2, fit3)



# CFA Groups

data.num <- extract.numeric.columns.by.regex(ses.data, 'mystical\\d+|spiritual\\d+|psyphys\\d+|sex|pe.gate')
nrow(data.num)
data.num <- cbind(data.num, ses.data['sex'])
nrow(data.num)
data.num <- data.num[!data.num$sex == "Intersex", ]
ses.data$sex <- as.factor(ses.data$sex)
# ses.data$pe.negphysical.gate <- as.factor(ses.data$pe.negphysical.gate)
str(data.num)
data.num <- na.omit(data.num)

nrow(data.num)

fit1 <- cfa(mod, data=data.num, std.lv = TRUE, group = "sex")
fit2 <- cfa(mod, data=data.num, std.lv = TRUE, group = "sex", group.equal = "loadings")
fit3 <- cfa(mod, data=data.num, std.lv = TRUE, group = "sex", group.equal = c("intercepts", "loadings"))
summary(cfa, fit.measures = TRUE, standardized = TRUE)
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


# TOTAL NUMBER OF PARTICIPANTS

nrow (kps.data)




#
# RESPONDENT BREAKDOWN BY AGE
#

# Age histogram
# Credit: https://www.datacamp.com/community/tutorials/make-histogram-ggplot2#gs.ko0NeIE
hist(kps.data$age, nclass = 15, xlim=c(0,100), main="Age Histogam", xlab="Age")
# qplot(kps.data$age, data=data.frame(kps.data$age), geom="histogram")


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



#
#
#
#

# Config
LCAOutputPath <- "output/lca/"
FAOutputPath <- "output/fa/"
generateFactors <- TRUE # will take lots of time if TRUE
generateLCAModel <- TRUE
options(error = browser())

# Create dirs
if( !dir.exists(LCAOutputPath) ) dir.create(LCAOutputPath)
if( !dir.exists(FAOutputPath) ) dir.create(FAOutputPath)

# Load utility functions
source("R/ses-utility.R") # REQUIRED utility functions
source("R/ses-fa-lca-functions.R") # Load Apollo Kundalini FIRE scoring functions


# Load survey response and variable information
# ses.data <- ses.loaddatafile() # question codes, or...
# ses.data <- ses.get.questiontext() # question text
# ses.vars <- ses.loadvarfile()

# Run factor analysis and extract all factors into an editable CSV.
# The CSV can be edited to rename default factor names.
fa.results <- ses.generate.factors(x = kps.numeric, threshold = 0.6, outputPath = FAOutputPath) # This will take a while to run due to parallel analysis
factor.scores <- data.frame(fa.results[[1]]$scores$scores)

# Data integrity checks
# tmp <- as.data.frame(apply(kps.numeric, 2, function(x) any(is.na(x))))
# tmp <- as.data.frame(apply(factor.scores, 2, function(x) any(is.na(x))))

# integer.scores <- as.data.frame(lapply(factor.scores, function(x) as.integer(x + 100 * 100)))
# fire.bestLCAModel(integer.scores, "cbind(PA1, PA2) ~ 1")



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
pred <- lavPredict(cfa)
# clusterdata <- pred[,c("consc", "altruism")]
pred <- as.data.frame(pred)
pred <- pred[,grepl( "hc|talents\\d+" , names( pred ) )]
clusterdata <- pred
bic <- mclustBIC(clusterdata)
# tmp <- as.data.frame(apply(clusterdata, 2, function(x) any(is.na(x))))
mod <- Mclust(clusterdata, x = bic)
summary(mod)
plot(mod, what = "classification")
plot(mod, what = "density")
describe(pred)
corPlot(pred)

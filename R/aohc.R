# °º¤ø,¸¸,ø¤º°`°º¤ø,¸,ø¤°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤°º¤ø,¸¸,ø¤:>
#
#              ASPECTS OF HIGHER CONSCIOUSNESS
#
#            All figures and quantifiable values
#
# °º¤ø,¸¸,ø¤º°`°º¤ø,¸,ø¤°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤°º¤ø,¸¸,ø¤:>


# Load libraries and utility scripts

library(ggplot2)
library(likert) # https://github.com/jbryer/likert
library(psych)
library(plyr) # For mapvalues
library(REdaS) # Bartlett's test

source("R/ses-utility.R")


#
# FULL DATA LOAD (respondent data and variable mappings)
#

kps.data <- ses.loaddatafile()
kps.vars <- ses.loadvarfile()


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
# Kaiser-Meyer-Olkin measure of sampling adequacy
#

KMO(kps.numeric)[["MSA"]]

#
# Bartlett’s Test of Sphericity
#

bart_spher(kps.numeric) # result is greater than the critical value for chi2


#
# Cronbach's Alpha
#

alpha(kps.numeric)


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

library("mclust")
clusterdata <- factor.scores
bic <- mclustBIC(clusterdata)
# tmp <- as.data.frame(apply(clusterdata, 2, function(x) any(is.na(x))))

bic <- mclustBIC(clusterdata)
mod <- Mclust(clusterdata, x = bic)
summary(mod)
plot(mod)

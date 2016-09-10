# °º¤ø,¸¸,ø¤º°`°º¤ø,¸,ø¤°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤°º¤ø,¸¸,ø¤:>
#
#          KUNDALINI PROFILE SURVEY ANALYSIS
#
# °º¤ø,¸¸,ø¤º°`°º¤ø,¸,ø¤°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤°º¤ø,¸¸,ø¤:>

# Load utility script

source("R/kps-utility.R")

# ETL
kps.data <- kps.loaddatafile()
kps.vars <- kps.loadvarfile()

#
# COMPARATIVE FACTOR ANALYSIS
#


## Basic likert visualizations ##

library(likert)

# Mystical
q <- kps.data[,grepl("mystical", names(kps.data))]
q.questiontext <- kps.get.questiontext(q)
plot(likert(q.questiontext), centered = FALSE)

# Spiritual
q <- kps.data[,grepl("spiritual", names(kps.data))]
q.questiontext <- kps.get.questiontext(q)
plot(likert(q.questiontext), centered = FALSE)



#TODO: Factor analysis on "mystical" (only)

#TODO: Factor analysis on "spiritual" (only)

#TODO: Factor analysis on both "mystical" and "spiritual" (combined)

kps.data <- kps.loaddatafile()

q <- kps.data[,grepl("mystical|spiritual", names(kps.data))]
q.num <- as.data.frame(lapply(q, as.numeric)) # Convert all values to numeric

# Generate the scree with parallel analysis
q.par <- fa.parallel(x = q, cor = "poly", fa = "fa") # Also generates plot
print(q.par) # Will suggest number of factors

# Parallel analysis suggests that the number of factors = 6  and the number of components =  NA 

# Generate factors
q.poly.fa.pro <- fa.poly(x = q.num, nfactors = 6, fm = "pa", rotate = "promax")


#
# TODO: COMPOSITE SCORE CREATION
#        - Crate composite variables based on factors
#        - Simple average of individual factor variables
#


#
#TODO: COMPOSITE SCORE CORRELATIONS ANALYSIS
#        - Create correlations analysis based composite variables
#        - Polychoric and Pearsons


#
#TODO: CLUSTER ANALYSIS
#        - Conduct cluster analysis based on primary composite variables
#        - Goal is to identify unique types of spiritual experiences


#
# TODO: OPEN TEXT CROSS-VALIDATION
#        - Cross-validate experience classifications using open text response fields.
#
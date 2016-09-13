# °º¤ø,¸¸,ø¤º°`°º¤ø,¸,ø¤°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤°º¤ø,¸¸,ø¤:>
#
#          KUNDALINI PROFILE SURVEY ANALYSIS
#
# °º¤ø,¸¸,ø¤º°`°º¤ø,¸,ø¤°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤°º¤ø,¸¸,ø¤:>

# Load utility script

source("R/kps-utility.R")

#
# ETL
#
kps.data <- kps.loaddatafile()
kps.vars <- kps.loadvarfile()



#
# DATA SUMMARY VISUALIZATIONS
#

library(ggplot2)

# Total number of participants
barplot(nrow (kps.data), width = 1, main="Kundalini Profile Survey", ylim=c(0,400), ylab="Number of Participants", col="darkgreen")

# Number of participants by sex
ggplot(data=kps.data, aes(x = kps.data$sex, fill = kps.data$sex)) +
  guides(fill = FALSE) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..), vjust=-1) +
  labs(x="Sex of Participant", y="Number of participants", fill="Sex")

# Age histogram
# Credit: https://www.datacamp.com/community/tutorials/make-histogram-ggplot2#gs.ko0NeIE
ggplot(data=kps.data, aes(x = kps.data$age, width = .4)) +
  geom_histogram(binwidth = 5, col = "white", aes(fill =..count..), alpha = .8) +
  labs(x="Age of Participant", y="Count") +
  scale_x_continuous(breaks = seq(0, 100, by = 5))



#
# COMPARATIVE FACTOR ANALYSIS
#

## Basic likert visualizations ##

library(likert)
library(psych)

# Mystical
q <- kps.data[,grepl("mystical", names(kps.data))]
q.questiontext <- kps.get.questiontext(q)
plot(likert(q.questiontext), centered = FALSE)

# Spiritual
q <- kps.data[,grepl("spiritual", names(kps.data))]
q.questiontext <- kps.get.questiontext(q)
plot(likert(q.questiontext), centered = FALSE)



# Factor analysis on "mystical" (only)

q <- kps.data[,grepl("mystical", names(kps.data))]
q.num <- as.data.frame(lapply(q, as.numeric)) # Convert all values to numeric

# Generate the scree with parallel analysis
q.par <- fa.parallel(x = q.num, cor = "poly", fa = "fa") # Also generates plot
print(q.par) # Will suggest number of factors

# 9/13 - n = 338 - Parallel analysis suggests that the number of factors = 3

# Generate factors with rotation
q.poly.fa.pro <- fa.poly(x = q.num, nfactors = 3, fm = "pa", rotate = "promax")
print(q.poly.fa.pro)
loadings <- kps.format.loadings(q.poly.fa.pro$fa$loadings)

# Export to CSV to analyze/filter in a spreadsheet app
write.csv(loadings, file = "output/mystical-loadings.csv")
write.csv(q.poly.fa.pro$rho, file = "output/mystical-poly-correlations.csv")



# Factor analysis on "spiritual" (only)

q <- kps.data[,grepl("spiritual", names(kps.data))]
q.num <- as.data.frame(lapply(q, as.numeric)) # Convert all values to numeric

# Generate the scree with parallel analysis
q.par <- fa.parallel(x = q.num, cor = "poly", fa = "fa") # Also generates plot
print(q.par) # Will suggest number of factors

# 9/13 - n = 338 - Parallel analysis suggests that the number of factors = 5

# Generate factors with rotation
q.poly.fa.pro <- fa.poly(x = q.num, nfactors = 5, fm = "pa", rotate = "promax")
print(q.poly.fa.pro)
loadings <- kps.format.loadings(q.poly.fa.pro$fa$loadings)

# Export to CSV to analyze/filter in a spreadsheet app
write.csv(loadings, file = "output/spiritual-loadings.csv")
write.csv(q.poly.fa.pro$rho, file = "output/spiritual-poly-correlations.csv")



# Factor analysis on both "mystical" and "spiritual" (combined)

q <- kps.data[,grepl("mystical|spiritual", names(kps.data))]
q.num <- as.data.frame(lapply(q, as.numeric)) # Convert all values to numeric

# Generate the scree with parallel analysis
q.par <- fa.parallel(x = q.num, cor = "poly", fa = "fa") # Also generates plot
print(q.par) # Will suggest number of factors

# 9/13 - n = 338 - Parallel analysis suggests that the number of factors = 6

# Generate factors with rotation
q.poly.fa.pro <- fa.poly(x = q.num, nfactors = 6, fm = "pa", rotate = "promax")
print(q.poly.fa.pro)
loadings <- kps.format.loadings(q.poly.fa.pro$fa$loadings)

# Export to CSV to analyze/filter in a spreadsheet app
write.csv(loadings, file = "output/mysticalspiritual-loadings.csv")
write.csv(q.poly.fa.pro$rho, file = "output/mysticalspiritual-poly-correlations.csv")



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
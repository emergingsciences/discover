# °º¤ø,¸¸,ø¤º°`°º¤ø,¸,ø¤°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤°º¤ø,¸¸,ø¤:>
#
#          KUNDALINI PROFILE SURVEY ANALYSIS
#
# °º¤ø,¸¸,ø¤º°`°º¤ø,¸,ø¤°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤°º¤ø,¸¸,ø¤:>

source("R/kps-utility.R")

#
# FULL DATA LOAD (respondent data and variable mappings)
#
kps.data <- kps.loaddatafile()
kps.vars <- kps.loadvarfile()



#
# DATA SUMMARY VISUALIZATIONS
#

library(ggplot2)
library(likert)

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

# Mystical likert visualization
q <- kps.data[,grepl("mystical", names(kps.data))]
q.questiontext <- kps.get.questiontext(q)
plot(likert(q.questiontext), centered = FALSE)

# Spiritual likert visualization
q <- kps.data[,grepl("spiritual", names(kps.data))]
q.questiontext <- kps.get.questiontext(q)
plot(likert(q.questiontext), centered = FALSE)



#
# COMPARATIVE FACTOR ANALYSIS
#

# kps.fa() - Conduct polychoric factor analysis with PAF and promax rotation
#            with optional parallel analysis. Also outputs factor analysis
#            diagrams and the output of the ICLUST algorithm for examining
#            correlations between variables. Typically run first to determine 
#            the number of factors using the "parallel" parameter, then without
#            "parallel" with the desired number of factors.
#
#            See: http://personality-project.org/r/psych/HowTo/factor.pdf
#            Contains technical details for all of the above methods
#
#
# Parameters
#
# grepmatch: The regular expression matching what variables you want to pull in
# prefix: Filename prefix to output factor loadings and correlations matrix
# nfactors: The number of factors to use as an input for factor analysis
# parallel: TRUE if you only want to run parallel analysis to determine the number of factors
#           Defaults to FALSE.
#

library(psych)
kps.fa <- function(data, grepmatch = NULL, prefix = "default", nfactors = 1, parallel = FALSE) {
 
  print(paste("Matched", sum(grepl(grepmatch, names(data))), "columns"))
  q <- data[,grepl(grepmatch, names(data))]
  q.num <- as.data.frame(lapply(q, as.numeric)) # Convert all values to numeric  
   
  # Run parallel analysis
  if(parallel == TRUE) {
    q.par <- fa.parallel(x = q.num, cor = "poly", fa = "fa") # Also generates plot
    print(q.par) # Will suggest number of factors
    return()
  }
  
  # Generate factors with rotation
  
  q.poly.fa.pro <- fa.poly(x = q.num, nfactors = nfactors, fm = "pa", rotate = "promax")
  print(q.poly.fa.pro)
  loadings <- kps.format.loadings(q.poly.fa.pro$fa$loadings)
  
  # FA diagram and ICLUST output (runs fa.poly twice)
  fa.diagram(q.poly.fa.pro)
  clust <- iclust(q.poly.fa.pro$rho)
  
  q.num <- kps.get.questiontext(q.num) # For full question text output
  
  q.poly.fa.pro <- fa.poly(x = q.num, nfactors = nfactors, fm = "pa", rotate = "promax")
  fa.diagram(q.poly.fa.pro)
  clust <- iclust(q.poly.fa.pro$rho)
  
  
  # Export to CSV to analyze/filter in a spreadsheet app
  write.csv(loadings, file = paste("output/", prefix, "-loadings.csv", sep = ''))
  write.csv(q.poly.fa.pro$rho, file = paste("output/", prefix, "-poly-correlations.csv", sep = ''))  
}


# These can take (very) long depending on the number of variables
kps.fa(kps.data, grepmatch = "mystical", prefix = "mystical", nfactors = 3) # 9/13 - n = 338 - Parallel analysis factors = 3
kps.fa(kps.data, grepmatch = "spiritual", prefix = "spiritual", nfactors = 5 ) # 9/13 - n = 338 - Parallel analysis factors = 5
kps.fa(kps.data, grepmatch = "mystical|spiritual", prefix = "mystical-spiritual", nfactors = 6) # 9/13 - n = 338 - Parallel analysis factors = 6



#
# TODO: COMPOSITE SCORE CREATION
#        - Crate composite variables based on factors
#        - Simple average of individual factor variables
#

# kps.compvar() - Create a KPS composite variable
# 
# Parameters
#
# data: Original dataset to process
# names: Vector of names to use to create the composite variable.
#        These will be automatically removed from the original dataset
# newname: The name of the new composite variable
kps.compvar <- function(data, newname, names) {
  
  data[newname] <- rowMeans(data[,c(names)]) # Calc row means and create comp var
  data <- data[,-which(names(data) %in% names)] # Remove original vars
  return(data)
  
  # Move new name to front (optional)
  # col_idx <- grep(newname, names(data))
  # compvar <- compvar[, c(col_idx, (1:ncol(data))[-col_idx])]
}

## Create Composite Variables ##

# Make a copy of the original dataset
# compvar <- kps.data
compvar <- kps.data[,grepl("mystical", names(kps.data))]

# Get all likert question names
likert.names <- grepl('mystical|spiritual|psyphys|psychic|talents|invmov|sensation|negphysical|otherphysical|negpsych|psybliss|psygrowth',
                      names(compvar))

# Convert all likert questions to numbers so we can calculate the row means
compvar[,likert.names] <- as.data.frame(lapply(compvar[,likert.names], as.numeric)) # Convert all values to numeric


# Create all composite variables

compvar <- kps.compvar(compvar, "CONSCIOUSNESS", c("mystical24","mystical25","mystical27"))


# Drop a composite variable  
# q.num <- q.num[ , !(names(q.num) %in% c("CONSCIOUSNESS"))]



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
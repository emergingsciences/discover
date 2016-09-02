# °º¤ø,¸¸,ø¤º°`°º¤ø,¸,ø¤°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤°º¤ø,¸¸,ø¤:>
#
#      Kundalini Profile Survey Analysis GUIDE
#               *** EXAMPLES ONLY ***
#
# °º¤ø,¸¸,ø¤º°`°º¤ø,¸,ø¤°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤°º¤ø,¸¸,ø¤:>

#
# ETL AND BASIC VISUALIZATIONS
#

source("R/kps-utility.R")

# ETL
kps.data <- kps.loaddatafile()
kps.vars <- kps.loadvarfile()

# Select only mystical questions
q <- kps.data[,grepl("mystical", names(kps.data))]
q.num <- as.data.frame(lapply(q, as.numeric)) # Convert all values to numeric

# Visualize likert questions

library(likert)

q.questiontext <- kps.get.questiontext(q)
plot(likert(q.questiontext), centered = FALSE)



#
# CORRELATIONS
#

# Pearson correlations

library(corrplot)

q.pcor <- cor(x = q.num, method = "pearson") # Calculate correlations
q.pcor.test <- cor.test(x = q.num[,1], q.num[,2], method = "pearson") # Example only
corrplot(q.pcor) # Plot correlation matrix

# Example p-value (using pearson)
cor.test(x = q.num$mystical1, y = q.num$mystical2, use = "pairwise", method = "pearson")


# Polychoric correlations (recommended method to generate correlations)

library(psych)
library(corrplot)

# Quick polychoric correlations matrix
q.poly <- polychoric(q) # Create a polycohric correlations matrix
corrplot(q.poly$rho) # View correlations matrix

# A fancy way to generate polychoric p-values via bootstrapping
# The following command is resource intensive and may take a while to run, especially
# if n.iter is increased.
# Source: http://stackoverflow.com/questions/19506180/polychoric-correlation-matrix-with-significance-in-r
q.poly.boot <- cor.ci(x = q.num, poly = TRUE, n.iter = 5) # Generate correlations matrix w/ confidences
print(q.poly.boot$ci)

# TODO: Compare with pearson p-values



#
# FACTOR ANALYSIS
#


# Parallel analysis to determine number of factors

# Generate the scree with parallel analysis
library(psych)

q.par <- fa.parallel(x = q, cor = "poly", fa = "fa") # Also generates plot
print(q.par) # Will suggest number of factors


# Factor analysis and eigenvalues (simple example only)

q.poly.fa <- fa.poly(x = q.num, fm = "pa") # Run a basic factor analysis (includes polychoric correlations)
print(q.poly.fa$fa$values) # Output eigenvalues
plotnScree(nScree(q.poly.fa$fa$values)) # Plot eigenvalues


# Principal factoring analysis and promax

q.poly.fa.pro <- fa.poly(x = q.num, nfactors = 3, fm = "pa", rotate = "promax")


# Generating and selecting factors

q.poly.fa.pro <- fa.poly(x = q.num, nfactors = 3, fm = "pa", rotate = "promax")

loadings <- kps.format.loadings(q.poly.fa.pro$fa$loadings)

# Tada! Here's your factor analysis!
print(loadings)

# Export to CSV to analyze/filter in a spreadsheet app
write.csv(loadings, file = "output/factloadings.csv")
write.csv(q.poly.fa.pro$rho, file = "output/correlations.csv")

# Composite scores example

q.num$CONSCIOUSNESS <- rowMeans(q.num[,c("mystical24","mystical22","mystical27")])

# q.num <- q.num[ , !(names(q.num) %in% c("CONSCIOUSNESS"))] # Drop the composite variable
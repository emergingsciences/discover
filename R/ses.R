# # # # # # # # # # # # # # # # # # # # #
#                                       #
# THIS FILES NEEDS TO BE CLEANED UP!!!  #
#                                       #
# # # # # # # # # # # # # # # # # # # # #

# # Factor correlations
# kps.data.withfascores <- kps.data.withfascores[,c("CONSCIOUSNESS", "GRACE", "BLISS", "SYNCHRONICITY", "OOB", "REBIRTH", "AURAL", "INTUITION", "ENERGY", "LIGHT")]
# cor.mat <- cor(method = "pearson", x = kps.data.withfascores)
# cor.mat.melt <- melt(cor.mat)
# cor.plot(cor.mat, numbers = TRUE)
# plot(kps.data.withfascores$ENERGY, kps.data.withfascores$LIGHT)


# Pairwise correlation example
plot(
  jitter(as.numeric(kps.data$mystical24), factor = 1.2)
  , jitter(as.numeric(kps.data$mystical22), factor = 1.2)
)




# Example polychoric correlations matrix to faciliate data analysis

library(psych)
raw.data <- kps.data[,grepl("psybliss\\d+", names(kps.data))]
raw.data <- lapply(raw.data, as.ordered)
raw.data <- lapply(raw.data, as.numeric)
poly.results <- polychoric(as.data.frame(raw.data))
cor.plot(poly.results$rho, numbers = TRUE)


# TODO: Implement IRT FA information
# q.irt.fa <- irt.fa(x = q.num, nfactors = 3, fm = "pa", rotate = "promax")



#
# MCLASS CREATION
#

#
# Conduct Latent Class Analysis to analyze response patterns
# Source: http://statistics.ohlsen-web.de/latent-class-analysis-polca/
#

library("poLCA")
library("reshape2")
library("plyr")
library("ggplot2")

# Format data properly. Convert text to numbers and then to ordered
q <- kps.data

likert.names <- grepl('mystical\\d+|spiritual\\d+|psyphys\\d+|psychic\\d+|talents\\d+|invmov\\d+|sensation\\d+|negphysical\\d+|otherphysical\\d+|negpsych\\d+|psybliss\\d+|psygrowth\\d+',
                      names(q))
q[,likert.names] <- lapply(q[,likert.names], function(x) {
  x <- mapvalues(x, 
                 from=c('Not at all', 'Very Weak/low intensity', 'Weak', 'Moderate', 'Strong', 'Very strong/high intensity'), 
                 to=c("1", "2","3", "4", "5", "6"))
  x <- ordered(x)
  return(x)
})




#
# DECISION TREES (WITH MCLASS AVAILABLE)
#

# Source: http://www.edureka.co/blog/implementation-of-decision-tree/

library(rpart)
library(rpart.plot) # See: http://www.milbo.org/rpart-plot/prp.pdf
library(plyr)

# MCLASS psychological decision trees

q.sub <- subset(q, psybliss.gate == "Y")
tree <- rpart(
  MCLASS ~ .
  , data = q.sub[,grepl('psybliss\\d+|MCLASS', names(q.sub))]
)
rpart.plot(tree, tweak = 1.1)

q.sub <- subset(q, psygrowth.gate == "Y")
tree <- rpart(
  MCLASS ~ .
  , data = q.sub[,grepl('psygrowth\\d+|MCLASS', names(q.sub))]
)
rpart.plot(tree, tweak = 1.1)


# Negative physical symtoms decision trees

q.sub <- subset(q, psygrowth.gate == "Y")
tree <- rpart(
  pe.negphysical.gate ~ .
  , data = q.sub[,grepl('pe.negphysical.gate|psygrowth\\d+|MCLASS', names(q.sub))]
)
rpart.plot(tree, tweak = 1.25)

q.sub <- subset(q, psybliss.gate == "Y")
tree <- rpart(
  pe.negphysical.gate ~ .
  , data = q.sub[,grepl('pe.negphysical.gate|psybliss\\d+|MCLASS', names(q.sub))]
)
rpart.plot(tree, tweak = 1.1)


# Negative psychological decision tree

q.sub <- subset(q, psybliss.gate == "Y")
tree <- rpart(
  negpsych.gate ~ .
  , data = q.sub[,grepl('negpsych.gate|psybliss\\d+|MCLASS', names(q.sub))]
)
rpart.plot(tree, tweak = 1.1)


q.sub <- subset(q, psygrowth.gate == "Y")
tree <- rpart(
  negpsych.gate ~ .
  , data = q.sub[,grepl('negpsych.gate|psygrowth\\d+|MCLASS', names(q.sub))]
)
rpart.plot(tree, tweak = 1.1)


q.sub <- q
tree <- rpart(
  MCLASS ~ .
  , data = q.sub[,grepl('talents\\d+|MCLASS', names(q.sub))]
)
rpart.plot(tree, tweak = 1.1)



#
# MCLASS PROFILING
#

library(coin)
library(plyr)
library(polycor)

# Returns a data frame containing all descriptive and statistical output
kps.profile <- function(data = NULL, target = NULL, grepstr = "", prefix = "") {
  
  # Populate "compare" data frame one row at a time
  
  compare <- data.frame( # Destination for all our statistics
                        question = character(),
                        polychor = double(),
                        chisq = double(),
                        # wilcox.u = double(), # Mann-Whitney U statistic
                        # wilcox.p = double(), # P-value
                        # wilcox.r = double(), # Effect size
                        # median.m1 = double(), # Median of mclass 1
                        # median.m2 = double(), # Median of mclass 2
                        # median.diff = double(), # m2 minus m1
                        # t.test.t = double(), # T-test t statistic
                        # t.test.p = double(), # T-test p-value
                    stringsAsFactors = FALSE)
  
  varnames <- names( data[,!grepl(target, names(data))]) # All but the MCLASS
  
  for(i in 1:length(varnames)) {
    compare[i,"question"] <- varnames[i]
    
    # Conduct a Mann-Whitney U Test
    # This generates the u (sometimes called w), p, and r statistics. For more information
    # see the resources below:
    #
    # http://www.stata-journal.com/sjpdf.html?articlenum=st0253
    # https://statistics.laerd.com/premium-sample/mwut/mann-whitney-test-in-spss-2.php
    # http://yatani.jp/teaching/doku.php?id=hcistats:mannwhitney
    
    if(length(levels(data$MCLASS)) == 2) {
      wc <- wilcox.test(as.formula(paste("as.numeric(",varnames[i],") ~ MCLASS")), data = data)
      compare[i,"wilcox.u"] <- wc$statistic
      compare[i,"wilcox.p"] <- wc$p.value

      wc <- wilcox_test(as.formula(paste("as.numeric(",varnames[i],") ~ MCLASS")), data = data)
      compare[i,"wilcox.r"] <- statistic(wc) / sqrt(nrow(data))
      
      # Conduct a standard t test
      
      t <- t.test(as.numeric(subset(data[,varnames[i]], data$MCLASS == 1)), as.numeric(subset(data[,varnames[i]], data$MCLASS == 2)))
      
      compare[i,"t.test.t"] <- t$statistic
      compare[i,"t.test.p"] <- t$p.value
      
      compare[i,"median.m1"] <- median(as.numeric(data[data$MCLASS == "1",varnames[i]]))
      compare[i,"median.m2"] <- median(as.numeric(data[data$MCLASS == "2",varnames[i]]))
      compare[i,"median.diff"] <- compare[i,"median.m2"] - compare[i,"median.m1"]
    }
    
    compare[i, "polychor"] <- polychor(data[,target], data[,varnames[i]])
    
    chiresult <- chisq.test(data[,target], data[,varnames[i]])
    compare[i, "chisq"] <- as.double(chiresult$statistic)
  }
  
  # Tack on the question text to the end
  compare <- merge(compare, kps.vars[c("varname", "question.text")], by.x = "question", by.y = "varname")
  
  # Write file out to the output folder with the specified prefix
  write.csv(compare, file = paste("output/", prefix, "-KPSANALYSIS-profile.csv", sep = ""))
  
  return(compare)
}



# Subselect data and pass to kps.profile()
q.sub <- q[,grepl("MCLASS|mystical\\d+|spiritual\\d+|psyphys\\d+|psychic\\d+|talents\\d+|invmov\\d+|sensation\\d+|negphysical\\d+|otherphysical\\d+|negpsych\\d+|psybliss\\d+|psygrowth\\d+", names(q))]
kps.profile(data = q.sub, target = "MCLASS", prefix = "all")


# Likert plot (sample)
plot(likert(items = as.data.frame(q.sub[,grepl(likert.names, names(q.sub))]), grouping = q.sub$MCLASS))


#
# TODO: OPEN TEXT CONFIRMATION
#        - Confirm experience classifications using open text response fields.
#

# TODO: Score a file based on open text responses
# TODO: Validate findings to date against confirmation results



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




# Format factor loadings from the psych fa() family of functions
# into a user friendly format.
#
# Original loadings usually kept in an object similar to fa.object$fa$loadings
#
ses.format.loadings <- function(original.loadings = NULL) {
  var.names <- ses.loadvarfile()
  
  # Create initial data frame from loadings. Create additional columns
  loadings <- data.frame(unclass(original.loadings))
  loadings <- data.frame("code"= rownames(loadings), "text" = rownames(loadings), loadings, stringsAsFactors=FALSE)
  rownames(loadings) = NULL
  
  # Replace question codes with question text
  matches <- match(loadings$text, var.names$varname)
  loadings$text[!is.na(matches)] <- as.character(var.names$question.text[na.omit(matches)])
  return(loadings)
}



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
# compvar <- kps.data[,grepl("mystical", names(kps.data))]

# Get all likert question names
# likert.names <- grepl('mystical|spiritual|psyphys|psychic|talents|invmov|sensation|negphysical|otherphysical|negpsych|psybliss|psygrowth',
#                      names(compvar))

# Convert all likert questions to numbers so we can calculate the row means
# compvar[,likert.names] <- as.data.frame(lapply(compvar[,likert.names], as.numeric)) # Convert all values to numeric


# Create all composite variables

# compvar <- kps.compvar(compvar, "CONSCIOUSNESS", c("mystical24","mystical25","mystical27"))


# Drop a composite variable  
# q.num <- q.num[ , !(names(q.num) %in% c("CONSCIOUSNESS"))]


# fire.profile()
#
# Profile all non-primary experience clusters. Returns a data frame
# containing all descriptive and statistical output.
#
fire.profile <- function(data = NULL, target = "", grepstr = "", prefix = "") {
  
  # Populate "compare" data frame one row at a time
  
  compare <- data.frame( # Destination for all our statistics
    question = character(),
    polychor = double(), # Polychoric r
    chisq = double(), # Chi-square
    stringsAsFactors = FALSE)
  
  varnames <- names( data[,!grepl(target, names(data))]) # All but the MCLASS
  
  for(i in 1:length(varnames)) {
    compare[i,"question"] <- varnames[i]
    
    # Conduct a Mann-Whitney U Test
    # This generates the u (sometimes called w), p, and r statistics. For more information
    # see the resources below:
    #
    # http://www.stata-journal.com/sjpdf.html?articlenum=st0253
    # https://statistics.laerd.com/premium-sample/mwut/mann-whitney-test-in-spss-2.php
    # http://yatani.jp/teaching/doku.php?id=hcistats:mannwhitney
    
    compare[i, "polychor"] <- polychor(data[,target], data[,varnames[i]])
    
    chiresult <- chisq.test(data[,target], data[,varnames[i]])
    compare[i, "chisq"] <- as.double(chiresult$statistic)
  }
  
  # Tack on the question text to the end
  # compare <- merge(compare, kps.vars[c("varname", "question.text")], by.x = "question", by.y = "varname")
  
  # Write file out to the output folder with the specified prefix
  write.csv(compare, file = paste("output/", prefix, "-FIRE-profile.csv", sep = ""))
  
  return(data.frame(compare))
}

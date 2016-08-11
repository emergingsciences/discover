# 
# Kundalini Profile Survey analysis script
#

library("ggplot2")
library("psych")
library("nFactors")


# Load the default data file
kps.loaddatafile <- function(file = "data/kps1-results.txt") {
  return(dget(file))
}



# Load the default variables file. Contains original question text
kps.loadvarfile <- function(file = "data/kps1-variables.csv") {
  return(read.csv(file = file))
}



# Replace the question codes with question text in the data frame
kps.get.questiontext <- function(x = kps.loaddatafile()) {
  var.names <- kps.loadvarfile()
  mv <- match(names(x), var.names$varname)
  names(x)[!is.na(mv)] <- as.character(var.names$question.text[na.omit(mv)])
  return(x)
}



# polyfa()
# 
# Outputs the following, in order:
# 1. Descriptive statistics
# 2. Eigenvalues
# 3. Scree plot and parallel analysis (plot)
# 4. Factor loadings
# 5. Phi matrix
# 6. Factor plot (plot)
#
# Credit: https://blogs.baylor.edu/grantmorgan/2013/03/08/click-here-for-exploratory-factor-analysis-code/
#
kps.polyfa <- function(x, nfactors = NULL) {
  
  # Descriptive stats to ensure quality
  
  x.num <- as.data.frame(lapply(x, as.numeric))
  print(describe(x.num)) # Output descriptive statistics
  
  # See eigenvalues
  
  x.temp <- fa.poly(x = x.num, fm = "pa")
  print(x.temp$fa$values)  # Output eigenvalues
  
  # Polychoric correlations viz to determine # of factors
  
  x.poly <- polychoric(x.num)
  x.ev <- eigen(x.poly$rho)
  plotnScree(nScree(x.ev$values), main = "Scree Plot & Parallel Analysis")
  
  # Extract factors based on scree
  if(is.null(nfactors)) {
    warning("nfactors not set. Will not generate loadings table, phi matrix, or factor plot.")
  } else {
    x.out <- fa.poly(x = x.num, fm = "pa", nfactors = nfactors, rotate = "promax", residual = TRUE)
    print(x.out$fa$loadings)  # The factor loadings table, or the "strength" of a variable for a factor
    print(x.out$fa$Phi)  # The phi matrix represents correlations between factors
    factor.plot(x.out$fa)    
  }
}


# Create sample plot for sex of participants
kps.sample.histplot <- function() {
  kps.data <- kps.loaddatafile()
  ggplot(data=kps.data, aes(kps.data$sex)) + geom_bar() + labs(x="Sex of Participant", y="Count")  
}

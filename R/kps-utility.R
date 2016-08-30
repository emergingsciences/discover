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



# Create sample plot for sex of participants
kps.sample.histplot <- function() {
  kps.data <- kps.loaddatafile()
  ggplot(data=kps.data, aes(kps.data$sex)) + geom_bar() + labs(x="Sex of Participant", y="Count")  
}


# Format factor loadings into a user friendly format
#
# Original loadings usually kept in an object similar to fa.object$fa$loadings
kps.format.loadings <- function(original.loadings = NULL) {
  var.names <- kps.loadvarfile()
  
  # Create initial data frame from loadings. Create additional columns
  loadings <- data.frame(unclass())
  loadings <- data.frame("code"= rownames(loadings), "text" = rownames(loadings), loadings, stringsAsFactors=FALSE)
  rownames(loadings) = NULL
  
  # Replace question codes with question text
  matches <- match(loadings$text, var.names$varname)
  loadings$text[!is.na(matches)] <- as.character(var.names$question.text[na.omit(matches)])
  return(loadings)
}
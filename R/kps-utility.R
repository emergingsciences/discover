# 
# Kundalini Profile Survey analysis script
#


# Load the default data file
kps.loaddatafile <- function(file = "data/kps1-results.txt") {
  df <- dget(file)
  
  likert.names <- grepl('mystical|spiritual|psyphys|psychic|talents|invmov|sensation|negphysical|otherphysical|negpsych|psybliss|psygrowth',
    names(df))
  
  df[,likert.names] <- data.frame(lapply(df[,likert.names], function(x) {
      if(length(levels(x)) == 6) {
        x <- ordered(x, labels = c('Not at all', 'Very Weak/low intensity', 'Weak', 'Moderate', 'Strong', 'Very strong/high intensity'))
      } else if( length(levels(x)) == 7 ) {
        x <- ordered(x, labels = c(NA, 'Not at all', 'Very Weak/low intensity', 'Weak', 'Moderate', 'Strong', 'Very strong/high intensity'))
      }
      return(x)
    } ), check.names = FALSE)
  
  return(df)
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



# Format factor loadings into a user friendly format
#
# Original loadings usually kept in an object similar to fa.object$fa$loadings
kps.format.loadings <- function(original.loadings = NULL) {
  var.names <- kps.loadvarfile()
  
  # Create initial data frame from loadings. Create additional columns
  loadings <- data.frame(unclass(original.loadings))
  loadings <- data.frame("code"= rownames(loadings), "text" = rownames(loadings), loadings, stringsAsFactors=FALSE)
  rownames(loadings) = NULL
  
  # Replace question codes with question text
  matches <- match(loadings$text, var.names$varname)
  loadings$text[!is.na(matches)] <- as.character(var.names$question.text[na.omit(matches)])
  return(loadings)
}
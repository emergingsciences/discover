# 
# Kundalini Profile Survey Utility Script
#
# This script contains:
#   - Basic data loading functions
#   - Mapping functions to use whenever we would like to use full question text
#     instead of just the question codes
#


# Load the default data file
kps.loaddatafile <- function(file = "data/kps1-results.txt") {
  df <- dget(file)
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



# Format factor loadings from the psych fa() family of functions
# into a user friendly format.
#
# Original loadings usually kept in an object similar to fa.object$fa$loadings
#
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


# Pass in a kps dataset and replace likert labels to numerical
#
# Parameters:
#
# df - A KPS data frame with question codes
kps.numerical.levels <- function(df = NULL) {
  
  if(is.null(df)) {
    warning("Need to specify data parameter in the form of a KPS data frame")
    return()
  }
  
  likert.names <- grepl('mystical|spiritual|psyphys|psychic|talents|invmov|sensation|negphysical|otherphysical|negpsych|psybliss|psygrowth',
                        names(data))
  
  df <- data.frame(lapply(df[,likert.names], function(x) {
    if(length(levels(x)) == 6) {
      x <- ordered(x, labels = c('Not at all', 'Very Weak/low intensity', 'Weak', 'Moderate', 'Strong', 'Very strong/high intensity'))
    } else if( length(levels(x)) == 7 ) {
      x <- ordered(x, labels = c(NA, 'Not at all', 'Very Weak/low intensity', 'Weak', 'Moderate', 'Strong', 'Very strong/high intensity'))
    }
    return(x)
  } ), check.names = FALSE)
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
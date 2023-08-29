# 
# Spiritual Experience Survey Utility Functions
#
# This script contains:
#   - Basic data loading functions
#   - Mapping functions to use whenever we would like to use full question text
#     instead of just the question codes
#


# Load the SES respondent data file
# @return The SES respondent data file
ses.loaddatafile <- function(file = "data/ses-data.txt") {
  df <- dget(file)
  return(df)
}


# Loads the SES variable information file
# @return The SES variable information file
ses.loadvarfile <- function(file = "data/ses-vars.csv") {
  return(read.csv(file = file))
}


# Loads the SES data file and then replaces question codes with question text
# @return The SES data file with question text instead of question codes (default)
ses.get.questiontext <- function(x = ses.loaddatafile()) {
  var.names <- ses.loadvarfile()
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
  loadings <- data.frame(
	"code"= rownames(loadings),
	"text" = rownames(loadings),
	loadings,
	stringsAsFactors=FALSE)
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

#' kps.fa()
#'
#' Runs factor analysis on a given subset of data. Factors are generated
#' using principal axis factoring with promax rotation.
#'
#' @param data The data to process
#' @param grepmatch A regular expression indicating which columns should be processed
#'
#' @return The generated factor analysis
#'
kps.fa <- function(data, grepmatch = NULL) {
  
  # Provide some basic output to show how many data records and
  # the names of the columns for validation
  print(paste("Matched", sum(grepl(grepmatch, names(data))), "columns"))
  print(paste("Names: "
              , paste(
                names(data[,grepl(grepmatch, names(data))])
                , collapse = ", "
              )
  ))
  
  # Exit if no columns match
  if(sum(grepl(grepmatch, names(data))) < 1) return()
  
  # Extract columns specified
  q <- data[,grepl(grepmatch, names(data))]
  q.num <- as.data.frame(lapply(q, as.numeric)) # Convert all values to numeric  
  
  # Run parallel analysis
  q.par <- fa.parallel(x = q.num, cor = "poly", fa = "fa") # Also generates plot
  suggestedFactors <- q.par$nfact
  
  print(paste("Parallel analysis resulted in a",suggestedFactors,"factor solution"))
  
  # Generate factors with rotation
  q.poly.fa.pro <- fa(r = q.num, nfactors = suggestedFactors, fm = "pa", rotate = "promax", cor = "poly")
  
  return(q.poly.fa.pro)
}

kps.likert.to.numerical <- function(x) {
  likert.names <- grepl('mystical\\d+|spiritual\\d+|psyphys\\d+|psychic\\d+|talents\\d+|invmov\\d+|sensation\\d+|negphysical\\d+|otherphysical\\d+|negpsych\\d+|psybliss\\d+|psygrowth\\d+',
                        names(x))
  x[,likert.names] <- lapply(x[,likert.names], function(x) {
    x <- mapvalues(x, 
                   from=c('Not at all', 'Very Weak/low intensity', 'Weak', 'Moderate', 'Strong', 'Very strong/high intensity'), 
                   to=c("1", "2","3", "4", "5", "6"))
    x <- ordered(x)
    return(x)
  })
  
  return(x)
}

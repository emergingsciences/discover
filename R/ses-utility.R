# 
# Kundalini Profile Survey Utility Functions
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
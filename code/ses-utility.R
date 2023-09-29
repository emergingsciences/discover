# 
# Spiritual Experience Survey Utility Functions
#
# This script contains:
#   - Basic data loading functions
#   - Mapping functions to use whenever we would like to use full question text
#     instead of just the question codes
#

# # # # # # # # # # # #
# Survey Data Load ----
# # # # # # # # # # # #

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


# Extract columns by a regex matching the column names, with all data converted
# into numeric values
# @return A subset of the data containing only columns specified in the regex
extract.numeric.columns.by.regex <- function(data, grepmatch = "mystical\\d+|spiritual\\d+|psyphys\\d+|psychic\\d+|talents\\d+|invmov\\d+|sensation\\d+|negphysical\\d+|otherphysical\\d+|negpsych\\d+|psybliss\\d+|psygrowth\\d+") {
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
  return(q.num)
}

# Loads the SES data file and then replaces question codes with question text
# @return The SES data file with question text instead of question codes (default)
ses.get.questiontext <- function(x = ses.loaddatafile(), max_length = NULL) {
  var.names <- ses.loadvarfile()
  mv <- match(names(x), var.names$varname)
  new_names <- as.character(var.names$question.text[na.omit(mv)])
  
  if (!is.null(max_length)) {
    new_names <- sapply(new_names, function(name) {
      if (is.na(max_length) || max_length < 1) {
        return(name)
      } else {
        substr(name, 1, max_length)
      }
    })
  }
  
  colnames(x)[!is.na(mv)] <- new_names
  return(x)
}

ses.likert.to.numerical <- function(x) {
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

# # # # # # # # # # # #
# Output Utilities ----
# # # # # # # # # # # #

# Limit console output to n_rows
#
# nrows - Specify the number of rows you want to display (e.g., first 10 rows)
limit.output <- function(consoleObject, n_rows = 10) {
  # Create a summary object
  summary_result <- consoleObject
  # Capture and store the summary as text
  summary_text <- capture.output(summary_result)
  
  # Display the first n rows of the summary
  cat(paste(head(summary_text, n_rows), collapse = "\n"))
}


# Format factor loadings from the psych fa() family of functions
# into a user friendly format.
#
# Original loadings usually kept in an object similar to fa.object$fa$loadings
#
ses.format.loadings <- function(original.loadings = NULL) {
  var.names <- ses.loadvarfile()
  
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

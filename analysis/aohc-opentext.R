# °º¤ø,¸¸,ø¤º°`°º¤ø,¸,ø¤°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤°º¤ø,¸¸,ø¤:>
#
#              OPEN TEXT WEB PAGE CREATION
#
#       For ease of accessing open text responses
#
# °º¤ø,¸¸,ø¤º°`°º¤ø,¸,ø¤°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤°º¤ø,¸¸,ø¤:>

source("code/ses-utility.R")


#
# FULL DATA LOAD (respondent data and variable mappings)
#

ses.data <- ses.loaddatafile()
ses.vars <- ses.loadvarfile()

# Create .R file for example where we make html from an existing file
# 
fileConn <- file("analysis/opentext.html")
contents <- "<html><head><title>Open Text Responses</title></head><body>"

text_columns <- grep("text", names(ses.data), value = TRUE)
columns_to_print <- c("id", text_columns)
otdata <- data.frame(id = ses.data$id, ses.data[, grepl("text", names(ses.data))], CLUST = results$CLUST)

# "Table of contents" ID's
ids <- NULL
for (i in seq_len(nrow(otdata))) {
  if(otdata[i, "CLUST"] == 4)
    ids <- paste0(ids, "<a href=\"#", ses.data$id[i], "\">", ses.data$id[i], "</a><br>")
}

contents <- paste0(contents, ids)

opentext <- NULL
# Open text responses
for (i in seq_len(nrow(otdata))) {
  if(otdata[i, "CLUST"] == 4) {
    opentext <- paste0(opentext, "<h1><a id=\"", otdata$id[i], "\">", otdata$id[i], "</a></h1><br>")
    for (col_name in text_columns) {
      if (!is.na(otdata[i, col_name])) {
        opentext <- paste0(opentext, "Column:", col_name, "<br>")
        opentext <- paste0(opentext, otdata[i, col_name], "<br><br>")
      }
    }    
  }
}

contents <- paste0(contents, opentext)

# End

contents <- c(contents, "</body></html>")
writeLines(contents, fileConn)
close(fileConn)
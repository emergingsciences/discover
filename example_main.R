# Example main script.

library(likert)

source("R/kps.R")

kps.data <- kps.loaddatafile()

# Select only mystical questions
questions <- kps.data[,grepl("mystical", names(kps.data))]

# Plot with original question text
questions.questiontext <- kps.get.questiontext(questions)
plot(likert(questions.questiontext), centered = FALSE)


# Conduct factor analysis
kps.polyfa(questions, 3)

#
# Create Level 1 Data File (Limited PII)
#

library(digest)

# Read in output file from LimeSurvey (question codes and answer codes)
#results <- read.csv("data/pbs-results-raw.csv", na.strings = "")
#var.names <- read.csv("data/pbs-variables.csv", stringsAsFactors = FALSE)

# Remove "." characters in column names
#names(results) <- gsub("..", "_", names(results), fixed = TRUE)
#names(results) <- gsub(".", "", names(results), fixed = TRUE)

#rownames(results) <- NULL

#dput(results, file = "data/pbs-data.txt")
results <- dget("data/pbs-data.txt")

#
# Create Higher Consciousness composite variable
#
results$HC <- results$spirex01 +
  results$spirex02 + results$spirex03 +
  results$spirex05 + results$spirex09


#
# Get subset of data / questions
#

# sub <- results[,grepl('HC|pbnature\\d+|pbenoughnature\\d+|pbsocialpercep\\d+|pbelechabits\\d+|beliefs\\d+|behaviors\\d+|spirprac\\d+', names(results))]
# sub <- results[,grepl('HC|hered.*', names(results))]  
# sub <- results[,grepl('HC|pbsocialpercep\\d+', names(results))]
# sub <- results[,grepl('HC|beliefs\\d+', names(results))]
sub <- results[,grepl('HC|spirpract\\d+', names(results))]
# sub <- results[,grepl('HC|pbelechabits\\d+', names(results))]
# sub <- results[,grepl('HC|dsediet\\d+', names(results))]

library(likert)
sub[,!grepl("HC", names(sub))] <- lapply(sub[,!grepl("HC", names(sub))], as.numeric)

#
# Simple decision tree
#
library(rpart)
library(rpart.plot) # See: http://www.milbo.org/rpart-plot/prp.pdf
tree <- rpart(
  HC ~ .
  , sub
  , control = rpart.control(minsplit = 7)
)
rpart.plot(tree, tweak = 1.1)


#
# Random Forest
#
library(randomForest)
(randF <- randomForest::randomForest(HC ~ ., 
                                     data = sub, 
                                     na.action = na.exclude))
varImpPlot(randF)


#
# Correlations matrix
#
corMat <- cor(x = sub)

#
# Scatterplot
#
library(stats)
scatter.smooth(
  jitter(sub$HC, factor=1.5),
  jitter(sub$spirpract45, factor=1.5)
)


#
# Likert visualization
#
library(likert)
sub[,!grepl("HC", names(sub))] <- lapply(sub[,!grepl("HC", names(sub))], function(x) {
  ordered(x, levels=c(1,2,3,4,5,6))
})
plot(likert(sub[,!grepl("HC", names(sub))]), centered = FALSE)

# install.packages("car", dependencies = TRUE)
# library(car)
# tmp <- (hetcor(data = sub))
# tmp$correlations
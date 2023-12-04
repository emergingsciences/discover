# HC Complete Decision Tree

library(rpart)
library(rpart.plot) # See: http://www.milbo.org/rpart-plot/prp.pdf
library(plyr)

# Get predicted lavaan results
# results <- pred

grepmatch <- '\\bmystical6\\b|\\bmystical22\\b|\\bmystical25\\b|\\bmystical15\\b|\\bmystical8\\b|\\bmystical13\\b|\\bmystical10\\b|\\bmystical5\\b|\\bmystical7\\b|\\bmystical4\\b|\\bspiritual3\\b|\\bspiritual2\\b|\\bspiritual26\\b|\\bpsyphys5\\b|\\bpsyphys3\\b|\\bpsyphys9\\b|\\bpsyphys11\\b|\\bpsyphys1\\b'
tmp <- extract.numeric.columns.by.regex(ses.data, grepmatch = grepmatch)
nrow(tmp)

tree.set <- data.frame(tmp)
tree.set$CLUST <- mod$classification

table(mod$classification)

# Modify the CLUST column
tree.set$CLUST <- ifelse(tree.set$CLUST == 3, 1, 0) # Be sure to check this!!!

table(tree.set$CLUST)

# Convert the CLUST column to a factor
tree.set$CLUST <- factor(tree.set$CLUST, levels = c(0, 1), labels = c("Nhc", "HC"))

table(tree.set$CLUST)

# Convert all columns to factors
col_names <- names(subset(tree.set, select = -c(CLUST)))
tree.set[,col_names] <- lapply(tree.set[,col_names] , factor) # Change to factor
# tree.set[,col_names] <- lapply(tree.set[,col_names] , as.numeric) # Change to numeric

tree <- rpart(CLUST~., data = tree.set, control = rpart.control())
summary(tree)
rpart.plot(tree, tweak = 1.2)
printcp(tree)
# plotcp(tree)
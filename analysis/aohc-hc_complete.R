# HC Complete Decision Tree ----

library(rpart)
library(rpart.plot) # See: http://www.milbo.org/rpart-plot/prp.pdf
library(plyr)

# Get predicted lavaan results
# results <- pred

grepmatch <- '\\bmystical6\\b|\\bmystical22\\b|\\bmystical25\\b|\\bmystical15\\b|\\bmystical8\\b|\\bmystical13\\b|\\bmystical10\\b|\\bmystical5\\b|\\bmystical7\\b|\\bmystical4\\b|\\bspiritual3\\b|\\bspiritual2\\b|\\bspiritual26\\b|\\bpsyphys5\\b|\\bpsyphys3\\b|\\bpsyphys9\\b|\\bpsyphys11\\b|\\bpsyphys1\\b'
tmp <- extract.numeric.columns.by.regex(ses.data, grepmatch = grepmatch)
nrow(tmp)

tree.set <- data.frame(tmp)
# tree.set$CLUST <- clustmod$classification
tree.set$CLUST <- results$CLUST

table(tree.set$CLUST)

# Modify the CLUST column
tree.set$CLUST <- ifelse(tree.set$CLUST == 4, 1, 0) # Be sure to check this!!! Set the cluster indicating HC Complete

table(tree.set$CLUST)

# Convert the CLUST column to a factor
tree.set$CLUST <- factor(tree.set$CLUST, levels = c(1, 0), labels = c("HCC", "Not HCC"))

table(tree.set$CLUST)

# Convert all columns to factors
col_names <- names(subset(tree.set, select = -c(CLUST)))
# tree.set[,col_names] <- lapply(tree.set[,col_names] , factor) # Change to factor
tree.set[,col_names] <- lapply(tree.set[,col_names] , as.numeric) # Change to numeric


# CP parameter tuning ----

library(caret)

# Define the tuning grid
tuneGrid <- expand.grid(cp = seq(0.1, 0.002, by = -0.002))

# Define the control parameters for cross-validation
ctrl <- trainControl(method = "repeatedcv",   # Cross-validation
                     number = 7,     # 10 folds
                     repeats = 50)   # 100 iterations

# Perform hyperparameter tuning using train function
# set.seed(1234567)
tree_model <- train( CLUST ~ ., 
                    data = tree.set,
                    method = "rpart",
                    trControl = ctrl,
                    tuneGrid = tuneGrid,
                    # tuneLength = 1,
                    # model = tree,
                    # modelType = "rpart",
                    control = rpart.control(minsplit = 7, maxdepth = 5)
                    )

# View the best model
print(tree_model)

# Plot the best model's performance
plot(tree_model)


# K-Fold ----
tree.kfold<- function(model, dats, n.folds, reps){
  print(paste("Record count: ", nrow(dats)))
  
  results <- data.frame(matrix(ncol=2,nrow=0, dimnames=list(NULL, c(
    "Accuracy", "Kappa")
  )))     
  
  folds = rep(1:n.folds, length.out = nrow(dats)) 
  
  for(r in 1:reps) {
    folds = sample(folds) # Random permutation
    print(paste("Repetition",r))
    
    for (i in 1:n.folds){
      indis <- which(folds == i)
      # print(paste("Fitting on fold with", length(indis), "rows"))

      # Hyperparameter validation
      model <- rpart(CLUST~., data = tree.set[-indis,], method = "class", control =
        rpart.control(
          cp = 0.02,
          minsplit = 8,
          maxdepth = 5
        )
      )
            
      predictions <- predict(model, dats[indis,], type = "class")
      cm <- confusionMatrix(predictions, dats[indis,]$CLUST)
      
      fit.df <- data.frame(
        model = "Tree",
        Accuracy = cm$overall['Accuracy'],
        Kappa = cm$overall['Kappa']
      )
      
      results <- rbind(results, fit.df)
      rownames(results) = NULL
    }    
  }
  
  return(as.data.frame(results))
}

tree.kfold.results <- tree.kfold(NULL, tree.set, 10, 100)

mean(tree.kfold.results$Kappa)
mean(tree.kfold.results$Accuracy)

ggplot(tree.kfold.results, aes(x=model, y=Accuracy, fill=factor(model))) +
  geom_boxplot(fill = "grey", aes(group = factor(model))) + 
  geom_jitter(width = 0.09, height = .007, colour = rgb(0,0,0,.3)) + 
  xlab("Decision Tree") + ylab("Accuracy") + 
  theme(legend.position="none", axis.title.x=element_blank(), axis.text.x=element_blank()) +
  scale_fill_grey(start=.3,end=.7)

ggplot(tree.kfold.results, aes(x=model, y=Kappa, fill=factor(model))) +
  geom_boxplot(fill = "grey", aes(group = factor(model))) + 
  geom_jitter(width = 0.09, height = .02, colour = rgb(0,0,0,.3)) + 
  xlab("Decision Tree") + ylab("Kappa") + 
  theme(legend.position="none", axis.title.x=element_blank(), axis.text.x=element_blank()) +
  scale_fill_grey(start=.3,end=.7)



set.seed(12345678)

percent <- 1 # Can be modified for crossvalidation
test_idx <- sort(sample(x = nrow(tree.set), size = floor(percent*nrow(tree.set)), replace = F))

# Hyperparameter tuning - https://medium.com/data-and-beyond/hyperparameter-tuning-and-pruning-more-about-k-means-clustering-in-r-with-rpart-9a405685a09b
tree <- rpart(CLUST~., data = tree.set[test_idx,], method = "class", control =
                rpart.control(
                  cp = 0.02,
                  minsplit = 8,
                  maxdepth = 5
                )
)
rpart.plot(tree, tweak = 1, box.palette = list('grey90', 'grey70', "grey60"))
printcp(tree)
plotcp(tree)
# pruned_tree <- prune(tree, cp = .02)
# tree <- pruned_tree
predictions <- predict(tree, tree.set[test_idx,], type = "class") # Change to -test_idx for cross-validation
confusionMatrix(predictions, tree.set[test_idx,]$CLUST) # Change to -test_idx for cross-validation
summary(tree)

set.seed(NULL)
# Takes a data set, a lavaan model, xnames, ynames, and a sequence of lambdas
# and performs k-fold cross-validation to determine the best lambda
# cv.lavPredictYReg <- function(fit, data, xnames, ynames, n.folds = 10, lambda.seq) {
# 
#   results <- data.frame(matrix(ncol=2,nrow=0, dimnames=list(NULL, c(
#     "mse", "lambda")
#   )))
# 
#   folds = rep(1:n.folds, length.out = nrow(data))
#   folds = sample(folds)
# 
#   for (i in 1:n.folds){
#     indis <- which(folds == i)
#     fold.fit <- sem(fit, data = data[-indis,], estimator = "MLR", meanstructure = T, warn = F)
# 
#     for(l in lambda.seq) {
#       # yhat <- matrix(NA, nrow(data), length(ynames))
#       yhat <- lavPredictYreg(
#         fold.fit,
#         newdata = data[indis,],
#         xnames = xnames,
#         ynames = ynames,
#         regtype = 'ridge',
#         lambda = l
#       )
#       mse <- mean((as.matrix(data[indis,ynames]) - as.matrix(yhat))^2)
#       results <- rbind(results, data.frame(mse = mse, lambda = l))
#     }
#   }
# 
#   # Group by lambda and determine average MSE per group
#   # results$lambda <- as.factor(results$lambda)
#   avg <- aggregate(results$mse, by = list(results$lambda), FUN=mean)
#   avg <- avg[order(avg[,2]),]
#   names(avg) <- c("lambda", "mse")
# 
#   # clipr::write_clip(results)
#   # clipr::write_clip(avg)
# 
#   return(list(
#     results = as.data.frame(avg),
#     lambda.min = as.numeric(avg[avg$mse == min(avg$mse),'lambda'])
#   ))
# }



library(parallel)

cv.lavPredictYReg <- function(fit, data, xnames, ynames, n.folds = 10, lambda.seq) {
  # Load parallel package
  library(parallel)
  
  # Detect the number of available cores
  n.cores <- detectCores(logical = FALSE)  # Use physical cores
  
  # Set up a cluster using available cores
  cl <- makeCluster(n.cores)
  
  # Export required objects to the cluster
  clusterExport(cl, varlist = c("fit", "data", "xnames", "ynames", "n.folds", "sem", "lavPredictYreg", "lavData", "lav_data_full", "lav_dataframe_vartable", "lav_data_missing_patterns", "lav_predict_y_conditional_mean"), envir = environment())
  
  results <- data.frame(matrix(ncol = 2, nrow = 0, dimnames = list(NULL, c("mse", "lambda"))))
  
  # Create fold assignments
  folds <- rep(1:n.folds, length.out = nrow(data))
  folds <- sample(folds)
  
  # Define the function for processing each fold
  fold_function <- function(i, folds, lambda.seq, fit, data, xnames, ynames) {
    indis <- which(folds == i)
    
    # Fit the model on the training data
    fold.fit <- sem(fit, data = data[-indis,], estimator = "MLR", meanstructure = TRUE, warn = FALSE)
    
    fold_result <- data.frame(mse = numeric(), lambda = numeric())
    
    for (l in lambda.seq) {
      # Get predictions with the current lambda value
      yhat <- lavPredictYreg(
        fold.fit,
        newdata = data[indis,],
        xnames = xnames,
        ynames = ynames,
        regtype = 'ridge',
        lambda = l  # l is defined in the current loop
      )
      
      # Calculate mean squared error
      mse <- mean((as.matrix(data[indis, ynames]) - as.matrix(yhat))^2)
      fold_result <- rbind(fold_result, data.frame(mse = mse, lambda = l))
    }
    
    return(fold_result)
  }
  
  # Parallel loop using parLapply
  fold_results <- parLapply(cl, 1:n.folds, fold_function, folds = folds, lambda.seq = lambda.seq, fit = fit, data = data, xnames = xnames, ynames = ynames)
  
  # Stop the cluster
  stopCluster(cl)
  
  # Combine the results from all folds
  results <- do.call(rbind, fold_results)
  
  # Group by lambda and determine average MSE per group
  avg <- aggregate(results$mse, by = list(results$lambda), FUN = mean)
  avg <- avg[order(avg[,2]), ]
  names(avg) <- c("lambda", "mse")
  
  return(list(
    results = as.data.frame(avg),
    lambda.min = as.numeric(avg[avg$mse == min(avg$mse), 'lambda'])
  ))
}


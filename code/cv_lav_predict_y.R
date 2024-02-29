# Takes a data set, a lavaan model, xnames, ynames, and a sequence of lambdas
# and performs k-fold cross-validation to determine the best lambda
cv.lavPredictYReg <- function(fit, data, xnames, ynames, n.folds = 10, lambda.seq) {
  
  results <- data.frame(matrix(ncol=2,nrow=0, dimnames=list(NULL, c(
    "mse", "lambda")
  )))

  folds = rep(1:n.folds, length.out = nrow(data))
  folds = sample(folds)
  
  for (i in 1:n.folds){
    indis <- which(folds == i)
    fold.fit <- sem(fit, data = data[-indis,], estimator = "MLR", meanstructure = T, warn = F)
    
    for(l in lambda.seq) {
      # yhat <- matrix(NA, nrow(data), length(ynames))
      yhat <- lavPredictYreg(
        fold.fit,
        newdata = data[indis,],
        xnames = xnames,
        ynames = ynames,
        regtype = 'ridge',
        lambda = l
      )
      mse <- mean((as.matrix(data[indis,ynames]) - as.matrix(yhat))^2)
      results <- rbind(results, data.frame(mse = mse, lambda = l))
    }
  }
  
  # Group by lambda and determine average MSE per group
  # results$lambda <- as.factor(results$lambda)
  avg <- aggregate(results$mse, by = list(results$lambda), FUN=mean)
  avg <- avg[order(avg[,2]),]
  names(avg) <- c("lambda", "mse")
  
  # clipr::write_clip(results)
  # clipr::write_clip(avg)
  
  return(list(
    results = as.data.frame(avg),
    lambda.min = as.numeric(avg[avg$mse == min(avg$mse),'lambda'])
  ))
}


library(parallel)

# This version uses parallel (multicore) processing
cv.lavPredictYReg <- function(fit, data, xnames, ynames, n.folds = 10, lambda.seq) {
  
  results <- data.frame(matrix(ncol=2,nrow=0, dimnames=list(NULL, c(
    "mse", "lambda")
  )))
  
  folds = rep(1:n.folds, length.out = nrow(data))
  folds = sample(folds)
  
  # Define a function for cross-validation within each fold
  cv_fold <- function(fold_index) {
    indis <- which(folds == fold_index)
    fold.fit <- sem(fit, data = data[-indis,], estimator = "MLR", meanstructure = TRUE, warn = FALSE)
    
    fold_results <- lapply(lambda.seq, function(l) {
      yhat <- lavPredictYreg(
        fold.fit,
        newdata = data[indis,],
        xnames = xnames,
        ynames = ynames,
        regtype = 'ridge',
        lambda = l
      )
      mse <- mean((as.matrix(data[indis, ynames]) - as.matrix(yhat))^2)
      return(data.frame(mse = mse, lambda = l))
    })
    
    return(do.call(rbind, fold_results))
  }
  
  # Perform parallel processing for cross-validation
  cl <- makeCluster(detectCores())  # Use all available cores
  clusterExport(cl, c("xnames", "ynames", "sem", "lavPredictYreg", "lavData", "lav_data_full", "lav_dataframe_vartable", "lav_data_missing_patterns", "lav_predict_y_conditional_mean"))  # Export necessary functions
  fold_results <- parLapply(cl, 1:n.folds, cv_fold)
  stopCluster(cl)
  
  results <- do.call(rbind, fold_results)
  
  # Group by lambda and determine average MSE per group
  avg <- aggregate(results$mse, by = list(results$lambda), FUN=mean)
  avg <- avg[order(avg[,2]),]
  names(avg) <- c("lambda", "mse")
  
  return(list(
    results = as.data.frame(avg),
    lambda.min = as.numeric(avg[avg$mse == min(avg$mse),'lambda'])
  ))
}

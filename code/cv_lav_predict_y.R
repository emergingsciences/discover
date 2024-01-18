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

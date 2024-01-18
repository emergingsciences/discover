library(cvsem)

# Model 1 ----
mod <- hc.mod
# mod <- NULL
mod <- paste0(mod, "\n", 'oneness =~ psybliss21 + psybliss18 + psybliss22 + psybliss19')
mod <- paste0(mod, "\n", 'altruism =~ pg30 + pg10 + pg45 + pg33 + pg34')
mod <- paste0(mod, "\n", 'selfless =~ oneness + altruism')
mod <- paste0(mod, "\n", 'g =~ unityconsc + bliss + insight + energy + light')
mod <- paste0(mod, "\n", 'g ~ selfless')

# Model 2 ----
mod2 <- hc.mod
# mod2 <- NULL
mod2 <- paste0(mod2, "\n", 'oneness =~ psybliss21 + psybliss18 + psybliss22 + psybliss19')
mod2 <- paste0(mod2, "\n", 'altruism =~ pg30 + pg10 + pg45 + pg33 + pg34 + pg35')
mod2 <- paste0(mod2, "\n", 'selfless =~ oneness + altruism')
mod2 <- paste0(mod2, "\n", 'g =~ unityconsc + bliss + insight + energy + light')
mod2 <- paste0(mod2, "\n", 'g ~ selfless')

cfa.fit1 <- cfa(mod, data=data.num[], ordered = T, estimator = "WLSMV")
# cfa.fit1 <- cfa(mod, data=data.num[], ordered = F, estimator = "MLR")
lavaanPlot(model = cfa.fit1, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stand = TRUE)
fitMeasures(cfa.fit1, c("cfi.robust",	"tli.robust",	"rmsea.robust", "srmr"))
lavResiduals(cfa.fit1)
modindices(cfa.fit1, sort = TRUE)
# summary(cfa.fit1, fit.measures = TRUE, standardized = TRUE)

cfa.fit2 <- cfa(mod2, data=data.num[], ordered = T, estimator = "WLSMV")
# cfa.fit2 <- cfa(mod2, data=data.num[], ordered = F, estimator = "MLR")
lavaanPlot(model = cfa.fit2, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stand = TRUE)
fitMeasures(cfa.fit2, c("cfi.robust",	"tli.robust",	"rmsea.robust", "srmr"))
lavResiduals(cfa.fit2)
modindices(cfa.fit2, sort = TRUE)
# summary(cfa.fit2, fit.measures = TRUE, standardized = TRUE)


out <- bootstrapLavaan(cfa.fit1, R = 2, verbose = T, FUN = function(x){
  ## get all parameter estimates, including standardized
  pts <- parameterestimates(x, standardized = T) 
  return(pts[22:24,"std.all"]) ## select the rows for the correlations, the standardized
})


out <- bootstrapLavaan(cfa.fit1, R = 1000, FUN = function(x) {
  standardizedSolution(x)$est.std }, verbose = TRUE)
std <- standardizedSolution(cfa.fit1)

write.csv(out, file = paste("outputs/", "selflessmimic", "-loadings.csv", sep = ''))
write.csv(std, file = paste("outputs/", "std", ".csv", sep = ''))

quantile(na.omit(out[,29]), c(0.025, 0.975))
quantile(na.omit(out[,30]), c(0.025, 0.975))

cfa.fit1 <- cfa(mod, data=data.num[], ordered = F, estimator = "MLR")
lavaanPlot(model = cfa.fit1, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stand = TRUE)
modindices(cfa.fit1, sort = TRUE)

  # KL Divergence
models <- cvgather(mod, mod2)
cvsem(data.num[-train,], Models = models, k = 5)

# ANOVA / LRT Test
cfa.fit1 <- cfa(mod, data=data.num[-train,], ordered = T, estimator="WLSMV")
cfa.fit2 <- cfa(mod2, data=data.num[-train,], ordered = T, estimator="WLSMV")
lavTestLRT(cfa.fit1, cfa.fit2)


# Bootstrap
result_df <- data.frame(
  metric = character(0),
  'q2.5' = numeric(0),
  'q97.5' = numeric(0),
  'run' = integer(0)
)

# Initialize a global variable to keep track of the run number
run_number <- 0

# cfa.fit1 <- cfa(mod, data=data.num, ordered = T, estimator = "WLSMV") # WLSMV - Confirmatory Factor Analysis p. 354
# cfa.fit2 <- cfa(mod2, data=data.num, ordered = T, estimator = "WLSMV") # WLSMV - Confirmatory Factor Analysis p. 354

boot <- bootstrapLavaan(cfa.fit1, R = 100, FUN=fitMeasures)
append_bootstrap_quantiles(boot)
boot2 <- bootstrapLavaan(cfa.fit2, R = 100, FUN=fitMeasures)
append_bootstrap_quantiles(boot2)
quantile(boot[,"cfi"], c(0.025, 0.975))
quantile(boot[,"tli"], c(0.025, 0.975))
quantile(boot[,"rmsea"], c(0.025, 0.975))
quantile(boot[,"cfi.scaled"], c(0.025, 0.975))
quantile(boot[,"tli.scaled"], c(0.025, 0.975))
quantile(boot[,"rmsea.scaled"], c(0.025, 0.975))
quantile(boot[,"srmr"], c(0.025, 0.975))


# Reset DF
result_df <- result_df[result_df$run == 1,]
run_number
run_number <- 1

result_df <- round_df(result_df, 3)

round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  df[,nums] <- round(df[,nums], digits = digits)
  (df)
}





append_bootstrap_quantiles <- function(boot, quantiles = c(0.025, 0.975)) {
  # Increment the run number with each function call
  run_number <<- run_number + 1
  
  # Create an empty data frame for the current run
  current_run <- data.frame(
    metric = character(0),
    'q2.5' = numeric(0),
    'q97.5' = numeric(0),
    'run' = integer(0)
  )
  
  # Specify the metrics
  metrics <- c("cfi.scaled", "tli.scaled", "rmsea.scaled", "srmr", "cfi.robust", "tli.robust", "rmsea.robust")
  
  # Loop through the metrics to append rows for the current run
  for (metric in metrics) {
    current_run <- rbind(current_run, data.frame(
      metric = metric,
      'q2.5' = round(quantile(boot[, metric], quantiles[1]), 3),
      'q97.5' = round(quantile(boot[, metric], quantiles[2]), 3),
      'run' = run_number
    ))
    rownames(current_run) <- NULL
  }
  
  # Append the current run to the global result_df
  result_df <<- rbind(result_df, current_run)
  
  return(current_run)
}


## K-fold Cross-Validation ----
kfold<- function(dats, n.folds, reps){
  results <- data.frame(matrix(ncol=5,nrow=0, dimnames=list(NULL, c(
    "model", "cfi", "tli", "rmsea", "srmr")
  )))     
  
  folds = rep(1:n.folds, length.out = nrow(dats)) 
  
  for(r in 1:reps) {
    folds = sample(folds) # Random permutation
    print(paste("Repetition",r))
    
    for (i in 1:n.folds){
      indis <- which(folds == i)
      print(paste("Fitting on fold with", length(indis), "rows"))
      
      # Use the fold and fit model 1 and model 2 on it
      
      # print(nrow(test))
      cfa.fit1 <- cfa(mod, data=dats[indis,], ordered = T, estimator = "WLSMV")
      cfa.fit2 <- cfa(mod2, data=dats[indis,], ordered = T, estimator = "WLSMV")
      
      fit.df <- data.frame(
        model = "model1",
        cfi = fitmeasures(cfa.fit1, "cfi.robust"),
        tli = fitmeasures(cfa.fit1, "tli.robust"),
        rmsea = fitmeasures(cfa.fit1, "rmsea.robust"),
        srmr = fitmeasures(cfa.fit1, "srmr")
      )
      
      fit.df2 <- data.frame(
        model = "model2",
        cfi = fitmeasures(cfa.fit2, "cfi.robust"),
        tli = fitmeasures(cfa.fit2, "tli.robust"),
        rmsea = fitmeasures(cfa.fit2, "rmsea.robust"),
        srmr = fitmeasures(cfa.fit2, "srmr")
      )
      
      results <- rbind(results, fit.df)
      results <- rbind(results, fit.df2)
      rownames(results) = NULL
    }    
  }
  
  return(as.data.frame(results))
}

results <- kfold(data.num, 2, 50)

ggplot(results, aes(x=model, y=cfi, fill=factor(model))) +
  geom_boxplot(aes(group = factor(model))) + 
  geom_jitter(width = 0.05, height = 0, colour = rgb(0,0,0,.3)) + 
  xlab("Model") + ylab("CFI") + 
  theme(legend.position="none") +
  scale_fill_grey(start=.3,end=.7)

round(quantile(results[,"cfi"], c(0.025, .5, 0.975)), 2)

ggplot(results, aes(x=model, y=tli, fill=factor(model))) +
  geom_boxplot(aes(group = factor(model))) + 
  geom_jitter(width = 0.05, height = 0, colour = rgb(0,0,0,.3)) + 
  xlab("Model") + ylab("TLI") + 
  theme(legend.position="none") +
  scale_fill_grey(start=.3,end=.7)

round(quantile(results[,"tli"], c(0.025, .5, 0.975)), 2)

ggplot(results, aes(x=model, y=rmsea, fill=factor(model))) +
  geom_boxplot(aes(group = factor(model))) + 
  geom_jitter(width = 0.05, colour = rgb(0,0,0,.3)) +
  xlab("Model") + ylab("RMSEA") + 
  theme(legend.position="none") +
  scale_fill_grey(start=.3,end=.7)

round(quantile(results[,"rmsea"], c(0.025, .5, 0.975)), 2)

ggplot(results, aes(x=model, y=srmr, fill=factor(model))) +
  geom_boxplot(aes(group = factor(model))) + 
  geom_jitter(width = 0.05, colour = rgb(0,0,0,.3)) +
  xlab("Model") + ylab("SRMR") + 
  theme(legend.position="none") +
  scale_fill_grey(start=.3,end=.7)

round(quantile(results[,"srmr"], c(0.025, .5, 0.975)), 2)

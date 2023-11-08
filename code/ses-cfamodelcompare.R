mod <- NULL
# mod <- paste0(mod, "\n", 'g =~ unityconsc + bliss + insight + energy + light')
mod <- paste0(mod, "\n", 'unityconsc =~ mystical6 + mystical22 + mystical25 + spiritual26 + mystical15 + mystical8 + mystical13 + mystical10')
# mod <- paste0(mod, "\n", 'consc =~ mystical6 + mystical2 + mystical25 + mystical15')
# mod <- paste0(mod, "\n", 'unity =~ mystical8 + mystical13 + mystical10')
mod <- paste0(mod, "\n", 'bliss =~ mystical5 + mystical7 + mystical4')
mod <- paste0(mod, "\n", 'insight =~ spiritual3 + spiritual2')
mod <- paste0(mod, "\n", 'energy =~ psyphys5 + psyphys3 + psyphys9')
mod <- paste0(mod, "\n", 'light =~ psyphys11 + psyphys1')
mod <- paste0(mod, "\n")


mod2 <- NULL
# mod2 <- paste0(mod2, "\n", 'g =~ unityconsc + bliss + insight + energy + light')
mod2 <- paste0(mod2, "\n", 'unityconsc =~ mystical6 + mystical22 + mystical25 + mystical15 + mystical8 + mystical13 + mystical10')
# mod2 <- paste0(mod2, "\n", 'consc =~ mystical6 + mystical2 + mystical25 + mystical15')
# mod2 <- paste0(mod2, "\n", 'unity =~ mystical8 + mystical13 + mystical10')
mod2 <- paste0(mod2, "\n", 'bliss =~ mystical5 + mystical7 + mystical4')
mod2 <- paste0(mod2, "\n", 'insight =~ spiritual3 + spiritual2 + spiritual26')
mod2 <- paste0(mod2, "\n", 'energy =~ psyphys5 + psyphys3 + psyphys9')
mod2 <- paste0(mod2, "\n", 'light =~ psyphys11 + psyphys1')
mod2 <- paste0(mod2, "\n")

# mod <- paste0(mod, "\n", 'oneness =~ psybliss21 + psybliss18 + psybliss22 + psybliss19')
# mod <- paste0(mod, "\n", 'altruism =~ psygrowth30 + psygrowth10 + psygrowth45 + psygrowth33 + psygrowth34')
# # mod <- paste0(mod, "\n", 'selfless =~ oneness + altruism')

mod <- NULL
mod <- paste0(mod, "\n", 'oneness =~ psybliss21 + psybliss18 + psybliss22 + psybliss19')
mod <- paste0(mod, "\n", 'altruism =~ psygrowth30 + psygrowth10 + psygrowth45 + psygrowth33 + psygrowth34 + psygrowth35')
mod <- paste0(mod, "\n", hc.mod)
mod <- paste0(mod, "\n", 'selfless =~ oneness + altruism')
# mod <- paste0(mod, "\n", 'g ~ selfless')
mod <- paste0(mod, "\n", 'g =~ unityconsc + bliss + insight + energy + light')

mod2 <- NULL
mod2 <- paste0(mod2, "\n", 'oneness =~ psybliss21 + psybliss18 + psybliss22 + psybliss19')
mod <- paste0(mod, "\n", 'altruism =~ psygrowth30 + psygrowth10 + psygrowth45 + psygrowth33 + psygrowth34 + psygrowth35')
mod2 <- paste0(mod2, "\n", hc.mod)
mod2 <- paste0(mod2, "\n", 'selfless =~ oneness + altruism')
mod2 <- paste0(mod2, "\n", 'g =~ unityconsc + bliss + insight + energy + light')
# mod2 <- paste0(mod2, "\n", 'oneness ~ altruism')

cfa.fit1 <- cfa(mod, data=data.num[], ordered = T, estimator = "WLSMV", std.lv = T) # WLSMV - Confirmatory Factor Analysis p. 354
# cfa.fit1 <- cfa(mod, data=data.num[], ordered = F, estimator = "MLR", std.lv = T)
cfa.fit2 <- cfa(mod2, data=data.num[], ordered = T, estimator = "WLSMV") # WLSMV - Confirmatory Factor Analysis p. 354
summary(cfa.fit1, fit.measures = TRUE, standardized = TRUE)
summary(cfa.fit2, fit.measures = TRUE, standardized = TRUE)
fitMeasures(cfa.fit1, c("cfi.scaled", "tli.scaled", "rmsea.scaled"))
fitMeasures(cfa.fit2, c("cfi.scaled", "tli.scaled", "rmsea.scaled"))

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
cfa.fit1 <- cfa(mod, data=data.num[-train,], ordered = F, estimator="MLR")
cfa.fit2 <- cfa(mod2, data=data.num[-train,], ordered = F, estimator="MLR")
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

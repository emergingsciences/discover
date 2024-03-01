# ~~~~~~~~~~~~~~~~~~~~~~ ----
# # # # # # # # # # # # # # # #
# Selflessness SEM / MIMIC ----
# # # # # # # # # # # # # # # #

set.seed(1234567)

# ETL
data.num <- extract.numeric.columns.by.regex(ses.data, paste0('mystical\\d+|spiritual\\d+|psyphys\\d+|psygrowth\\d+|psybliss\\d+')) # No gate
data.num <- na.omit(data.num)
nrow(data.num)
names(data.num)[names(data.num) == 'psygrowth30'] <- 'pg30'
names(data.num)[names(data.num) == 'psygrowth10'] <- 'pg10'
names(data.num)[names(data.num) == 'psygrowth45'] <- 'pg45'
names(data.num)[names(data.num) == 'psygrowth33'] <- 'pg33'
names(data.num)[names(data.num) == 'psygrowth34'] <- 'pg34'
names(data.num)[names(data.num) == 'psygrowth35'] <- 'pg35' # Under test
names(data.num)[names(data.num) == 'psygrowth5'] <- 'pg5' # Under test
names(data.num)[names(data.num) == 'psygrowth14'] <- 'pg14' # Under test


# Factor analysis

# fa.res <- ses.qgroup("psygrowth", grepmatch, parallel = T, omit.na = T)
# fa.res <- ses.qgroup("psygrowthbliss", "psygrowth\\d+|psybliss\\d+", parallel = T, omit.na = T)
fa.res <- ses.qgroup("psygrowthbliss", "psygrowth\\d+|psybliss\\d+", nfactors = 9, omit.na = T)

# mod <- NULL # Start the model here - to check local fit, etc.
mod <- hc.mod
# mod <- NULL
mod <- paste0(mod, "\n", 'oneness =~ psybliss21 + psybliss18 + psybliss23')
mod <- paste0(mod, "\n", 'altruism =~ pg30 + pg34 + pg35')
mod <- paste0(mod, "\n", 'selfless =~ oneness + altruism')
mod <- paste0(mod, "\n", 'g =~ unityconsc + bliss + insight + energy + light')
mod <- paste0(mod, "\n", 'g ~ selfless')
# non.reg <- mod
cfa_selfless <- cfa(mod, data=data.num, ordered = F, estimator = "MLR")
cfa_selfless <- cfa(mod, data=data.num, ordered = T, estimator = "WLSMV", std.lv = T)
# modindices(cfa, sort = TRUE)
fitMeasures(cfa_selfless, c("cfi.robust",	"tli.robust",	"rmsea.robust", "srmr"))
summary(cfa_selfless, fit.measures = TRUE, standardized = TRUE)
lavaanPlot(model = cfa_selfless, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stand = TRUE)
lavInspect(cfa_selfless, "cov.lv")
cov2cor(lavInspect(cfa_selfless, what = "est")$psi)

EFAtools::SL(cfa_selfless, g_name = "selfless") # Must have higher order factor 'g'
EFAtools::OMEGA(cfa_selfless, g_name = "selfless") # Must have higher order factor 'g'



## Predictor K-Fold and RMSEP ----

xnames <- c("psybliss21", "psybliss23", "psybliss18", "pg30", "pg34", "pg35")
ynames <- c("mystical6", "mystical22", "mystical25", "mystical15", "mystical8", "mystical13", "mystical10", "mystical5", "mystical7", "mystical4", "spiritual3", "spiritual2", "spiritual26", "psyphys5", "psyphys3", "psyphys9", "psyphys11", "psyphys1")
# ynames <- c("mystical6", "mystical22", "mystical25", "mystical15", "mystical8", "mystical13", "mystical10")
# ynames <- c("mystical22")

library(glmnet)
source(paste0(getwd(), "/code/lav_predict_y_reg.R"))
source(paste0(getwd(), "/code/cv_lav_predict_y.R"))
source(paste0(getwd(), "/code/lav_data_patterns.R"))
source(paste0(getwd(), "/code/lav_data.R"))
source(paste0(getwd(), "/code/lav_dataframe.R"))

# Attempt to regularize the S matrix (failed)
# library(regsem)
# extractMatrices(cfa_selfless)$S
# regsem.res <- cv_regsem(cfa_selfless, type = "alasso", pars_pen = c(24:53), lambda.start = 0, jump = .1, n.lambda = 20)
# summary(regsem.res)
# regsem.res$fits
# plot(regsem.res, show.minimum="BIC")

ypred_results <- ses.pred_kfold(
  data.num,
  # data.num[sample(nrow(data.num), 200),],
  mod,
  n.folds = 10,
  reps = 100,
  xnames,
  ynames,
  lambda.seq = seq(from = 0, to = 2, by = .2)
)
# saveRDS(ypred_results, file = "outputs/temp.rds")

# n = 50, Full HC/Selflessness model, predicting unityconsc only by seflessness
ypred_results <- readRDS(file = "outputs/selfless-unityconsc-n50-fullhcmodel.rds")

# n = 100, Full HC/Selflessness model, predicting unityconsc only by seflessness
ypred_results <- readRDS(file = "outputs/selfless-unityconsc-n100-fullhcmodel.rds")

# ** n = 200, Full HC/Selflessness model, predicting unityconsc only by seflessness
ypred_results <- readRDS(file = "outputs/selfless-unityconsc-n200-fullhcmodel.rds")

# n = 300, Full HC/Selflessness model, predicting unityconsc only by seflessness
ypred_results <- readRDS(file = "outputs/selfless-unityconsc-n300-fullhcmodel.rds")

# n = 400, Full HC/Selflessness model, predicting unityconsc only by seflessness
ypred_results <- readRDS(file = "outputs/selfless-unityconsc-n400-fullhcmodel.rds")

# n = 500, Full HC/Selflessness model, predicting unityconsc only by seflessness
ypred_results <- readRDS(file = "outputs/selfless-unityconsc-n500-fullhcmodel.rds")

# n = 50, Unity-Consciousness-only factor model, predicting unityconsc only by seflessness
ypred_results <- readRDS(file = "outputs/selfless-unityconsc-n50-ucmodel.rds")

# ** n = 100, Unity-Consciousness-only factor model, predicting unityconsc only by seflessness
ypred_results <- readRDS(file = "outputs/selfless-unityconsc-n100-ucmodel.rds")


# plotdata <- ypred_results
# plotdata <- ypred_results[ypred_results$model == 1 | ypred_results$model == 2 , ]
ggplot(ypred_results, aes(x = factor(model), y = rmsep, fill = factor(model))) +
  geom_boxplot(fill = "grey", aes(group = factor(model))) +
  geom_jitter(width = 0.05, height = 0, colour = rgb(0, 0, 0, 0.3)) +
  scale_x_discrete(labels=c("OOS", "ROSEM", "REG", "MLR")) +
  xlab("Data set") +
  ylab("RMSEp") +
  theme(legend.position="none") + # , axis.title.x=element_blank(), axis.text.x=element_blank()
  scale_fill_grey(start = 0.3, end = 0.7)


sd(ypred_results[ypred_results$model == 1,'rmsep'])
sd(ypred_results[ypred_results$model == 2,'rmsep'])
bartlett.test(
  rmsep ~ model,data = ypred_results[ypred_results$model == 1 | ypred_results$model == 2,]
)
t.test(rmsep ~ model, data = ypred_results[ypred_results$model == 1 | ypred_results$model == 2,])

round(quantile(ypred_results[ypred_results$model == "SEM","rmsep"], c(.025, .5, .975)), 3)
round(quantile(ypred_results[ypred_results$model == "SEMREG","rmsep"], c(.025, .5, .975)), 3)

mean(ypred_results[ypred_results$model == 'SEM','rmsep'])

ggplot(ypred_results, aes(x = model, y = rmsep, fill = factor(model))) +
  geom_boxplot(fill = "grey", aes(group = factor(model))) +
  geom_jitter(width = 0.05, height = 0, colour = rgb(0, 0, 0, 0.3)) +
  xlab("Approach") +
  ylab("RMSEp") +
  theme(legend.position="none") +
  # theme(legend.position="none", axis.title.x=element_blank(), axis.text.x=element_blank()) +
  scale_fill_grey(start = 0.3, end = 0.7)



ypred_item_results <- ses.pred_item_kfold(
  data.num,
  # data.num[sample(nrow(data.num), 200),],
  mod,
  n.folds = 10,
  reps = 100,
  xnames,
  ynames,
  lambda.seq = seq(from = 0, to = 2, by = .2)
)

saveRDS(ypred_item_results, file = "outputs/ITEM_selfless-unityconsc-n500-fullhcmodel.rds")
# ypred_item_results <- readRDS(file = "outputs/ITEM_selfless-unityconsc-n50-fullhcmodel.rds")

ggplot(ypred_item_results$results, aes(x = factor(model), y = rmsep, fill = factor(model))) +
  geom_boxplot(fill = "grey", aes(group = factor(model))) +
  geom_jitter(width = 0.05, height = 0, colour = rgb(0, 0, 0, 0.3)) +
  scale_x_discrete(labels=c("UnityConsc", "Bliss", "Insight", "Energy", "Light")) +
  xlab("Data set") +
  ylab("RMSEp") +
  theme(legend.position="none") + # , axis.title.x=element_blank(), axis.text.x=element_blank()
  scale_fill_grey(start = 0.3, end = 0.7)

# Get the number of matrices in the list
num_matrices <- length(ypred_item_results$predictions)
# Initialize a matrix to store the sum of all predictions
summed_matrix <- matrix(0, nrow = nrow(ypred_item_results$predictions[[1]]), ncol = ncol(ypred_results$predictions[[1]]))
# Sum all matrices element-wise
for (i in 1:num_matrices) {
  summed_matrix <- summed_matrix + ypred_item_results$predictions[[i]]
}
# Calculate the average by dividing by the total number of matrices
average_matrix <- summed_matrix / num_matrices

# Plot actual vs predicted values
ggplot(average_matrix, aes(x = Actual, y = Residuals)) +
  # geom_point(color = "blue", shape = 16) +
  geom_point() +
  labs(x = "Actuals", y = "Residuals", title = "Actuals vs Residuals")




## Model K-fold Cross-Validation of Fit Measures ----
kfold<- function(dats, mod, n.folds, reps){
  print(paste("Model: ", mod))
  
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
      
      cfa.fit <- cfa(mod, data=dats[indis,], ordered = F, estimator = "MLR")
      
      fit.df <- data.frame(
        model = "Selfless and HC",
        cfi = fitmeasures(cfa.fit, "cfi.robust"),
        tli = fitmeasures(cfa.fit, "tli.robust"),
        rmsea = fitmeasures(cfa.fit, "rmsea.robust"),
        srmr = fitmeasures(cfa.fit, "srmr")
      )
      
      results <- rbind(results, fit.df)
      rownames(results) = NULL
    }    
  }
  
  return(as.data.frame(results))
}

set.seed(1234567)
self_results <- kfold(data.num, mod = mod, n.folds = 2, reps = 100)
set.seed(NULL)

ggplot(self_results, aes(x=model, y=cfi, fill=factor(model))) +
  geom_boxplot(fill = "grey", aes(group = factor(model))) + 
  geom_jitter(width = 0.05, height = 0, colour = rgb(0,0,0,.3)) + 
  xlab("Model") + ylab("CFI") + 
  theme(legend.position="none", axis.title.x=element_blank(), axis.text.x=element_blank()) +
  scale_fill_grey(start=.3,end=.7)

round(quantile(results[,"cfi"], c(0.025, .5, 0.975)), 2)

ggplot(self_results, aes(x=model, y=tli, fill=factor(model))) +
  geom_boxplot(fill = "grey", aes(group = factor(model))) + 
  geom_jitter(width = 0.05, height = 0, colour = rgb(0,0,0,.3)) + 
  xlab("Model") + ylab("TLI") + 
  theme(legend.position="none", axis.title.x=element_blank(), axis.text.x=element_blank()) +
  scale_fill_grey(start=.3,end=.7)

round(quantile(results[,"tli"], c(0.025, .5, 0.975)), 2)

ggplot(self_results, aes(x=model, y=rmsea, fill=factor(model))) +
  geom_boxplot(fill = "grey", aes(group = factor(model))) + 
  geom_jitter(width = 0.05, colour = rgb(0,0,0,.3)) +
  xlab("Model") + ylab("RMSEA") + 
  theme(legend.position="none", axis.title.x=element_blank(), axis.text.x=element_blank()) +
  scale_fill_grey(start=.3,end=.7)

round(quantile(self_results[,"rmsea"], c(0.025, .5, 0.975)), 2)

ggplot(self_results, aes(x=model, y=srmr, fill=factor(model))) +
  geom_boxplot(fill = "grey", aes(group = factor(model))) + 
  geom_jitter(width = 0.05, colour = rgb(0,0,0,.3)) +
  xlab("Model") + ylab("SRMR") + 
  theme(legend.position="none", axis.title.x=element_blank(), axis.text.x=element_blank()) +
  scale_fill_grey(start=.3,end=.7)

round(quantile(self_results[,"srmr"], c(0.025, .5, 0.975)), 2)

write.csv(self_results, file="outputs/selfless-kfold.csv")


cfa <- NULL
cfa <- cfa(mod, data=data.num, ordered = TRUE, estimator = "WLSMV", std.lv = TRUE, warn = FALSE)
tmp <- lavInspect(cfa, "std.coef")
tmp <- coef(cfa)


parameterestimates(cfa, standardized = TRUE)



#
## Selflessness Bootstrap parameter estimates ----
#

getParamEstimates <- function(data, idx, n) {
  # Get "default" coefficient vector in case of non-convergence
  cfa <- cfa(mod, ordered = TRUE, estimator = "WLSMV", std.lv = TRUE, warn = FALSE)
  blank_coef <- coef(cfa)
  cfa <- cfa(mod, data = data[idx,], ordered = TRUE, estimator = "WLSMV", std.lv = TRUE, warn = FALSE)
  # Check for non-convergence
  if (!lavInspect(cfa, "converged")) {
    cat("Non-convergence detected. Filling return vector with NA.\n")
    return(rep(NA, length(blank_coef[1:n]))) # Range to ensure same-length vectors are returned in the case of no values for ordinals
  } else {
    coef <- standardizedsolution(cfa)$est.std[1:n]
    return(coef) # Range to ensure same-length vectors are returned in the case of no values for ordinals
  }
}

set.seed(1234567)
nrow(data.num)
# Run initial bootstrap
n <- 32 # Number of rows from the lavaan parameter estimates table
casesNeeded <- 1000
casesFound <- 0
boot.self <- boot(data.num, getParamEstimates, R = casesNeeded, n = n)

# Update boot object with completed cases and counts
casesFound <- sum(complete.cases(boot.self$t))
boot.self$t <- boot.self$t[complete.cases(boot.self$t) , ]
boot.self$R <- casesFound
# Fill with number of cases needed in case of a non-convergence
while(casesNeeded > casesFound) {
  newboot <- boot(data.num, getParamEstimates, R = 1, n = n)
  if(sum(complete.cases(newboot$t)) == 1) {
    boot.self$t <- rbind(boot.self$t, newboot$t)
    casesFound <- sum(complete.cases(boot.self$t))
    boot.self$R <- casesFound
  }
}
set.seed(NULL)

# saveRDS(boot.self, file = "outputs/boot_selfless_params.rds")
boot.selfless.params <- readRDS(file = "outputs/boot_selfless_params.rds")

# Format empirical values and CI's, e.g., ".90 [.89, .91]"
cfa <- cfa(mod, data=data.num, ordered = T, estimator = "WLSMV", std.lv = T)
parameterestimates(cfa, standardized = T)
formatLoading <- function(num) { sub("^0+", "", sprintf("%.2f", num)) } # No leading spaces, 2 decimal places
for(i in 1:32) {
  bootci <- boot.ci(boot.self, type = "bca", index=i)
  print(paste0(
    parameterestimates(cfa)$lhs[i],
    parameterestimates(cfa)$op[i],
    parameterestimates(cfa)$rhs[i], ": ",
    formatLoading(boot.self$t0[i]),
    " [", formatLoading(bootci$bca[4]), ", ", formatLoading(bootci$bca[5]), "]"
  ))
}
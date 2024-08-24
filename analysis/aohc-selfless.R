# # # # # # # # # # # # # # # #
# Selflessness SEM / MIMIC
# # # # # # # # # # # # # # # #

set.seed(1234567)

# ETL
data.num <- extract.numeric.columns.by.regex(ses.data, paste0('mystical\\d+|spiritual\\d+|psyphys\\d+|psygrowth\\d+|psybliss\\d+')) # No gate
data.num$CLUST <- results$CLUST
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


## Factor analysis including SL ----

altruism.fields <- c('psybliss21','psybliss18','psybliss23','psybliss22','psybliss27','psybliss26','psybliss19','psybliss20','psybliss28','psybliss14','pg30','pg45','pg33','pg34','pg35','pg10')
self.data <- data.num[,altruism.fields]

library(psych)
fa.parallel(self.data, n.iter = 1000)
fa.res <- fa(self.data, 3, rotate = "oblimin", fm = "pa")

library(fungible)
Btarg <- matrix(c(1, 1, 0,
                  1, 1, 0,
                  1, 1, 0,
                  1, 1, 0,
                  1, 1, 0,
                  1, 1, 0,
                  1, 1, 0,
                  1, 1, 0,
                  1, 1, 0,
                  1, 1, 0,
                  1, 0, 1,
                  1, 0, 1,
                  1, 0, 1,
                  1, 0, 1,
                  1, 0, 1,
                  1, 0, 1 ), 16, 3, byrow=TRUE)
bifad.res <- fungible::BiFAD(
  R = cor(self.data),
  # B = Btarg,
  numFactors = 3,
  facMethod = "fapa",
  rotate = "oblimin"
)
clipr::write_clip(bifad.res$BstarSL)
clipr::write_clip(bifad.res$BstarFR)
clipr::write_clip(names(self.data))

sli.res <- fungible::SLi(
  R = cor(self.data),
  numFactors = c(2,1),
  facMethod = "fapa"
)
clipr::write_clip(sli.res$loadings)

# fa.res <- ses.qgroup("psygrowth", grepmatch, parallel = T, omit.na = T)
# fa.res <- ses.qgroup("psygrowthbliss", "psygrowth\\d+|psybliss\\d+", parallel = T, omit.na = T)
fa.res <- ses.qgroup("psygrowthbliss", "psygrowth\\d+|psybliss\\d+", nfactors = 9, omit.na = T)


# Selflessness Model Creation ----

mod <- '
oneness =~ psybliss21 + psybliss18 + psybliss23
altruism =~ pg30 + pg34 + pg35
selfless =~ oneness + altruism

# oneness =~ psybliss21 + psybliss18 + psybliss23
# altruism =~ pg30 + pg34 + pg35
# selfless =~ psybliss21 + psybliss18 + psybliss23 + pg30 + pg34 + pg35
# selfless ~~ 0*oneness + 0*altruism
# oneness ~~ 0*altruism

g ~ selfless
'
mod <- paste0(mod, "\n", hc.HO)

cfa_selfless <- cfa(mod, std.lv = T, data=data.num, ordered = F, estimator = "MLR")
cfa_selfless <- cfa(mod, data=data.num, ordered = T, estimator = "WLSMV", std.lv = T)

# Confidence intervals for regression
# Manually compute confidence intervals for standardized estimates
std_estimates <- standardizedSolution(cfa_selfless)

summary(cfa_selfless, fit.measures = TRUE, standardized = TRUE)
# modindices(cfa, sort = TRUE)
lavaanPlot(model = cfa_selfless, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stand = TRUE)
lavInspect(cfa_selfless, "cov.lv")
cov2cor(lavInspect(cfa_selfless, what = "est")$psi)

clipr::write_clip(fitMeasures(cfa_selfless, c("cfi",	"tli", "rmsea", "cfi.scaled",	"tli.scaled",	"rmsea.scaled", "cfi.robust",	"tli.robust",	"rmsea.robust", "srmr", "chisq.scaled", "df.scaled")))
stats <- fitMeasures(cfa_selfless, c("cfi.robust",	"tli.robust",	"rmsea.robust", "srmr"))
clipr::write_clip(paste(c("CFI = ", gsub("^0", "", as.character(round(stats[1], 3))),
                          ", TLI = ", gsub("^0", "", as.character(round(stats[2], 3))),
                          ", RMSEA = ", gsub("^0", "", as.character(round(stats[3], 3))),
                          ", SRMR = ", gsub("^0", "", as.character(round(stats[4], 3)))
), collapse = ""))

EFAtools::SL(cfa_selfless, g_name = "selfless") # Must have higher order factor 'g'
EFAtools::OMEGA(cfa_selfless, g_name = "selfless") # Must have higher order factor 'g'

## Model Cross-Validation ----

res1 <- kfold.res

kfold.res <- ses.kfold(data.num, 2, 100, mod)
ggplot(kfold.res, aes(x=model, y=cfi, fill=factor(model)), environment = environment()) +
  geom_boxplot(fill = "grey", aes(group = factor(model))) + 
  geom_jitter(width = 0.05, height = 0, colour = rgb(0,0,0,.3)) + 
  xlab("Higher Concsiousness") + ylab("Robust CFI") + 
  theme(legend.position="none", axis.title.x=element_blank(), axis.text.x=element_blank()) +
  scale_fill_grey(start=.3,end=.7)

ggplot(kfold.res, aes(x=model, y=tli, fill=factor(model)), environment = environment()) +
  geom_boxplot(fill = "grey", aes(group = factor(model))) + 
  geom_jitter(width = 0.05, height = 0, colour = rgb(0,0,0,.3)) + 
  xlab("Higher Concsiousness") + ylab("Robust TLI") + 
  theme(legend.position="none", axis.title.x=element_blank(), axis.text.x=element_blank()) +
  scale_fill_grey(start=.3,end=.7)

ggplot(kfold.res, aes(x=model, y=rmsea, fill=factor(model))) +
  geom_boxplot(fill = "grey", aes(group = factor(model))) + 
  geom_jitter(width = 0.05, colour = rgb(0,0,0,.3)) +
  xlab("Higher Concsiousness") + ylab("Robust RMSEA") + 
  theme(legend.position="none", axis.title.x=element_blank(), axis.text.x=element_blank()) +
  scale_fill_grey(start=.3,end=.7)

ggplot(kfold.res, aes(x=model, y=srmr, fill=factor(model))) +
  geom_boxplot(fill = "grey", aes(group = factor(model))) + 
  geom_jitter(width = 0.05, colour = rgb(0,0,0,.3)) +
  xlab("Higher Concsiousness") + ylab("SRMR") + 
  theme(legend.position="none", axis.title.x=element_blank(), axis.text.x=element_blank()) +
  scale_fill_grey(start=.3,end=.7)  

## Congeneric Model of Selflessness ----
mod <- '
oneness =~ psybliss21 + psybliss18 + psybliss23
altruism =~  + pg30 + pg34 + pg35
'
cfa <- sem(mod, std.lv = T, data=data.num, ordered = F,
            estimator = "MLR")
cfa <- sem(mod, std.lv = T, data=data.num, ordered = T,
           estimator = "WLSMV")
fitMeasures(cfa, c("cfi.robust",	"tli.robust",	"rmsea.robust", "srmr", "chisq.scaled", "df.scaled"))
summary(cfa, fit.measures = TRUE, standardized = TRUE)

# Output for table
clipr::write_clip(fitMeasures(cfa, c("cfi",	"tli", "rmsea", "cfi.scaled",	"tli.scaled",	"rmsea.scaled", "cfi.robust",	"tli.robust",	"rmsea.robust", "srmr", "chisq.scaled", "df.scaled")))

# Output for inline stats
stats <- fitMeasures(cfa, c("cfi.robust",	"tli.robust",	"rmsea.robust", "srmr"))
clipr::write_clip(paste(c("CFI = ", gsub("^0", "", as.character(round(stats[1], 3))),
                          ", TLI = ", gsub("^0", "", as.character(round(stats[2], 3))),
                          ", RMSEA = ", gsub("^0", "", as.character(round(stats[3], 3))),
                          ", SRMR = ", gsub("^0", "", as.character(round(stats[4], 3)))
), collapse = ""))


library(fungible)
bi.data.num <- data.num[,c("psybliss21", "psybliss18", "psybliss23", "pg30", "pg34", "pg35")]
bi.data.num <- as.data.frame(lapply(bi.data.num, as.ordered))
BiFAD(polychoric(bi.data.num)$rho, numFactors = 2)

## ESEM Selflessness Model ----
# Unity-Consciousness
efa.fit <- efa(data.num[,c("psybliss21", "psybliss18", "psybliss23", "pg30", "pg34", "pg35")], nfactors = 2)
summary(efa.fit)

self.target <- matrix(c( 1, 0,
                         1, 0,
                         1, 0,
                         0, 1,
                         0, 1,
                         0, 1), nrow = 6, ncol = 2, byrow = TRUE)

mod <- '
efa("efa1")*f1 +
efa("efa1")*f2 =~ psybliss21 + psybliss18 + psybliss23 + pg30 + pg34 + pg35
'
cfa <- sem(mod, std.lv = F, data=data.num, ordered = F,
            estimator = "MLR",
            rotation="geomin")
cfa <- sem(mod, std.lv = F, data=data.num, ordered = F,
           estimator = "MLR",
           rotation="target",
           rotation.args = list(target = self.target))
cfa <- sem(mod, std.lv = F, data=data.num, ordered = T,
           estimator = "WLSMV",
           rotation="geomin")
fitMeasures(cfa, c("cfi.robust",	"tli.robust",	"rmsea.robust", "srmr", "chisq.scaled", "df.scaled"))
summary(cfa, fit.measures = TRUE, standardized = TRUE)

## Selflessness Bifactor ----


# mod1 <- ' # S-1, Oneness = reference factor
# oneness =~ psybliss21 + psybliss18 + psybliss23
# altruism =~ pg30 + pg34 + pg35
# g =~ psybliss21 + psybliss18 + psybliss23 + pg30 + pg34 + pg35
# g ~~ 0*altruism + 0*oneness
# oneness ~~ 0*altruism
# '

mod1 <- ' # S-1, Oneness = reference factor
# oneness =~ psybliss21 + psybliss18 + psybliss23
altruism =~ pg30 + pg34 + pg35
g =~ psybliss21 + psybliss18 + psybliss23 + pg30 + pg34 + pg35
g ~~ 0*altruism
'

mod2 <- ' # S-1, Altruism = reference factor
oneness =~ psybliss21 + psybliss18 + psybliss23
# altruism =~ pg30 + pg34 + pg35
g =~ psybliss21 + psybliss18 + psybliss23 + pg30 + pg34 + pg35

g ~~ 0*oneness
'
cfa1 <- cfa(mod1, std.lv = T, data=data.num, ordered = F, estimator = "MLR", orthogonal = F)
# cfa1 <- cfa(mod1, std.lv = T, data=data.num, ordered = T, estimator = "WLSMV")
fitMeasures(cfa1, c("cfi.robust",	"tli.robust",	"rmsea.robust", "srmr", "chisq.scaled", "df.scaled"))
summary(cfa1, fit.measures = TRUE, standardized = TRUE)
cfa2 <- cfa(mod2, std.lv = F, data=data.num, ordered = F, estimator = "MLR", orthogonal = F)
# cfa2 <- cfa(mod2, std.lv = T, data=data.num, ordered = T, estimator = "WLSMV")
fitMeasures(cfa2, c("cfi.robust",	"tli.robust",	"rmsea.robust", "srmr", "chisq.scaled", "df.scaled"))
summary(cfa2, fit.measures = TRUE, standardized = TRUE)

lavTestLRT(cfa1, cfa2)

selfless.besem <- '
efa("efa1")*g =~ psybliss21 + psybliss18 + psybliss23 + pg30 + pg34 + pg35
efa("efa1")*f1 +
efa("efa1")*f2 =~ psybliss21 + psybliss18 + psybliss23 + pg30 + pg34 + pg35
g ~~ 0*f1 + 0*f2
f1 ~~ 0*f2
'
cfa1 <- sem(selfless.besem, std.lv = F, data=data.num, ordered = F,
            estimator = "MLR",
            orthogonal = T,
            rotation="bigeomin",
            rotation.args = list(orthogonal = T))
fitMeasures(cfa1, c("cfi.robust",	"tli.robust",	"rmsea.robust", "srmr", "chisq.scaled", "df.scaled"))
summary(cfa1, fit.measures = TRUE, standardized = TRUE)

lavTestLRT(cfa1, cfa2)


mod <- '
oneness =~ psybliss21 + psybliss18 + psybliss23
oneness ~ pg30 + pg34 + pg35
'
cfa <- cfa(mod, std.lv = T, data=data.num, ordered = F, estimator = "MLR")
summary(cfa, fit.measures = TRUE, standardized = TRUE)

mod <- '
altruism =~ pg30 + pg34 + pg35
altruism ~ psybliss21 + psybliss18 + psybliss23
'
cfa <- cfa(mod, std.lv = T, data=data.num, ordered = F, estimator = "MLR")
summary(cfa, fit.measures = TRUE, standardized = TRUE)


## Predictor K-Fold and RMSEP ----

xnames <- c("psybliss21", "psybliss23", "psybliss18", "pg30", "pg34", "pg35")
ynames <- c("mystical6", "mystical22", "mystical25", "mystical15", "mystical8", "mystical13", "mystical10", "mystical5", "mystical7", "mystical4", "spiritual3", "spiritual2", "spiritual26", "psyphys5", "psyphys3", "psyphys9", "psyphys11", "psyphys1")
# ynames <- c("mystical6", "mystical22", "mystical25", "mystical15", "mystical8", "mystical13", "mystical10")
# ynames <- c("mystical22")

library(glmnet)
cv.reg <- cv.glmnet(
  as.matrix(data.num[,xnames]),
  as.matrix(data.num[,ynames]),
  family = "mgaussian",
  alpha = 0, # Ridge
)
regmod <- glmnet(
  as.matrix(data.num[,xnames]),
  as.matrix(data.num[,ynames]),
  family = "mgaussian",
  alpha = 0,
  lambda = cv.reg$lambda.min
)
coef(regmod)

library(glmnet)
source(paste0(getwd(), "/code/lav_predict_y_reg.R"))
source(paste0(getwd(), "/code/cv_lav_predict_y.R"))
source(paste0(getwd(), "/code/lav_data_patterns.R"))
source(paste0(getwd(), "/code/lav_data.R"))
source(paste0(getwd(), "/code/lav_dataframe.R"))


ypred_results <- ses.pred_kfold(
  data.num,
  # data.num[sample(nrow(data.num), 200),],
  mod,
  n.folds = 2,
  reps = 4,
  xnames,
  ynames,
  lambda.seq = seq(from = 0, to = 2, by = .2)
)
saveRDS(ypred_results, file = "outputs/temp.rds")
# saveRDS(ypred_results, file = "outputs/selfless-unityconsc-n200-fullhcmodel.rds")

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


## Predict HC based on values of Selflessness ----
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

# saveRDS(ypred_item_results, file = "outputs/ITEM_selfless-unityconsc-n494-fullhcmodel.rds")
ypred_item_results <- readRDS(file = "outputs/ITEM_selfless-unityconsc-n494-fullhcmodel.rds")

ggplot(ypred_item_results$results, aes(x = factor(model), y = rmsep, fill = factor(model))) +
  geom_boxplot(fill = "grey", aes(group = factor(model))) +
  geom_jitter(width = 0.05, height = 0, colour = rgb(0, 0, 0, 0.3)) +
  scale_x_discrete(labels=c("UnityConsc", "Bliss", "Insight", "Energy", "Light")) +
  xlab("Factor") +
  ylab("RMSEp") +
  theme(legend.position="none") + # , axis.title.x=element_blank(), axis.text.x=element_blank()
  scale_fill_grey(start = 0.3, end = 0.7)

# Get the number of matrices in the list
num_matrices <- length(ypred_item_results$predictions)
# Initialize a matrix to store the sum of all predictions
summed_matrix <- matrix(0, nrow = nrow(ypred_item_results$predictions[[1]]), ncol = ncol(ypred_item_results$predictions[[1]]))
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

round(quantile(self_results[,"cfi"], c(0.025, .5, 0.975)), 2)

ggplot(self_results, aes(x=model, y=tli, fill=factor(model))) +
  geom_boxplot(fill = "grey", aes(group = factor(model))) + 
  geom_jitter(width = 0.05, height = 0, colour = rgb(0,0,0,.3)) + 
  xlab("Model") + ylab("TLI") + 
  theme(legend.position="none", axis.title.x=element_blank(), axis.text.x=element_blank()) +
  scale_fill_grey(start=.3,end=.7)

round(quantile(self_results[,"tli"], c(0.025, .5, 0.975)), 2)

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
  bootci <- boot.ci(boot.selfless.params, type = "bca", index=i)
  print(paste0(
    parameterestimates(cfa)$lhs[i],
    parameterestimates(cfa)$op[i],
    parameterestimates(cfa)$rhs[i], ": ",
    formatLoading(boot.selfless.params$t0[i]),
    " [", formatLoading(bootci$bca[4]), ", ", formatLoading(bootci$bca[5]), "]"
  ))
}



## Regularized regression model ----

mod <- '
unityconsc =~ mystical6 + mystical22 + mystical25 + mystical15 + mystical8 + mystical13 + mystical10
bliss =~ mystical5 + mystical7 + mystical4
insight =~ spiritual3 + spiritual2 + spiritual26
energy =~ psyphys5 + psyphys3 + psyphys9
light =~ psyphys11 + psyphys1
'

cfa <- cfa(mod, data=data.num, ordered = T, estimator = "WLSMV")
pred <- lavPredict(cfa)
library(glmnet)
pred.df <- as.data.frame(pred)
nrow(data.num[,ynames])
nrow(pred.df)

xnames <- altruism.fields
cv.reg <- cv.glmnet(
  as.matrix(data.num[,xnames]),
  as.matrix(pred.df),
  family = "mgaussian",
  alpha = 0, # Ridge
  lambda = seq(from = 0, to = 4, by = .02)
)
regmod <- glmnet(
  as.matrix(data.num[,xnames]),
  as.matrix(pred.df),
  family = "mgaussian",
  alpha = 0,
  lambda = cv.reg$lambda.min
)
coef(regmod)
yhat <- predict(regmod,  as.matrix(data.num[,xnames]), s = "lambda.min")
rmsep <-  sqrt(sum((pred.df - yhat)^2)/(length(ynames) * nrow(pred)))
rmsep


reg.set <- data.num
# Modify the CLUST column
reg.set$CLUST <- ifelse(reg.set$CLUST == 4, 1, 0) # Be sure to check this!!! Set the cluster indicating HC Complete

table(reg.set$CLUST)

# Convert the CLUST column to a factor
reg.set$CLUST <- factor(reg.set$CLUST, levels = c(1, 0), labels = c("HCC", "Not HCC"))

table(reg.set$CLUST)

# Convert all columns to factors (or numeric)
# col_names <- names(subset(reg.set, select = -c(CLUST)))
# reg.set[,col_names] <- lapply(reg.set[,col_names] , factor) # Change to factor
# reg.set[,col_names] <- lapply(reg.set[,col_names] , as.numeric) # Change to numeric


library(glmnet)
cv.reg <- cv.glmnet(
  as.matrix(reg.set[,xnames]),
  as.matrix(reg.set$CLUST),
  family = "binomial",
  alpha = 1, # 0 = Ridge, 1 = Lasso
  # lambda = seq(from = 0, to = 2, by = .01)
)
plot(cv.reg)

regmod <- glmnet(
  as.matrix(reg.set[,xnames]),
  as.matrix(reg.set$CLUST),
  family = "binomial",
  alpha = 0,
  lambda = cv.reg$lambda.min
)
coef(regmod)
probabilities <- predict(regmod, as.matrix(reg.set[,xnames]), type = "response")
predicted.classes <- as.vector(ifelse(probabilities < 0.5, 1, 0))
observed.classes <- ifelse(reg.set$CLUST == "HCC", 1, 0)
round(mean(predicted.classes == observed.classes), 3)


library(glmnet)
library(caret)
logistic.kfold<- function(model, dats, n.folds, reps){
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
      
      cv.reg <- cv.glmnet(
        as.matrix(reg.set[-indis,xnames]),
        as.matrix(reg.set[-indis,"CLUST"]),
        family = "binomial",
        alpha = 1, # 0 = Ridge, 1 = Lasso
        # lambda = seq(from = 0, to = 2, by = .01)
      )
      
      regmod <- glmnet(
        as.matrix(reg.set[-indis,xnames]),
        as.matrix(reg.set[-indis,"CLUST"]),
        family = "binomial",
        alpha = 1,
        lambda = cv.reg$lambda.min
      )
      probabilities <- predict(regmod, as.matrix(reg.set[indis,xnames]), type = "response")
      predicted.classes <- as.vector(ifelse(probabilities < 0.5, 1, 0))
      observed.classes <- ifelse(reg.set[indis,"CLUST"] == "HCC", 1, 0)
      round(mean(predicted.classes == observed.classes), 3)
      
      # cm <- confusionMatrix(as.factor(predicted.classes, levels = c(1,0)), as.factor(observed.classes, levels = c(1,0)))
      
      fit.df <- data.frame(
        model = "Tree",
        # Accuracy = cm$overall['Accuracy'],
        Accuracy = mean(predicted.classes == observed.classes)
        # Kappa = cm$overall['Kappa']mean(predicted.classes == observed.classes)
      )
      
      results <- rbind(results, fit.df)
      rownames(results) = NULL
    }    
  }
  
  return(as.data.frame(results))
}

logistic.results <- logistic.kfold(NULL, reg.set, 10, 100)

mean(logistic.results$Kappa, na.rm = T)
mean(logistic.results$Accuracy, na.rm = T)
summary(logistic.results$Accuracy)

ggplot(logistic.results, aes(x=model, y=Accuracy, fill=factor(model))) +
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




# HC Complete Decision Tree ----

library(rpart)
library(rpart.plot) # See: http://www.milbo.org/rpart-plot/prp.pdf
library(plyr)

# Get predicted lavaan results
# results <- pred


tree.set <- data.num
table(tree.set$CLUST)

# Modify the CLUST column
tree.set$CLUST <- ifelse(tree.set$CLUST == 4, 1, 0) # Be sure to check this!!! Set the cluster indicating HC Complete

table(tree.set$CLUST)

# Convert the CLUST column to a factor
tree.set$CLUST <- factor(tree.set$CLUST, levels = c(1, 0), labels = c("HCC", "Not HCC"))

table(tree.set$CLUST)

# Convert all columns to factors (or numeric)
col_names <- names(subset(tree.set, select = -c(CLUST)))
# tree.set[,col_names] <- lapply(tree.set[,col_names] , factor) # Change to factor
tree.set[,col_names] <- lapply(tree.set[,col_names] , as.numeric) # Change to numeric


# CP parameter tuning ----

library(caret)

# Define the tuning grid
tuneGrid <- expand.grid(cp = seq(.000, .5, by = 0.005))

# Define the control parameters for cross-validation
ctrl <- trainControl(method = "repeatedcv",   # Cross-validation
                     number = 10,     # 10 folds
                     repeats = 50)   # 100 iterations

# Perform hyperparameter tuning using train function
# set.seed(1234567)
library(caret)
form <- as.formula(paste("CLUST ~", paste(altruism.fields, collapse = " + ")))
tree_model <- train( form, 
                     data = tree.set,
                     method = "rpart",
                     trControl = ctrl,
                     tuneGrid = tuneGrid,
                     # tuneLength = 1,
                     # model = tree,
                     # modelType = "rpart",
                     control = rpart.control(minsplit = 7, maxdepth = 6)
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
                         cp = 0.05,
                         minsplit = 7,
                         maxdepth = 6
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

mean(tree.kfold.results$Kappa, na.rm = T)
mean(tree.kfold.results$Accuracy, na.rm = T)
summary(tree.kfold.results$Accuracy)

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

percent <- 1 # Can be modified for cross-validation
test_idx <- sort(sample(x = nrow(tree.set), size = floor(percent*nrow(tree.set)), replace = F))

# Hyperparameter tuning - https://medium.com/data-and-beyond/hyperparameter-tuning-and-pruning-more-about-k-means-clustering-in-r-with-rpart-9a405685a09b
tree <- rpart(form, data = tree.set[test_idx,], method = "class", control =
                rpart.control(
                  cp = 0.05,
                  minsplit = 7,
                  maxdepth = 6
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


# Logistic regression


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

g ~ selfless
'
mod <- paste0(mod, "\n", hc.HO)

cfa <- cfa(mod, std.lv = T, data=data.num, ordered = F, estimator = "MLR")
cfa <- cfa(mod, data=data.num, ordered = T, estimator = "WLSMV", std.lv = T)

# Confidence intervals for regression
# Manually compute confidence intervals for standardized estimates
standardizedSolution(cfa, ci = TRUE, level = 0.95)

summary(cfa, fit.measures = TRUE, standardized = TRUE)
# modindices(cfa, sort = TRUE)
lavaanPlot(model = cfa, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stand = TRUE)
lavInspect(cfa, "cov.lv")
cov2cor(lavInspect(cfa, what = "est")$psi)

# Table stats
fitm <- fitMeasures(cfa, c("cfi",	"tli", "rmsea", "cfi.scaled",	"tli.scaled",	"rmsea.scaled", "cfi.robust",	"tli.robust",	"rmsea.robust", "srmr", "chisq.scaled", "df.scaled"))
fitm
clipr::write_clip(
  paste0(
    c(paste0(c(fitm[1], fitm[2], fitm[3]), collapse = "\t"),
      paste0(c(fitm[4], fitm[5], fitm[6]), collapse = "\t"),
      paste0(c(fitm[7], fitm[8], fitm[9]), collapse = "\t"),
      paste0(c(fitm[10], fitm[11], fitm[12]), collapse = "\t")
    ),
    collapse = "\t\t"
  )
)

# Inline stats
fitMeasures(cfa, c("cfi",	"tli", "rmsea", "cfi.scaled",	"tli.scaled",	"rmsea.scaled", "cfi.robust",	"tli.robust",	"rmsea.robust", "srmr", "chisq.scaled", "df.scaled"))
stats <- fitMeasures(cfa, c("cfi.robust",	"tli.robust",	"rmsea.robust", "srmr"))
clipr::write_clip(paste(c("CFI = ", gsub("^0", "", as.character(round(stats[1], 3))),
                          ", TLI = ", gsub("^0", "", as.character(round(stats[2], 3))),
                          ", RMSEA = ", gsub("^0", "", as.character(round(stats[3], 3))),
                          ", SRMR = ", gsub("^0", "", as.character(round(stats[4], 3)))
), collapse = ""))

EFAtools::SL(cfa, g_name = "selfless") # Must have higher order factor 'g'
EFAtools::OMEGA(cfa, g_name = "selfless") # Must have higher order factor 'g'

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
fitm <- fitMeasures(cfa, c("cfi",	"tli", "rmsea", "cfi.scaled",	"tli.scaled",	"rmsea.scaled", "cfi.robust",	"tli.robust",	"rmsea.robust", "srmr", "chisq.scaled", "df.scaled"))
fitm
clipr::write_clip(
  paste0(
    c(paste0(c(fitm[1], fitm[2], fitm[3]), collapse = "\t"),
      paste0(c(fitm[4], fitm[5], fitm[6]), collapse = "\t"),
      paste0(c(fitm[7], fitm[8], fitm[9]), collapse = "\t"),
      paste0(c(fitm[10], fitm[11], fitm[12]), collapse = "\t")
    ),
    collapse = "\t\t"
  )
)

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

# Output for table
fitm <- fitMeasures(cfa, c("cfi",	"tli", "rmsea", "cfi.scaled",	"tli.scaled",	"rmsea.scaled", "cfi.robust",	"tli.robust",	"rmsea.robust", "srmr", "chisq.scaled", "df.scaled"))
fitm
clipr::write_clip(
  paste0(
    c(paste0(c(fitm[1], fitm[2], fitm[3]), collapse = "\t"),
      paste0(c(fitm[4], fitm[5], fitm[6]), collapse = "\t"),
      paste0(c(fitm[7], fitm[8], fitm[9]), collapse = "\t"),
      paste0(c(fitm[10], fitm[11], fitm[12]), collapse = "\t")
    ),
    collapse = "\t\t"
  )
)


## Selflessness Bifactor ----

mod <- ' # Full bifactor
oneness =~ psybliss21 + psybliss18 + psybliss23
altruism =~ pg30 + pg34 + pg35
g =~ psybliss21 + psybliss18 + psybliss23 + pg30 + pg34 + pg35
'
cfa <- cfa(mod, std.lv = F, data=data.num, ordered = F, estimator = "MLR", orthogonal = T)

summary(cfa, fit.measures = TRUE, standardized = TRUE)

# Output for inline stats
stats <- fitMeasures(cfa, c("cfi.robust",	"tli.robust",	"rmsea.robust", "srmr"))
clipr::write_clip(paste(c("CFI = ", gsub("^0", "", as.character(round(stats[1], 3))),
                          ", TLI = ", gsub("^0", "", as.character(round(stats[2], 3))),
                          ", RMSEA = ", gsub("^0", "", as.character(round(stats[3], 3))),
                          ", SRMR = ", gsub("^0", "", as.character(round(stats[4], 3)))
), collapse = ""))


mod1 <- ' # S-1, Oneness = reference factor
# oneness =~ psybliss21 + psybliss18 + psybliss23
altruism =~ pg30 + pg34 + pg35
g =~ psybliss21 + psybliss18 + psybliss23 + pg30 + pg34 + pg35
g ~~ 0*altruism
'

cfa <- cfa(mod1, std.lv = T, data=data.num, ordered = F, estimator = "MLR", orthogonal = F)
# cfa1 <- cfa(mod1, std.lv = T, data=data.num, ordered = T, estimator = "WLSMV")
fitMeasures(cfa1, c("cfi.robust",	"tli.robust",	"rmsea.robust", "srmr", "chisq.scaled", "df.scaled"))
summary(cfa1, fit.measures = TRUE, standardized = TRUE)

mod2 <- ' # S-1, Altruism = reference factor
oneness =~ psybliss21 + psybliss18 + psybliss23
# altruism =~ pg30 + pg34 + pg35
g =~ psybliss21 + psybliss18 + psybliss23 + pg30 + pg34 + pg35
g ~~ 0*oneness
'

cfa <- cfa(mod2, std.lv = F, data=data.num, ordered = F, estimator = "MLR", orthogonal = F)
# cfa2 <- cfa(mod2, std.lv = T, data=data.num, ordered = T, estimator = "WLSMV")
fitMeasures(cfa2, c("cfi.robust",	"tli.robust",	"rmsea.robust", "srmr", "chisq.scaled", "df.scaled"))
summary(cfa2, fit.measures = TRUE, standardized = TRUE)

stats <- fitMeasures(cfa, c("cfi.robust",	"tli.robust",	"rmsea.robust", "srmr"))
clipr::write_clip(paste(c("CFI = ", gsub("^0", "", as.character(round(stats[1], 3))),
                          ", TLI = ", gsub("^0", "", as.character(round(stats[2], 3))),
                          ", RMSEA = ", gsub("^0", "", as.character(round(stats[3], 3))),
                          ", SRMR = ", gsub("^0", "", as.character(round(stats[4], 3)))
), collapse = ""))

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


## RO-SEM Tests on Selflessness ----

xnames <- c("psybliss21", "psybliss23", "psybliss18", "pg30", "pg34", "pg35")
ynames <- c("mystical6", "mystical22", "mystical25", "mystical15", "mystical8", "mystical13", "mystical10", "mystical5", "mystical7", "mystical4", "spiritual3", "spiritual2", "spiritual26", "psyphys5", "psyphys3", "psyphys9", "psyphys11", "psyphys1")

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
  # data.num,
  data.num[sample(nrow(data.num), 200),],
  mod,
  n.folds = 10,
  reps = 100,
  xnames,
  ynames,
  lambda.seq = seq(from = 0, to = 2, by = .05)
  # lambda.seq = 10^seq(-2.2, .1, length = 100) # glmnet style sequence of lambdas
)
# saveRDS(ypred_results, file = "outputs/temp.rds")
# saveRDS(ypred_results, file = "outputs/selfless-unityconsc-n200-fullhcmodel.rds")

# ** n = 200, Full HC/Selflessness model, predicting unityconsc only by seflessness
ypred_results <- readRDS(file = "outputs/selfless-unityconsc-n200-fullhcmodel.rds")


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

png(filename = "documents/figures/Fig9.png", width = 800, height = 640)
ggplot(ypred_results, aes(x = factor(model), y = rmsep, fill = factor(model))) +
  geom_boxplot(fill = "grey80", aes(group = factor(model))) +  # Matching the grey80 fill from the first plot
  geom_jitter(width = 0.05, height = 0, colour = rgb(0, 0, 0, 0.5), size = 4) +  # Similar jitter format
  scale_x_discrete(labels = c("OOS", "RO-SEM", "REG", "MLR")) + 
  xlab("Prediction Method") + 
  ylab("RMSEp") + 
  theme_minimal(base_size = 14) +  # Use the minimal theme and base font size
  theme(
    legend.position = "none", 
    axis.title.x = element_text(size = 25, margin = margin(t = 20)),  # Match x-axis title format
    axis.text.x = element_text(size = 25, margin = margin(r = 10)),  # Added visible x-axis labels and larger font size
    axis.ticks.x = element_blank(),  # Keep ticks removed from x-axis for clean appearance
    axis.title.y = element_text(size = 25),  # y-axis title formatted for consistency
    axis.text.y = element_text(size = 25),  # Larger font size for y-axis numeric labels
    panel.grid.major.x = element_blank(),  # Remove vertical grid lines
    panel.grid.major.y = element_line(size = 0.5, colour = "grey85"),  # Keep horizontal grid lines
    panel.grid.minor = element_blank()  # No minor grid lines
  ) +
  scale_fill_grey(start = 0.5, end = 0.8)  # Adjusted grey scale to match first plot

dev.off()


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
  mod,
  n.folds = 2,
  reps = 100,
  xnames,
  ynames,
  lambda.seq = seq(from = 0, to = 1, by = .05)
)

# saveRDS(ypred_item_results, file = "outputs/ITEM_selfless-unityconsc-fullhcmodel.rds")
ypred_item_results <- readRDS(file = "outputs/ITEM_selfless-unityconsc-fullhcmodel.rds")
nrow(ypred_item_results$results) # 18 y's * 100 reps * 489 subjects


### Data prep ----

# Add factor mapping to the data
factor_mapping <- data.frame(
  variable = c("mystical22", "mystical25", "mystical15", "mystical13", "mystical10", 
               "mystical6", "mystical8", "psyphys11", "psyphys1", "spiritual26", 
               "spiritual2", "spiritual3", "psyphys5", "psyphys3", "psyphys9", 
               "mystical7", "mystical4", "mystical5"),
  factor = c("unityconsc", "unityconsc", "unityconsc", "unityconsc", "unityconsc", 
             "unityconsc", "unityconsc", "light", "light", "insight", 
             "insight", "insight", "energy", "energy", "energy", 
             "bliss", "bliss", "bliss")
)

# Merge the factor mapping with the prediction results
ypred_item_results$results <- merge(ypred_item_results$results, factor_mapping, by = "variable")

# Calculate residuals
ypred_item_results$results$residuals <- ypred_item_results$results$actual_value - ypred_item_results$results$predicted_value
# Ensure residuals are numeric
ypred_item_results$results$residuals <- as.numeric(ypred_item_results$results$residuals)

### Global metrics ----

# Calculate RMSE per variable
library(dplyr)
rmse_results <- ypred_item_results$results %>%
  group_by(factor) %>%
  summarise(RMSE = sqrt(mean(residuals^2)))

# Calculate overall RMSE
overall_rmse <- sqrt(mean(ypred_item_results$results$residuals^2))
overall_rmse

# Calculate overall R²
mean_actual <- mean(ypred_item_results$results$actual_value)
ss_total <- sum((ypred_item_results$results$actual_value - mean_actual)^2)
ss_residual <- sum(ypred_item_results$results$residuals^2)
r_squared <- 1 - (ss_residual / ss_total)
r_squared

# Calculate overall MAE
mae <- mean(abs(ypred_item_results$results$residuals))
mae


head(ypred_item_results$rmsep)

library(dplyr)
library(ggplot2)

# Calculate RMSEp for each variable
rmsep_df <- ypred_item_results$results %>%
  group_by(variable) %>%
  summarise(rmsep = sqrt(mean((predicted_value - actual_value)^2)))

# Now use ggplot to create the chart
ggplot(rmsep_df, aes(x = factor(variable), y = rmsep, fill = factor(variable))) +
  geom_boxplot(fill = "grey", aes(group = factor(variable))) +
  geom_jitter(width = 0.05, height = 0, colour = rgb(0, 0, 0, 0.3)) +
  xlab("Indicator") +
  ylab("RMSEp") +
  theme(legend.position = "none") +
  scale_fill_grey(start = 0.3, end = 0.7)

# Calculate RMSEp for each factor
rmsep_by_factor <- grouped_results %>%
  group_by(factor) %>%
  summarise(rmsep = sqrt(mean((actual_value - predicted_value)^2)))

# Plot RMSEp by factor
# png(filename = "documents/figures/Fig11.png", width = 800, height = 800)
ggplot(rmsep_by_factor, aes(x = factor(factor), y = rmsep, fill = factor(factor))) +
  geom_boxplot(fill = "grey", aes(group = factor(factor))) +
  geom_jitter(width = .3, height = 0, colour = rgb(0, 0, 0, 0.3)) +  # Increased jitter width
  xlab("Factor") +
  ylab("RMSEp") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 25, margin = margin(t = 10)),
    axis.text.x = element_text(size = 25),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(size = 25),
    axis.text.y = element_text(size = 25),
    panel.grid.major = element_line(size = 0.5, colour = "grey85"),
    panel.grid.minor = element_blank()
  ) +
  scale_fill_grey(start = 0.3, end = 0.7)

# dev.off()



### Residuals vs Actuals plot ----

png(filename = "documents/figures/Fig10.png", width = 800, height = 640)
ggplot(ypred_item_results$results, aes(x = as.factor(actual_value), y = predicted_value)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey90", size = 1) +  # Ideal prediction line
  geom_violin(fill = "grey80", color = "black", alpha = 0.7) +  # Violin plot style in greyscale
  geom_boxplot(width = 0.1, fill = "white", color = "black", outlier.shape = NA) +  # Boxplot overlay in greyscale
  stat_boxplot(geom = "errorbar", width = 0.3, color = "black") +  # Add whiskers in greyscale
  labs(x = "Actual (Likert value)", y = "Predicted") +  # Remove the title
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6), minor_breaks = NULL) +  # Ensure y-axis labels are shown
  theme_minimal() +
  theme(
    text = element_text(size = 14),  # Enlarged font size
    axis.title.x = element_text(size = 25, margin = margin(t = 20)),  # Padding for x label
    axis.title.y = element_text(size = 25, margin = margin(r = 20)),   # Padding for y label
    axis.text.x = element_text(size = 20),  # Increase x-axis text size
    axis.text.y = element_text(size = 20),  # Increase y-axis text size    
    panel.grid.major.x = element_blank(),  # Remove vertical gridlines
    panel.grid.minor.x = element_blank()   # Remove vertical minor gridlines
  )
dev.off()





## Model K-fold Cross-Validation of Fit Measures ----
kfold <- function(dats, mod, n.folds, reps){
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

mod <- '
oneness =~ psybliss21 + psybliss18 + psybliss23
altruism =~ pg30 + pg34 + pg35
selfless =~ oneness + altruism
g ~ selfless
'
mod <- paste0(mod, "\n", hc.HO)

self_results <- kfold(data.num, mod = mod, n.folds = 2, reps = 100)
# write.csv(self_results, file="outputs/selfless-kfold.csv")
set.seed(NULL)

png(filename = "Fig7-1.png", width = 400, height = 400)
ggplot(self_results, aes(x=model, y=cfi, fill=factor(model)), environment = environment()) +
  geom_boxplot(fill = "grey80", aes(group = factor(model))) + 
  geom_jitter(width = 0.1, height = 0, colour = rgb(0, 0, 0, 0.5), size = 4) + 
  xlab("Robust CFI") + 
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none", 
    axis.title.x = element_text(size = 25, margin = margin(t = 10)),  # Label placed below the plot
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(),  
    axis.title.y = element_blank(),  # No label on the y-axis
    axis.text.y = element_text(size = 25),  # Increased font size for numeric labels on the left
    panel.grid.major = element_line(size = 0.5, colour = "grey85"),
    panel.grid.minor = element_blank()
  ) +
  scale_fill_grey(start = 0.5, end = 0.8)
dev.off()

png(filename = "Fig7-2.png", width = 400, height = 400)
ggplot(self_results, aes(x=model, y=tli, fill=factor(model)), environment = environment()) +
  geom_boxplot(fill = "grey80", aes(group = factor(model))) + 
  geom_jitter(width = 0.1, height = 0, colour = rgb(0, 0, 0, 0.5), size = 4) + 
  xlab("Robust TLI") + 
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none", 
    axis.title.x = element_text(size = 25, margin = margin(t = 10)),  # Label placed below the plot
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(),  
    axis.title.y = element_blank(),  # No label on the y-axis
    axis.text.y = element_text(size = 25),  # Increased font size for numeric labels on the left
    panel.grid.major = element_line(size = 0.5, colour = "grey85"),
    panel.grid.minor = element_blank()
  ) +
  scale_fill_grey(start = 0.5, end = 0.8)
dev.off()

png(filename = "Fig7-3.png", width = 400, height = 400)
ggplot(self_results, aes(x=model, y=rmsea, fill=factor(model)), environment = environment()) +
  geom_boxplot(fill = "grey80", aes(group = factor(model))) + 
  geom_jitter(width = 0.1, height = 0, colour = rgb(0, 0, 0, 0.5), size = 4) + 
  xlab("Robust RMSEA") + 
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none", 
    axis.title.x = element_text(size = 25, margin = margin(t = 10)),  # Label placed below the plot
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(),  
    axis.title.y = element_blank(),  # No label on the y-axis
    axis.text.y = element_text(size = 25),  # Increased font size for numeric labels on the left
    panel.grid.major = element_line(size = 0.5, colour = "grey85"),
    panel.grid.minor = element_blank()
  ) +
  scale_fill_grey(start = 0.5, end = 0.8)
dev.off()

png(filename = "Fig7-4.png", width = 400, height = 400)
ggplot(self_results, aes(x=model, y=srmr, fill=factor(model)), environment = environment()) +
  geom_boxplot(fill = "grey80", aes(group = factor(model))) + 
  geom_jitter(width = 0.1, height = 0, colour = rgb(0, 0, 0, 0.5), size = 4) + 
  xlab("SRMR") + 
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none", 
    axis.title.x = element_text(size = 25, margin = margin(t = 10)),  # Label placed below the plot
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(),  
    axis.title.y = element_blank(),  # No label on the y-axis
    axis.text.y = element_text(size = 25),  # Increased font size for numeric labels on the left
    panel.grid.major = element_line(size = 0.5, colour = "grey85"),
    panel.grid.minor = element_blank()
  ) +
  scale_fill_grey(start = 0.5, end = 0.8)
dev.off()


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
library(boot)
boot.self <- boot(data.num, getParamEstimates, R = casesNeeded, n = n, parallel = "multicore", ncpus = 4)

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
boot.self <- readRDS(file = "outputs/boot_selfless_params.rds")

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




# aohc-cluster.R

library(mclust)
library(lavaan)
library(reshape2)
library(ggplot2)

data.num <- extract.numeric.columns.by.regex(ses.data, paste0('mystical\\d+|spiritual\\d+|psyphys\\d+|psygrowth\\d+|psybliss\\d+|*.gate'))
gate.questions <- names(data.num[ , grepl("*.gate", names(data.num))]) # Confirm gate questions
nrow(data.num)
cfa <- cfa(hc.mod, data=data.num, ordered = T, estimator = "WLSMV")
pred <- lavPredict(cfa, method = "EBM", transform = TRUE)
# pred <- lavPredict(cfa)

# Max and min
rbind(round(apply(pred,2,min), 2), round(apply(pred,2,max), 2))

# Individual indicators
# grepmatch <- '\\bmystical6\\b|\\bmystical22\\b|\\bmystical25\\b|\\bmystical15\\b|\\bmystical8\\b|\\bmystical13\\b|\\bmystical10\\b|\\bmystical5\\b|\\bmystical7\\b|\\bmystical4\\b|\\bspiritual3\\b|\\bspiritual2\\b|\\bspiritual26\\b|\\bpsyphys5\\b|\\bpsyphys3\\b|\\bpsyphys9\\b|\\bpsyphys11\\b|\\bpsyphys1\\b'
# indvars <- extract.numeric.columns.by.regex(ses.data, grepmatch = grepmatch)
# clusterdata <- indvars

nrow(pred)
clusterdata <- as.data.frame(pred)


# hc1 <- hc(clusterdata, use = "SVD")
# bic <- mclustBIC(clusterdata,
#   # initialization = list(
#     # hcPairs = hc1
#   # )
# )
# summary(bic)
# plot(bic)


# BIC <- NULL
# for(j in 1:20) {
#   rBIC <- mclustBIC(clusterdata, verbose = FALSE,
#                     initialization = list(hcPairs = hcRandomPairs(clusterdata)))
#   BIC <- mclustBICupdate(BIC, rBIC)
# }


# ICL <- mclustICL(clusterdata)
# summary(ICL)
# plot(ICL)

# Bootstrap LRT to generate a p value
# lrt <- mclustBootstrapLRT(clusterdata, modelName = "VVV", maxG = 5)

clustmod <- Mclust(clusterdata, G = 4)
# clustmod <- Mclust(clusterdata, x = bic)

summary(clustmod, parameters = T)
plot(clustmod, what = "classification")

comb <- clustCombi(clustmod, clusterdata)
# comb <- clustCombi(data = clusterdata, modelName = "VVV", G = 1:9)
plot(comb)
# comb$classification[[5]]
# optim <- clustCombiOptim(comb, reg = 4) #
# optim$numClusters.combi
# optim$cluster.combi


# bootClust <- MclustBootstrap(clustmod, nboot = 999, type = "bs")
# plot(bootClust)
# summary(bootClust, what = "se")
# summary(bootClust, what = "ci")

plot(clustmod, what = "classification")
plot(clustmod, what = "uncertainty")
plot(clustmod, what = "density")
plot(clustmod, what = "density", type = "hdr", prob = c(0.5, .9))
plot(clustmod, what = "density", type = "persp")

# add a column in myData CLUST with the cluster
# results <- data.frame(cbind(pe.negphysicalgate=factor(tmp$pe.negphysical.gate), pred)) #, tmp$pe.negphysical.gate,
results <- data.frame(pred, data.num[ , grepl("*.gate", names(data.num))], data.num) #, tmp$pe.negphysical.gate,
# results <- pred
results$CLUST <- clustmod$classification
# results$CLUST <- comb$classification[[4]]
nrow(results)
# now to write it out:
write.csv(results, # reorder columns to put CLUST first
          file="outputs/4-clusters.csv",                  # output filename
          row.names=FALSE,                 # don't save the row numbers
          quote=FALSE)                     # don't surround column names in ""


table(results$CLUST)

boxplot(CLUST ~ pe.gate, data = results)

boxplot(unityconsc ~ CLUST, data = results)
boxplot(bliss ~ CLUST, data = results)
boxplot(insight ~ CLUST, data = results)
boxplot(energy ~ CLUST, data = results)
boxplot(light ~ CLUST, data = results)


# Re-organize
results[results$CLUST == 4, "CLUST"] <- 5
results[results$CLUST == 3, "CLUST"] <- 4
results[results$CLUST == 2, "CLUST"] <- 3
results[results$CLUST == 1, "CLUST"] <- 2
results[results$CLUST == 5, "CLUST"] <- 1


library(dplyr)
mean_factor_scores <- results %>%
  group_by(CLUST) %>%
  summarise(
    Mean_Factor1 = mean(unityconsc),
    Mean_Factor2 = mean(bliss),
    Mean_Factor3 = mean(insight),
    Mean_Factor4 = mean(energy),
    Mean_Factor5 = mean(light),
    Total_Count = n()    
  )
clipr::write_clip(mean_factor_scores)

min_max_factor_scores <- results %>%
  summarise(
    Min_Factor1 = min(unityconsc),
    Min_Factor2 = min(bliss),
    Min_Factor3 = min(insight),
    Min_Factor4 = min(energy),
    Min_Factor5 = min(light),
    Max_Factor1 = max(unityconsc),
    Max_Factor2 = max(bliss),
    Max_Factor3 = max(insight),
    Max_Factor4 = max(energy),
    Max_Factor5 = max(light),
  )
clipr::write_clip(min_max_factor_scores)

#
# Primary cluster visualization
#
melted <- melt(results[ , c("CLUST", "unityconsc", "bliss", "insight", "energy", "light")], id="CLUST")
levels(melted[, "variable"]) <- c("Unity-Consciousness", "Bliss", "Insight", "Somatic Energy Sens.", "Luminosity")

# png(file="scatter.png", width = 800, height = 600)
tiff(file="Fig5.tiff", width = 800, height = 600)
ggplot(melted, aes(x = factor(CLUST), y = value, fill = factor(variable))) +
  geom_boxplot(width = .8, outlier.size = .8) +
  labs(x = "Cluster", y = "Factor Score") +
  scale_x_discrete(labels = c("Low Exp", "SES Exp", "Light Exp", "HC Complete")) +
  theme_bw() +
  scale_fill_grey(name = "Factor", start = 1, end = .5) +
  theme(
    axis.title.x = element_text(size = 30, margin = margin(t = 10)),  # Add padding and enlarge x-axis label
    axis.title.y = element_text(size = 30, margin = margin(r = 10)),  # Add padding and enlarge y-axis label
    axis.text.x = element_text(size = 18),   # Enlarge x-axis text
    axis.text.y = element_text(size = 23),   # Enlarge y-axis text
    legend.title = element_text(size = 30),  # Enlarge legend title
    legend.text = element_text(size = 23)    # Enlarge legend text
  )
dev.off()




# Interactive 3D scatterplot
library("car")
library("rgl")
scatter3d(
  clusterdata$bliss,
  clusterdata$unityconsc,
  clusterdata$insight,
  groups = as.factor(ifelse(results$CLUST == 2 | results$CLUST == 3, 1, results$CLUST)),
  surface = F,
  point.col = "blue",
  cex.lab=par("cex.lab")
)

library(scatterplot3d)
scatterplot3d(clusterdata[,c("bliss", "insight", "unityconsc")], angle = 60, pch = ifelse(results$CLUST == 2 | results$CLUST == 3, 1, results$CLUST))

library(car)
scatterplot(clusterdata[,c("unityconsc")], clusterdata[,c("bliss")], groups = ifelse(results$CLUST == 2 | results$CLUST == 3, 1, results$CLUST))


library(ggplot2)

# Assuming the data is in the 'results' dataframe
# Scatterplot for unitconsc (x-axis) and bliss (y-axis), colored by CLUST
tiff(file="Fig4.tiff", width = 800, height = 600)
ggplot(results, aes(x = unityconsc, y = bliss)) +
  geom_point(aes(shape = factor(predictions), fill = factor(predictions))
  # geom_point(aes(shape = factor(ifelse(CLUST == 2 | CLUST == 3, 1, CLUST)), fill = factor(ifelse(CLUST == 2 | CLUST == 3, 1, CLUST)))
    , color = "black", size = 7, alpha = 0.7) +
  scale_shape_manual(values = c(21, 25)) +  # Different point shapes for clusters
  scale_fill_manual(values = c("gray20", "gray100")) +  # Grayscale colors for fill
  labs(x = "Unity Consciousness", y = "Bliss") +
  theme_bw() +
  theme(
    text = element_text(size = 30),
    axis.title.x = element_text(size = 30, margin = margin(t = 15)),  # Add padding between x-axis label and axis
    axis.title.y = element_text(size = 30, margin = margin(r = 15)),  # Add padding between y-axis label and axis
    axis.text.x = element_text(size = 30),  # Increase x-axis text size
    axis.text.y = element_text(size = 30),  # Increase y-axis text size
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black"),
    axis.line = element_line(color = "black"),
    legend.position = "none"  # Optional: Hide legend if not needed
  )
dev.off()



#
# Gate question ----
#
gatequestions <- names(ses.data[ , grepl("*.gate", names(ses.data))])

for(gateq in gatequestions) {
  cat(sum(data.num[results$CLUST == 4, gateq] == 1, na.rm = TRUE), '\n')
}

round(
sum(data.num[results$CLUST == 4, gateq], na.rm = TRUE) / sum(data.num[, gateq], na.rm = TRUE)
,2)



# Step 1: Filter results for CLUST == 4
clust4_data <- ses.data[results$CLUST == 4, ]

# Step 2: Get the names of the gate questions
gatequestions <- names(ses.data[, grepl("*.gate", names(ses.data))])

# Step 3: Calculate the percentage of 'Yes' (1) answers for each gate question
percentage_yes <- sapply(gatequestions, function(question) {
  # Get the column corresponding to the current gate question
  gate_responses <- clust4_data[[question]]
  
  # Calculate the percentage of responses that are '1'
  mean(gate_responses == 1, na.rm = TRUE)
})

# Display the results
clipr::write_clip(percentage_yes)


#
# Ad Hoc Tests ----
#

oneway.test(unityconsc ~ CLUST,
  data = results[results$CLUST == 3 | results$CLUST == 4,], # 
  var.equal = F # assuming equal variances
)
test <- aov(unityconsc ~ CLUST, data = results[results$CLUST == 2 | results$CLUST == 4,])
summary(test)

ggplot(results, aes(x = pe.invmov.gate, y = unityconsc, fill = factor(pe.invmov.gate))) +
  geom_boxplot() +
  labs(title = "Gate Question Comparison",
       x = "Gate Question",
       y = "Unityconsc") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"))  # Specify colors for the two levels

table(results$CLUST, results$pe.gate)

table(results$CLUST, results$pe.gate)

chisq.test(
  table(results[results$CLUST == 2 | results$CLUST == 3 | results$CLUST == 4 , ]$CLUST,
        results[results$CLUST == 2 | results$CLUST == 3 | results$CLUST == 4 , ]$pe.invmov.gate)
)

round(table(results[ , ]$CLUST, results[ , ]$pe.gate)
      / rowSums(table(results[ , ]$CLUST, results[ , ]$pe.gate))
      , 2)


# M8 Analysis ----

library(car)

# Modify the CLUST column
results$cat4 <- as.factor(ifelse(results$CLUST == 4, 1, 0)) # Be sure to check this!!! Set the cluster indicating HC Complete
# Convert the CLUST column to a factor
results$cat4 <- factor(results$cat4, labels = c("Not HCC", "HCC"))

table(results$cat4)

# Specify the columns you want to scale
columns_to_scale <- names(results[,grepl("mystical\\d+|spiritual\\d+|psyphys\\d+", names(results))])

# Scale the specified columns
library(dplyr)
df_scaled <- results %>%
  mutate(across(all_of(columns_to_scale), scale))

model <- glm(cat4 ~ mystical6 + mystical22 + mystical25 + mystical15 + mystical8 + mystical13 + mystical10, data = df_scaled, family="binomial")
summary(model)
vif(model)

# Step 1: Logistic Regression Model
model <- glm(cat4 ~ mystical6 + mystical22 + mystical25 + mystical15 + mystical8 + mystical13 + mystical10, 
             data = df_scaled, 
             family = "binomial")

# Step 2: Summary of the Model
summary(model)

# Step 3: Individual ROC Curves for Each Predictor
library(pROC)

# List of predictors
predictors <- c("mystical6", "mystical22", "mystical25", "mystical15", "mystical8", "mystical13", "mystical10")

# Loop through predictors to generate ROC curves and AUC values
for (predictor in predictors) {
  # Generate ROC curve
  roc_curve <- roc(df_scaled$cat4, df_scaled[[predictor]])
  
  # Plot ROC curve
  plot(roc_curve, main = paste("ROC Curve for", predictor))
  
  # Calculate and print AUC
  auc_value <- auc(roc_curve)
  print(paste("AUC for", predictor, ":", auc_value))
}

# Step 4: Standardize Predictors and Fit the Logistic Regression Model
df_scaled_std <- df_scaled
df_scaled_std[predictors] <- scale(df_scaled[predictors])

model_std <- glm(cat4 ~ mystical6 + mystical2 + mystical25 + spiritual26 + mystical15 + mystical8 + mystical13 + mystical10 + mystical5 + mystical7 + mystical4 + spiritual3 + spiritual2 + psyphys5 + psyphys3 + psyphys9 + psyphys11 + psyphys1, 
                 data = df_scaled_std, 
                 family = "binomial")

# Summary of the Standardized Model
summary(model_std)



library(glmnet)
x <- model.matrix(model_std)[,-1]
y <- results$cat4
cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial")
cv.lasso$lambda.min
cv.lasso$lambda.1se
model <- glmnet(x, y, alpha = 1, family = "binomial",
                lambda = cv.lasso$lambda.min)
summary(model)
coef(model)



# install.packages("randomForest")
library(randomForest)

# Assuming df_scaled is your dataframe and HC_complete is your binary outcome variable
# Convert HC_complete to a factor if it isn't already
df_scaled$cat4 <- as.factor(df_scaled$cat4)

# Fit the random forest model
set.seed(123457)  # For reproducibility
# set.seed(NULL)
rf_data <- results
rf_data$cat4 <- as.factor(results$cat4)
rf_model <- randomForest(cat4 ~ mystical6 + mystical2 + mystical25 + spiritual26 + mystical15 + mystical8 + mystical13 + mystical10 + mystical5 + mystical7 + mystical4 + spiritual3 + spiritual2 + psyphys5 + psyphys3 + psyphys9 + psyphys11 + psyphys1, 
                         data = rf_data, 
                         importance = TRUE, 
                         ntree = 1000)  # You can adjust the number of trees

# Print the model summary
print(rf_model)

# Variable importance
importance(rf_model)
varImpPlot(rf_model)


# LPA Analysis ----

library(tidyLPA)
library(dplyr)

# clusterdata %>%
results %>%
  dplyr::select(unityconsc, bliss, insight, energy, light) %>%
  estimate_profiles(1:5,
                    variances = c("varying"),
                    covariances = c("varying")
                    ) # %>%
  # compare_solutions(statistics = c("AIC", "BIC"))
  # plot_profiles()

# library(poLCA)

hist(results$unityconsc, 30, xlim=c(-2, 2))
hist(results$bliss, 30, xlim=c(-2, 2))
hist(results$insight, 30, xlim=c(-2, 2))
hist(results$energy, 30, xlim=c(-2, 2))
hist(results$light, 30, xlim=c(-2, 2))


# Generalized Additive Graph of Light and Energy ----
require(mgcv)
library(ggplot2)

# Create the ggplot object
p <- ggplot(results, aes(y = unityconsc)) +
  # Points and GAM curve for energy (red)
  geom_point(aes(x = energy, color = "Energy"), alpha = 0.2, position = position_jitter(width = 0.3, height = .3)) +
  stat_smooth(aes(x = energy, color = "Energy"), method = "gam", formula = y ~ s(x)) +
  
  # Points and GAM curve for light (blue)
  geom_point(aes(x = light, color = "Light"), alpha = 0.2, position = position_jitter(width = 0.3, height = .3)) +
  stat_smooth(aes(x = light, color = "Light"), method = "gam", formula = y ~ s(x)) +
  
  # Labels and theme
  labs(x = "Energy / Light", y = "Unity Consciousness", 
       title = "GAM Curves for Unity Consciousness vs Energy and Light") +
  scale_color_manual(name = "Type", values = c("Energy" = "red", "Light" = "blue")) +
  theme_minimal()

print(p)



p <- ggplot(results, aes(y = unityconsc)) +
  # Points and GAM curve for energy (red)
  geom_point(aes(x = insight, color = "Unity"), alpha = 0.2, position = position_jitter(width = 0.3, height = .3)) +
  stat_smooth(aes(x = insight, color = "Unity"), method = "gam", formula = y ~ s(x)) +
  
  # Points and GAM curve for light (blue)
  geom_point(aes(x = energy, color = "Energy"), alpha = 0.0, position = position_jitter(width = 0.3, height = .3)) +
  stat_smooth(aes(x = energy, color = "Energy"), method = "gam", formula = y ~ s(x)) +
  
  geom_point(aes(x = light, color = "Light"), alpha = 0.0, position = position_jitter(width = 0.3, height = .3)) +
  stat_smooth(aes(x = light, color = "Light"), method = "gam", formula = y ~ s(x)) +  
  
  # Labels and theme
  labs(x = "Energy / Light", y = "Unity Consciousness", 
       title = "GAM Curves for Unity Consciousness vs Energy and Light") +
  scale_color_manual(name = "Type", values = c("Unity" = "orange", "Energy" = "red", "Light" = "blue")) +
  theme_minimal()

print(p)

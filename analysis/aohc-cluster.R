# aohc-cluster.R

library(mclust)
# source("code/ses-combiClust.R")
# pred <- lavPredict(cfa, append.data = TRUE)

nrow(data.num)
cfa <- cfa(hc.mod, data=data.num, ordered = T, estimator = "WLSMV")
pred <- lavPredict(cfa)

# Max and min
rbind(round(apply(pred,2,min), 2), round(apply(pred,2,max), 2))

# Individual indicators
# grepmatch <- '\\bmystical6\\b|\\bmystical22\\b|\\bmystical25\\b|\\bmystical15\\b|\\bmystical8\\b|\\bmystical13\\b|\\bmystical10\\bmystical5\\b|\\bmystical7\\b|\\bmystical4\\bspiritual3\\b|\\bspiritual2\\b|\\bspiritual26\\bpsyphys5\\b|\\bpsyphys3\\b|\\bpsyphys9\\bpsyphys11\\b|\\bpsyphys1\\b'
# indvars <- extract.numeric.columns.by.regex(data.num, grepmatch = grepmatch)
# clusterdata <- indvars



nrow(pred)
clusterdata <- as.data.frame(pred)
# clusterdata <- clusterdata[, c("unityconsc", "energy", "light")]

# Interactive 3D scatterplot
# library("car")
# library("rgl")
# scatter3d(pred$light, pred$unityconsc, pred$energy, surface = F, point.col = "blue")

bic <- mclustBIC(clusterdata)
summary(bic)
plot(bic)

ICL <- mclustICL(clusterdata)
summary(ICL)
plot(ICL)

# Bootstrap LRT to generate a p value
lrt <- mclustBootstrapLRT(clusterdata, modelName = "VVV", maxG = 5)

clustmod <- Mclust(clusterdata, x = bic)
summary(clustmod, parameters = T)

# comb <- clustCombi(clustmod, clusterdata)
# comb <- clustCombi(data = clusterdata, modelName = "VVV", G = 1:9)
# plot(comb)
# comb$classification[[5]]
# optim <- clustCombiOptim(comb, reg = 4) # 
# optim$numClusters.combi
# optim$cluster.combi


bootClust <- MclustBootstrap(clustmod, nboot = 999, type = "bs")
plot(bootClust)
summary(bootClust, what = "se")
summary(bootClust, what = "ci")

plot(clustmod, what = "classification")
plot(clustmod, what = "uncertainty")
plot(clustmod, what = "density")
plot(clustmod, what = "density", type = "hdr", prob = c(0.5, .9))
plot(clustmod, what = "density", type = "persp")

# add a column in myData CLUST with the cluster
# results <- data.frame(cbind(pe.negphysicalgate=factor(tmp$pe.negphysical.gate), pred)) #, tmp$pe.negphysical.gate,
results <- data.frame(pred, data.num[ , grepl("*.gate", names(data.num))]) #, tmp$pe.negphysical.gate,
# results <- pred
results$CLUST <- clustmod$classification
# results$CLUST <- optim$cluster.combi
# results$CLUST <- comb$classification[[7]]
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



oneway.test(unityconsc ~ CLUST,
  data = results[results$CLUST == 2 | results$CLUST == 4,], # 
  var.equal = F # assuming equal variances
)
test <- aov(unityconsc ~ CLUST, data = results[results$CLUST == 2 | results$CLUST == 4,])
summary(test)

ggplot(results, aes(x = pe.gate, y = unityconsc, fill = factor(pe.gate))) +
  geom_boxplot() +
  labs(title = "Gate Question Comparison",
       x = "Gate Question",
       y = "Unityconsc") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"))  # Specify colors for the two levels

table(results$CLUST, results$pe.gate)

table(results$CLUST, results$pe.gate)

chisq.test(
  table(results[results$CLUST == 2 | results$CLUST == 4 , ]$CLUST,
        results[results$CLUST == 2 | results$CLUST == 4 , ]$pe.negphysical.gate)
)

round(table(results[ , ]$CLUST, results[ , ]$pe.gate)
      / rowSums(table(results[ , ]$CLUST, results[ , ]$pe.gate))
      , 2)

library(reshape2)
melted <- melt(results[ , c("CLUST", "unityconsc", "bliss", "insight", "energy", "light")], id="CLUST")
levels(melted[, "variable"]) <- c("Unity-Consciousness", "Bliss", "Insight", "Somatic Energy Sens.", "Luminosity")

# Move HCC to cluster #4
melted[melted$CLUST == 4, "CLUST"] <- 5
melted[melted$CLUST == 3, "CLUST"] <- 4
melted[melted$CLUST == 5, "CLUST"] <- 3

ggplot(melted, aes(x = factor(CLUST), y = value, fill = factor(variable))) +
  geom_boxplot(width = .8, outlier.size = .8) +
  labs(x = "Cluster",
       y = "Factor Score") +
  scale_x_discrete(labels= c("Low Exp", "SES Exp", "Light Exp", "HC Complete")) +
  theme_bw() + scale_fill_grey(name = "Factor", start = 1, end = .5)

# aohc-cluster.R

library(mclust)
# source("code/ses-combiClust.R")
# pred <- lavPredict(cfa, append.data = TRUE)

cfa <- cfa(hc.mod, data=data.num, ordered = T, estimator = "WLSMV")
pred <- lavPredict(cfa)
nrow(pred)
pred <- as.data.frame(pred)
clusterdata <- pred

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
# lrt <- mclustBootstrapLRT(clusterdata, modelName = "VVV", maxG = 5)

mod <- Mclust(clusterdata, x = bic)
summary(mod, parameters = T)

# comb <- clustCombi(mod, clusterdata)
# comb <- clustCombi(data = clusterdata, modelName = "VVV", G = 1:5)
# plot(comb)
# comb$classification[[5]]
# optim <- clustCombiOptim(comb, reg = 2) # 
# optim$numClusters.combi
# optim$cluster.combi


bootClust <- MclustBootstrap(mod, nboot = 999, type = "bs")
plot(bootClust)
summary(bootClust, what = "se")
summary(bootClust, what = "ci")

plot(mod, what = "classification")
plot(mod, what = "uncertainty")
plot(mod, what = "density")
plot(mod, what = "density", type = "hdr", prob = c(0.5, .9))
plot(mod, what = "density", type = "persp")

# add a column in myData CLUST with the cluster
# results <- data.frame(cbind(pe.negphysicalgate=factor(tmp$pe.negphysical.gate), pred)) #, tmp$pe.negphysical.gate,
results <- data.frame(pred) #, tmp$pe.negphysical.gate,
# results <- pred
results$CLUST <- mod$classification
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
boxplot(bliss ~ CLUST, data = results, col = c("red", "blue"))
boxplot(insight ~ CLUST, data = results, col = c("red", "blue"))
boxplot(energy ~ CLUST, data = results, col = c("red", "blue"))
boxplot(light ~ CLUST, data = results, col = c("red", "blue"))

oneway.test(insight ~ CLUST,
  data = results[results$CLUST == 2 | results$CLUST == 4,], # 
  var.equal = F # assuming equal variances
)
test <- aov(CLUST ~ pe.negphysicalgate, data = results)
summary(test)



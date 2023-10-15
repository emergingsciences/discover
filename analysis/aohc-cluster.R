# aohc-cluster.R

library(mclust)
source("code/ses-combiClust.R")
# pred <- lavPredict(cfa, append.data = TRUE)

mod <- hc.mod

# tmp <- na.omit(data.num) # TODO: Account for missing records
tmp <- data.num
nrow(tmp)
pred <- lavPredict(cfa, transform = T, newdata = tmp)
nrow(pred)
pred <- as.data.frame(pred)
clusterdata <- pred
# clusterdata <- pred[,grepl( "hc|energy|light" , names(pred) )]

bic <- mclustBIC(clusterdata)
summary(bic)
plot(bic)

ICL <- mclustICL(clusterdata)
summary(ICL)
plot(ICL)

# Bootstrap LRTto generate a p value
# LRT <- mclustBootstrapLRT(clusterdata, modelName = "VVV", maxG = 6)

mod <- Mclust(clusterdata, x = bic)
mod <- Mclust(clusterdata, G = 2, modelName = "VVV") # Set number of clusters
summary(mod, parameters = T)

comb <- clustCombi(mod, clusterdata)
# comb <- clustCombi(data = clusterdata, modelName = "VVV", G = 1:5)
plot(comb)
# comb$classification[[5]]
optim <- clustCombiOptim(comb, reg = 2) # 
optim$numClusters.combi
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
results <- data.frame(cbind(tmp$id, pred)) #tmp$pe.gate, tmp$pe.negphysical.gate,
results$CLUST <- mod$classification
# results$CLUST <- optim$cluster.combi
# results$CLUST <- comb$classification[[5]]
nrow(results)
# now to write it out:
write.csv(results, # reorder columns to put CLUST first
          file="outputs/5-clusters.csv",                  # output filename
          row.names=FALSE,                 # don't save the row numbers
          quote=FALSE)                     # don't surround column names in ""

table(results$CLUST)

boxplot(unityconsc ~ CLUST, data = results, col = c("red", "blue"))
boxplot(bliss ~ CLUST, data = results, col = c("red", "blue"))
boxplot(insight ~ CLUST, data = results, col = c("red", "blue"))
boxplot(energy ~ CLUST, data = results, col = c("red", "blue"))
boxplot(light ~ CLUST, data = results, col = c("red", "blue"))

oneway.test(energy ~ CLUST,
  data = results[results$CLUST == 3 | results$CLUST == 4,], # 
  var.equal = FALSE # assuming equal variances
)
aov(consc ~ tmp.pe.gate, data = results)
summary(test)



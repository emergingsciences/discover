# aohc-cluster.R

library(mclust)
# source("code/ses-combiClust.R")
# pred <- lavPredict(cfa, append.data = TRUE)

mod <- hc.mod

# tmp <- na.omit(data.num) # TODO: Account for missing records
tmp <- data.num
nrow(tmp)
pred <- lavPredict(cfa, newdata = tmp)
nrow(pred)
pred <- as.data.frame(pred)
clusterdata <- pred
# clusterdata <- pred[,grepl( "unityconsc|bliss|insight|light" , names(pred) )]

# Interactive 3D scatterplot
library("car")
library("rgl")
scatter3d(pred$light, pred$unityconsc, pred$energy, surface = F, point.col = "blue")


hc1 <- hc(clusterdata, modelName = "VVV", use = "SVD")
bic1 <- mclustBIC(clusterdata, initialization = list(hcPairs = hc1))
summary(bic1)
plot(bic1)

hc2 <- hc(clusterdata, modelName = "VVV", use = "VARS")
bic2 <- mclustBIC(clusterdata, initialization = list(hcPairs = hc2))
summary(bic2)
plot(bic2)

bic <- mclustBIC(clusterdata)
summary(bic)
plot(bic)

bic <- mclustBICupdate(bic, bic1, bic2)
summary(bic)

ICL <- mclustICL(clusterdata)
summary(ICL)
plot(ICL)

# Bootstrap LRT to generate a p value
lrt <- mclustBootstrapLRT(clusterdata, modelName = "VVV", maxG = 5)

mod <- Mclust(clusterdata, x = bic, hc = hc2)
# mod <- Mclust(clusterdata, G = 4, modelName = "VVV") # Set number of clusters
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
results <- data.frame(cbind(pe.negphysicalgate=factor(tmp$pe.negphysical.gate), pred)) #, tmp$pe.negphysical.gate,
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

oneway.test(unityconsc ~ CLUST,
  data = results[results$CLUST == 2 | results$CLUST == 3,], # 
  var.equal = F # assuming equal variances
)
test <- aov(CLUST ~ pe.negphysicalgate, data = results)
summary(test)



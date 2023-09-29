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
# clusterdata <- pred
clusterdata <- pred[,grepl( "consc|unity|bliss|insight" , names(pred) )]

bic <- mclustBIC(clusterdata)
summary(bic)
plot(bic)

ICL <- mclustICL(clusterdata)
summary(ICL)
plot(ICL)

# Bootstrap LRTto generate a p value
# LRT <- mclustBootstrapLRT(clusterdata, modelName = "VVV", maxG = 6)

mod <- Mclust(clusterdata, x = bic)
# mod <- Mclust(clusterdata, G = 5, modelName = "VVV") # Set number of clusters
summary(mod, parameters = T)

comb <- clustCombi(mod, clusterdata)
# comb <- clustCombi(data = clusterdata, modelName = "VVV", G = 1:5)
plot(comb)
optim <- clustCombiOptim(comb, reg = 3) # 
optim$numClusters.combi

combiClusters <- reassignClusters(mod, comb$combiM, 4)

bootClust <- MclustBootstrap(mod, nboot = 999, type = "bs")
plot(bootClust)
summary(bootClust, what = "se")
summary(bootClust, what = "ci")

plot(mod, what = "classification")
plot(mod, what = "uncertainty")
plot(mod, what = "density")
plot(mod, what = "density", type = "hdr", prob = c(0.5, .9))

# add a column in myData CLUST with the cluster
results <- data.frame(tmp$pe.gate, pred)
results$CLUST <- mod$classification
# results$CLUST <- combiClusters
nrow(results)
# now to write it out:
write.csv(results, # reorder columns to put CLUST first
          file="outputs/5-clusters.csv",                  # output filename
          row.names=FALSE,                 # don't save the row numbers
          quote=FALSE)                     # don't surround column names in ""

boxplot(consc ~ CLUST, data = results, col = c("red", "blue"))
boxplot(unity ~ CLUST, data = results, col = c("red", "blue"))
boxplot(bliss ~ CLUST, data = results, col = c("red", "blue"))
boxplot(insight ~ CLUST, data = results, col = c("red", "blue"))
boxplot(energy ~ CLUST, data = results, col = c("red", "blue"))
boxplot(light ~ CLUST, data = results, col = c("red", "blue"))

oneway.test(insight ~ CLUST,
  data = results[results$CLUST == 2 | results$CLUST == 3,], # 
  var.equal = FALSE # assuming equal variances
)
aov(consc ~ tmp.pe.gate, data = results)
summary(test)



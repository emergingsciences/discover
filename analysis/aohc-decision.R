# aohc-decision.R
#
# Decision tree analysis


library(rpart)
library(rpart.plot) # See: http://www.milbo.org/rpart-plot/prp.pdf
library(plyr)

pred <- lavPredict(cfa, data.num)

exclude_col <- c("unityconsc", "bliss", "insight", "energy", "light")
tmp <- data.frame(pred, data.num)
res <- tmp
res <- tmp[!(names(tmp) %in% exclude_col)]

tree <- rpart(hc~., data = res)
rpart.plot(tree, tweak = 1.1)

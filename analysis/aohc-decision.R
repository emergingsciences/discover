# aohc-decision.R
#
# Decision tree analysis


library(rpart)
library(rpart.plot) # See: http://www.milbo.org/rpart-plot/prp.pdf
library(plyr)

pred <- lavPredict(cfa, data.num)

exclude_col <- c("consc", "unity", "bliss", "energy", "light", "insight", "f2_psysom")
tmp <- as.data.frame(pred)
res <- tmp[!(names(tmp) %in% exclude_col)]

tree <- rpart(
  hc ~ .
  , data = res
)
rpart.plot(tree, tweak = 1.1)
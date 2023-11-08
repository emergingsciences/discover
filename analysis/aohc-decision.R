# aohc-decision.R
#
# Decision tree analysis




grepmatch = "psychic\\d+|talents\\d+|invmov\\d+|sensation\\d+|negphysical\\d+|otherphysical\\d+|negpsych\\d+|psybliss\\d+|psygrowth\\d+"
data.num <- extract.numeric.columns.by.regex(ses.data, grepmatch)

library(rpart)
library(rpart.plot) # See: http://www.milbo.org/rpart-plot/prp.pdf
library(plyr)

pred <- lavPredict(cfa)

exclude_col <- c("unityconsc", "bliss", "insight", "energy", "light")
tmp <- data.frame(pred, data.num)
res <- tmp
res <- tmp[!(names(tmp) %in% exclude_col)]

tree <- rpart(CLUST~., data = res)
rpart.plot(tree, tweak = 2.9)

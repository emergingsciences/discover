# From https://stackoverflow.com/questions/13486000/pairwise-correlation-table
pairwiseCor <- function(data.num){
  pairs <- combn(names(data.num), 2, simplify=FALSE)
  
  df <- data.frame(Vairable1=rep(0,length(pairs)), Variable2=rep(0,length(pairs)), 
                   AbsCor=rep(0,length(pairs)), Cor=rep(0,length(pairs)))
  for(i in 1:length(pairs)){
    df[i,1] <- pairs[[i]][1]
    df[i,2] <- pairs[[i]][2]
    df[i,3] <- round(abs(cor(data.num[,pairs[[i]][1]], data.num[,pairs[[i]][2]])),4)
    df[i,4] <- round(cor(data.num[,pairs[[i]][1]], data.num[,pairs[[i]][2]]),4)
  }
  pairwiseCorDF <- df
  pairwiseCorDF <- pairwiseCorDF[order(pairwiseCorDF$AbsCor, decreasing=TRUE),]
  row.names(pairwiseCorDF) <- 1:length(pairs)
  pairwiseCorDF <<- pairwiseCorDF
  pairwiseCorDF
}

df <- pairwiseCor(data.num[1:5])

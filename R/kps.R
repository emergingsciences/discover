# Load Kundalini Profile Survey data

library("ggplot2")

load.kps <- function(file = "data/kps1-results.txt") {
  return(dget(file))
}

example.kps.plot <- function(kpsdata = load_KPS()) {
  ggplot(data=kpsdata, aes(kpsdata$sex)) + geom_bar() + labs(x="Sex of Participant", y="Count")
}
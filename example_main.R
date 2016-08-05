# Example script which loads Kundalini Profile Survey data and creates a simple plot

source("R/kps.R")
kps.data <- load.kps()
example.kps.plot(kps.data)
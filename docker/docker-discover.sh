#!/bin/sh

docker run \
-p 8787:8787 \
-e PASSWORD=password \
-v /home/mike/Desktop/esf/R/discover:/home/rstudio \
discover

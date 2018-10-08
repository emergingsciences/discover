##
##
## SES LCA and Factor Analysis Script
## Challenge: We need a way to group the population into clusters on a per-factor basis.
##            For example, a Higher Concsiousness factor which can be further divided into
##            clusters based on their scores relative to the total population.
##
## Solution: Use Principle Axis Factoring with Promax rotation to automatically generate
##           factors. Then apply LCA analysis based on the generated factors.
##


# Config
LCAOutputPath <- "output/lca/"
FAOutputPath <- "output/fa/"
generateFactors <- TRUE # will take lots of time if TRUE
generateLCAModel <- TRUE

# Create dirs
if( !dir.exists(LCAOutputPath) ) dir.create(LCAOutputPath)
if( !dir.exists(FAOutputPath) ) dir.create(FAOutputPath)

# Load utility functions
source("R/ses-utility.R") # REQUIRED utility functions
source("R/ses-fa-lca-functions.R") # Load Apollo Kundalini FIRE scoring functions


# Load survey response and variable information
ses.data <- ses.loaddatafile()
# ses.data <- ses.get.questiontext # in case you want question text instead of question codes
ses.vars <- ses.loadvarfile()


# Run factor analysis and extract all factors into an editable CSV.
# The CSV can be edited to rename default factor names.
if(generateFactors) {
  factors <- ses.generate.factors(x = ses.data, threshold = .6, outputPath = FAOutputPath) # This will take a while to run due to parallel analysis
  if( !dir.exists("output/fa") ) dir.create("output/fa")
  write.csv(factors, file = "output/fa/factors.csv", row.names = FALSE)
}

#
# STOP HERE - Edit factors CSV file and rename the factors appropriately
#

# Import the CSV file (names should be added at this point)
factors = read.csv(file = "output/fa/factors.csv")
uniqFactors <- unique(factors[complete.cases(factors),'factor'])

#
# For each factor, generate an LCA model and save to disk
#

library(poLCA)
library(plyr)

# Contains the number of classes for each factor (from the LCA model)
lca.table <- data.frame(
  factor=character(),
  numclasses=integer(),
  stringsAsFactors = FALSE
)

# Contains the percentages of the total pouplation in each class (from the LCA model)
lca.class.table <- data.frame(
  factor=character(),
  class=integer(),
  percentage=numeric(),
  stringsAsFactors = FALSE
)

# lca.results.table <- as.data.frame(ses.data[,c('id', 'token')]) # For Apollo production
lca.results.table <- as.data.frame(x = as.character(ses.data[,c('id')]))
colnames(lca.results.table) <- c('id')
rownames(lca.results.table) <- NULL


kps.numeric <- ses.data
# Ensure all labels are used in the subset
likert.names <- grepl('mystical\\d+|spiritual\\d+|psyphys\\d+|psychic\\d+|talents\\d+|invmov\\d+|sensation\\d+|negphysical\\d+|otherphysical\\d+|negpsych\\d+|psybliss\\d+|psygrowth\\d+',
                      names(kps.numeric))
kps.numeric[,likert.names] <- lapply(kps.numeric[,likert.names], function(x) {
  x <- mapvalues(x,
                 from=c('Not at all', 'Very Weak/low intensity', 'Weak', 'Moderate', 'Strong', 'Very strong/high intensity'),
                 to=c("1", "2","3", "4", "5", "6")
                 , warn_missing = FALSE)
  x <- ordered(x)
  return(x)
})

kps.numeric[is.na(kps.numeric)] <- 1

# uniqFactors = c('mystical-PA1','mystical-PA2') # FOR TESTING ONLY!!!

for(factorName in uniqFactors) {
  # factorName <- 'mystical-PA1'
  factorVars <- as.vector(subset(factors,factor == factorName)$code)
  
  if(length(factorVars) > 1) {
    print(paste("Generating LCA model for",factorName))
    f <- with(kps.numeric, as.formula(
      paste("cbind(",paste(factorVars,collapse=", "),") ~1",sep="")
    )) # E.g., 'cbind(mystical1,mystical10) ~ 1'
    
    print(paste("Using formula:",paste("cbind(",paste(factorVars,collapse=", "),") ~1",sep="")))
    
    bestModel <- NULL
    
    if(!generateLCAModel) { # DO NOT re-generate models
      bestModel <- readRDS(file=paste("output/lca/",paste(factorName,".rda",sep=""), sep=""))
      num.classes <- bestModel$num.classes
    } else { # use models saved to disk
      
      bestModel <- fire.bestLCAModel(kps.numeric, f)  
      
      if(is.null(bestModel)) {
        warning(paste("Skipping model generation for",factorName))
        next()
      } else {
        if( !dir.exists(LCAOutputPath) ) dir.create(LCAOutputPath)
        saveRDS(bestModel, file=paste("output/lca/",paste(factorName,".rda",sep=""), sep=""))
        num.classes <- bestModel$num.classes
      }
    }
    
    # Populate lca.table
    factor.info <- data.frame(factorName, num.classes)
    names(factor.info) <- c("factor", "numclasses")
    lca.table <- rbind(lca.table, factor.info)
    
    # Populate lca.class.table
    classPercentages <- transform(
      table(bestModel$model$predclass) / sum(table(bestModel$model$predclass))
    )
    classStats <- data.frame(factorName, classPercentages)
    names(classStats) <- c('factor', 'class', 'percentage')
    lca.class.table <- rbind(lca.class.table, classStats)
    
    limitCols <-  kps.numeric[c('id',factorVars)]
    limitCols[is.na(limitCols)] <- 1 # Replace all NA's with 1's
    
    result <- apply(limitCols, 1, function(x) {
      if( sum(is.na(x[factorVars])) > 0 ) {
        warning(paste("Discovered NA value on ID",x['id']))
        return()
      } else {
        newRow <- data.frame(
          id = x['id'],
          factor = which.max(poLCA.posterior(
            bestModel$model,
            as.numeric(x[factorVars]))
          )
        )
        colnames(newRow) <- c('id', factorName)
        return(newRow)
      }
    })
    
    tmp <- do.call(rbind, result)
    tmp$id <- factor(tmp$id)
    colnames(tmp) <- c('id', factorName)
    rownames(tmp) <- NULL
    
    lca.results.table <- merge(lca.results.table, tmp, by='id', all.x = TRUE)
  } else {
    warning(paste("Skipping LCA generation for",factorName,"- less than 2 variables!"))
    next()
  }
}

write.csv(lca.table, file = paste(LCAOutputPath,"lca-number-classes.csv",sep = ""), row.names = FALSE)
write.csv(lca.class.table, file = paste(LCAOutputPath,"lca-class-percentages.csv",sep = ""), row.names = FALSE)
write.csv(lca.results.table, file = paste(LCAOutputPath,"lca-respondent-scores.csv",sep = ""), row.names = FALSE)

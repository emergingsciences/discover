# °º¤ø,¸¸,ø¤º°`°º¤ø,¸,ø¤°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤°º¤ø,¸¸,ø¤º°`°º¤:>
#
# Kundalini Findings, Insights, and Recommendations Engine (FIRE)
#
# °º¤ø,¸¸,ø¤º°`°º¤ø,¸,ø¤°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤°º¤ø,¸¸,ø¤º°`°º¤:>


library("poLCA") # LCA functions
library("psych") # LCA library

# Generate factors based on parallel analysis
#
# @param factorGroupCSVLocation The location of the file listing out target
#                               question groups for factor analysis
ses.generate.factors <- function(x = NULL, threshold = .6, outputPath = "output/fa/", factorGroupCSVLocation = "data/ses-question-groups.csv") {
  factors <- NULL
  kps.groups <- read.csv(file = factorGroupCSVLocation)
  
  # Extract loadings in format suitable for further LCA analysis
  for(group in kps.groups$group) {
    print(paste(group,"\\d+",sep=""))
    fa.results <- kfire.fa(
      x,
      grepmatch = paste(group,"\\d+",sep=""),
      prefix = group,
      outputPath = outputPath
    )
    
    groupFactors <- kfire.process.loadings(fa.results$fa$loadings, threshold=threshold, outputPath = outputPath)
    
    if(is.null(factors)) {
      factors <- groupFactors
    } else {
      factors <- rbind(factors, groupFactors)  
    }
  }
  
  return(factors)
}

# Conduct polychoric factor analysis with PAF and promax rotation
# with optional parallel analysis. Also outputs factor analysis
# diagrams and the output of the ICLUST algorithm for examining
# correlations between variables.
#
# See: http://personality-project.org/r/psych/HowTo/factor.pdf
# Contains technical details for all of the above methods
#
# @param grepmatch The regular expression matching what variables you want to pull in
# @param prefix Filename prefix to output factor loadings and correlations matrix
# @param nfactors The number of factors to use as an input for factor analysis
# @param parallel TRUE if you only want to run parallel analysis to determine the number of factors
#           Defaults to FALSE.
#
kfire.fa <- function(data, grepmatch = NULL, prefix = NULL, outputPath="output/fa/") {
  
  # Provide some basic output to show how many data records and
  # the names of the columns for validation
  print(paste("Matched", sum(grepl(grepmatch, names(data))), "columns"))
  print(paste("Names: "
              , paste(
                names(data[,grepl(grepmatch, names(data))])
                , collapse = ", "
              )
  ))
  
  # Exit if no columns match
  if(sum(grepl(grepmatch, names(data))) < 1) return()
  
  # Extract columns specified
  q <- data[,grepl(grepmatch, names(data))]
  q.num <- as.data.frame(lapply(q, as.numeric)) # Convert all values to numeric  
  
  # Run parallel analysis to determine number of factors to use
  q.par <- fa.parallel(x = q.num, cor = "poly", fa = "fa") # Also generates plot
  suggestedFactors <- q.par$nfact
  
  print(paste("Parallel analysis resulted in a",suggestedFactors,"factor solution"))
  
  # Generate factors with rotation
  q.poly.fa.pro <- fa.poly(x = q.num, nfactors = suggestedFactors, fm = "pa", rotate = "promax")
  print(q.poly.fa.pro)
  
  # FA diagram and ICLUST output
  fa.diagram(q.poly.fa.pro)
  clust <- iclust(q.poly.fa.pro$rho)
  
  # Write polychoric correlations to disk
  if( !dir.exists(outputPath) ) dir.create(outputPath)
  write.csv(q.poly.fa.pro$rho, file = paste(outputPath, prefix, "-poly-correlations.csv", sep = ''))
  
  return(q.poly.fa.pro)
}

#' Format factor loadings from the psych fa() family of functions
#' into a user friendly format. Write additional factor info to disk
#'
#' Original loadings usually kept in an object similar to fa.object$fa$loadings
#' @param original.loadings The psych library's fa() loadings
kfire.process.loadings <- function(original.loadings, threshold=0.6, outputPath="output/fa/") {
  var.names <- ses.loadvarfile()
  
  # Create initial data frame from loadings. Create additional columns
  loadings <- data.frame(unclass(original.loadings))
  loadings <- data.frame(
    "code"= rownames(loadings),
    "category" = rownames(loadings),
    "text" = rownames(loadings),
    loadings,
    stringsAsFactors=FALSE)
  # rownames(loadings) = NULL
  
  # Replace question codes with question text
  matches <- match(loadings$text, var.names$varname)
  loadings$text[!is.na(matches)] <- as.character(var.names$question.text[na.omit(matches)])
  loadings$category[!is.na(matches)] <- as.character(var.names$category[na.omit(matches)])
  
  # Loading data frame should be built by now
  
  # Identify which column has the largest loading, above a certain theshold
  factorNames <- apply(loadings, 1, FUN = function(x) {
    factorsOnly <- x[-c(1,2,3)]
    if(as.double(max(factorsOnly)) > threshold) {
      return(paste(x[['category']], names(factorsOnly[which.max(factorsOnly)]), sep="-"))
    } else {
      return(NA)
    }
  })
  factorNames <- as.data.frame(factorNames)
  factorMap <- cbind(loadings$code, loadings$text, factorNames)
  colnames(factorMap) <- c('code', 'text', 'factor')
  rownames(factorMap) <- NULL
  
  rownames(loadings) <- NULL
  if( !dir.exists(outputPath) ) dir.create(outputPath)
  write.csv(loadings, file = paste(outputPath, loadings$category[1], "-loadings.csv", sep = ""))
  
  return(factorMap)
}





# fire.generate.model()
#
# Generates an LCA model for FIRE scoring and stores the model
# for later use. Classes are selected by the lowest BIC
#
# @param x The dataset
# @param f The poLCA formula
# @param maxnclasses The maximum number of classes to test for
# @return The best LCA model
#
fire.bestLCAModel <- function(x, f, maxnclasses = 6) {

  min_bic <- 100000 # Some ridiculous starting max. We're looking for the smallest BIC
  num.classes <- 1
  for(i in 1:maxnclasses){
    lc <- poLCA(f, x, nclass=i, maxiter=3000, 
                tol=1e-5, na.rm=TRUE,  
                nrep=10, verbose=FALSE, calc.se=TRUE)
    print(paste("BIC of a", i, "class model is", lc$bic))
    if(lc$bic < min_bic && !lc$eflag){
      num.classes <- i
      min_bic <- lc$bic
      LCA_best_model<-lc
    }
  }
  
  if(exists("LCA_best_model") && num.classes > 1) {
    print(paste("Best model: BIC",LCA_best_model$bic,"with",num.classes,"classes"))
    
    intenseVals <- lapply(LCA_best_model$probs, function(x) {
      return(rowMeans(x[,c(ncol(x), ncol(x)-1)])) # Get the means of the "strong" responses
    })
    
    avgVec <- vector()
    for(i in 1:length(intenseVals)) {
      avgVec <- rbind(avgVec, unlist(intenseVals[i]))
    }
    ord.vec <- order(colMeans(avgVec))
    
    LCA_best_model <- poLCA(f,x,nclass=num.classes, verbose = FALSE, na.rm = TRUE
                            ,probs.start= poLCA.reorder(
                              LCA_best_model$probs
                              , ord.vec
                            )
    )
    
    return(list(model=LCA_best_model, num.classes=num.classes))
  }
  
  if(exists("LCA_best_model") && length(unique(LCA_best_model$predclass)) == 1) {
    warning(paste("One class model generated."))
    return(NULL)
  } else {
    return(LCA_best_model)  
  }
}
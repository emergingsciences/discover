# 
# Spiritual Experience Survey Utility Functions
#
# This script contains:
#   - Basic data loading functions
#   - Mapping functions to use whenever we would like to use full question text
#     instead of just the question codes
#

# # # # # # # # # # # #
# Survey Data Load ----
# # # # # # # # # # # #

# Load the SES respondent data file
# @return The SES respondent data file
ses.loaddatafile <- function(file = "data/ses-data.txt") {
  df <- dget(file)
  return(df)
}


# Loads the SES variable information file
# @return The SES variable information file
ses.loadvarfile <- function(file = "data/ses-vars.csv") {
  return(read.csv(file = file))
}


# Extract columns by a regex matching the column names, with all data converted
# into numeric values
# @return A subset of the data containing only columns specified in the regex
extract.numeric.columns.by.regex <- function(data, grepmatch = "mystical\\d+|spiritual\\d+|psyphys\\d+|psychic\\d+|talents\\d+|invmov\\d+|sensation\\d+|negphysical\\d+|otherphysical\\d+|negpsych\\d+|psybliss\\d+|psygrowth\\d+") {
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
  return(q.num)
}

# Loads the SES data file and then replaces question codes with question text
# @return The SES data file with question text instead of question codes (default)
ses.get.questiontext <- function(x = ses.loaddatafile(), max_length = NULL) {
  var.names <- ses.loadvarfile()
  mv <- match(names(x), var.names$varname)
  new_names <- as.character(var.names$question.text[na.omit(mv)])
  
  if (!is.null(max_length)) {
    new_names <- sapply(new_names, function(name) {
      if (is.na(max_length) || max_length < 1) {
        return(name)
      } else {
        substr(name, 1, max_length)
      }
    })
  }
  
  colnames(x)[!is.na(mv)] <- new_names
  return(x)
}

ses.likert.to.numerical <- function(x) {
  likert.names <- grepl('mystical\\d+|spiritual\\d+|psyphys\\d+|psychic\\d+|talents\\d+|invmov\\d+|sensation\\d+|negphysical\\d+|otherphysical\\d+|negpsych\\d+|psybliss\\d+|psygrowth\\d+',
                        names(x))
  x[,likert.names] <- lapply(x[,likert.names], function(x) {
    x <- mapvalues(x, 
                   from=c('Not at all', 'Very Weak/low intensity', 'Weak', 'Moderate', 'Strong', 'Very strong/high intensity'), 
                   to=c("1", "2","3", "4", "5", "6"))
    x <- ordered(x)
    return(x)
  })
  
  return(x)
}

# # # # # # # # # # # #
# Output Utilities ----
# # # # # # # # # # # #

# Limit console output to n_rows
#
# nrows - Specify the number of rows you want to display (e.g., first 10 rows)
limit.output <- function(consoleObject, n_rows = 10) {
  # Create a summary object
  summary_result <- consoleObject
  # Capture and store the summary as text
  summary_text <- capture.output(summary_result)
  
  # Display the first n rows of the summary
  cat(paste(head(summary_text, n_rows), collapse = "\n"))
}


# Format factor loadings from the psych fa() family of functions
# into a user friendly format.
#
# Original loadings usually kept in an object similar to fa.object$fa$loadings
#
ses.format.loadings <- function(original.loadings = NULL) {
  var.names <- ses.loadvarfile()
  
  # Create initial data frame from loadings. Create additional columns
  loadings <- data.frame(unclass(original.loadings))
  loadings <- data.frame(
	"code"= rownames(loadings),
	"text" = rownames(loadings),
	loadings,
	stringsAsFactors=FALSE)
  rownames(loadings) = NULL
  
  # Replace question codes with question text
  matches <- match(loadings$text, var.names$varname)
  loadings$text[!is.na(matches)] <- as.character(var.names$question.text[na.omit(matches)])
  return(loadings)
}


# High-level Analysis Utilities ----
ses.qgroup <- function(fname = "", grepmatch = "", parallel = F, nfactors = NULL, omit.na = F) {
  return.list <- list()
  
  data.num <- extract.numeric.columns.by.regex(ses.data, grepmatch)
  if(omit.na == T) {
    data.num <- na.omit(data.num)
  }
  return.list <- c(return.list, list(data=data.num))
  
  #
  ## FA Reliability Tests (pre-FA) ----
  #
  
  # Kaiser-Meyer-Olkin measure of sampling adequacy
  print("KMO Output")
  flush.console()
  kmo <- psych::KMO(data.num)[["MSA"]]
  print(kmo)
  return.list <- c(return.list, list(kmo=kmo))
  
  # Bartlett’s Test of Sphericity
  print("Bartlett's Test Output")
  flush.console()
  bart <- bartlett.test(data.num) # result is greater than the critical value for chi2
  print(bart)
  return.list <- c(return.list, list(bartlett = bart))
  
  # Cronbach's Alpha
  print("Cronbach's Alpha")
  flush.console()
  alf <- psych::alpha(data.num)
  print(alf)
  return.list <- c(return.list, list(alpha = alf))
  
  
  #
  ## Select # of Factors ----
  #  
  
  # Cattell's scree test
  # Raymond B. Cattell (1966) The Scree Test For The Number Of Factors, Multivariate Behavioral Research, 1:2, 245-276, DOI: 10.1207/s15327906mbr0102_10
  print("Cattell's scree test")
  flush.console()  
  sc <- scree(data.num)
  print(sc)
  return.list <- c(return.list, list(scree = sc))
  
  # Revelle’s Very Simple Structure
  # Revelle, W., & Rocklin, T. (1979). Very simple structure: An alternative procedure for estimating the optimal number of interpretable factors. Multivariate behavioral research, 14(4), 403-414.
  print("VSS")
  flush.console()   
  vss <- vss(data.num)
  print(vss)
  return.list <- c(return.list, list(vss=vss))
  
  ## Experience Item ICLUST ----
  # Revelle, W. (1978). ICLUST: A cluster analytic approach to exploratory and confirmatory scale construction. Behavior Research Methods & Instrumentation, 10(5), 739-742.
  # print("Generating polychoric correlations matrix")
  # flush.console()
  # pchor <- polychoric(data.num)
  # iclust <- iclust(pchor$rho)
  # iclust <- iclust(data.num)  
  
  # Horn’s parallel analysis
  # Horn, J.L. A rationale and test for the number of factors in factor analysis. Psychometrika 30, 179–185 (1965). https://doi.org/10.1007/BF02289447
  # Dinno, A. (2009). Exploring the sensitivity of Horn's parallel analysis to the distributional form of random data. Multivariate behavioral research, 44(3), 362-388.
  if(parallel) {
    print("Parallel analysis")
    flush.console()
    par.pa <- fa.parallel(x = data.num, cor = "poly", fa = "fa", fm = "pa", sim = TRUE, n.iter=20)  
    print(par.pa)
    return.list <- c(return.list, list(parallel=par.pa))
    # par.pc <- fa.parallel(x = data.num, cor = "poly", fa = "pc", n.iter=20)
    print("Parallel analysis completed. Not running factor extraction unless parallel is F and nfactors is specified")
    flush.console()
    return(return.list)
  }

  if(!nfactors > 0) {
    print("nfactors not specified. Returning")
    flush.console()
    return(return.list)
  }
  
  #
  ## Factor Extraction ----
  #
  data.fa.res <- fa(r = data.num, nfactors = nfactors, fm = "pa", rotate = "promax", cor = "poly")
  return.list <- c(return.list, list(fa=data.fa.res))
  fa.diagram(data.fa.res)
  # pca.res <- principal(r = data.num, nfactors = 2, cor = "poly")
  # pca.loadings <- as.data.frame.matrix(pca.res$loadings)
  
  # McDonald's Omega(h and t)
  # Revelle, W., & Condon, D. M. (2019). Reliability from α to ω: A tutorial. Psychological assessment, 31(12), 1395.
  print("Omega")
  omega <- omega(m = data.num, nfactors = nfactors, fm="pa", rotate="promax", poly = T)
  print(omega)
  return.list <- c(return.list, list(omega=omega))
  
  # Factor loadings (ouput to CSV)
  print(paste0("Writing loadings to ", fname,"-loadings.csv file"))
  friendly.loadings <- ses.format.loadings(data.fa.res$loadings)
  write.csv(friendly.loadings, file = paste("outputs/", fname, "-loadings.csv", sep = ''))
  
  # Factor correlations (ouput to CSV)
  write.csv(data.fa.res$Phi, file = paste("outputs/", fname, "-factorcorrelations.csv", sep = ''))
  
  return(return.list)
}

ses.regsem <- function(grepmatch, hc.mod, data, n.lambda, jump) {
  mod <- hc.mod # Start with the HC model
  all.vars <- paste('g ~', colnames(data[,grepl(grepmatch, names(data))]),collapse="\n")
  
  mod <- paste(mod, all.vars)
  print(paste("Model: ", mod))
  reg.mod <- mod
  reg.mod <- paste0(reg.mod, "\n", 'g =~ NA*unityconsc + bliss + insight + energy + light')
  reg.mod <- paste0(reg.mod, "\n", 'g ~~ 1*g')
  cfa <- cfa(reg.mod, data=as.data.frame(scale(data)), orthogonal = F)
  summary(cfa, fit.measures = TRUE, standardized = TRUE)
  reg.out <- cv_regsem(cfa,n.lambda=n.lambda,jump=jump,type="alasso",pars_pen="regressions")
  plot(reg.out)
  summary(reg.out)
  return(as.data.frame(reg.out$final_pars))
}

ses.kfold <- function(dats, n.folds, reps, mod){
  print(paste("Model: ", mod))
  print(paste("Record count: ", nrow(dats)))
  
  results <- data.frame(matrix(ncol=5,nrow=0, dimnames=list(NULL, c(
    "model", "cfi", "tli", "rmsea", "srmr")
  )))     
  
  folds = rep(1:n.folds, length.out = nrow(dats)) 
  r <- 1
  
  while(r <= reps) {
    folds = sample(folds) # Random permutation
    print(paste("Repetition",r))
    
    repetition_success <- TRUE
    
    fold_results <- results
    for (i in 1:n.folds){
      indis <- which(folds == i)
      print(paste("Fitting on fold with", length(indis), "rows"))
      
      repetition_success <- tryCatch(
        {
          # cfa.fit <- cfa(mod, data=dats[indis,], ordered = F, std.lv = T, estimator = "MLR")
          cfa.fit <- cfa(mod, data=dats[indis,], ordered = F, std.lv = T, estimator = "MLR")
          
          if (lavInspect(cfa, "converged")) {
            fit.df <- data.frame(
              model = "HC",
              cfi = fitmeasures(cfa.fit, "cfi.robust"),
              tli = fitmeasures(cfa.fit, "tli.robust"),
              rmsea = fitmeasures(cfa.fit, "rmsea.robust"),
              srmr = fitmeasures(cfa.fit, "srmr")
            )
            
            fold_results <- rbind(fold_results, fit.df)
            rownames(fold_results) = NULL
            TRUE
          }
          else {
            print(e)
            print("Model did not converge. Repeating the current repetition.")
            FALSE
          }
        }, warning = function(e) {
          print(e)
          print("Warning during model fitting. Repeating the current repetition.")
          TRUE
        }, error = function(e) {
          print(e)
          print("Error in model fitting. Repeating the current repetition.")
          FALSE
        }) # end tryCatch
      
      if (!repetition_success) break
    } # end for loop of folds
    
    if (repetition_success) {
      results <- fold_results
      r <- r + 1
    }    
    
  } # end while loop of repetitions
  
  return(as.data.frame(results))
}

# Out of sample K-Fold
ses.pred_kfold <- function(dats, mod, n.folds, reps, xnames, ynames, lambda.seq){
  results <- data.frame(matrix(ncol=2,nrow=0, dimnames=list(NULL, c(
    "model", "rmsep")
  )))
  
  # args <- list(...)
  
  folds = rep(1:n.folds, length.out = nrow(dats)) 
  
  for(r in 1:reps) {
    folds = sample(folds) # Random permutation
    print(paste("Repetition",r))
    
    yhat <- matrix(NA, nrow(dats), length(ynames))
    yhat2 <- matrix(NA, nrow(dats), length(ynames))
    yhat3 <- matrix(NA, nrow(dats), length(ynames))
    yhat4 <- matrix(NA, nrow(dats), length(ynames))
    
    for (i in 1:n.folds){
      indis <- which(folds == i)
      
      # OOS Approach
      cfa <- sem(mod, data=dats[-indis,], ordered = F, estimator = "MLR", meanstructure = T)
      yhat[indis,] = lavPredictY(cfa, newdata = dats[indis,], xnames = xnames, ynames = ynames)
      
      # RO-SEM Approach
      reg.results <- cv.lavPredictYReg(cfa, dats[-indis,], xnames, ynames, n.folds = 10, lambda.seq = lambda.seq)
      lambda <- reg.results$lambda.min
      print(paste("lambda.min: ",lambda))
      # lambda <- cv.reg$lambda.min
      yhat2[indis,] = lavPredictYreg(cfa, newdata = dats[indis,], xnames = xnames, ynames = ynames, lambda = lambda)            

      # Regularized Linear Regression Approach
      cv.reg <- cv.glmnet(
        as.matrix(dats[-indis,xnames]),
        as.matrix(dats[-indis,ynames]),
        family = "mgaussian",
        alpha = 0, # Ridge
        lambda = lambda.seq
      )
      print(paste("Linear reg lambda min:",cv.reg$lambda.min))
      regmod <- glmnet(
        as.matrix(dats[-indis,xnames]),
        as.matrix(dats[-indis,ynames]),
        family = "mgaussian",
        alpha = 0,
        lambda = cv.reg$lambda.min
      )
      yhat3[indis,] <- predict(regmod, as.matrix(dats[indis,xnames]), s = "lambda.min")
      
      # Linear Regression Approach
      dats.sub <- dats[,c(xnames, ynames)]
      fit <- lm(formula(
        paste('cbind(',
          paste(ynames, collapse=","),
          ") ~ .")
        ),
        data = dats.sub[-indis , ])
      yhat4[indis, ] <- predict(fit, newdata = dats[indis , ])      
    }
    
    # Global RMSEp
    rmsep <-  sqrt(sum((dats[,ynames] - yhat )^2)/(length(ynames) * nrow(dats)))
    rmsep2 <- sqrt(sum((dats[,ynames] - yhat2)^2)/(length(ynames) * nrow(dats)))
    rmsep3 <- sqrt(sum((dats[,ynames] - yhat3)^2)/(length(ynames) * nrow(dats)))
    rmsep4 <- sqrt(sum((dats[,ynames] - yhat4)^2)/(length(ynames) * nrow(dats)))
    
    results <- rbind(results, data.frame(model = 1, rmsep = rmsep))
    results <- rbind(results, data.frame(model = 2, rmsep = rmsep2))
    results <- rbind(results, data.frame(model = 3, rmsep = rmsep3))
    results <- rbind(results, data.frame(model = 4, rmsep = rmsep4))
  }
  
  return(as.data.frame(results))
}



# To get results to clipboard
# clipr::write_clip(results)

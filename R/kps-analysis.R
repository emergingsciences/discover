# °º¤ø,¸¸,ø¤º°`°º¤ø,¸,ø¤°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤°º¤ø,¸¸,ø¤:>
#
#          KUNDALINI PROFILE SURVEY ANALYSIS
#
# °º¤ø,¸¸,ø¤º°`°º¤ø,¸,ø¤°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤°º¤ø,¸¸,ø¤:>

source("R/kps-utility.R")

#
# FULL DATA LOAD (respondent data and variable mappings)
#
kps.data <- kps.loaddatafile()
kps.vars <- kps.loadvarfile()


#
# DATA SUMMARY VISUALIZATIONS
#

library(ggplot2)
library(likert)

# Total number of participants
barplot(nrow (kps.data), width = 1, main="Kundalini Profile Survey", ylim=c(0,400), ylab="Number of Participants", col="darkgreen")

# Number of participants by sex
ggplot(data=kps.data, aes(x = kps.data$sex, fill = kps.data$sex)) +
  guides(fill = FALSE) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..), vjust=-1) +
  labs(x="Sex of Participant", y="Number of participants", fill="Sex")

# Age histogram
# Credit: https://www.datacamp.com/community/tutorials/make-histogram-ggplot2#gs.ko0NeIE
ggplot(data=kps.data, aes(x = kps.data$age, width = .4)) +
  geom_histogram(binwidth = 5, col = "white", aes(fill =..count..), alpha = .8) +
  labs(x="Age of Participant", y="Count") +
  scale_x_continuous(breaks = seq(0, 100, by = 5))

# Mystical likert visualization
q <- kps.data[,grepl("mystical\\d+", names(kps.data))]
q.questiontext <- kps.get.questiontext(q)
plot(likert(q.questiontext), centered = FALSE)

# Spiritual likert visualization
q <- kps.data[,grepl("spiritual\\d+", names(kps.data))]
q.questiontext <- kps.get.questiontext(q)
plot(likert(q.questiontext), centered = FALSE)

# PsychoPhysical likert visualization
q <- kps.data[,grepl("psyphys\\d+", names(kps.data))]
q.questiontext <- kps.get.questiontext(q)
plot(likert(q.questiontext), centered = FALSE)

# Psychic likert visualization
q <- kps.data[,grepl("psychic\\d+", names(kps.data))]
q.questiontext <- kps.get.questiontext(q)
plot(likert(q.questiontext), centered = FALSE)

# Talents likert visualization
q <- kps.data[,grepl("talents\\d+", names(kps.data))]
q.questiontext <- kps.get.questiontext(q)
plot(likert(q.questiontext), centered = FALSE)



#
# COMPARATIVE FACTOR ANALYSIS
#

# kps.fa() - Conduct polychoric factor analysis with PAF and promax rotation
#            with optional parallel analysis. Also outputs factor analysis
#            diagrams and the output of the ICLUST algorithm for examining
#            correlations between variables. Typically run first to determine 
#            the number of factors using the "parallel" parameter, then without
#            "parallel" with the desired number of factors.
#
#            See: http://personality-project.org/r/psych/HowTo/factor.pdf
#            Contains technical details for all of the above methods
#
#
# Parameters
#
# grepmatch: The regular expression matching what variables you want to pull in
# prefix: Filename prefix to output factor loadings and correlations matrix
# nfactors: The number of factors to use as an input for factor analysis
# parallel: TRUE if you only want to run parallel analysis to determine the number of factors
#           Defaults to FALSE.
#

library(psych)

kps.fa <- function(data, grepmatch = NULL, prefix = "default", nfactors = 1, parallel = FALSE, scores = NULL) {
 
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
   
  # Run parallel analysis
  if(parallel == TRUE) {
    q.par <- fa.parallel(x = q.num, cor = "poly", fa = "fa") # Also generates plot
    print(q.par) # Will suggest number of factors
    return()
  }
  
  # Generate factors with rotation
  q.poly.fa.pro <- fa.poly(x = q.num, nfactors = nfactors, fm = "pa", rotate = "promax")
  print(q.poly.fa.pro)
  loadings <- kps.format.loadings(q.poly.fa.pro$fa$loadings)
  
  # FA diagram and ICLUST output (runs fa.poly twice)
  fa.diagram(q.poly.fa.pro)
  clust <- iclust(q.poly.fa.pro$rho)
  
  # Export to CSV to analyze/filter in a spreadsheet app
  write.csv(loadings, file = paste("output/", prefix, "-loadings.csv", sep = ''))
  write.csv(q.poly.fa.pro$rho, file = paste("output/", prefix, "-poly-correlations.csv", sep = ''))
  
  #
  # Run again with question text
  #
  q.num <- kps.get.questiontext(q.num)
  
  if(!is.null(scores)) {
    q.poly.fa.pro <- fa.poly(x = q.num, nfactors = nfactors, fm = "pa", rotate = "promax", scores = "tenBerge")  
  } else {
    q.poly.fa.pro <- fa.poly(x = q.num, nfactors = nfactors, fm = "pa", rotate = "promax")
  }
  
  fa.diagram(q.poly.fa.pro)
  clust <- iclust(q.poly.fa.pro$rho)
  
  return(q.poly.fa.pro)
}

kps.data.withfascores <- kps.data

# These can take (very) long depending on the number of variables

# 4/5/2017 - n = 449 - Parallel analysis factors = 3
fa.results <- kps.fa(scores = "regression"
                     ,kps.data, grepmatch = "mystical\\d+"
                     ,prefix = "mystical"
                     ,nfactors = "4", parallel=TRUE)
# scores <- fa.results$scores$scores
# colnames(scores)[colnames(scores)=="PA1"] <- "CONSCIOUSNESS"
# colnames(scores)[colnames(scores)=="PA2"] <- "GRACE"
# colnames(scores)[colnames(scores)=="PA3"] <- "BLISS"
# kps.data.withfascores <- cbind(kps.data.withfascores, scores)

# 4/5/2017 - n = 449 - Parallel analysis factors = 6
fa.results <- kps.fa(scores = "regression"
                     , kps.data
                     , grepmatch = "spiritual\\d+"
                     , prefix = "spiritual"
                     , nfactors = 6)

# scores <- fa.results$scores$scores
# colnames(scores)[colnames(scores)=="PA1"] <- "SYNCHRONICITY"
# colnames(scores)[colnames(scores)=="PA2"] <- "OOB"
# colnames(scores)[colnames(scores)=="PA3"] <- "REBIRTH"
# colnames(scores)[colnames(scores)=="PA4"] <- "AURAL"
# colnames(scores)[colnames(scores)=="PA5"] <- "INTUITION"
# kps.data.withfascores <- cbind(kps.data.withfascores, scores)

# 4/5/2017 - n = 449 - Parallel analysis factors = 4
fa.results <- kps.fa(scores = "regression"
                     , kps.data, grepmatch = "psyphys\\d+"
                     , prefix = "psyphys"
                     , nfactors = 4)

# scores <- fa.results$scores$scores
# colnames(scores)[colnames(scores)=="PA1"] <- "ENERGY"
# colnames(scores)[colnames(scores)=="PA2"] <- "LIGHT"
# kps.data.withfascores <- cbind(kps.data.withfascores, scores)

fa.results <- kps.fa(scores = "regression", kps.data, grepmatch = "psychic\\d+", prefix = "psychic", nfactors = 3, parallel = TRUE) # 11/10 - n = 371 - Parallel analysis factors = 3

fa.results <- kps.fa(scores = "regression", kps.data, grepmatch = "talents\\d+", prefix = "talents", nfactors = 3) # 11/10 - n = 371 - Parallel analysis factors = 3

fa.results <- kps.fa(scores = "regression", kps.data, grepmatch = "invmov\\d+", prefix = "invmov", nfactors = 6) # 11/10 - n = 371 - Parallel analysis factors = 6

fa.results <- kps.fa(scores = "regression", kps.data, grepmatch = "sensation\\d+", prefix = "sensation", nfactors = 4) # 11/10 - n = 371 - Parallel analysis factors = 4

fa.results <- kps.fa(scores = "regression", kps.data, grepmatch = "negphysical\\d+", prefix = "negphysical", nfactors = 7) # 11/10 - n = 371 - Parallel analysis factors = 7

fa.results <- kps.fa(scores = "regression", kps.data, grepmatch = "otherphysical\\d+", prefix = "otherphysical", nfactors = 9) # 11/10 - n = 371 - Parallel analysis factors = 9

# 4/5/2017 - n = 449 - Parallel analysis factors = 8
kps.data.pg <- subset(kps.data, psygrowth.gate == "Y")
fa.results <- kps.fa(scores = "regression"
                     , kps.data.pg
                     , grepmatch = "psygrowth\\d+"
                     , prefix = "psygrowth"
                     , nfactors = 8)

fa.results <- kps.fa(scores = "regression", kps.data, grepmatch = "negpsych\\d+", prefix = "negpsych", nfactors = 7) # 11/10 - n = 371 - Parallel analysis factors = 7

# 4/5/2017 - n = 449 - Parallel analysis factors = 4
kps.data.pb <- subset(kps.data, psybliss.gate == "Y")
fa.results <- kps.fa(scores = "regression"
                     , kps.data.pb
                     , grepmatch = "psybliss\\d+"
                     , prefix = "psybliss"
                     , nfactors = 4)


# # Factor correlations
# kps.data.withfascores <- kps.data.withfascores[,c("CONSCIOUSNESS", "GRACE", "BLISS", "SYNCHRONICITY", "OOB", "REBIRTH", "AURAL", "INTUITION", "ENERGY", "LIGHT")]
# cor.mat <- cor(method = "pearson", x = kps.data.withfascores)
# cor.mat.melt <- melt(cor.mat)
# cor.plot(cor.mat, numbers = TRUE)
# plot(kps.data.withfascores$ENERGY, kps.data.withfascores$LIGHT)


# Sample scatterplot
plot(
  jitter(as.numeric(kps.data$mystical24), factor = 1.2)
  , jitter(as.numeric(kps.data$mystical22), factor = 1.2)
)



# Psygrowth factor analysis
kps.fa(data = subset(kps.data, kps.data$psygrowth.gate == "Y"),
       grepmatch = "psygrowth\\d+",
       prefix = "psygrowth",
       nfactors = 6) # 11/08 - n = 366 - Parallel analysis factors = 6

# Psybliss factor analysis
kps.fa(data = subset(kps.data, kps.data$psybliss.gate == "Y"),
       grepmatch = "psybliss\\d+",
       prefix = "psybliss",
       nfactors = 4) # 10/26 - n = 366 - Parallel analysis factors = 4




# Example polychoric correlations matrix to faciliate data analysis

library(psych)
raw.data <- kps.data[,grepl("psybliss\\d+", names(kps.data))]
raw.data <- lapply(raw.data, as.ordered)
raw.data <- lapply(raw.data, as.numeric)
poly.results <- polychoric(as.data.frame(raw.data))
cor.plot(poly.results$rho, numbers = TRUE)


# TODO: Implement IRT FA information
# q.irt.fa <- irt.fa(x = q.num, nfactors = 3, fm = "pa", rotate = "promax")



#
# MCLASS CREATION
#

#
# Conduct Latent Class Analysis to analyze response patterns
# Source: http://statistics.ohlsen-web.de/latent-class-analysis-polca/
#

library("poLCA")
library("reshape2")
library("plyr")
library("ggplot2")

# Format data properly. Convert text to numbers and then to ordered
q <- kps.data

likert.names <- grepl('mystical\\d+|spiritual\\d+|psyphys\\d+|psychic\\d+|talents\\d+|invmov\\d+|sensation\\d+|negphysical\\d+|otherphysical\\d+|negpsych\\d+|psybliss\\d+|psygrowth\\d+',
                      names(q))
q[,likert.names] <- lapply(q[,likert.names], function(x) {
  x <- mapvalues(x, 
                 from=c('Not at all', 'Very Weak/low intensity', 'Weak', 'Moderate', 'Strong', 'Very strong/high intensity'), 
                 to=c("1", "2","3", "4", "5", "6"))
  x <- ordered(x)
  return(x)
})


#
# Define the model function
# TODO: Create empirical criteria for variable selection
#

# CONSCIOUSNESS BLISS Model Option (no covariates, only indicators)
# CONSCIOUSNESS
f<-with(q, cbind(
  mystical24, # Experience of Higher consciousness/cosmic consciousness
  mystical22, # Expansion of consciousness
  mystical27, # Revelation: Knowledge that comes from a divine source where the individual becomes aware of the source of that knowledge
  mystical26, # Personal identification with all of creation
  mystical15, # Expanded comprehension of reality
  mystical6, # New knowledge / awareness of the unbounded intelligence behind universe
  mystical25, # An experience of union with the Divine-God or universal consciousness
  mystical12, # Experience of deep unity and expansive consciousness
  mystical13, # All sense of separateness disappears
  mystical23, # Union with Life Energy
  mystical2, # Expansion / explosion of consciousness
  mystical10, # Increased feelings of unity with creation
  mystical14, # Profound feelings of connection with a spiritural source
  mystical8 # Loss of fear of death / certainty of immortality
)~1)


#
# KPS LCA clustering
#
# x - The data to cluster
# f - The formula
kps.cluster <- function(x, f, newVarName, c = NULL) {
  
  if(is.null(c)) {
    min_bic <- 100000 # some ridiculous starting max
    for(i in 2:6){
      lc <- poLCA(f, x, nclass=i, maxiter=3000, 
                  tol=1e-5, na.rm=TRUE,  
                  nrep=10, verbose=FALSE, calc.se=TRUE)
      if(lc$bic < min_bic && !lc$eflag){
        min_bic <- lc$bic
        LCA_best_model<-lc
      }
    }
  } else {
    LCA_best_model <- poLCA(f, x, nclass=c, maxiter=3000, 
                            tol=1e-5, na.rm=TRUE,  
                            nrep=10, verbose=FALSE, calc.se=TRUE)
  }
  
  if(exists("LCA_best_model")) {
    num.classes <- length(unique(LCA_best_model$predclass))
    
    intenseVals <- lapply(LCA_best_model$probs, function(x) {
      return(rowMeans(x[,c(ncol(x), ncol(x)-1)])) # Get the means of the "strong" responses
    })
    
    avgVec <- vector()
    for(i in 1:length(intenseVals)) {
      avgVec <- rbind(avgVec, unlist(intenseVals[i]))
    }
    ord.vec <- order(colMeans(avgVec))
    
    LCA_best_model <- poLCA(f,x,nclass=num.classes, verbose = FALSE
                            ,probs.start= poLCA.reorder(
                              LCA_best_model$probs
                              , ord.vec
                            )
    )
    
    plot(LCA_best_model)
    title(main = newVarName)
    print(paste(num.classes,"classes generated for",newVarName))
    
    x[newVarName] <- ordered(LCA_best_model$predclass)  
    
    # Plots

    # Graph displaying classes by question
    lcModelProbs <- melt(LCA_best_model$probs)
    zp1 <- ggplot(lcModelProbs,aes(x = L1, y = value, fill = Var2))
    zp1 <- zp1 + geom_bar(stat = "identity", position = "stack")
    zp1 <- zp1 + facet_grid(Var1 ~ .) 
    zp1 <- zp1 + theme_bw()
    zp1 <- zp1 + labs(x = "Questions",y="Class Respones Distribution", fill ="Likert Responses")
    zp1 <- zp1 + theme( axis.text.y=element_blank(),
                        axis.ticks.y=element_blank(),                    
                        panel.grid.major.y=element_blank(),
                        axis.text.x=element_text(angle = 45, hjust = 1)
    )
    zp1 <- zp1 + guides(fill = guide_legend(reverse=TRUE))
    print(zp1)
    
    
    # Graph displaying questions by class
    lcModelProbs <- melt(LCA_best_model$probs)
    zp2 <- ggplot(lcModelProbs,
                  aes(x = Var1, y = value, fill = Var2))
    zp2 <- zp2 + geom_bar(stat = "identity", position = "stack")
    zp2 <- zp2 + facet_wrap(~ L1)
    zp2 <- zp2 + scale_x_discrete("Class", expand = c(0, 0))
    zp2 <- zp2 + scale_y_continuous("Proportion", expand = c(0, 0))
    zp2 <- zp2 + theme_bw()
    print(zp2)
  }
  
  return(x)
}

q <- kps.cluster(q, f, "MCLASS", c = 2)




#
# DECISION TREES (WITH MCLASS AVAILABLE)
#

# Source: http://www.edureka.co/blog/implementation-of-decision-tree/

library(rpart)
library(rpart.plot) # See: http://www.milbo.org/rpart-plot/prp.pdf
library(plyr)

# MCLASS psychological decision trees

q.sub <- subset(q, psybliss.gate == "Y")
tree <- rpart(
  MCLASS ~ .
  , data = q.sub[,grepl('psybliss\\d+|MCLASS', names(q.sub))]
)
rpart.plot(tree, tweak = 1.1)

q.sub <- subset(q, psygrowth.gate == "Y")
tree <- rpart(
  MCLASS ~ .
  , data = q.sub[,grepl('psygrowth\\d+|MCLASS', names(q.sub))]
)
rpart.plot(tree, tweak = 1.1)


# Negative physical symtoms decision trees

q.sub <- subset(q, psygrowth.gate == "Y")
tree <- rpart(
  pe.negphysical.gate ~ .
  , data = q.sub[,grepl('pe.negphysical.gate|psygrowth\\d+|MCLASS', names(q.sub))]
)
rpart.plot(tree, tweak = 1.25)

q.sub <- subset(q, psybliss.gate == "Y")
tree <- rpart(
  pe.negphysical.gate ~ .
  , data = q.sub[,grepl('pe.negphysical.gate|psybliss\\d+|MCLASS', names(q.sub))]
)
rpart.plot(tree, tweak = 1.1)


# Negative psychological decision tree

q.sub <- subset(q, psybliss.gate == "Y")
tree <- rpart(
  negpsych.gate ~ .
  , data = q.sub[,grepl('negpsych.gate|psybliss\\d+|MCLASS', names(q.sub))]
)
rpart.plot(tree, tweak = 1.1)


q.sub <- subset(q, psygrowth.gate == "Y")
tree <- rpart(
  negpsych.gate ~ .
  , data = q.sub[,grepl('negpsych.gate|psygrowth\\d+|MCLASS', names(q.sub))]
)
rpart.plot(tree, tweak = 1.1)


q.sub <- q
tree <- rpart(
  MCLASS ~ .
  , data = q.sub[,grepl('talents\\d+|MCLASS', names(q.sub))]
)
rpart.plot(tree, tweak = 1.1)



#
# MCLASS PROFILING
#

library(coin)
library(plyr)
library(polycor)

# Returns a data frame containing all descriptive and statistical output
kps.profile <- function(data = NULL, target = NULL, grepstr = "", prefix = "") {
  
  # Populate "compare" data frame one row at a time
  
  compare <- data.frame( # Destination for all our statistics
                        question = character(),
                        polychor = double(),
                        chisq = double(),
                        # wilcox.u = double(), # Mann-Whitney U statistic
                        # wilcox.p = double(), # P-value
                        # wilcox.r = double(), # Effect size
                        # median.m1 = double(), # Median of mclass 1
                        # median.m2 = double(), # Median of mclass 2
                        # median.diff = double(), # m2 minus m1
                        # t.test.t = double(), # T-test t statistic
                        # t.test.p = double(), # T-test p-value
                    stringsAsFactors = FALSE)
  
  varnames <- names( data[,!grepl(target, names(data))]) # All but the MCLASS
  
  for(i in 1:length(varnames)) {
    compare[i,"question"] <- varnames[i]
    
    # Conduct a Mann-Whitney U Test
    # This generates the u (sometimes called w), p, and r statistics. For more information
    # see the resources below:
    #
    # http://www.stata-journal.com/sjpdf.html?articlenum=st0253
    # https://statistics.laerd.com/premium-sample/mwut/mann-whitney-test-in-spss-2.php
    # http://yatani.jp/teaching/doku.php?id=hcistats:mannwhitney
    
    if(length(levels(data$MCLASS)) == 2) {
      wc <- wilcox.test(as.formula(paste("as.numeric(",varnames[i],") ~ MCLASS")), data = data)
      compare[i,"wilcox.u"] <- wc$statistic
      compare[i,"wilcox.p"] <- wc$p.value

      wc <- wilcox_test(as.formula(paste("as.numeric(",varnames[i],") ~ MCLASS")), data = data)
      compare[i,"wilcox.r"] <- statistic(wc) / sqrt(nrow(data))
      
      # Conduct a standard t test
      
      t <- t.test(as.numeric(subset(data[,varnames[i]], data$MCLASS == 1)), as.numeric(subset(data[,varnames[i]], data$MCLASS == 2)))
      
      compare[i,"t.test.t"] <- t$statistic
      compare[i,"t.test.p"] <- t$p.value
      
      compare[i,"median.m1"] <- median(as.numeric(data[data$MCLASS == "1",varnames[i]]))
      compare[i,"median.m2"] <- median(as.numeric(data[data$MCLASS == "2",varnames[i]]))
      compare[i,"median.diff"] <- compare[i,"median.m2"] - compare[i,"median.m1"]
    }
    
    compare[i, "polychor"] <- polychor(data[,target], data[,varnames[i]])
    
    chiresult <- chisq.test(data[,target], data[,varnames[i]])
    compare[i, "chisq"] <- as.double(chiresult$statistic)
  }
  
  # Tack on the question text to the end
  compare <- merge(compare, kps.vars[c("varname", "question.text")], by.x = "question", by.y = "varname")
  
  # Write file out to the output folder with the specified prefix
  write.csv(compare, file = paste("output/", prefix, "-KPSANALYSIS-profile.csv", sep = ""))
  
  return(compare)
}



# Subselect data and pass to kps.profile()
q.sub <- q[,grepl("MCLASS|mystical\\d+|spiritual\\d+|psyphys\\d+|psychic\\d+|talents\\d+|invmov\\d+|sensation\\d+|negphysical\\d+|otherphysical\\d+|negpsych\\d+|psybliss\\d+|psygrowth\\d+", names(q))]
kps.profile(data = q.sub, target = "MCLASS", prefix = "all")


# Likert plot (sample)
plot(likert(items = as.data.frame(q.sub[,grepl(likert.names, names(q.sub))]), grouping = q.sub$MCLASS))


#
# TODO: OPEN TEXT CONFIRMATION
#        - Confirm experience classifications using open text response fields.
#

# TODO: Score a file based on open text responses
# TODO: Validate findings to date against confirmation results
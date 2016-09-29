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
q <- kps.data[,grepl("mystical", names(kps.data))]
q.questiontext <- kps.get.questiontext(q)
plot(likert(q.questiontext), centered = FALSE)

# Spiritual likert visualization
q <- kps.data[,grepl("spiritual", names(kps.data))]
q.questiontext <- kps.get.questiontext(q)
plot(likert(q.questiontext), centered = FALSE)

# PsychoPhysical likert visualization
q <- kps.data[,grepl("psyphys", names(kps.data))]
q.questiontext <- kps.get.questiontext(q)
plot(likert(q.questiontext), centered = FALSE)

# Psychic likert visualization
q <- kps.data[,grepl("psychic", names(kps.data))]
q.questiontext <- kps.get.questiontext(q)
plot(likert(q.questiontext), centered = FALSE)

# Talents likert visualization
q <- kps.data[,grepl("talents", names(kps.data))]
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

kps.fa <- function(data, grepmatch = NULL, prefix = "default", nfactors = 1, parallel = FALSE) {
 
  print(paste("Matched", sum(grepl(grepmatch, names(data))), "columns"))
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
  
  q.num <- kps.get.questiontext(q.num) # For full question text output
  
  q.poly.fa.pro <- fa.poly(x = q.num, nfactors = nfactors, fm = "pa", rotate = "promax")
  fa.diagram(q.poly.fa.pro)
  clust <- iclust(q.poly.fa.pro$rho)
  
  
  # Export to CSV to analyze/filter in a spreadsheet app
  write.csv(loadings, file = paste("output/", prefix, "-loadings.csv", sep = ''))
  write.csv(q.poly.fa.pro$rho, file = paste("output/", prefix, "-poly-correlations.csv", sep = ''))
  return(q.poly.fa.pro)
}


# These can take (very) long depending on the number of variables
kps.fa(kps.data, grepmatch = "mystical", prefix = "mystical", nfactors = 3) # 9/13 - n = 338 - Parallel analysis factors = 3
kps.fa(kps.data, grepmatch = "spiritual", prefix = "spiritual", nfactors = 5 ) # 9/13 - n = 338 - Parallel analysis factors = 5
kps.fa(kps.data, grepmatch = "mystical|spiritual", prefix = "mystical-spiritual", nfactors = 6) # 9/13 - n = 338 - Parallel analysis factors = 6

# TODO: Implement IRT FA information
# q.irt.fa <- irt.fa(x = q.num, nfactors = 3, fm = "pa", rotate = "promax")

#
# TODO: COMPOSITE SCORE CREATION
#        - Crate composite variables based on factors
#        - Simple average of individual factor variables
#

# kps.compvar() - Create a KPS composite variable
# 
# Parameters
#
# data: Original dataset to process
# names: Vector of names to use to create the composite variable.
#        These will be automatically removed from the original dataset
# newname: The name of the new composite variable
kps.compvar <- function(data, newname, names) {
  
  data[newname] <- rowMeans(data[,c(names)]) # Calc row means and create comp var
  data <- data[,-which(names(data) %in% names)] # Remove original vars
  return(data)
  
  # Move new name to front (optional)
  # col_idx <- grep(newname, names(data))
  # compvar <- compvar[, c(col_idx, (1:ncol(data))[-col_idx])]
}

## Create Composite Variables ##

# Make a copy of the original dataset
# compvar <- kps.data
compvar <- kps.data[,grepl("mystical", names(kps.data))]

# Get all likert question names
likert.names <- grepl('mystical|spiritual|psyphys|psychic|talents|invmov|sensation|negphysical|otherphysical|negpsych|psybliss|psygrowth',
                      names(compvar))

# Convert all likert questions to numbers so we can calculate the row means
compvar[,likert.names] <- as.data.frame(lapply(compvar[,likert.names], as.numeric)) # Convert all values to numeric


# Create all composite variables

compvar <- kps.compvar(compvar, "CONSCIOUSNESS", c("mystical24","mystical25","mystical27"))


# Drop a composite variable  
# q.num <- q.num[ , !(names(q.num) %in% c("CONSCIOUSNESS"))]



#
#TODO: COMPOSITE SCORE CORRELATIONS ANALYSIS
#        - Create correlations analysis based composite variables
#        - Polychoric and Pearsons


#
#TODO: CLUSTER ANALYSIS
#        - Conduct cluster analysis based on primary composite variables
#        - Goal is to identify unique types of spiritual experiences


## Conduct Latent Class Analysis to determine different response patterns ##

# Source: http://statistics.ohlsen-web.de/latent-class-analysis-polca/

library("poLCA")
library("reshape2")
library("ggplot2")

q <- kps.data[,grepl("mystical|spiritual|psyphys|psygrowth|psygrowth.gate", names(kps.data))]
paste(names(q), collapse = ", ")


# Define the model function
#
# The following model uses important higher consciousness variables as covariates

# Higher Consciousness (predictors) - mystical24, mystical22
# Peace and love - mystical4, mystical5
# Spiritual rebirth - spiritual2, spiritual1
# Instruction - spiritual20, spiritual17
# OOB - spiritual14, spiritual15
# Energy - psyphys5, psyphys3
# Light - psyphys11, psyphys12
f<-with(q, cbind(mystical4, mystical5, spiritual2, spiritual1, spiritual20, spiritual17, spiritual14, spiritual15, psyphys5, psyphys3, psyphys11, psyphys12)~mystical24, mystical22)



# Source: http://stanfordphd.com/BIC.html
# BIC attempts to mitigate the risk of over-fitting by introducing the
# penalty term d * log(N), which grows with the number of parameters.
# This allows to filter out unnecessarily complicated models, which have
# too many parameters to be estimated accurately on a given data set.
# BIC has preference for simpler models.

min_bic <- 100000 # some ridiculous max to start
for(i in 2:3){
  lc <- poLCA(f, q, nclass=i, maxiter=3000, 
              tol=1e-5, na.rm=FALSE,  
              nrep=10, verbose=TRUE, calc.se=TRUE)
  if(lc$bic < min_bic){
    min_bic <- lc$bic
    LCA_best_model<-lc
  }
}    	
LCA_best_model # 9/27 For all mystical questions, BIC(4): 25771.84 (lowest)
LCA_best_model <- poLCA(f, q, nclass=3, maxiter=3000, tol=1e-5, na.rm=FALSE, nrep=5, verbose=TRUE, calc.se=TRUE)
# poLCA.reorder()

## LCA Plots ##

# Default 3D plot
plot(LCA_best_model)

# Graph displaying classes by question

lcModelProbs <- melt(LCA_best_model$probs)
zp1 <- ggplot(lcModelProbs,aes(x = L1, y = value, fill = Var2))
zp1 <- zp1 + geom_bar(stat = "identity", position = "stack")
zp1 <- zp1 + facet_grid(Var1 ~ .) 
zp1 <- zp1 + scale_fill_brewer(type="seq", palette="Greys") +theme_bw()
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
zp2 <- zp2 + scale_fill_brewer(type="seq", palette="Greys") +theme_bw()
zp2 <- zp2 + theme_bw()
print(zp2)



#
# TODO: PROFILE ANALYSIS BASED ON MCLASS 
#        - 
#

library(likert)

q$MCLASS <- factor(LCA_best_model$predclass, levels = c("1", "3", "2"), labels = c("1", "2", "3"))

q.sub <- subset(q, psygrowth.gate == "Y")
q.sub <- q.sub[,grepl("gate|psygrowth|MCLASS", names(q.sub))]
q.sub <- kps.get.questiontext(q.sub)

plot(likert(q.sub[,2:10], grouping = q.sub$MCLASS), centered = FALSE)
plot(likert(q.sub[,11:20], grouping = q.sub$MCLASS), centered = FALSE)
plot(likert(q.sub[,21:30], grouping = q.sub$MCLASS), centered = FALSE)
plot(likert(q.sub[,31:40], grouping = q.sub$MCLASS), centered = FALSE)
plot(likert(q.sub[,41:48], grouping = q.sub$MCLASS), centered = FALSE)



## TODO: Run decision trees ##

# Source: http://www.edureka.co/blog/implementation-of-decision-tree/

library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

paste0(names(q.sub), collapse = " + ")
tree <- rpart(MCLASS ~ psygrowth1 + psygrowth2 + psygrowth3 + psygrowth4 + psygrowth5 + psygrowth6 + psygrowth7 + psygrowth8 + psygrowth9 + psygrowth10 + psygrowth11 + psygrowth12 + psygrowth13 + psygrowth14 + psygrowth15 + psygrowth16 + psygrowth17 + psygrowth18 + psygrowth19 + psygrowth20 + psygrowth21 + psygrowth22 + psygrowth23 + psygrowth24 + psygrowth25 + psygrowth26 + psygrowth27 + psygrowth28 + psygrowth29 + psygrowth30 + psygrowth31 + psygrowth32 + psygrowth33 + psygrowth34 + psygrowth35 + psygrowth36 + psygrowth37 + psygrowth38 + psygrowth39 + psygrowth40 + psygrowth41 + psygrowth42 + psygrowth43 + psygrowth44 + psygrowth45 + psygrowth46 + psygrowth47
              , data = q.sub
              , method = "class")
plot(tree)
text(tree, use.n = TRUE)
fancyRpartPlot(tree)


#
# TODO: OPEN TEXT CROSS-VALIDATION
#        - Cross-validate experience classifications using open text response fields.
#
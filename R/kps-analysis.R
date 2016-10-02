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

# File level setting
#
# Denotes level typand access based on security levels:
# - Level 1 - Likert questions only
# - Level 2 - Likert questions & open text
fileLevel <- 2

#
# Generate mclass file with mystical, spiritual, and psychophysiological open text fields
#
if(fileLevel == 2) {
  q.sub <- q[,grepl("MCLASS|text$", names(q))]
  write.csv(x = q.sub, file = "output/mclass-mystical-spiritual-text.csv")
}


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

kps.fa <- function(data, grepmatch = NULL, prefix = "default", nfactors = 1, parallel = FALSE) {
 
  print(paste("Matched", sum(grepl(grepmatch, names(data))), "columns"))
  print(paste("Names: "
    , paste(
      names(data[,grepl(grepmatch, names(data))])
      , collapse = ", "
    )
  ))
  
  if(sum(grepl(grepmatch, names(data))) < 1) return()
  
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
fa.results <- kps.fa(kps.data, grepmatch = "mystical\\d+", prefix = "mystical", nfactors = 3) # 9/13 - n = 338 - Parallel analysis factors = 3
fa.results <- kps.fa(kps.data, grepmatch = "spiritual\\d+", prefix = "spiritual", nfactors = 5 ) # 9/13 - n = 338 - Parallel analysis factors = 5
fa.results <- kps.fa(kps.data, grepmatch = "psyphys\\d+", prefix = "spiritual", nfactors = 2 ) # 9/13 - n = 338 - Parallel analysis factors = 5
fa.results <- kps.fa(kps.data, grepmatch = "mystical|spiritual\\d+", prefix = "mystical-spiritual", nfactors = 6) # 9/13 - n = 338 - Parallel analysis factors = 6
# TODO: mystical|spiritual|psyphys

# Example polychoric correlations matrix to faciliate data analysis

library(psych)
raw.data <- kps.data[,grepl("mystical12|mystical24|mystical25|mystical27|mystical14", names(kps.data))]
raw.data <- lapply(raw.data, as.ordered)
raw.data <- lapply(raw.data, as.numeric)
poly.results <- polychoric(as.data.frame(raw.data))
cor.plot(poly.results$rho, numbers = TRUE)


# TODO: Implement IRT FA information
# q.irt.fa <- irt.fa(x = q.num, nfactors = 3, fm = "pa", rotate = "promax")



#
# CLUSTER ANALYSIS
#        - Conduct cluster analysis based on primary composite variables
#        - Goal is to identify unique types of spiritual experiences


## Conduct Latent Class Analysis to analyze response patterns ##

# Source: http://statistics.ohlsen-web.de/latent-class-analysis-polca/

library("poLCA")
library("reshape2")
library("ggplot2")

q <- kps.data[,grepl("mystical|spiritual|psyphys|psygrowth|psygrowth.gate|mystical.text|spiritual.text|psyphys.text", names(kps.data))]

likert.names <- grepl('mystical\\d+|spiritual\\d+|psyphys\\d+|psychic\\d+|talents\\d+|invmov\\d+|sensation\\d+|negphysical\\d+|otherphysical\\d+|negpsych\\d+|psybliss\\d+|psygrowth\\d+',
                           names(q))
q[,likert.names] <- lapply(q[,likert.names], function(x){
  return(ordered(x))
})

# Define the model function
#
# The following model uses important higher consciousness variables as covariates:
# Higher Consciousness (predictors) - mystical24, mystical22
# Peace and love - mystical4, mystical5
# Spiritual rebirth - spiritual2, spiritual1
# Instruction - spiritual20, spiritual17
# OOB - spiritual14, spiritual15
# Energy - psyphys5, psyphys3
# Light - psyphys11, psyphys12

paste(names(q), collapse = ", ")

# No covariates
# f<-with(q, cbind(mystical24, mystical22,mystical4, mystical5, spiritual2, spiritual1, spiritual20, spiritual17, spiritual14, spiritual15, psyphys5, psyphys3, psyphys11, psyphys12)~1)

# With covariates
f<-with(q, cbind(
    mystical4, mystical5,
    spiritual2, spiritual1, spiritual20, spiritual17, spiritual14, spiritual15,
    psyphys5, psyphys3, psyphys11, psyphys12
  ) ~mystical24, mystical22, mystical26, mystical13
)

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
LCA_best_model
# LCA_best_model <- poLCA(f, q, nclass=2, maxiter=3000, tol=1e-5, na.rm=FALSE, nrep=5, verbose=TRUE, calc.se=TRUE)
# TODO: Reorder classes based on some consistent criteria using poLCA.reorder()


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
# DECISION TREES (WITH MCLASS AVAILABLE)
#

# Source: http://www.edureka.co/blog/implementation-of-decision-tree/

library(rpart)
library(rpart.plot) # See: http://www.milbo.org/rpart-plot/prp.pdf
library(plyr)

plot(LCA_best_model) # IMPORANT: Verify mclass order here

q <- kps.data # Initial load

# IMPORTANT! Perform any necessary remapping (need to set this up manually for now)
q$MCLASS <- as.ordered(mapvalues(LCA_best_model$predclass, 
  from=c('1', '2'), 
  to=c('2', '1'))
)

# Select columns
q <- q[,grepl("gate|psygrowth|psybliss|MCLASS", names(q))]

# Convert likert text to numbers for readability
likert.names <- grepl('mystical\\d+|spiritual\\d+|psyphys\\d+|psychic\\d+|talents\\d+|invmov\\d+|sensation\\d+|negphysical\\d+|otherphysical\\d+|negpsych\\d+|psybliss\\d+|psygrowth\\d+',
                      names(q.sub))
q[,likert.names] <- lapply(q[,likert.names], function(x) {
  x <- mapvalues(x, 
            from=c('Not at all', 'Very Weak/low intensity', 'Weak', 'Moderate', 'Strong', 'Very strong/high intensity'), 
            to=c("1", "2","3", "4", "5", "6"))
  x <- ordered(x)
   return(x)
})

# In case you need to cut and paste
# paste0(names(q.sub), collapse = " + ")

# mclass decision trees

qsub <- subset(q.sub, psybliss.gate == "Y")
tree <- rpart(
  MCLASS ~ .
  , data = q.sub[,grepl('psybliss\\d+|MCLASS', names(q.sub))]
)
rpart.plot(tree, tweak = 1.1)

q.sub <- subset(q.sub, psygrowth.gate == "Y")
tree <- rpart(
  MCLASS ~ .
  , data = q.sub[,grepl('psygrowth\\d+|MCLASS', names(q.sub))]
)
rpart.plot(tree, tweak = 1.1)


# Negative physical symtoms decision trees

q.sub <- subset(q.sub, psygrowth.gate == "Y")
tree <- rpart(
  pe.negphysical.gate ~ .
  , data = q.sub[,grepl('pe.negphysical.gate|psygrowth\\d+|MCLASS', names(q.sub))]
)
rpart.plot(tree, tweak = 1.25)

qsub <- subset(q.sub, psybliss.gate == "Y")
tree <- rpart(
  pe.negphysical.gate ~ .
  , data = q.sub[,grepl('pe.negphysical.gate|psybliss\\d+|MCLASS', names(q.sub))]
)
rpart.plot(tree, tweak = 1.1)


# Negative psychological decision tree

qsub <- subset(q.sub, psybliss.gate == "Y")
tree <- rpart(
  negpsych.gate ~ .
  , data = q.sub[,grepl('negpsych.gate|psybliss\\d+|MCLASS', names(q.sub))]
)
rpart.plot(tree, tweak = 1.1)

qsub <- subset(q.sub, psygrowth.gate == "Y")
tree <- rpart(
  negpsych.gate ~ .
  , data = q.sub[,grepl('negpsych.gate|psygrowth\\d+|MCLASS', names(q.sub))]
)
rpart.plot(tree, tweak = 1.1)



#
# TODO: OPEN TEXT CONFIRMATION
#        - Confirm experience classifications using open text response fields.
#

# TODO: Score a file based on open text responses
# TODO: Validate findings to date against confirmation results
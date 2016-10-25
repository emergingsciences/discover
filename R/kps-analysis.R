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
  
  
  # Export to CSV to analyze/filter in a spreadsheet app
  write.csv(loadings, file = paste("output/", prefix, "-loadings.csv", sep = ''))
  write.csv(q.poly.fa.pro$rho, file = paste("output/", prefix, "-poly-correlations.csv", sep = ''))
  return(q.poly.fa.pro)
}

kps.data.withfascores <- kps.data

# These can take (very) long depending on the number of variables
fa.results <- kps.fa(scores = "regression", kps.data, grepmatch = "mystical\\d+", prefix = "mystical", nfactors = 3) # 9/13 - n = 338 - Parallel analysis factors = 3
# scores <- fa.results$scores$scores
# colnames(scores)[colnames(scores)=="PA1"] <- "CONSCIOUSNESS"
# colnames(scores)[colnames(scores)=="PA2"] <- "GRACE"
# colnames(scores)[colnames(scores)=="PA3"] <- "BLISS"
# kps.data.withfascores <- cbind(kps.data.withfascores, scores)

fa.results <- kps.fa(scores = "regression", kps.data, grepmatch = "spiritual\\d+", prefix = "spiritual", nfactors = 5) # 10/24 - n = 362 - Parallel analysis factors = 5
# scores <- fa.results$scores$scores
# colnames(scores)[colnames(scores)=="PA1"] <- "SYNCHRONICITY"
# colnames(scores)[colnames(scores)=="PA2"] <- "OOB"
# colnames(scores)[colnames(scores)=="PA3"] <- "REBIRTH"
# colnames(scores)[colnames(scores)=="PA4"] <- "AURAL"
# colnames(scores)[colnames(scores)=="PA5"] <- "INTUITION"
# kps.data.withfascores <- cbind(kps.data.withfascores, scores)

fa.results <- kps.fa(scores = "regression", kps.data, grepmatch = "psyphys\\d+", prefix = "psyphys", nfactors = 4 ) # 10/24 - n = 362 - Parallel analysis factors = 4
# scores <- fa.results$scores$scores
# colnames(scores)[colnames(scores)=="PA1"] <- "ENERGY"
# colnames(scores)[colnames(scores)=="PA2"] <- "LIGHT"
# kps.data.withfascores <- cbind(kps.data.withfascores, scores)


# # Factor correlations
# kps.data.withfascores <- kps.data.withfascores[,c("CONSCIOUSNESS", "GRACE", "BLISS", "SYNCHRONICITY", "OOB", "REBIRTH", "AURAL", "INTUITION", "ENERGY", "LIGHT")]
# cor.mat <- cor(method = "pearson", x = kps.data.withfascores)
# cor.mat.melt <- melt(cor.mat)
# cor.plot(cor.mat, numbers = TRUE)
# plot(kps.data.withfascores$ENERGY, kps.data.withfascores$LIGHT)



# Psygrowth factor analysis
kps.fa(data = subset(kps.data, kps.data$psygrowth.gate == "Y"),
       grepmatch = "psygrowth\\d+",
       prefix = "psygrowth",
       nfactors = 6) # 10/18 - n = 338 - Parallel analysis factors = 6


# Psygrowth factor analysis with Higher Consciousness indicators
# mystical22 - CONSCIOUSNESS - Expansion of consciousness
# mystical26 - CONSCIOUSNESS - Personal identification with all of creation
# mystical12 - CONSCIOUSNESS - Experience of deep unity and expansive con
# mystical23 - CONSCIOUSNESS - Union with Life Energy
# mystical4 - BLISS - Intense feeling of peace
# mystical5 - BLISS - Overwhelming sense of love
# mystical7 - BLISS - Overwhelming sense of wonder and awe
# mystical9 - BLISS - Overwhelming sense of bliss, joy and or contentment
kps.fa(data = subset(kps.data, kps.data$psygrowth.gate == "Y"),
       grepmatch = "psygrowth\\d+|mystical22$|mystical26$|mystical12$|mystical23$|mystical4$|mystical5$|mystical7$|mystical9$",
       prefix = "psygrowth",
       nfactors = 7) # 10/18 - n = 338 - Parallel analysis factors = 7





# Psybliss factor analysis
kps.fa(data = subset(kps.data, kps.data$psybliss.gate == "Y"),
       grepmatch = "psybliss\\d+",
       prefix = "psybliss",
       nfactors = 4) # 10/18 - n = 338 - Parallel analysis factors = 4


# Psybliss factor analysis with Higher Consciousness indicators
# mystical22 - CONSCIOUSNESS - Expansion of consciousness
# mystical26 - CONSCIOUSNESS - Personal identification with all of creation
# mystical12 - CONSCIOUSNESS - Experience of deep unity and expansive con
# mystical23 - CONSCIOUSNESS - Union with Life Energy
# mystical4 - BLISS - Intense feeling of peace
# mystical5 - BLISS - Overwhelming sense of love
# mystical7 - BLISS - Overwhelming sense of wonder and awe
# mystical9 - BLISS - Overwhelming sense of bliss, joy and or contentment
kps.fa(data = subset(kps.data, kps.data$psybliss.gate == "Y"),
       grepmatch = "psybliss\\d+|mystical22$|mystical26$|mystical12$|mystical23$|mystical4$|mystical5$|mystical7$|mystical9$",
       prefix = "psybliss",
       nfactors = 5) # 10/18 - n = 338 - Parallel analysis factors = 5



# fa.results <- kps.fa(kps.data, grepmatch = "mystical\\d+|spiritual\\d+|psyphys\\d+", prefix = "mystical-spiritual-psyphys", nfactors = 6) # 9/13 - n = 338 - Parallel analysis factors = 6
# fa.results$fa$score.cor

# q <- lapply(kps.data[,grepl("mystical\\d+|spiritual\\d+|psyphys\\d+", names(kps.data))], as.numeric)
# cor.mat <- cor(x = as.data.frame(q))
# summary(fa.results$scores)

# Example polychoric correlations matrix to faciliate data analysis

library(psych)
raw.data <- kps.data[,grepl("mystical5|mystical7|mystical9", names(kps.data))]
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
f<-with(q, cbind(mystical22, # CONSCIOUSNESS - Expansion of consciousness
                 mystical26, # CONSCIOUSNESS - Personal identification with all of creation
                 mystical12, # CONSCIOUSNESS - Experience of deep unity and expansive consciousness
                 mystical23, # CONSCIOUSNESS - Union with Life Energy
                 mystical4, # BLISS - Intense feeling of peace
                 mystical5, # BLISS - Overwhelming sense of love
                 mystical7, # BLISS - Overwhelming sense of wonder and awe
                 mystical9  # BLISS - Overwhelming sense of bliss, joy and or contentment
                 )~1)

# Primary Experience Indicator Model Option (with covariates)
f<-with(q, cbind(
  # Indicators
  mystical4, # Intense feeling of peace
  mystical5, # Overwhelming sense of love
  spiritual2, # New understanding of spiritual truths/Insight into the inner meaning of spiritual teachings
  spiritual1, # Spiritual rebirth - spontaneous religious conversion or dramatic spiritual awakening, including a major reorientation of spiritual beliefs.
  # spiritual20, # Feeling of connection with a spiritual guide or lineage
  spiritual17, # Receiving inner instruction
  # spiritual14, # Out of body experiences
  # spiritual15, # Astral/time travel experiences
  psyphys5, # Feelings of energy flowing or vibrating within
  psyphys3, # Sensations of energy rushing up the spine
  psyphys11, # Visions of light
  psyphys12 # Floating in the light
) ~ # Covariates
    # mystical24, # CONSCIOUSNESS - Experience of Higher consciousness/cosmic consciousness
  mystical22, # CONSCIOUSNESS - Expansion of consciousness  
  mystical12, # CONSCIOUSNESS - Experience of deep unity and expansive consciousness
  mystical26, # CONSCIOUSNESS - Personal identification with all of creation
  mystical13  # CONSCIOUSNESS - All sense of separateness disappears
)

# Create optimize model based on BIC criteria.
# For more info: http://stanfordphd.com/BIC.html
min_bic <- 100000 # some ridiculous max to start
for(i in 2:5){
  lc <- poLCA(f, q, nclass=i, maxiter=3000, 
              tol=1e-5, na.rm=FALSE,  
              nrep=10, verbose=TRUE, calc.se=TRUE)
  if(lc$bic < min_bic){
    min_bic <- lc$bic
    LCA_best_model<-lc
  }
}
LCA_best_model
LCA_best_model <- poLCA(f, q, nclass=2, maxiter=3000, tol=1e-5, na.rm=FALSE, nrep=10, verbose=TRUE, calc.se=TRUE)

plot(LCA_best_model) # Run this to confirm poLCA class order

# Re-order based on ascending population share (mclass = 2 is usually larger)
# CONFIRM THIS VISUALLY!
LCA_best_model <- poLCA(f,q,nclass=2 # Ensure nclass is set properly!!!
  ,probs.start= poLCA.reorder(
    LCA_best_model$probs
    , c(2, 1)
  )
)

q$MCLASS <- ordered(LCA_best_model$predclass)

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

# Select columns
# q <- q[,grepl("gate|psygrowth|psybliss|MCLASS", names(q))]

# Convert likert text to numbers for readability
likert.names <- grepl('mystical\\d+|spiritual\\d+|psyphys\\d+|psychic\\d+|talents\\d+|invmov\\d+|sensation\\d+|negphysical\\d+|otherphysical\\d+|negpsych\\d+|psybliss\\d+|psygrowth\\d+',
                      names(q))
q[,likert.names] <- lapply(q[,likert.names], function(x) {
  x <- mapvalues(x, 
            from=c('Not at all', 'Very Weak/low intensity', 'Weak', 'Moderate', 'Strong', 'Very strong/high intensity'), 
            to=c("1", "2","3", "4", "5", "6"))
  x <- ordered(x)
   return(x)
})
q$MCLASS <- ordered(LCA_best_model$predclass)

# End creating "q"


# In case you need to cut and paste
# paste0(names(q.sub), collapse = " + ")


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

# Returns a data frame containing all descriptive and statistical output
kps.profile <- function(data = NULL, grepstr = "", prefix = "") {
  
  # Populate "compare" data frame one row at a time
  
  compare <- data.frame( # Destination for all our statistics
                        question = character(),
                        wilcox.u = double(), # Mann-Whitney U statistic
                        wilcox.p = double(), # P-value
                        wilcox.r = double(), # Effect size
                        median.m1 = double(), # Median of mclass 1
                        median.m2 = double(), # Median of mclass 2
                        median.diff = double(), # m2 minus m1
                        t.test.t = double(), # T-test t statistic
                        t.test.p = double(), # T-test p-value
                    stringsAsFactors = FALSE)
  
  varnames <- names( data[,!grepl("MCLASS", names(data))]) # All but the MCLASS
  
  for(i in 1:length(varnames)) {
    compare[i,"question"] <- varnames[i]
    
    # Conduct a Mann-Whitney U Test
    # This generates the u (sometimes called w), p, and r statistics. For more information
    # see the resources below:
    #
    # http://www.stata-journal.com/sjpdf.html?articlenum=st0253
    # https://statistics.laerd.com/premium-sample/mwut/mann-whitney-test-in-spss-2.php
    # http://yatani.jp/teaching/doku.php?id=hcistats:mannwhitney
    
    wc <- wilcox.test(as.formula(paste("as.numeric(",varnames[i],") ~ MCLASS")), data = data)
    compare[i,"wilcox.u"] <- wc$statistic
    compare[i,"wilcox.p"] <- wc$p.value
    
    
    wc <- wilcox_test(as.formula(paste("as.numeric(",varnames[i],") ~ MCLASS")), data = data)
    compare[i,"wilcox.r"] <- statistic(wc) / sqrt(nrow(data))
    
    compare[i,"median.m1"] <- median(as.numeric(data[data$MCLASS == "1",varnames[i]]))
    compare[i,"median.m2"] <- median(as.numeric(data[data$MCLASS == "2",varnames[i]]))
    compare[i,"median.diff"] <- compare[i,"median.m2"] - compare[i,"median.m1"]
    
    # Conduct a standard t test
    
    t <- t.test(as.numeric(subset(data[,varnames[i]], data$MCLASS == 1)), as.numeric(subset(data[,varnames[i]], data$MCLASS == 2)))
    
    compare[i,"t.test.t"] <- t$statistic
    compare[i,"t.test.p"] <- t$p.value
  }
  
  # Tack on the question text to the end
  compare <- merge(compare, kps.vars[c("varname", "question.text")], by.x = "question", by.y = "varname")
  
  # Write file out to the output folder with the specified prefix
  write.csv(compare, file = paste("output/", prefix, "-m2class-profile.csv", sep = ""))
  
  return(compare)
}


# likert.names <- grepl('mystical\\d+|spiritual\\d+|psyphys\\d+|psychic\\d+|talents\\d+|invmov\\d+|sensation\\d+|negphysical\\d+|otherphysical\\d+|negpsych\\d+|psybliss\\d+|psygrowth\\d+',
#                       names(data))
# data[,likert.names] <- lapply(data[,likert.names], function(x) {
#   x <- mapvalues(x, 
#                  from=c('Not at all', 'Very Weak/low intensity', 'Weak', 'Moderate', 'Strong', 'Very strong/high intensity'), 
#                  to=c("1", "2","3", "4", "5", "6"))
#   x <- ordered(x)
#   return(x)
# })


q$MCLASS <- ordered(LCA_best_model$predclass)

# Subselect data and pass to kps.profile()
q.sub <- subset(q, psybliss.gate == "Y")
q.sub <- q.sub[,grepl("psybliss\\d+|MCLASS", names(q.sub))]
compare <- kps.profile(data = q.sub, grepstr = "psybliss\\d+", prefix = "psybliss")

q.sub <- subset(q, psygrowth.gate == "Y")
q.sub <- q.sub[,grepl("psygrowth\\d+|MCLASS", names(q.sub))]
compare <- kps.profile(data = q.sub, grepstr = "psygrowth\\d+", prefix = "psygrowth")

q.sub <- q
q.sub <- q.sub[,grepl("talents\\d+|MCLASS", names(q.sub))]
compare <- kps.profile(data = q.sub, grepstr = "talents\\d+", prefix = "talents")


q.sub <- subset(q, pe.negphysical.gate == "Y")
q.sub <- q.sub[,grepl("negphysical\\d+|MCLASS", names(q.sub))]
compare <- kps.profile(data = q.sub, grepstr = "negphysical\\d+", prefix = "negphysical")

q.sub <- subset(q, pe.negphysical.gate == "Y")
q.sub <- q.sub[,grepl("sensation\\d+|MCLASS", names(q.sub))]
compare <- kps.profile(data = q.sub, grepstr = "sensation\\d+", prefix = "sensation")


# Likert plot (just in case)
# plot(likert(items = as.data.frame(q.sub[,grepl("psybliss6$",names(q.sub))]), grouping = q.sub$MCLASS))



#
# TODO: OPEN TEXT CONFIRMATION
#        - Confirm experience classifications using open text response fields.
#

# TODO: Score a file based on open text responses
# TODO: Validate findings to date against confirmation results
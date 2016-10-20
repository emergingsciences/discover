# °º¤ø,¸¸,ø¤º°`°º¤ø,¸,ø¤°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤°º¤ø,¸¸,ø¤º°`°º¤:>
#
# Kundalini Findings, Insights, and Recommendations Engine (FIRE)
#
# Accepts an ID and generates a profile readout
#
# °º¤ø,¸¸,ø¤º°`°º¤ø,¸,ø¤°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤°º¤ø,¸¸,ø¤º°`°º¤:>


source("R/kps-utility.R")


library("poLCA") # LCA library
library("plyr") # Mapping
library("rpart") # Decision trees
library("rpart.plot") # Decision tree plotting See: http://www.milbo.org/rpart-plot/prp.pdf


id = "INSERT_RECORD_ID_HERE" # Insert ID here to specifify record for analysis

#
# Full data load (without variable names)
#
kps.data <- kps.loaddatafile()

#
# Required transforms and utility functions (e.g., fire.cluster())
#

likert.names <- grepl('mystical\\d+|spiritual\\d+|psyphys\\d+|psychic\\d+|talents\\d+|invmov\\d+|sensation\\d+|negphysical\\d+|otherphysical\\d+|negpsych\\d+|psybliss\\d+|psygrowth\\d+',
                      names(kps.data))
kps.data[,likert.names] <- lapply(kps.data[,likert.names], function(x) {
  x <- mapvalues(x, 
                 from=c('Not at all', 'Very Weak/low intensity', 'Weak', 'Moderate', 'Strong', 'Very strong/high intensity'), 
                 to=c("1", "2","3", "4", "5", "6"))
  x <- ordered(x)
  return(x)
})

# x - The data to cluster
# f - The formula
fire.cluster <- function(x, f, newVarName, c = NULL) {
  
  if(is.null(c)) {
    min_bic <- 100000 # some ridiculous starting max
    for(i in 2:6){
      lc <- poLCA(f, x, nclass=i, maxiter=3000, 
                  tol=1e-5, na.rm=FALSE,  
                  nrep=10, verbose=FALSE, calc.se=TRUE)
      if(lc$bic < min_bic && !lc$eflag){
        min_bic <- lc$bic
        LCA_best_model<-lc
      }
    }
  } else {
    LCA_best_model <- poLCA(f, x, nclass=c, maxiter=3000, 
                            tol=1e-5, na.rm=FALSE,  
                            nrep=10, verbose=TRUE, calc.se=TRUE)
  }
  
  #TODO: Reorder based on probability that the higher scores are a higher class number
  
  # TODO: Turn these off in production
  # LCA_best_model
  plot(LCA_best_model)
  print(paste(num.classes,"classes generated for",newVarName))
  
  x[newVarName] <- ordered(LCA_best_model$predclass)
  return(x)
}



#
# Generate M-Class and merge with data
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
kps.data <- fire.cluster(kps.data, f, "MCLASS")

# Extract subject row
sub <- kps.data[match(id, kps.data$id),]

print(paste("Subject mclass is",as.integer(sub$MCLASS)))
print(sub$mystical22)
print(sub$mystical26)
print(sub$mystical12)
print(sub$mystical23)
print(sub$mystical4)
print(sub$mystical5)
print(sub$mystical7)
print(sub$mystical9)



#
# Generate psychological profile
#

# Run an LCA on each of the psychological factors to determine high vs. low
# Provide readout based on ID

if(sub$psygrowth.gate == "Y") {
  print("Subject answered YES to psygrowth.gate")
  
  # Generate psygrowth clusters
  
  # Psygrowth Virtue (PG.ALTRUISM)
  f<-with(q, cbind(psygrowth30, # Deep compassion
                   psygrowth10, # An interest in helping others, compassion for the suffering
                   psygrowth47, # Increase in morals
                   psygrowth45  # A deep abiding love for humanity and family
  )~1)
  kps.data <- fire.cluster(kps.data, f, "PG.ALTRUISM")
  
  sub <- kps.data[match(id, kps.data$id),]
  print(sub$PG.ALTRUISM)
  print(sub$psygrowth30)
  print(sub$psygrowth10)
  print(sub$psygrowth47)
  print(sub$psygrowth45)
  print(paste("Subject is PG.VIRTUE class",as.integer(sub$PG.ALTRUISM)))
  
  table(kps.data$MCLASS, kps.data$PG.ALTRUISM)
  
  
  # Psygrowth Abilities (PG.ABIL)
  f<-with(q, cbind(psygrowth6, # Competence in some special area
                   psygrowth5, # Special capacities, intelligence, creativity
                   psygrowth14 # Non-ordinary abilities
  )~1)
  kps.data <- fire.cluster(kps.data, f, "PG.ABIL")
  
  sub <- kps.data[match(id, kps.data$id),]
  print(sub$PG.ABIL)
  print(sub$psygrowth6)
  print(sub$psygrowth5)
  print(sub$psygrowth14)
  print(paste("Subject is PG.ABIL class",as.integer(sub$PG.ABIL)))
  
  table(kps.data$MCLASS, kps.data$PG.ABIL)
  
  
  # Psygrowth Yearning (PG.YEARN)
  f<-with(q, cbind(
    psygrowth20,	# Increased study of spiritual topics
    psygrowth15,	# An interest in spiritual growth, religion, metaphysics, the non-ordinary, the beyond
    psygrowth19,	# Increased focus on spiritual issues and settings
    psygrowth31,	# Spiritual yearning
    psygrowth18,	# Increase spiritual practices (prayer, meditation)
    psygrowth17 	# A desire to pray, meditate, study scripture, read about holy people, go to holy places
  )~1)
  kps.data <- fire.cluster(kps.data, f, "PG.YEARN")
  
  sub <- kps.data[match(id, kps.data$id),]
  print(sub$PG.YEARN)
  print(sub$psygrowth20)
  print(sub$psygrowth15)
  print(sub$psygrowth19)
  print(sub$psygrowth31)
  print(sub$psygrowth18)
  print(sub$psygrowth17)
  print(paste("Subject is PG.YEARN class",as.integer(sub$PG.YEARN)))
  
  table(kps.data$MCLASS, kps.data$PG.YEARN)
  
  
} else {
  print("Subject answered NO to psygrowth.gate")
}


if(sub$psybliss.gate == "Y") {
  print("Subject answered YES to psybliss.gate")
  
  # Psybliss Virtue (PB.VIRTUE)
  f<-with(q, cbind(
    psybliss21,	# Increased beief in the unity of humanity
    psybliss18,	# Increased belief in the divine or One
    psybliss22,	# Increased tolerance and balance
    psybliss23,	# Increase ability to see the underlying point of things
    psybliss26,	# Highly developed moral sense
    psybliss20,	# Increased clarity of self and values
    psybliss27,	# Increased virtue
    psybliss19	# Increased maturation of the personality
  )~1)
  kps.data <- fire.cluster(kps.data, f, "PB.VIRTUE")
  
  sub <- kps.data[match(id, kps.data$id),]
  print(sub$PB.VIRTUE)
  print(sub$psybliss21)
  print(sub$psybliss18)
  print(sub$psybliss22)
  print(sub$psybliss23)
  print(sub$psybliss26)
  print(sub$psybliss20)
  print(sub$psybliss27)
  print(sub$psybliss19)
  print(paste("Subject is PB.VIRTUE class",as.integer(sub$PB.VIRTUE)))
  
  table(kps.data$MCLASS, kps.data$PB.VIRTUE)
} else {
  print("Subject answered NO to psybliss.gate")
}


#
# Post Hoc Analysis
#

# List of composite variables
# PG.ALTRUISM
# PG.ABIL
# PG.YEARN
# PB.VIRTUE
#
# tree <- rpart(
#   MCLASS ~ .
#   , data = kps.data[,grepl('MCLASS|PG.ALTRUISM$|PG.ABIL$|PG.YEARN$|PB.VIRTUE$', names(kps.data))]
# )
# rpart.plot(tree, tweak = 1.1)

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
sub <- kps.data[match(id, kps.data$id),] # Record check


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
    
    #TODO: Reorder based on probability that the higher scores are a higher class number
    # slist <- LCA_best_model$probs[["mystical22"]]
    # avgVec <- rbind(avgVec, slist[,ncol(slist)])
    
    intenseVals <- lapply(LCA_best_model$probs, function(x) {
      # return(x[,ncol(x)])
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
  }

  return(x)
}



#
# Generate M-Class and merge with data
#

# CONSCIOUSNESS BLISS Model Option (no covariates, only indicators)
f<-with(kps.data, cbind(
  mystical22, # CONSCIOUSNESS - Expansion of consciousness
  mystical26, # CONSCIOUSNESS - Personal identification with all of creation
  mystical12, # CONSCIOUSNESS - Experience of deep unity and expansive consciousness
  mystical23, # CONSCIOUSNESS - Union with Life Energy
  mystical4,  # BLISS - Intense feeling of peace
  mystical5,  # BLISS - Overwhelming sense of love
  mystical7,  # BLISS - Overwhelming sense of wonder and awe
  mystical9   # BLISS - Overwhelming sense of bliss, joy and or contentment
)~1)
kps.data <- fire.cluster(kps.data, f, "MCLASS")


# ENERGY (no covariates, only indicators)
f<-with(kps.data, cbind(
  psyphys3, # Sensations of energy rushing up the spine
  psyphys9, # Sensations of energy along the seven major chakras (chakras are spinning vortices of energy) that run from the base of the spine to the crown of the head
  psyphys5  # Feelings of energy flowing or vibrating within
)~1)
kps.data <- fire.cluster(kps.data, f, "ENERGY")


# REBIRTH (no covariates, only indicators)
f<-with(kps.data, cbind(
  spiritual1, # Spiritual rebirth - spontaneous religious conversion or dramatic spiritual awakening, including a major reorientation of spiritual beliefs.
  spiritual2, # New understanding of spiritual truths/Insight into the inner meaning of spiritual teachings
  spiritual3  # An unshakable conviction about the reality of the experience
)~1)
kps.data <- fire.cluster(kps.data, f, "REBIRTH")

# OOB
f<-with(kps.data, cbind(
  spiritual14,	# Out of body experiences
  spiritual15,	# Astral/time travel experiences
  spiritual13	  # Encounters with nonmaterial entities(the deceased, lower astral beings, aliens, spirit guides)
)~1)
kps.data <- fire.cluster(kps.data, f, "OOB")


# INTUITION
f<-with(kps.data, cbind(
  spiritual20, # Feeling of connection with a spiritual guide or lineage
  spiritual24, # Greater incidence of prayers being answered
  spiritual7   # Increased desire for spiritual experiences
)~1)
kps.data <- fire.cluster(kps.data, f, "INTUITION")

# GRACE
f<-with(kps.data, cbind(
  mystical19, # Tasting sacred nectar dripping from the roof of mouth or back of throat (amrita or soma)
  mystical1,  # Experiences or visions of deities, gurus, icons, saints or mystics or other religious prophets, religious icons or universal archetypes
  mystical17, # Receiving instructions from the divine
  mystical20  # Feeling sacred touch (wind, presence, touch)
)~1)
kps.data <- fire.cluster(kps.data, f, "GRACE")



#
# Generate psychological profile
#

# Run an LCA on each of the psychological factors to determine high vs. low
# Provide readout based on ID

kps.data.pg <- subset(kps.data, psygrowth.gate == "Y")
  
# Generate psygrowth clusters

# Psygrowth Virtue (PG.ALTRUISM)
f<-with(kps.data.pg, cbind(psygrowth30, # Deep compassion
                 psygrowth10, # An interest in helping others, compassion for the suffering
                 psygrowth47, # Increase in morals
                 psygrowth45  # A deep abiding love for humanity and family
)~1)

kps.data.pg <- fire.cluster(kps.data.pg, f, "PG.ALTRUISM")


# Psygrowth Abilities (PG.ABIL)
f<-with(kps.data.pg, cbind(psygrowth6, # Competence in some special area
                 psygrowth5, # Special capacities, intelligence, creativity
                 psygrowth14 # Non-ordinary abilities
)~1)
kps.data.pg <- fire.cluster(kps.data.pg, f, "PG.ABIL")

table(kps.data.pg$MCLASS, kps.data.pg$PG.ABIL)


# Psygrowth Yearning (PG.YEARN)
f<-with(kps.data.pg, cbind(
  psygrowth20,	# Increased study of spiritual topics
  psygrowth15,	# An interest in spiritual growth, religion, metaphysics, the non-ordinary, the beyond
  psygrowth19,	# Increased focus on spiritual issues and settings
  psygrowth31,	# Spiritual yearning
  psygrowth18,	# Increase spiritual practices (prayer, meditation)
  psygrowth17 	# A desire to pray, meditate, study scripture, read about holy people, go to holy places
)~1)
kps.data.pg <- fire.cluster(kps.data.pg, f, "PG.YEARN")

table(kps.data.pg$MCLASS, kps.data.pg$PG.YEARN)


# Merge back with original data
kps.data <- merge(kps.data, kps.data.pg[ , grepl("id|PG.", names(kps.data.pg))]
                  , by=c("id")
                  , all.x = TRUE)


# Psybliss

kps.data.pb <- subset(kps.data, psybliss.gate == "Y")

# Psybliss Virtue (PB.VIRTUE)
f<-with(kps.data.pb, cbind(
  psybliss21,	# Increased beief in the unity of humanity
  psybliss18,	# Increased belief in the divine or One
  psybliss22,	# Increased tolerance and balance
  psybliss23,	# Increase ability to see the underlying point of things
  psybliss26,	# Highly developed moral sense
  psybliss20,	# Increased clarity of self and values
  psybliss27,	# Increased virtue
  psybliss19	# Increased maturation of the personality
)~1)
kps.data.pb <- fire.cluster(kps.data.pb, f, "PB.VIRTUE")

# Merge back with original data
kps.data <- merge(kps.data,kps.data.pb[ , grepl("id|PB.", names(kps.data.pb))]
                  , by=c("id")
                  , all.x = TRUE)


#
# Subject Readout
#

# Extract subject row
sub <- kps.data[match(id, kps.data$id),]

print(paste("Subject mclass is",as.integer(sub$MCLASS)))
print(paste("Subject is ENERGY class",as.integer(sub$ENERGY)))
print(paste("Subject is REBIRTH class",as.integer(sub$REBIRTH)))
print(paste("Subject is OOB class",as.integer(sub$OOB)))
print(paste("Subject is INTUITION class",as.integer(sub$INTUITION)))
print(paste("Subject is GRACE class",as.integer(sub$GRACE)))

if(sub$psygrowth.gate == "Y") {
  print("Subject answered YES to psygrowth.gate")
  print(paste("Subject is PG.ALTRUISM class",as.integer(sub$PG.ALTRUISM)))  
  print(paste("Subject is PG.ABIL class",as.integer(sub$PG.ABIL)))
  print(paste("Subject is PG.YEARN class",as.integer(sub$PG.YEARN)))
} else {
  print("Subject answered NO to psygrowth.gate")
}

if(sub$psybliss.gate == "Y") {
  print("Subject answered YES to psybliss.gate")
  print(paste("Subject is PB.VIRTUE class",as.integer(sub$PB.VIRTUE)))
} else {
  print("Subject answered NO to psybliss.gate")
}

#
# Decision Tree Analysis of FIRE Model
#

tree <- rpart(
  MCLASS ~ .
  , data = kps.data[,grepl('MCLASS|PG.|PB.|ENERGY|REBIRTH|OOB|INTUITION|GRACE', names(kps.data))]
  , minsplit = 15, cp = .01, method = "class"
)

rpart.plot(tree, tweak = 1, fallen.leaves = FALSE)


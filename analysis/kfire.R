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
library("caret")
library("polycor")

id = "INSERT_RECORD_ID_HERE" # Insert ID here to specifify record for analysis


#
# Full data load (without variable names)
#
kps.data <- kps.loaddatafile()
kps.vars <- kps.loadvarfile()

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
    for(i in 1:6){
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
  
  if(exists("LCA_best_model") && length(unique(LCA_best_model$predclass)) > 1) {
    print(paste("bic is",LCA_best_model$bic))
    
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
  }
  
  if(exists("LCA_best_model") && length(unique(LCA_best_model$predclass)) == 1) {
    print(paste("One class model generated for", newVarName))
  }

  return(x)
}



#
# MYSTICAL
#

# HC
f<-with(kps.data, cbind(
  mystical2, #	Expansion / explosion of consciousness
  mystical22, #	Expansion of consciousness
  mystical3, #	Loss of personal limitations / boundaries
  mystical15, #	Expanded comprehension of reality
  mystical13, #	All sense of separateness disappears
  mystical24, #	Experience of Higher consciousness/cosmic consciousness
  mystical6, #	New knowledge / awareness of the unbounded intelligence behind universe
  mystical12, #	Experience of deep unity and expansive consciousness
  mystical23, #	Union with Life Energy
  mystical27, #	Revelation: Knowledge that comes from a divine source where the individual becomes aware of the source of that knowledge
  mystical8, #	Loss of fear of death / certainty of immortality
  mystical10, #	Increased feelings of unity with creation
  mystical26, #	Personal identification with all of creation
  mystical14, #	Profound feelings of connection with a spiritural source
  mystical25 #	An experience of union with the Divine-God or universal consciousness
)~1)
kps.data <- fire.cluster(kps.data, f, "MS.HC")


# ECSTACY
f<-with(kps.data, cbind(
  mystical5, #	Overwhelming sense of love
  mystical4, #	Intense feeling of peace
  mystical9, #	Overwhelming sense of bliss, joy and or contentment
  mystical7 #	Overwhelming sense of wonder and awe
)~1)
kps.data <- fire.cluster(kps.data, f, "MS.ECSTACY")


# ALTERED
f<-with(kps.data, cbind(
  mystical11, #	Divine altered states
  mystical20, #	Feeling sacred touch (wind, presence, touch)
  mystical16, #	Holy trance
  mystical18, #	Visions of glowing geometric shapes
  mystical1, #	Experiences or visions of deities, gurus, icons, saints or mystics or other religious prophets, religious icons or universal archetypes
  mystical17, #	Receiving instructions from the divine
  mystical21, #	Insightful compreshension of scripture
  mystical19 #	Tasting sacred nectar dripping from the roof of mouth or back of throat (amrita or soma)
)~1)
kps.data <- fire.cluster(kps.data, f, "MS.ALTERED")


#
# SPIRITUAL
#

# REBIRTH
f<-with(kps.data, cbind(
  spiritual5, #	Glimpses of bliss, spiritual advancement
  spiritual3, #	An unshakable conviction about the reality of the experience
  spiritual4, #	Transient, atypical, pivotal, or opening experiences
  spiritual1, #	Spiritual rebirth - spontaneous religious conversion or dramatic spiritual awakening, including a major reorientation of spiritual beliefs.
  spiritual2 #	New understanding of spiritual truths/Insight into the inner meaning of spiritual teachings
)~1)
kps.data <- fire.cluster(kps.data, f, "SP.REBIRTH")

# CONNECTION
f<-with(kps.data, cbind(
  spiritual10, #	Ability to heal or to balance disturbed energies in self and others
  spiritual20, #	Feeling of connection with a spiritual guide or lineage
  spiritual9, #	Past lives recall
  spiritual8, #	Increased intuition/Intuitive awareness
  spiritual17, #	Receiving inner instruction
  spiritual13, #	Encounters with nonmaterial entities(the deceased, lower astral beings, aliens, spirit guides)
  spiritual18, #	Communion with nature and animals
  spiritual11, #	Ability to see auras
  spiritual12 #	Feeling able to influence another's mind or know the contents of their mind
spiritual16	Near death experiences
)~1)
kps.data <- fire.cluster(kps.data, f, "SP.CONNECTION")

# SYNCH
f<-with(kps.data, cbind(
  spiritual24, #	Greater incidence of prayers being answered
  spiritual23, #	Increased experience of unsual synchronistic events
  spiritual25, #	Spontaneously going into a deep meditative experience
  spiritual27, #	A newly developed power of spiritual insight and wisdom
  spiritual26, #	A profound insight into the inner workings of the Cosmos
  spiritual21, #	Deep intuitive knowledge of self and others
  spiritual22 #	Inspired creativity
)~1)
kps.data <- fire.cluster(kps.data, f, "SP.SYNCH")

# AURAL
f<-with(kps.data, cbind(
  spiritual19, #	Sacred auditions (of teachings, mantras, music)
  spiritual6 #	Glossalalia (spontaneously reciting phonemes, mantras, prayers, or poems, singing hymns, or speaking foreign languages unknown to the individual)
)~1)
kps.data <- fire.cluster(kps.data, f, "SP.AURAL")

# OOB
f<-with(kps.data, cbind(
  spiritual15, #	Astral/time travel experiences
  spiritual14 #	Out of body experiences
)~1)
kps.data <- fire.cluster(kps.data, f, "SP.OOB")

# LONGING
f<-with(kps.data, cbind(
  spiritual7 #	Increased desire for spiritual experiences
)~1)
kps.data <- fire.cluster(kps.data, f, "SP.LONGING")



#
# PSYPHYS
#

# SENSORY
f<-with(kps.data, cbind(
  psyphys6, #	Inner sensations of heat or cold
  psyphys7, #	Transient or unusual symptoms or some features atypical for a standard diagnosis
  psyphys8 #	Inner sensory experiences (sounds, light, smells, tastes, touch)
)~1)
kps.data <- fire.cluster(kps.data, f, "PP.SENSORY")

# LIGHTSOUND
f<-with(kps.data, cbind(
  psyphys1, #	Sensations of white light or luminosity
  psyphys2, #	Inner sound (rushing of water, humming of bees, distant music)
  psyphys11 #	Visions of light
)~1)
kps.data <- fire.cluster(kps.data, f, "PP.LIGHTSOUND")

# FLOATING
f<-with(kps.data, cbind(
  psyphys10, #	Loss of body awareness
  psyphys12 #	Floating in the light
)~1)
kps.data <- fire.cluster(kps.data, f, "PP.FLOATING")

# ENERGY
f<-with(kps.data, cbind(
  psyphys3, #	Sensations of energy rushing up the spine
  psyphys5, #	Feelings of energy flowing or vibrating within
  psyphys9, #	Sensations of energy along the seven major chakras (chakras are spinning vortices of energy) that run from the base of the spine to the crown of the head
  psyphys4 #	Marked increase in sensitivity of the five senses
)~1)
kps.data <- fire.cluster(kps.data, f, "PP.ENERGY")



#
# Generate psychological profile
#

# Run an LCA on each of the psychological factors to determine high vs. low
# Provide readout based on ID

kps.data.pg <- subset(kps.data, psygrowth.gate == "Y")
# kps.data.pg <- kps.data.pg[,grepl("psygrowth\\d+", names(kps.data.pg))]
# Generate psygrowth clusters

# Ensure all labels are used in the subset
likert.names <- grepl('mystical\\d+|spiritual\\d+|psyphys\\d+|psychic\\d+|talents\\d+|invmov\\d+|sensation\\d+|negphysical\\d+|otherphysical\\d+|negpsych\\d+|psybliss\\d+|psygrowth\\d+',
                      names(kps.data.pg))
kps.data.pg[,likert.names] <- lapply(kps.data.pg[,likert.names], function(x) {
  x <- mapvalues(x, 
                 from=c('Not at all', 'Very Weak/low intensity', 'Weak', 'Moderate', 'Strong', 'Very strong/high intensity'), 
                 to=c("1", "2","3", "4", "5", "6")
                 , warn_missing = FALSE)
  x <- ordered(x)
  return(x)
})


# PG.YEARN
f<-with(kps.data.pg, cbind(
  psygrowth17, #	A desire to pray, meditate, study scripture, read about holy people, go to holy places
  psygrowth20, #	Increased study of spiritual topics
  psygrowth31, #	Spiritual yearning
  psygrowth18, #	Increase spiritual practices (prayer, meditation)
  psygrowth19, #	Increased focus on spiritual issues and settings
  psygrowth15, #	An interest in spiritual growth, religion, metaphysics, the non-ordinary, the beyond
  psygrowth1, #	A deep yearning for inner development and self-understanding
  psygrowth16 #	A sense that something non-ordinary, transformative, or holy is happening within
)~1)
kps.data.pg <- fire.cluster(kps.data.pg, f, "PG.YEARN")

# PG.SURFACE
f<-with(kps.data.pg, cbind(
  psygrowth43, #	Intensification of unresolved psychological issues
  psygrowth42 #	Spontaneous surfacing of unconscious issues
)~1)
kps.data.pg <- fire.cluster(kps.data.pg, f, "PG.SURFACE")

# PG.SOLITUDE
f<-with(kps.data.pg, cbind(
  psygrowth38, #	Desire for a different life
  psygrowth7, #	Being particular about one's environment, food, company, etc.
  psygrowth27, #	Decreased interest in activities formerly enjoyed
  psygrowth28, #	Making a sudden change in one's life
  psygrowth26, #	Aversion to negative people or crowds
  psygrowth13, #	Seeking respite, answers, guidance, meaning
  psygrowth21, #	Increased desire for a simpler, more balanced lifestyle
  psygrowth36, #	Increased desire to heal from emotional issues
  psygrowth12, #	Being introspective about values, purpose
  psygrowth9 #	A heightened inner awareness
)~1)
kps.data.pg <- fire.cluster(kps.data.pg, f, "PG.SOLITUDE")

# PG.SEPARATED
f<-with(kps.data.pg, cbind(
  psygrowth3, #	Feeling separated, empty, hollow
  psygrowth2 #	Feeling different, not fitting in
)~1)
kps.data.pg <- fire.cluster(kps.data.pg, f, "PG.SEPARATED")

# PG.INWARD
f<-with(kps.data.pg, cbind(
  psygrowth29, #	Keeping of silence
  psygrowth41, #	The mind becomes inward and vacant
  psygrowth37, #	Detachment
  psygrowth35, #	Decreased materialism and greed
  psygrowth40, #	Increased mental flexibility
  psygrowth32, #	Increased centeredness and objectivity
  psygrowth39 #	Improved concentration
)~1)
kps.data.pg <- fire.cluster(kps.data.pg, f, "PG.INWARD")

# PG.HEALTH
f<-with(kps.data.pg, cbind(
  psygrowth23, #	Increased rest and relaxation
  psygrowth22, #	Improved dietary observations
  psygrowth25, #	Increased activities involving creativity and concentration
  psygrowth24, #	Increased uplifting physical activity (walking, gardening, singing, dancing)
  psygrowth44 #	An increase in energy level and health
)~1)
kps.data.pg <- fire.cluster(kps.data.pg, f, "PG.HEALTH")

# PG.ALTRUISM
f<-with(kps.data.pg, cbind(
  psygrowth47, #	Increase in morals
  psygrowth10, #	An interest in helping others, compassion for the suffering
  psygrowth30, #	Deep compassion
  psygrowth46, #	Increase in discipline
  psygrowth45, #	A deep abiding love for humanity and family
  psygrowth34, #	Increased altruism
  psygrowth33 #	Increase sensitivity to others' feelings
)~1)
kps.data.pg <- fire.cluster(kps.data.pg, f, "PG.ALTRUISM")

# PG.ABILITY
f<-with(kps.data.pg, cbind(
  psygrowth4, #	An elevated sensitivity
  psygrowth5, #	Special capacities, intelligence, creativity
  psygrowth6, #	Competence in some special area
  psygrowth14, #	Non-ordinary abilities
  psygrowth8, #	Good intuition, insight, perceptiveness, empathy
  psygrowth11 #	Some ability for inner focus and concentration
)~1)
kps.data.pg <- fire.cluster(kps.data.pg, f, "PG.ABILITY")


# Merge back with original data
kps.data <- merge(kps.data, kps.data.pg[ , grepl("id|PG\\.", names(kps.data.pg))]
                  , by=c("id")
                  , all.x = TRUE)


# Psybliss

kps.data.pb <- subset(kps.data, psybliss.gate == "Y")

# PB.ONENESS
f<-with(kps.data.pb, cbind(
  psybliss19, #	Increased maturation of the personality
  psybliss22, #	Increased tolerance and balance
  psybliss23, #	Increase ability to see the underlying point of things
  psybliss18, #	Increased belief in the divine or One
  psybliss21 #	Increased belief in the unity of humanity
)~1)
kps.data.pb <- fire.cluster(kps.data.pb, f, "PB.ONENESS")

# PB.INTUNE
f<-with(kps.data.pb, cbind(
  psybliss7, #	Increased focus on and great energy for purposeful work
  psybliss12, #	A floaty or spacey sensation
  psybliss13, #	Devotional focus
  psybliss5, #	A good capacity for natural highs (peak experiences)
  psybliss3, #	Flood of pure emotion, feeling of overwhelming emotion, devotion, reverence, cascading tears
  psybliss11, #	Feeling buoyant and full of energy
  psybliss14, #	Increased capacity for love and forgiveness
  psybliss4, #	Feeling inwardly connected, in tune, fulfilled
  psybliss15, #	Emotional balance in perturbed conditons
  psybliss1 #	Loss of fear or death/ certainty of immortality
)~1)
kps.data.pb <- fire.cluster(kps.data.pb, f, "PB.INTUNE")

# PB.INSPIRATION
f<-with(kps.data.pb, cbind(
  psybliss24, #	Increased creativity
  psybliss6, #	Spontaneous composition of poems, prose, or music
  psybliss16, #	Special talents
  psybliss2, #	Flashes of genius
  psybliss25, #	Increased personal magnetism
  psybliss8 #	Increase altruistic service activities
)~1)
kps.data.pb <- fire.cluster(kps.data.pb, f, "PB.INSPIRATION")

# PB.DETACHMENT
f<-with(kps.data.pb, cbind(
  psybliss17, #	Increased self-actualizing traits
  psybliss26, #	Highly developed moral sense
  psybliss28, #	Increased insight regarding personal issues
  psybliss27, #	Increased virtue
  psybliss29, #	Abandonment of self-destructive patterns
  psybliss20, #	Increased clarity of self and values
  psybliss9, #	Enhanced ability to overcome addictions and negative behaviors
  psybliss10 #	Increased ability to disengage from dysfunctional relationships
)~1)
kps.data.pb <- fire.cluster(kps.data.pb, f, "PB.DETACHMENT")

# Merge back with original data
kps.data <- merge(kps.data,kps.data.pb[ , grepl("id|PB\\.", names(kps.data.pb))]
                  , by=c("id")
                  , all.x = TRUE)

#
# Generate negative psychological profile
#

kps.data.np <- subset(kps.data, negpsych.gate == "Y")
# Generate negpsych clusters

# Ensure all labels are used in the subset
likert.names <- grepl('mystical\\d+|spiritual\\d+|psyphys\\d+|psychic\\d+|talents\\d+|invmov\\d+|sensation\\d+|negphysical\\d+|otherphysical\\d+|negpsych\\d+|psybliss\\d+|psygrowth\\d+',
                      names(kps.data.np))
kps.data.np[,likert.names] <- lapply(kps.data.np[,likert.names], function(x) {
  x <- mapvalues(x, 
                 from=c('Not at all', 'Very Weak/low intensity', 'Weak', 'Moderate', 'Strong', 'Very strong/high intensity'), 
                 to=c("1", "2","3", "4", "5", "6")
                 , warn_missing = FALSE)
  x <- ordered(x)
  return(x)
})

# DEPRESSION
f<-with(kps.data.np, cbind(
  negpsych6, #	An inner (existential) restlessness or despair
  negpsych19, #	Anxiety, anger, fear, dread, or guilt
  negpsych2, #	Extreme anxiety, helplessness or confusion
  negpsych17, #	Periods of depression
  negpsych5, #	Feeling frustrated about physical challenges/changes
  negpsych18 #	Loneliness
)~1)
kps.data.np <- fire.cluster(kps.data.np, f, "NP.DEPRESSION")

# NEGENTITY
f<-with(kps.data.np, cbind(
  negpsych1, #	Experiences or visions of devils, evil or death
  negpsych3, #	Extreme fear, panic or paranoia
  negpsych12 #	Fear of being influenced by unknown forces
)~1)
kps.data.np <- fire.cluster(kps.data.np, f, "NP.NEGENTITY")

# NOFOCUS
f<-with(kps.data.np, cbind(
  negpsych23, #	Scattered thoughts
  negpsych22, #	Difficulty concentrating on tasks
  negpsych35, #	Lack of ability to focus
  negpsych24 #	Mental dulling
)~1)
kps.data.np <- fire.cluster(kps.data.np, f, "NP.NOFOCUS")

# AVOIDANCE
f<-with(kps.data.np, cbind(
  negpsych21, #	Temporary confusion
  negpsych25, #	Pain attacks
  negpsych28, #	Do not feel like oneself
  negpsych9, #	Aversion or inability to work
  negpsych8, #	Dislike for interaction
  negpsych29, #	Feeling detached
  negpsych27, #	Boundary issues
  negpsych30, #	Derealization
  negpsych7	# Unusual, non-pathological experiences
)~1)
kps.data.np <- fire.cluster(kps.data.np, f, "NP.INTROVERT")

# MOODSWINGS
f<-with(kps.data.np, cbind(
  negpsych10, #	Sudden uncharacteristic acting out
  negpsych15, #	Mood swings
  negpsych16, #	Emotional instability
  negpsych11, #	Spontaneous surfacing of repressed unconscious issues
  negpsych20, #	Psychological symptoms more intensly experienced
  negpsych14	# A conviction one is dying
)~1)
kps.data.np <- fire.cluster(kps.data.np, f, "NP.MOODSWINGS")

# COMPULSION
f<-with(kps.data.np, cbind(
  negpsych33, #	Fanaticism
  negpsych32, #	Inflation
  negpsych31, #	Delusions
  negpsych34	# Obsessive/compulsive behavior
)~1)
kps.data.np <- fire.cluster(kps.data.np, f, "NP.COMPULSION")

# FEARCONTROL
f<-with(kps.data.np, cbind(
  negpsych26, #	Fear of losing control
  negpsych4, #	Feeling confused about non-ordinary experiences
  negpsych13	# Fear of going crazy
)~1)
kps.data.np <- fire.cluster(kps.data.np, f, "NP.FEARCONTROL")

# Merge back with original data
kps.data <- merge(kps.data, kps.data.np[ , grepl("id|NP\\.", names(kps.data.np))]
                  , by=c("id")
                  , all.x = TRUE)



#
# Generate other physical profile
#

kps.data.op <- subset(kps.data, pe.gate == "Y")
# Generate otherphysical clusters

# Ensure all labels are used in the subset
likert.names <- grepl('mystical\\d+|spiritual\\d+|psyphys\\d+|psychic\\d+|talents\\d+|invmov\\d+|sensation\\d+|negphysical\\d+|otherphysical\\d+|negpsych\\d+|psybliss\\d+|psygrowth\\d+',
                      names(kps.data.op))
kps.data.op[,likert.names] <- lapply(kps.data.op[,likert.names], function(x) {
  x <- mapvalues(x, 
                 from=c('Not at all', 'Very Weak/low intensity', 'Weak', 'Moderate', 'Strong', 'Very strong/high intensity'), 
                 to=c("1", "2","3", "4", "5", "6")
                 , warn_missing = FALSE)
  x <- ordered(x)
  return(x)
})

# DIGESTIVE
f<-with(kps.data.op, cbind(
  otherphysical20, #	Shifting food sensitivities and cravings or aversions
  otherphysical13, #	Digestive system changes
  otherphysical17, #	Increased sensitivity to environmental toxins
  otherphysical19, #	New or intensified addictions
  otherphysical14, #	Appetite swings
  otherphysical10	# Exhaustion
)~1)
kps.data.op <- fire.cluster(kps.data.op, f, "OP.DIGESTIVE")

# TRANCE
f<-with(kps.data.op, cbind(
  otherphysical15, #	Increased body heat
  otherphysical8 #	Falling down into trance
)~1)
kps.data.op <- fire.cluster(kps.data.op, f, "OP.TRANCE")

# SLEEP
f<-with(kps.data.op, cbind(
  otherphysical23, #	Restlessness
  otherphysical4, #	Decreased or increased need for sleep
  otherphysical25, #	Early morning awakenings
  otherphysical21, #	Sleep pattern changes
  otherphysical24, #	Insomnia
  otherphysical7	# Night sweats
)~1)
kps.data.op <- fire.cluster(kps.data.op, f, "OP.SLEEP")

# FACIALRADIANCE
f<-with(kps.data.op, cbind(
  otherphysical3, #	Pupils dilating
  otherphysical2, #	The eyes shining
  otherphysical1 #	The face glowing
)~1)
kps.data.op <- fire.cluster(kps.data.op, f, "OP.FACIALRADIANCE")

# ENERGETIC
f<-with(kps.data.op, cbind(
  otherphysical12, #	Enhanced health
  otherphysical9, #	Hyperactivity
  otherphysical11 #	Increased energy
)~1)
kps.data.op <- fire.cluster(kps.data.op, f, "OP.ENERGETIC")

# LETHARGY
f<-with(kps.data.op, cbind(
  otherphysical18, #	Lethargy
  otherphysical22, #	Sexual activity changes
  otherphysical5 #	Eyes tearing
)~1)
kps.data.op <- fire.cluster(kps.data.op, f, "OP.LETHARGY")

# BODYDISSO
f<-with(kps.data.op, cbind(
  otherphysical26, #	Enhanced sensory acuity and sensitivity
  otherphysical27, #	A sensation of something throbbing insisde or crawling on the skin
  otherphysical6, #	Hair standing on end
  otherphysical28, #	Feeling intoxicated
  otherphysical29 #	Feeling bodiless
)~1)
kps.data.op <- fire.cluster(kps.data.op, f, "OP.BODYDISSO")

# SEXDESIRE
# f<-with(kps.data, cbind(
#   otherphysical30	# Changes in sexual desires
# )~1)
# kps.data.op <- fire.cluster(kps.data.op, f, "OP.SEXDESIRE")

kps.data.op$OP.SEXDESIRE <- kps.data.op$otherphysical30

# HOTFLASH
# f<-with(kps.data, cbind(
#   otherphysical16	# Hot flashes, cold flashes or localized sensations
# )~1)
# kps.data.op <- fire.cluster(kps.data.op, f, "OP.HOTFLASH")

kps.data.op$OP.HOTFLASH <- kps.data.op$otherphysical16

# Merge back with original data
kps.data <- merge(kps.data, kps.data.op[ , grepl("id|OP\\.", names(kps.data.op))]
                  , by=c("id")
                  , all.x = TRUE)


#
# Generate sensation profile
#

kps.data.se <- subset(kps.data, pe.sensation.gate == "Y")
# Generate sensation clusters

# Ensure all labels are used in the subset
likert.names <- grepl('mystical\\d+|spiritual\\d+|psyphys\\d+|psychic\\d+|talents\\d+|invmov\\d+|sensation\\d+|negphysical\\d+|otherphysical\\d+|negpsych\\d+|psybliss\\d+|psygrowth\\d+',
                      names(kps.data.se))
kps.data.se[,likert.names] <- lapply(kps.data.se[,likert.names], function(x) {
  x <- mapvalues(x, 
                 from=c('Not at all', 'Very Weak/low intensity', 'Weak', 'Moderate', 'Strong', 'Very strong/high intensity'), 
                 to=c("1", "2","3", "4", "5", "6")
                 , warn_missing = FALSE)
  x <- ordered(x)
  return(x)
})


# NERVES
f<-with(kps.data.se, cbind(
  sensation9, #	Nausea
  sensation8, #	Nervous energy
  sensation7	# Energy swings
)~1)
kps.data.se <- fire.cluster(kps.data.se, f, "SE.NERVES")

# ITCHTHROB
f<-with(kps.data.se, cbind(
  sensation1, #	Uncontrollable Itching
  sensation3 #	Throbbing at the anal area
)~1)
kps.data.se <- fire.cluster(kps.data.se, f, "SE.ITCHTHROB")

# SEXSTIM
f<-with(kps.data.se, cbind(
  sensation2, #	Spontaneous orgasm
  sensation6	# Sexual sensations
)~1)
kps.data.se <- fire.cluster(kps.data.se, f, "SE.SEXSTIM")

# ENERGYSENS
f<-with(kps.data.se, cbind(
  sensation10, #	Sensations of tiny electric currents felt below the scalp zipping and fizzing around the head, and/or throughout the body
  sensation4, #	Electric tingling
  sensation5	# Energy vibrations and currents in the nerves, spine and brain
)~1)
kps.data.se <- fire.cluster(kps.data.se, f, "SE.ENERGYSENS")

# Merge back with original data
kps.data <- merge(kps.data, kps.data.se[ , grepl("id|SE\\.", names(kps.data.se))]
                  , by=c("id")
                  , all.x = TRUE)


#
# Generate psychic profile
#

# MENTALPSY
f<-with(kps.data, cbind(
  psychic3, #	Clairvoyance: Having or claiming to have the power of seeing objects or actions beyond the range of natural vision
  psychic1, #	Precognition: A sense of knowing or foretelling of events before they occur
  psychic2, #	Telepathy: Communication from one mind to another by extrasensory means
  psychic6	# Hearing things outside the range of normal perception, e.g., heavenly music, the voice of spirits or angels, etc.
)~1)
kps.data <- fire.cluster(kps.data, f, "PS.MENTALPSY")

# MEDIUM
f<-with(kps.data, cbind(
  psychic5, #	Channeling: The act or practice of serving as a medium through which a spirit guide communicates with living persons.
  psychic4 #	Psychokinesis: Movement of physical objects by the mind without use of physical means
)~1)
kps.data <- fire.cluster(kps.data, f, "PS.MEDIUM")

# EMPATH
f<-with(kps.data, cbind(
  psychic9,	# Psychic Empathy: The ability to tune into the emotions and physical feelings of another person beyond normal perception
  psychic7, #	Clairsentience: The ability to acquire new knowledge or a greater insight into things beyond normal perception
  psychic8 #	Dreams which foretell events as if by supernatural intervention
)~1)
kps.data <- fire.cluster(kps.data, f, "PS.EMPATH")


#
# Generate talents profile
#

# LANGSTRENGTH
f<-with(kps.data, cbind(
  talents3, #	Musical talents
  talents8, #	Rapid or instantaneous acquisition of new languages
  talents1	# Physical talents or feats of strength
  )~1)
kps.data <- fire.cluster(kps.data, f, "TL.LANGSTRENGTH")

# VIZTALENT
# f<-with(kps.data, cbind(
#  talents2	# Visual artistic talents
# )~1)
# kps.data <- fire.cluster(kps.data, f, "TL.VIZTALENT")

kps.data$TL.VIZTALENT <- kps.data$talents2

# COMMTALENT
f<-with(kps.data, cbind(
  talents6, #	Written talents including poetry and prose
  talents7, #	Speaking talents
  talents5, #	Uncommon dedication to an important social cause
  talents4 #	Intellectual talents such as uncommon mental accomplishments
)~1)
kps.data <- fire.cluster(kps.data, f, "TL.COMMTALENT")



#
# Generate involutary movements profile
#

kps.data.im <- subset(kps.data, pe.invmov.gate == "Y")
# Generate invmov clusters

# Ensure all labels are used in the subset
likert.names <- grepl('mystical\\d+|spiritual\\d+|psyphys\\d+|psychic\\d+|talents\\d+|invmov\\d+|sensation\\d+|negphysical\\d+|otherphysical\\d+|negpsych\\d+|psybliss\\d+|psygrowth\\d+',
                      names(kps.data.im))
kps.data.im[,likert.names] <- lapply(kps.data.im[,likert.names], function(x) {
  x <- mapvalues(x, 
                 from=c('Not at all', 'Very Weak/low intensity', 'Weak', 'Moderate', 'Strong', 'Very strong/high intensity'), 
                 to=c("1", "2","3", "4", "5", "6")
                 , warn_missing = FALSE)
  x <- ordered(x)
  return(x)
})

# BODYSPASM
f<-with(kps.data, cbind(
  invmov8, #	Outstretching of the arms and legs
  invmov12, #	The tongue rolling back
  invmov13 #	Eyes closing or rotating or gazing between the eyebrows
)~1)
kps.data.im <- fire.cluster(kps.data.im, f, "IM.BODYSPASM")

# CONTRACT
f<-with(kps.data, cbind(
  invmov14, #	Involuntary jerks or spasms or shaking
  invmov15, #	Contractions of the body
  invmov11	# Head and facial movements
)~1)
kps.data.im <- fire.cluster(kps.data.im, f, "IM.CONTRACT")

# SPONKRIYA
f<-with(kps.data, cbind(
  invmov2, #	Automatic gesturing of arms and hands(mudras)
  invmov3, #	Spontaneous movement or posturing(kriyas)
  invmov1	# Spontaneous bodily movements, gestures, postures, or breathing patterns
)~1)
kps.data.im <- fire.cluster(kps.data.im, f, "IM.SPONKRIYA")

# DANCEINV
f<-with(kps.data, cbind(
  invmov10, #	Involuntary weeping or laughing
  invmov6, #	Spontaneous dancing
  invmov7, #	Hand clapping
  invmov9	# Sudden running or jumping
)~1)
kps.data.im <- fire.cluster(kps.data.im, f, "IM.DANCEINV")

# SPINEINV
f<-with(kps.data, cbind(
  invmov19, #	Assuming a fixed Posture
  invmov5, #	Engaging the three bandhas(contracting locks at the anus, abdomen, and throat)
  invmov20, #	Involuntary shouting/sounds
  invmov17, #	Pelvic movements
  invmov16, #	Arching of the back
  invmov18	# The seated body jumping
)~1)
kps.data.im <- fire.cluster(kps.data.im, f, "IM.SPINEINV")

# BREATHEINV
# f<-with(kps.data, cbind(
#   invmov4	# Deep automatic breathing or retention of breath (pranayama)
# )~1)
# kps.data.im <- fire.cluster(kps.data.im, f, "IM.BREATHEINV")

kps.data.im$IM.BREATHEINV <- kps.data.im$invmov4

# Merge back with original data
kps.data <- merge(kps.data, kps.data.im[ , grepl("id|IM\\.", names(kps.data.im))]
                  , by=c("id")
                  , all.x = TRUE)


#
# Generate negative physical profile
#

kps.data.nh <- subset(kps.data, pe.negphysical.gate == "Y")
# Generate negphysical clusters

# Ensure all labels are used in the subset
likert.names <- grepl('mystical\\d+|spiritual\\d+|psyphys\\d+|psychic\\d+|talents\\d+|invmov\\d+|sensation\\d+|negphysical\\d+|otherphysical\\d+|negpsych\\d+|psybliss\\d+|psygrowth\\d+',
                      names(kps.data.nh))
kps.data.nh[,likert.names] <- lapply(kps.data.nh[,likert.names], function(x) {
  x <- mapvalues(x, 
                 from=c('Not at all', 'Very Weak/low intensity', 'Weak', 'Moderate', 'Strong', 'Very strong/high intensity'), 
                 to=c("1", "2","3", "4", "5", "6")
                 , warn_missing = FALSE)
  x <- ordered(x)
  return(x)
})

# ANOREXIA
# f<-with(kps.data, cbind(
#   negphysical13	# Anorexia
# )~1)
# kps.data.nh <- fire.cluster(kps.data.nh, f, "NH.ANOREXIA")

kps.data.nh$NH.ANOREXIA <- kps.data.nh$negphysical13

# BLINDNESS
# f<-with(kps.data, cbind(
#   negphysical12	# Temporary blindness
# )~1)
# kps.data.nh <- fire.cluster(kps.data.nh, f, "NH.BLINDNESS")

kps.data.nh$NH.BLINDNESS <- kps.data.nh$negphysical12

# NUMBHEAD
f<-with(kps.data, cbind(
  negphysical7, #	Migratory pains
  negphysical5, #	Headaches
  negphysical8	# Localized temporary numbness
)~1)
kps.data.nh <- fire.cluster(kps.data.nh, f, "NH.NUMBHEAD")

# ABBACKPAIN
f<-with(kps.data, cbind(
  negphysical2, #	Abdominal pain
  negphysical6	# Backaches
)~1)
kps.data.nh <- fire.cluster(kps.data.nh, f, "NH.ABBACKPAIN")

# HEARTPROB
f<-with(kps.data, cbind(
  negphysical3, #	Discomfort in the heart (spinning, pressure, pain)
  negphysical4 #	Dramatic variations in heartbeat (pounding, racing, stopping)
)~1)
kps.data.nh <- fire.cluster(kps.data.nh, f, "NH.HEARTPROB")

# RASHFEVER
f<-with(kps.data, cbind(
  negphysical1, #	Fevers
  negphysical14	# Transient rashes
)~1)
kps.data.nh <- fire.cluster(kps.data.nh, f, "NH.RASHFEVER")

# VISUAL
f<-with(kps.data, cbind(
  negphysical11, #	Visual difficulties
  negphysical10, #	Eyes burning
  negphysical9	# Light sensitivity
)~1)
kps.data.nh <- fire.cluster(kps.data.nh, f, "NH.VISUAL")

# Merge back with original data
kps.data <- merge(kps.data, kps.data.nh[ , grepl("id|NH\\.", names(kps.data.nh))]
                  , by=c("id")
                  , all.x = TRUE)

#
# Subject Readout
#

# Extract subject row
sub <- kps.data[match(id, kps.data$id),]
sub <- sub[,grepl("[A-Z]+", names(sub))]


#
# Convert class data to ordinal
#

composite.names <- grepl('[A-Z]+|PB.|PG.',
                      names(kps.data))
kps.data[,composite.names] <- lapply(kps.data[,composite.names], ordered )


#
# Decision Tree Analysis of MCLASS
#

paste0(names(kps.data), collapse = "|")


treemod <- rpart(
  MS.CONSCIOUSNESS ~ .
  # , data = kps.data[,grepl('CONSCIOUSNESS|ECSTACY|GRACE|REBIRTH|CONNECTION|NOETIC|AURAL|OOB|LIGHT|ENERGY|PG.|PB.', names(kps.data))]
  , data = kps.data[,grepl('MS.CONSCIOUSNESS|PG.', names(kps.data))]
  , minsplit = 15, cp = .02, method = "class"
)
rpart.plot(treemod, tweak = 1.1, fallen.leaves = FALSE)

treemod <- rpart(
  MS.CONSCIOUSNESS ~ .
  , data = kps.data[,grepl('MS.CONSCIOUSNESS|PB\\.', names(kps.data))]
  , minsplit = 15, cp = .01, method = "class"
)
rpart.plot(treemod, tweak = 1.1, fallen.leaves = FALSE)

printcp(treemod)

# train.rpart <- train(CONSCIOUSNESS ~ .
#                      , data=kps.data[,grepl('CONSCIOUSNESS|PG.', names(kps.data))]
#                      , method="rpart",na.action = na.omit)
# train.rpart
# rpart.plot(train.rpart$finalModel, tweak = 1.0, fallen.leaves = FALSE)




#
# fire.profile() - Profile all non-primary experience clusters
#                  Returns a data frame containing all descriptive
#                  and statistical output.
# 
fire.profile <- function(data = NULL, target = "", grepstr = "", prefix = "") {
  
  # Populate "compare" data frame one row at a time
  
  compare <- data.frame( # Destination for all our statistics
    question = character(),
    polychor = double(), # Polychoric r
    chisq = double(), # Chi-square
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
    
    compare[i, "polychor"] <- polychor(data[,target], data[,varnames[i]])
    
    chiresult <- chisq.test(data[,target], data[,varnames[i]])
    compare[i, "chisq"] <- as.double(chiresult$statistic)
  }
  
  # Tack on the question text to the end
  # compare <- merge(compare, kps.vars[c("varname", "question.text")], by.x = "question", by.y = "varname")
  
  # Write file out to the output folder with the specified prefix
  write.csv(compare, file = paste("output/", prefix, "-FIRE-profile.csv", sep = ""))
  
  return(data.frame(compare))
}

kps.sub <- kps.data[,grepl("[A-Z]+|PB.|PG.", names(kps.data))]
fprofile <- fire.profile(data = kps.sub, target = "MS.HC", prefix = "all")

# Output subject record
t(sub)

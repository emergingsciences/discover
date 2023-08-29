#
# LimeSurvey Survey Data ETL and Import Script
#
# Imports survey scripts and performs necessary transformations on the
# data to prep it for analysis
#
# @return kps.extract.df The return object
#


library(digest)

# Flag indicating whether to generate a Level 2 file.
#
# FILE LEVELS
#
# Level 1 - Likert responses and minor text fields
# Level 2 - All data contained in the Level 2 file plus all open text responses
generateLevel2 <- TRUE


# Read in output file from LimeSurvey (question codes and answer codes)
raw_survey_results.df <- read.csv("data/ses-results-raw.csv", na.strings = "")
var.names <- read.csv("data/ses-vars.csv", stringsAsFactors = FALSE)

# Remove "." characters in column names. R has problems with these
names(raw_survey_results.df) <- gsub(".", "", names(raw_survey_results.df), fixed = TRUE)

# Extract only relevant fields based on flag
if(isTRUE(generateLevel2)) {
  extract.df <- raw_survey_results.df[,grepl('id|token|CurrentAge$|Sex$|MysticalSymptoms\\d+|PersonalandPsychic\\d+|TalentsSymptoms\\d+|PsyPhyList\\d+|PsychicSymptoms\\d+|PEInvMovSymptoms\\d+|PEFeelaSenseList\\d+|PEIlloDisSymptoms\\d+|PEOtherBehavSymp\\d+|PsyGrowthList\\d+|NegPsyEffList\\d+|PsychoBlissList\\d+|Gate$|MysticalText$|SpExOpenText$|PsychicOpenText$|TalentsOpenText$|PEFeelingsSensOpenT$|PEIllnessDisOpenText$|PsyGrowthOpenText$|PsyBlissOpenText$'
                                             , names(raw_survey_results.df))]  
} else {
  extract.df <- raw_survey_results.df[,grepl('id|token|CurrentAge$|Sex$|MysticalSymptoms\\d+|PersonalandPsychic\\d+|TalentsSymptoms\\d+|PsyPhyList\\d+|PsychicSymptoms\\d+|PEInvMovSymptoms\\d+|PEFeelaSenseList\\d+|PEIlloDisSymptoms\\d+|PEOtherBehavSymp\\d+|PsyGrowthList\\d+|NegPsyEffList\\d+|PsychoBlissList\\d+|Gate$'
                                             , names(raw_survey_results.df))]    
}


#
# Convert all raw answer codes from LimeSurvey to the appropriate Likert indicator
#

likert.questions <- grepl('MysticalSymptoms\\d+|PersonalandPsychic\\d+|TalentsSymptoms\\d+|PsyPhyList\\d+|PsychicSymptoms\\d+|PEInvMovSymptoms\\d+|PEFeelaSenseList\\d+|PEIlloDisSymptoms\\d+|PEOtherBehavSymp\\d+|PsyGrowthList\\d+|NegPsyEffList\\d+|PsychoBlissList\\d+',
                          names(extract.df))

likert.names <- names(extract.df[,likert.questions])

extract.df$Sex <- factor(extract.df$Sex, levels = c('A1', 'A2', 'A3'), labels = c('female', 'male', 'intersex'))

likert.levels <- c('Not at all', 'Very Weak/low intensity', 'Weak', 'Moderate', 'Strong', 'Very strong/high intensity')

# 'Not at all', 'Very Weak/low intensity', 'Weak', 'Moderate', 'Strong', 'Very strong/high intensity'
require(plyr)
extract.df[,likert.names] <- lapply(extract.df[,likert.names], function(x) {
  x <- mapvalues(x,
            from=c("L001","L002","L003", "L004", "L005", "L006"),
            to=c('Not at all', 'Very Weak/low intensity', 'Weak', 'Moderate', 'Strong', 'Very strong/high intensity')
  )

  # Convert the likert columns to an ordered factor
  x <- factor(x, levels = likert.levels, ordered = TRUE)
  return(x)
})




#
# Rename columns to final names
#
names(extract.df) <- gsub("MysticalSymptoms", "mystical", names(extract.df), fixed = TRUE)
names(extract.df) <- gsub("PersonalandPsychic", "spiritual", names(extract.df), fixed = TRUE)
names(extract.df) <- gsub("PsyPhyList", "psyphys", names(extract.df), fixed = TRUE)
names(extract.df) <- gsub("PsychicSymptoms", "psychic", names(extract.df), fixed = TRUE)
names(extract.df) <- gsub("TalentsSymptoms", "talents", names(extract.df), fixed = TRUE)
names(extract.df) <- gsub("PEInvMovSymptoms", "invmov", names(extract.df), fixed = TRUE)
names(extract.df) <- gsub("PEFeelaSenseList", "sensation", names(extract.df), fixed = TRUE)
names(extract.df) <- gsub("PEIlloDisSymptoms", "negphysical", names(extract.df), fixed = TRUE)
names(extract.df) <- gsub("PEOtherBehavSymp", "otherphysical", names(extract.df), fixed = TRUE)
names(extract.df) <- gsub("NegPsyEffList", "negpsych", names(extract.df), fixed = TRUE)
names(extract.df) <- gsub("PsychoBlissList", "psybliss", names(extract.df), fixed = TRUE)
names(extract.df) <- gsub("PsyGrowthList", "psygrowth", names(extract.df), fixed = TRUE)

# Non-likert responses
names(extract.df)[names(extract.df) == 'CurrentAge'] <- 'age'
names(extract.df)[names(extract.df) == 'Sex'] <- 'sex'

# Rename the gate questions
names(extract.df)[names(extract.df) == 'PEGate'] <- 'pe.gate'
names(extract.df)[names(extract.df) == 'PEInvMovGate'] <- 'pe.invmov.gate'
names(extract.df)[names(extract.df) == 'PEFeelandSensGate'] <- 'pe.sensation.gate'
names(extract.df)[names(extract.df) == 'PEIlloDisGate'] <- 'pe.negphysical.gate'
names(extract.df)[names(extract.df) == 'PsyGrowthGate'] <- 'psygrowth.gate'
names(extract.df)[names(extract.df) == 'NegPsyEffGate'] <- 'negpsych.gate'
names(extract.df)[names(extract.df) == 'PsyBlissGate'] <- 'psybliss.gate'

# Rename open text questions
if(isTRUE(generateLevel2)) {
  names(extract.df)[names(extract.df) == 'MysticalText'] <- 'mystical.text'
  names(extract.df)[names(extract.df) == 'SpExOpenText'] <- 'spiritual.text'
  names(extract.df)[names(extract.df) == 'PsyPhyOpentext'] <- 'psyphys.text'
  names(extract.df)[names(extract.df) == 'PsychicOpenText'] <- 'psychic.text'
  names(extract.df)[names(extract.df) == 'TalentsOpenText'] <- 'talents.text'
  names(extract.df)[names(extract.df) == 'PEFeelingsSensOpenT'] <- 'pe.sensation.text'
  names(extract.df)[names(extract.df) == 'PEIllnessDisOpenText'] <- 'pe.negphysical.text'
  names(extract.df)[names(extract.df) == 'PsyGrowthOpenText'] <- 'psygrowth.text'
  names(extract.df)[names(extract.df) == 'PsyBlissGate'] <- 'negpsych.text'
  names(extract.df)[names(extract.df) == 'PsyBlissOpenText'] <- 'psybliss.text'
}

# Remove rows with missing values in the "Age" column
print(paste("Initial row count is ", nrow(extract.df)))
extract.df <- extract.df[!is.na(extract.df$mystical1), ]
print(paste("After scrubbing data rows with missing values, the row count is ", nrow(extract.df)))


# Not sure why this line exists. Try it out!
extract.df.copy <- extract.df
rownames(extract.df.copy) <- NULL
dput(extract.df.copy, file = "data/ses-data.txt")

getKPS <- function () {
  return(extract.df)
}




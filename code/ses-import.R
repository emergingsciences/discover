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
generateLevel2 <- FALSE

# 'data' now contains all data

# Limesurvey R file import
setwd("~/Desktop/esf/discover/data") # Set to path of import files
source("survey_575246_R_syntax_file.R") # R import file
setwd("~/Desktop/esf/discover") # Set to path of working directory
raw_survey_results.df <- data
rm(data)

# Read in output file from LimeSurvey (question codes and answer codes)
# raw_survey_results.df <- read.csv("data/ses-results-raw.csv", na.strings = "")
var.names <- read.csv("data/ses-vars.csv", stringsAsFactors = FALSE)

# Remove "." characters in column names. R has problems with these
names(raw_survey_results.df) <- gsub(".", "", names(raw_survey_results.df), fixed = TRUE)

# Also remove underscores from all column names
colnames(raw_survey_results.df) <- gsub("_", "", colnames(raw_survey_results.df))

# Extract only relevant fields based on flag
if(isTRUE(generateLevel2)) {
  extract.df <- raw_survey_results.df[,grepl(
    'id|token|Country$|CurrentAge$|Sex$|MysticalSymptoms\\d+|PersonalandPsychic\\d+|TalentsSymptoms\\d+|PsyPhyList\\d+|PsychicSymptoms\\d+|PEInvMovSymptoms\\d+|PEFeelaSenseList\\d+|PEIlloDisSymptoms\\d+|PEOtherBehavSymp\\d+|PsyGrowthList\\d+|NegPsyEffList\\d+|PsychoBlissList\\d+|Gate$|MysticalText$|SpExOpenText$|PsychicOpenText$|TalentsOpenText$|PEFeelingsSensOpenT$|PEIllnessDisOpenText$|PsyGrowthOpenText$|PsyBlissOpenText$'
    , names(raw_survey_results.df)
    )]  
} else {
  extract.df <- raw_survey_results.df[,grepl(
    'id|token|Country|CurrentAge$|Sex$|MysticalSymptoms\\d+|PersonalandPsychic\\d+|TalentsSymptoms\\d+|PsyPhyList\\d+|PsychicSymptoms\\d+|PEInvMovSymptoms\\d+|PEFeelaSenseList\\d+|PEIlloDisSymptoms\\d+|PEOtherBehavSymp\\d+|PsyGrowthList\\d+|NegPsyEffList\\d+|PsychoBlissList\\d+|Gate$'
    , names(raw_survey_results.df)
    )]    
}

# Exclude columns that also contain "Questiontime" before "Gate"
extract.df <- extract.df[,!grepl("Questiontime", names(extract.df))]

# Encrypt each value in the "token" column using sha256
extract.df$id <- sapply(extract.df$token, function(x) substr(digest(x, algo = "sha256"), 0, 8))
extract.df <- extract.df[order(extract.df$id),]

drops <- c("token")
extract.df <- extract.df[ , !(names(extract.df) %in% drops)]


#
# Convert all raw answer codes from LimeSurvey to the appropriate Likert indicator
#

likert.questions <- grepl('MysticalSymptoms\\d+|PersonalandPsychic\\d+|TalentsSymptoms\\d+|PsyPhyList\\d+|PsychicSymptoms\\d+|PEInvMovSymptoms\\d+|PEFeelaSenseList\\d+|PEIlloDisSymptoms\\d+|PEOtherBehavSymp\\d+|PsyGrowthList\\d+|NegPsyEffList\\d+|PsychoBlissList\\d+',
                          names(extract.df))

likert.names <- names(extract.df[,likert.questions])

extract.df$Sex <- factor(extract.df$Sex)
# extract.df$Sex <- factor(extract.df$Sex, levels = c('A1', 'A2', 'A3'), labels = c('female', 'male', 'intersex'))

likert.levels <- c('Not at all', 'Very Weak/low intensity', 'Weak', 'Moderate', 'Strong', 'Very strong/high intensity')

# 'Not at all', 'Very Weak/low intensity', 'Weak', 'Moderate', 'Strong', 'Very strong/high intensity'
require(plyr)
extract.df[,likert.names] <- lapply(extract.df[,likert.names], function(x) {
  x <- mapvalues(x,
            # from=c("L001","L002","L003", "L004", "L005", "L006"),
            from=likert.levels,
            to=c(1, 2, 3, 4, 5, 6)
  )

  # Convert the likert columns to an ordered factor
  x <- ordered(x)
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
names(extract.df)[names(extract.df) == 'Country'] <- 'country'

# Rename the gate questions
names(extract.df)[names(extract.df) == 'PEGate'] <- 'pe.gate'
names(extract.df)[names(extract.df) == 'PEInvMovGate'] <- 'pe.invmov.gate'
names(extract.df)[names(extract.df) == 'PEFeelandSensGate'] <- 'pe.sensation.gate'
names(extract.df)[names(extract.df) == 'PEIlloDisGate'] <- 'pe.negphysical.gate'
names(extract.df)[names(extract.df) == 'PsyGrowthGate'] <- 'psygrowth.gate'
names(extract.df)[names(extract.df) == 'NegPsyEffGate'] <- 'negpsych.gate'
names(extract.df)[names(extract.df) == 'PsyBlissGate'] <- 'psybliss.gate'

# 'Not at all', 'Very Weak/low intensity', 'Weak', 'Moderate', 'Strong', 'Very strong/high intensity'
matching_gate <- grepl("*.\\.gate", names(extract.df))
require(plyr)
extract.df[,matching_gate] <- lapply(extract.df[,matching_gate], function(x) {
  x <- mapvalues(x,
                 from=c("Yes", "No"),
                 to=c(1, 2)
  )
  return(x)
})

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

# Data Cleanup ----

# Remove rows with age < 18
print(paste("Initial row count before age cleanup is", nrow(extract.df)))

# Count how many rows have age < 18
under_18_count <- nrow(extract.df[extract.df$age < 18, ])
print(paste(under_18_count, "rows have age < 18 and will be removed."))

# Remove rows with age < 18
extract.df <- extract.df[extract.df$age >= 18, ]
print(paste("After removing rows with age < 18, the row count is", nrow(extract.df)))


## Clean up ungated / primary questions ----
grepmatch = "mystical\\d+|spiritual\\d+|psyphys\\d+|psychic\\d+|talents\\d+"

matching_cols <- grepl(grepmatch, names(extract.df))

# Remove rows with any NA values in the matched columns
cleaned_df <- extract.df[complete.cases(extract.df[, matching_cols]), ]

nrow(cleaned_df)

extract.df <- cleaned_df

# For the ungated questions, code any NA values with 1
matching_cols <- grep(grepmatch, names(extract.df), value = TRUE)
extract.df[matching_cols][is.na(extract.df[matching_cols])] <- 1

extract.df$pe.gate <- ifelse(extract.df$pe.gate == 2, 0, extract.df$pe.gate)
extract.df$pe.invmov.gate <- ifelse(extract.df$pe.invmov.gate == 2, 0, extract.df$pe.invmov.gate)
extract.df$pe.sensation.gate <- ifelse(extract.df$pe.sensation.gate == 2, 0, extract.df$pe.sensation.gate)
extract.df$pe.negphysical.gate <- ifelse(extract.df$pe.negphysical.gate == 2, 0, extract.df$pe.negphysical.gate)
extract.df$psygrowth.gate <- ifelse(extract.df$psygrowth.gate == 2, 0, extract.df$psygrowth.gate)
extract.df$negpsych.gate <- ifelse(extract.df$negpsych.gate == 2, 0, extract.df$negpsych.gate)
extract.df$psybliss.gate <- ifelse(extract.df$psybliss.gate == 2, 0, extract.df$psybliss.gate)

extract.df.copy <- extract.df
rownames(extract.df.copy) <- NULL # Don't need these
dput(extract.df.copy, file = "data/ses-data.txt")
write.csv(extract.df.copy, file = "data/ses-data.csv", quote = FALSE, row.names = FALSE)

return(extract.df)

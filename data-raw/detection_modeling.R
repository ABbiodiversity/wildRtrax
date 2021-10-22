

#-----------------------------------------------------------------------------------------------------------------------

# Title: Detection probability modeling - API example.
# Description: Using data from ABMI's Ecosystem Health program (2015-2020) for five target species, detection
#              probability is estimated and model results are made available via an API.
# Authors: Richard Hedley, Marcus Becker, Alex MacPhail
# Date: 2020-11-28

#-----------------------------------------------------------------------------------------------------------------------

# Path to Google Drive
root <- "/volumes/GoogleDrive/Shared drives/wildRtrax/"

# Attach packages
library(unmarked)

# Load data
data <- read.csv(paste0(root, "data/APPENDED_REPORT.csv"), stringsAsFactors = FALSE)

#-----------------------------------------------------------------------------------------------------------------------

# Data cleaning

# Get all 3-minute surveys carried out between 4am and 6:59am. I am discarding 1-minute surveys
# and longer surveys for simplicity, with the idea that 3-minute surveys can be truncated to 1 or
# 2 minutes for this visualization.

table(data$method)
data=data[data$method %in% c('3m 1SPM','3m 1SPR','1m 1SPM','10m 1SPM','3m1SPM+7mVS'),]

data$hour=sapply(strsplit(data$recording_time, ':'), '[[', 1)

table(data$hour) # Hardly any surveys between 4 and 5am

# Select 5am to 7:59am then.
data=data[data$hour %in% c('00', '02', '03', '04', '05', '06', '07', '08', '22'),]

# Select recordings in late May to early July.
data$julian=as.POSIXlt(data$recording_date)$yday+1

# May 25 to July 15 is julian date 145 to 196
data=data[data$julian %in% as.character(90:220),]

# Use file prefix to define station.
data$prefix = sapply(strsplit(data$original_file_name, '_'), '[[', 1)

# Check if prefix is the same as organization-location.
head(data$prefix, 20)
head(paste0(data$organization, '-', data$location), 20) # not always... Leading zeroes removed, and also OG data included

# Use prefix.
sort(table(data$prefix))

files=unique(data$original_file_name)

data$survey.id=paste0(data$original_file_name, '_',data$transcriber)
length(unique(data$survey.id))
length(unique(data$original_file_name))
# More surveys than file names, meaning some files were transcribed more than once.
# For those files, randomly select one.

data$keep=TRUE

i=1

for(i in 1:length(files)) {
  if(length(unique(data$transcriber[data$original_file_name == files[i]]))>1) {
    sub=data[data$original_file_name == files[i],]
    Obs=sample(unique(sub$transcriber), 1) # Randomly select one transcriber.
    sub$keep[sub$transcriber==Obs]=TRUE # Keep data from that transcriber.
    sub$keep[sub$transcriber!=Obs]=FALSE # Discard otherwise.
    data$keep[data$original_file_name == files[i]]=sub$keep
  }
  if(i %% 1000 == 0) print(i)
}
rm(list=c('sub', 'i', 'Obs', 'files'))

#Filter out duplicate surveys.
data=data[data$keep,]

#Select data with light wind, rain, and noise.
table(data$wind)
data=data[data$wind %in% c(0:2),]
table(data$rain)
data=data[data$rain %in% c(0:2),]
table(data$noise)
data=data[data$noise %in% c(0:2),]

# How many surveys per location? Going to want at least 4.
SurveyCount=data.frame(station=unique(data$prefix), count=NA, stringsAsFactors=F)
i=1
for(i in 1:nrow(SurveyCount)) {
  S=SurveyCount$station[i]
  sub=data[data$prefix==S,]
  SurveyCount$count[SurveyCount$station==S]=length(unique(sub$survey.id))
  if(i %% 100 == 0) print(i)
}

rm(list=c('i', 'S', 'sub'))

table(SurveyCount$count)
SurveyCount$keep = SurveyCount$count>=4
c = max(SurveyCount$count)
SurveyCount=SurveyCount[SurveyCount$keep,]

data = data[data$prefix %in% SurveyCount$station,]
# Note that a lot of stations got removed, probably because of filtering out recordings with high wind, rain, and noise.
# Sample size can be increased substantially (probably more than twofold, if these criteria are more liberal).

# Now we have 3-minute surveys in peak breeding season, near dawn, with good conditions, and one transcription per file.

#-----------------------------------------------------------------------------------------------------------------------

# Occupancy Modeling

# Simple model: occupancy rate with no covariates, and detection probability with no covariates.
# The idea is to just give a measure of detection probability, with survey length = 1, 2, or 3 minutes.

# Target species: Ovenbird, Clay-Colored Sparrow, Olive-Sided Flycatcher, Tennessee Warbler, White-Throated Sparrow
species = c('GGOW','GHOW','NSWO','SEOW','WOLF','TEWA','ALFL','OSFL','WTSP','CONI','OVEN','SWTH','LISP','YRWA','CCSP')

for(i in 1:length(species)) {
  Sp=species[i]
  # Number of detections for this species.
  sum(data$species_code == Sp)

  # Build the occupancy frame. Will be r x 3c where r = number of stations, c is 14, the max number of surveys.
  # The first c columns are 1-minute surveys, second c are 2-minute, third c are 3-minute.

  frame = matrix(NA, nrow=nrow(SurveyCount), ncol=c*3)
  row.names(frame) = SurveyCount$station

  for(j in 1:nrow(frame)) {
    S = row.names(frame)[j]
    sub = data[data$prefix == S,]
    # Determine in which surveys the species were detected in first, second, third minute.
    detected1min = sub[sub$species_code == Sp & sub$min0_start!="" & sub$species_individual_name == 1,]
    detected2min = sub[sub$species_code == Sp & sub$min1_start!="" & sub$species_individual_name == 1,]
    detected3min = sub[sub$species_code == Sp & sub$min2_start!="" & sub$species_individual_name == 1,]

    surveys = unique(sub$original_file_name)
    # Figure out minute-by-minute detection history, then
    detection_history1 = surveys %in% detected1min$original_file_name
    detection_history1 = as.numeric(detection_history1)
    detection_history2 = surveys %in% detected2min$original_file_name
    detection_history2 = pmax(detection_history1, detection_history2)
    detection_history3 = surveys %in% detected3min$original_file_name
    detection_history3 = pmax(detection_history2, detection_history3)

    # Pad with NA if needed.
    if(length(detection_history1)<c) {
      detection_history1[(length(surveys)+1):c]=NA
      detection_history2[(length(surveys)+1):c]=NA
      detection_history3[(length(surveys)+1):c]=NA
    }
    frame[j,] = as.numeric(c(detection_history1, detection_history2,detection_history3))
  }

  y1 = frame[,1:c]
  y2 = frame[,(1:c)+c]
  y3 = frame[,(1:c)+2*c]

  occ1 <- unmarkedFrameOccu(y = y1)
  print('occ1 success!')
  occ2 <- unmarkedFrameOccu(y = y2)
  print('occ2 success!')
  occ3 <- unmarkedFrameOccu(y = y3)
  print('occ3 success!')

  fm1 <- occu(~1 ~1, occ1)
  print('Model 1 success!')
  fm2 <- occu(~1 ~1, occ2)
  print('Model 2 success!')
  fm3 <- occu(~1 ~1, occ3)
  print('Model 3 success!')

  assign(paste0(Sp, '1'),fm1)
  assign(paste0(Sp, '2'),fm2)
  assign(paste0(Sp, '3'),fm3)

  rm(list = c(paste0('fm', 1:3), 'frame', paste0('occ', 1:3), 'sub', paste0('y', 1:3),
              paste0('detection_history',1:3), paste0('detected', 1:3, 'min')))
}

for(i in 1:length(species)) {
  x1=get(paste0(species[i], 1))
  x2=get(paste0(species[i], 2))
  x3=get(paste0(species[i], 3))
  print(paste(species[i], '1 minute occupancy rate =',round(backTransform(x1, 'state')@estimate,2)))
  print(paste(species[i], '1 minute detection probability =',round(backTransform(x1, 'det')@estimate,2)))

  print(paste(species[i], '2 minute occupancy rate =',round(backTransform(x2, 'state')@estimate,2)))
  print(paste(species[i], '2 minute detection probability =',round(backTransform(x2, 'det')@estimate,2)))

  print(paste(species[i], '3 minute occupancy rate =',round(backTransform(x3, 'state')@estimate,2)))
  print(paste(species[i], '3 minute detection probability =',round(backTransform(x3, 'det')@estimate,2)))
}

#-----------------------------------------------------------------------------------------------------------------------

# Save models

library(purrr)

names <- list(sp = species, mod = c(1, 2, 3)) %>%
  cross() %>%
  map(lift(paste0)) %>%
  unlist()

detection_models <- list(GGOW1, GGOW2, GGOW3, GHOW1, GHOW2, GHOW3, SEOW1, SEOW2, SEOW3, NSWO1, NSWO2, NSWO3,
                         OVEN1, OVEN2, OVEN3, OSFL1, OSFL2, OSFL3, CCSP1, CCSP2, CCSP3, TEWA1, TEWA2, TEWA3,
                         ALFL1, ALFL2, ALFL3, LISP1, LISP2, LISP3, SWTH1, SWTH2, SWTH3, YRWA1, YRWA2, YRWA3,
                         CONI1, CONI2, CONI3, WOLF1, WOLF2, WOLF3, WTSP1, WTSP2, WTSP3) %>%
  set_names(names)

# Save models in data/
usethis::use_data(detection_models, overwrite = TRUE)

# Pin model outputs to repository
pins::pin(x = models,
          name = "pins/detection_modeling/models",
          description = "Detection modeling for fifteen species: OVEN, OSFL, CCSP, TEWA, WTSP, ALFL, LISP, SWTH, YRWA, CONI, GGOW, GHOW, SEOW, NSWO and WOLF",
          board = "github")

#-------------------------------------------------------------------------------------------------------------------------

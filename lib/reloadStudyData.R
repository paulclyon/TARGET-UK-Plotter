# reloadStudyData.R - functions to load/reload study data from Castor
# 
# The castor_api and castor_open_api globals needs to be present and set (castorAPI.R)


# Return an array of study name strings
getStudyNames <- function()
{
  logger(">> IN getStudyNames ")
  if (length(castor_api)==0)
  {
    logger("Nothing to do here until we can get into the API...")
    return(NULL)
  }
  
  # Check castor_api is also not atomic
  if (is.atomic(castor_api))
  {
    logger("Castor API has length but is not atomic, something is wrong, cannot get study names...")
    return(NULL)
  }
  else
  {
    studies <- castor_api$getStudies()
  }
  
  logger("<< OUT getStudyNames ")
  
  # Check its not atomic, if it is we can't use $name
  if (is.atomic(studies))
  {
    return(NULL)
  }
  else
  {
    return(studies$name)
  }
}

# Find study by name and return index of it, if not found return 0
findStudyIndex <- function(studies, studyName, stopIfNotFound=F) {
  index <- which(studies$name == studyName)
  if (length(index) == 0) {
    msg <- paste("Could not find study with name", studyName)
    if (stopIfNotFound==T) {
      stop(msg)
    }
    
    logger(msg)
    return(0)
  }

  logger(paste("Found study '", studyName,"' Index=", index, sep=''))
  return(index)
}

# FIXME - not sure this function is correct or indeed is ever called?
#getPatientData <- function(studyID) {
#  patientData <- (studyID)
#  return(patientData)
#}

# The basic (and slow!) Castor EDC method to get the data as a data.frame which used to come with metadata and field names in castoRedc v1.x but not in v2.x!
getStudyData <- function(studyID) {
  studyData <- castor_api$getStudyData(studyID)
  return(studyData)
}

# The basic (and slow!) Castor EDC method to get the field info as a data.frame which comes with metadata and field names, which we now need to use for castoRedc v2.x!
# We did not require to do this previously but we do now otherwise I cant get metadata!
getFieldData <- function(studyID) {
  fieldData <- castor_api$getFields(studyID)
  return(fieldData)
}

# Get the study name for a particular study ID, NA if not found
getStudyName <- function(id)
{
  if (length(castor_api)==0) {
    logger("Nothing to do here until we can get into the API...")
    return(NA)
  }
  studies <- castor_api$getStudies()
  index <- which(studies$study_id == id)
  if (length(index) == 0) {
    return(NA)
  }
  return(studies$name[[index]])
}

# Pull in the patient data...
# This method is historical only required for older versions of OxTAR-Plotter pre first TARGET-Plotter release
# It is tied in with different endpoints for this in castoRedc library 1.1.0 vs newer 2.1.0
# But actually you don't need it anyway as castor_api$getStudyData(studyID) has everything as a dataframe
# Everything but the field_id which is missing from the metadata in castoRedc v2.x...
# TARGET-Plotter was re-written so the getStudyData() method is used for everything and we no longer need to get patient data
pullPatientData <- function (studyID) {
  studyName <- getStudyName(studyID)
  logger(paste("Loading patient data for study ID '", studyID,"' (",studyName,")",sep=""))
  
  castorLibMajorVersion <- strtoi(substr(toString(packageDescription("castoRedc")$Version),1,1))
  if (castorLibMajorVersion < 2)
  {
    patientData <- castor_api$getRecords(studyID) 
    logger (paste ("Found ", nrow(patientData), " patients in study ID '",studyID,"' (",studyName,")", sep=""))
  }
  else # We have castoRedc >2.x.x, so use the updated library
  {
    # The newwer version of castor has different endpoints and this is how you get the participant data
    patientData <- castor_api$getParticipants("YOUR_STUDY_ID")
    logger (paste ("Found ", nrow(patientData), " patients in study ID '",studyID,"' (",studyName,")", sep=""))
  }
  return(patientData)
}

# Pull in the study data...
pullStudyData <- function (studyID) {
  studyName = getStudyName(studyID)
  logger(paste("Loading study data for study ID '", studyID,"' (",studyName,")...",sep=""))
  studyData <- getStudyData(studyID)
  logger(paste("Study data loaded successfully"))
  logger(paste ("Found ", length(studyData$Study$Participant_ID), " rows of data in studyID '...",studyID,"' (",studyName,")", sep=""))
  return(studyData)
}

# Pull in the study data via Open API...
pullStudyDataOpenAPI <- function (studyID) {
  studyName = getStudyName(studyID)
  logger(paste("Loading study data via Open API for study ID '", studyID,"' (",studyName,")...",sep=""))
  studyDataOpenAPI <- getStudyDataOpenAPI(studyID)
  logger(paste("Study data loaded successfully via Open API"))
  return(studyDataOpenAPI)
}

# reloadStudyData interrogates castor to load the study data.
# It sets the global values:
#
# patientData 
# studyData
reloadStudyData <- function(studyName)
{
  if (length(castor_api)==0) {
    logger("Nothing to do here until we can get into the API...")
    return()
  }
  
  if(is.na(studyName)) {
    logger("No study with the name '",studyName,"' - cannot load study...")
    return()
  }
  
  studies <- castor_api$getStudies()
  studyIndex <- findStudyIndex(studies, studyName)
  if (studyIndex==0)
  {
    logger(paste("Could not find study '", studyName, "' to load...",sep=""))
    return()
  }
  
  # Pull in the data for the TARGET study, note TARGET-Plotter no longer reqiures patientData and uses studyData only
  targetStudyID     <<- studies[["study_id"]][studyIndex]
  target_study_name <- studies[["name"]][studyIndex]
  #patientData      <<- pullPatientData(targetStudyID)
  studyData        <<- pullStudyData(targetStudyID)
}


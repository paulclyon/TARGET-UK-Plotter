## Functions which work on patientData

# Function to find a patient using ID and return the index number
# If not found it returns 0
getPatientIndex <- function(ptID, ignoreIfArchived = TRUE)
{
  index <- which(studyData$Study$Participant_ID == ptID & (ignoreIfArchived == FALSE | patientData$archived == FALSE))
  
  if (length(index) == 0) {
    return(0)
  }
  
  return(index)
}

# Return true only if the patient is found and the patient is archived, otherwise false
# This will only work with paientData from castoRedc v1.1.0 its been deprecated in later versions of the library
# FIXME fix for castoRedc v2.x
patientArchived <- function(ptID)
{
  archived <- patientData$archived[patientData$id == ptID]
  if (length(archived) == 0) {
    return(FALSE)
  }
  return(archived)
}

## Functions that primarily work on studyData
## studyData is loaded from reloadStudyData.R 

# Return true if the study is a valid TARGET study with at least one patient, false otherwise
# This function only works with castoRedc 2.1.0 or greater, not 1.1.0 as that would need colnames(studyData) instead
ifValidTARGETStudy <- function(stopIfNotValid = F) {
  if ("Participant_ID" %in% colnames(studyData$Study)) {
    if (length(studyData$Study$Participant_ID) > 0 ) {
      if ("pt_ref_count" %in% colnames(studyData$Study)) {
        return(TRUE)
      }
    }
  }
  # FIXME: Does this need to stop if not valid?
  return(FALSE)
}

# Return the indexed data otherwise return NA
# Note that NA may be returned if the field does not exist in the data
# or if stopIfNotFound = T the code will terminate with an error
# The issue with having stopIfNotFound = T as default is that even if the field exists but has no data,
# it is not returned in the studyData (despite being seen in the Excel export data from the web client)
# Note this is a new implementation based on castoRedc library version 2.1.0 and above and there is a
# key change; it is not the export data value that is returned like '0' or '1' instead it is the text
# equivalent as implemented in the Castor design "No", "Yes" and "Unknown"
getDataEntry <- function(field, index, stopIfNotFound = F) {
  if (field %in% colnames(studyData$Study)) {
    return(studyData$Study[[field]][index])
  } 
  
  if (stopIfNotFound == T) {
    msg = paste("Field '", field, "' not found in the data - stopping", sep = '')
    logger(msg)
    stop(msg)
  }
  else
  {
    #logger(paste("Field '", field, "' not found in the data, but this may be because data not populated rather than missing field", sep = ''))
  }
  return(NA)
}


# Set the indexed data to the value provided
# If stopIfNotFound = T the code will terminate with an error if the field does not exist, other will return NA
# It returns 0 if the set is successful

setDataEntry <- function(studyID, patientID, patientIndex, fieldName, newData, stopIfNotFound = T) {
  if (!fieldName %in% colnames(studyData))
  {
    if (stopIfNotFound == T) {
      msg = paste("Field '", fieldName, "' not found in the data - stopping", sep = '')
      logger(msg)
      stop(msg)
    }
    else
    {
      #logger(paste("Field '", fieldName, "' not found in the data, but this may be because data not populated rather than missing field", sep = ''))
      return(NA)
    }
  }
  else
  {
    logger(paste(">> Updating record patientID=",patientID, " field='",fieldName,"' old data='",studyData[[fieldName]][patientIndex],"' new data='",newData,"'", sep=""))
    
    # First update the local study data but this does not update the source Castor EDC data
    studyData[[fieldName]][patientIndex] = newData

    # To update the Castor EDC data use we can use the Open API interface, which is the only way I know to do it
    # This heavily borrows from this worked example:
    # https://git.lumc.nl/egjvonasmuth/castor-api-tutorial/-/blob/main/example_rapiclient.R?ref_type=heads

    fieldID <- getFieldIDForName(studyData,fieldName)
    updateStudyDataOpenAPI(studyID, patientID, fieldID, newData, "TARGET Plotter Update")
  }
  return(0)
}

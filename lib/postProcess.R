# Post-process the processed data
postProcessData <- function()
{
  # The as.Dates() work around is needed to set Dates as the column types otherwise if the first element in the column is NA, it is represented as just the number which is still the date but unredable to the human
  # This is important as this makes them all a Dates object which displays nicely in the tables - you can check the types of the data frame easily with str(survivalData)
  if (!is.null(rxdone_pt_list))
  {

    # First replacing any NA operator with 'unspecified'
    rxdone_operator1_list     <<- rxdone_operator1_list %>% replace_na("unspecified")
    rxdone_operator2_list     <<- rxdone_operator2_list %>% replace_na("unspecified")
    rxdone_operator3_list     <<- rxdone_operator3_list %>% replace_na("unspecified")
    rxdone_anaesthetist1_list <<- rxdone_anaesthetist1_list %>% replace_na("unspecified")
    rxdone_anaesthetist2_list <<- rxdone_anaesthetist2_list %>% replace_na("unspecified")
    rxdone_anaesthetist3_list <<- rxdone_anaesthetist3_list %>% replace_na("unspecified")
    
    # If the list is not empty
    rxDoneData <<- data.frame(
      ID = rxdone_pt_list,
      Gender = rxdone_sex_list,
      RefDate = asDateWithOrigin(rxdone_refdate_list),
      DTTDate = asDateWithOrigin(rxdone_dttdate_list),
      RxDate = asDateWithOrigin(rxdone_rxdate_list),
      Ref_DTT = as.numeric(rxdone_dtt_days_list),
      DTT_Rx = as.numeric(rxdone_dtt_rx_days_list),
      Ref_RxDone = as.numeric(rxdone_rx_days_list),
      Organs = rxdone_organ_list,
      Modality = rxdone_modality_list,
      Tariff = rxdone_tariff_list,
      Operator1 = rxdone_operator1_list,
      Operator2 = rxdone_operator2_list,
      Operator3 = rxdone_operator3_list,
      Anaesthetist1 = rxdone_anaesthetist1_list,
      Anaesthetist2 = rxdone_anaesthetist2_list,
      Anaesthetist3 = rxdone_anaesthetist3_list,
      ClockStopDaysPreDTT = as.numeric(rxdone_clockstop_days_predtt_list),
      ClockStopDaysPostDTT = as.numeric(rxdone_clockstop_days_postdtt_list),
      ClockStopWhy = rxdone_clockstop_reason_list,
      Postcode = rxdone_postcode_list,
      FreeText = rxdone_freetext_list
    )
    
  } else {
    rxDoneData <<- NA
  }
  
  if (!is.null(rxwait_pt_list)) {
    # If the list is not empty
    rxWaitData <<- data.frame(
      ID = rxwait_pt_list,
      RefDate = asDateWithOrigin(rxwait_ref_date_list),
      DTTDate = asDateWithOrigin(rxwait_dtt_date_list),
      ProvisionalRxDate = asDateWithOrigin(rxwait_provisional_rxdate_list),
      Ref_DTT = as.numeric(rxwait_dtt_days_list),
      DaysWaiting = as.numeric(rxwait_days_list),
      Organs = rxwait_organ_list,
      ClockStopDaysPreDTT = as.numeric(rxwait_clockstop_days_predtt_list),
      ClockStopDaysPostDTT = as.numeric(rxwait_clockstop_days_postdtt_list),
      ClockStopWhy = rxwait_clockstop_reason_list
    )
  } else {
    rxWaitData <<- NA
  }
  
  # This is just a list the different organ targets which have been referred or treated
  organFactors <<- levels(factor(append(rxdone_organ_list, rxwait_organ_list)))
  
  # Similar for Genders
  genderFactors <<- levels(factor(rxdone_sex_list))

  # Similarly for operators
  operator1Factors       <<- c("ALL",levels(factor(rxdone_operator1_list)))
  operator2Factors       <<- c("ALL",levels(factor(rxdone_operator2_list)))
  operator3Factors       <<- c("ALL",levels(factor(rxdone_operator3_list)))
  operatorAllFactors     <<- c(levels(factor(append(append(rxdone_operator1_list,rxdone_operator2_list),rxdone_operator3_list))))
  anaesthetist1Factors   <<- c("ALL",levels(factor(rxdone_anaesthetist1_list)))
  anaesthetist2Factors   <<- c("ALL",levels(factor(rxdone_anaesthetist2_list)))
  anaesthetist3Factors   <<- c("ALL",levels(factor(rxdone_anaesthetist3_list)))
  anaesthetistAllFactors <<- c(levels(factor(append(append(rxdone_anaesthetist1_list,rxdone_anaesthetist2_list),rxdone_anaesthetist3_list))))
  
  # Similarly for CCTAE grades
  cctaeGradeFactors      <<- c(levels(factor(aeData$Grade)))
  
  # See https://thriv.github.io/biodatasci2018/r-survival.html
  # Here Time is survival time in days (since first Rx)
  # Censoring status is 1=censored (could still be alive but we don't know), 2=dead
  if (length(survival_pt_list)==0)
  {
    survivalData <<- NA
    survivalFitSex <<- NA
    survivalFitOrgan <<- NA
    survivalPlotOrgan <<- NA
    survivalPlotSex <<- NA
  }
  else
  {
    survivalData <<- data.frame(
      ID = survival_pt_list,
      Gender = survival_sex_list,
      FirstRxDate = asDateWithOrigin(survival_first_rx_date),
      AgeOnFirstRx = survival_age_list,
      Organ = survival_organ_list,
      TimeLRF = local_recurrence_days_list/365.25,     # Time to local recurrence
      StatusLRF = local_recurrence_status_list,
      TimeLRFOS = lrf_os_survival_days_list/365.25,        # Local recurrence-free survival i.e. time to LR or Death, if NA they have not recurred or died
      StatusLRFOS = lrf_os_survival_status_list,
      TimeLRFCSS = lrf_cs_survival_days_list/365.25,        # Local recurrence-free survival i.e. time to LR or Death, if NA they have not recurred or died
      StatusLRFCSS = lrf_cs_survival_status_list,
      LastImagingDate = last_imaging_follow_up_list,
      FirstLocalRecurrenceDate = local_recurrence_date_list,
      LastKnownAlive = survival_last_alive_list,
      StatusOverallSurvival = survival_overall_status_list,    # Overall survival
      StatusCancerSpecificSurvival = survival_cancer_specific_status_list, # Cancer related survival
      TimeSurvival = survival_days_list/365.25,
      Deceased = survival_deceased_list,
      DeceasedDate = asDateWithOrigin(survival_deceased_date),
      CancerRelatedDeath = survival_deceased_related,
      LostToFU = survival_lost_to_fu,
      LostToFUDate = asDateWithOrigin(survival_lost_to_fu_date)
    )
  }
}

# This is a useful function to change variations of anaesthetists names to a single common identifier
# e.g. Joe_Bloggs, Joe, Mr. Bloggs > JBloggs
updateAnaesthetistNames <- function(studyID, oldNames, newName)
{
  # Iterate through patient records one by one to find the matching names...
  patientCount <- count(studyData$Study)$n
  for (i in 1:patientCount)
  {
    ptID <- studyData$Study$Participant_ID[i]
    pt_rx_count  <- getDataEntry("pt_rx_count", i)
    if (!is.na(pt_rx_count) && pt_rx_count >0 )
    {
      for (iRx in 1:pt_rx_count)
      {
        # Go through each of the primary, secondary and tertiary anaesthetists
        for (no in 1:3)
        {
          fieldName = paste("anaes_anaesthetist",no,"o_", as.integer(iRx), sep = "")
          anaesthetistString <- tolower(getDataEntry(fieldName, i))
          if (!is.na(anaesthetistString) && anaesthetistString %in% oldNames)
          {
            logger(paste("** Attempting to replace patientID=",ptID," record ",i,"/", patientCount, " '",anaesthetistString, "' with '",newName,"'", sep=""))
            setDataEntry(studyID, ptID, i, fieldName, newName)
            logger(paste("<< Completed replacing patientID=",ptID," record ",i,"/", patientCount, " '",anaesthetistString, "' with '",newName,"'", sep=""))
          }
        }
      }
    }
  }
}

# This is a useful function to change variations of operator names to a single common identifier
# e.g. Joe_Bloggs, Joe, Mr. Bloggs > JBloggs
updateOperatorNames <- function(studyID, oldNames, newName)
{
  # Iterate through patient records one by one to find the matching names...
  patientCount <- count(studyData$Study)$n
  for (i in 1:patientCount)
  {
    ptID <- studyData$Study$Participant_ID[i]
    pt_rx_count  <- getDataEntry("pt_rx_count", i)
    if (!is.na(pt_rx_count) && pt_rx_count >0 )
    {
      for (iRx in 1:pt_rx_count)
      {
        # Go through each of the primary, secondary and tertiary operators
        for (no in 1:3)
        {
          fieldName = paste("rx_operator",no,"o_", as.integer(iRx), sep = "")
          operatorString <- tolower(getDataEntry(fieldName, i))
          if (!is.na(operatorString) && operatorString %in% oldNames)
          {
            logger(paste("** Attempting to replace patientID=",ptID," record ",i,"/", patientCount, " '",operatorString, "' with '",newName,"'", sep=""))
            setDataEntry(studyID, ptID, i, fieldName, newName)
            logger(paste("<< Completed replacing patientID=",ptID," record ",i,"/", patientCount, " '",operatorString, "' with '",newName,"'", sep=""))
          }
        }
      }
    }
  }
}

# Get the tariff for the modality in the given organ
getTariffForRx <- function(organForRx, modalityForRx)
{
  GBP <- 0
  # Returns list of index rows for matching organs and matching modalities
  matches <- intersect(which(tariffCodes$Organ==organForRx), which(tariffCodes$Modality == modalityForRx))
  if (length(matches) > 0)
  {
    # We can only return the first match... FIXME the CS Score is not yet used
    GBP <- tariffCodes$Tariff[matches[1]]
  }
  return (GBP)
}

# Calculate total tariff for the filtered Rx data supplied
calculateTotalTariff <- function(rxData)
{
  totalTariff <- 0
  for (j in 1:nrow(rxData))
  {
    totalTariff <- totalTariff + rxData$Tariff[j]
  }
  return(totalTariff) 
}

addDataIntegrityError <- function(ptID = NA, refID = NA, date = NA, organs = NA, errorStr = NA)
{
  logger(paste(" >>> Data integrity issue Pt=",ptID," Ref=",refID," Date=",date," (",organs,") >>>:",errorStr, sep=""), TRUE)
  dataIntegrity.df <<- rbind(dataIntegrity.df,data.frame(PtID=ptID, RefID=refID, Date=date, Organs=organs, Error=errorStr))
}

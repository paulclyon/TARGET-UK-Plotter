# So when the Diagnosis1o is NA or blank, to avoid it getting lost as a subtype in later analyses change NA as follows:
# e.g. Kidney/NA becomes Kidney/Kidney: Unspecified
fixDx1o <- function(org, dx) {
  org <- as.character(org)
  dx  <- as.character(dx)
  dx <- trimws(dx)
  dx[is.na(dx) | dx == ""] <- paste0(org[is.na(dx) | dx == ""], ": Unspecified")
  dx
}

# Post-process the processed data
postProcessData <- function()
{
  logger("Performing post processing analysis...")
  
  # The as.Dates() work around is needed to set Dates as the column types otherwise if the first element in the column is NA, it is represented as just the number which is still the date but unredable to the human
  # This is important as this makes them all a Dates object which displays nicely in the tables - you can check the types of the data frame easily with str(cancerPerPatientData)
  if (!is.null(rxdone_pt_list))
  {
    
    # Replacing any NA operator with 'unspecified'
    rxdone_operator1_list     <<- rxdone_operator1_list %>% replace_na("unspecified")
    rxdone_operator2_list     <<- rxdone_operator2_list %>% replace_na("unspecified")
    rxdone_operator3_list     <<- rxdone_operator3_list %>% replace_na("unspecified")
    rxdone_anaesthetist1_list <<- rxdone_anaesthetist1_list %>% replace_na("unspecified")
    rxdone_anaesthetist2_list <<- rxdone_anaesthetist2_list %>% replace_na("unspecified")
    rxdone_anaesthetist3_list <<- rxdone_anaesthetist3_list %>% replace_na("unspecified")
    
    # If the list is not empty
    rxDoneData <<- data.frame(
      ID            = rxdone_pt_list,
      Gender        = rxdone_sex_list,
      RefDate       = asDateWithOrigin(rxdone_refdate_list),
      DTTDate       = asDateWithOrigin(rxdone_dttdate_list),
      RxDate        = asDateWithOrigin(rxdone_rxdate_list),
      Ref_DTT       = as.numeric(rxdone_dtt_days_list),
      DTT_Rx        = as.numeric(rxdone_dtt_rx_days_list),
      Ref_RxDone    = as.numeric(rxdone_rx_days_list),
      Organ         = rxdone_organ_list,
      TumourSizes   = rxdone_tumour_size_list,
      TumourCount   = as.numeric(rxdone_tumour_count_list),
      DiagnosisType = rxdone_diagnosis_type_list,
      Diagnosis1o   = rxdone_diagnosis_1o_list,
      Diagnosis2o   = rxdone_diagnosis_2o_list,
      DiagnosisBn.  = rxdone_diagnosis_bn_list,
      DiagnosisUn   = rxdone_diagnosis_un_list,
      RxModality    = rxdone_modality_list,
      Tariff        = rxdone_tariff_list,
      Operator1     = rxdone_operator1_list,
      Operator2     = rxdone_operator2_list,
      Operator3     = rxdone_operator3_list,
      Anaesthetist1 = rxdone_anaesthetist1_list,
      Anaesthetist2 = rxdone_anaesthetist2_list,
      Anaesthetist3 = rxdone_anaesthetist3_list,
      ClockStopDaysPreDTT = as.numeric(rxdone_clockstop_days_predtt_list),
      ClockStopDaysPostDTT = as.numeric(rxdone_clockstop_days_postdtt_list),
      ClockStopWhy  = rxdone_clockstop_reason_list,
      Postcode      = rxdone_postcode_list,
      FreeText      = rxdone_freetext_list
    )
    
    # e.g. Kidney/NA becomes Kidney/Kidney: Unspecified
    rxDoneData <<- rxDoneData |> mutate(Diagnosis1o = fixDx1o(Organ, Diagnosis1o))
    
  } else {
    rxDoneData <<- NA
  }
  
  if (!is.null(rxwait_pt_list)) {
    # If the list is not empty
    rxWaitData <<- data.frame(
      ID = rxwait_pt_list,
      RefDate = asDateWithOrigin(rxwait_ref_date_list),
      DTTDate = asDateWithOrigin(rxwait_dtt_date_list),
      TciDate = asDateWithOrigin(rxwait_tci_date_list),
      TciStatus = rxwait_tci_status_list,
      Ref_DTT = as.numeric(rxwait_dtt_days_list),
      DaysWaiting = as.numeric(rxwait_days_list),
      Organ = rxwait_organ_list,
      ClockStopDaysPreDTT = as.numeric(rxwait_clockstop_days_predtt_list),
      ClockStopDaysPostDTT = as.numeric(rxwait_clockstop_days_postdtt_list),
      ClockStopWhy = rxwait_clockstop_reason_list
    )
  } else {
    rxWaitData <<- NA
  }
  
  # These are lists of the 1o, 2o and benign diagnoses
  diagnosis_1o_Factors <<- sort(unique(na.omit(rxDoneData$Diagnosis1o)))
  diagnosis_2o_Factors <<- sort(unique(na.omit(rxDoneData$Diagnosis2o)))
  diagnosis_bn_Factors <<- sort(unique(na.omit(rxDoneData$DiagnosisBn))) # Benign
  diagnosis_un_Factors <<- sort(unique(na.omit(rxDoneData$DiagnosisUn))) # Unknown
  
  #diagnosis_1o_Factors <<- levels(factor(rxdone_diagnosis_1o_list))
  #diagnosis_bn_Factors <<- levels(factor(rxdone_diagnosis_bn_list)) # Benign
  ##diagnosis_2o_Factors <<- levels(factor(rxdone_diagnosis_2o_list))
  #diagnosis_un_Factors <<- levels(factor(rxdone_diagnosis_un_list)) # Unknown
  
  # This is a list the different organ targets which have been referred or treated
  organFactors <<- levels(factor(append(rxdone_organ_list, rxwait_organ_list)))
  
  # This is a list the different modalities treated
  modalityFactors <<- levels(factor(rxdone_modality_list))
  
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
    cancerPerPatientData <<- NA
    survivalFitSex <<- NA
    survivalFitOrgan <<- NA
    survivalPlotOrgan <<- NA
    survivalPlotSex <<- NA
    cancerPerLesionData <<- NA
  }
  else
  {
    allData <<- data.frame(
      ID                           = survival_pt_list,
      Gender                       = survival_sex_list,
      FirstRxDate                  = asDateWithOrigin(survival_first_rx_date),
      AgeOnFirstRx                 = survival_age_list,
      Organ                        = survival_organ_list,
      MaxTumourSize                = survival_max_tumour_size_list,
      TumourSizes                  = survival_tumour_size_list,
      RxModalities                 = survival_rx_modalities,
      DiagnosisType                = survival_diagnosis_type_list,
      Diagnosis1o                  = survival_diagnosis_1o_list,
      Diagnosis2o                  = survival_diagnosis_2o_list,
      DiagnosisBn                  = survival_diagnosis_bn_list,
      DiagnosisUn                  = survival_diagnosis_un_list,
      TimeLTPF                     = ltp_days_list/365.25,                     # Time to LTP
      StatusLTPF                   = ltp_status_list,
      TimeLTPFOS                   = ltpf_os_survival_days_list/365.25,        # LTP-free survival i.e. time to LR or Death, if NA they have not recurred or died
      StatusLTPFOS                 = ltpf_os_survival_status_list,
      TimeLTPFCSS                  = ltpf_cs_survival_days_list/365.25,        # LTP-free survival i.e. time to LR or Death, if NA they have not recurred or died
      StatusLTPFCSS                = ltpf_cs_survival_status_list,
      LastImagingDate              = asDateWithOrigin(last_imaging_follow_up_list),
      FirstLTPDate                 = asDateWithOrigin(ltp_date_list),
      LTPCountMax                  = ltp_count_max_list,
      LastKnownAlive               = asDateWithOrigin(survival_last_alive_list),
      StatusOverallSurvival        = survival_overall_status_list,    # Overall survival
      StatusCancerSpecificSurvival = survival_cancer_specific_status_list, # Cancer related survival
      TimeSurvival                 = survival_days_list/365.25,
      Deceased                     = survival_deceased_list,
      DeceasedDate                 = asDateWithOrigin(survival_deceased_date),
      CancerRelatedDeath           = survival_deceased_related,
      LostToFU                     = survival_lost_to_fu,
      LostToFUDate                 = asDateWithOrigin(survival_lost_to_fu_date)
    )
    
    # There was an issue ...
    # When computing TimeLTPF, use a cascade of fallback dates for censoring
    # Use a proper Date fallback chain
    censorDate <- dplyr::coalesce(
      allData$LastImagingDate,
      allData$LastKnownAlive,
      allData$DeceasedDate,
      allData$FirstRxDate   # final fallback: censor at time 0 if no follow-up exists
    )
    eventOrCensorDate <- dplyr::coalesce(
      allData$FirstLTPDate,
      censorDate
    )
    allData$TimeLTPF <- as.numeric(difftime(
      eventOrCensorDate,
      allData$FirstRxDate,
      units = "days"
    )) / 365.25
    allData$StatusLTPF <- ifelse(!is.na(allData$FirstLTPDate), 2L, 1L)
    
    # e.g. Kidney/NA becomes Kidney/Kidney: Unspecified
    allData <<- allData |> mutate(Diagnosis1o = fixDx1o(Organ, Diagnosis1o))
    
    # Now clean up the data into individual cancer and benign tables
    allData$Organ <- as.character(allData$Organ)
    allData$Diagnosis1o <- as.character(allData$Diagnosis1o)     # Force Diagnosis1o to be a plain character vector
    
    # Replace missing / blank primary diagnosis with "<Organ>: Unspecified"
    ix <- is.na(allData$Diagnosis1o) | trimws(allData$Diagnosis1o) == ""
    allData$Diagnosis1o[ix] <- paste0(allData$Organ[ix], ": Unspecified")
    cancerPerPatientData <<- allData[!is.na(allData$DiagnosisType) & allData$DiagnosisType != "B", ]
    cancerPerPatientData <<- cancerPerPatientData[, !colnames(cancerPerPatientData) %in% c("DiagnosisBn")]
    
    # Rebuild subtype lists from the cleaned table
    diagnosis_1o_Factors <<- sort(unique(na.omit(cancerPerPatientData$Diagnosis1o)))
    diagnosis_2o_Factors <<- sort(unique(na.omit(cancerPerPatientData$Diagnosis2o)))
    diagnosis_un_Factors <<- sort(unique(na.omit(cancerPerPatientData$DiagnosisUn)))
    
    benignData <<- allData[!is.na(allData$DiagnosisType) & allData$DiagnosisType == "B", ]
    benignData <<- benignData[, !colnames(benignData) %in% c("Diagnosis1o", "Diagnosis2o", "LTPCountMax", "TimeLTPF", "StatusLTPF", "TimeLTPFOS", "StatusLTPFOS", "TimeLTPFCSS", "StatusLTPFCSS")]
    
    # Build the per-lesion dataset for per-lesion LTP Kaplan-Meier analysis
    # Unlike cancerPerPatientData which is one row per patient, this is one row per referral episode
    # i.e. each treated malignant referral is an independent analytical unit with its own LTP clock
    # Base dataset is all malignant referral episodes from rxDoneData
    # LTP events are joined from ltp_perlesion lists where ltp.list has been populated in the EDC
    # Episodes without a matched LTP event are censored at the patient's last imaging follow-up date
    # This mirrors how landmark ablation studies e.g. COLLISION analyse LTP - per treated lesion
    if (is.data.frame(rxDoneData) && nrow(rxDoneData) > 0)
    {
      # Start with all treated referral episodes and extract the columns we need
      # rxDoneData$ID is in the format 'PtID-RxNo' e.g. '001-3'
      rxEpisodes       <- rxDoneData[, c("ID", "RxDate", "Organ", "DiagnosisType",
                                         "Diagnosis1o", "Diagnosis2o", "DiagnosisUn",
                                         "TumourCount", "RxModality", "Gender")]
      rxEpisodes$PtID  <- sub("-[0-9]+$", "", rxEpisodes$ID)        # Extract patient ID
      rxEpisodes$RxNo <- as.integer(sub(".*-", "", rxEpisodes$ID))  # Extract referral number
      
      # Remove benign episodes — per-lesion LTP analysis is for malignant only
      rxEpisodes <- rxEpisodes[!is.na(rxEpisodes$DiagnosisType) & rxEpisodes$DiagnosisType != "B", ]
      
      # Build the per-lesion LTP event table from parsed ltp.list entries
      # Only populated where the ablationist has confirmed which specific lesion(s) had RD/LTP
      if (length(ltp_perlesion_ptid_list) > 0)
      {
        ltpPerLesion <- data.frame(
          PtID     = ltp_perlesion_ptid_list,
          RxNo     = as.integer(ltp_perlesion_rxno_list),
          LesionNo = as.integer(ltp_perlesion_id_list),
          LTPDate  = asDateWithOrigin(ltp_perlesion_date_list),
          LTPCount = as.integer(ltp_perlesion_count_list)
        )
      }
      else
      {
        # No ltp.list entries yet — empty event table, all episodes will be censored
        # This is expected until ltp.list is populated in the EDC imaging follow-up matrix
        ltpPerLesion <- data.frame(
          PtID      = character(0),
          RxNo      = integer(0),
          LesionNo  = integer(0),
          LTPDate   = as.Date(character(0)),
          LTPCount = integer(0)
        )
      }
      
      nTumoursVec <- suppressWarnings(as.integer(rxEpisodes$TumourCount))
      validIdx    <- which(!is.na(nTumoursVec) & nTumoursVec >= 1L)
      
      # Repeat each valid episode's row the right number of times, and generate
      # LesionNo = 1..n for each episode in one vectorised step (no rbind/do.call at all)
      repIdx <- rep(validIdx, nTumoursVec[validIdx])
      rxLesions <- rxEpisodes[repIdx, ]
      rxLesions$LesionNo <- sequence(nTumoursVec[validIdx])
      rownames(rxLesions) <- NULL
      
      # TumourSize still needs per-episode alignment (sizes come from a list-column),
      # but unlist(lapply(...)) here just concatenates vectors - it's iterative in C,
      # not subject to the do.call(rbind,...) stack issue at all
      rxLesions$TumourSize <- unlist(lapply(validIdx, function(i) {
        sizes <- rxdone_tumour_size_mm[[i]]
        n <- nTumoursVec[i]
        if (length(sizes) < n) sizes <- c(sizes, rep(NA_real_, n - length(sizes)))
        sizes[seq_len(n)]
      }))
      
      # Log a single data integrity entry per skipped episode (missing/zero TumourCount)
      skippedEpisodes <- rxEpisodes[is.na(suppressWarnings(as.integer(rxEpisodes$TumourCount))) |
                                      suppressWarnings(as.integer(rxEpisodes$TumourCount)) < 1L, ]
      
      if (nrow(skippedEpisodes) > 0) {
        for (i in seq_len(nrow(skippedEpisodes))) {
          addDataIntegrityError(ptID   = skippedEpisodes$PtID[i],
                                refID  = skippedEpisodes$ID[i],
                                date   = skippedEpisodes$RxDate[i],
                                organ  = skippedEpisodes$Organ[i],
                                errorStr = "Episode skipped from per-lesion LTP analysis — no TumourCount recorded in Rx table.")
        }
      }
      
      # Join LTP events onto all referral episodes
      # all.x = TRUE keeps all episodes; LTPDate will be NA for episodes without a linked LTP event i.e. censored
      cancerPerLesionData <<- merge(
        rxLesions, # This is needed rather than rxEpisodes as we are merging on three ID fields and they need to be present in all tables
        ltpPerLesion,
        by = c("PtID", "RxNo", "LesionNo"),
        all.x = TRUE
      )
      
      # After merging ltpPerLesion onto rxLesions, deduplicate to one row per lesion
      # keeping the earliest LTP event (LTPCount == 1) where multiple exist
      # This is due to parseLTPLesionIDs() generating one row per LTP event rather than one row per lesion, and we get duplicate factors on merge
      cancerPerLesionData <<- cancerPerLesionData[
        order(cancerPerLesionData$PtID, 
              cancerPerLesionData$RxNo, 
              cancerPerLesionData$LesionNo,
              cancerPerLesionData$LTPDate,   # NA sorts last
              na.last = TRUE), ]
      cancerPerLesionData <<- cancerPerLesionData[
        !duplicated(cancerPerLesionData[, c("PtID", "RxNo", "LesionNo")]), ]
      
      # Join last imaging follow-up date from cancerPerPatientData for censoring episodes without LTP
      lastImaging <- cancerPerPatientData[, c("ID", "LastImagingDate")]
      cancerPerLesionData <<- merge(
        cancerPerLesionData,
        lastImaging,
        by.x = "PtID",
        by.y = "ID",
        all.x = TRUE
      )
      
      # Calculate time-to-LTP in years from the Rx date of that specific referral episode
      # Where LTP occurred use LTPDate
      # Where no LTP, censor at LastImagingDate if available
      # Where neither is available (no imaging follow-up yet), censor at RxDate i.e. time = 0
      # — these patients are known to have been treated but have no follow-up data yet,
      #   they are retained in the denominator rather than silently excluded
      cancerPerLesionData$TimeLTPEpisode <<- as.numeric(ifelse(
        !is.na(cancerPerLesionData$LTPDate),
        as.numeric(difftime(cancerPerLesionData$LTPDate,           cancerPerLesionData$RxDate, units = "days"), units = "days"),
        ifelse(
          !is.na(cancerPerLesionData$LastImagingDate),
          as.numeric(difftime(cancerPerLesionData$LastImagingDate, cancerPerLesionData$RxDate, units = "days"), units = "days"),
          0  # No follow-up yet — censor at time of treatment
        )
      )) / 365.25
      
      # Status: 2 = LTP event, 1 = censored (no LTP confirmed at this episode)
      cancerPerLesionData$StatusLTPEpisode <<- ifelse(!is.na(cancerPerLesionData$LTPDate), 2, 1)
      
      # Only remove rows where time is negative (data integrity issue e.g. LTPDate before RxDate)
      # Rows with TimeLTPEpisode = 0 are retained — these are censored at time of treatment
      cancerPerLesionData <<- cancerPerLesionData[
        !is.na(cancerPerLesionData$TimeLTPEpisode) & cancerPerLesionData$TimeLTPEpisode >= 0, ]
      
      # Get just the organs that have been used for cancer patients
      cancerOrganFactors <<- sort(unique(cancerPerPatientData$Organ[!is.na(cancerPerPatientData$Organ)]))
    }
    else
    {
      cancerPerLesionData <<- data.frame()
    }
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

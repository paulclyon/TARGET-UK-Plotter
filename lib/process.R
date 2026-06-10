# Uses global variables to reprocess the referral data
processData <- function()
{
  # Initialise important data.frames
  aeData <<- data.frame(matrix(ncol = length(aeTableColNames), nrow = 0))
  dataIntegrity.df <<- data.frame(matrix(ncol = length(dataIntegrityColNames), nrow = 0))
  colnames(aeData) <<- aeTableColNames
  colnames(dataIntegrity.df) <<- dataIntegrityColNames
  notForRxButRxdCount <<- 0
  noRefsProcessed <<- 0
  
  # Crunch the Referral Data
  deceased = as.integer()
  iRef = as.integer()
  pt_ref_count = as.integer()
  pt_rx_count = as.integer()
  
  # Iterate through patient records one by one
  patientCount <- count(studyData$Study)$n
  for (i in 1:patientCount)
  {
    ptID <- studyData$Study$Participant_ID[i]
    
    # If the patient ID is all zeros e.g. 0, 00, 000 etc then we assume its a dummy patient, and we skip it...
    ptIDInteger = strtoi(ptID)
    if (!is.na(ptIDInteger) && ptIDInteger == 0)
    {
      logger(paste("\n", i, "/", patientCount, " Pt=", ptID, " is being skipped from analysis as dummy patient...", sep = ""))
      next
    }
    else
    {
      logger(paste(i, "/", patientCount, " Pt=", ptID, " performing referral analysis...", sep = ""))
    }
    
    # Get Diagnosis - FIXME we assume the first diagnosis is the one we care about for TARGET-UK, but what about the second diagnosis? It would be useful for multiple organ treatments for example
    diagnosis_type <- getDataEntry("dx_diagonsis_type_1",i)   # Key> P=Primary, S=Secondary, B=Benign, U=Unknown, N=N/A (FIXME: diagnosis spelt wrong in Castor!)
    if (!is.na(diagnosis_type)) diagnosis_type <- substr(diagnosis_type, start = 1, stop = 1) # Get the first char otherwise it returns the descriptive text
    diagnosis_1o <- NA
    diagnosis_2o <- NA
    diagnosis_bn <- NA
    diagnosis_un <- NA
    modalityListForThisPatient <- c() # Reset this for each new patient
    
    if (!is.na(diagnosis_type) && diagnosis_type == "P")
    {
      diagnosis_1o <- getDataEntry("dx_histology_primary_1",i, returnLabelIfFactor=TRUE)    # e.g. Lung: SCLC
    }
    else if (!is.na(diagnosis_type) && diagnosis_type == "S")
    {
      diagnosis_2o <- getDataEntry("dx_histology_secondary_1",i, returnLabelIfFactor=TRUE)  # e.g. Colorectal
    }
    else if (!is.na(diagnosis_type) && diagnosis_type == "B")
    {
      diagnosis_bn <- getDataEntry("dx_histology_benign_1",i, returnLabelIfFactor=TRUE)     # e.g. Desmoid
    }
    else if (!is.na(diagnosis_type) && diagnosis_type == "U")
    {
      diagnosis_un <- "Unknown"                                                             # we don't know what it is as it hasn't been specified
    }
    else
    {
      # If unknown or N/A diagnosis type then nothing to do - everything is NA
    }
    
    #FIXME I don't know how to tell if archived any more since v1.1.0 to 2.1.0 of castoRedc
    #Assuming if archived, it won't come back in studyData
    #ptArchived <- patientData[["archived"]][i]
    #if (ptArchived) {
    #  logger(paste("\n", i, "/", patientCount, " Pt=", ptID, " Archived, skipping referral analysis...", sep = ""))
    #  next
    #}
    # Also see patientArchived() function which needs fixing up for latest castoRedc
    
    postcode <- getDataEntry("pt_postcode", i)
    sex <- as.integer(getDataEntry("sex", i))
    if (is.na(sex))
    {
      sex <- "Unknown"
    } 
    else if (sex == 1)
    {
      sex <- "Female"
    }
    else if (sex == 2)
    {
      sex <- "Male"
    }
    else
    {
      sex <- "Unknown"
    }
    birth_year <- getDataEntry("birth_year", i)
    if (!is.na(birth_year) && !is.integer(birth_year)) {
      addDataIntegrityError(ptID, error=paste(i, "/", patientCount, " Pt=", ptID, " : patient birth year", birth_year, " is not a number.", sep = ""))
      birth_year <- NA
    }
    age_at_event <- NA
    pt_ref_count <- as.integer(getDataEntry("pt_ref_count", i))
    pt_rx_count  <- as.integer(getDataEntry("pt_rx_count", i))
    
    if (is.na(pt_ref_count)) {
      logger(paste("     Ref Count=NA, skipping from analysis...", sep = ""), logOnlyAsDebug = TRUE)
      next
    }
    if (pt_ref_count == 0) {
      logger(paste(i, "/", patientCount, " Pt=", ptID, " Ref Count=0 ,skipping from analysis...", sep = ""), logOnlyAsDebug = TRUE)
      next
    }
    
    # Otherwise we have at least one referral for this patient so keep going...
    
    # Get survival status
    deceased_date <- NA
    deceasedStr <- getDataEntry("fu_deceased", i)
    if (!is.na(deceasedStr) && deceasedStr == "Yes") {
      deceased <- 1
    } else {
      deceased <- 0
    }
    if (deceased == 1) {
      deceased_date <- getDataEntry("fu_deceased_date", i)
      deceased_date = convertToDate(deceased_date)
    } else {
      deceased_date <- NA
    }
    
    # True if patient died with active cancer following previous ablation, if no e.g died of heart attack patient is censored in survival analysis
    deceased_relatedStr <- getDataEntry("fu_deceased_related", i)
    if (!is.na(deceased_relatedStr) && deceased_relatedStr == "Yes")
    {
      deceased_related <- 1
    }
    else
    {
      deceased_related <- 0
    }
    
    lost_to_fu_date <- NA
    lost_to_fu <- getDataEntry("fu_lost", i)
    
    if (!is.na(lost_to_fu) && lost_to_fu == 1) {
      lost_to_fu_date <- getDataEntry("fu_lost_date", i)
      lost_to_fu_date <- convertToDate(lost_to_fu_date)
      
      if (!is.na(birth_year)) {
        age_at_event <- as.integer(format(asDateWithOrigin(lost_to_fu_date), "%Y")) - birth_year
      }
      
      if (is.na(lost_to_fu_date)) {
        lost_to_fu <- 0
        lost_to_fu_date <- NA
        addDataIntegrityError(ptID, date=lost_to_fu_date, error=paste(i, "/", patientCount, " Pt=", ptID, " patient is lost to follow-up but date of lost-to-follow-up not specified or valid, thus cannot count as lost to follow-up.", sep = ""))
      }
    }
    else
    {
      lost_to_fu <- 0
      lost_to_fu_date <- NA
    }
    
    survival_organ <- NA # Set this now as null, and check on each referral
    date_of_last_clinical_fu <- NA
    
    # FIX 1: Initialise patient-level max tumour size accumulator (was never updated in original)
    survival_max_tumour_size <- NA
    
    # Get clinical status / last clinical follow-up
    clinicalfuJSON <- getDataEntry("fu_clinical_matrix", i)
    last_alive_date <- NA
    if (!is.na(clinicalfuJSON) && str_length(clinicalfuJSON) > 0)
    {
      clinicalfuMatrix <- jsonlite::fromJSON(clinicalfuJSON)
      clinicalfu.df <- as.data.frame(t(sapply(clinicalfuMatrix, unlist)))
      colnames(clinicalfu.df) <- clinicalfuColNames
      clinicalfu.df <- clinicalfu.df[apply(clinicalfu.df, 1, function(row) any(row != "")), ]
      
      # For each row in the matrix, check if there is any clinical follow-up data , if so grab the last date of follow-up
      for (j in 1:nrow(clinicalfu.df))
      {
        # Get the date of the clinical follow-up
        if (!is.na(clinicalfu.df$followup.date[j]) && isConvertibleToDate(clinicalfu.df$followup.date[j]))
        {
          # Get the last imaging follow-up date
          thisClinicalFUDate <- convertToDate(clinicalfu.df$followup.date[j])
          
          # Get the last clinical follow-up date
          if (is.na(date_of_last_clinical_fu) || date_of_last_clinical_fu < thisClinicalFUDate)
          {
            date_of_last_clinical_fu <- thisClinicalFUDate
          }
        }
      }
    }
    
    # Get the last known alive date if it is filled in
    # This might be for example after the last CT and after the last clinic date, and could be another imaging date e.g. chest x-ray which proves they are still alive
    date_last_alive_fu <- NA
    date_last_alive_fu <- getDataEntry("fu_last_alive_date", i)
    if (!is.na(date_last_alive_fu)) date_last_alive_fu <- convertToDate(date_last_alive_fu)
    
    # Decide if it is benign or malignant, if not specified there will be no imaging follow-up table
    date_of_last_imaging_fu <- NA
    date_of_first_ltp <- NA
    no_rx_before_first_ltp <- NA
    ltp <- 0
    ltp_status <- NA
    
    # Get local tumour progression status if malignant lesion
    indication <- getDataEntry("fu_indication", i)
    
    if (!is.na(indication) & indication == "Malignant")
      #if (is.na(indication) | indication == "Malignant") # FIXME this is temporary until we define the malignant ones in imaging FU in EDC
    {
      #FIXME: Matrix flipping! The _1 is just like a v2
      if (newRecurrenceMatrix == T)
      {
        imagingJSON <- getDataEntry("fu_image_matrix_malignant_1", i, stopIfNotFound = TRUE)
      }
      else
      {
        imagingJSON <- getDataEntry("fu_image_matrix_malignant", i, stopIfNotFound = TRUE)
      }

      # Go row by row in the imaging follow-up matrix...
      if (!is.na(imagingJSON) && str_length(imagingJSON) > 0)
      {
        imagingMatrix <- jsonlite::fromJSON(imagingJSON)
        imaging.df <- as.data.frame(t(sapply(imagingMatrix, unlist)))
        
        if (newRecurrenceMatrix ==T)
          (stopifnot("Recurrence matrix has unexpected number of columns - has the Castor schema changed?" = 
                       ncol(imaging.df) == length(recurrenceColNames)))
        colnames(imaging.df) <- recurrenceColNames
        imaging.df <- imaging.df[apply(imaging.df, 1, function(row) any(row != "")), ] # Strip out any empty trailing rows
        
        # For each row in the matrix, check if there is any local tumour progression, if so grab the first date of local tumour progression
        for (j in seq_len(nrow(imaging.df))) # This is safer as if nrow=0, without seq_len the loop will run twice first with j=1 then j=0 !
        {
          # Get the date of the imaging
          img_date <- imaging.df$imaging.date[j]
          if (!is.na(img_date) && isTRUE(isConvertibleToDate(img_date)))
          {
            # Get the last imaging follow-up date
            thisImagingDate <- convertToDate(img_date)
            if (is.na(date_of_last_imaging_fu) || date_of_last_imaging_fu < thisImagingDate) # This OR || is correct as TRUE & NA = NA, not TRUE
            {
              date_of_last_imaging_fu <- thisImagingDate
            }
            
            # If this row shows local tumour progression...
            # 
            # Original simple coding before improvements for:
            #   'N' = No
            #   'YA' = Yes (recurrent) and Ablatable Recurrence
            #   'YNA' = Yes (recurrent) and Non-ablatable Recurrence
            #
            # This is the new coding, and the new table returns the code not the label:
            #
            #   0: No RD/LTP
            #   1: 1st RD/LTP
            #   2: 2nd RD/LTP
            #   3: >2nd RD/LTP
            #
            # Note specifically this is a per-patient LTP analysis, rather than per-lesion analysis
            thisLTP <- imaging.df$ltp[j]
            if (newRecurrenceMatrix ==T) # FIXME this is a fix for the new table only
            {
              thisLTP <- switch(as.character(thisLTP),
                                "0" = "No RD/LTP",
                                "1" = "1st RD/LTP",
                                "2" = "2nd RD/LTP", 
                                "3" = ">2nd RD/LTP",
                                thisLTP  # default: return as-is for unknown codes
              )
            }

            # if (thisLTP == "Y" || thisLTP == "YA" || thisLTP == "YNA")
            if (substring(thisLTP, 1, 1) != "N") # Good catch all for local tumour progression, also respecting historical coding "N"
            {
              # This records if it is local tumour progression after 1, 2 or >2 treatments (the definition of LTP can vary!) - the local tumour progression string is like '1LTP-U' for 1st LTP/RD - untreated
              thisRecurrenceAfterRxNo <- substring(thisLTP,1,1)
              
              # The date of this imaging, which has show local tumour progression
              thisRecurrenceDate <- thisImagingDate
              
              # If we have not yet recurred...
              if (ltp == 0)
              {
                ltp <- 1
                date_of_first_ltp <- thisRecurrenceDate
                no_rx_before_first_ltp <- thisRecurrenceAfterRxNo
              }
              # We have already recurred, so we just want the earliest local tumour progression date
              else
              {
                if (thisRecurrenceDate < date_of_first_ltp)
                {
                  date_of_first_ltp <- thisRecurrenceDate
                  no_rx_before_first_ltp <- thisRecurrenceAfterRxNo
                }
              }
              
              # Per-lesion LTP linkage via ltp.list field
              # ltp.list contains the referral and lesion number(s) where LTP occurred
              # in the format 'Rx.Tumour' e.g. '3.1' or '3.1, 4.3'
              # This is only processed if ltp is TRUE - if ltp is FALSE, ltp.list is ignored
              # (and a data integrity warning is raised if ltp.list has content when ltp is FALSE - see below)
              if ("ltp.list" %in% colnames(imaging.df))
              {
                thisLTPLesionIDs <- imaging.df$ltp.list[j]
                if (!is.null(thisLTPLesionIDs) && !is.na(thisLTPLesionIDs) && trimws(thisLTPLesionIDs) != "")
                {
                  # ltp.list has content — attempt to parse
                  parsedLTPLesions <- parseltpListString(thisLTPLesionIDs)
                  
                  if (nrow(parsedLTPLesions) > 0)
                  {
                    # Valid parse — store per-lesion linkage for per-lesion analysis
                    for (k in 1:nrow(parsedLTPLesions))
                    {
                      ltp_perlesion_ptid_list     <<- append(ltp_perlesion_ptid_list,    ptID)
                      ltp_perlesion_refno_list    <<- append(ltp_perlesion_refno_list,   parsedLTPLesions$ref_no[k])
                      ltp_perlesion_lesionno_list <<- append(ltp_perlesion_lesionno_list,parsedLTPLesions$lesion_no[k])
                      ltp_perlesion_date_list     <<- append(ltp_perlesion_date_list,    thisImagingDate)
                    }
                  }
                  else
                  {
                    # parseLTPLesionIDs returned empty — content was unparseable
                    # LTP event still counts for per-patient analysis but lesion linkage is lost
                    addDataIntegrityError(ptID, date=thisImagingDate,
                                          error=paste("LTP = TRUE but ltp.list '", thisLTPLesionIDs,
                                                      "' could not be parsed - expected format e.g. '1.1,2.3'. ",
                                                      "LTP event retained for per-patient analysis only.", sep=""))
                  }
                }
                # else: ltp.list is empty/NA — LTP confirmed but no lesion linkage specified, silently accepted
                # Per-patient analysis is unaffected, per-lesion analysis will not include this event
              }
            }
            else
            {
              # ltp is FALSE (no LTP) — ltp.list should be empty, warn if not
              if ("ltp.list" %in% colnames(imaging.df))
              {
                thisLTPLesionIDs <- imaging.df$ltp.list[j]
                if (!is.null(thisLTPLesionIDs) && !is.na(thisLTPLesionIDs) && trimws(thisLTPLesionIDs) != "")
                {
                  addDataIntegrityError(ptID, date=thisImagingDate,
                                        error=paste("ltp.list = '", thisLTPLesionIDs,
                                                    "' but ltp FALSE on row ", j, " - value ignored.", sep=""))
                }
              }
            }
          }
          else
          {
            if (!is.na(img_date))
            {
              addDataIntegrityError(ptID, date=img_date, error=paste(i, "/", patientCount, " Pt=", ptID, " patient's imaging follow-up date (",img_date,") is not valid date on row ",j, sep = ""))
            }
          }
        }
        
        # Set the local tumour progression status, just like we do with survival
        # https://thriv.github.io/biodatasci2018/r-survival.html
        if (ltp == 1)
        {
          ltp_status <- 2  # This is cf. death due to cancer i.e. counts on the Kaplin-Meyer
        }
        else
        {
          ltp_status <- 1
        }
      }
    }
    else if (!is.na(indication) & indication == "Benign")
    {
      # Get the benign imaging follow-up table
      imagingJSON <- getDataEntry("fu_image_matrix_benign", i)
    }
    else
    {
      # If its not malignant or benign there is no follow-up imaging data
    }
    
    # Work out when last known alive as the latest date via imaging, clinical follow-up or last known alive field if filled out
    if (!is.na(date_of_last_clinical_fu))
    {
      last_alive_date <- date_of_last_clinical_fu
    }
    if (!is.na(date_of_last_imaging_fu))
    {
      if (is.na(last_alive_date) || date_of_last_imaging_fu > last_alive_date)
      {
        last_alive_date <- date_of_last_imaging_fu
      }
    }
    if (!is.na(date_last_alive_fu))
    {
      if (is.na(last_alive_date) || date_last_alive_fu > last_alive_date)
      {
        last_alive_date <- date_last_alive_fu
      }
    }
    
    if (deceased == 1 && !is.na(deceased_date))
    {
      if(!is.na(last_alive_date) && deceased_date < last_alive_date)
      {
        addDataIntegrityError(ptID, date=deceased_date, error=paste("Patient recorded as deceased on ",deceased_date," but has follow-up on ",last_alive_date,".", sep = ""))
      }
      # Whatever else happens they were alive the day before they died
      last_alive_date = deceased_date-1
    }
    
    # Get these dates...
    date_of_diagnosis <- NA # Reset this each time we enter the referral loop
    date_of_first_rx  <- NA # Reset this each time we enter the referral loop
    
    if (pt_ref_count == 0)
    {
      logger(" No referrals for this patient")
    }
    
    # Initialise per-patient survival variables before referral loop
    survival_overall_status  <- NA
    survival_cancer_specific_status <- NA
    survival_days <- NA
    ltpf_os_survival_days   <- NA
    ltpf_os_survival_status <- NA
    ltpf_cs_survival_days   <- NA
    ltpf_cs_survival_status <- NA
    ltp_days  <- NA
    
    # For each referral...
    for (iRef in 1:pt_ref_count)
    {
      noRefsProcessed <<- noRefsProcessed + 1
      ptForRx <- getDataEntry(paste("ref_intention_rx_", as.integer(iRef), sep = ""), i)
      ptOffPathway <- getDataEntry(paste("ref_off_rx_pathway_", as.integer(iRef), sep = ""), i)
      
      # Only continue if the patient is for treatment and is not off the pathway
      # (or if these things haven't been filled out and are NA so we can fix missing data using the plot)
      # Note that earlier version of castoRedc used "1" here, now it is "Yes" and "No"
      if ( (is.na(ptForRx) | ptForRx == "Yes") & (is.na(ptOffPathway) | ptOffPathway != "Yes"))
      {
        organForRx = NA
        org <- getDataEntry(paste("ref_organ_", as.integer(iRef), sep = ""), i)
        
        # The older getDataEntry() function/castoRedc library would give "L", "K" etc but the newer gives "Liver", "Kidney" etc which I don't understand!
        if (is.na(org))
        {
          org = "Other/Unspecified"
        }
        if (packageVersion("castoRedc") == '1.1.0')
        {
          organForRx <- switch(as.character(org),
                               "LIV" = "Liver",
                               "L" = "Lung",
                               "K" = "Kidney",
                               "P" = "Pancreas",
                               "A" = "Adrenal",
                               "M" = "MSK",
                               "B" = "Bone",
                               "MO" = "Multiple Organs",
                               "STMO" = "ST Multiple Organs",
                               "Other/Unspecified" #This is the catch-all default
          )
        }
        else
        {
          # With the later version of the castoRedc we dont need to convert anything
          # as the getStudyData() function returns the text not the code!
          organForRx <- as.character(org)
        }
        if (is.na(survival_organ)) {
          survival_organ <- organForRx
        }
        else if (survival_organ != organForRx)
        {
          # If this treatment is for a different organ, then we have multiple organs across
          # the multiple referrals
          survival_organ <- "Multiple Organs"
        }
        
        # Get the date of referral, if it is NA later on we set it as the treatment date, if that is set, as a bit of a hack
        ref_date <- convertToDate(getDataEntry(paste("ref_date_recd_", as.integer(iRef), sep = ""), i))
        
        # If we don't have valid referral date received, use referral letter date as next best thing
        if (is.na(ref_date))
        {
          ref_date <- convertToDate(getDataEntry(paste("ref_letter_date_", as.integer(iRef), sep = ""), i))
        }
        
        # Get the date of diagnosis which is the earliest referral date of all the referrals encountered
        if (!is.na(ref_date))
        {
          if (!is.na(date_of_diagnosis)) {
            date_of_diagnosis <- convertToDate(date_of_diagnosis)
            date_of_diagnosis <- dplyr::if_else(
              asDateWithOrigin(ref_date) < asDateWithOrigin(date_of_diagnosis),
              ref_date,
              date_of_diagnosis
            )
          } else {
            date_of_diagnosis <- ref_date
          }
        }
        
        # Sort DTT, even if we don't have a valid referral date, otherwise we loose treatments from the plots and tariffs ...
        ref_dtt_date <- convertToDate(getDataEntry(paste("ref_date_dtt_", as.integer(iRef), sep = ""), i))
        if (!is.na(ref_date) & !is.na(ref_dtt_date))
        {
          ref_dtt_days <- as.numeric(difftime(ref_dtt_date, ref_date, units = "days"), units = "days")
        }
        else
        {
          ref_dtt_days <- NA
        }
        
        # Get To Come In (TCI) rx date and status, NA if not set
        ref_tci_date <- convertToDate(getDataEntry(paste("ref_tci_date_", as.integer(iRef), sep = ""), i))
        thisTciStatus <- getDataEntry(paste("ref_tci_status_", as.integer(iRef), sep = ""), i)
        ref_tci_status <- NA
        if (!is.na(thisTciStatus))
        {
          # These must match the modality radiobutton options is app.R
          ref_tci_status <- switch(as.character(thisTciStatus), "1"="Provisional", "2"="Confirmed", "3"="Deferred", "4"="Cancelled", "Status Unknown")
        }
        
        # Sort Clock Stop
        # Parse the JSON and get it into a nice data frame
        refclockstop.df <- NA
        clockstoppedDaysPreDTT <- 0
        clockstoppedDaysPostDTT <- 0
        clockstoppedReason <- ""
        clockStopJSON <- getDataEntry(paste("ref_clock_stop_", as.integer(iRef), sep = ""), i)
        
        if (!is.na(clockStopJSON) && str_length(clockStopJSON) > 0 )
        {
          clockStopMatrix <- jsonlite::fromJSON(clockStopJSON)
          refclockstop.df <- as.data.frame(t(sapply(clockStopMatrix, unlist)))
          
          # Clock stop padding
          target_cols <- length(clockStopColNames)
          if (ncol(refclockstop.df) < target_cols) {
            for (col in (ncol(refclockstop.df) + 1):target_cols) {
              refclockstop.df[[as.character(col)]] <- NA
            }
          }
          colnames(refclockstop.df) <- clockStopColNames
          refclockstop.df <- refclockstop.df[apply(refclockstop.df, 1, function(row) any(row != "")), ]    # Remove the blank rows
          
          # For each row in the matrix
          if (nrow(refclockstop.df) > 0)
          {
            for (j in 1:nrow(refclockstop.df))
            {
              dateClockstopped <- convertToDate(refclockstop.df$date.stopped[j])
              dateClockRestart <- convertToDate(refclockstop.df$date.restart[j])
              
              if (!is.na(dateClockstopped)) {
                if (is.na(dateClockRestart)) {
                  # Make the restart date today if there is no useful restart date
                  dateClockRestart = Sys.Date()
                  virtualRestart = T # This is just so we can put an asterix if it hasn't been restarted yet, virtual stop
                } else {
                  virtualRestart = F
                }
                
                # If we don't yet have a DTT date (for example see in clinic after radiotherapy), or have stopped the clock before the DTT...
                if (is.na(ref_dtt_date) || (!is.na(ref_dtt_date) && dateClockstopped < ref_dtt_date))
                {
                  clockstoppedDaysPreDTT <- clockstoppedDaysPreDTT +
                    as.integer(difftime(dateClockstopped, dateClockRestart, units = "days"), units = "days")
                }
                else
                {
                  clockstoppedDaysPostDTT <- clockstoppedDaysPostDTT +
                    as.integer(difftime(dateClockstopped, dateClockRestart, units = "days"), units = "days")
                }
                
                clockstoppedReason <- paste(clockstoppedReason, " ", j, ":", refclockstop.df$reason[j], sep = "")
                
                if (virtualRestart == T) {
                  clockstoppedReason <- paste(clockstoppedReason, "*", sep = "")
                }
              }
            }
          }
        }
        if (clockstoppedDaysPreDTT != 0 || clockstoppedDaysPostDTT != 0) {
          clockstoppedReason <- paste("\nClock Stops:", clockstoppedReason, " (", clockstoppedDaysPreDTT, " days pre-DTT, ", clockstoppedDaysPostDTT, " days post-DTT)", sep = "")
        }
        if (clockstoppedDaysPreDTT == 0) {
          clockstoppedDaysPreDTT = NA # Don't plot all the zeros, only if it is non-zero
        }
        if (clockstoppedDaysPostDTT == 0) {
          clockstoppedDaysPostDTT = NA # Don't plot all the zeros, only if it is non-zero
        }
        
        # Sort Rx Date. Note used to use the ref_date_rx_i field but it is a calculation of anaes_date_i
        # and had some issues with it being a lot of NAs but not all NAs (weird), anyway went for the source
        # and it works much better!
        ref_rx_date <- convertToDate(getDataEntry(paste("anaes_date_", as.integer(iRef), sep = ""), i))
        
        # If Rx date is set patient must have been referred (even if we don't have a referral date) and treated...
        # Therefore we add a row to the treatment data frame no matter what from this point...
        if (!is.na(ref_rx_date))
        {
          
          # If the Ref is NA then set it to the Rx date as next best guess... Otherwise it may be missed from TARGET & Audit
          if (is.na(ref_date))
          {
            addDataIntegrityError(ptID, refID=paste(iRef, "/", pt_ref_count, sep=""), date=ref_rx_date, organs=organForRx,
                                  error=paste("Patient for Rx but doesn't have a valid referral date, defaulting to Rx date.", sep = ""))
            ref_date <- ref_rx_date
          }
          
          ptRefTreated = T
          operatorString1 <- getDataEntry(paste("rx_operator1o_", as.integer(iRef), sep = ""), i)
          operatorString2 <- getDataEntry(paste("rx_operator2o_", as.integer(iRef), sep = ""), i)
          operatorString3 <- getDataEntry(paste("rx_operator3o_", as.integer(iRef), sep = ""), i)
          operatorString1 <- tolower(trimws(operatorString1))
          operatorString2 <- tolower(trimws(operatorString2))
          operatorString3 <- tolower(trimws(operatorString3))
          operatorString1[operatorString1 == ''] <- NA  # Replace any '' blank field with NA
          operatorString2[operatorString2 == ''] <- NA
          operatorString3[operatorString3 == ''] <- NA
          
          anaesthetistString1 <- getDataEntry(paste("anaes_anaesthetist1o_", as.integer(iRef), sep = ""), i)
          anaesthetistString2 <- getDataEntry(paste("anaes_anaesthetist2o_", as.integer(iRef), sep = ""), i)
          anaesthetistString3 <- getDataEntry(paste("anaes_anaesthetist3o_", as.integer(iRef), sep = ""), i)
          anaesthetistString1 <- tolower(trimws(anaesthetistString1))
          anaesthetistString2 <- tolower(trimws(anaesthetistString2))
          anaesthetistString3 <- tolower(trimws(anaesthetistString3))
          anaesthetistString1[anaesthetistString1 == ''] <- NA  # Replace any '' blank field with NA
          anaesthetistString2[anaesthetistString2 == ''] <- NA
          anaesthetistString3[anaesthetistString3 == ''] <- NA
          
          rxFreeText <- ""
          
          # Get the free text for Rx and the modality of the treatment (for cost code purposes)
          rxTableJSON <- getDataEntry(paste("rx_tumour_rx_matrix_", as.integer(iRef), sep = ""), i, T)
          
          # FIX 2: Initialise maxTumourSize and modalityForRx before the JSON block so they are always defined
          maxTumourSize <- NA
          modalityForRx <- NA
          
          if (!is.na(rxTableJSON) && str_length(rxTableJSON) > 0 )
          {
            rxTableMatrix <- jsonlite::fromJSON(rxTableJSON)
            rxTable.df <- as.data.frame(t(sapply(rxTableMatrix, unlist)))
            colnames(rxTable.df) <- rxTableColNames
            rxTable.df <- rxTable.df[apply(rxTable.df, 1, function(row) any(row != "")), ]
            
            # Get the free text for the treatments and concat into a string for the Treatment pathway table
            for (j in 1:nrow(rxTable.df))
            {
              thisRxFreeText <- rxTable.df$free.text[j]
              if (!is.na(thisRxFreeText) && thisRxFreeText != "")
              {
                rxFreeText <- paste(rxFreeText," ",j,":",thisRxFreeText,sep="")
              }
            }
            
            # For each row in the matrix find out which modality has been used
            # Once a modality has been found we are done as we make assumption we don't mix and max for any given case
            # If there are multiple modalities for different tumours, multiple organs, the code is not that clever yet...
            for (j in 1:nrow(rxTable.df))
            {
              modalityForRx <- rxTable.df$modality[j]
              if (!is.na(modalityForRx))
              {
                # These must match the modality radiobutton options is app.R
                modalityForRx <- switch(modalityForRx, "M"="Microwave", "C"="Cryotherapy", "R"="Radiofrequency", "Other/Unknown")
                break
              }
              else
              {
                modalityForRx <- "Other/Unknown"
              }
            }
            # This is like 'microwave,cryotherapy' but won't repeat e.g. not 'microwave,cryotherapy,cryotherapy'
            modalityListForThisPatient <- unique(append(modalityListForThisPatient, modalityForRx))
            
            # maxTumourSize computed only when rxTable.df exists
            maxTumourSize <- if (all(is.na(as.numeric(rxTable.df$lesion.size)))) NA else max(as.numeric(rxTable.df$lesion.size), na.rm = TRUE)
          }
          
          # Roll up per-referral maxTumourSize into patient-level survival_max_tumour_size
          if (!is.na(maxTumourSize)) {
            survival_max_tumour_size <- if (is.na(survival_max_tumour_size)) maxTumourSize else max(survival_max_tumour_size, maxTumourSize, na.rm = TRUE)
          }
          
          # Get the tariff for the Rx
          tariffForRx <- getTariffForRx(organForRx, modalityForRx)
          
          # Get the early and late complication matrices
          earlyAETable.df <- NA
          earlyAETableJSON <- getDataEntry(paste("complication_early_matrix_", as.integer(iRef), sep = ""), i, F)
          if (!is.na(earlyAETableJSON) && str_length(earlyAETableJSON) > 0 )
          {
            # These complications occurred early, ie. during the admission of the ablation
            earlyAETableMatrix <- jsonlite::fromJSON(earlyAETableJSON)
            earlyAETable.df <- as.data.frame(t(sapply(earlyAETableMatrix, unlist)))
            
            # Sometimes in Castor it can return tables with shorter number of columns which can then crash, unless we pad out to expected no. cols with NAs
            # earlyAETable padding
            target_cols <- length(aeTableColNames)-3
            if (ncol(earlyAETable.df) < target_cols) {
              for (col in (ncol(earlyAETable.df) + 1):target_cols) {
                earlyAETable.df[[as.character(col)]] <- NA
              }
            }
            
            # Now we can add new columns either side
            earlyAETable.df <- earlyAETable.df[apply(earlyAETable.df, 1, function(row) any(row != "")), ] # Remove the empty rows
            earlyAETable.df <- cbind(organForRx,earlyAETable.df)  # Pre-pend the Organ for Rx
            earlyAETable.df <- cbind(ptID,earlyAETable.df)        # Pre-pend the Patient ID
            earlyAETable.df <- cbind(earlyAETable.df,0)           # Append the post-discharge field i.e. early means before discharge
            earlyAETable.df <- cbind(earlyAETable.df,NA)          # Append the duration field
            colnames(earlyAETable.df) <- aeTableColNames
            for (j in 1:nrow(earlyAETable.df))
            {
              if (!is.na(earlyAETable.df$Complication[j]) && earlyAETable.df$Complication[j] != "")
              {
                if (!is.na(convertToDate(earlyAETable.df$DateofResolution[j])) && !is.na(convertToDate(earlyAETable.df$DateofOnset[j])))
                {
                  earlyAETable.df$Duration[j] <- difftime(convertToDate(earlyAETable.df$DateofResolution[j]), convertToDate(earlyAETable.df$DateofOnset[j]), units="days")
                }
                aeData <<- rbind(aeData,earlyAETable.df[j,])
              }
            }
          }
          lateAETable.df <- NA
          lateAETableJSON  <- getDataEntry(paste("cx_late_matrix_", as.integer(iRef), sep = ""), i, F)
          if (!is.na(lateAETableJSON) && str_length(lateAETableJSON) > 0 )
          {
            # These complications occurred later, after discharge
            lateAETableMatrix <- jsonlite::fromJSON(lateAETableJSON)
            lateAETable.df <- as.data.frame(t(sapply(lateAETableMatrix, unlist)))
            
            # Sometimes in Castor it can return tables with shorter number of columns which can then crash, unless we pad out to expected no. cols with NAs
            target_cols <- length(aeTableColNames)-3
            if (ncol(lateAETable.df) < target_cols) {
              for (col in (ncol(lateAETable.df) + 1):target_cols) {
                lateAETable.df[[as.character(col)]] <- NA
              }
            }
            
            lateAETable.df <- lateAETable.df[apply(lateAETable.df, 1, function(row) any(row != "")), ]  # Remove the empty rows
            lateAETable.df <- cbind(organForRx,lateAETable.df)  # Pre-pend the Organ for Rx
            lateAETable.df <- cbind(ptID,lateAETable.df)        # Pre-pend the Patient ID
            lateAETable.df <- cbind(lateAETable.df,1)           # Append the post-discharge field i.e. late means after discharge
            lateAETable.df <- cbind(lateAETable.df,NA)          # Append the duration field
            colnames(lateAETable.df) <- aeTableColNames
            for (j in 1:nrow(lateAETable.df))
            {
              if (!is.na(lateAETable.df$Complication[j]) && lateAETable.df$Complication[j] != "")
              {
                if (!is.na(convertToDate(lateAETable.df$DateofResolution[j])) && !is.na(convertToDate(lateAETable.df$DateofOnset[j])))
                {
                  lateAETable.df$Duration[j] <- difftime(convertToDate(lateAETable.df$DateofResolution[j]), convertToDate(lateAETable.df$DateofOnset[j]), unit="days")
                }
                aeData <<- rbind(aeData,lateAETable.df[j,])
              }
            }
          }
          
          # Change any blanks to 'Unspecified'
          aeData$Grade[is.na(aeData$Grade)]  <<- "Unspecified" # Replace 'unspecified i.e. NA grade with 'Unspecified'
          aeData$Grade[aeData$Grade == ""]   <<- "Unspecified" # Replace 'unspecified i.e. "" grade with 'Unspecified'
          aeData$Grade[aeData$Grade == " "]  <<- "Unspecified" # Replace 'unspecified i.e. "" grade with 'Unspecified'
          
          # Time between referral and Rx
          ref_rx_days = as.numeric(difftime(ref_rx_date, ref_date, units = "days"), units = "days")
          if (ref_rx_days < 0)
          {
            addDataIntegrityError(ptID, date=ref_rx_date, error=paste("Patient's treatment date is before referral date.", sep = ""))
          }
          
          # Correct for clockstop
          if (!is.na(clockstoppedDaysPreDTT)) {
            ref_dtt_days <- ref_dtt_days + clockstoppedDaysPreDTT # Remember clockstoppedDaysPreDTT is -ve
            ref_rx_days  <- ref_rx_days  + clockstoppedDaysPreDTT # Remember clockstoppedDaysPreDTT is -ve
          }
          if (!is.na(clockstoppedDaysPostDTT)) {
            ref_rx_days <- ref_rx_days + clockstoppedDaysPostDTT # Remember clockstoppedDaysPostDTT is -ve
          }
          if (!is.na(ref_dtt_days)) {
            ref_dtt_rx_days <- ref_rx_days - ref_dtt_days
          } else {
            ref_dtt_rx_days <- NA
          }
          
          # Update date of first rx
          if (!is.na(date_of_first_rx)) {
            date_of_first_rx <- dplyr::if_else(   # FIX 3: was incorrectly updating date_of_diagnosis here
              asDateWithOrigin(ref_rx_date) < asDateWithOrigin(date_of_first_rx),
              ref_rx_date,
              date_of_first_rx
            )
          }
          else
          {
            date_of_first_rx <- ref_rx_date
          }
          
          # Add to the treatment done table
          # Note that 'Multiple Organs' is phased out now, as we are more specific e.g. Liver+Lung in Castor
          if (organForRx == "Liver+Lung" | organForRx == "Multiple Organs")
          {
            organsToAdd <- c("Lung", "Liver") # FIXME all our multiples have been lung and liver but suggest change drop down to Lung+Liver and being cleverer
          }
          else
          {
            organsToAdd <- c(organForRx)
          }
          # Use this trick to add multiple organs as separate rows, with everything the same except the organ itself
          for (organ in organsToAdd)
          {
            rxdone_pt_list                     <<- append(rxdone_pt_list,                       paste(ptID, "-", iRef, sep = ""))
            rxdone_sex_list                    <<- append(rxdone_sex_list,                      sex)
            rxdone_diagnosis_type_list         <<- append(rxdone_diagnosis_type_list,           diagnosis_type) # S=Secondary, P=Primary or B=Benign
            rxdone_diagnosis_1o_list           <<- append(rxdone_diagnosis_1o_list,             diagnosis_1o)   # Cancer
            rxdone_diagnosis_2o_list           <<- append(rxdone_diagnosis_2o_list,             diagnosis_2o)   # Cancer
            rxdone_diagnosis_bn_list           <<- append(rxdone_diagnosis_bn_list,             diagnosis_bn)   # Benign
            rxdone_diagnosis_un_list           <<- append(rxdone_diagnosis_un_list,             diagnosis_un)   # Unknown
            rxdone_organ_list                  <<- append(rxdone_organ_list,                    organ)
            rxdone_modality_list               <<- append(rxdone_modality_list,                 modalityForRx)
            rxdone_max_tumour_size_list        <<- append(rxdone_max_tumour_size_list,          maxTumourSize)
            rxdone_tariff_list                 <<- append(rxdone_tariff_list,                   tariffForRx)
            rxdone_refdate_list                <<- append(rxdone_refdate_list,                  ref_date)
            rxdone_dttdate_list                <<- append(rxdone_dttdate_list,                  ref_dtt_date)
            rxdone_rxdate_list                 <<- append(rxdone_rxdate_list,                   ref_rx_date)
            rxdone_dtt_days_list               <<- append(rxdone_dtt_days_list,                 ref_dtt_days)    # Days from Ref to DTT
            rxdone_rx_days_list                <<- append(rxdone_rx_days_list,                  ref_rx_days)     # Days from Ref to Rx
            rxdone_dtt_rx_days_list            <<- append(rxdone_dtt_rx_days_list,              ref_dtt_rx_days) # Days from DTT to Rx
            rxdone_clockstop_days_predtt_list  <<- append(rxdone_clockstop_days_predtt_list,    clockstoppedDaysPreDTT)
            rxdone_clockstop_days_postdtt_list <<- append(rxdone_clockstop_days_postdtt_list,   clockstoppedDaysPostDTT)
            rxdone_clockstop_reason_list       <<- append(rxdone_clockstop_reason_list,         clockstoppedReason)
            rxdone_operator1_list              <<- append(rxdone_operator1_list,                operatorString1)
            rxdone_operator2_list              <<- append(rxdone_operator2_list,                operatorString2)
            rxdone_operator3_list              <<- append(rxdone_operator3_list,                operatorString3)
            rxdone_anaesthetist1_list          <<- append(rxdone_anaesthetist1_list,            anaesthetistString1)
            rxdone_anaesthetist2_list          <<- append(rxdone_anaesthetist2_list,            anaesthetistString2)
            rxdone_anaesthetist3_list          <<- append(rxdone_anaesthetist3_list,            anaesthetistString3)
            rxdone_postcode_list               <<- append(rxdone_postcode_list,                 postcode)
            rxdone_freetext_list               <<- append(rxdone_freetext_list,                 rxFreeText)
          }
        }
        else
        {
          # We have not treated so we can use the difference from current date and referral
          ptRefTreated = F
          ref_rx_days <- difftime(Sys.Date(), ref_date, units = "days")
          
          if (!is.na(clockstoppedDaysPreDTT)) {
            ref_rx_days <- ref_rx_days + clockstoppedDaysPreDTT  # Remember clockstoppedDaysPreDTT is -ve
          }
          
          if (!is.na(clockstoppedDaysPostDTT)) {
            ref_rx_days <- ref_rx_days + clockstoppedDaysPostDTT  # Remember clockstoppedDaysPostDTT is -ve
          }
          
          # Add to the treatment waiting table
          # Note that 'Multiple Organs' is phased out now, as we are more specific e.g. Liver+Lung in Castor
          if (organForRx == "Liver+Lung" | organForRx == "Multiple Organs")
          {
            organsToAdd <- c("Lung", "Liver") # FIXME all our multiples have been lung and liver but suggest change drop down to Lung+Liver and being cleverer
          }
          else
          {
            organsToAdd <- c(organForRx)
          }
          # Use this trick to add multiple organs as separate rows, with everything the same except the organ itself
          for (organ in organsToAdd)
          {
            rxwait_organ_list                  <<- append(rxwait_organ_list,                  organ)
            rxwait_pt_list                     <<- append(rxwait_pt_list,                     paste(ptID, "-", iRef, sep = ""))
            rxwait_ref_date_list               <<- append(rxwait_ref_date_list,               ref_date)
            rxwait_tci_date_list               <<- append(rxwait_tci_date_list,               ref_tci_date)
            rxwait_tci_status_list             <<- append(rxwait_tci_status_list,             ref_tci_status)
            rxwait_dtt_date_list               <<- append(rxwait_dtt_date_list,               ref_dtt_date)
            rxwait_dtt_days_list               <<- append(rxwait_dtt_days_list,               ref_dtt_days)       # Days from Ref to DTT
            rxwait_days_list                   <<- append(rxwait_days_list,                   ref_rx_days)        # Days from Ref to Today
            rxwait_clockstop_days_predtt_list  <<- append(rxwait_clockstop_days_predtt_list,  clockstoppedDaysPreDTT)
            rxwait_clockstop_days_postdtt_list <<- append(rxwait_clockstop_days_postdtt_list, clockstoppedDaysPostDTT)
            rxwait_clockstop_reason_list       <<- append(rxwait_clockstop_reason_list,       clockstoppedReason)
          }
        }
      } 
      else 
      {
        # Patient is not for treatment, if they have a treatment date specified this is a data integrity issue to log - they are still ignored from treatment data...
        ref_rx_date <- convertToDate(getDataEntry(paste("anaes_date_", as.integer(iRef), sep = ""), i))
        if (!is.na(ref_rx_date))
        {
          logger(paste('Converting to date',getDataEntry(paste("anaes_date_", as.integer(iRef), sep = ""), i)), TRUE)
          notForRxButRxdCount <<- notForRxButRxdCount+1
          addDataIntegrityError(ptID, refID=paste(iRef, "/", pt_ref_count, sep=""), date=ref_rx_date, error=paste("Referral info states patient is not for treatment but yet this referral has a treatment date - ignored from treatment data.", sep = ""))
        }
      }
    } # FIX 4: This correctly ends the for loop for each referral for this patient
    
    # Add a data integrity catch for each entry where the last imaging date is before the first treatment date, as this doesn't make sense
    if (!is.na(date_of_first_rx) &&
        !is.na(date_of_last_imaging_fu) &&
        date_of_last_imaging_fu < date_of_first_rx)
    {
      addDataIntegrityError(
        ptID,
        date = date_of_last_imaging_fu,
        error = paste(
          "Last imaging follow-up date (",
          date_of_last_imaging_fu,
          ") is before first treatment date (",
          date_of_first_rx,
          ").",
          sep = ""
        )
      )
    }
  
    # Now we can do the survival & local tumour progression analysis as we have been through ALL referrals...
    # We only care about recording survival & local tumour progression if we have treated the patient
    if (!is.na(date_of_first_rx))
    {
      # If we don't have a date for when they are last alive, we can at least assume they were alive on the day we first treated them
      if (is.na(last_alive_date))
      {
        last_alive_date <- date_of_first_rx
      }
      
      # Set the survival status
      # Censored observations are subjects who either die of causes other than the disease of interest or are lost to follow-up
      # https://thriv.github.io/biodatasci2018/r-survival.html
      if (deceased == 1)
      {
        survival_overall_status <- 2
        if (deceased_related == 0)
        {
          survival_cancer_specific_status <- 0   # Censored as died of something else
        }
        else
        {
          survival_cancer_specific_status <- 1   # Dead due to cancer at the time of death - these are the only ones which count on Kaplan-Meyer
          # Note the use of 0/1 rather than 1/2 - if we use 1/2 and there are no cancer deaths then the package thinks they are ALL CR-deaths!
        }
        if (!is.na(deceased_date))
        {
          survival_days <- as.numeric(difftime(deceased_date, date_of_first_rx, units = "days"), units = "days")
        }
        else
        {
          # If deceased but no valid deceased date, log it and use current date as deceased date as best guess
          survival_days <- as.numeric(difftime(Sys.Date(), date_of_first_rx, units = "days"), units = "days")
          addDataIntegrityError(ptID, date=deceased_date, error=paste("Patient is deceased but date of death not specified or valid, thus defaulting to today.", sep = ""))
        }
      }
      else
      {
        # The patient is still alive ...
        survival_cancer_specific_status <- 0
        survival_overall_status <- 1   # Censored after the point of last clinical or imaging follow-up, there is no other option for status it is dead or censored
        if (!is.na(last_alive_date))
        {
          survival_days <- as.numeric(difftime(last_alive_date, date_of_first_rx, units = "days"), units = "days")
        }
        else
        {
          # If there is no record of death but we don't have any follow-up we should assume alive I don't think...
          survival_days <- NA
        }
      }
      
      # If you have cancer... 'Dead or alive, you are coming with me...'
      if (!is.na(diagnosis_type) && diagnosis_type != "B")
      {
        # Get the local tumour progression days i.e. if there is local tumour progression, how long did it take after first treatment?
        ltp_days <- NA
        if (!is.na(date_of_first_ltp) && isConvertibleToDate(date_of_first_ltp))
        {
          # FIXME, if there are two treatments before the first LTP is true, then this date is still from 1st not 2nd Rx
          ltp_days = as.numeric(difftime(date_of_first_ltp, date_of_first_rx, units = "days"), units = "days")
        }
        else
        {
          # Otherwise the point of censoring is the last imaging date...
          if (isConvertibleToDate(date_of_last_imaging_fu))
          {
            ltp_days = as.numeric(difftime(date_of_last_imaging_fu, date_of_first_rx, units = "days"), units = "days")
          }
        }
        
        # Set the local tumour progression-free survival days and status
        if (!is.na(date_of_first_ltp))
        {
          ltpf_os_survival_days   <- ltp_days
          ltpf_os_survival_status <- 2   # Recurred, treated like death
          ltpf_cs_survival_days   <- ltp_days
          ltpf_cs_survival_status <- 2   # Recurred, treated like death
        }
        else if (deceased == 1)
        {
          if (!is.na(ltp_days) && survival_days < ltp_days)
          {
            addDataIntegrityError(ptID, date=date_of_first_ltp, error = paste("Patient appears to have died before local tumour progression date.", sep = ""))
          }
          
          ltpf_os_survival_days <- survival_days
          ltpf_os_survival_status <- 2     # Dead, treated like local tumour progression
          ltpf_cs_survival_days <- survival_days
          if (deceased_related == 0)
          {
            ltpf_cs_survival_status <- 1   # Dead but not of cancer cause, censor this
          }
          else
          {
            ltpf_cs_survival_status <- 2   # Dead of cancer cause, treated same as local tumour progression (it counts)
          }
        }
        else
        {
          ltpf_os_survival_days <- survival_days
          ltpf_os_survival_status <- 1   # Censored at point of last follow-up
          ltpf_cs_survival_days <- survival_days
          ltpf_cs_survival_status <- 1   # Censored at point of last follow-up
        }
      }
      
      if (Sys.getenv("DEBUG_MODE") == T)
      {
        logger(paste("     Survival summary: first-rx=", date_of_first_rx,
                   " survival-days=", survival_days,
                   dplyr::if_else(deceased == 1, " (deceased)", " (alive)", missing = " (alive)"),
                   sep = ""))
      }
      
      # Now we have been through all the referrals update the survival data...
      age_at_first_rx <- NA
      if (!is.na(birth_year))
      {
        age_at_first_rx <- as.integer(format(asDateWithOrigin(date_of_first_rx), "%Y")) - birth_year
      }
      survival_pt_list              <<- append(survival_pt_list, ptID)
      survival_sex_list             <<- append(survival_sex_list, sex)
      survival_age_list             <<- append(survival_age_list, age_at_first_rx)
      survival_organ_list           <<- append(survival_organ_list, survival_organ)
      survival_max_tumour_size_list <<- append(survival_max_tumour_size_list, survival_max_tumour_size)  # FIX 1: correct variable name and value
      survival_diagnosis_type_list  <<- append(survival_diagnosis_type_list, diagnosis_type)
      survival_diagnosis_1o_list    <<- append(survival_diagnosis_1o_list,   diagnosis_1o)
      survival_diagnosis_2o_list    <<- append(survival_diagnosis_2o_list,   diagnosis_2o)
      survival_diagnosis_bn_list    <<- append(survival_diagnosis_bn_list,   diagnosis_bn)
      survival_diagnosis_un_list    <<- append(survival_diagnosis_un_list,   diagnosis_un)
      survival_first_rx_date        <<- append(survival_first_rx_date, date_of_first_rx)
      suvival_rx_modalities         <<- append(suvival_rx_modalities, paste(modalityListForThisPatient, collapse="," )) # This is a list of all the modalities the patient has been treated with
      survival_deceased_list        <<- append(survival_deceased_list, as.integer(deceased))
      survival_deceased_date        <<- append(survival_deceased_date, deceased_date)
      survival_last_alive_list      <<- append(survival_last_alive_list, last_alive_date)
      survival_days_list            <<- append(survival_days_list, survival_days)
      survival_deceased_related     <<- append(survival_deceased_related, deceased_related)
      survival_lost_to_fu           <<- append(survival_lost_to_fu, lost_to_fu)
      survival_lost_to_fu_date      <<- append(survival_lost_to_fu_date, lost_to_fu_date)
      survival_overall_status_list  <<- append(survival_overall_status_list, survival_overall_status)
      survival_cancer_specific_status_list <<- append(survival_cancer_specific_status_list, survival_cancer_specific_status)      
      ltpf_os_survival_days_list    <<- append(ltpf_os_survival_days_list, ltpf_os_survival_days)        # Local tumour progression-free overall survival i.e. time to LTP or death of any cause
      ltpf_os_survival_status_list  <<- append(ltpf_os_survival_status_list, ltpf_os_survival_status)
      ltpf_cs_survival_days_list    <<- append(ltpf_cs_survival_days_list, ltpf_cs_survival_days)        # Local tumour progression-free cancer specific survival i.e. time to LTP or cancer specific death
      ltpf_cs_survival_status_list  <<- append(ltpf_cs_survival_status_list, ltpf_cs_survival_status)
      
      # And the local tumour progression data if it is a malignant indication
      #if (!is.na(indication) & indication == "Malignant")
      if (is.na(indication) | indication == "Malignant") # FIXME this is temporary until we define the malignant ones in imaging FU in EDC
      {
        ltp_list                     <<- append(ltp_list, ltp)
        ltp_status_list              <<- append(ltp_status_list, ltp_status)
        ltp_date_list                <<- append(ltp_date_list, date_of_first_ltp)
        ltp_no_rx_before             <<- append(ltp_no_rx_before, no_rx_before_first_ltp)
        ltp_days_list                <<- append(ltp_days_list, ltp_days)
        last_imaging_follow_up_list  <<- append(last_imaging_follow_up_list, date_of_last_imaging_fu)
      }
    }
  } # This ends the for loop for each patient...
  
  # Do the post-processing
  postProcessData()
}

# Parse 'Lesion(s) with new RD/LTP (Rx.Tumour e.g. 3.1, 4.3)' field
# Input:  string like "3.1", "3.1, 4.3", "", or NA
# Output: data frame with columns ref_no (integer), lesion_no (integer)
#         Empty data frame if no LTP lesions specified
#
parseltpListString <- function(ltpListStr)
{
  emptyResult <- data.frame(ref_no = integer(0), lesion_no = integer(0))
  
  if (is.null(ltpListStr) || is.na(ltpListStr) || trimws(ltpListStr) == "")
  {
    return(emptyResult)
  }
  
  tokens <- trimws(unlist(strsplit(ltpListStr, ",")))
  tokens <- tokens[tokens != ""]
  
  results <- lapply(tokens, function(token)
  {
    if (grepl("^[0-9]+\\.[0-9]+$", token))
    {
      parts <- strsplit(token, "\\.")[[1]]
      data.frame(
        ref_no    = as.integer(parts[1]),
        lesion_no = as.integer(parts[2])
      )
    }
    else
    {
      logger(paste("WARNING: parseltpListString() could not parse token '", token, 
                   "' in '", ltpListStr, "' - expected format e.g. '3.1' or '3.1, 4.3'", sep = ""), logOnlyAsDebug = TRUE)
      emptyResult
    }
  })
  
  do.call(rbind, results)
}

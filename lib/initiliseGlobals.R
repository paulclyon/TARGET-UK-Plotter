# initialiseGlobals() sets all of the default values
initialiseGlobals <- function()
{
  clockStopColNames              <<- c("date.stopped","date.restart","reason","free.text")   # Order of the clock stop columns
  rxTableColNames                <<- c("rx.organ","lesion.size","max.depth","closest.serosa","punctures","no.applicators.or.postitions","modality","max.power","min.power","rx.time","new.or.rec","free.text")
  aeTableColNames                <<- c("PtID","Organ","Complication","DateofOnset","DateofResolution","Grade","Description","Intervention","InterventionDate","AdditionalHopsitalDays","PostDischarge","Duration")
  dataIntegrityColNames          <<- c("PtID","RefID","Date","Organ(s)","Error")
  loggerColNames                 <<- c("TimeStamp","IsError","Message")
  
  newRecurrenceMatrix <<- T
  if (newRecurrenceMatrix == T)
  {
    # FIXME : new table
    recurrenceColNames           <<- c("imaging.date","exam.type","organ","ltp","ltp.list","treatment.for.ltp","new.in.target.organ","distant.progression","disease.status")
  }
  else
  {
    # FIXME : old table
    recurrenceColNames           <<- c("imaging.date","exam.type","ltp","new.in.target.organ","distant.progression","disease.status","ablated.organ")
  }
  
  clinicalfuColNames             <<- c("followup.date","clinician.type","clinician.name","impression","outcome")
  rxdone_diagnosis_type_list     <<- c()
  rxdone_diagnosis_1o_list       <<- c()
  rxdone_diagnosis_2o_list       <<- c()
  rxdone_diagnosis_bn_list       <<- c()
  rxdone_diagnosis_un_list       <<- c()
  rxdone_organ_list              <<- c()
  rxdone_modality_list           <<- c()
  rxdone_max_tumour_size_list    <<- c()
  rxdone_sex_list                <<- c()
  rxdone_tariff_list             <<- c()
  rxdone_pt_list                 <<- c()
  rxdone_refdate_list            <<- c()
  rxdone_dttdate_list            <<- c()
  rxdone_rxdate_list             <<- c()
  rxdone_dtt_days_list           <<- c()
  rxdone_rx_days_list            <<- c()
  rxdone_dtt_rx_days_list        <<- c()
  rxdone_operator1_list          <<- c()
  rxdone_operator2_list          <<- c()
  rxdone_operator3_list          <<- c()
  rxdone_anaesthetist1_list      <<- c()
  rxdone_anaesthetist2_list      <<- c()
  rxdone_anaesthetist3_list      <<- c()
  rxdone_postcode_list           <<- c()
  rxdone_freetext_list           <<- c()
  rxwait_organ_list              <<- c()
  rxwait_pt_list                 <<- c()
  rxwait_ref_date_list           <<- c()
  rxwait_tci_date_list           <<- c()
  rxwait_tci_status_list         <<- c()
  rxwait_dtt_days_list           <<- c()
  rxwait_dtt_date_list           <<- c()
  rxwait_days_list               <<- c()
  rxwait_clockstop_days_predtt_list  <<- c()
  rxwait_clockstop_days_postdtt_list <<- c()
  rxwait_clockstop_reason_list       <<- c()
  rxdone_clockstop_days_predtt_list  <<- c()
  rxdone_clockstop_days_postdtt_list <<- c()
  rxdone_clockstop_reason_list   <<- c()
  survival_pt_list               <<- c()
  survival_sex_list              <<- c()
  survival_age_list              <<- c()
  survival_organ_list            <<- c()
  survival_diagnosis_type_list   <<- c()
  survival_diagnosis_1o_list     <<- c()
  survival_diagnosis_2o_list     <<- c()
  survival_diagnosis_bn_list     <<- c()
  survival_diagnosis_un_list     <<- c()
  survival_first_rx_date         <<- c()
  suvival_rx_modalities          <<- c()
  survival_max_tumour_size_list  <<- c()
  survival_days_list             <<- c()
  survival_deceased_list         <<- c()
  survival_deceased_date         <<- c()
  survival_deceased_related      <<- c()
  survival_lost_to_fu            <<- c()
  survival_lost_to_fu_date       <<- c()
  survival_overall_status_list   <<- c()
  survival_cancer_specific_status_list <<- c()  
  survival_last_alive_list       <<- c() # This is the last clinical follow-up date or last imaging date, whatever is later
  ltp_list                       <<- c()
  ltp_status_list                <<- c()
  ltp_date_list                  <<- c()
  ltp_days_list                  <<- c()
  ltp_no_rx_before               <<- c()
  ltpf_os_survival_days_list     <<- c() # Local Tumour Progression-free overall survival days
  ltpf_os_survival_status_list   <<- c() # Local Tumour Progression-free overall survival status
  ltpf_cs_survival_days_list     <<- c() # Local Tumour Progression-free cancer specific survival days
  ltpf_cs_survival_status_list   <<- c() # Local Tumour Progression-free cancer specific survival status
  ltp_perlesion_ptid_list        <<- c()
  ltp_perlesion_refno_list       <<- c()
  ltp_perlesion_lesionno_list    <<- c()
  ltp_perlesion_date_list        <<- c()
  last_imaging_follow_up_list    <<- c()
  operator1                      <<- NA
  operator2                      <<- NA
  operator3                      <<- NA
  operator1Factors               <<- c()
  operator2Factors               <<- c()
  operator3Factors               <<- c()
  operatorAllFactors             <<- c()
  anaesthetist1                  <<- NA
  anaesthetist2                  <<- NA
  anaesthetist3                  <<- NA
  anaesthetist1Factors           <<- c()
  anaesthetist2Factors           <<- c()
  anaesthetist3Factors           <<- c()
  anaesthetistAllFactors         <<- c()
  cctaeGradeFactors              <<- c()
  
  # The major dataframes
  rxDoneData                     <<- NA
  rxWaitData                     <<- NA
  #monthlyRxWaitData              <<- NA
  
  monthlyRxWaitData <<- data.frame(
    MonthStart    = as.Date(character()),
    Counts        = integer(),
    Type          = character(),
    OnWaitingList = integer()
  )
  
  refclockstop.df                <<- NA
  recurrence.df                  <<- NA
  clinicalfu.df                  <<- NA
  refTciCalendar                 <<- NA
  rxdonePlot                     <<- ggplot()
  rxwaitPlot                     <<- ggplot()
  monthlyWaitingPlot             <<- ggplot()
  spcRxTimePlot                  <<- ggplot()
  operatorPlot                   <<- ggplot()
  patientData                    <<- NA
  studyData                      <<- NA
  cancerPerPatientData           <<- NA
  cancerPerLesionData            <<- NA
  benignData                     <<- NA
  aeData                         <<- NA
  survivalPlotSex                <<- ggplot()
  survivalPlotOrgan              <<- ggplot()
  survivalFitSex                 <<- NA
  survivalFitOrgan               <<- NA
  targetStudyID                  <<- NA
  fieldData                      <<- NA
  referralMap                    <<- NULL
  dataIntegrity.df               <<- NA
  notForRxButRxdCount            <<- 0
  noRefsProcessed                <<- 0
  
  # The Open API stuff is used to write to Castor...
  castorOpenAPI                  <<- NA
  studyDataOpenAPI               <<- NA
  fieldDataOpenAPI               <<- NA
  openAPIoperations              <<- NA
  openAPIschemas                 <<- NA
  
  # Only clear logger.df once as whilst the process runs the log gets longer...
  # We have to initialise logger early otherwise we can't log the start up!
  logger.df <<- data.frame(matrix(ncol = length(loggerColNames), nrow = 0))
  colnames(logger.df) <<- loggerColNames
  
  logger("System information:")
  logger(Sys.info())
  if (Sys.getenv("DEBUG_MODE") == TRUE) logger(paste("Working Directory: ",getwd()))
  shinyVersion <- Sys.getenv("SHINY_SERVER_VERSION")
  if (!is.na(shinyVersion) && shinyVersion != "")
  {
    logger(paste("Running in a docker! Shiny server version = ", Sys.getenv("SHINY_SERVER_VERSION"),sep=""))
    isDocker <<- T
  } else {
    isDocker <<- F
    logger("Running on a native machine, not a docker")
  }
  
  # The HRG Codes for tariff calculations based on 2022/3 Workbook
  # Note if its a microwave and the organ is other or multiple, assume same as a liver microwave
  tariffCodes <<- data.frame (
    Code =       c("YD01Z",      "Lung_Cryo",   "YG01A",     "YG01B",     "YL02Z",     "YL01Z",       "YH20Z",     "YH20Z",       "XXX",               "YYY"),
    Organ =      c("Lung",       "Lung",        "Liver",     "Liver",     "Kidney",    "Kidney",      "Bone",      "Bone",        "Other/Unspecified", "Multiple Organs"),
    Modality =   c("Microwave",  "Cryotherapy", "Microwave", "Microwave", "Microwave", "Cryotherapy", "Microwave", "Cryotherapy", "Microwave",         "Microwave"),
    MaxCCScore = c(20,           20,             20,          1,           20,          20,            20,          20,            20,                 20),
    MinCCScore = c( 0,           0,              2,           0,           0,           0,             0,           0,             0,                  0),
    Tariff =     c(4703,         0,              6564,        5254,        4669,        5253,          2029,        2029,          5254,               5254),
    Description = c("Percutaneous Ablation of Lesion of Respiratory Tract (MWA)",
                    "Percutaneous Ablation of Lesion of Respiratory Tract (Cryo)",
                    "Percutaneous Ablation of Lesion of, Liver or Pancreas, with CC Score 2+",
                    "Percutaneous Ablation of Lesion of, Liver or Pancreas, with CC Score 0-1",
                    "Standard Percutaneous Ablation of Lesion of Kidney (MWA)",
                    "Complex Percutaneous Ablation of Lesion of Kidney (Cryo)",
                    "Bone ablation (MWA)",
                    "Bone ablation (Cryo)",
                    "Other/Unspecified (MWA)",
                    "Multiple Organs (MWA)"
                    )
  )
}

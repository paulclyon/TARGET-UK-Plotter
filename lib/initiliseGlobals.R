#' initialiseGlobals sets all of the default values
initialiseGlobals <- function()
{
  # This is the order of the clock stop columns
  clockStopColNames              <<- c("date.stopped","date.restart","reason","free.text")
  rxTableColNames                <<- c("rx.organ","lesion.size","max.depth","closest.serosa","punctures","no.applicators.or.postitions","modality","max.power","min.power","rx.time","new.or.rec","free.text")
  recurrenceColNames             <<- c("imaging.date","exam.type","local.recurrence","new.in.target.organ","distant.progression","disease.status","free-text")
  clinicalfuColNames             <<- c("followup.date","clinician.type","clinician.name","impression","outcome")
  rxdone_organ_list              <<- c()
  rxdone_sex_list                <<- c()
  rxdone_modality_list           <<- c()
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
  rxwait_organ_list              <<- c()
  rxwait_pt_list                 <<- c()
  rxwait_ref_date_list           <<- c()
  rxwait_provisional_rxdate_list <<- c()
  rxwait_dtt_days_list           <<- c()
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
  survival_first_rx_date         <<- c()
  survival_days_list             <<- c()
  survival_deceased_list         <<- c()
  survival_deceased_date         <<- c()
  survival_deceased_related      <<- c()
  survival_lost_to_fu            <<- c()
  survival_lost_to_fu_date       <<- c()
  survival_status_list           <<- c()
  survival_last_alive_list       <<- c() # This is the last clinical follow-up date or last imaging date, whatever is later
  local_recurrence_list          <<- c()
  local_recurrence_status_list   <<- c()
  local_recurrence_date_list     <<- c()
  local_recurrence_days_list     <<- c()
  lrf_survival_days_list         <<- c() # Local recurrence-free survival days
  lrf_survival_status_list       <<- c() # Local recurrence-free survival status
  last_imaging_follow_up_list    <<- c()
  survivalData                 <<- NA
  # recurrenceData               <<- NA
  operator1                    <<- NA
  operator2                    <<- NA
  operator3                    <<- NA
  operator1Factors             <<- c()
  operator2Factors             <<- c()
  operator3Factors             <<- c()
  operatorAllFactors           <<- c()
  anaesthetist1                <<- NA
  anaesthetist2                <<- NA
  anaesthetist3                <<- NA
  anaesthetist1Factors         <<- c()
  anaesthetist2Factors         <<- c()
  anaesthetist3Factors         <<- c()
  anaesthetistAllFactors       <<- c()
  
  # The major dataframes
  rxDoneData                   <<- NA
  rxWaitData                   <<- NA
  refclockstop.df              <<- NA
  recurrence.df                <<- NA
  clinicalfu.df                <<- NA
  rxdonePlot                   <<- ggplot()
  rxwaitPlot                   <<- ggplot()
  operatorPlot                 <<- ggplot()
  patientData                  <<- NA
  studyData                    <<- NA
  survivalPlotSex              <<- ggplot()
  survivalPlotOrgan            <<- ggplot()
  survivalFitSex               <<- NA
  survivalFitOrgan             <<- NA
  targetStudyID                <<- NA
  fieldData                    <<- NA
  referralMap                  <<- NULL
  
  # The Open API stuff is used to write to Castor...
  castorOpenAPI                <<- NA
  studyDataOpenAPI             <<- NA
  fieldDataOpenAPI             <<- NA
  openAPIoperations            <<- NA
  openAPIschemas               <<- NA
  
  # The HRG Codes for tariff calculations based on 2022/3 Workbook
  tariffCodes <<- data.frame (
    Code =       c("YD01Z",      "Lung_Cryo",   "YG01A",     "YG01B",     "YL02Z",     "YL01Z",       "YH20Z",     "YH20Z"),
    Organ =      c("Lung",       "Lung",        "Liver",     "Liver",     "Kidney",    "Kidney",      "Bone",      "Bone"),
    Modality =   c("Microwave",  "Cryotherapy", "Microwave", "Microwave", "Microwave", "Cryotherapy", "Microwave", "Cryotherapy"),
    MaxCCScore = c(20,           20,             20,          1,           20,          20,            20,          20),
    MinCCScore = c( 0,           0,              2,           0,           0,           0,             0,           0),
    Tariff =     c(4703,         0,              6564,        5254,        4669,        5253,          2029,        2029),
    Description = c("Percutaneous Ablation of Lesion of Respiratory Tract (MWA)",
                    "Percutaneous Ablation of Lesion of Respiratory Tract (Cryo)",
                    "Percutaneous Ablation of Lesion of, Liver or Pancreas, with CC Score 2+",
                    "Percutaneous Ablation of Lesion of, Liver or Pancreas, with CC Score 0-1",
                    "Standard Percutaneous Ablation of Lesion of Kidney (MWA)",
                    "Complex Percutaneous Ablation of Lesion of Kidney (Cryo)",
                    "Bone ablation (MWA)",
                    "Bone ablation (Cryo)")
  )
  
  logger("System information:")
  logger(Sys.info(), TRUE)
}
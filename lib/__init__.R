# __init__.R contains common functions that other function files may require.

# logger function if stderr = TRUE then it goes to stderr, otherwise stdout, may want to update this to use the ShinyApp
logger <- function(msg, stderr=FALSE) {
  if (Sys.getenv("DEBUG_MODE") == T)
  {
    if (!is.na(stderr) && stderr!=T)
    {
      write(toString(msg),stdout())
    }
    else
    {
      write(toString(msg),stderr())
    }
  }
  return()
}

# This is a wrapper for the as.Date function but it sets the origin
# This is because on some OS e.g. Linux, R may be more strict and fall over if origin is not set
asDateWithOrigin <- function(thisDate)
{
  as.Date(thisDate, origin = '1970-01-01')
}

# initialiseGlobals sets all of the default values

initialiseGlobals <- function() {
  
  # This is the order of the clock stop columns
  clockStopColNames              <<- c("date.stopped","date.restart","reason","free.text")
  rxTableColNames                <<- c("rx.organ","lesion.size","max.depth","closest.serosa","punctures","no.applicators.or.postitions","modality","max.power","min.power","rx.time","new.or.rec","free.text")
  recurrenceColNames             <<- c("imaging.date","exam.type","local.recurrence","new.in.target.organ","distant.progression","disease.status","free-text")
  rxdone_organ_list              <<- c()
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
  local_recurrence_list          <<- c()
  local_recurrence_date_list     <<- c()
  local_recurrence_days_list     <<- c()
  survivalData                 <<- NA
  recurrenceData               <<- NA
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
  organFactors                 <<- c()
  organCounts                  <<- c()
  organPercents                <<- c()
  Organs                       <<- NA
  OrganPercents                <<- NA
  organPie.df                  <<- NA
  rxDoneData                   <<- NA
  rxWaitData                   <<- NA
  refclockstop.df              <<- NA
  recurrence.df                <<- NA
  rxdonePlot                   <<- ggplot()
  rxwaitPlot                   <<- ggplot()
  operatorPlot                 <<- ggplot()
  rxdonePie                    <<- ggplot()
  rxwaitPie                    <<- ggplot()
  patientData                  <<- NA
  studyData                    <<- NA
  survivalPlotSex              <<- ggplot()
  survivalPlotOrgan            <<- ggplot()
  survivalFitSex               <<- NA
  survivalFitOrgan             <<- NA
  targetStudyID                <<- NA
  fieldData                    <<- NA
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

# Populate monthlyRxWaitData which is basically a list of patients on the waiting list each month
processMonthlyRxWaitingList <- function(startDate,endDate,organs)
{
  showNotification("Generating treeatment waiting list...")
  firstRefDate <- min(c(rxWaitData$RefDate, rxDoneData$RefDate), na.rm=T)
  lastRefDate  <- max(c(rxWaitData$RefDate, rxDoneData$RefDate), na.rm=T)
  if (isConvertibleToDate(endDate)) lastRefDate  <- min(lastRefDate,convertToDate(endDate))
  
  # Combine the rxDone and rxWait data frames into allDates data frame
  allRefDates <- c(rxDoneData$RefDate, rxWaitData$RefDate)
  allRxDates  <- c(rxDoneData$RxDate, rep(NA, length(rxWaitData$RefDate)))
  allOrgans   <- c(rxDoneData$Organs, rxWaitData$Organs)
  allDates    <- data.frame(refDate=allRefDates, rxDate=allRxDates, Organs=allOrgans)
  
  # Loose records with ref date of NA or outside the end date bound provided, if provided
  allDates <- allDates %>% filter(!is.na(refDate))
  if (isConvertibleToDate(endDate))   allDates <- allDates %>% filter(refDate <= convertToDate(endDate))

  # Filter out only the organs we are interested in
  if (length(organs)>0)
  {
    allDates <- allDates %>% filter(Organs %in% organs)
  }
  
  # Go through each month for which we have referral data one by one
  monthlyWaitingListDates  <- c()
  monthlyRefCounts         <- c()
  monthlyRxCounts          <- c()
  monthlyWaitingTotal      <- c()
  
  if (nrow(allDates)>0)
  {
    waitingListCount <- 0
    monthSpan <- interval(firstRefDate, lastRefDate) %/% months(1) + 1
    for (m in 1:monthSpan)
    {
      lastMonthsDate <- convertToDate(format(min(allDates$refDate) %m+% months(m-1), "01-%m-%Y"))
      thisMonthsDate <- convertToDate(format(min(allDates$refDate) %m+% months(m),   "01-%m-%Y"))
      refCount <- 0
      rxCount  <- 0
      
      for (j in 1:nrow(allDates))
      {
        if (allDates$refDate[j] >= lastMonthsDate && allDates$refDate[j] < thisMonthsDate)
        {
          refCount <- refCount + 1
          waitingListCount <- waitingListCount + 1
        }
        if (!is.na(allDates$rxDate[j]) && allDates$rxDate[j] >= lastMonthsDate && allDates$rxDate[j] < thisMonthsDate)
        {
          rxCount <- rxCount + 1
          waitingListCount <- waitingListCount - 1          
        }
      }
      monthlyWaitingListDates  <- c(monthlyWaitingListDates, convertToDate(thisMonthsDate))
      monthlyRefCounts         <- c(monthlyRefCounts, refCount)
      monthlyRxCounts          <- c(monthlyRxCounts, rxCount)
      monthlyWaitingTotal      <- c(monthlyWaitingTotal, waitingListCount)
    }
  }
  
  monthlyRxWaitData <<- data.frame(
    MonthStart    = as.Date(monthlyWaitingListDates),
    Referred      = monthlyRefCounts,
    Treated       = monthlyRxCounts,
    OnWaitingList = monthlyWaitingTotal)

  # Whilst we could do this upfront like endDate the difficulty is it won't know about referrals made before the cut off
  # start date which is an important offset as it affects everything moving forward, and it would underrepresent the true
  # waiting list, hence only loose the ones at the start once we have the referral data...
  startPlotDate <- floor_date(convertToDate(startDate),unit="month")-as.difftime(1,units="days")
  if (isConvertibleToDate(startDate)) monthlyRxWaitData <<- monthlyRxWaitData %>% filter(MonthStart >= startPlotDate)
}

makeRxDonePlot <- function(startDate, endDate, organs)
{
  # Filter the dates and organs
  rxDoneData.filtered <- rxDoneData
  if (isConvertibleToDate(startDate))
  {
    rxDoneData.filtered <- rxDoneData.filtered %>% filter(RxDate >= convertToDate(startDate))
  }
  if (isConvertibleToDate(endDate))
  {
    rxDoneData.filtered <- rxDoneData.filtered %>% filter(RxDate <= convertToDate(endDate))
  }
  if (length(organs)>0)
  {
    rxDoneData.filtered <- rxDoneData.filtered %>% filter(Organs %in% organs)
  }
  if (!is.null(nrow(rxDoneData.filtered)))
  {
    rxdonePlotColors <<- c(
      "Ref to DTT"           = "red",
      "DTT to Rx"            = "yellow",
      "Ref to Rx"            = "green",
      "Clock Stops Pre-DTT"  = "purple",
      "Clock Stops Post-DTT" = "cyan",
      "Operator1"            = "blue",
      "Waiting List"         = "orange"
    )
    rxdonePlot <<- ggplot(rxDoneData.filtered, aes(x = RxDate, text = paste(ID, " (", Organs, ")\n", ClockStopWhy, sep = ""))) +
      geom_point(aes(y = Ref_DTT, color = "Ref to DTT"), size=1) +
      geom_point(aes(y = DTT_Rx, color = "DTT to Rx"), size=1) +
      geom_point(aes(y = Ref_RxDone, color = "Ref to Rx"), size=1) +
      geom_point(aes(y = ClockStopDaysPreDTT, color = "Clock Stops Pre-DTT"), size=1) +
      geom_point(aes(y = ClockStopDaysPostDTT, color = "Clock Stops Post-DTT"), size=1) +
      theme(legend.position = "bottom") +
      scale_color_manual(values = rxdonePlotColors) +
      guides(color = guide_legend("Treated Patients:\n")) +
      labs(y = "Number of Days") +
      labs(title = paste0("Time to Treatment (Generated ",format(Sys.time(), "%a %b %d %Y %X"),")")) +
      theme(plot.title = element_text(size = 10)) +
      scale_x_date(date_breaks = "month", date_labels = "%b-%y") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  else
  {
    rxdonePlot <<- NA
  }
  rxdonePlot
}

makeRxWaitPlot <- function(startDate, endDate, organs)
{
  # Filter the dates and organs
  rxWaitData.filtered <- rxWaitData
  if (isConvertibleToDate(startDate))
  {
    rxWaitData.filtered <- rxWaitData.filtered %>% filter(RefDate >= convertToDate(startDate))
  }
  if (isConvertibleToDate(endDate))
  {
    rxWaitData.filtered <- rxWaitData.filtered %>% filter(RefDate <= convertToDate(endDate))
  }
  if (length(organs)>0)
  {
    rxWaitData.filtered <- rxWaitData.filtered %>% filter(Organs %in% organs)
  }
  if (!is.null(nrow(rxWaitData))) {
    rxwaitPlotColors <<- c(
      "Days to DTT"          = "blue",
      "Days Waiting"         = "red",
      "Clock Stops Pre-DTT"  = "purple",
      "Clock Stops Post-DTT" = "cyan"
    )
    # FIXME: the issue is here if two are referred on same day then they are plotted exactly on top of each other
    # There are different librarys to handle it including jitter, but I had no success
    # The next best work around is to make opacification 0.3 with alpha, so you know if there is one hiding, but tooltiptext doesn't show other PtID
    rxwaitPlot <<- ggplot(rxWaitData.filtered, aes(x = RefDate, text = paste(ID, " (", Organs, ")\n", "Provisional RxDate=", ProvisionalRxDate, ClockStopWhy, sep = ""))) +
      geom_point(aes(y = Ref_DTT, colour = "Days to DTT", alpha=0.2), size=1) +
      geom_point(aes(y = DaysWaiting, colour = "Days Waiting", alpha=0.2), size=1) +
      geom_point(aes(y = ClockStopDaysPreDTT, color = "Clock Stops Pre-DTT"), size=1) +
      geom_point(aes(y = ClockStopDaysPostDTT, color = "Clock Stops Post-DTT"), size=1) +
      theme(legend.position = "bottom") +
      scale_color_manual(values = rxwaitPlotColors) +
      guides(color = guide_legend("Patients with DTT:\n")) +
      labs(y = "Number of Days") +
      labs(title =  paste0("Time on Waiting List (Generated ",format(Sys.time(), "%a %b %d %Y %X"),")")) +
      theme(plot.title = element_text(size = 10)) +
      scale_x_date(date_breaks = "month", date_labels = "%b-%y") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  else
  {
    rxwaitPlot <<- ggplot()
  }
  rxwaitPlot
}

# Plot type is a switch to make this more efficient!
makeOperatorPlot <- function(startDate, endDate, organs)
{
  # Create a custom color scale/palette
  # There are 11 colours in the Spectral palette but if we have more than 11 operqtors, we need to interpolate new colours hence the colour ramp
  opColors <- colorRampPalette(brewer.pal(11, "Spectral"))(length(operator1Factors))
  names(opColors) <- operator1Factors
  opColScale <- scale_fill_manual(name = "Operators", values = opColors)
  
  # The operatorPlot gets subsetted to a single operator in app.R
  operatorPlot <<- ggplot(rxDoneData, aes(
    x = lubridate::floor_date(RxDate, "month"), fill = Operator1,
    text = paste(
      ID, "-", RxDate, "\n",
      paste("Operators: ", Operator1, Operator2, Operator3), "\n",
      paste("Anaesthetists: ", Anaesthetist1, Anaesthetist2, Anaesthetist3)))) +
    geom_bar() +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)) +
    scale_y_continuous(breaks = seq(0, 100, by = 1)) + # Use this to get an integer y-axis, the 100 is the number of max ticks after which x>100 still plots tickless
    # guides(color=guide_legend("1o Operator Legend")) +
    labs(x = "Date", y = "Ablation Count") +
    labs(title = paste0("Primary Operator Logbook (Generated ",format(Sys.time(), "%a %b %d %Y %X"),")")) +
    theme(plot.title = element_text(size = 10)) +
    theme(legend.position = "bottom") +
    opColScale
  
  operatorPlot
}

makeWaitingListPlot <- function(startDate, endDate, organs)
{
  processMonthlyRxWaitingList(startDate, endDate, organs)
  if (!is.null(nrow(monthlyRxWaitData))) {
    monthlyWaitingPlotColors <<- c(
      "Waiting Total"    = "red",
      "Referred"         = "orange",
      "Treated"          = "green"
    )
  monthlyWaitingPlot <<- ggplot(monthlyRxWaitData, aes(x = MonthStart, y = OnWaitingList, color = "Waiting Total")) +
    geom_line(color="black") +
    geom_point(size = 1) +
    geom_point(aes(y = Referred, color = "Referred")) +
    geom_point(aes(y = Treated, color = "Treated")) +
    geom_vline(xintercept = as.numeric(Sys.Date()), 
               color = "purple", linetype = "dotted", size = 0.5) +
    scale_color_manual(values = monthlyWaitingPlotColors) +
    guides(color = guide_legend("Waiting List by Month:\n")) +
    labs(title = paste0("Monthly Treatment Waiting List (Generated ",format(Sys.time(), "%a %b %d %Y %X"),")")) +
    theme(plot.title = element_text(size = 10)) +
    scale_x_date(date_breaks = "month", date_labels = "%b-%y") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  else
  {
    monthlyWaitingPlot <<- ggplot()
  }
  monthlyWaitingPlot
}

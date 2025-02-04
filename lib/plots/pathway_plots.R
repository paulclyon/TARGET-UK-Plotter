# Populate monthlyRxWaitData which is basically a list of patients on the waiting list each month
processMonthlyRxWaitingList <- function(startDate,endDate)
{
  firstRefDate <- min(rxDoneData$RefDate, na.rm=T)
  lastRefDate  <- max(rxDoneData$RefDate, na.rm=T)
  monthSpan    <- interval(firstRefDate, lastRefDate) %/% months(1)
  
  # Combine the rxDone and rxWait data frames into allDates data frame
  allRefDates <- c(rxDoneData$RefDate, rxWaitData$RefDate)
  allRxDates <-  c(rxDoneData$RxDate, rep(NA, length(rxWaitData$RefDate)))
  allDates <- data.frame(refDate=allRefDates, rxDate=allRxDates)
  
  # Loose records with ref date of NA or outside the date bounds provided, if provided
  allDates <- allDates %>% filter(!is.na(refDate))
  if (isConvertibleToDate(startDate)) allDates <- allDates %>% filter(refDate >= convertToDate(startDate))
  if (isConvertibleToDate(endDate))   allDates <- allDates %>% filter(refDate <= convertToDate(endDate))

  # Go through each month for which we have referral data one by one
  monthlyRxWaitDates <- c()
  monthlyRxWaitCounts <- c()
  for (m in 1:monthSpan)
  {
    thisMonthsDate <- convertToDate(format(firstRefDate %m+% months(m), "01-%m-%Y"))
    monthlyRxWaitCount <- 0
    for (j in 1:nrow(rxDoneData))
    {
      if (thisMonthsDate >= as.Date(allRefDates[j]))
      {
        if (is.na(allRxDates[j]) || as.Date(allRxDates[j]) > thisMonthsDate)
        {
          monthlyRxWaitCount <- monthlyRxWaitCount + 1
        }
      }
    }
    monthlyRxWaitDates  <- c(monthlyRxWaitDates, convertToDate(thisMonthsDate))
    monthlyRxWaitCounts <- c(monthlyRxWaitCounts, monthlyRxWaitCount)
  }
  monthlyRxWaitData <<- data.frame(
    MonthStart = as.Date(monthlyRxWaitDates),
    PtsOnWaitingList = monthlyRxWaitCounts)
}

makeRxPathwayPlots <- function(startDate, endDate)
{
  processMonthlyRxWaitingList(startDate, endDate)
  monthlyWaitingPlot <<- ggplot(monthlyRxWaitData, aes(x = MonthStart, y = PtsOnWaitingList)) +
    geom_line()
  
  if (!is.null(nrow(rxDoneData))) {
    rxdonePlotColors <<- c(
      "Ref to DTT"           = "red",
      "DTT to Rx"            = "yellow",
      "Ref to Rx"            = "green",
      "Clock Stops Pre-DTT"  = "purple",
      "Clock Stops Post-DTT" = "cyan",
      "Operator1"            = "blue",
      "Waiting List"         = "orange"
    )
    rxdonePlot <<- ggplot(rxDoneData, aes(x = RxDate, text = paste(ID, " (", Organs, ")\n", ClockStopWhy, sep = ""))) +
      geom_point(aes(y = Ref_DTT, color = "Ref to DTT")) +
      geom_point(aes(y = DTT_Rx, color = "DTT to Rx")) +
      geom_point(aes(y = Ref_RxDone, color = "Ref to Rx")) +
      geom_point(aes(y = ClockStopDaysPreDTT, color = "Clock Stops Pre-DTT")) +
      geom_point(aes(y = ClockStopDaysPostDTT, color = "Clock Stops Post-DTT")) +
      theme(legend.position = "bottom") +
      scale_color_manual(values = rxdonePlotColors) +
      guides(color = guide_legend("Treated Patients...")) +
      labs(y = "Number of Days") +
      ggtitle("Time to Treatment")

    # operatorPlot <<- ggplot(rxDoneData, aes(x=RxDate, text=paste(ID, ' (',paste(Operator1,Operator2,Operator3),')\n'),sep='')) +
    #  geom_point( aes(y=Operator1,      color="Operator1")) +
    #  theme(legend.position="bottom") +
    #  scale_color_manual(values = rxdonePlotColors) +
    #  guides(color=guide_legend("Treated Patients...")) +
    #  labs(y="First Operator (bug in that it doesn't count multiple Rx same day!") +
    #  ggtitle("Treatment Operators")

    # Create a custom color scale/palette
    # There are 11 colours in the Spectral palette but if we have more than 11 operqtors, we need to interpolate new colours hence the colour ramp
    opColors <- colorRampPalette(brewer.pal(11, "Spectral"))(length(operator1Factors))
    names(opColors) <- operator1Factors
    opColScale <- scale_fill_manual(name = "Operators", values = opColors)

    # The operatorPlot gets subsetted to a single operator in app.R
    # operatorPlot <<- ggplot(rxDoneData, aes(x=format(RxDate, "%Y-%m"), text=paste(ID, ' (',paste(Operator1,Operator2,Operator3),')\n'),sep='')) +

    operatorPlot <<- ggplot(rxDoneData, aes(
      x = lubridate::floor_date(RxDate, "month"), fill = Operator1,
      text = paste(
        ID, "-", RxDate, "\n",
        paste("Operators: ", Operator1, Operator2, Operator3), "\n",
        paste("Anaesthetists: ", Anaesthetist1, Anaesthetist2, Anaesthetist3)
      )
    )) +
      geom_bar() +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)) +
      scale_y_continuous(breaks = seq(0, 100, by = 1)) + # Use this to get an integer y-axis, the 100 is the number of max ticks after which x>100 still plots tickless
      # guides(color=guide_legend("1o Operator Legend")) +
      labs(x = "Date", y = "Ablation Count") +
      ggtitle("Primary Operator Logbook") +
      theme(legend.position = "bottom") +
      opColScale

    # This method works but does not give you Pt ID per case....
    # rxDateByMonth <- rxDoneData %>%
    #  group_by(month = lubridate::floor_date(RxDate, "month"), operator=Operator1) %>%
    #  reframe(casesPerMonth = str_count(operator))

    # operatorPlot <<- ggplot(rxDateByMonth, aes(x=month, y=operator)) +
    #  geom_bar() +
    #  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
    #  scale_y_continuous(breaks = seq(0, 100, by = 1)) +  # Use this to get an integer y-axis, the 100 is the number of max ticks after which x>100 still plots tickless
    #  guides(color=guide_legend("1o Operator Legend")) +
    #  labs(x="Date", y="Ablation Count") +
    #  ggtitle("Primary Operator Logbook") +
    #  theme(legend.position="bottom") +
    #  opColScale
  } else {
    rxdonePlot <<- NA
    operatorPlot <<- NA
  }

  if (!is.null(nrow(rxWaitData))) {
    rxwaitPlotColors <<- c(
      "Days to DTT"          = "blue",
      "Days Waiting"         = "red",
      "Clock Stops Pre-DTT"  = "purple",
      "Clock Stops Post-DTT" = "cyan"
    )
    rxwaitPlot <<- ggplot(rxWaitData, aes(x = RefDate, text = paste(ID, " (", Organs, ")\n", "Provisional RxDate=", ProvisionalRxDate, ClockStopWhy, sep = ""))) +
      geom_point(aes(y = Ref_DTT, colour = "Days to DTT")) +
      geom_point(aes(y = DaysWaiting, colour = "Days Waiting")) +
      geom_point(aes(y = ClockStopDaysPreDTT, color = "Clock Stops Pre-DTT")) +
      geom_point(aes(y = ClockStopDaysPostDTT, color = "Clock Stops Post-DTT")) +
      theme(legend.position = "bottom") +
      scale_color_manual(values = rxwaitPlotColors) +
      guides(color = guide_legend("Patients with DTT...")) +
      labs(y = "Number of Days") +
      ggtitle("Time on Waiting List")
  }
  else
  {
    rxwaitPlot <<- ggplot()
  }
}

# Populate monthlyRxWaitData which is basically a list of patients on the waiting list each month
processMonthlyRxWaitingList <- function(startDate, endDate, organs) {
  showNotification("Generating treatment waiting list...")

  firstRefDate <- min(c(rxWaitData$RefDate, rxDoneData$RefDate), na.rm = T)
  lastRefDate <- max(c(rxWaitData$RefDate, rxDoneData$RefDate), na.rm = T)

  if (isConvertibleToDate(endDate)) lastRefDate <- min(lastRefDate, convertToDate(endDate))

  # Combine the rxDone and rxWait data frames into allDates data frame
  allRefDates <- c(rxDoneData$RefDate, rxWaitData$RefDate)
  allRxDates <- c(rxDoneData$RxDate, rep(NA, length(rxWaitData$RefDate)))
  allOrgans <- c(rxDoneData$Organs, rxWaitData$Organs)
  allDates <- data.frame(refDate = allRefDates, rxDate = allRxDates, Organs = allOrgans)

  # Loose records with ref date of NA or outside the end date bound provided, if provided
  allDates <- allDates %>% filter(!is.na(refDate))
  if (isConvertibleToDate(endDate)) allDates <- allDates %>% filter(refDate <= convertToDate(endDate))

  # Filter out only the organs we are interested in
  if (length(organs) > 0) {
    allDates <- allDates %>% filter(Organs %in% organs)
  }

  # Go through each month for which we have referral data one by one
  monthlyWaitingListDates <- c()
  monthlyCounts <- c()
  type <- c()

  monthlyWaitingTotal <- c()
  monthlyRefAndRxd <- c()

  if (nrow(allDates) > 0) {
    waitingListCount <- 0
    monthSpan <- interval(firstRefDate, lastRefDate) %/% months(1) + 1
    for (m in 1:monthSpan) {
      lastMonthsDate <- convertToDate(format(min(allDates$refDate) %m+% months(m - 1), "01-%m-%Y"))
      thisMonthsDate <- convertToDate(format(min(allDates$refDate) %m+% months(m), "01-%m-%Y"))
      refCount <- 0
      rxCount <- 0

      for (j in seq_len(nrow(allDates))) {
        if (allDates$refDate[j] >= lastMonthsDate && allDates$refDate[j] < thisMonthsDate) {
          refCount <- refCount + 1
          waitingListCount <- waitingListCount + 1
        }
        if (!is.na(allDates$rxDate[j]) && allDates$rxDate[j] >= lastMonthsDate && allDates$rxDate[j] < thisMonthsDate) {
          rxCount <- rxCount + 1
          waitingListCount <- waitingListCount - 1
        }
      }
      monthlyWaitingListDates <- c(monthlyWaitingListDates, rep(convertToDate(lastMonthsDate), 2))
      monthlyCounts <- c(monthlyCounts, c(refCount, rxCount))
      type <- c(type, c("Referred", "Treated"))

      monthlyWaitingTotal <- c(monthlyWaitingTotal, c(0, waitingListCount))
    }
  }

  monthlyRxWaitData <<- data.frame(
    MonthStart = asDateWithOrigin(monthlyWaitingListDates),
    Counts = monthlyCounts,
    Type = type,
    OnWaitingList = monthlyWaitingTotal
  )

  # We could filter upfront like endDate, but it won't account for referrals
  # made before the cut-off start date. This is an important offset as it
  # affects everything moving forward and would underrepresent the true
  # waiting list. Hence, we only filter the ones at the start once we have
  # the referral data.
  startPlotDate <- floor_date(convertToDate(startDate), unit = "month") - as.difftime(1, units = "days")
  if (isConvertibleToDate(startDate)) monthlyRxWaitData <<- monthlyRxWaitData %>% filter(MonthStart >= startPlotDate)
}

makeRxDonePlot <- function(startDate, endDate, organs) {
  # Filter the dates and organs
  rxDoneData.filtered <- rxDoneData

  if (isConvertibleToDate(startDate)) {
    rxDoneData.filtered <- rxDoneData.filtered %>% filter(RxDate >= convertToDate(startDate))
  }

  if (isConvertibleToDate(endDate)) {
    rxDoneData.filtered <- rxDoneData.filtered %>% filter(RxDate <= convertToDate(endDate))
  }

  if (length(organs) > 0) {
    rxDoneData.filtered <- rxDoneData.filtered %>% filter(Organs %in% organs)
  }

  if (is.null(nrow(rxDoneData.filtered))) {
    rxdonePlot <<- NA
    return(rxdonePlot)
  }

  rxdonePlot <<- commonRxDonePlot(rxDoneData.filtered) +
    labs(title = paste0("Time to Treatment (Generated ", format(Sys.time(), "%a %b %d %Y %X"), ")")) +
    theme(plot.title = element_text(size = 10))

  rxdonePlot
}

commonRxDonePlot <- function(filteredData, pointSize = 1, legend.position = "bottom") {
  donePlot <- ggplot(filteredData, aes(x = RxDate, text = paste(ID, " (", Organs, ")\n", ClockStopWhy, sep = ""))) +
    geom_point(aes(y = Ref_DTT, color = "Ref to DTT"), size = pointSize) +
    geom_point(aes(y = DTT_Rx, color = "DTT to Rx"), size = pointSize) +
    geom_point(aes(y = Ref_RxDone, color = "Ref to Rx"), size = pointSize) +
    geom_point(aes(y = ClockStopDaysPreDTT, color = "Clock Stops Pre-DTT"), size = pointSize) +
    geom_point(aes(y = ClockStopDaysPostDTT, color = "Clock Stops Post-DTT"), size = pointSize) +
    theme(legend.position = legend.position) +
    scale_color_manual(values = c(
      "Ref to DTT"           = "red",
      "DTT to Rx"            = "yellow",
      "Ref to Rx"            = "green",
      "Clock Stops Pre-DTT"  = "purple",
      "Clock Stops Post-DTT" = "cyan",
      "Operator1"            = "blue",
      "Waiting List"         = "orange"
    )) +
    guides(color = guide_legend("Treated Patients:\n", position = "right"), ) +
    labs(y = "Number of Days") +
    scale_x_date(date_breaks = "month", date_labels = "%b-%y") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  donePlot
}

makeRxWaitPlot <- function(startDate, endDate, organs) {
  # Filter the dates and organs
  rxWaitData.filtered <- rxWaitData

  if (isConvertibleToDate(startDate)) {
    rxWaitData.filtered <- rxWaitData.filtered %>% filter(RefDate >= convertToDate(startDate))
  }

  if (isConvertibleToDate(endDate)) {
    rxWaitData.filtered <- rxWaitData.filtered %>% filter(RefDate <= convertToDate(endDate))
  }

  if (length(organs) > 0) {
    rxWaitData.filtered <- rxWaitData.filtered %>% filter(Organs %in% organs)
  }

  if (is.null(nrow(rxWaitData.filtered))) {
    rxwaitPlot <<- ggplot()
    return(rxwaitPlot)
  }

  # FIXME: the issue is here if two are referred on same day then they are plotted exactly on top of each other
  # There are different librarys to handle it including jitter, but I had no success
  # The next best work around is to make opacification 0.3 with alpha,
  # so you know if there is one hiding, but tooltiptext doesn't show other PtID
  rxwaitPlot <<- rxWaitData.filtered |>
    ggplot(aes(
      x = RefDate,
      text = paste(ID, " (", Organs, ")\n", "Provisional RxDate=", ProvisionalRxDate, ClockStopWhy, sep = "")
    )) +
    geom_point(aes(y = Ref_DTT, colour = "Days to DTT", alpha = 0.2), size = 1) +
    geom_point(aes(y = DaysWaiting, colour = "Days Waiting", alpha = 0.2), size = 1) +
    geom_point(aes(y = ClockStopDaysPreDTT, color = "Clock Stops Pre-DTT"), size = 1) +
    geom_point(aes(y = ClockStopDaysPostDTT, color = "Clock Stops Post-DTT"), size = 1) +
    theme(legend.position = "bottom") +
    scale_color_manual(values = c(
      "Days to DTT"          = "blue",
      "Days Waiting"         = "red",
      "Clock Stops Pre-DTT"  = "purple",
      "Clock Stops Post-DTT" = "cyan"
    )) +
    guides(color = guide_legend("Patients with DTT:\n")) +
    labs(y = "Number of Days") +
    labs(title = paste0("Time on Waiting List (Generated ", format(Sys.time(), "%a %b %d %Y %X"), ")")) +
    theme(plot.title = element_text(size = 10)) +
    scale_x_date(date_breaks = "month", date_labels = "%b-%y") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  rxwaitPlot
}

# Plot type is a switch to make this more efficient!
makeOperatorPlot <- function(startDate, endDate, organs) {
  # Create a custom color scale/palette
  # There are 11 colours in the Spectral palette but if we have more than 11 operators,
  # we need to interpolate new colours hence the colour ramp
  opColors <- colorRampPalette(brewer.pal(11, "Spectral"))(length(operator1Factors))
  names(opColors) <- operator1Factors
  opColScale <- scale_fill_manual(name = "Operators", values = opColors)

  # The operatorPlot gets subsetted to a single operator in app.R
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
    # We want an integer y-axis with up to 100  ticks after which y>100 is tickless
    scale_y_continuous(breaks = seq(0, 100, by = 1)) +
    labs(x = "Date", y = "Ablation Count") +
    labs(title = paste0("Primary Operator Logbook (Generated ", format(Sys.time(), "%a %b %d %Y %X"), ")")) +
    theme(plot.title = element_text(size = 10)) +
    theme(legend.position = "bottom") +
    opColScale

  operatorPlot
}

makeWaitingListPlot <- function(startDate, endDate, organs) {
  processMonthlyRxWaitingList(startDate, endDate, organs)

  if (is.null(nrow(monthlyRxWaitData))) {
    monthlyWaitingPlot <<- ggplot()
    return(monthlyWaitingPlot)
  }

  monthlyWaitingPlotColors <<- c(
    "Waiting Total"    = "red",
    "Referred"         = "orange",
    "Treated"          = "green"
  )

  # FIXME: Works well but when running from the app (not command line)
  # I get the following warning which I don't know how to fix:
  #  Warning: 'bar' objects don't have these attributes: 'mode'

  monthlyWaitingPlot <<- ggplot() +
    geom_line(
      data = filter(monthlyRxWaitData, Type == "Treated"),
      aes(x = MonthStart, y = OnWaitingList, color = "Waiting Total")
    ) +
    geom_point(
      data = filter(monthlyRxWaitData, Type == "Treated"),
      aes(x = MonthStart, y = OnWaitingList, color = "Waiting Total"), size = 1
    ) +
    geom_bar(
      data = monthlyRxWaitData,
      aes(fill = Type, x = MonthStart, y = Counts),
      stat = "identity",
      alpha = 0.75,
      just = 0,
      position = "dodge2"
    ) +
    scale_fill_manual(values = c("gray", "lightgreen")) +
    scale_x_date(date_breaks = "month", date_labels = "%b-%y") +
    geom_vline(
      xintercept = as.numeric(Sys.Date()),
      color = "purple", linetype = "dotted", size = 0.5
    ) +
    guides(color = guide_legend("Waiting List by Month:\n")) +
    labs(title = paste0("Monthly Treatment Waiting List (Generated ", format(Sys.time(), "%a %b %d %Y %X"), ")")) +
    theme(plot.title = element_text(size = 10)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  monthlyWaitingPlot
}

filterPatientData <- function(
    strStart,
    strEnd,
    selectedOrgans,
    selectedDiagnosisType,
    selectedSubtypes,
    selectedGenders,
    selectedModality = "All",
    minTumourSize = NULL,
    maxTumourSize = NULL
)
{
  start <- as.Date(strStart, format="%d/%m/%Y")
  end   <- as.Date(strEnd, format="%d/%m/%Y")
  
  filteredData <- cancerPerPatientData |>
    filter(between(FirstRxDate, start, end)) |>
    filter(Gender %in% selectedGenders)
  
  if (!("All" %in% selectedOrgans))
  {
    filteredData <- filteredData |>
      filter(Organ %in% selectedOrgans)
  }
  
  if (selectedDiagnosisType != "All")
  {
    filteredData <- switch(
      substring(selectedDiagnosisType,1,1),
      "P" = filteredData |> filter(DiagnosisType=="P", Diagnosis1o %in% selectedSubtypes),
      "S" = filteredData |> filter(DiagnosisType=="S", Diagnosis2o %in% selectedSubtypes),
      "B" = filteredData |> filter(DiagnosisType=="B", DiagnosisBn %in% selectedSubtypes),
      "U" = filteredData |> filter(DiagnosisType=="U", DiagnosisUn %in% selectedSubtypes),
      filteredData   # fallback: no recognised diagnosis code, leave data untouched rather than silently becoming NULL
    )
  }
  
  if (!is.null(minTumourSize) &&
      !is.null(maxTumourSize))
  {
    filteredData <- filteredData |>
      filter(is.na(MaxTumourSize) |
               between(MaxTumourSize,
                       minTumourSize,
                       maxTumourSize))
  }
  
  if (!("All" %in% selectedModality))
  {
    filteredData <- filteredData |>
      filter(
        Reduce(
          `|`,
          lapply(selectedModality, function(m)
            grepl(m, RxModalities, fixed = TRUE)
          )
        )
      )
  }
  
  filteredData
}

applyIgnoreFirstLTP <- function(filteredData)
{
  for (i in which(
    !is.na(filteredData$TimeLTPF) &
    filteredData$StatusLTPF == 2))
  {
    ptID    <- filteredData$ID[i]
    ptOrgan <- filteredData$Organ[i]
    
    ltpDates <- cancerPerLesionData$LTPDate[
      cancerPerLesionData$PtID == ptID &
        !is.na(cancerPerLesionData$LTPDate)
    ]
    
    if (length(ltpDates) == 0)
      next
    
    firstLtpDt <- min(ltpDates)
    
    laterRxCount <- length(unique(
      cancerPerLesionData$RxNo[
        cancerPerLesionData$PtID == ptID &
          cancerPerLesionData$Organ == ptOrgan &
          !is.na(cancerPerLesionData$RxDate) &
          cancerPerLesionData$RxDate > firstLtpDt
      ]
    ))
    
    if (laterRxCount == 1)
    {
      filteredData$StatusLTPF[i] <- 1
      
      if (filteredData$Deceased[i] == 0)
      {
        filteredData$StatusLTPFOS[i]  <- 1
        filteredData$StatusLTPFCSS[i] <- 1
      }
    }
  }
  filteredData
}

noDataPlot <- function(msg = "No data available for selected filters")
{
  ggplot() +
    annotate(
      "text",
      x = 0.5,
      y = 0.5,
      label = msg,
      size = 6,
      hjust = 0.5
    ) +
    theme_void()
}

makeSurvivalPlot <- function(strStart, strEnd, minMonthsFollowup = 0, maxYearsFollowup = 5, selectedOrgans, selectedDiagnosisType, selectedSubtypes, selectedGenders, selectedModality = "All", survivalType, ignoreFirstLTP = FALSE, minTumourSize = NULL, maxTumourSize = NULL)
{
  # Filter the data using helper function
  filteredSurvivalData <- filterPatientData(
    strStart,
    strEnd,
    selectedOrgans,
    selectedDiagnosisType,
    selectedSubtypes,
    selectedGenders,
    selectedModality,
    minTumourSize,
    maxTumourSize
  )
  
  # This is the bit where we think about the number of Rx (ablations) before we call LR
  # So if LTPCountMax is just 1, we dont count it as real LR if ignoreFirstLTP is TRUE
  # A way to do this is to change the StatusLTPF/LTPFOS/LTPFCSS column status to 1 for all those recurring after just 1 Rx, if not deceased, as follows
  if (ignoreFirstLTP)
    filteredSurvivalData <- applyIgnoreFirstLTP(filteredSurvivalData)
  
  # Get rid of anything which doesn't have the necessary recurrence data, so that we know if its going to be an empty fit before we fit it
  #filteredSurvivalData <- filteredSurvivalData |>
  #  filter(!is.na(TimeLTPF), !is.na(StatusLTPF))
  # -- this is specific to the radiobutton
  
  # If no rows to plot, let the user know there is no data
  if (nrow(filteredSurvivalData) == 0) {
    return(list(noDataPlot(), NULL))
  }
  
  # Censoring = 1=censored, 2=dead - 
  # The sample is censored in that you only know that the individual survived up to the loss to followup,
  # but you don't know anything about survival after that. I used to have 0=alive, but this isn't recognised
  # See https://thriv.github.io/biodatasci2018/r-survival.html
  #survivalFit         <- survfit(Surv(Time, Status)~1,     data = filteredSurvivalData)
  #survivalFit         <- survfit(Surv(Time, Status)~Sex,   data = filteredSurvivalData)
  
  # Switch on the radiobutton for Survival Type
  if (survivalType == 0)  # This is plain old overall survival
  {
    survivalFit        <- ggsurvfit::survfit2(Surv(TimeSurvival, StatusOverallSurvival)~Organ, 
                                              data = filteredSurvivalData,
                                              start.time = 0) # The start.time avoids error messages when the last imaging date is before 1st Rx date (captured elswhere as data integrity)
    titleStr           <- paste0("Overall Survival (ignore first LTP=",ignoreFirstLTP,")")
    
  }
  else if (survivalType == 1) # This is cancer specific survival
  {
    survivalFit        <- ggsurvfit::survfit2(Surv(TimeSurvival, StatusCancerSpecificSurvival)~Organ,
                                              data = filteredSurvivalData,
                                              start.time = 0) # The start.time avoids error messages when the last imaging date is before 1st Rx date (captured elswhere as data intergrity)
    titleStr           <- paste0("Cancer Specific Survival (ignore first LTP=",ignoreFirstLTP,")")
    
  }
  else if (survivalType == 2)  # This is LTPF overall survival
  {
    survivalFit        <- ggsurvfit::survfit2(Surv(TimeLTPFOS, StatusLTPFOS)~Organ,
                                              data = filteredSurvivalData,
                                              start.time = 0) # The start.time avoids error messages when the last imaging date is before 1st Rx date (captured elswhere as data intergrity)
    titleStr           <- paste0("LTP-Free Overall Survival (ignore first LTP=",ignoreFirstLTP,")")
    
  }
  else if (survivalType == 3)  # This is LTPF cancer specific survival
  {
    survivalFit        <- ggsurvfit::survfit2(Surv(TimeLTPFCSS, StatusLTPFCSS)~Organ,
                                              data = filteredSurvivalData,
                                              start.time = 0) # The start.time avoids error messages when the last imaging date is before 1st Rx date (captured elswhere as data intergrity)
    titleStr           <- paste0("LTP-Free Cancer Specific Survival (ignore first LTP=",ignoreFirstLTP,")")
    
  }
  # Original method but don't know how to change risk table to just e.g. 5 follow-up years
  #survivalPlot         <- ggsurvplot(survivalFit,
  #                                   ylab = "Probability",
  #                                   xlab = titleStr,   risk.table = TRUE,
  #                                   ggtheme = theme(plot.title = element_text(hjust = 0.5)))
  #survivalPlot$plot    <- survivalPlot$plot + coord_cartesian(xlim = c(0, maxYearsFollowup))
  #survivalPlot
  
  # Newwer method, which requires survfit2 wrapper rather than survfit, and allows maxYears on plot + table
  year_breaks <- seq(0, maxYearsFollowup, by = 1)
  survivalPlot <- survivalFit |>
    ggsurvfit(linewidth = 1) +
    add_confidence_interval() +
    add_censor_mark() +
    add_risktable(
      times = year_breaks,
      risktable_height = 0.35,   # was 0.25 (default) — bump up to give the table more room
      risktable_stats = c(
        "n.risk",
        "{scales::percent(estimate, accuracy = 0.1)}",
        "{scales::percent(conf.low, accuracy = 0.1)}",
        "{scales::percent(conf.high, accuracy = 0.1)}"
      ),
      stats_label = list(
        "n.risk" = "At risk",
        "{scales::percent(estimate, accuracy = 0.1)}" = "%",
        "{scales::percent(conf.low, accuracy = 0.1)}" = "Lower 95%",
        "{scales::percent(conf.high, accuracy = 0.1)}" = "Upper 95%"
      ),
      size = 5,
      theme = theme(
        axis.text.y  = element_text(size = 14, face = "bold"),
        axis.title.y = element_blank(),
        axis.text.x  = element_text(size = 12),
        axis.title.x = element_blank()   # or size it up if you want to keep "time"
      )
    ) +
    #add_quantile(y_value = 0.6, color = "gray50", linewidth = 0.75) +
    scale_ggsurvfit() +
    coord_cartesian(xlim = c(-0.4, maxYearsFollowup + 0.3), expand = FALSE) +
    labs(title = titleStr, y = "Probability", x = "Time (Years)") +
    theme(
      plot.margin = margin(10, 20, 10, 10),  # Give a little breathing space so we don't clip values in the risk table
      axis.text = element_text(size = 14),   # axis tick labels
      axis.title = element_text(size = 16)   # axis titles
    )
  list(survivalPlot, survivalFit)
}

# This can do both per-patient and per-tumour LTP analysis
makeRecurrencePlot <- function(strStart, strEnd, minMonthsFollowup = 0, maxYearsFollowup = 100, selectedOrgans, selectedDiagnosisType, selectedSubtypes, selectedGenders, selectedModality = "All", ignoreFirstLTP = FALSE, minTumourSize = NULL, maxTumourSize = NULL, ltpAnalysisUnit = "patient")
{
  # Filter the using helper function
  filteredSurvivalData <- filterPatientData(
    strStart,
    strEnd,
    selectedOrgans,
    selectedDiagnosisType,
    selectedSubtypes,
    selectedGenders,
    selectedModality,
    minTumourSize,
    maxTumourSize
  )
  
  # This is the bit where we think about the number of Rx (ablations) before we call LR
  # So if LTPCountMax is just 1, we dont count it as real LR if ignoreFirstLTP is TRUE
  # A way to do this is to change the StatusLTPF column status to 1 for all those recurring after just 1 Rx, as follows
  if (ignoreFirstLTP)
    filteredSurvivalData <- applyIgnoreFirstLTP(filteredSurvivalData)
  
  # Switch between per-patient and per-lesion LTP analysis
  # Per-patient: time-to-LTP measured from date of first Rx across all referrals (current approach)
  # Per-lesion:  time-to-LTP measured from the Rx date of the specific referral where LTP occurred,
  #              requires ltp.list to have been populated in the EDC imaging follow-up matrix
  if (ltpAnalysisUnit == "patient")
  {
    # Per-patient analysis (existing behaviour)
    
    # Get rid of anything which doesn't have the necessary recurrence data, so that we know if its going to be an empty fit before we fit it
    filteredSurvivalData <- filteredSurvivalData |>
      filter(!is.na(TimeLTPF), !is.na(StatusLTPF), TimeLTPF >= 0)
    
    # If no rows to plot, let the user know there is no data
    if (nrow(filteredSurvivalData) == 0) {
      return(list(noDataPlot(), NULL))
    }
    
    # Censoring = 1=censored, 2=recurred (almost treat as if death) - 
    # The sample is censored in that you only know that the individual survived up to the loss to followup,
    # but you don't know anything about survival after that. I used to have 0=alive, but this isn't recognised
    # See https://thriv.github.io/biodatasci2018/r-survival.html
    # StatusLTPF == 2 evaluates to FALSE for all censored rows (value=1) and TRUE for event rows (value=2), regardless of whether any events are present
    recurrenceFit <- ggsurvfit::survfit2(Surv(TimeLTPF, StatusLTPF == 2) ~ Organ,
                                         data = filteredSurvivalData,
                                         start.time = 0) # The start.time avoids error messages when the last imaging date is before 1st Rx date (captured elswhere as data intergrity)
    titleStr      <- paste0("Local Tumour Progression Free Analysis (Per Patient, ignore first LTP=",ignoreFirstLTP,")")
  }
  else
  {
    # Per-lesion analysis
    # cancerPerLesionData is pre-built in postProcessData() and contains all malignant referral episodes
    # with TimeLTPEpisode and StatusLTPEpisode pre-computed, ready for Kaplan-Meier fitting
    # Episodes with a confirmed LTP event (ltp.list populated) have StatusLTPEpisode=2
    # Episodes without a confirmed LTP event are censored at last imaging date with StatusLTPEpisode=1
    if (!is.data.frame(cancerPerLesionData) || nrow(cancerPerLesionData) == 0)
    {
      return(list(noDataPlot(), NULL))
    }
    
    # Apply filters to per-lesion data matching those applied to per-patient data above
    start <- as.Date(strStart, "%d/%m/%Y")
    end   <- as.Date(strEnd, "%d/%m/%Y")
    filteredPerLesionData <- cancerPerLesionData[
      cancerPerLesionData$RxDate >= start & cancerPerLesionData$RxDate <= end, ]
    
    keep_ids <- unique(sub("-[0-9]+$", "", filteredSurvivalData$ID))
    filteredPerLesionData <- filteredPerLesionData[
      filteredPerLesionData$PtID %in% keep_ids, ]
    
    filteredPerLesionData <- filteredPerLesionData[
      filteredPerLesionData$Gender %in% selectedGenders, ]
    
    if (!("All" %in% selectedOrgans))
    {
      filteredPerLesionData <- filteredPerLesionData[filteredPerLesionData$Organ %in% selectedOrgans, ]
    }
    
    if (selectedDiagnosisType == "All")
    {
      # no diagnosis filter here
    }
    else if (selectedDiagnosisType == "1o & 2o")
    {
      filteredPerLesionData <- filteredPerLesionData[
        filteredPerLesionData$Diagnosis1o %in% selectedSubtypes |
          filteredPerLesionData$Diagnosis2o %in% selectedSubtypes, ]
    }
    else
    {
      filteredPerLesionData <- switch(substring(selectedDiagnosisType, 1, 1),
                                      "P" = filteredPerLesionData[filteredPerLesionData$Diagnosis1o %in% selectedSubtypes, ],
                                      "S" = filteredPerLesionData[filteredPerLesionData$Diagnosis2o %in% selectedSubtypes, ],
                                      "U" = filteredPerLesionData[filteredPerLesionData$DiagnosisUn %in% selectedSubtypes, ],
                                      filteredPerLesionData
      )
    }
    
    if (!("All" %in% selectedModality) &&
        "RxModality" %in% names(filteredPerLesionData))
    {
      filteredPerLesionData <- filteredPerLesionData[
        Reduce(
          `|`,
          lapply(selectedModality, function(m)
            grepl(m,
                  filteredPerLesionData$RxModality,
                  fixed = TRUE)
          )
        ),
      ]
    }
    
    # Filter by tumour size if specified — episodes with no recorded size are kept
    if (!is.null(minTumourSize) && !is.null(maxTumourSize))
      filteredPerLesionData <- filteredPerLesionData[
        is.na(filteredPerLesionData$TumourSize) |
          (filteredPerLesionData$TumourSize >= minTumourSize &
             filteredPerLesionData$TumourSize <= maxTumourSize), ]
    
    # Get rid of anything which doesn't have the necessary recurrence data
    filteredPerLesionData <- filteredPerLesionData[
      !is.na(filteredPerLesionData$TimeLTPEpisode) &
        !is.na(filteredPerLesionData$StatusLTPEpisode), ]
    
    # So the LTPCount is encoded 0: No LTP, 1: 1st LTP, 2: 2nd LTP, 3: >2nd LTP
    # Note that LTPCount is effectively like a cumulative global: i.e. allow up to 2 recurrences before calling LTP
    # For per-lesion: censor an episode's LTP if the patient had a subsequent ablation after it
    # i.e. the LTP was "managed" by re-treatment rather than being terminal
    # laterRxCount == 0: no re-treatment after LTP → terminal event, keep
    # laterRxCount == 1: one re-treatment → managed LTP, censor
    # laterRxCount >= 2: two or more re-treatments → recurrent/terminal, keep
    if (ignoreFirstLTP) {
      for (i in which(!is.na(filteredPerLesionData$LTPDate) & 
                      filteredPerLesionData$StatusLTPEpisode == 2)) {
        ptID  <- filteredPerLesionData$PtID[i]
        ltpDt <- filteredPerLesionData$LTPDate[i]
        
        # CRITICAL: use full cancerPerLesionData not filteredPerLesionData
        # so that subsequent Rx count is not affected by size/organ filters
        laterRxCount <- length(unique(
          cancerPerLesionData$RxNo[
            cancerPerLesionData$PtID == ptID & 
              !is.na(cancerPerLesionData$RxDate) &
              cancerPerLesionData$RxDate > ltpDt
          ]
        ))
        
        if (laterRxCount == 1) {
          filteredPerLesionData$StatusLTPEpisode[i] <- 1
        }
      }
    }
    
    # If no rows to plot, let the user know there is no data
    if (nrow(filteredPerLesionData) == 0)
    {
      return(list(noDataPlot(), NULL))
    }
    
    logger(paste("LTP perLesion analysis: Total No. Filtered Lesions=", nrow(filteredPerLesionData),
                 ", events=", sum(filteredPerLesionData$StatusLTPEpisode == 2, na.rm=TRUE),
                 ", censored=", sum(filteredPerLesionData$StatusLTPEpisode == 1, na.rm=TRUE)), logOnlyAsDebug=TRUE)
    
    # Censoring = 1=censored, 2=recurred (almost treat as if death) - 
    # The sample is censored in that you only know that the individual survived up to the loss to followup,
    # but you don't know anything about survival after that. I used to have 0=alive, but this isn't recognised
    # See https://thriv.github.io/biodatasci2018/r-survival.html
    # StatusLTPEpisode == 2 evaluates to FALSE for all censored rows (value=1) and TRUE for event rows (value=2), regardless of whether any events are present
    recurrenceFit <- ggsurvfit::survfit2(
      Surv(TimeLTPEpisode, StatusLTPEpisode == 2) ~ Organ,
           data = filteredPerLesionData,
           start.time = 0) # The start.time avoids error messages when the last imaging date is before 1st Rx date (captured elswhere as data intergrity)
    titleStr      <- paste0("Local Tumour Progression Free Analysis (Per Lesion, ignore first LTP=",ignoreFirstLTP,")")
    
  }
  
  # Original method but don't know how to change risk table to just e.g. 5 follow-up years
  #recurrencePlot       <- ggsurvplot(recurrenceFit,
  #                                   ylab = "Probability",
  #                                   xlab = "Time to Local Tumour Progression (Years)",   risk.table = TRUE,
  #                                   ggtheme = theme(plot.title = element_text(hjust = 0.5)))
  #recurrencePlot$plot <- recurrencePlot$plot + coord_cartesian(xlim = c(0, maxYearsFollowup))
  #recurrencePlot
  
  # Newwer method, which requires survfit2 wrapper rather than survfit, and allows maxYears on plot + table
  year_breaks <- seq(0, maxYearsFollowup, by = 1)
  recurrencePlot <- recurrenceFit |>
    ggsurvfit(linewidth = 1) +
    add_confidence_interval() +
    add_censor_mark() +
    add_risktable(
      times = year_breaks,
      risktable_height = 0.35,   # was 0.25 (default) — bump up to give the table more room
      risktable_stats = c(
        "n.risk",
        "{scales::percent(estimate, accuracy = 0.1)}",
        "{scales::percent(conf.low, accuracy = 0.1)}",
        "{scales::percent(conf.high, accuracy = 0.1)}"
      ),
      stats_label = list(
        "n.risk" = "At risk",
        "{scales::percent(estimate, accuracy = 0.1)}" = "%",
        "{scales::percent(conf.low, accuracy = 0.1)}" = "Lower 95%",
        "{scales::percent(conf.high, accuracy = 0.1)}" = "Upper 95%"
      ),
      size = 5,
      theme = theme(
        axis.text.y  = element_text(size = 14, face = "bold"),
        axis.title.y = element_blank(),
        axis.text.x  = element_text(size = 12),
        axis.title.x = element_blank()   # or size it up if you want to keep "time"
      )
    ) +
    #  #add_quantile(y_value = 0.6, color = "gray50", linewidth = 0.75) +
    scale_ggsurvfit() +
    coord_cartesian(xlim = c(-0.4, maxYearsFollowup + 0.3), expand = FALSE) +
    labs(title = titleStr, y = "Probability", x = "Time (Years)") +
    theme(
      plot.margin = margin(10, 70, 10, 10),  # Give a little breathing space so we don't clip values in the risk table
      axis.text = element_text(size = 14),   # axis tick labels
      axis.title.y = element_blank()
      )
  list(recurrencePlot, recurrenceFit)
}
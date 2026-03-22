prepareOrganCountRecurrenceData <- function(filtered_df)
{
  filtered_df |>
    group_by(Organs) |>
    summarize(Count = n()) |>
    mutate(
      Percent = (Count / sum(Count)),
      Organ = factor(Organs),
      csum = rev(cumsum(rev(Count))),
      pos = Count / 2 + lead(csum, 1),
      pos = if_else(is.na(pos), Count / 2, pos)
    )
}

makeSurvivalPlot <- function(strStart, strEnd, maxYearsFollowup, selectedOrgans, selectedDiagnosisType, selectedSubtypes, selectedGenders, survivalType)
{
  # Filter the dates
  start <- as.Date(strStart, format = "%d/%m/%Y")
  end <- as.Date(strEnd, format = "%d/%m/%Y")
  
  #if (!is.data.frame(survivalData) || nrow(survivalData) == 0) {
  #  return(ggplot()) # Any empty plot
  #}
  
  filteredSurvivalData <<- survivalData |>
    filter(between(FirstRxDate, start, end)) |>
    filter(Gender %in% selectedGenders)
  
  if (selectedOrgans != "All")
  {
    filteredSurvivalData <<- filteredSurvivalData |>
      filter(Organ %in% selectedOrgans)
  }
  
  if (selectedDiagnosisType == "All")
  {
    if (! "All" %in% selectedSubtypes)
    {
      # This just removes all rows as no Organ is All, and therefore we get the error message for the blank plot
      filteredSurvivalData <<- filteredSurvivalData |> filter(Organ %in% selectedSubtypes)
    }
  }
  else
  {
    # We just want to use the first letter of what is selected in the GUI e.g. P(rimary) to match the EDC data
    #filteredSurvivalData <<- filteredSurvivalData |> filter(DiagnosisType == substring(selectedDiagnosisType, 1, 1))
    filteredSurvivalData <<- switch(substring(selectedDiagnosisType, 1, 1),
                                    "P" = filteredSurvivalData |> filter(Diagnosis1o %in% selectedSubtypes),
                                    "S" = filteredSurvivalData |> filter(Diagnosis2o %in% selectedSubtypes),
                                    "B" = filteredSurvivalData |> filter(DiagnosisBn %in% selectedSubtypes),
                                    "U" = filteredSurvivalData |> filter(DiagnosisUn %in% selectedSubtypes)
    )
  }
  
  # Get rid of anything which doesn't have the necessary recurrence data, so that we know if its going to be an empty fit before we fit it
  #filteredSurvivalData <- filteredSurvivalData |>
  #  filter(!is.na(TimeLRF), !is.na(StatusLRF))
  # -- this is specific to the radiobutton
  
  # If no rows to plot, let the user know there is no data
  if (nrow(filteredSurvivalData) == 0) {
    return(ggplot()+
             annotate("text", x = 0.5, y = 0.5,
                      label = "No data available for selected filters",
                      size = 6, hjust = 0.5) +
             theme_void()
           )
  }
  
  # Censoring = 1=censored, 2=dead - 
  # The sample is censored in that you only know that the individual survived up to the loss to followup,
  # but you don’t know anything about survival after that. I used to have 0=alive, but this isn't recognised
  # See https://thriv.github.io/biodatasci2018/r-survival.html
  #survivalFit         <- survfit(Surv(Time, Status)~1,     data = filteredSurvivalData)
  #survivalFit         <- survfit(Surv(Time, Status)~Sex,   data = filteredSurvivalData)
  
  # Switch on the radiobutton for Survival Type
  if (survivalType == 0)  # This is plain old overall survival
  {
    survivalFit        <- ggsurvfit::survfit2(Surv(TimeSurvival, StatusOverallSurvival)~Organ, data = filteredSurvivalData) 
    titleStr           <- "Overall Survival"
  }
  else if (survivalType == 1) # This is cancer specific survival
  {
    survivalFit        <- ggsurvfit::survfit2(Surv(TimeSurvival, StatusCancerSpecificSurvival)~Organ, data = filteredSurvivalData)
    titleStr           <- "Cancer Specific Survival"
  }
  else if (survivalType == 2)  # This is LRF overall survival
  {
    survivalFit        <- ggsurvfit::survfit2(Surv(TimeLRFOS, StatusLRFOS)~Organ, data = filteredSurvivalData)
    titleStr           <- "Overall Local Recurrence-Free Overall Survival"
  }
  else if (survivalType == 3)  # This is LRF cancer specific survival
  {
    survivalFit        <- ggsurvfit::survfit2(Surv(TimeLRFCSS, StatusLRFCSS)~Organ, data = filteredSurvivalData)
    titleStr           <- "Overall Local Recurrence-Free Cancer Specific Survival"
  }
  # Original method but don't know how to change risk table to just e.g. 5 follow-up years
  #survivalPlot         <- ggsurvplot(survivalFit,
  #                                   ylab = "Probability",
  #                                   xlab = titleStr,   risk.table = TRUE,
  #                                   ggtheme = theme(plot.title = element_text(hjust = 0.5)))
  #survivalPlot$plot    <- survivalPlot$plot + coord_cartesian(xlim = c(0, maxYearsFollowup))
  #survivalPlot
  
  # Newwer method, which requires survfit2 wrapper rather than survfit, and allows maxYears on plot + table
  survivalPlot <- survivalFit |>
                  ggsurvfit(linewidth = 1) +
                  add_confidence_interval() + add_censor_mark() +
                  add_risktable(times=c(0:maxYearsFollowup), size=5) +
                  #add_quantile(y_value = 0.6, color = "gray50", linewidth = 0.75) +
                  scale_ggsurvfit() + coord_cartesian(xlim = c(0, maxYearsFollowup)) +
                  labs(title = titleStr, y = "Probability", x = "Time (Years)")
  survivalPlot
}

# Make the recurrence plot
# The ignoreFirstRecurrence means that we allow up to two treatments (ablations) before we call local recurrence
makeRecurrencePlot <- function(strStart, strEnd, maxYearsFollowup, selectedOrgans, selectedDiagnosisType, selectedSubtypes, selectedGenders, ignoreFirstLR = FALSE)
{
  # Filter the dates
  start <- as.Date(strStart, format = "%d/%m/%Y")
  end <- as.Date(strEnd, format = "%d/%m/%Y")
  
  if (!is.data.frame(survivalData) || nrow(survivalData) <= 0) {
    return(ggplot())
  }
  
  filteredSurvivalData <<- survivalData |>
    filter(between(FirstRxDate, start, end)) |>
    filter(Gender %in% selectedGenders)

  if (selectedOrgans != "All")
  {
    filteredSurvivalData <<- filteredSurvivalData |>
      filter(Organ %in% selectedOrgans)
  }
    
  if (selectedDiagnosisType == "All")
  {
    if (! "All" %in% selectedSubtypes)
    {
      # This just removes all rows as no Organ is All, and therefore we get the error message for the blank plot
      filteredSurvivalData <<- filteredSurvivalData |> filter(Organ %in% selectedSubtypes)
    }
  }
  else
  {
    # We just want to use the first letter of what is selected in the GUI e.g. P(rimary) to match the EDC data
    #filteredSurvivalData <<- filteredSurvivalData |> filter(DiagnosisType == substring(selectedDiagnosisType, 1, 1))
    filteredSurvivalData <<- switch(substring(selectedDiagnosisType, 1, 1),
      "P" = filteredSurvivalData |> filter(Diagnosis1o %in% selectedSubtypes),
      "S" = filteredSurvivalData |> filter(Diagnosis2o %in% selectedSubtypes),
      "B" = filteredSurvivalData |> filter(DiagnosisBn %in% selectedSubtypes),
      "U" = filteredSurvivalData |> filter(DiagnosisUn %in% selectedSubtypes)
    )
  }
  
  # This is the bit where we think about the number of Rx (ablations) before we call LR
  # So if NoRxBeforeFirstLR is just 1, we dont count it as real LR if ignoreFirstLR is TRUE
  # A way to do this is to change the StatusLRF column status to 1 for all those recurring after just 1 Rx, as follows
  if (ignoreFirstLR == TRUE)
  {
    filteredSurvivalData$StatusLRF[filteredSurvivalData$NoRxBeforeFirstLR==1] <- 1
  }
  
  # Get rid of anything which doesn't have the necessary recurrence data, so that we know if its going to be an empty fit before we fit it
  filteredSurvivalData <- filteredSurvivalData |>
    filter(!is.na(TimeLRF), !is.na(StatusLRF))
  
  # If no rows to plot, let the user know there is no data
  if (nrow(filteredSurvivalData) == 0) {
    return(ggplot()+
             annotate("text", x = 0.5, y = 0.5,
                      label = "No data available for selected filters",
                      size = 6, hjust = 0.5) +
             theme_void()
    )
  }
  
  # Censoring = 1=censored, 2=recurred (almost treat as if death) - 
  # The sample is censored in that you only know that the individual survived up to the loss to followup,
  # but you don’t know anything about survival after that. I used to have 0=alive, but this isn't recognised
  # See https://thriv.github.io/biodatasci2018/r-survival.html
  recurrenceFit         <- ggsurvfit::survfit2(Surv(TimeLRF, StatusLRF)~Organ, data = filteredSurvivalData)
  
  # Original method but don't know how to change risk table to just e.g. 5 follow-up years
  #recurrencePlot       <- ggsurvplot(recurrenceFit,
  #                                   ylab = "Probability",
  #                                   xlab = "Time to Local Recurrence (Years)",   risk.table = TRUE,
  #                                   ggtheme = theme(plot.title = element_text(hjust = 0.5)))
  #recurrencePlot$plot <- recurrencePlot$plot + coord_cartesian(xlim = c(0, maxYearsFollowup))
  #recurrencePlot
  
  # Newwer method, which requires survfit2 wrapper rather than survfit, and allows maxYears on plot + table
  recurrencePlot <- recurrenceFit |>
                    ggsurvfit(linewidth = 1) +
                    add_confidence_interval() + add_censor_mark() +
                    add_risktable(times=c(0:maxYearsFollowup), size=5) +
                    #add_quantile(y_value = 0.6, color = "gray50", linewidth = 0.75) +
                    scale_ggsurvfit() + coord_cartesian(xlim = c(0, maxYearsFollowup)) +
                    labs(title = "Local Recurrence Free Analysis", y = "Probability", x = "Time (Years)")
  
  recurrencePlot
}

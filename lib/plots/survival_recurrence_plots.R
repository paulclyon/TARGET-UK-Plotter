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

makeSurvivalPlot <- function(strStart, strEnd, maxYearsFollowup, selectedOrgans, selectedGenders, survivalType)
{
  # Filter the dates
  start <- as.Date(strStart, format = "%d/%m/%Y")
  end <- as.Date(strEnd, format = "%d/%m/%Y")
  
  if (!is.data.frame(survivalData) || nrow(survivalData) == 0) {
    return(ggplot()) # Any empty plot
  }
  filteredSurvivalData <<- survivalData |>
    filter(between(FirstRxDate, start, end)) |>
    filter(Organ %in% selectedOrgans) |>
    filter(Gender %in% selectedGenders)
  if (nrow(filteredSurvivalData) == 0) {
    return(ggplot())
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
    xlabStr            <- "Overall Survival (Years)"
    
  }
  else if (survivalType == 1) # This is cancer specific survival
  {
    survivalFit        <- ggsurvfit::survfit2(Surv(TimeSurvival, StatusCancerSpecificSurvival)~Organ, data = filteredSurvivalData)
    xlabStr            <- "Cancer Specific Survival (Years)"
  }
  else if (survivalType == 2)  # This is LRF survival
  {
    survivalFit        <- ggsurvfit::survfit2(Surv(TimeLRFS, StatusLRFS)~Organ, data = filteredSurvivalData)
    xlabStr            <- "Overall Local Recurrence-Free Survival (Years)"
  }
  
  # Original method but don't know how to change risk table to just e.g. 5 follow-up years
  #survivalPlot         <- ggsurvplot(survivalFit,
  #                                   ylab = "Probability",
  #                                   xlab = xlabStr,   risk.table = TRUE,
  #                                   ggtheme = theme(plot.title = element_text(hjust = 0.5)))
  #survivalPlot$plot    <- survivalPlot$plot + coord_cartesian(xlim = c(0, maxYearsFollowup))
  #survivalPlot
  
  # Newwer method, which requires survfit2 wrapper rather than survfit, and allows maxYears on plot + table
  survivalPlot <- survivalFit |>
                  ggsurvfit(linewidth = 1) +
                  add_confidence_interval() + add_censor_mark() +
                  add_risktable(times=c(0:maxYearsFollowup), size=5) +
                  #add_quantile(y_value = 0.6, color = "gray50", linewidth = 0.75) +
                  scale_ggsurvfit() + coord_cartesian(xlim = c(0, maxYearsFollowup))
  survivalPlot
}

makeRecurrencePlot <- function(strStart, strEnd, maxYearsFollowup, selectedOrgans)
{
  # Filter the dates
  start <- as.Date(strStart, format = "%d/%m/%Y")
  end <- as.Date(strEnd, format = "%d/%m/%Y")
  
  if (!is.data.frame(survivalData) || nrow(survivalData) <= 0) {
    return(ggplot())
  }
  
  filteredSurvivalData <<- survivalData |>
    filter(between(FirstRxDate, start, end)) |>
    filter(Organ %in% selectedOrgans)
  if (nrow(filteredSurvivalData) == 0) {
    return(ggplot())
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
                    scale_ggsurvfit() + coord_cartesian(xlim = c(0, maxYearsFollowup))
  recurrencePlot
}

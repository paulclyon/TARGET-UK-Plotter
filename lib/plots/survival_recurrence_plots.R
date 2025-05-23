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

makeSurvivalPlot <- function(strStart, strEnd, selectedOrgans, selectedGenders, lrfs)
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
  
  if (lrfs == 0) # This is the radiobutton for LRFS yes/no
  {
    survivalFit        <- survfit(Surv(TimeSurvival, StatusSurvival)~Organ, data = filteredSurvivalData) # This is plain old survival
    xlabStr            <- "Overall Survival (Years)"
    
  }
  else
  {
    survivalFit        <- survfit(Surv(TimeLRFS, StatusLRFS)~Organ, data = filteredSurvivalData)         # This is LRF survival
    xlabStr            <- "Overall Local Recurrence-Free Survival (Years)"
  }
  survivalPlot         <- ggsurvplot(survivalFit,
                                     ylab = "Probability",
                                     xlab = xlabStr,   risk.table = TRUE,
                                     ggtheme = theme(plot.title = element_text(hjust = 0.5)))
  survivalPlot
}

makeRecurrencePlot <- function(strStart, strEnd, selectedOrgans)
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
  recurrenceFit        <- survfit(Surv(TimeLRF, StatusLRF)~Organ, data = filteredSurvivalData)
  recurrencePlot       <- ggsurvplot(recurrenceFit,
                                     ylab = "Probability",
                                     xlab = "Time to Local Recurrence (Years)",   risk.table = TRUE,
                                     ggtheme = theme(plot.title = element_text(hjust = 0.5)))
  recurrencePlot
}

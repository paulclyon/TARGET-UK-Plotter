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

makeSurvivalPlot <- function(strStart, strEnd, selectedOrgans, selectedGenders)
{
  # Filter the dates
  start <- as.Date(strStart, format = "%d/%m/%Y")
  end <- as.Date(strEnd, format = "%d/%m/%Y")
  
  if (!is.data.frame(survivalData) || nrow(survivalData) == 0) {
    return(ggplot()) # Any empty plot
  }
  
  logger(paste("selected genders ",selectedGenders))
  
  filteredSurvivalData <- survivalData |>
    filter(between(FirstRxDate, start, end)) |>
    filter(Organ %in% selectedOrgans) |>
    filter(Gender %in% selectedGenders)
  
  logger (paste("survival filtered to ", nrow(survivalData), nrow(filteredSurvivalData)))
  
  
  if (nrow(filteredSurvivalData) == 0) {
    return(ggplot())
  }
  
  # Censoring = 1=censored, 2=dead - 
  # The sample is censored in that you only know that the individual survived up to the loss to followup,
  # but you don’t know anything about survival after that. I used to have 0=alive, but this isn't recognised
  # See https://thriv.github.io/biodatasci2018/r-survival.html
  
  #survivalFitSex      <<- survfit(Surv(Time, Status) ~ Sex, data = filteredSurvivalData)
  #survivalFitOrgan    <<- survfit(Surv(Time, Status) ~ Organ, data = filteredSurvivalData)
  #summary(survivalFit)
  survivalFit          <- survfit(Surv(Time, Status)~Organ, data = filteredSurvivalData)
  survivalPlot         <- ggsurvplot(survivalFit,
                                     xlab = "Time (Days)",
                                     ggtheme = theme(plot.title = element_text(hjust = 0.5)))
  survivalPlot
}

makeRecurrencePlotOrgan <- function(strStart, strEnd, selectedOrgans, selectedGenders)
{
  # Filter the dates
  start <- as.Date(strStart, format = "%d/%m/%Y")
  end <- as.Date(strEnd, format = "%d/%m/%Y")
  
  if (!is.data.frame(rxDoneData) || nrow(rxDoneData) <= 0) {
    return(ggplot())
  }
  
  filtered_df <- rxDoneData |>
    filter(between(RxDate, start, end)) |>
    filter(Organs %in% selectedOrgans)
  
  if (!is.data.frame(filtered_df) || nrow(filtered_df) <= 0) {
    return(ggplot())
  }
  
  #pie_df <- prepareOrganCountRecurrenceData(filtered_df)
  recurrenceFitOrgan <- survfit(Surv(Time, Status)~Organ, data=recurrenceData)
  p <- ggsurvplot(recurrenceFitOrgan,xlab="Follow-up Time (Days)",ggtheme=theme(plot.title=element_text(hjust=0.5)))
  p
}

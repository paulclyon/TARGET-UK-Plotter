makeTreatmentVolumePlot <- function(filteredRxDoneData, volumePlotDuration) {
  # Organ colour scale
  orgColors <- brewer.pal(length(organFactors), "Pastel1")
  names(orgColors) <- organFactors
  orgColScale <- scale_fill_manual(name = "Organs", values = orgColors)

  if (!is.null(nrow(filteredRxDoneData))) {
    if (volumePlotDuration == "year") {
      yAxisFreq <- 5
    } else if (volumePlotDuration == "month") {
      yAxisFreq <- 2
    } else # Must be by week...
    {
      yAxisFreq <- 1
    }

    volumePlot <<- ggplot(filteredRxDoneData, aes(
      x = lubridate::floor_date(RxDate, volumePlotDuration), fill = Organs,
      text = paste(
        ID, "-", RxDate, "\n",
        paste("Operators: ", Operator1, Operator2, Operator3, "\n"),
        paste("Rx Modality: ", Modality, " (£", Tariff, ")")
      )
    )) +
      geom_bar() +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)) +
      scale_y_continuous(breaks = seq(0, 100, by = yAxisFreq)) + # Use this to get an integer y-axis, the 100 is the number of max ticks after which x>100 still plots tickless
      labs(x = "Date", y = "Ablation Count") +
      ggtitle("Treatment Volume Plot") +
      theme(legend.position = "bottom") +
      orgColScale
  } else {
    volumePlot <<- NA
  }
  return(volumePlot)
}

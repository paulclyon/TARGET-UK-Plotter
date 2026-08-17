makeRxVolumePlot <- function(filteredRxDoneData, volumePlotDuration) {
  # Organ colour scale
  orgColors <- brewer.pal(length(organFactors), "Pastel1")
  names(orgColors) <- organFactors
  orgColScale <- scale_fill_manual(name = "Organ", values = orgColors)

  bucketStarts <- seq(
    lubridate::floor_date(min(filteredRxDoneData$RxDate), volumePlotDuration),
    lubridate::floor_date(max(filteredRxDoneData$RxDate), volumePlotDuration),
    by = switch(
      volumePlotDuration,
      "week" = "1 week",
      "month" = "1 month",
      "quarter" = "3 months",
      "year" = "1 year"
    )
  )
  
  halfBucketDays <- switch(
    volumePlotDuration,
    "week" = 3.5,
    "month" = 15.5,
    "quarter" = 46,
    "year" = 183
  )
  
  xAxisScale <- if (volumePlotDuration == "quarter") {
    scale_x_date(
      breaks = bucketStarts,
      labels = function(x) {
        quarterEnd <- vapply(
          as.Date(x),
          function(quarterStart) {
            format(
              seq(quarterStart, by = "3 months", length.out = 2)[2] - 1,
              "%b %y"
            )
          },
          character(1)
        )
        paste0(format(x, "%b"), "–", quarterEnd)
      },
      limits = range(c(bucketStarts - halfBucketDays, bucketStarts + halfBucketDays)),
      expand = expansion(mult = 0)
    )
  } else {
    scale_x_date(
      breaks = bucketStarts,
      date_labels = switch(
        volumePlotDuration,
        "week" = "%e %b %y",
        "month" = "%b %y",
        "year" = "%Y"
      ),
      limits = range(c(bucketStarts - halfBucketDays, bucketStarts + halfBucketDays)),
      expand = expansion(mult = 0)
    )
  }
  
  if (!is.null(nrow(filteredRxDoneData))) {
    if (volumePlotDuration == "year") {
      yAxisFreq <- 5
    } else if (volumePlotDuration %in% c("month", "quarter")) {
      yAxisFreq <- 2
    } else # Must be by week...
    {
      yAxisFreq <- 1
    }

    volumePlot <<- ggplot(filteredRxDoneData, aes(
      x = lubridate::floor_date(RxDate, volumePlotDuration), fill = Organ,
      text = paste(
        ID, "-", RxDate, "\n",
        paste("Operators: ", Operator1, Operator2, Operator3, "\n"),
        paste("Rx Modality: ", RxModality, " (£", Tariff, ")")
      )
    )) +
      geom_bar() +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)) +
      xAxisScale +
      scale_y_continuous(breaks = seq(0, 100, by = yAxisFreq)) + # Use this to get an integer y-axis, the 100 is the number of max ticks after which x>100 still plots tickless
      labs(
        x = "Date",
        y = paste0(
          "Ablation Count (per ",
          tools::toTitleCase(volumePlotDuration),
          ")"
        )
      ) +
    
      ggtitle(
        paste0(
          "Treatment Volume Plot (",
          format(min(filteredRxDoneData$RxDate, na.rm = TRUE), "%d/%b/%Y"),
          " – ",
          format(max(filteredRxDoneData$RxDate, na.rm = TRUE), "%d/%b/%Y"),
          ")"
        )
      ) +
      
      theme(legend.position = "bottom") +
      orgColScale
  } else {
    volumePlot <<- NA
  }
  return(volumePlot)
}

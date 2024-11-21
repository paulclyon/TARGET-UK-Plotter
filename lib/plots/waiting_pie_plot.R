makeRxWaitPie <- function(inputStartDate, inputEndDate, inputOrganList) {
  # Initialise some stuff
  startDate <- as.Date(inputStartDate, format = "%d/%m/%Y")
  endDate <- as.Date(inputEndDate, format = "%d/%m/%Y")
  rxWaitOrganCounts <<- c()
  rxWaitOrganPercents <<- c()

  # Now for the Rx waiting Pie...
  if (is.data.frame(rxWaitData) && nrow(rxWaitData > 0)) {
    # Filter just the organs we need from rxWaitData
    rxWaitData.filtered <<- rxWaitData %>% filter(rxWaitData$Organs %in% inputOrganList)

    # Now just because the user asked for an organ doesn't mean we have any of that organ on waiting list, so need to factor...
    rxWaitOrganFactors.filtered <<- levels(factor(rxWaitData.filtered$Organs))
    rxWaitOrganFactors.final <<- c()

    # Get the data for the Rx Wait Organ Pie Chart
    for (organ in rxWaitOrganFactors.filtered)
    {
      count <- length(which(rxWaitData.filtered$Organs == organ))
      if (count > 0) {
        rxWaitOrganFactors.final <<- c(rxWaitOrganFactors.final, organ)
        rxWaitOrganCounts <<- c(rxWaitOrganCounts, count)
        rxWaitOrganPercents <<- c(rxWaitOrganPercents, count)
      }
    }

    waitingTotal <- sum(rxWaitOrganCounts)
    rxWaitOrganPercents <<- rxWaitOrganPercents / waitingTotal
    rxWaitOrganPie.df <<- data.frame(
      OrganRxFactors = rxWaitOrganFactors.final,
      OrganRxCounts = rxWaitOrganCounts,
      OrganRxPercents = rxWaitOrganPercents
    )

    if (waitingTotal > 0) {
      # Make and label the pie chart
      rxWaitOrganPie.df <<- rxWaitOrganPie.df %>%
        mutate(
          csum = rev(cumsum(rev(OrganRxCounts))),
          pos = OrganRxCounts / 2 + lead(csum, 1),
          pos = if_else(is.na(pos), OrganRxCounts / 2, pos)
        )

      logger(paste("FIXME123", length(rxWaitOrganPie.df$OrganRxCounts), length(fct_inorder(rxWaitOrganPie.df$OrganRxFactors)), length(rxWaitOrganCounts), length(rxWaitOrganPercents)))
      logger(paste("Glue: ", glue::glue("{rxWaitOrganCounts} ({scales::percent(rxWaitOrganPercents)}")))

      p <- ggplot(rxWaitOrganPie.df, aes(x = "", y = OrganRxCounts, fill = fct_inorder(OrganRxFactors))) +
        geom_col(width = 1, color = "grey") +
        coord_polar(theta = "y") +
        scale_fill_brewer(palette = "Pastel1") +
        guides(fill = guide_legend(title = "Waiting List Piechart")) +
        theme_void()

      # Weirdly the labelling is tied in with the dynamic refresh, without this line the pie doesn't refresh itself
      # The label glue attribute is interesting: if you replace with OrganRxCounts and OrganPercents automatic update is lost
      p <- p + geom_label_repel(
        data = rxWaitOrganPie.df,
        aes(
          y = pos,
          label = glue::glue("{rxWaitOrganCounts} ({scales::percent(rxWaitOrganPercents)})"),
          fill = OrganRxFactors
        ),
        size = 5, nudge_x = 3, show.legend = FALSE
      )
      rxwaitPie <<- p
    } else {
      rxwaitPie <<- ggplot()
    }
  }
}

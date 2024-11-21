makeRxDonePie <- function(inputStartDate, inputEndDate, inputOrganList) {
  # Initialise some stuff
  startDate <- as.Date(inputStartDate, format = "%d/%m/%Y")
  endDate <- as.Date(inputEndDate, format = "%d/%m/%Y")
  rxDoneOrganCounts <<- c()
  rxDoneOrganPercents <<- c()

  if (is.data.frame(rxDoneData) && nrow(rxDoneData > 0)) {
    # Filter just the dates we need from rxDoneData
    rxDoneData.filtered <- rxDoneData %>% filter(between(
      RxDate, as.Date(inputStartDate, format = "%d/%m/%Y"),
      as.Date(inputEndDate, format = "%d/%m/%Y")
    ))
    rxDoneData.filtered <- rxDoneData.filtered %>% filter(rxDoneData.filtered$Organs %in% inputOrganList)
    rxDoneOrganFactors.filtered <<- levels(factor(rxDoneData.filtered$Organs))
    rxDoneOrganFactors.final <<- c()

    # Get the data for the Rx Done Organ Pie Chart
    for (organ in rxDoneOrganFactors.filtered)
    {
      count <- length(which(rxDoneData.filtered$Organs == organ))
      if (count > 0) {
        rxDoneOrganFactors.final <<- c(rxDoneOrganFactors.final, organ)
        rxDoneOrganCounts <<- c(rxDoneOrganCounts, count)
        rxDoneOrganPercents <<- c(rxDoneOrganPercents, count)
      }
    }

    treatedTotal <- sum(rxDoneOrganCounts)
    rxDoneOrganPercents <<- rxDoneOrganPercents / treatedTotal
    rxDoneOrganPie.df <<- data.frame(
      OrganRxFactors = rxDoneOrganFactors.final,
      OrganRxCounts = rxDoneOrganCounts,
      OrganRxPercents = rxDoneOrganPercents
    )

    logger(paste("FIXME4", rxDoneOrganCounts))
    logger(paste("FIXME5", rxDoneOrganFactors.final))
    logger(paste("FIXME6", treatedTotal))
    logger(paste("FIXME7", rxDoneOrganPercents))
    logger(paste(glue::glue("{rxDoneOrganPie.df$OrganRxCounts} ({scales::percent(rxDoneOrganPie.df$OrganRxPercents)})")))
    logger(paste(glue::glue("{rxDoneOrganCounts} ({scales::percent(rxDoneOrganPercents)})")))

    if (treatedTotal > 0) {
      # Make and label the pie chart
      rxDoneOrganPie.df <<- rxDoneOrganPie.df %>%
        mutate(
          csum = rev(cumsum(rev(OrganRxCounts))),
          pos = OrganRxCounts / 2 + lead(csum, 1),
          pos = if_else(is.na(pos), OrganRxCounts / 2, pos)
        )

      p <- ggplot(rxDoneOrganPie.df, aes(x = "", y = OrganRxCounts, fill = fct_inorder(OrganRxFactors))) +
        geom_col(width = 1, color = "grey") +
        coord_polar(theta = "y") +
        scale_fill_brewer(palette = "Pastel1") +
        guides(fill = guide_legend(title = "Treated Organs Piechart")) +
        theme_void()

      logger(paste("FIXME8", nrow(rxDoneOrganPie.df)))

      # Weirdly the labelling is tied in with the dynamic refresh, without this line the pie doesn't refresh itself
      # The label glue attribute is interesting: if you replace with OrganRxCounts and OrganPercents automatic update is lost
      p <- p + geom_label_repel(
        data = rxDoneOrganPie.df,
        aes(
          y = pos,
          label = glue::glue("{rxDoneOrganCounts} ({scales::percent(rxDoneOrganPercents)})"),
          fill = OrganRxFactors
        ),
        size = 5, nudge_x = 3, show.legend = FALSE
      )
      rxdonePie <<- p
    } else {
      rxdonePie <<- ggplot()
    }
  }
}

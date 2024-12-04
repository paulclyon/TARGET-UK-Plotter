prepareOrganCountPieData <- function(filtered_df) {
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

makeOrganCountPie <- function(pie_df) {
  ggplot(pie_df, aes(x = "", y = Count, fill = fct_inorder(Organ))) +
    geom_col(width = 1, color = "grey") +
    coord_polar(theta = "y") +
    scale_fill_brewer(palette = "Pastel1") +
    theme_void() +
    geom_label_repel(
      aes(
        y = pos,
        label = force(glue::glue("{Count} ({scales::percent(Percent)})")),
        fill = Organ
      ),
      size = 5, nudge_x = 3, show.legend = FALSE
    )

}

makeRxDonePie <- function(strStart, strEnd, selectedOrgans) {
  # Initialise some stuff
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

  pie_df <- prepareOrganCountPieData(filtered_df)

  p <- makeOrganCountPie(pie_df) +
    guides(fill = guide_legend(title = "Treated Organs Piechart"))

  p
}


makeRxWaitPie <- function(strStart, strEnd, selectedOrgans) {
  # Initialise some stuff
  start <- as.Date(strStart, format = "%d/%m/%Y") # Not used!
  end <- as.Date(strEnd, format = "%d/%m/%Y")

  if (!is.data.frame(rxWaitData) || nrow(rxWaitData) <= 0) {
    return(ggplot())
  }

  filtered_df <- rxWaitData |>
    full_join(rxDoneData) |>
    filter(is.na(RxDate) | RxDate < end) |> # Only include those that have not been treated or treated by the end date
    filter(RefDate < end) |> # Only include those that have been referred by the end date
    filter(Organs %in% selectedOrgans)

  if (!is.data.frame(filtered_df) || nrow(filtered_df) <= 0) {
    return(ggplot())
  }

  pie_df <- prepareOrganCountPieData(filtered_df)

  p <- makeOrganCountPie(pie_df) +
    guides(fill = guide_legend(title = "Waiting List Piechart"))

  p
}

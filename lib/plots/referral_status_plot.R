processReferralStatusPerPeriod <- function(doneData, waitingData, start, end, range_by_string) {
  range_by <- "month"
  if (grepl("(\\d+ )?((day)|(week)|(month)|(quarter)|(year))s?", range_by_string)) {
    range_by <- range_by_string
  }

  yAxisFreq <- 1
  if (grepl("year", range_by)) {
    yAxisFreq <- 5
  } else if (grepl("(month)|(quarter)", range_by)) {
    yAxisFreq <- 2
  }

  range_sequence <- seq(as.Date(start), as.Date(end), range_by)

  range_tibble <- tibble(start = start, end = end, name = paste(start, "–", end, sep = "\n"))
  if (length(range_sequence) > 1) {
    range_tibble <- tibble(
      start = range_sequence[1:length(range_sequence) - 1],
      end = range_sequence[2:length(range_sequence)],
      name = paste(start, "–", end, sep = "\n")
    )
  }

  range_tibble <- range_tibble |> select(name, start, end)

  range_start <- function(name) {
    range_tibble$start[range_tibble$name == name]
  }
  range_end <- function(name) {
    range_tibble$end[range_tibble$name == name]
  }

  status_set_in <- function(x, status_name, status_field, period) {
    l <- list()
    l[[status_name]] <- sapply(x[[status_field]], \(x) ((as.Date(x) < as.Date(range_end(period))) && (as.Date(x) >= as.Date(range_start(period)))))
    as_tibble(l)
  }

  status_set_between <- function(x, status_name, start_status, end_status, period) {
    l <- list()
    l[[status_name]] <- sapply(x[[start_status]], \(x) ((as.Date(x) < range_start(period)))) & sapply(x[[end_status]], \(x) (is.na(x) || (as.Date(x) >= range_end(period)))) & sapply(x$RxDate, \(x) (is.na(x) || as.Date(x) >= range_end(period)))
    as_tibble(l)
  }

  date_columns <- c("RefDate" = "Referred", "DTTDate" = "DTT", "RxDate" = "RxD") # Ideally we want the clock stops too
  status_fns <- c(
    Referred = \(x, period) status_set_in(x, "Referred", "RefDate", period),
    WaitingDTT = \(x, period) status_set_between(x, "WaitingDTT", "RefDate", "DTTDate", period),
    DTT = \(x, period) status_set_in(x, "DTT", "DTTDate", period),
    WaitingRx = \(x, period) status_set_between(x, "WaitingRx", "DTTDate", "RxDate", period),
    RxD = \(x, period) status_set_in(x, "RxD", "RxDate", period)
  )

  statuses <- c("Referred", "WaitingDTT", "Referred+DTT", "DTT", "WaitingRx", "Referred+WaitingRx", "Referred+DTT+RxD", "DTT+RxD", "Referred+RxD", "RxD")

  doneData |>
    full_join(waitingData) |> # Join the done data with the waiting data so we can review all patients
    (\(allData) {
      range_tibble$name |>
        purrr::map(function(period) { # For each period in the provided range
          unname(status_fns) |> # For each status in the status fn list
            purrr::map(\(fn) fn(allData, period)) |> # check if each patient has that status and report as a column
            purrr::list_cbind() |> # bind all these columns together as a table
            cbind(ID = allData$ID) |> # Add back in the ID column
            tidyr::pivot_longer(cols = names(status_fns)) |> # pivot from wide to long format
            filter(value == TRUE) |> # Only keep the true values
            group_by(ID) |> # Group by ID
            summarize(`status` = paste(name, collapse = "+")) |> # and merge all the statuses together with '+'
            cbind(period = range_start(period))
        }) |>
        list_rbind() |>
        cbind(Count = 1)
    })() |>
    mutate(ID = ID, Period = period, Status = factor(status, levels = rev(statuses), ordered = T), Count = Count, .keep = "none")
}

makeReferralStatusPlot <- function(referralStatuses) {
  referralStatuses |>
    group_by(Period, Status) |>
    summarise(Count = n(), ID = paste(ID, collapse = ", ")) |>
    select(Status, Period, Count, ID) |>
    ggplot(aes(fill = Status, x = Period, y = Count, text = paste("IDs:", ID))) + # And then plot as a bar chart
    geom_bar(stat = "identity") +
    ggtitle("Referral Status Plot") +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)) +
    scale_fill_brewer(palette = "Paired")
}

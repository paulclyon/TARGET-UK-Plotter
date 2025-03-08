processWaitTimesPerPeriod <- function(doneData, waitingData, start, end, range_by_string) {
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

  status_fns <- c(
    RefToDTT = \(x, period) {
      x |>
        # Only include patients referred before the end date
        filter(RefDate < as.Date(range_end(period)) &
          # & either DTTDate after the start of the period
          (DTTDate >= as.Date(range_start(period)) |
            # or DTTDate is missing and RxDate is missing or after the start of the period
            (is.na(DTTDate) &
              (is.na(RxDate) | RxDate >= as.Date(range_start(period)))))) |>
        mutate(
          DTTDateFixed = pmin(DTTDate, as.Date(range_end(period)), RxDate, na.rm = T),
          RefToDTT = DTTDateFixed - RefDate,
          RefToDTTClockStopped = ifelse(is.na(ClockStopDaysPreDTT),
            RefToDTT,
            RefToDTT + ClockStopDaysPreDTT
          ),
          DTTStopped = RefToDTTClockStopped < 0,
          RefToDTTNoStopped = ifelse(DTTStopped, NA, RefToDTTClockStopped),
        ) |>
        summarise(
          MeanRefToDTT = mean(RefToDTTNoStopped, na.rm = TRUE),
          RefToDTTStopped = sum(DTTStopped, na.rm = TRUE),
          RefToDTTNotStopped = sum(RefToDTTClockStopped >= 0, na.rm = TRUE),
          "RefToDTT ≤10 days" = sum(RefToDTTNoStopped <= 10, na.rm = TRUE),
          "RefToDTT ≤21 days" = sum(RefToDTTNoStopped > 10 & RefToDTTNoStopped <= 21, na.rm = TRUE),
          "RefToDTT >21 days" = sum(RefToDTTNoStopped > 21, na.rm = TRUE)
        )
    },
    DTTToRx = \(x, period) {
      x |>
        filter(
          # Only include patients who had a DTT date within the period
          (DTTDate < as.Date(range_end(period)) &
            DTTDate >= as.Date(range_start(period))) |
            # or patients who have no DTT date recorded but have a RxDate in the period
            (is.na(DTTDate) & !is.na(RxDate) &
              RxDate < as.Date(range_end(period)) &
              RxDate >= as.Date(range_start(period)))
        ) |>
        # Then filter again to remove patients treated before the period
        filter(is.na(RxDate) |
          RxDate >= as.Date(range_start(period))) |>
        mutate(
          DTTDateFixed = pmin(DTTDate, RxDate, na.rm = T),
          RxDateFixed = pmin(RxDate, as.Date(range_end(period)), na.rm = T),
          DTTToRx = RxDateFixed - DTTDateFixed,
          DTTToRxClockStopped = ifelse(is.na(ClockStopDaysPostDTT),
            DTTToRx,
            DTTToRx + ClockStopDaysPostDTT
          ),
          RxStopped = DTTToRxClockStopped < 0,
          DTTToRxNoStopped = ifelse(RxStopped, NA, DTTToRxClockStopped),
        ) |>
        summarise(
          MeanDTTToRx = mean(DTTToRxNoStopped, na.rm = TRUE),
          DTTToRxStopped = sum(RxStopped, na.rm = TRUE),
          DTTToRxNotStopped = sum(DTTToRxClockStopped >= 0, na.rm = TRUE),
          "DTTToRx ≤31 days" = sum(DTTToRxNoStopped <= 31, na.rm = TRUE),
          "DTTToRx ≤45 days" = sum(DTTToRxNoStopped > 31 & DTTToRxNoStopped <= 45, na.rm = TRUE),
          "DTTToRx ≤60 days" = sum(DTTToRxNoStopped > 45 & DTTToRxNoStopped <= 60, na.rm = TRUE),
          "DTTToRx >60 days" = sum(DTTToRxNoStopped > 60, na.rm = TRUE)
        )
    },
    RefToRx = \(x, period) {
      x |>
        filter(
          # Only include patients who had a referral date before the end of the period
          RefDate < as.Date(range_end(period)) &
            # and either haven't had treatment yet or a rxDate after the start of the period
            (is.na(RxDate) | RxDate >= as.Date(range_start(period)))
        ) |>
        mutate(
          RxDateFixed = pmin(RxDate, as.Date(range_end(period)), na.rm = T),
          RefToRx = RxDateFixed - RefDate,
          ClockStopDays = replace_na(ClockStopDaysPreDTT, 0) + replace_na(ClockStopDaysPostDTT, 0),
          RefToRxClockStopped = RefToRx + ClockStopDays,
          RxStopped = RefToRxClockStopped < 0,
          RefToRxNoStopped = ifelse(RxStopped, NA, RefToRxClockStopped),
        ) |>
        summarise(
          MeanRefToRx = mean(RefToRxNoStopped, na.rm = TRUE),
          RefToRxStopped = sum(RxStopped, na.rm = TRUE),
          RefToRxNotStopped = sum(RefToRxClockStopped >= 0, na.rm = TRUE),
          "RefToRx ≤90 days" = sum(RefToRxNoStopped <= 90, na.rm = TRUE),
          "RefToRx >90 days" = sum(RefToRxNoStopped > 90, na.rm = TRUE)
        )
    }
  )

  doneData |>
    full_join(waitingData) |>
    (\(allData) {
      range_tibble$name |>
        purrr::map(function(period) {
          unname(status_fns) |>
            purrr::map(\(fn) fn(allData, period)) |>
            purrr::list_cbind() |>
            cbind(Period = period, PeriodStart = range_start(period), PeriodEnd = range_end(period))
        }) |>
        purrr::list_rbind()
    })()
}

refToDTTMeanPlot <- function(referralTimes, range_by = "Monthly") {
  maxY <- max(25, ceiling((max(referralTimes$MeanRefToDTT, na.rm = T) + 1) / 5) * 5)

  referralTimes |>
    mutate(BreachStatus = if_else(MeanRefToDTT > 10, if_else(MeanRefToDTT > 21, ">21 days", ">10 days"), "<10 days")) |>
    ggplot() +
    geom_line(aes(x = PeriodStart, y = MeanRefToDTT, color = "grey")) +
    geom_point(aes(x = PeriodStart, y = MeanRefToDTT, color = BreachStatus)) +
    geom_hline(yintercept = 10, linetype = "dashed", color = "orange") +
    geom_hline(yintercept = 21, linetype = "dashed", color = "red") +
    scale_color_manual(
      name = "Breach Status",
      values = c("≤10 days" = "grey", "≤21 days" = "orange", ">21 days" = "red")
    ) +
    guides(color = "none") +
    scale_x_date(date_breaks = "month", date_labels = "%b-%y") +
    scale_y_continuous(breaks = seq(0, maxY, by = 5), limits = c(0, maxY)) +
    labs(
      title =
        paste0(
          "Referral to DTT Time ",
          "(By ",stringr::str_to_title(range_by),")",
          "\n(Generated ",
          format(Sys.time(), "%a %b %d %Y %X"), ")"
        ),
      x = "",
      y = "Number of Days"
    ) +
    theme(plot.title = element_text(size = 10)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.spacing.y = unit(0.1, "cm"))
}

refToDTTCountPlot <- function(referralTimes, range_by = "Monthly") {
  referralTimes |>
    select(
      PeriodStart, `RefToDTT ≤10 days`,
      `RefToDTT ≤21 days`, `RefToDTT >21 days`
    ) |>
    pivot_longer(cols = -PeriodStart, names_to = "Breach Status", values_to = "Count") |>
    mutate(
      `Breach Status` = case_match(
        `Breach Status`,
        "RefToDTT ≤10 days" ~ "≤10 days",
        "RefToDTT ≤21 days" ~ "≤21 days",
        "RefToDTT >21 days" ~ ">21 days"
      ),
      `Breach Status` = factor(
        `Breach Status`,
        levels = c(">21 days", "≤21 days", "≤10 days")
      )
    ) |>
    ggplot() +
    geom_bar(aes(x = PeriodStart, y = Count, fill = `Breach Status`),
      stat = "identity",
      alpha = 0.75,
      just = 0,
      position = "stack"
    ) +
    scale_x_date(date_breaks = "month", date_labels = "%b-%y") +
    labs(
      title =
        paste0(
          "Referral to DTT Count ",
          "(By ",stringr::str_to_title(range_by),")",
          "\n(Generated ",
          format(Sys.time(), "%a %b %d %Y %X"), ")"
        ),
      x = "",
      y = "Number of Patients"
    ) +
    theme(plot.title = element_text(size = 10)) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.spacing.y = unit(0.1, "cm"),
      legend.position = "bottom"
    )
}

dttToRxMeanPlot <- function(referralTimes, range_by = "Monthly") {
  maxY <- max(65, ceiling((max(referralTimes$MeanDTTToRx, na.rm = T) + 1) / 5) * 5)

  referralTimes |>
    mutate(BreachStatus = case_when(
      MeanDTTToRx > 60 ~ ">60 days",
      MeanDTTToRx > 45 ~ "≤60 days",
      MeanDTTToRx > 31 ~ "≤45 days",
      TRUE ~ "≤31 days"
    )) |>
    ggplot() +
    geom_line(aes(x = PeriodStart, y = MeanDTTToRx, color = "grey")) +
    geom_point(aes(x = PeriodStart, y = MeanDTTToRx, color = BreachStatus)) +
    geom_hline(yintercept = 31, linetype = "dashed", color = "orange") +
    geom_hline(yintercept = 45, linetype = "dashed", color = "red") +
    geom_hline(yintercept = 60, linetype = "dashed", color = "darkred") +
    scale_color_manual(
      name = "Breach Status",
      values = c("≤31 days" = "grey", "≤45 days" = "orange", "≤60 days" = "red", ">60 days" = "darkred")
    ) +
    guides(color = "none") +
    scale_x_date(date_breaks = "month", date_labels = "%b-%y") +
    scale_y_continuous(breaks = seq(0, maxY, by = 5), limits = c(0, maxY)) +
    labs(
      title =
        paste0(
          "DTT to Rx Times ",
          "(By ",stringr::str_to_title(range_by),")",
          "\n(Generated ",
          format(Sys.time(), "%a %b %d %Y %X"), ")"
        ),
      x = "",
      y = "Number of Days"
    ) +
    theme(plot.title = element_text(size = 10)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.spacing.y = unit(0.1, "cm"))
}

dttToRxCountPlot <- function(referralTimes, range_by = "Monthly") {
  referralTimes |>
    select(
      PeriodStart, `DTTToRx ≤31 days`,
      `DTTToRx ≤45 days`, `DTTToRx ≤60 days`, `DTTToRx >60 days`
    ) |>
    pivot_longer(cols = -PeriodStart, names_to = "Breach Status", values_to = "Count") |>
    mutate(
      `Breach Status` = case_match(
        `Breach Status`,
        "DTTToRx ≤31 days" ~ "≤31 days",
        "DTTToRx ≤45 days" ~ "≤45 days",
        "DTTToRx ≤60 days" ~ "≤60 days",
        "DTTToRx >60 days" ~ ">60 days"
      ),
      `Breach Status` = factor(
        `Breach Status`,
        levels = c(
          ">60 days",
          "≤60 days",
          "≤45 days",
          "≤31 days"
        )
      )
    ) |>
    ggplot() +
    geom_bar(aes(x = PeriodStart, y = Count, fill = `Breach Status`),
      stat = "identity",
      alpha = 0.75,
      just = 0,
      position = "stack"
    ) +
    scale_x_date(date_breaks = "month", date_labels = "%b-%y") +
    labs(
      title =
        paste0(
          "DTT to Rx Counts ",
          "(By ",stringr::str_to_title(range_by),")",
          "\n(Generated ",
          format(Sys.time(), "%a %b %d %Y %X"), ")"
        ),
      x = "",
      y = "Number of Patients"
    ) +
    theme(plot.title = element_text(size = 10)) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.spacing.y = unit(0.1, "cm"),
      legend.position = "bottom"
    )
}

refToRxMeanPlot <- function(referralTimes, range_by = "Monthly") {
  maxY <- max(90, ceiling((max(referralTimes$MeanRefToRx, na.rm = T) + 1) / 5) * 5)

  referralTimes |>
    mutate(BreachStatus = if_else(MeanRefToRx > 90, ">90 days", "<90 days")) |>
    ggplot() +
    geom_line(aes(x = PeriodStart, y = MeanRefToRx, color = "grey")) +
    geom_point(aes(x = PeriodStart, y = MeanRefToRx, color = BreachStatus)) +
    geom_hline(yintercept = 90, linetype = "dashed", color = "red") +
    scale_color_manual(
      name = "Breach Status",
      values = c("<90 days" = "grey", ">90 days" = "red")
    ) +
    guides(color = "none") +
    scale_x_date(date_breaks = "month", date_labels = "%b-%y") +
    scale_y_continuous(breaks = seq(0, maxY, by = 5), limits = c(0, maxY)) +
    labs(
      title =
        paste0(
          "Referral to Rx Time ",
          "(By ",stringr::str_to_title(range_by),")",
          "\n(Generated ",
          format(Sys.time(), "%a %b %d %Y %X"), ")"
          
        ),
      x = "",
      y = "Number of Days"
    ) +
    theme(plot.title = element_text(size = 10)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.spacing.y = unit(0.1, "cm"))
}

refToRxCountPlot <- function(referralTimes, range_by = "Monthly") {
  referralTimes |>
    select(PeriodStart, `RefToRx ≤90 days`, `RefToRx >90 days`) |>
    pivot_longer(cols = -PeriodStart, names_to = "Breach Status", values_to = "Count") |>
    mutate(
      `Breach Status` = case_match(
        `Breach Status`,
        "RefToRx ≤90 days" ~ "≤90 days",
        "RefToRx >90 days" ~ ">90 days"
      ),
      `Breach Status` = factor(`Breach Status`, levels = c(">90 days", "≤90 days"))
    ) |>
    ggplot() +
    geom_bar(aes(x = PeriodStart, y = Count, fill = `Breach Status`),
      stat = "identity",
      alpha = 0.75,
      just = 0,
      position = "stack"
    ) +
    scale_x_date(date_breaks = "month", date_labels = "%b-%y") +
    labs(
      title =
        paste0(
          "Referral to Rx Count ",
          "(By ",stringr::str_to_title(range_by),")",
          "\n(Generated ",
          format(Sys.time(), "%a %b %d %Y %X"), ")"
        ),
      x = "",
      y = "Number of Patients"
    ) +
    theme(plot.title = element_text(size = 10)) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.spacing.y = unit(0.1, "cm"),
      legend.position = "bottom"
    )
}

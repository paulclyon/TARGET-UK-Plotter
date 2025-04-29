# Implemented by Dr. Andrew Thornton.
# How it works:
#
# By Ablation Date:
# Group patients by their ablation date, i.e. the date that is the x-axis of the graph is the Ablation date. Compare:
#
# By DTT date:
# Group patients by their DTT date (or if that is missing and they have an RxDate use that), i.e. the date that is the x-axis of the graph is the DTT date. (This would apply to the Ref-To-DTT graph only). See:
#  
# Waiting :
# Count the patients who are waiting for a DTT or Rx in each period of time. This is slightly different for each section:
#
# In RefToDTT:
# For each period of time, get the patients whose RefDate was before the end of the period but haven't had a DTT before the end. 
# 
# In DTTToRx:
# For each period of time, get the patients who have had a DTT made before the end of the period but haven't had Rx before the end
#
# In RefToRx:
# For each period of time, get the patients whose RefDate was before the end of the period but haven't had a Rx before the end of the period.
#
# All:
# Count both the number of patients who are waiting for DTT/Rx and who have had their DTT/Rx  in each period.
#
# So similar to waiting, for RefToDTT and RefToRx filter all of the patients whose RefDate was before the end of the period, and filter only those whose DTTDate/RxDate was after the start of the period, or it is unset. In DTTToRx, filter all of the patients whose DTTDate was before the end of the period and whose RxDate is after the start of the period or is unset. See for example:
# 
# In all of the above cases there are adjustments made when making the summaries for ClockStops, (see the appropriate summarise... functions)
# Why have these?
# Well By Ablation Date only gives you an idea of those patients you've actually completed treatment on and it can falsely represent where/when the problems are. For example, when using by ablation date, the period used for the x-axis on the RefToDTT graph does not represent either RefDate or DTTDate, and depending on how long ago the period between DTTDate and RxDate the DTT may have occurred quite a while ago. There's no indication of what the current wait times are, or how many people are waiting. Consider the patient who has been waiting for 12 months for an ablation and who is still waiting, on a by ablation date basis this person will not appear and in fact it's better for your graphs and data that they never get treated! When they do finally get treated there will be a spike only at the point that they are treated making it look like the problem was in the month they got treated, not before then.
# So, why would we use By Ablation Date by default for these values? Well, it's often only after Ablation has occurred that data (in particular ClockStops) is completely filled in, so the other values can be more error prone and over-estimate how bad the situation actually is. 
# If these are likely to be error prone why have them? Well, for a start it should encourage better more timely data input to make them more accurate - and without them you have no idea how long people who are waiting have been waiting.
#
# Q. If the Waiting group selected, wouldn’t the Ref-To-Rx plot go blank?
# A. No as it's showing the current waiting time for those who are awaiting treatment in each period.
#    The All group is similar to that of the current waiting times but also includes those who are treated within the period, i.e. A would be counted in March, with their waiting time being fixed to RxDate instead of the end of March.
#    The idea is to give a picture of the current state of the waiting list, instead of just those who are treated. The problem with it is that in order for it to be correct you need to keep on top of the ClockStops and put them in proactively, as people who should be clock stopped but are not marked stopped will continue to accumulate current waiting time and distort the data. (There's also a slight issue in that the ClockStops are front loaded rather than only taking effect when they actually occur but that's a minor thing.)
#    If you just rely on the By Ablation Date graphs you're only assessing how long people who are treated are waiting and are not showing what your current actual waiting times are. It defers the presentation of a problem to the date of the ablation, not when it's actually occuring. 
#    Say for example, one you goes off sick for a while, during the period of sickness the by ablation date graphs will in general look fine, with perhaps a slow worsening - because anyone who is treated is likely to have waited a similar length of time to before the sickness. However, in reality there will be more and more people waiting, and their current waiting times will be increasing. When the sickness is over and that person comes back or a locum is appointed, By Ablation Date graphs will suddenly look worse because of the catch-up. If someone is relying on the By Ablation Date graphs to justify a locum or their own appointment this both underestimates the problem when it's happening AND overestimates it when the solution is in place - making the newly appointed consultant look like they're worsening the situation instead of improving things. 
#    In a similar fashion By Ablation Date is particularly bad for DTT related delays.
#    Imagine an extreme case of someone who has been missed, and thus has been waiting 2 years for treatment. In the By Ablation Date graphs they will not appear at all, until they're actually treated making it look like the problem was the day they were finally treated instead of them appearing all the way through.
#
# Clock Stops are a bit tricky here, rather than splicing it up, clock stops are assumed as pre-loaded at the start to ensure waiting is monotonic, in other words always increasing until treatment

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

  # Andy reassures me that the patients without DTT date are excluded from the Ref-to-DTT data as they are NA'd (missing data)
  summariseRefToDTT <- function(x) {
    x |>
      mutate(
        RefToDTT = DTTDateFixed - RefDate,
        RefToDTTClockStopped = ifelse(is.na(ClockStopDaysPreDTT),
          RefToDTT,
          RefToDTT + ClockStopDaysPreDTT
        ),
        DTTStopped = RefToDTTClockStopped < 0,
        RefToDTTNoStopped = ifelse(DTTStopped, NA, RefToDTTClockStopped),
        MissingDTT = is.na(DTTDate) & !is.na(RxDate),
        ValidDTT   = !is.na(DTTDate) & !is.na(RxDate)
      ) |>
      summarise(
        NotStopped = list(RefToDTTNoStopped),
        Mean = mean(RefToDTTNoStopped, na.rm = TRUE),
        CountStopped = sum(DTTStopped, na.rm = TRUE),
        CountNotStopped = sum(RefToDTTClockStopped >= 0, na.rm = TRUE),
        "≤10 days" = sum(RefToDTTNoStopped <= 10, na.rm = TRUE),
        "≤21 days" = sum(RefToDTTNoStopped > 10 & RefToDTTNoStopped <= 21, na.rm = TRUE),
        ">21 days" = sum(RefToDTTNoStopped > 21, na.rm = TRUE),
        "Treated but missing DTT" = sum(MissingDTT, na.rm = TRUE),
        "Treated with DTT" = sum(ValidDTT, na.rm = TRUE),
        "Treated Total" = sum(MissingDTT, na.rm = TRUE) + sum(ValidDTT, na.rm = TRUE)
      ) |>
      mutate(
        measure = "RefToDTT",
      )
  }

  summariseDTTToRx <- function(x) {
    x |>
      mutate(
        DTTToRx = RxDateFixed - DTTDate,
        DTTToRxClockStopped = ifelse(is.na(ClockStopDaysPostDTT),
          DTTToRx,
          DTTToRx + ClockStopDaysPostDTT
        ),
        RxStopped = DTTToRxClockStopped < 0,
        DTTToRxNoStopped = ifelse(RxStopped, NA, DTTToRxClockStopped),
        MissingDTT = is.na(DTTDate) & !is.na(RxDate),
        ValidDTT   = !is.na(DTTDate) & !is.na(RxDate)
      ) |>
      summarise(
        NotStopped = list(DTTToRxNoStopped),
        Mean = mean(DTTToRxNoStopped, na.rm = TRUE),
        CountStopped = sum(RxStopped, na.rm = TRUE),
        CountNotStopped = sum(DTTToRxClockStopped >= 0, na.rm = TRUE),
        "≤31 days" = sum(DTTToRxNoStopped <= 31, na.rm = TRUE),
        "≤45 days" = sum(DTTToRxNoStopped > 31 & DTTToRxNoStopped <= 45, na.rm = TRUE),
        "≤60 days" = sum(DTTToRxNoStopped > 45 & DTTToRxNoStopped <= 60, na.rm = TRUE),
        ">60 days" = sum(DTTToRxNoStopped > 60, na.rm = TRUE),
        "Treated but missing DTT" = sum(MissingDTT, na.rm = TRUE),
        "Treated with DTT" = sum(ValidDTT, na.rm = TRUE),
        "Treated Total" = sum(MissingDTT, na.rm = TRUE) + sum(ValidDTT, na.rm = TRUE)
      ) |>
      mutate(
        measure = "DTTToRx",
      )
  }

  summariseRefToRx <- function(x) {
    x |>
      mutate(
        RefToRx = RxDateFixed - RefDate,
        ClockStopDays = replace_na(ClockStopDaysPreDTT, 0) + replace_na(ClockStopDaysPostDTT, 0),
        RefToRxClockStopped = RefToRx + ClockStopDays,
        RxStopped = RefToRxClockStopped < 0,
        RefToRxNoStopped = ifelse(RxStopped, NA, RefToRxClockStopped),
      ) |>
      summarise(
        NotStopped = list(RefToRxNoStopped),
        Mean = mean(RefToRxNoStopped, na.rm = TRUE),
        CountStopped = sum(RxStopped, na.rm = TRUE),
        CountNotStopped = sum(RefToRxClockStopped >= 0, na.rm = TRUE),
        "≤90 days" = sum(RefToRxNoStopped <= 90, na.rm = TRUE),
        ">90 days" = sum(RefToRxNoStopped > 90, na.rm = TRUE)
      ) |>
      mutate(
        measure = "RefToRx",
      )
  }

  add_period <- function(x, period) {
    mutate(x,
      Period = period,
      PeriodStart = range_start(period),
      PeriodEnd = range_end(period)
    )
  }

  status_fns <- c(
    RefToDTTByAblation = \(x, period) {
      x |>
        # RxDate is within the period
        filter(RxDate >= as.Date(range_start(period)) & RxDate < as.Date(range_end(period))) |>
        mutate(
          DTTDateFixed = DTTDate,
        ) |>
        summariseRefToDTT() |>
        mutate(group = "Ablation Date") |>
        add_period(period)
    },
    RefToDTTByDTT = \(x, period) {
      x |>
        filter(RefDate < as.Date(range_end(period)) &
          # & either DTTDate is within the period
          (
            (DTTDate >= as.Date(range_start(period)) & DTTDate < as.Date(range_end(period))) |
              # or DTTDate is missing and RxDate is within the period
              (is.na(DTTDate) &
                (is.na(RxDate) |
                  (RxDate >= as.Date(range_start(period)) & RxDate < as.Date(range_end(period))))
              )
          )) |>
        mutate(
          DTTDateFixed = DTTDate,
        ) |>
        summariseRefToDTT() |>
        mutate(group = "DTT Date") |>
        add_period(period)
    },
    RefToDTTWaitingOnly = \(x, period) {
      x |>
        filter(RefDate < as.Date(range_end(period)) &
          # & either DTTDate is after the period
          (
            DTTDate >= as.Date(range_end(period)) |
              # or DTTDate is missing and RxDate is after the period
              (is.na(DTTDate) &
                (is.na(RxDate) | RxDate > as.Date(range_end(period)))
              )
          )) |>
        mutate(
          DTTDateFixed = pmin(as.Date(range_end(period)), DTTDate, na.rm = T),
        ) |>
        summariseRefToDTT() |>
        mutate(group = "Waiting") |>
        add_period(period)
    },
    RefToDTTAll = \(x, period) {
      x |>
        # Only include patients referred before the end date
        filter(RefDate < as.Date(range_end(period)) &
          # & either DTTDate after the start of the period
          (
            DTTDate >= as.Date(range_start(period)) |
              # or DTTDate is missing and RxDate is missing or after the start of the period
              (is.na(DTTDate) &
                (is.na(RxDate) | RxDate >= as.Date(range_start(period)))
              )
          )) |>
        mutate(
          DTTDateFixed = pmin(DTTDate, as.Date(range_end(period)), RxDate, na.rm = T),
        ) |>
        summariseRefToDTT() |>
        mutate(group = "All") |>
        add_period(period)
    },
    DTTToRxByAblation = \(x, period) {
      x |>
        # RxDate is within the period
        filter(RxDate >= as.Date(range_start(period)) & RxDate < as.Date(range_end(period))) |>
        mutate(
          RxDateFixed = RxDate,
        ) |>
        summariseDTTToRx() |>
        mutate(group = "Ablation Date") |>
        add_period(period)
    },
    DTTToRxWaitingOnly = \(x, period) {
      x |>
        filter(
          # Only include patients who had a DTT date before the end of the period
          DTTDate < as.Date(range_end(period)) &
            (is.na(RxDate) | RxDate > as.Date(range_end(period)))
        ) |>
        mutate(
          RxDateFixed = pmin(RxDate, as.Date(range_end(period)), na.rm = T),
        ) |>
        summariseDTTToRx() |>
        mutate(group = "Waiting") |>
        add_period(period)
    },
    DTTToRxAll = \(x, period) {
      x |>
        filter(
          # Only include patients who had a DTT date before the end of the period
          DTTDate < as.Date(range_end(period)) &
            # and who were treated after the start of the period or haven't been treated yet
            (is.na(RxDate) |
              RxDate >= as.Date(range_start(period)))
        ) |>
        mutate(
          RxDateFixed = pmin(RxDate, as.Date(range_end(period)), na.rm = T),
        ) |>
        summariseDTTToRx() |>
        mutate(group = "All") |>
        add_period(period)
    },
    RefToRxByAblation = \(x, period) {
      x |>
        # RxDate is within the period
        filter(RxDate >= as.Date(range_start(period)) & RxDate < as.Date(range_end(period))) |>
        mutate(
          RxDateFixed = pmin(RxDate, as.Date(range_end(period)), na.rm = T),
        ) |>
        summariseRefToRx() |>
        mutate(group = "Ablation Date") |>
        add_period(period)
    },
    RefToRxWaitingOnly = \(x, period) {
      x |>
        filter(
          # Only include patients who had a referral date before the end of the period
          RefDate < as.Date(range_end(period)) &
            # and hasn't had treatment or is after the end of the period
            (is.na(RxDate) | RxDate >= as.Date(range_end(period)))
        ) |>
        mutate(
          RxDateFixed = pmin(RxDate, as.Date(range_end(period)), na.rm = T),
        ) |>
        summariseRefToRx() |>
        mutate(group = "Waiting") |>
        add_period(period)
    },
    RefToRxAll = \(x, period) {
      x |>
        filter(
          # Only include patients who had a referral date before the end of the period
          RefDate < as.Date(range_end(period)) &
            # and either haven't had treatment yet or a rxDate after the start of the period
            (is.na(RxDate) | RxDate >= as.Date(range_start(period)))
        ) |>
        mutate(
          RxDateFixed = pmin(RxDate, as.Date(range_end(period)), na.rm = T),
        ) |>
        summariseRefToRx() |>
        mutate(group = "All") |>
        add_period(period)
    }
  )

  doneData |>
    full_join(waitingData) |>
    (\(allData) {
      range_tibble$name |>
        purrr::map(function(period) {
          unname(status_fns) |>
            purrr::map(\(fn) fn(allData, period)) |>
            purrr::list_rbind()
        }) |>
        purrr::list_rbind()
    })()
}

refToDTTMeanPlot <- function(referralTimes, range_by = "Monthly", selectedGroup = "All") {
  refToDTT <- referralTimes |> filter(measure == "RefToDTT" & group == selectedGroup)
  maxY <- max(
    25,
    ceiling((max(refToDTT$Mean, na.rm = T) + 1) / 5) * 5,
    ceiling((max(refToDTT$`Treated but missing DTT`, na.rm = T) + 1) / 5) * 5
  )

  # Only the negative clock stops are excluded (bad data), those with postiive clock stops are included in the mean etc
  refToDTT |>
    mutate(BreachStatus = if_else(Mean > 10, if_else(Mean > 21, ">21 days", "≤21 days"), "≤10 days"), N = CountNotStopped) |>
    ggplot() +
    geom_line(aes(x = PeriodStart, y = Mean, color = "grey")) +
    geom_point(aes(x = PeriodStart, y = Mean, color = BreachStatus, N = N)) +
    geom_col(aes(x = PeriodStart, y = `Treated Total`, fill = `Treated with DTT`), alpha = 0.7, position = "dodge2") +
    geom_hline(yintercept = 10, linetype = "dashed", color = "orange") +
    geom_hline(yintercept = 21, linetype = "dashed", color = "red") +
    scale_color_manual(
      name = "Breach Status",
      values = c("≤10 days" = "grey", "≤21 days" = "orange", ">21 days" = "red")
    ) +
    guides(color = "none", fill = "none") +
    scale_x_date(date_breaks = "month", date_labels = "%b-%y") +
    scale_y_continuous(breaks = seq(0, maxY, by = 5), limits = c(0, maxY)) +
    labs(
      title =
        paste0(
          "Ref-DTT Mean",
          " (", selectedGroup, ", By ", stringr::str_to_title(range_by), ")",
          "\n(Generated ",
          format(Sys.time(), "%a %b %d %Y %X"), ")"
        ),
      x = "",
      y = "Number of Days"
    ) +
    theme(plot.title = element_text(size = 10)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.spacing.y = unit(0.1, "cm"))
}

refToDTTCountPlot <- function(referralTimes, range_by = "Monthly", selectedGroup = "All") {
  referralTimes |>
    filter(`measure` == "RefToDTT" & `group` == selectedGroup) |>
    select(
      PeriodStart, `≤10 days`, `≤21 days`, `>21 days`
    ) |>
    pivot_longer(cols = -PeriodStart, names_to = "Breach Status", values_to = "Count") |>
    mutate(
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
          "Ref-DTT Count",
          " (", selectedGroup, ", By ", stringr::str_to_title(range_by), ")",
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

refToDTTBoxPlot <- function(referralTimes, range_by = "Monthly", selectedGroup = "All") {
  referralTimes |>
    filter(`measure` == "RefToDTT" & `group` == selectedGroup) |>
    select(
      PeriodStart, `NotStopped`,
    ) |>
    unnest(cols = "NotStopped") |>
    ggplot() +
    geom_hline(yintercept = 10, linetype = "dashed", color = "orange") +
    geom_hline(yintercept = 21, linetype = "dashed", color = "red") +
    geom_boxplot(aes(x = PeriodStart, y = NotStopped, group = PeriodStart)) +
    scale_x_date(date_breaks = "month", date_labels = "%b-%y") +
    labs(
      title =
        paste0(
          "Ref-DTT Times",
          " (", selectedGroup, ", By ", stringr::str_to_title(range_by), ")",
          "\n(Generated ",
          format(Sys.time(), "%a %b %d %Y %X"), ")"
        ),
      x = "",
      y = "Number of Days"
    ) +
    theme(plot.title = element_text(size = 10)) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.spacing.y = unit(0.1, "cm"),
      legend.position = "bottom"
    )
}

dttToRxMeanPlot <- function(referralTimes, range_by = "Monthly", selectedGroup = "All") {
  dttToRx <- referralTimes |> filter(measure == "DTTToRx" & group == selectedGroup)

  maxY <- max(
    65,
    ceiling((max(dttToRx$Mean, na.rm = T) + 1) / 5) * 5,
    ceiling((max(dttToRx$`Treated but missing DTT`, na.rm = T) + 1) / 5) * 5
  )

  dttToRx |>
    mutate(BreachStatus = case_when(
      Mean > 60 ~ ">60 days",
      Mean > 45 ~ "≤60 days",
      Mean > 31 ~ "≤45 days",
      TRUE ~ "≤31 days"
    ), N = CountNotStopped) |>
    ggplot() +
    geom_line(aes(x = PeriodStart, y = Mean, color = "grey")) +
    geom_point(aes(x = PeriodStart, y = Mean, color = BreachStatus, N = N)) +
    geom_col(aes(x = PeriodStart, y = `Treated Total`, fill = `Treated with DTT`), alpha = 0.7, position = "dodge2") +
    geom_hline(yintercept = 31, linetype = "dashed", color = "orange") +
    geom_hline(yintercept = 45, linetype = "dashed", color = "red") +
    geom_hline(yintercept = 60, linetype = "dashed", color = "darkred") +
    scale_color_manual(
      name = "Breach Status",
      values = c("≤31 days" = "grey", "≤45 days" = "orange", "≤60 days" = "red", ">60 days" = "darkred")
    ) +
    guides(color = "none", fill = "none") +
    scale_x_date(date_breaks = "month", date_labels = "%b-%y") +
    scale_y_continuous(breaks = seq(0, maxY, by = 5), limits = c(0, maxY)) +
    labs(
      title =
        paste0(
          "DTT-Rx Mean",
          " (", selectedGroup, ", By ", stringr::str_to_title(range_by), ")",
          "\n(Generated ",
          format(Sys.time(), "%a %b %d %Y %X"), ")"
        ),
      x = "",
      y = "Number of Days"
    ) +
    theme(plot.title = element_text(size = 10)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.spacing.y = unit(0.1, "cm"))
}

dttToRxCountPlot <- function(referralTimes, range_by = "Monthly", selectedGroup = "All") {
  referralTimes |>
    filter(`measure` == "DTTToRx" & `group` == selectedGroup) |>
    select(
      PeriodStart, `≤31 days`,
      `≤45 days`, `≤60 days`, `>60 days`
    ) |>
    pivot_longer(cols = -PeriodStart, names_to = "Breach Status", values_to = "Count") |>
    mutate(
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
          "DTT-Rx Count",
          " (", selectedGroup, ", By ", stringr::str_to_title(range_by), ")",
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

dttToRxBoxPlot <- function(referralTimes, range_by = "Monthly", selectedGroup = "All") {
  referralTimes |>
    filter(`measure` == "DTTToRx" & `group` == selectedGroup) |>
    select(
      PeriodStart, `NotStopped`,
    ) |>
    unnest(cols = "NotStopped") |>
    ggplot() +
    geom_hline(yintercept = 31, linetype = "dashed", color = "orange") +
    geom_hline(yintercept = 45, linetype = "dashed", color = "red") +
    geom_hline(yintercept = 60, linetype = "dashed", color = "darkred") +
    geom_boxplot(aes(x = PeriodStart, y = NotStopped, group = PeriodStart)) +
    scale_x_date(date_breaks = "month", date_labels = "%b-%y") +
    labs(
      title =
        paste0(
          "DTT-Rx Times",
          " (", selectedGroup, ", By ", stringr::str_to_title(range_by), ")",
          "\n(Generated ",
          format(Sys.time(), "%a %b %d %Y %X"), ")"
        ),
      x = "",
      y = "Number of Days"
    ) +
    theme(plot.title = element_text(size = 10)) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.spacing.y = unit(0.1, "cm"),
      legend.position = "bottom"
    )
}

refToRxMeanPlot <- function(referralTimes, range_by = "Monthly", selectedGroup = "All") {
  refToRx <- referralTimes |> filter(measure == "RefToRx" & group == selectedGroup)
  maxY <- max(90, ceiling((max(refToRx$Mean, na.rm = T) + 1) / 5) * 5)

  refToRx |>
    mutate(BreachStatus = if_else(Mean > 90, ">90 days", "<90 days"),
           N = CountNotStopped) |>
    ggplot() +
    geom_line(aes(x = PeriodStart, y = Mean, color = "grey")) +
    geom_point(aes(x = PeriodStart, y = Mean, color = BreachStatus, N = N)) +
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
          "Ref-Rx Mean",
          " (", selectedGroup, ", By ", stringr::str_to_title(range_by), ")",
          "\n(Generated ",
          format(Sys.time(), "%a %b %d %Y %X"), ")"
        ),
      x = "",
      y = "Number of Days"
    ) +
    theme(plot.title = element_text(size = 10)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.spacing.y = unit(0.1, "cm"))
}

refToRxCountPlot <- function(referralTimes, range_by = "Monthly", selectedGroup = "All") {
  referralTimes |>
    filter(`measure` == "RefToRx" & `group` == selectedGroup) |>
    select(PeriodStart, `≤90 days`, `>90 days`) |>
    pivot_longer(cols = -PeriodStart, names_to = "Breach Status", values_to = "Count") |>
    mutate(
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
          "Ref-Rx Count",
          " (", selectedGroup, ", By ", stringr::str_to_title(range_by), ")",
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

refToRxBoxPlot <- function(referralTimes, range_by = "Monthly", selectedGroup = "All") {
  referralTimes |>
    filter(`measure` == "RefToDTT" & `group` == selectedGroup) |>
    select(
      PeriodStart, `NotStopped`,
    ) |>
    unnest(cols = "NotStopped") |>
    ggplot() +
    geom_hline(yintercept = 90, linetype = "dashed", color = "red") +
    geom_boxplot(aes(x = PeriodStart, y = NotStopped, group = PeriodStart)) +
    scale_x_date(date_breaks = "month", date_labels = "%b-%y") +
    labs(
      title =
        paste0(
          "Ref-Rx Times",
          " (", selectedGroup, ", By ", stringr::str_to_title(range_by), ")",
          "\n(Generated ",
          format(Sys.time(), "%a %b %d %Y %X"), ")"
        ),
      x = "",
      y = "Number of Days"
    ) +
    theme(plot.title = element_text(size = 10)) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.spacing.y = unit(0.1, "cm"),
      legend.position = "bottom"
    )
}

doWaitPlot <- function(measure, type, referralTimes, range_by, selectedGroup) {
  if (selectedGroup == "Performed") {
    if (measure == "RefToDTT") {
      selectedGroup <- "DTT Date"
    } else {
      selectedGroup <- "Ablation Date"
    }
  }

  switch(paste(measure, type, sep = ""),
    "RefToDTTmean" = refToDTTMeanPlot(referralTimes, range_by, selectedGroup),
    "RefToDTTcounts" = refToDTTCountPlot(referralTimes, range_by, selectedGroup),
    "RefToDTTboxplot" = refToDTTBoxPlot(referralTimes, range_by, selectedGroup),
    "DTTToRxmean" = dttToRxMeanPlot(referralTimes, range_by, selectedGroup),
    "DTTToRxcounts" = dttToRxCountPlot(referralTimes, range_by, selectedGroup),
    "DTTToRxboxplot" = dttToRxBoxPlot(referralTimes, range_by, selectedGroup),
    "RefToRxmean" = refToRxMeanPlot(referralTimes, range_by, selectedGroup),
    "RefToRxcounts" = refToRxCountPlot(referralTimes, range_by, selectedGroup),
    "RefToRxboxplot" = refToRxBoxPlot(referralTimes, range_by, selectedGroup)
  )
}

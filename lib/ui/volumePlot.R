volumePlotTab <- function() {
  list(
    fluidRow(
      tabPanel(
        "VolumePlots",
        column(
          width = 3,
          dateInput(
            "volumePlotStart",
            "Start Date:",
            format = "dd/mm/yyyy",
            value = Sys.Date() - 365
          ),
          dateInput(
            "volumePlotEnd",
            "End Date:",
            format = "dd/mm/yyyy",
            value = Sys.Date()
          ),
          actionButton(inputId="volumePlotRefresh", label = "Refresh Plot")
        ),
        column(
          width = 3,
          checkboxGroupInput(
            "volumePlotOrganCheckbox",
            "Organs to Plot",
            choices = organFactors,
            selected = organFactors
          ),
        ),
        column(
          width = 3,
          checkboxGroupInput(
            "volumePlotModalityCheckbox",
            "Modalities to Plot",
            choices = modalityFactors,
            selected = modalityFactors
          ),
        ),
        column(
          width = 3,
          radioButtons(
            "volumePlotDurationRadio",
            "Duration of Plot",
            c(
              "Weekly" = "week",
              "Monthly" = "month",
              "Yearly" = "year"
            )
          )
        ),
        column(
          width = 3,
        ),
        column(
          width = 6,
          tariffComponent()
        )
      )
    ),
    fluidRow(box(
      width = 12,
      plotlyOutput("plotVolume")
    ))
  )
}

volumePlotServer <- function(input, output, session, tariff, plots) {
  # Note plotly vs. plot gives you the tool tip text
  output$plotVolume <- renderPlotly({
    filteredRxDoneData <- rxDoneData |>
      filter(Organs %in% input$volumePlotOrganCheckbox) |>
      filter(Modality %in% input$volumePlotModalityCheckbox) |>
      filter(RxDate >= input$volumePlotStart) |>
      filter(RxDate <= input$volumePlotEnd)

    # We need to call this as if the duration radiobutton changes, it otherwise doesn't trigger a replot
    p <- makeTreatmentVolumePlot(filteredRxDoneData, input$volumePlotDurationRadio)
    p <- p %+% subset(filteredRxDoneData)

    # Work out the tariff
    tariff$theTotalTariff <<- calculateTotalTariff(filteredRxDoneData)

    # We need to round up to get the bin to include the full month otherwise it looses treatmnet data
    # p <- p + scale_x_date(date_breaks = "1 month", date_labels = "%b %y",
    #                      limits = as.Date(c(input$volumePlotDate1, ceiling_date(input$volumePlotDate2,"month"))))

    if (input$volumePlotDurationRadio == "week") {
      p <- p + scale_x_date(date_breaks = "1 week", date_labels = "%e %b %y")
    } else if (input$volumePlotDurationRadio == "month") {
      p <- p + scale_x_date(date_breaks = "1 month", date_labels = "%b %y")
    } else if (input$volumePlotDurationRadio == "year") {
      p <- p + scale_x_date(date_breaks = "1 year", date_labels = "%Y")
    }

    plots$activePlot <- p

    plots$activePlot
  })

  observeEvent(input$volumePlotRefresh, {
    plots$activePlot <- ggplot()
  })

  # ? Not sure what this for ?
  finalVolumePlotInput <- reactive({
    volumePlot
  })
}

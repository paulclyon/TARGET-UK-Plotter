waitTimesDashboardPlotTab <- function() {
  list(
    fluidRow(
      tabPanel(
        "waitTimesDashboard",
        column(
          width = 3,
          dateInput(
            "waitTimesDashboardPlotStart",
            "Start Date:",
            format = "dd/mm/yyyy",
            value = Sys.Date() - 365
          ),
          dateInput(
            "waitTimesDashboardPlotEnd",
            "End Date:",
            format = "dd/mm/yyyy",
            value = Sys.Date()
          ),
        ),
        column(
          width = 3,
          checkboxGroupInput(
            "waitTimesDashboardPlotOrganCheckbox",
            "Organs to Plot",
            choices = organFactors,
            selected = organFactors
          ),
        ),
        column(
          width = 3,
          radioButtons(
            "waitTimesDashboardPlotDurationRadio",
            "Duration of Plot",
            c(
              "Weekly" = "week",
              "2 Weekly" = "2 weeks",
              "Monthly" = "month",
              "Quarterly" = "quarter",
              "Yearly" = "year"
            ),
            selected = "month"
          )
        ),
        column(
          width = 3,
          radioButtons(
            "waitTimesDashboardPlotTypeRadio",
            "Type of Plot",
            c(
              "Mean" = "mean",
              "Counts" = "counts",
              "Boxplot" = "boxplot"
            ),
            selected = "mean"
          ),
          actionButton(inputId = "waitTimesDashboardPlotRefresh", label = "Refresh Plot")
        )
      )
    ),
    fluidRow(
      box(
        width = 4,
        plotlyOutput("plotWaitTimesDashboardRefToDTT")
      ),
      box(
        width = 4,
        plotlyOutput("plotWaitTimesDashboardDTTToRx")
      ),
      box(
        width = 4,
        plotlyOutput("plotWaitTimesDashboardRefToRx")
      ),
    ),
    detectHeightJS("waitTimesDashboard", "plotWaitTimesDashboardRefToDTT"),
    detectHeightJS("waitTimesDashboard", "plotWaitTimesDashboardDTTToRx"),
    detectHeightJS("waitTimesDashboard", "plotWaitTimesDashboardRefToRx")
  )
}

waitTimesDashboardPlotServer <- function(input, output, session, api, plots) {
  observe({
    updateCheckboxGroupInput(session, "waitTimesDashboardPlotOrganCheckbox", "Organs to Plot",
      choices = api$organFactors,
      selected = api$organFactors
    )
  })

  waitTimesTimes <- reactive(processWaitTimesPerPeriod(
    rxDoneData %>% filter(Organs %in% input$waitTimesDashboardPlotOrganCheckbox),
    rxWaitData %>% filter(Organs %in% input$waitTimesDashboardPlotOrganCheckbox),
    input$waitTimesDashboardPlotStart,
    input$waitTimesDashboardPlotEnd,
    input$waitTimesDashboardPlotDurationRadio
  ))


  output$plotWaitTimesDashboardRefToDTT <- renderPlotly({
    p <- ggplot()
    if (input$waitTimesDashboardPlotTypeRadio == "mean") {
      p <- refToDTTMeanPlot(waitTimesTimes(), input$waitTimesDashboardPlotDurationRadio)
    } else if (input$waitTimesDashboardPlotTypeRadio == "boxplot") {
      p <- refToDTTBoxPlot(waitTimesTimes(), input$waitTimesDashboardPlotDurationRadio)
    } else {
      p <- refToDTTCountPlot(waitTimesTimes(), input$waitTimesDashboardPlotDurationRadio)
    }

    height <- detectedHeight(input, "plotWaitTimesDashboardRefToDTT")

    p <- ggplotly(p, height = height)
    plots$activePlot <- p
    p
  })

  output$plotWaitTimesDashboardRefToRx <- renderPlotly({
    p <- ggplot()
    if (input$waitTimesDashboardPlotTypeRadio == "mean") {
      p <- refToRxMeanPlot(waitTimesTimes(), input$waitTimesDashboardPlotDurationRadio)
    } else if (input$waitTimesDashboardPlotTypeRadio == "boxplot") {
      p <- refToRxBoxPlot(waitTimesTimes(), input$waitTimesDashboardPlotDurationRadio)
    } else {
      p <- refToRxCountPlot(waitTimesTimes(), input$waitTimesDashboardPlotDurationRadio)
    }

    height <- detectedHeight(input, "plotWaitTimesDashboardRefToRx")

    p <- ggplotly(p, height = height)
    plots$activePlot <- p
    p
  })

  output$plotWaitTimesDashboardDTTToRx <- renderPlotly({
    p <- ggplot()
    if (input$waitTimesDashboardPlotTypeRadio == "mean") {
      p <- dttToRxMeanPlot(waitTimesTimes(), input$waitTimesDashboardPlotDurationRadio)
    } else if (input$waitTimesDashboardPlotTypeRadio == "boxplot") {
      p <- dttToRxBoxPlot(waitTimesTimes(), input$waitTimesDashboardPlotDurationRadio)
    } else {
      p <- dttToRxCountPlot(waitTimesTimes(), input$waitTimesDashboardPlotDurationRadio)
    }

    height <- detectedHeight(input, "plotWaitTimesDashboardDTTToRx")

    p <- ggplotly(p, height = height)
    plots$activePlot <- p
    p
  })


  observeEvent(input$waitTimesDashboardPlotRefresh, {
    plots$activePlot <- ggplot()
  })
}

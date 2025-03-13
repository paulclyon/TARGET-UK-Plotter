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
            "waitTimesDashboardPlotGroupRadio",
            "Type of Plot",
            c(
              "By Ablation Date" = "By Ablation Date",
              "Performed" = "Performed",
              "Waiting" = "Waiting",
              "All" = "All"
            ),
            selected = "By Ablation Date"
          ),
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
    p <- doWaitPlot(
      "RefToDTT",
      input$waitTimesDashboardPlotTypeRadio,
      waitTimesTimes(),
      input$waitTimesDashboardPlotDurationRadio,
      input$waitTimesDashboardPlotGroupRadio
    )

    height <- detectedHeight(input, "plotWaitTimesDashboardRefToDTT")

    p <- ggplotly(p, height = height)
    plots$activePlot <- p
    p
  })

  output$plotWaitTimesDashboardRefToRx <- renderPlotly({
    p <- doWaitPlot(
      "RefToRx",
      input$waitTimesDashboardPlotTypeRadio,
      waitTimesTimes(),
      input$waitTimesDashboardPlotDurationRadio,
      input$waitTimesDashboardPlotGroupRadio
    )

    height <- detectedHeight(input, "plotWaitTimesDashboardRefToRx")

    p <- ggplotly(p, height = height)
    plots$activePlot <- p
    p
  })

  output$plotWaitTimesDashboardDTTToRx <- renderPlotly({
    p <- doWaitPlot(
      "DTTToRx",
      input$waitTimesDashboardPlotTypeRadio,
      waitTimesTimes(),
      input$waitTimesDashboardPlotDurationRadio,
      input$waitTimesDashboardPlotGroupRadio
    )

    height <- detectedHeight(input, "plotWaitTimesDashboardDTTToRx")

    p <- ggplotly(p, height = height)
    plots$activePlot <- p
    p
  })


  observeEvent(input$waitTimesDashboardPlotRefresh, {
    plots$activePlot <- ggplot()
  })
}

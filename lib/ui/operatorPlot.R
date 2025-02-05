operatorPlotTab <- function() {
  list(
    fluidRow(
      tabPanel(
        "OperatorPlots",
        column(
          width = 3,
          dateInput(
            "operatorPlotDate1",
            "Start Date:",
            format = "dd/mm/yyyy",
            value = Sys.Date() - 365
          ),
          dateInput(
            "operatorPlotDate2",
            "End Date:",
            format = "dd/mm/yyyy",
            value = Sys.Date()
          )
        ),
        column(
          width = 6,
          selectInput(
            "operatorPlotDropdown",
            "Operators to Plot",
            choices = operator1Factors
          ),
          selectInput(
            "anaesthetistPlotDropdown",
            "Anaesthetists to Plot",
            choices = anaesthetist1Factors
          ),
        ),
        column(
          width = 3,
          actionButton(inputId = "refreshOperatorPlot", label = "Refresh Plot")
        )
      )
    ),
    fluidRow(box(
      width = 12,
      plotlyOutput("plotOperators")
    )),
    detectHeightJS("operatorplots", "plotOperators")
  )
}

operatorPlotServer <- function(input, output, session, plots) {
  finalOperatorPlotInput <- reactive({
    makeOperatorPlot()
    operatorPlot
  })

  filteredPlot <- reactive({
    filteredRxDoneData <- rxDoneData
    if (input$operatorPlotDropdown != "ALL") {
      filteredRxDoneData <- filteredRxDoneData %>% filter(Operator1 %in% input$operatorPlotDropdown)
    }
    if (input$anaesthetistPlotDropdown != "ALL") {
      filteredRxDoneData <- filteredRxDoneData %>% filter(Anaesthetist1 %in% input$anaesthetistPlotDropdown)
    }
    p <- finalOperatorPlotInput()
    p <- p %+% subset(filteredRxDoneData)

    # We need to round up to get the bin to include the full month otherwise it loses treatment data
    p <- p + scale_x_date(
      date_breaks = "1 month", date_labels = "%b %y",
      limits = as.Date(c(input$operatorPlotDate1, ceiling_date(input$operatorPlotDate2, "month")))
    )
    p
  })

  # Note plotly vs. plot gives you the tool tip text
  output$plotOperators <- renderPlotly({
    p <- filteredPlot()
    height <- detectedHeight(input, "plotOperators")

    plots$activePlot <- ggplotly(p, height = height)
    plots$activePlot
  })

  observeEvent(input$refreshOperatorPlot, {
    plots$activePlot <- ggplot()
  })
}

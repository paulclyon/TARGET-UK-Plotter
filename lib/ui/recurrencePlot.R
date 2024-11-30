recurrencePlotTab <- function() {
  fluidRow(
    box(
      width = 12,
      radioButtons(
        "recurrencePlotRadio", "Recurrence Analysis",
        c("By Sex" = "recurrencePlotSex", "By Organ" = "recurrencePlotOrgan")
      ),
      actionButton(inputId = "refreshRecurrencePlot", label = "Refresh Plot")
    ),
    tabPanel(
      "RecurrencePlot",
      plotOutput("plotRecurrenceCurve")
    ),
    detectHeightJS("recurrenceplot", "plotRecurrenceCurve")
  )
}

recurrencePlotServer <- function(input, output, session, plots) {
  observeEvent(input$refreshRecurrencePlot, {
    plots$activePlot <- ggplot()
  })

  finalRecurrencePlotInput <- reactive({
    recurrencePlotOrgan
  })

  height <- reactive(detectedHeight(input, "pieRxPathway"))

  output$plotRecurrenceCurve <- renderPlot({
    # See this for dynmaic survival curves in shiny
    #    https://stackoverflow.com/questions/61273513/issue-with-r-shiny-app-interactive-survival-plots
    p <- finalRecurrencePlotInput()
    plots$activePlot <- p
    plots$activePlot
  }, height = height)
}

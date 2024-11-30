survivalPlotTab <- function() {
  fluidRow(
    box(
      width = 12,
      radioButtons("survivalPlotRadio", "Survival Analysis",
        c("By Sex" = "survivalPlotSex", "By Organ" = "survivalPlotOrgan")
      ),
      actionButton(inputId = "refreshSurvivalPlot", label = "Refresh Plot"),
    ),
    tabPanel("SurvivalPlot", plotOutput("plotSurvivalCurve")),
    detectHeightJS("survivalplot", "plotSurvivalCurve")
  )
}

survivalPlotServer <- function(input, output, session, plots) {
  finalSurvivalPlotInput <- reactive({
    switch(input$survivalPlotRadio,
      "survivalPlotSex" = survivalPlotSex,
      "survivalPlotOrgan" = survivalPlotOrgan
    )
  })

  height <- reactive(detectedHeight(input, "plotSurvivalCurve", border = 100))

  output$plotSurvivalCurve <- renderPlot({
    # See this for dynmaic survival curves in shiny
    #    https://stackoverflow.com/questions/61273513/issue-with-r-shiny-app-interactive-survival-plots
    p <- finalSurvivalPlotInput()
    plots$activePlot <- p
    plots$activePlot
  }, height = height)

  observeEvent(input$refreshSurvivalPlot, {
    plots$activePlot <- ggplot()
  })
}

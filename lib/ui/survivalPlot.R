survivalPlotTab <- function() {
  fluidRow(
    tabPanel(
      "SurvivalPlot",
      column(
        width = 3,
        dateInput(
          "survivalStartDate",
          "Start Date:",
          format = "dd/mm/yyyy",
          value = Sys.Date() - 365*20
        ),
        dateInput(
          "survivalEndDate",
          "End Date:",
          format = "dd/mm/yyyy",
          value = Sys.Date()
        )
      ),
      column(
        width = 3,
        checkboxGroupInput(
          "survivalSelectedOrgans",
          "Organs to Chart",
          choices = organFactors,
          selected = organFactors
        )
      ),
      column(
        width = 3,
        checkboxGroupInput(
          "survivalSelectedGenders",
          "Genders to Chart",
          choices = genderFactors,
          selected = genderFactors
        )
      ),
      column(
        width = 3,
        radioButtons(
          "survivalLRFSRadio",
          "Survival Plot Type",
          c("Local Recurrence-Free" = 1, "Survival Only" = 0)
        ),
        actionButton(inputId = "refreshSurvivalPlot", label = "Refresh Plot")
      )
    ),
    
    fluidRow(box(
      width = 12,
      plotOutput("plotSurvivalCurve")
    )),
    detectHeightJS("survivalplot", "plotSurvivalCurve")
  )
}

survivalPlotServer <- function(input, output, session, api, plots)
{
  observeEvent(input$refreshSurvivalPlot, {
    logger("here refresh!")
    plots$activePlot <- ggplot()
  })
  
  observe({
    updateCheckboxGroupInput(session, "survivalSelectedOrgans", "Organs to Chart",
                             choices = api$organFactors,
                             selected = api$organFactors
    )
  })
  
  observe({
    updateCheckboxGroupInput(session, "survivalSelectedGenders", "Genders to Chart",
                             choices = api$genderFactors,
                             selected = api$genderFactors
    )
  })
  
  finalSurvivalPlotInput <- reactive({
    makeSurvivalPlot(
      input$survivalStartDate,
      input$survivalEndDate,
      input$survivalSelectedOrgans,
      input$survivalSelectedGenders,
      input$survivalLRFSRadio
    )
  })

  height <- reactive(detectedHeight(input, "plotSurvivalCurve"))

  # See this for dynamic generation of filtered survival curves in shiny
  #    https://stackoverflow.com/questions/61273513/issue-with-r-shiny-app-interactive-survival-plots
  output$plotSurvivalCurve <- renderPlot({
    p <- finalSurvivalPlotInput()
    plots$activePlot <- p
    plots$activePlot
  }, height = height)

}

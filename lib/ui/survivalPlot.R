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
        ),
        numericInput(
          "maxYearsFollowup",
          "Max Years Follow-up:",
          value = 5, 
          min = 1, 
          max = 20
        )
      ),
      column(
        width = 4,
        selectInput("survivalSelectedOrgans","Organ to Chart", choices = organFactors, selected = organFactors[1]),
        checkboxGroupInput(
          "survivalSelectedGenders",
          "Genders to Chart",
          choices = genderFactors,
          selected = genderFactors
        )
      ),
      column(
        width = 4,
        radioButtons(
          "survivalLRFSRadio",
          "Survival Plot Type",
          c("Overall Survival (OS)" = 0, "Cancer Specific Survival (CSS)" = 1, "Local Recurrence-Free OS" = 2, "Local Recurrence-Free CSS" = 3)
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
    plots$activePlot <- ggplot()
  })
  
  observe({
    updateSelectInput(session, "survivalSelectedOrgans", "Organ to Chart",
                      choices = api$organFactors,
                      selected = api$organFactors[1]
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
      input$maxYearsFollowup,
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

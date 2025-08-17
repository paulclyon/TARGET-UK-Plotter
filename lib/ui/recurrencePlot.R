recurrencePlotTab <- function() {
  fluidRow(
    tabPanel(
      "RecurrencePlot",
      column(
        width = 3,
        dateInput(
          "recurrenceStartDate",
          "Start Date:",
          format = "dd/mm/yyyy",
          value = Sys.Date() - 365*10
        ),
        dateInput(
          "recurrenceEndDate",
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
        width = 3,
        checkboxGroupInput(
          "recurrenceSelectedOrgans",
          "Organs to Plot",
          choices = organFactors,
          selected = organFactors
        )
      ),
      column(
        width = 3,
        checkboxGroupInput(
          "sexRadio", "Gender",
          choices = c("Male","Female","Unknown"),
          selected = c("Male","Female","Unknown")
        ),
      ),
      column(
        width = 3,
        actionButton(inputId = "refreshRecurrencePlot", label = "Refresh Plot")
      )
    ),
    
    fluidRow(box(
      width = 12,
      plotOutput("plotRecurrenceCurve")
    )),
    detectHeightJS("recurrenceplot", "plotRecurrenceCurve")
  )
}

recurrencePlotServer <- function(input, output, session, api, plots)
{
  observeEvent(input$refreshRecurrencePlot, {
    plots$activePlot <- ggplot()
  })
  
  observe({
    updateCheckboxGroupInput(session, "recurrenceSelectedOrgans", "Organs to Chart",
                             choices = api$organFactors,
                             selected = api$organFactors
    )
  })

  finalRecurrencePlotInput <- reactive({
    makeRecurrencePlot(
      input$recurrenceStartDate,
      input$recurrenceEndDate,
      input$maxYearsFollowup,
      input$recurrenceSelectedOrgans)
  })

  height <- reactive(detectedHeight(input, "plotRecurrenceCurve"))

  output$plotRecurrenceCurve <- renderPlot({
    # See this for dynmaic survival curves in shiny
    #    https://stackoverflow.com/questions/61273513/issue-with-r-shiny-app-interactive-survival-plots
    p <- finalRecurrencePlotInput()
    plots$activePlot <- p
    plots$activePlot
  }, height = height)
  
  

}

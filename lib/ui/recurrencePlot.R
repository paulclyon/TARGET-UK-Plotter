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
        width = 4,
        selectInput("recurrenceSelectedOrgans", "Organ to Chart", choices = organFactors),
        checkboxGroupInput(
          "recurrenceSelectedGenders", "Genders to Chart",
          choices = genderFactors,
          selected = genderFactors
        )
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
    updateSelectInput(session, "recurrenceSelectedOrgans", "Organ to Chart",
                      choices = api$organFactors,
                      selected = api$organFactors[1]
    )
  })
  
  observe({
    updateCheckboxGroupInput(session, "recurrenceSelectedGenders", "Genders to Chart",
                             choices = api$genderFactors,
                             selected = api$genderFactors
    )
  })

  finalRecurrencePlotInput <- reactive({
    makeRecurrencePlot(
      input$recurrenceStartDate,
      input$recurrenceEndDate,
      input$maxYearsFollowup,
      input$recurrenceSelectedOrgans,
      input$recurrenceSelectedGenders
    )
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

recurrencePlotTab <- function() {
  fluidRow(
    tabPanel(
      "RecurrencePlot",
      column(
        width = 3,
        dateInput(
          "recurrenceStartDate",
          "Start Date",
          format = "dd/mm/yyyy",
          value = Sys.Date() - 365*10
        ),
        dateInput(
          "recurrenceEndDate",
          "End Date",
          format = "dd/mm/yyyy",
          value = Sys.Date()
        ),
        numericInput(
          "maxYearsFollowup",
          "Max Years Follow-up",
          value = 5, 
          min = 1, 
          max = 20
        ),
        checkboxInput("recurrenceAllow2Rx","Allow two Rx before LR", value=TRUE)
      ),
      column(
        width = 3,
        selectInput("recurrenceSelectedOrgans", "Target Organ", choices = organFactors),
        selectInput("recurrenceSelectedDiagnosisType", "Diagnosis Type", choices = diagnosis_type_Factors),
        checkboxGroupInput(
          "recurrenceSelectedGenders", "Genders",
          choices = genderFactors,
          selected = genderFactors
        )
      ),
      column(
        width = 6,
        checkboxGroupInput(
          "recurrenceSelectedSubtypes", "Subtypes",
          choices = diagnosisSubtypeFactors,
          selected = diagnosisSubtypeFactors
        )
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
  subtypeChoices <- reactive({
    req(input$recurrenceSelectedDiagnosisType)
    
    switch(
      input$recurrenceSelectedDiagnosisType,
      "All"       = c("All"),
      "Primary"   = api$diagnosis_1o_Factors,
      "Secondary" = api$diagnosis_2o_Factors,
      "Benign"    = api$diagnosis_bn_Factors,
      api$diagnosisSubtypeFactors
    )
  })
  
  observeEvent(input$recurrenceSelectedDiagnosisType, {
    choices <- subtypeChoices()
    updateCheckboxGroupInput(
      session,
      "recurrenceSelectedSubtypes",
      choices = choices,
      selected = choices
    )
  }, ignoreInit = FALSE)
  
  observe({
    updateSelectInput(
      session, "recurrenceSelectedOrgans",
      choices = api$organFactors,
      selected = api$organFactors[1]
    )
  })
  
  observe({
    updateSelectInput(
      session, "recurrenceSelectedDiagnosisType",
      choices = api$diagnosis_type_Factors,
      selected = api$diagnosis_type_Factors[1]
    )
  })
  
  observe({
    updateCheckboxGroupInput(
      session, "recurrenceSelectedGenders",
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
      input$recurrenceSelectedDiagnosisType,
      input$recurrenceSelectedSubtypes,
      input$recurrenceSelectedGenders,
      input$recurrenceAllow2Rx
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

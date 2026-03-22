survivalPlotTab <- function() {
  fluidRow(
    tabPanel(
      "SurvivalPlot",
      column(
        width = 3,
        dateInput(
          "survivalStartDate",
          "Start Date",
          format = "dd/mm/yyyy",
          value = Sys.Date() - 365*20
        ),
        dateInput(
          "survivalEndDate",
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
        )
      ),
      column(
        width = 2,
        selectInput("survivalSelectedOrgans","Target Organ", choices = organFactors, selected = organFactors[1]),
        selectInput("survivalSelectedDiagnosisType", "Diagnosis Type", choices = diagnosis_type_Factors),
        checkboxGroupInput(
          "survivalSelectedGenders",
          "Genders",
          choices = genderFactors,
          selected = genderFactors
        )
      ),
      column(
        width = 3,
        radioButtons(
          "survivalLRFSRadio",
          "Survival Plot Type",
          c("Overall Survival (OS)" = 0, "Cancer Specific Survival (CSS)" = 1, "Local Recurrence-Free OS" = 2, "Local Recurrence-Free CSS" = 3)
        )
      ),
      column(
        width = 4,
        checkboxGroupInput(
          "survivalSelectedSubtypes", "Subtypes",
          choices = diagnosisSubtypeFactors,
          selected = diagnosisSubtypeFactors
        )
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
  subtypeChoices <- reactive({
    req(input$survivalSelectedDiagnosisType)
    
    switch(
      input$survivalSelectedDiagnosisType,
      "All"       = c("All"),
      "Primary"   = api$diagnosis_1o_Factors,
      "Secondary" = api$diagnosis_2o_Factors,
      "Benign"    = api$diagnosis_bn_Factors,
      api$diagnosisSubtypeFactors
    )
  })
  
  observeEvent(input$survivalSelectedDiagnosisType, {
    choices <- subtypeChoices()
    updateCheckboxGroupInput(
      session,
      "survivalSelectedSubtypes",
      choices = choices,
      selected = choices
    )
  }, ignoreInit = FALSE)
  
  observe({
    updateSelectInput(session, "survivalSelectedOrgans", "Target Organ",
                      choices = api$organFactors,
                      selected = api$organFactors[1]
    )
  })
  
  observe({
    updateSelectInput(
      session, "survivalSelectedDiagnosisType",
      choices = api$diagnosis_type_Factors,
      selected = api$diagnosis_type_Factors[1]
    )
  })
  
  observe({
    updateCheckboxGroupInput(session, "survivalSelectedGenders", "Genders",
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
      input$survivalSelectedDiagnosisType,
      input$survivalSelectedSubtypes,
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

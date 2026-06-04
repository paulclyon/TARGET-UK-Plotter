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
        sliderInput(
          "recurrenceTumourSizeRange",
          "Tumour Size (mm)",
          min   = 0,
          max   = 100,
          value = c(0, 100),
          step  = 1,
          ticks = TRUE
        ),
        checkboxInput("recurrenceAllow2Rx","Allow 2xRx before LTP", value=TRUE)
      ),
      column(
        width = 9,
        fluidRow(
          column(
            width = 3,
            selectInput("recurrenceSelectedOrgans", "Target Organ", choices = organFactors),
            selectInput("recurrenceSelectedDiagnosisType", "Diagnosis Type", 
                        choices = c(diagnosis_type_Factors[diagnosis_type_Factors != "Benign"], "1o & 2o")),
            checkboxGroupInput(
              "recurrenceSelectedGenders", "Genders",
              choices = genderFactors,
              selected = genderFactors
            )
          ),
          column(
            width = 9,
            checkboxGroupInput(
              "recurrenceSelectedSubtypes", "Subtypes",
              choices = diagnosisSubtypeFactors,
              selected = diagnosisSubtypeFactors
            )
          )
        ),
        div(style=
              "background-color: lightgrey;
               border: 5px solid blue;
               padding: 5px;
               margin: 5px;",
            textOutput("informationalRecurrencePlot"))
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
      "1o & 2o"   = c(api$diagnosis_1o_Factors, api$diagnosis_2o_Factors),
      "Unknown"   = api$diagnosis_un_Factors,
      c("All")
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
    sizes <- as.numeric(api$cancerData$survival_max_tumour_size)  # adjust column name to match your data frame
    sizes <- sizes[!is.na(sizes)]
    if (length(sizes) > 0) {
      dataMin <- floor(min(sizes))
      dataMax <- ceiling(max(sizes))
      updateSliderInput(
        session,
        "recurrenceTumourSizeRange",
        min   = dataMin,
        max   = dataMax,
        value = c(dataMin, dataMax)
      )
    }
  })
  
  observe({
    req(api$diagnosis_type_Factors)
    baseChoices <- api$diagnosis_type_Factors[api$diagnosis_type_Factors != "Benign"]
    updateSelectInput(
      session, "recurrenceSelectedDiagnosisType",
      choices = c(baseChoices[baseChoices == "All"], "Primary", "Secondary", "1o & 2o", "Unknown"),
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
      input$recurrenceAllow2Rx,
      input$recurrenceTumourSizeRange[1],   # minTumourSize
      input$recurrenceTumourSizeRange[2]    # maxTumourSize
    )
  })

  output$informationalRecurrencePlot <- renderText({ informationalText() })
  
  informationalText <- reactive({
    headerText <- "Informational:\n"
    paste(headerText,"This is a per-patient analysis i.e. time-to-LTP is measured from first ablation even if LTP occurs after a subsequent ablation of a new lesion.")}
  )
  
  height <- reactive(detectedHeight(input, "plotRecurrenceCurve"))

  output$plotRecurrenceCurve <- renderPlot({
    # See this for dynmaic survival curves in shiny
    #    https://stackoverflow.com/questions/61273513/issue-with-r-shiny-app-interactive-survival-plots
    p <- finalRecurrencePlotInput()
    plots$activePlot <- p
    plots$activePlot
  }, height = height)
}

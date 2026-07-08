recurrencePlotTab <- function()
{
  fluidRow(
    tags$head(
      tags$style(HTML("
        #recurrencePlot .form-group { margin-bottom: 2px; }
        #recurrencePlot .control-label { margin-bottom: 1px; }
        #recurrencePlot .shiny-input-container { margin-bottom: 2px; }
        #recurrencePlot .irs { margin-bottom: 0px; }
        #recurrencePlot .radio { margin-bottom: 0px; }
        #recurrencePlot .checkbox { margin-bottom: 0px; }
      "))
    ),
    tabPanel(
      "RecurrencePlot",
      column(
        width = 2,
        div(
          column(
            width = 2,
            div(
              style =  "background-color: lightgrey;
                          border: 2px solid blue;
                          border-radius: 5px;
                          padding: 10px;
                          margin: 5px;
                          height: 320px;
                          writing-mode: vertical-rl;
                          text-orientation: mixed;
                          transform: rotate(180deg);
                          overflow-wrap: break-word;
                          display: flex;
                          align-items: center;
                          justify-content: center;
                          text-align: center;
                ",
              textOutput("informationalRecurrencePlot")
            )
          )
        )
      ),
      column(
        width = 3,
        dateInput("recurrenceStartDate", "Start Date", format = "dd/mm/yyyy", value = Sys.Date() - 365*10),
        dateInput("recurrenceEndDate", "End Date", format = "dd/mm/yyyy", value = Sys.Date()),
        div(
          style = "display:flex; gap:10px;",
          numericInput("minMonthsFollowup", "MinFU(m)", value = 3, min = 1, max = 24, width = "60px"),
          numericInput("maxYearsFollowup", "MaxFU(y)", value = 5, min = 1, max = 20, width = "60px")
        ),
        radioButtons(
          "recurrenceLTPAnalysisUnit",
          "LTP Analysis Per",
          choices = c("Patient" = "patient", "Lesion" = "episode"),
          selected = "patient",
          inline = TRUE
        ),
        # This panel is only relevant if we are on per patient LTP analysis, not per lesion analysis
        #  for per-lesion analysis the concept doesn't apply in the same way — ignoreFrstLTP was designed to say
        # "don't count LTP after just 1 Rx as a true LTP event", but in per-lesion analysis each referral episode 
        # already has its own clock, so the number of prior Rx is implicit in which episode the LTP is linked to.
        # If we wanted to be consistent with the per-patient behaviour, the equivalent for per-lesion would be to 
        # ignore LTP events where LesionNo refers to a lesion that was being ablated for the first time — i.e. where
        # there was no prior local therapy at that site. But that information isn't currently stored in cancerPerLesionData,
        # and we are not currently mixing the two tables - simplist way is to just hide it unless per patient analysis.
        checkboxInput("recurrenceAllow2Rx", "Allow 2xRx before LTP", value = TRUE)
      ),
      column(
        width = 3,
        selectInput("recurrenceSelectedOrgans", "Target Organ", choices = organFactors),
        selectInput(
          "recurrenceSelectedDiagnosisType",
          "Diagnosis Type",
          choices = c(diagnosis_type_Factors[diagnosis_type_Factors != "Benign"], "1o & 2o")
        ),
        selectInput(
          "recurrenceSelectedModality",
          "Modality",
          choices = c("All"),
          selected = "All"
        ),
        sliderInput(
          "recurrenceTumourSizeRange",
          "Tumour Size (mm)",
          min = 0, max = 100, value = c(0, 100), step = 1, ticks = TRUE
        )
      ),
      column(
        width = 4,
        checkboxGroupInput(
          "recurrenceSelectedGenders",
          "Genders",
          choices = if (!is.null(genderFactors) &&
                        length(genderFactors) > 0 &&
                        !all(is.na(genderFactors))) {
            setNames(genderFactors, substr(as.character(genderFactors), 1, 1))
          } else {
            character(0)
          },
          selected = genderFactors,
          inline = TRUE
        ),
        checkboxGroupInput(
          "recurrenceSelectedSubtypes", "Subtypes",
          choices = diagnosisSubtypeFactors,
          selected = diagnosisSubtypeFactors
        )
      )
    ),
    fluidRow(
      box(width = 12, plotOutput("plotRecurrenceCurve"))
    ),
    detectHeightJS("recurrenceplot", "plotRecurrenceCurve")
  )
}

recurrencePlotServer <- function(input, output, session, api, plots)
{
  subtypeChoices <- reactive({
    req(input$recurrenceSelectedDiagnosisType)
    req(input$recurrenceSelectedOrgans)
    
    all_choices <- switch(
      input$recurrenceSelectedDiagnosisType,
      "All"       = c("All"),
      "Primary"   = api$diagnosis_1o_Factors,
      "Secondary" = api$diagnosis_2o_Factors,
      "1o & 2o"   = c(api$diagnosis_1o_Factors, api$diagnosis_2o_Factors),
      "Unknown"   = api$diagnosis_un_Factors,
      c("All")
    )
    
    organ <- tolower(input$recurrenceSelectedOrgans)
    if (organ %in% c("liver", "kidney", "lung")) {
      prefixed <- all_choices[grepl(paste0("^", organ, ":"), tolower(all_choices))]
      if (length(prefixed) > 0) return(prefixed)
    }
    all_choices})
  
  selectedSubtypes <- reactive({
    subtypeChoices()
  })
  
  observeEvent(list(input$recurrenceSelectedDiagnosisType, input$recurrenceSelectedOrgans), {
    updateCheckboxGroupInput(
      session,
      "recurrenceSelectedSubtypes",
      choices  = subtypeChoices(),
      selected = selectedSubtypes()
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
    sizes <- as.numeric(api$cancerPerPatientData$survival_max_tumour_size)  # adjust column name to match your data frame
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
    gf <- api$genderFactors
    if (!is.null(gf) && length(gf) > 0 && !all(is.na(gf))) {
      updateCheckboxGroupInput(
        session, "recurrenceSelectedGenders",
        choices = setNames(gf, substr(as.character(gf), 1, 1)),
        selected = gf,
        inline = TRUE
      )
    } else {
      updateCheckboxGroupInput(
        session, "recurrenceSelectedGenders",
        choices = character(0),
        selected = character(0),
        inline = TRUE
      )
    }
  })
  
  modalityInitialised <- reactiveVal(FALSE)
  observe({
    if (modalityInitialised()) return()
    req(api$modalityFactors)  # or however modality choices are exposed via api
    updateSelectInput(
      session, "recurrenceSelectedModality",
      choices  = c("All", api$modalityFactors),
      selected = "All"
    )
    modalityInitialised(TRUE)
  })
  
  finalRecurrencePlotInput <- reactive({
    makeRecurrencePlot(
      input$recurrenceStartDate,
      input$recurrenceEndDate,
      input$minMonthsFollowup,
      input$maxYearsFollowup,
      input$recurrenceSelectedOrgans,
      input$recurrenceSelectedDiagnosisType,
      input$recurrenceSelectedSubtypes,
      input$recurrenceSelectedGenders,
      input$recurrenceSelectedModality,
      input$recurrenceAllow2Rx,
      input$recurrenceTumourSizeRange[1],  # minTumourSize
      input$recurrenceTumourSizeRange[2],  # maxTumourSize
      input$recurrenceLTPAnalysisUnit      # per-patient or per-lesion
    )
  })
  
  output$informationalRecurrencePlot <- renderText({ informationalText() })
  
  informationalText <- reactive({
    headerText <- "Informational:\n"
    switch(input$recurrenceLTPAnalysisUnit,
           "patient" = paste(headerText,"This is a per-patient analysis i.e. time-to-LTP is measured from first ablation even if LTP occurs after a subsequent ablation."),
           "episode" = paste(headerText,"This is a per-lesion analysis i.e. time-to-LTP is measured across all tumours from each ablation.")
    )
  }
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

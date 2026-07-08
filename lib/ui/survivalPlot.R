survivalPlotTab <- function()
{
  
  fluidRow(
    tags$head(
      tags$style(HTML("
        #survvialPlot .form-group { margin-bottom: 2px; }
        #survvialPlot .control-label { margin-bottom: 1px; }
        #survvialPlot .shiny-input-container { margin-bottom: 2px; }
        #survvialPlot .irs { margin-bottom: 0px; }
        #survvialPlot .radio { margin-bottom: 0px; }
        #survvialPlot .checkbox { margin-bottom: 0px; }
      "))
    ),
    tabPanel(
      "SurvivalPlot",
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
              textOutput("informationalSurvivalPlot")
            )
          )
        )
      ),
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
        div(
          style = "display:flex; gap:10px;",
          numericInput("minMonthsFollowup", "MinFU(m)", value = 3, min = 1, max = 24, width = "60px"),
          numericInput("maxYearsFollowup", "MaxFU(y)", value = 5, min = 1, max = 20, width = "60px")
        ),
        sliderInput(
          "survivalTumourSizeRange",
          "Tumour Size (mm)",
          min   = 0,
          max   = 100,
          value = c(0, 100),
          step  = 1,
          ticks = TRUE
        )
      ),
      column(
        width = 3,
        selectInput("survivalSelectedOrgans","Target Organ", choices = organFactors, selected = organFactors[1]),
        selectInput("survivalSelectedDiagnosisType", "Diagnosis Type", choices = diagnosis_type_Factors),
        selectInput(
          "survivalSelectedModality",
          "Modality",
          choices = c("All"),
          selected = "All"
        ),
        radioButtons(
          "survivalLTPFSRadio",
          "Survival Plot Type",
          c("OS" = 0, "CSS" = 1, "LTP-Free OS" = 2, "LTP-Free CSS" = 3),
          inline = TRUE
        )
      ),
      column(
        width = 4,
        checkboxGroupInput(
          "survivalSelectedGenders",
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
          "survivalSelectedSubtypes", "Subtypes",
          choices = diagnosisSubtypeFactors,
          selected = diagnosisSubtypeFactors
        ),
        conditionalPanel(
          condition = "input.survivalLTPFSRadio == '2' || input.survivalLTPFSRadio == '3'",
          checkboxInput("survivalAllow2Rx", "Allow 2xRx before LTP", value = TRUE)
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
    req(input$survivalSelectedOrgans)
    
    all_choices <- switch(
      input$survivalSelectedDiagnosisType,
      "All"       = c("All"),
      "Primary"   = api$diagnosis_1o_Factors,
      "Secondary" = api$diagnosis_2o_Factors,
      "1o & 2o"   = unique(c(api$diagnosis_1o_Factors, api$diagnosis_2o_Factors)),
      "Unknown"   = api$diagnosis_un_Factors,
      c("All")
    )
    
    organ <- tolower(trimws(input$survivalSelectedOrgans))
    if (organ %in% c("liver", "kidney", "lung")) {
      prefixed <- all_choices[grepl(paste0("^", organ, "\\s*:"), tolower(all_choices))]
      if (length(prefixed) > 0) return(prefixed)
    }
    
    all_choices
  })
  
  selectedSubtypes <- reactive({
    subtypeChoices()
  })
  
  observeEvent(list(input$survivalSelectedDiagnosisType, input$survivalSelectedOrgans), {
    updateCheckboxGroupInput(
      session,
      "survivalSelectedSubtypes",
      choices  = subtypeChoices(),
      selected = selectedSubtypes()
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
    gf <- api$genderFactors
    if (!is.null(gf) && length(gf) > 0 && !all(is.na(gf))) {
      updateCheckboxGroupInput(
        session, "survivalSelectedGenders",
        choices = setNames(gf, substr(as.character(gf), 1, 1)),
        selected = gf,
        inline = TRUE
      )
    } else {
      updateCheckboxGroupInput(
        session, "survivalSelectedGenders",
        choices = character(0),
        selected = character(0),
        inline = TRUE
      )
    }
  })
  
  # Update tumour size slider range dynamically based on actual data
  observe({
    sizes <- as.numeric(api$cancerPerPatientData$survival_max_tumour_size)  # adjust column name to match your data frame
    sizes <- sizes[!is.na(sizes)]
    if (length(sizes) > 0) {
      dataMin <- floor(min(sizes))
      dataMax <- ceiling(max(sizes))
      updateSliderInput(
        session,
        "survivalTumourSizeRange",
        min   = dataMin,
        max   = dataMax,
        value = c(dataMin, dataMax)
      )
    }
  })
  
  modalityInitialised <- reactiveVal(FALSE)
  observe({
    if (modalityInitialised()) return()
    req(api$modalityFactors)  # or however modality choices are exposed via api
    updateSelectInput(
      session, "survivalSelectedModality",
      choices  = c("All", api$modalityFactors),
      selected = "All"
    )
    modalityInitialised(TRUE)
  })
  
  finalSurvivalPlotInput <- reactive({
    makeSurvivalPlot(
      input$survivalStartDate,
      input$survivalEndDate,
      input$minMonthsFollowup,
      input$maxYearsFollowup,
      input$survivalSelectedOrgans,
      input$survivalSelectedDiagnosisType,
      input$survivalSelectedSubtypes,
      input$survivalSelectedGenders,
      input$survivalSelectedModality,
      input$survivalLTPFSRadio,
      input$survivalAllow2Rx,
      input$survivalTumourSizeRange[1],   # minTumourSize
      input$survivalTumourSizeRange[2]    # maxTumourSize
    )
  })
  
  output$informationalSurvivalPlot <- renderText({ informationalText() })
  
  informationalText <- reactive({
    headerText <- "Informational:\n"
    switch(input$survivalLTPFSRadio,
           "0" = paste(headerText,"Overall Survival Plot"),
           "1" = paste(headerText,"Cancer Specific Survival Plot"),
           "2" = paste(headerText,"Local Tumour Progression-free Overall Survival Plot"),
           "3" = paste(headerText,"Local Tumour Progression-free Cancer Specific Survival Plot"))
  }
  )
  
  height <- reactive(detectedHeight(input, "plotSurvivalCurve"))
  output$plotSurvivalCurve <- renderPlot({
    p <- finalSurvivalPlotInput()
    plots$activePlot <- p
    plots$activePlot
  }, height = height)
}
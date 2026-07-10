survivalPlotTab <- function()
{
  fluidRow(
    tabPanel(
      "SurvivalPlot",
      column(
        width = 4,
        dateInput("survivalStartDate", "Start Date", format = "dd/mm/yyyy", value = Sys.Date() - 365*20),
        dateInput("survivalEndDate", "End Date", format = "dd/mm/yyyy", value = Sys.Date()),
        div(
          style = "display:flex; gap:10px;",
          numericInput("minMonthsFollowup", "MinFU(m)", value = 3, min = 1, max = 24, width = "60px"),
          numericInput("maxYearsFollowup", "MaxFU(y)", value = 5, min = 1, max = 20, width = "60px")
        ),
        conditionalPanel(
          condition = "input.survivalLTPFSRadio == '2' || input.survivalLTPFSRadio == '3'",
          checkboxInput("survivalAllow2Rx", "Allow 2xRx before LTP", value = TRUE)
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
        selectInput("survivalSelectedOrgans","Target Organ", choices = organFactors, selected = organFactors[1]),
        selectInput("survivalSelectedDiagnosisType", "Diagnosis Type", choices = diagnosis_type_Factors),
        selectInput(
          "survivalSelectedModality",
          "Modality",
          choices = c("All"),
          selected = "All"
        ),
        uiOutput("survivalHelpUI")
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
        sliderInput(
          "survivalTumourSizeRange",
          "Tumour Size (mm)",
          min   = 0,
          max   = 100,
          value = c(0, 100),
          step  = 1,
          ticks = TRUE
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
  
  informationalText <- reactive({
    plotTypeText <- switch(
      as.character(input$survivalLTPFSRadio),
      "0" = "Overall Survival",
      "1" = "Cancer Specific Survival",
      "2" = "Local Tumour Progression-free Overall Survival",
      "3" = "Local Tumour Progression-free Cancer Specific Survival",
      "Survival"
    )
    
    if (input$survivalLTPFSRadio %in% c("2", "3")) {
      if (isTRUE(input$survivalAllow2Rx)) {
        paste0(
          plotTypeText, ": time-to-LTP is measured from the first ablation. ",
          "LTP after one subsequent re-Rx is treated as managed disease and censored; ",
          "LTP after two or more subsequent re-Rx is counted as an event."
        )
      } else {
        paste0(
          plotTypeText, ": time-to-LTP is measured from the first ablation. ",
          "Every confirmed LTP is counted as an event, regardless of later re-Rx."
        )
      }
    } else {
      paste0(
        plotTypeText, ": time is measured from the first ablation. ",
        "Events are death-related according to the selected survival definition."
      )
    }
  })
  
  output$survivalHelpUI <- renderUI({
    tags$span(
      title = informationalText(),
      style = "color:#337ab7; cursor: help; display:inline-block; font-weight: bold;",
      tags$b("ⓘ Hover for Information")
    )
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
  
  informationalText <- reactive({
    
    header <- switch(
      as.character(input$survivalLTPFSRadio),
      "0" = "Overall Survival:\n",
      "1" = "Cancer Specific Survival:\n",
      "2" = "LTP-Free Overall Survival:\n",
      "3" = "LTP-Free Cancer Specific Survival:\n",
      "Survival:\n"
    )
    
    switch(
      as.character(input$survivalLTPFSRadio),
      
      # Overall Survival
      "0" = paste0(
        header,
        "Time-to-event is measured from the first ablation. ",
        "Death from any cause is counted as an event; patients still alive at last follow-up are censored."
      ),
      
      # Cancer Specific Survival
      "1" = paste0(
        header,
        "Time-to-event is measured from the first ablation. ",
        "Only deaths attributable to cancer are counted as events; ",
        "patients dying of other causes or alive at last follow-up are censored."
      ),
      
      # LTP-Free Overall Survival
      "2" = {
        if (isTRUE(input$survivalAllow2Rx)) {
          paste0(
            header,
            "Time-to-event is measured from the first ablation. ",
            "LTP occurring after only one subsequent re-ablation is considered managed disease and is censored. ",
            "LTP after two or more subsequent re-ablations, or death from any cause before LTP, is counted as an event."
          )
        } else {
          paste0(
            header,
            "Time-to-event is measured from the first ablation. ",
            "The first confirmed LTP is counted as an event regardless of any subsequent re-ablation. ",
            "Death before LTP is also counted as an event."
          )
        }
      },
      
      # LTP-Free Cancer Specific Survival
      "3" = {
        if (isTRUE(input$survivalAllow2Rx)) {
          paste0(
            header,
            "Time-to-event is measured from the first ablation. ",
            "LTP occurring after only one subsequent re-ablation is considered managed disease and is censored. ",
            "LTP after two or more subsequent re-ablations or cancer-related death before LTP is counted as an event. ",
            "Deaths from other causes are censored."
          )
        } else {
          paste0(
            header,
            "Time-to-event is measured from the first ablation. ",
            "The first confirmed LTP is counted as an event regardless of any subsequent re-ablation. ",
            "Cancer-related death before LTP is also counted as an event, whereas deaths from other causes are censored."
          )
        }
      }
    )
  })
  
  height <- reactive(detectedHeight(input, "plotSurvivalCurve"))
  output$plotSurvivalCurve <- renderPlot({
    p <- finalSurvivalPlotInput()
    plots$activePlot <- p
    plots$activePlot
  }, height = height)
}
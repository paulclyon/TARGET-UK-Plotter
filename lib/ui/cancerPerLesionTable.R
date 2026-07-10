cancerPerLesionTab <- function() {
  list(
    fluidRow(box(
      width = 12,
      column(
        width = 5,
        selectInput("cancerPerLesionTabSelectedOrgans","Target Organ", choices = organFactors, selected = organFactors[1]),
        selectInput("cancerPerLesionTabSelectedDiagnosisType", "Diagnosis Type", 
                    choices = c("All", "Primary", "Secondary", "1o & 2o", "Unknown")),
        selectInput("cancerPerLesionTabSelectedModality", "Modality", choices = c("All"))
        ),
      column(
        width = 5,
        checkboxGroupInput(
          "cancerPerLesionTabSelectedSubtypes", "Subtypes",
          choices = diagnosisSubtypeFactors,
          selected = diagnosisSubtypeFactors
        )
      ),
      column(
        width = 2,
        div(
          style = "border:1px solid #ddd;border-radius:8px;padding:2px;background:#f9f9f9;display:flex;flex-direction:column;gap:12px;",
          actionButton("buttonPasteCancerPerLesionTabData",   "> Clip", width = "100%"),
          actionButton("buttonSaveCancerPerLesionTabData",    "> File", width = "100%"),
          actionButton("buttonRefreshCancerPerLesionTabData", "Refresh", width = "100%")
        )
      )
    )),
    fluidRow(box(
      width = 12,
      DT::dataTableOutput("tableCancerPerLesion"), style = "overflow-y: scroll;overflow-x: scroll;"
    ))
  )
}

cancerPerLesionTableServer <- function(input, output, session, isDocker, api)
{
  refreshTrigger <- reactiveVal(0)
  
  subtypeChoices <- reactive({
    req(input$cancerPerLesionTabSelectedDiagnosisType)
    req(input$cancerPerLesionTabSelectedOrgans)
    all_choices <- switch(
      input$cancerPerLesionTabSelectedDiagnosisType,
      "All"       = api$diagnosisSubtypeFactors,
      "Primary"   = api$diagnosis_1o_Factors,
      "Secondary" = api$diagnosis_2o_Factors,
      "1o & 2o"   = c(api$diagnosis_1o_Factors, api$diagnosis_2o_Factors),
      "Unknown"   = api$diagnosis_un_Factors,
      api$diagnosisSubtypeFactors
    )
    organ <- tolower(input$cancerPerLesionTabSelectedOrgans)
    if (organ %in% c("kidney", "lung", "liver")) {
      organ_choices <- all_choices[grepl(paste0("^", organ, ":"), tolower(all_choices))]
      if (length(organ_choices) > 0) return(organ_choices)
    }
    all_choices
  })
  
  cancerPerLesionDataFiltered <- reactive({
    refreshTrigger()
    data <- cancerPerLesionData
    if (is.null(data) || !is.data.frame(data)) return(data.frame())
    
    if (!is.null(input$cancerPerLesionTabSelectedOrgans) && input$cancerPerLesionTabSelectedOrgans != "All")
      data <- data[data$Organ %in% input$cancerPerLesionTabSelectedOrgans, ]

    if (!is.null(input$cancerPerLesionTabSelectedModality) && input$cancerPerLesionTabSelectedModality != "All")
      data <- data[grepl(input$cancerPerLesionTabSelectedModality, data$RxModalities, fixed = TRUE), ]
    
    subtypes <- input$cancerPerLesionTabSelectedSubtypes
    if (is.null(subtypes)) subtypes <- c("All")
    
    diagType <- input$cancerPerLesionTabSelectedDiagnosisType
    if (is.null(diagType)) diagType <- "All"
    
    if (diagType == "All")
    {
      if (!("All" %in% subtypes)) {
        data <- data[data$Diagnosis1o %in% subtypes |
                       data$Diagnosis2o %in% subtypes |
                       data$DiagnosisUn %in% subtypes, ]
      }
    }
    else if (diagType == "1o & 2o")
    {
      data <- data[data$Diagnosis1o %in% subtypes | data$Diagnosis2o %in% subtypes, ]
    }
    else
    {
      data <- switch(substring(diagType, 1, 1),
                     "P" = data[data$Diagnosis1o %in% subtypes, ],
                     "S" = data[data$Diagnosis2o %in% subtypes, ],
                     "U" = data[data$DiagnosisUn %in% subtypes, ],
                     data
      )
    }
    data
  })
  
  observe({
    choices <- subtypeChoices()
    updateCheckboxGroupInput(
      session, "cancerPerLesionTabSelectedSubtypes",
      choices  = choices,
      selected = choices
    )
  })
  
  observeEvent(
    list(
      input$cancerPerLesionTabSelectedDiagnosisType,
      input$cancerPerLesionTabSelectedOrgans
    ),
    {
      choices <- subtypeChoices()
      updateCheckboxGroupInput(
        session,
        "cancerPerLesionTabSelectedSubtypes",
        choices = choices,
        selected = choices
      )
    },
    ignoreInit = FALSE
  )
  
  observe({
    updateSelectInput(session, "cancerPerLesionTabSelectedOrgans", "Target Organ",
                      choices = api$organFactors,
                      selected = api$organFactors[1]
    )
  })
  
  observe({
    req(api$diagnosis_type_Factors)
    updateSelectInput(
      session, "cancerPerLesionTabSelectedDiagnosisType",
      choices = c("All", "Primary", "Secondary", "1o & 2o", "Unknown"),
      selected = api$diagnosis_type_Factors[1]
    )
  })
  
  observe({
    req(api$modalityFactors)
    updateSelectInput(session, "cancerPerLesionTabSelectedModality", "Modality",
                      choices = c("All", api$modalityFactors),
                      selected = "All")
  })
  
  observeEvent(input$buttonPasteCancerPerLesionTabData, {
    if (isTRUE(isDocker)) {
      shinyCatch(
        {
          message("Sorry running in a Docker via Web interface therefore data export functions not available...")
        },
        prefix = ""
      )
    } else {
      copyDataToClipboard(cancerPerLesionDataFiltered())
      shinyCatch(
        {
          message("Copied data to the clipboard, please paste into app such as Microsoft Excel on a secure computer (patient IDs included).")
        },
        prefix = ""
      )
    }
  })
  
  observeEvent(input$buttonSaveCancerPerLesionTabData, {
    if (isTRUE(isDocker)) {
      shinyCatch(
        {
          message("Sorry running in a Docker via Web interface therefore data export functions not available...")
        },
        prefix = ""
      )
    } else {
      shinyCatch(
        {
          message("If this is a secure computer (patient IDs included), choose a file to export to...")
        },
        prefix = ""
      )
      exportFile <- tryCatch(
        file.choose(new = TRUE),
        error = function(err) { logger(err, FALSE); NA }
      )
      if (!is.na(exportFile) && nchar(exportFile) > 0) {
        if (!endsWith(exportFile, ".csv")) {
          exportFile <- paste(exportFile, ".csv", sep = "")
        }
        shinyCatch(
          {
            message(paste("Attempting to export cancerPerLesion data to file", exportFile))
          },
          prefix = ""
        )
        write.csv(cancerPerLesionDataFiltered(), exportFile, row.names = TRUE)
        shinyCatch(
          {
            message(paste("Exported cancerPerLesion data to file", exportFile))
          },
          prefix = ""
        )
      } else {
        shinyCatch(
          {
            message(paste("No file selected to export to, no data export performed"))
          },
          prefix = ""
        )
      }
    }
  })
  
  observeEvent(input$buttonRefreshCancerPerLesionTabData, {
    refreshTrigger(refreshTrigger() + 1)
  })
  
  output$tableCancerPerLesion <- DT::renderDataTable({
    refreshTrigger()
    DT::datatable(cancerPerLesionDataFiltered())
  })
}
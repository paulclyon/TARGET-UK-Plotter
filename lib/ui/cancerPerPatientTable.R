cancerPerPatientTab <- function() {
  list(
    fluidRow(box(
      width = 12,
      column(
        width = 5,
        selectInput("cancerPerPatientTabSelectedOrgans","Target Organ", choices = organFactors, selected = organFactors[1]),
        selectInput("cancerPerPatientTabSelectedDiagnosisType", "Diagnosis Type", 
                    choices = c("All", "Primary", "Secondary", "1o & 2o", "Unknown")),
        selectInput("cancerPerPatientTabSelectedModality", "Modality", choices = c("All"))
        ),
      column(
        width = 5,
        checkboxGroupInput(
          "cancerPerPatientTabSelectedSubtypes", "Subtypes",
          choices = diagnosisSubtypeFactors,
          selected = diagnosisSubtypeFactors
        )
      ),
      column(
        width = 2,
        div(
          style = "border:1px solid #ddd;border-radius:8px;padding:2px;background:#f9f9f9;display:flex;flex-direction:column;gap:12px;",
          actionButton("buttonPasteCancerPerPatientTabData",   "> Clip", width = "100%"),
          actionButton("buttonSaveCancerPerPatientTabData",    "> File", width = "100%"),
          actionButton("buttonRefreshCancerPerPatientTabData", "Refresh", width = "100%")
        )
      )
    )),
    fluidRow(box(
      width = 12,
      DT::dataTableOutput("tableCancerPerPatient"), style = "overflow-y: scroll;overflow-x: scroll;"
    ))
  )
}

cancerPerPatientTableServer <- function(input, output, session, isDocker, api)
{
  refreshTrigger <- reactiveVal(0)
  
  subtypeChoices <- reactive({
    req(input$cancerPerPatientTabSelectedDiagnosisType)
    req(input$cancerPerPatientTabSelectedOrgans)
    all_choices <- switch(
      input$cancerPerPatientTabSelectedDiagnosisType,
      "All"       = c("All"),
      "Primary"   = api$diagnosis_1o_Factors,
      "Secondary" = api$diagnosis_2o_Factors,
      "1o & 2o"   = c(api$diagnosis_1o_Factors, api$diagnosis_2o_Factors),
      "Benign"    = api$diagnosis_bn_Factors,
      "Unknown"   = api$diagnosis_un_Factors,
      c("All")
    )
    organ <- tolower(input$cancerPerPatientTabSelectedOrgans)
    if (organ %in% c("liver", "kidney", "lung")) {
      prefixed <- all_choices[grepl(paste0("^", organ, ":"), tolower(all_choices))]
      if (length(prefixed) > 0) return(prefixed)
    }
    all_choices
  })
  
  cancerPerPatientDataFiltered <- reactive({
    refreshTrigger()
    data <- cancerPerPatientData
    if (is.null(data) || !is.data.frame(data)) return(data.frame())
    
    if (!is.null(input$cancerPerPatientTabSelectedOrgans) && input$cancerPerPatientTabSelectedOrgans != "All")
      data <- data[data$Organ %in% input$cancerPerPatientTabSelectedOrgans, ]
    
    if (!is.null(input$cancerPerPatientTabSelectedModality) && input$cancerPerPatientTabSelectedModality != "All")
      data <- data[grepl(input$cancerPerPatientTabSelectedModality, data$RxModalities, fixed = TRUE), ]
    
    subtypes <- input$cancerPerPatientTabSelectedSubtypes
    if (is.null(subtypes)) subtypes <- c("All")
    
    diagType <- input$cancerPerPatientTabSelectedDiagnosisType
    if (is.null(diagType)) diagType <- "All"
    
    if (diagType == "All")
    {
      if (!("All" %in% subtypes))
      {
        data <- data[
          data$Diagnosis1o %in% subtypes |
            data$Diagnosis2o %in% subtypes |
            data$DiagnosisUn %in% subtypes,
        ]
      }
    }
    else if (diagType == "1o & 2o")
    {
      data <- data[data$Diagnosis1o %in% subtypes | data$Diagnosis2o %in% subtypes, ]
    }
    else
    {
      data <- switch(
        substring(diagType, 1, 1),
        "P" = data[data$DiagnosisType == "P" & data$Diagnosis1o %in% subtypes, ],
        "S" = data[data$DiagnosisType == "S" & data$Diagnosis2o %in% subtypes, ],
        "U" = data[data$DiagnosisType == "U" & data$DiagnosisUn %in% subtypes, ],
        data
      )
    }
    data
  })
  
  observe({
    choices <- subtypeChoices()
    updateCheckboxGroupInput(
      session, "cancerPerPatientTabSelectedSubtypes",
      choices  = choices,
      selected = choices
    )
  })
  
  observeEvent(
    list(
      input$cancerPerPatientTabSelectedDiagnosisType,
      input$cancerPerPatientTabSelectedOrgans
    ),
    {
      updateCheckboxGroupInput(
        session,
        "cancerPerPatientTabSelectedSubtypes",
        choices  = subtypeChoices(),
        selected = subtypeChoices()
      )
    },
    ignoreInit = FALSE
  )
  
  observe({
    updateSelectInput(session, "cancerPerPatientTabSelectedOrgans", "Target Organ",
                      choices = api$organFactors,
                      selected = api$organFactors[1]
    )
  })
  
  observe({
    req(api$diagnosis_type_Factors)
    updateSelectInput(
      session, "cancerPerPatientTabSelectedDiagnosisType",
      choices = c("All", "Primary", "Secondary", "1o & 2o", "Unknown"),
      selected = api$diagnosis_type_Factors[1]
    )
  })
  
  observe({
    req(api$modalityFactors)
    updateSelectInput(session, "cancerPerPatientTabSelectedModality", "Modality",
                      choices = c("All", api$modalityFactors),
                      selected = "All")
  })
  
  observeEvent(input$buttonPasteCancerPerPatientTabData, {
    if (isTRUE(isDocker)) {
      shinyCatch(
        {
          message("Sorry running in a Docker via Web interface therefore data export functions not available...")
        },
        prefix = ""
      )
    } else {
      copyDataToClipboard(cancerPerPatientDataFiltered())
      shinyCatch(
        {
          message("Copied data to the clipboard, please paste into app such as Microsoft Excel on a secure computer (patient IDs included).")
        },
        prefix = ""
      )
    }
  })
  
  observeEvent(input$buttonSaveCancerPerPatientTabData, {
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
            message(paste("Attempting to export cancerPerPatient data to file", exportFile))
          },
          prefix = ""
        )
        write.csv(cancerPerPatientDataFiltered(), exportFile, row.names = TRUE)
        shinyCatch(
          {
            message(paste("Exported cancerPerPatient data to file", exportFile))
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
  
  observeEvent(input$buttonRefreshCancerPerPatientTabData, {
    refreshTrigger(refreshTrigger() + 1)
  })
  
  output$tableCancerPerPatient <- DT::renderDataTable({
    refreshTrigger()
    DT::datatable(cancerPerPatientDataFiltered())
  })
}
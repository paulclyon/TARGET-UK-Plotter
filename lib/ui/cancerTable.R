cancerTab <- function() {
  list(
    fluidRow(box(
      width = 12,
      column(
        width = 5,
        selectInput("cancerTabSelectedOrgans","Target Organ", choices = organFactors, selected = organFactors[1]),
        selectInput("cancerTabSelectedDiagnosisType", "Diagnosis Type", 
                    choices = c("All", "Primary", "Secondary", "1o & 2o", "Unknown")),
        selectInput("cancerTabSelectedModality", "Modality", choices = c("All"))
        ),
      column(
        width = 5,
        checkboxGroupInput(
          "cancerTabSelectedSubtypes", "Subtypes",
          choices = diagnosisSubtypeFactors,
          selected = diagnosisSubtypeFactors
        )
      ),
      column(
        width = 2,
        div(
          style = "border:1px solid #ddd;border-radius:8px;padding:2px;background:#f9f9f9;display:flex;flex-direction:column;gap:12px;",
          actionButton("buttonPasteCancerTabData",   "> Clip", width = "100%"),
          actionButton("buttonSaveCancerTabData",    "> File", width = "100%"),
          actionButton("buttonRefrehCancerTabData", "Refresh", width = "100%")
        )
      )
    )),
    fluidRow(box(
      width = 12,
      DT::dataTableOutput("tableCancer"), style = "overflow-y: scroll;overflow-x: scroll;"
    ))
  )
}

cancerTableServer <- function(input, output, session, isDocker, api)
{
  refreshTrigger <- reactiveVal(0)
  
  subtypeChoices <- reactive({
    req(input$cancerTabSelectedDiagnosisType)
    switch(
      input$cancerTabSelectedDiagnosisType,
      "All"       = c("All"),
      "Primary"   = api$diagnosis_1o_Factors,
      "Secondary" = api$diagnosis_2o_Factors,
      "1o & 2o"   = c(api$diagnosis_1o_Factors, api$diagnosis_2o_Factors),
      api$diagnosisSubtypeFactors
    )
  })
  
  cancerDataFiltered <- reactive({
    refreshTrigger()
    data <- cancerData
    if (is.null(data) || !is.data.frame(data)) return(data.frame())
    
    if (!is.null(input$cancerTabSelectedOrgans) && input$cancerTabSelectedOrgans != "All")
      data <- data[data$Organ %in% input$cancerTabSelectedOrgans, ]
    
    if (!is.null(input$benignTabSelectedModality) && input$benignTabSelectedModality != "All")
      data <- data[grepl(input$benignTabSelectedModality, data$RxModalities, fixed = TRUE), ]
    
    subtypes <- input$cancerTabSelectedSubtypes
    if (is.null(subtypes)) subtypes <- c("All")
    
    diagType <- input$cancerTabSelectedDiagnosisType
    if (is.null(diagType)) diagType <- "All"
    
    if (diagType == "All")
    {
      if (!("All" %in% subtypes))
        {
        data <- data[data$Organ %in% subtypes, ]
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
      session, "cancerTabSelectedSubtypes",
      choices  = choices,
      selected = choices
    )
  })
  
  observeEvent(input$cancerTabSelectedDiagnosisType, {
    choices <- subtypeChoices()
    updateCheckboxGroupInput(
      session,
      "cancerTabSelectedSubtypes",
      choices = choices,
      selected = choices
    )
  }, ignoreInit = FALSE)
  
  observe({
    updateSelectInput(session, "cancerTabSelectedOrgans", "Target Organ",
                      choices = api$organFactors,
                      selected = api$organFactors[1]
    )
  })
  
  observe({
    req(api$diagnosis_type_Factors)
    updateSelectInput(
      session, "cancerTabSelectedDiagnosisType",
      choices = c("All", "Primary", "Secondary", "1o & 2o", "Unknown"),
      selected = api$diagnosis_type_Factors[1]
    )
  })
  
  observe({
    req(api$modalityFactors)
    updateSelectInput(session, "cancerTabSelectedModality", "Modality",
                      choices = c("All", api$modalityFactors),
                      selected = "All")
  })
  
  observeEvent(input$buttonPasteCancerTabData, {
    if (isTRUE(isDocker)) {
      shinyCatch(
        {
          message("Sorry running in a Docker via Web interface therefore data export functions not available...")
        },
        prefix = ""
      )
    } else {
      copyDataToClipboard(cancerDataFiltered())
      shinyCatch(
        {
          message("Copied data to the clipboard, please paste into app such as Microsoft Excel on a secure computer (patient IDs included).")
        },
        prefix = ""
      )
    }
  })
  
  observeEvent(input$buttonSaveCancerTabData, {
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
            message(paste("Attempting to export cancer data to file", exportFile))
          },
          prefix = ""
        )
        write.csv(cancerDataFiltered(), exportFile, row.names = TRUE)
        shinyCatch(
          {
            message(paste("Exported cancer data to file", exportFile))
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
  
  observeEvent(input$buttonRefreshCancerTabData, {
    refreshTrigger(refreshTrigger() + 1)
  })
  
  output$tableCancer <- DT::renderDataTable({
    refreshTrigger()
    DT::datatable(cancerDataFiltered())
  })
}
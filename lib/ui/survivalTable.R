survivalTab <- function() {
  list(
    fluidRow(box(
      width = 12,
      column(
        width = 5,
        selectInput("survivalTabSelectedOrgans","Target Organ", choices = organFactors, selected = organFactors[1]),
        selectInput("survivalTabSelectedDiagnosisType", "Diagnosis Type", 
                    choices = c("All", "Primary", "Secondary", "1o & 2o", "Benign", "Unknown"))
        ),
      column(
        width = 5,
        checkboxGroupInput(
          "survivalTabSelectedSubtypes", "Subtypes",
          choices = diagnosisSubtypeFactors,
          selected = diagnosisSubtypeFactors
        )
      ),
      column(
        width = 2,
        div(
          style = "border:1px solid #ddd;border-radius:8px;padding:2px;background:#f9f9f9;display:flex;flex-direction:column;gap:12px;",
          actionButton("buttonPasteSurvivalTabData",   "> Clip", width = "100%"),
          actionButton("buttonSaveSurvivalTabData",    "> File", width = "100%"),
          actionButton("buttonRefreshSurvivaTablData", "Refresh", width = "100%")
        )
      )
    )),
    fluidRow(box(
      width = 12,
      DT::dataTableOutput("tableSurvival"), style = "overflow-y: scroll;overflow-x: scroll;"
    ))
  )
}

survivalTableServer <- function(input, output, session, isDocker, api)
{
  refreshTrigger <- reactiveVal(0)
  
  subtypeChoices <- reactive({
    req(input$survivalTabSelectedDiagnosisType)
    switch(
      input$survivalTabSelectedDiagnosisType,
      "All"       = c("All"),
      "Primary"   = api$diagnosis_1o_Factors,
      "Secondary" = api$diagnosis_2o_Factors,
      "1o & 2o"   = c(api$diagnosis_1o_Factors, api$diagnosis_2o_Factors),
      "Benign"    = api$diagnosis_bn_Factors,
      api$diagnosisSubtypeFactors
    )
  })
  
  survivalDataFiltered <- reactive({
    refreshTrigger()
    data <- survivalData
    if (is.null(data) || !is.data.frame(data)) return(data.frame())
    
    if (!is.null(input$survivalTabSelectedOrgans) && input$survivalTabSelectedOrgans != "All")
      data <- data[data$Organ %in% input$survivalTabSelectedOrgans, ]
    
    subtypes <- input$survivalTabSelectedSubtypes
    if (is.null(subtypes)) subtypes <- c("All")
    
    diagType <- input$survivalTabSelectedDiagnosisType
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
                     "B" = data[data$DiagnosisBn %in% subtypes, ],
                     "U" = data[data$DiagnosisUn %in% subtypes, ],
                     data
      )
    }
    data
  })  
  observe({
    choices <- subtypeChoices()
    updateCheckboxGroupInput(
      session, "survivalTabSelectedSubtypes",
      choices  = choices,
      selected = choices
    )
  })
  
  observeEvent(input$survivalTabSelectedDiagnosisType, {
    choices <- subtypeChoices()
    updateCheckboxGroupInput(
      session,
      "survivalTabSelectedSubtypes",
      choices = choices,
      selected = choices
    )
  }, ignoreInit = FALSE)
  
  observe({
    updateSelectInput(session, "survivalTabSelectedOrgans", "Target Organ",
                      choices = api$organFactors,
                      selected = api$organFactors[1]
    )
  })
  
  observe({
    req(api$diagnosis_type_Factors)
    updateSelectInput(
      session, "survivalTabSelectedDiagnosisType",
      choices = c("All", "Primary", "Secondary", "1o & 2o", "Benign", "Unknown"),
      selected = api$diagnosis_type_Factors[1]
    )
  })
  
  observeEvent(input$buttonPasteSurvivalTabData, {
    if (isTRUE(isDocker)) {
      shinyCatch(
        {
          message("Sorry running in a Docker via Web interface therefore data export functions not available...")
        },
        prefix = ""
      )
    } else {
      copyDataToClipboard(survivalDataFiltered())
      shinyCatch(
        {
          message("Copied data to the clipboard, please paste into app such as Microsoft Excel on a secure computer (patient IDs included).")
        },
        prefix = ""
      )
    }
  })
  
  observeEvent(input$buttonSaveSurvivalTabData, {
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
            message(paste("Attempting to export survival data to file", exportFile))
          },
          prefix = ""
        )
        write.csv(survivalDataFiltered(), exportFile, row.names = TRUE)
        shinyCatch(
          {
            message(paste("Exported survival data to file", exportFile))
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
  
  observeEvent(input$buttonRefreshSurvivaTablData, {
    refreshTrigger(refreshTrigger() + 1)
  })
  
  output$tableSurvival <- DT::renderDataTable({
    refreshTrigger()
    DT::datatable(survivalDataFiltered())
  })
}
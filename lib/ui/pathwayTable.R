pathwayTab <- function() {
  list(
    fluidRow(box(
      width = 12,
      column(
        width = 5,
        radioButtons(
          "rxTimesTableRadio",
          "Pathway Table Type",
          c(
            "Treated" = "rxdoneTable",
            "Current Waiting" = "rxwaitTable",
            "Monthly Waiting List" = "monthlyWaitTable"
          )
        ),
        selectInput("pathwayTabSelectedOrgans", "Target Organ", choices = organFactors),
        selectInput("pathwayTabSelectedDiagnosisType", "Diagnosis Type",
                    choices = c("All", "Primary", "Secondary", "1o & 2o", "Benign", "Unknown")),
        selectInput("pathwayTabSelectedModality", "Modality", choices = modalityFactors)
      ),
      column(
        width = 5,
        checkboxGroupInput(
          "pathwayTabSelectedSubtypes", "Subtypes",
          choices = diagnosisSubtypeFactors,
          selected = diagnosisSubtypeFactors
        )
      ),
      column(
        width = 2,
        div(
          style = "border:1px solid #ddd;border-radius:8px;padding:2px;background:#f9f9f9;display:flex;flex-direction:column;gap:12px;",
          actionButton("buttonPasteRxTimesData", "> Clip", width = "100%"),
          actionButton("buttonSaveRxTimesData",  "> File", width = "100%"),
          actionButton("buttonRefreshTimesData", "Refresh", width = "100%")
        )
      )
    )),
    fluidRow(box(
      width = 12,
      DT::dataTableOutput("tableRxPathway"), style = "overflow-y: scroll;overflow-x: scroll;"
    ))
  )
}

pathwayTableServer <- function(input, output, session, isDocker, api) {
  
  subtypeChoices <- reactive({
    req(input$pathwayTabSelectedDiagnosisType)
    switch(
      input$pathwayTabSelectedDiagnosisType,
      "All"       = c("All"),
      "Primary"   = api$diagnosis_1o_Factors,
      "Secondary" = api$diagnosis_2o_Factors,
      "1o & 2o"   = c(api$diagnosis_1o_Factors, api$diagnosis_2o_Factors),
      "Benign"    = api$diagnosis_bn_Factors,
      api$diagnosisSubtypeFactors
    )
  })
  
  observe({
    req(api$organFactors)
    updateSelectInput(session, "pathwayTabSelectedOrgans", "Target Organ",
                      choices = api$organFactors,
                      selected = api$organFactors[1]
    )
  })
  
  observe({
    req(api$modalityFactors)
    updateSelectInput(session, "pathwayTabSelectedModality", "Modality",
                      choices = c("All", api$modalityFactors),
                      selected = "All")
  })
  
  observe({
    req(api$diagnosis_type_Factors)
    updateSelectInput(
      session, "pathwayTabSelectedDiagnosisType",
      choices = c("All", "Primary", "Secondary", "1o & 2o", "Benign", "Unknown"),
      selected = api$diagnosis_type_Factors[1]
    )
  })
  
  observe({
    choices <- subtypeChoices()
    updateCheckboxGroupInput(
      session, "pathwayTabSelectedSubtypes",
      choices  = choices,
      selected = choices
    )
  })
  
  observeEvent(input$pathwayTabSelectedDiagnosisType, {
    choices <- subtypeChoices()
    updateCheckboxGroupInput(
      session,
      "pathwayTabSelectedSubtypes",
      choices = choices,
      selected = choices
    )
  }, ignoreInit = FALSE)
  
  filterPathwayData <- function() {
    data <- finalRxTableDataInput()
    if (is.null(data) || !is.data.frame(data)) return(data.frame())
    
    if (!is.null(input$pathwayTabSelectedOrgans) && input$pathwayTabSelectedOrgans != "All")
      data <- data[data$Organs %in% input$pathwayTabSelectedOrgans, ]
    
    if (!is.null(input$pathwayTabSelectedModality) && input$pathwayTabSelectedModality != "All")
      data <- data[data$Modality %in% input$pathwayTabSelectedModality, ]
    
    subtypes <- input$pathwayTabSelectedSubtypes
    if (is.null(subtypes)) subtypes <- c("All")
    diagType <- input$pathwayTabSelectedDiagnosisType
    if (is.null(diagType)) diagType <- "All"
    
    if (diagType == "All") {
      if (!("All" %in% subtypes))
        data <- data[data$Organs %in% subtypes, ]
    } else if (diagType == "1o & 2o") {
      data <- data[data$Diagnosis1o %in% subtypes | data$Diagnosis2o %in% subtypes, ]
    } else {
      data <- switch(substring(diagType, 1, 1),
                     "P" = data[data$Diagnosis1o %in% subtypes, ],
                     "S" = data[data$Diagnosis2o %in% subtypes, ],
                     "B" = data[data$DiagnosisBn %in% subtypes, ],
                     "U" = data[data$DiagnosisUn %in% subtypes, ],
                     data
      )
    }
    data
  }
  
  finalRxTableDataInput <- reactive({
    switch(input$rxTimesTableRadio,
           "rxdoneTable"      = rxDoneData,
           "rxwaitTable"      = rxWaitData,
           "monthlyWaitTable" = {
             processMonthlyRxWaitingList(input$startDate, input$endDate, input$pathwayTabSelectedOrgans)
             monthlyRxWaitData
           }
    )
  })
  
  observeEvent(input$buttonPasteRxTimesData, {
    if (isTRUE(isDocker)) {
      shinyCatch(
        {
          message("Sorry running in a Docker via Web interface therefore data export functions not available...")
        },
        prefix = ""
      )
    } else {
      copyDataToClipboard(filterPathwayData())
      shinyCatch(
        {
          message("Copied data to the clipboard, please paste into app such as Microsoft Excel on a secure computer (patient IDs included).")
        },
        prefix = ""
      )
    }
  })
  
  observeEvent(input$buttonSaveRxTimesData, {
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
          message("If this is a secure computer (patient IDs included), choose a file to export pathway data to...")
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
            message(paste("Attempting to export pathway data to file", exportFile))
          },
          prefix = ""
        )
        write.csv(filterPathwayData(), exportFile, row.names = TRUE)
        shinyCatch(
          {
            message(paste("Exported pathway data to file", exportFile))
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
  
  observeEvent(input$buttonRefreshTimesData, {
    output$tableRxPathway <- DT::renderDataTable({
      DT::datatable(filterPathwayData())
    })
  })
  
  output$tableRxPathway <- DT::renderDataTable({
    DT::datatable(filterPathwayData())
  })
}
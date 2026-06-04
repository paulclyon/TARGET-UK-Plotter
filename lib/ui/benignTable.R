benignTab <- function() {
  list(
    fluidRow(box(
      width = 12,
      column(
        width = 5,
        selectInput("benignTabSelectedOrgans","Target Organ", choices = organFactors, selected = organFactors[1]),
        selectInput("benignTabSelectedDiagnosisType", "Diagnosis Type", 
                    choices = c("All", "Benign", "Unknown")),
        selectInput("benignTabSelectedModality", "Modality", choices = c("All"))
        ),
      column(
        width = 5,
        checkboxGroupInput(
          "benignTabSelectedSubtypes", "Subtypes",
          choices = diagnosisSubtypeFactors,
          selected = diagnosisSubtypeFactors
        )
      ),
      column(
        width = 2,
        div(
          style = "border:1px solid #ddd;border-radius:8px;padding:2px;background:#f9f9f9;display:flex;flex-direction:column;gap:12px;",
          actionButton("buttonPasteBenignTabData",   "> Clip", width = "100%"),
          actionButton("buttonSaveBenignTabData",    "> File", width = "100%"),
          actionButton("buttonRefreshBenignTabData", "Refresh", width = "100%")
        )
      )
    )),
    fluidRow(box(
      width = 12,
      DT::dataTableOutput("tableBenign"), style = "overflow-y: scroll;overflow-x: scroll;"
    ))
  )
}

benignTableServer <- function(input, output, session, isDocker, api)
{
  refreshTrigger <- reactiveVal(0)
  
  subtypeChoices <- reactive({
    req(input$benignTabSelectedDiagnosisType)
    switch(
      input$benignTabSelectedDiagnosisType,
      "All"       = c("All"),
      "Benign"    = api$diagnosis_bn_Factors,
      api$diagnosisSubtypeFactors
    )
  })
  
  benignDataFiltered <- reactive({
    refreshTrigger()
    data <- benignData
    if (is.null(data) || !is.data.frame(data)) return(data.frame())
    
    if (!is.null(input$benignTabSelectedOrgans) && input$benignTabSelectedOrgans != "All")
      data <- data[data$Organ %in% input$benignTabSelectedOrgans, ]
    
    if (!is.null(input$benignTabSelectedModality) && input$benignTabSelectedModality != "All")
      data <- data[grepl(input$benignTabSelectedModality, data$RxModalities, fixed = TRUE), ]
    
    subtypes <- input$benignTabSelectedSubtypes
    if (is.null(subtypes)) subtypes <- c("All")
    
    diagType <- input$benignTabSelectedDiagnosisType
    if (is.null(diagType)) diagType <- "All"
    
    if (diagType == "Benign") {
      if (!("All" %in% subtypes))
        data <- data[data$DiagnosisBn %in% subtypes, ]
    }
    data
  })
  
  observe({
    choices <- subtypeChoices()
    updateCheckboxGroupInput(
      session, "benignTabSelectedSubtypes",
      choices  = choices,
      selected = choices
    )
  })
  
  observeEvent(input$benignTabSelectedDiagnosisType, {
    choices <- subtypeChoices()
    updateCheckboxGroupInput(
      session,
      "benignTabSelectedSubtypes",
      choices = choices,
      selected = choices
    )
  }, ignoreInit = FALSE)
  
  observe({
    updateSelectInput(session, "benignTabSelectedOrgans", "Target Organ",
                      choices = api$organFactors,
                      selected = api$organFactors[1]
    )
  })
  
  observe({
    req(api$diagnosis_type_Factors)
    updateSelectInput(
      session, "benignTabSelectedDiagnosisType",
      choices = c("All", "Benign"),
      selected = "All"
    )
  })
  
  observe({
    req(api$modalityFactors)
    updateSelectInput(session, "benignTabSelectedModality", "Modality",
                      choices = c("All", api$modalityFactors),
                      selected = "All")
  })
  
  observeEvent(input$buttonPasteBenignTabData, {
    if (isTRUE(isDocker)) {
      shinyCatch(
        {
          message("Sorry running in a Docker via Web interface therefore data export functions not available...")
        },
        prefix = ""
      )
    } else {
      copyDataToClipboard(benignDataFiltered())
      shinyCatch(
        {
          message("Copied data to the clipboard, please paste into app such as Microsoft Excel on a secure computer (patient IDs included).")
        },
        prefix = ""
      )
    }
  })
  
  observeEvent(input$buttonSaveBenignTabData, {
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
            message(paste("Attempting to export benign data to file", exportFile))
          },
          prefix = ""
        )
        write.csv(benignDataFiltered(), exportFile, row.names = TRUE)
        shinyCatch(
          {
            message(paste("Exported benign data to file", exportFile))
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
  
  observeEvent(input$buttonRefreshBenignTabData, {
    refreshTrigger(refreshTrigger() + 1)
  })
  
  output$tableBenign <- DT::renderDataTable({
    refreshTrigger()
    DT::datatable(benignDataFiltered())
  })
}
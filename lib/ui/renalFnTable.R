renalFnTab <- function() {
  tagList(
    fluidRow(
      box(
        width = 12,
        column(
          width = 2
        ),
        column(
          width = 3,
          dateInput("renalFnTabStartDate", "Start Date:", format = "dd/mm/yyyy", value = Sys.Date() - 365.25*10),
          dateInput("renalFnTabEndDate", "End Date:", format = "dd/mm/yyyy", value = Sys.Date()),
        ),
        column(
          width = 5,
          selectInput("renalFnTabSelectedOrgans", "Target Organ", choices = organFactors),
          selectInput(
            "renalFnTabSelectedDiagnosisType",
            "Diagnosis Type",
            choices = c("All", "Primary", "Secondary", "1o & 2o", "Benign", "Unknown")
          ),
          checkboxGroupInput(
            "renalFnTabSelectedSubtypes",
            "Subtypes",
            choices = diagnosisSubtypeFactors,
            selected = diagnosisSubtypeFactors
          )
        ),
        column(
          width = 2,
          div(
            style = "border:1px solid #ddd;border-radius:8px;padding:2px;background:#f9f9f9;display:flex;flex-direction:column;gap:12px;",
            actionButton("buttonPasteRenalFnData", "> Clip", width = "100%"),
            actionButton("buttonSaveRenalFnData", "> File", width = "100%"),
            actionButton("buttonRefreshRenalFn", "Refresh", width = "100%")
          )
        )
      )
    ),
    fluidRow(
      box(
        width = 12,
        DT::dataTableOutput("tableRenalFn"),
        style = "overflow-y: scroll; overflow-x: scroll;"
      )
    )
  )
}

renalFnTableServer <- function(input, output, session, isDocker, api) {
  
  subtypeChoices <- reactive({
    req(input$renalFnTabSelectedDiagnosisType)
    req(input$renalFnTabSelectedOrgans)
    
    all_choices <- switch(
      input$renalFnTabSelectedDiagnosisType,
      "All"       = c("All"),
      "Primary"   = api$diagnosis_1o_Factors,
      "Secondary" = api$diagnosis_2o_Factors,
      "1o & 2o"   = c(api$diagnosis_1o_Factors, api$diagnosis_2o_Factors),
      "Benign"    = api$diagnosis_bn_Factors,
      "Unknown"   = api$diagnosis_un_Factors,
      c("All")
    )
    
    organ <- tolower(input$renalFnTabSelectedOrgans)
    if (organ %in% c("liver", "kidney", "lung")) {
      prefixed <- all_choices[grepl(paste0("^", organ, ":"), tolower(all_choices))]
      if (length(prefixed) > 0) return(prefixed)
    }
    
    all_choices
  })
  
  observe({
    req(api$organFactors)
    updateSelectInput(session, "renalFnTabSelectedOrgans", "Target Organ",
                      choices = api$organFactors,
                      selected = api$organFactors[1]
    )
  })
  
  observe({
    req(api$diagnosis_type_Factors)
    updateSelectInput(
      session, "renalFnTabSelectedDiagnosisType",
      choices = c("All", "Primary", "Secondary", "1o & 2o", "Benign", "Unknown"),
      selected = api$diagnosis_type_Factors[1]
    )
  })
  
  observeEvent(list(input$renalFnTabSelectedDiagnosisType, input$renalFnTabSelectedOrgans), {
    updateCheckboxGroupInput(
      session,
      "renalFnTabSelectedSubtypes",
      choices  = subtypeChoices(),
      selected = subtypeChoices()
    )
  }, ignoreInit = FALSE)
  
  observeEvent(input$buttonPasteRenalFnData, {
    if (isTRUE(isDocker)) {
      shinyCatch(
        {
          message("Sorry running in a Docker via Web interface therefore data export functions not available...")
        },
        prefix = ""
      )
    } else {
      copyDataToClipboard(filterData())
      shinyCatch(
        {
          message("Copied data to the clipboard, please paste into app such as Microsoft Excel on a secure computer (patient IDs included).")
        },
        prefix = ""
      )
    }
  })
  
  observeEvent(input$buttonSaveRenalFnData, {
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
            message(paste("Attempting to export adverse event data to file", exportFile))
          },
          prefix = ""
        )
        write.csv(filterData(), exportFile, row.names = TRUE)
        shinyCatch(
          {
            message(paste("Exported adverse event data to file", exportFile))
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
  
  filterData <- function()
  {
    
    
    if (!is.data.frame(renalFn.df)) {
      return(data.frame())
    }
    
    renalFnData.filtered <- renalFn.df
    
    # Filter by organ
    if (!is.null(input$renalFnTabSelectedOrgans) && input$renalFnTabSelectedOrgans != "All")
      renalFnData.filtered <- renalFnData.filtered[renalFnData.filtered$Organ %in% input$renalFnTabSelectedOrgans, ]
    
    # Filter by diagnosis type and subtypes
    subtypes <- input$renalFnTabSelectedSubtypes
    if (is.null(subtypes)) subtypes <- c("All")
    subtypes <- tolower(subtypes)
    
    diagType <- input$renalFnTabSelectedDiagnosisType
    if (is.null(diagType)) diagType <- "All"
    
    if (diagType == "All") {
      # do nothing
    } else if (diagType == "1o & 2o") {
      renalFnData.filtered <- renalFnData.filtered[
        tolower(renalFnData.filtered$Diagnosis1o) %in% subtypes |
          tolower(renalFnData.filtered$Diagnosis2o) %in% subtypes, ]
    } else {
      renalFnData.filtered <- switch(
        substring(diagType,1,1),
        "P" = renalFnData.filtered[tolower(renalFnData.filtered$Diagnosis1o) %in% subtypes, ],
        "S" = renalFnData.filtered[tolower(renalFnData.filtered$Diagnosis2o) %in% subtypes, ],
        "B" = renalFnData.filtered[tolower(renalFnData.filtered$DiagnosisBn) %in% subtypes, ],
        "U" = renalFnData.filtered[tolower(renalFnData.filtered$DiagnosisUn) %in% subtypes, ],
        renalFnData.filtered
      )
    }
    
    # Filter by date
    startDate <- asDateWithOrigin(input$renalFnTabStartDate)
    endDate <- asDateWithOrigin(input$renalFnTabEndDate)
    if (is.data.frame(renalFnData.filtered) && nrow(renalFnData.filtered) > 0) {
      renalFnData.filtered$Date <- as.Date(renalFnData.filtered$Date, "%d-%m-%Y")
      if (!is.na(startDate) && nrow(renalFnData.filtered) > 0)
        renalFnData.filtered <- renalFnData.filtered %>% filter(Date >= startDate)
      if (!is.na(endDate) && nrow(renalFnData.filtered) > 0)
        renalFnData.filtered <- renalFnData.filtered %>% filter(Date <= endDate)
    }
    return(renalFnData.filtered)
  }
  
  observeEvent(input$buttonRefreshRenalFn, {
    output$tableRenalFn <- DT::renderDataTable({
      DT::datatable(filterData())
    })
  })
  
  output$tableRenalFn <- DT::renderDataTable({
    DT::datatable(filterData())
  })
}
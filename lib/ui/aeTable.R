aeTab <- function() {
  tagList(
    fluidRow(
      box(
        width = 12,
        column(
          width = 2,
          checkboxGroupInput(
            "aeGradesCheckbox",
            "CCTAE Grades",
            choices = cctaeGradeFactors,
            selected = cctaeGradeFactors[grepl("\\b(3|4|5)\\b", as.character(cctaeGradeFactors))],
            inline = TRUE
          )
        ),
        column(
          width = 3,
          dateInput("aeTabStartDate", "Start Date:", format = "dd/mm/yyyy", value = Sys.Date() - 365),
          dateInput("aeTabEndDate", "End Date:", format = "dd/mm/yyyy", value = Sys.Date()),
        ),
        column(
          width = 5,
          selectInput("aeTabSelectedOrgans", "Target Organ", choices = organFactors),
          selectInput(
            "aeTabSelectedDiagnosisType",
            "Diagnosis Type",
            choices = c("All", "Primary", "Secondary", "1o & 2o", "Benign", "Unknown")
          ),
          checkboxGroupInput(
            "aeTabSelectedSubtypes",
            "Subtypes",
            choices = diagnosisSubtypeFactors,
            selected = diagnosisSubtypeFactors
          )
        ),
        column(
          width = 2,
          div(
            style = "border:1px solid #ddd;border-radius:8px;padding:2px;background:#f9f9f9;display:flex;flex-direction:column;gap:12px;",
            actionButton("buttonPasteAEData", "> Clip", width = "100%"),
            actionButton("buttonSaveAEData", "> File", width = "100%"),
            actionButton("buttonRefreshAE", "Refresh", width = "100%")
          )
        )
      )
    ),
    fluidRow(
      box(
        width = 12,
        DT::dataTableOutput("tableAE"),
        style = "overflow-y: scroll; overflow-x: scroll;"
      )
    )
  )
}

aeTableServer <- function(input, output, session, isDocker, api) {
  
  subtypeChoices <- reactive({
    req(input$aeTabSelectedDiagnosisType)
    req(input$aeTabSelectedOrgans)
    
    all_choices <- switch(
      input$aeTabSelectedDiagnosisType,
      "All"       = c("All"),
      "Primary"   = api$diagnosis_1o_Factors,
      "Secondary" = api$diagnosis_2o_Factors,
      "1o & 2o"   = c(api$diagnosis_1o_Factors, api$diagnosis_2o_Factors),
      "Benign"    = api$diagnosis_bn_Factors,
      "Unknown"   = api$diagnosis_un_Factors,
      c("All")
    )
    
    organ <- tolower(input$aeTabSelectedOrgans)
    if (organ %in% c("liver", "kidney", "lung")) {
      prefixed <- all_choices[grepl(paste0("^", organ, ":"), tolower(all_choices))]
      if (length(prefixed) > 0) return(prefixed)
    }
    
    all_choices
  })
  
  observe({
    req(api$organFactors)
    updateSelectInput(session, "aeTabSelectedOrgans", "Target Organ",
                      choices = api$organFactors,
                      selected = api$organFactors[1]
    )
  })
  
  observe({
    req(api$diagnosis_type_Factors)
    updateSelectInput(
      session, "aeTabSelectedDiagnosisType",
      choices = c("All", "Primary", "Secondary", "1o & 2o", "Benign", "Unknown"),
      selected = api$diagnosis_type_Factors[1]
    )
  })
  
  observeEvent(list(input$aeTabSelectedDiagnosisType, input$aeTabSelectedOrgans), {
    updateCheckboxGroupInput(
      session,
      "aeTabSelectedSubtypes",
      choices  = subtypeChoices(),
      selected = subtypeChoices()
    )
  }, ignoreInit = FALSE)
  
  observeEvent(input$buttonPasteAEData, {
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
  
  observeEvent(input$buttonSaveAEData, {
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
  
  filterData <- function() {
    
    # Filter by CCTAE grade
    bad_cols <- is.na(names(aeData)) | names(aeData) == ""
    aeData.filtered <- aeData[, !bad_cols, drop = FALSE]
    aeData.filtered$Grade <- as.character(aeData.filtered$Grade)
    aeData.filtered <- dplyr::filter(
      aeData.filtered,
      .data$Grade %in% input$aeGradesCheckbox
    )
    
    # Filter by organ
    if (!is.null(input$aeTabSelectedOrgans) && input$aeTabSelectedOrgans != "All")
      aeData.filtered <- aeData.filtered[aeData.filtered$Organ %in% input$aeTabSelectedOrgans, ]
    
    # Filter by diagnosis type and subtypes
    subtypes <- input$aeTabSelectedSubtypes
    if (is.null(subtypes)) subtypes <- c("All")
    subtypes <- tolower(subtypes)
    
    diagType <- input$aeTabSelectedDiagnosisType
    if (is.null(diagType)) diagType <- "All"
    
    if (diagType == "All") {
      if (!("all" %in% subtypes))
        aeData.filtered <- aeData.filtered[tolower(as.character(aeData.filtered$Organ)) %in% subtypes, ]
    } else if (diagType == "1o & 2o") {
      aeData.filtered <- aeData.filtered[
        tolower(as.character(aeData.filtered$Diagnosis1o)) %in% subtypes |
          tolower(as.character(aeData.filtered$Diagnosis2o)) %in% subtypes, ]
    } else {
      aeData.filtered <- switch(substring(diagType, 1, 1),
                                "P" = aeData.filtered[tolower(as.character(aeData.filtered$Diagnosis1o)) %in% subtypes, ],
                                "S" = aeData.filtered[tolower(as.character(aeData.filtered$Diagnosis2o)) %in% subtypes, ],
                                "B" = aeData.filtered[tolower(as.character(aeData.filtered$DiagnosisBn)) %in% subtypes, ],
                                "U" = aeData.filtered[tolower(as.character(aeData.filtered$DiagnosisUn)) %in% subtypes, ],
                                aeData.filtered)
    }
    
    # Filter by date
    startDate <- asDateWithOrigin(input$aeTabStartDate)
    endDate <- asDateWithOrigin(input$aeTabEndDate)
    if (nrow(aeData.filtered) > 0) {
      aeData.filtered$DateofOnset <- as.Date(aeData.filtered$DateofOnset, "%d-%m-%Y")
      aeData.filtered$DateofResolution <- as.Date(aeData.filtered$DateofResolution, "%d-%m-%Y")
      if (!is.na(startDate) && nrow(aeData.filtered) > 0)
        aeData.filtered <- aeData.filtered %>% filter(DateofOnset > startDate)
      if (!is.na(endDate) && nrow(aeData.filtered) > 0)
        aeData.filtered <- aeData.filtered %>% filter(DateofOnset < endDate)
    }
    return(aeData.filtered)
  }
  
  observeEvent(input$buttonRefreshAE, {
    output$tableAE <- DT::renderDataTable({
      DT::datatable(filterData())
    })
  })
  
  output$tableAE <- DT::renderDataTable({
    DT::datatable(filterData())
  })
}
# Report on renal function post ablation
renalFunctionReportTab <- function(id = NULL) {
  # if using as a module call NS for ids; otherwise leave as-is
  ns <- if (!is.null(id)) shiny::NS(id) else identity
  
  list(
    tags$head(
      tags$link(rel = "stylesheet", href = "styles.css")
    ),
    fluidRow(tabPanel(
      "RenalFnReport",
      column(
        width = 3,
        dateInput(
          "RenalFnReportDate1",
          "Treatment Start Date:",
          format = "dd/mm/yyyy",
          value = Sys.Date() - 365.25*10
        ),
        dateInput(
          "RenalFnReportDate2",
          "Treatment End Date:",
          format = "dd/mm/yyyy",
          value = Sys.Date()
        ),
        div(
          style = "display:flex; gap:10px;",
          numericInput("minMonthsFollowup", "MinFU(m)", value = 1, min = 1, max = 24, width = "60px"),
          numericInput("maxMonthsFollowup", "MaxFU(m)", value = 36, min = 1, max = 20, width = "60px")
        ),
        checkboxInput("renalFnReportAKIBin", "AKI (0-2m)", value = TRUE),
      ),
      column(
        width = 3,
        selectInput(
          "RenalFnReportSelectedOrgans",
          "Organ",
          choices = character(0),
          selected = NULL
        ),
        selectInput("RenalFnReportSelectedDiagnosisType", "Diagnosis Type", choices = diagnosis_type_Factors),
        sliderInput(
          "RenalFnReportTumourSizeRange",
          "Tumour Size (mm)",
          min = 0, max = 100, value = c(0, 100), step = 1, ticks = TRUE
        )
      ),
      column(
        width = 3,
        checkboxGroupInput(
          ns("RenalFnReportSelectedSubtypes"),
          "Subtypes",
          choices = character(0),
          selected = character(0)
        ),
        checkboxGroupInput(
          ns("RenalFnReportSelectedModality"),
          "Modality",
          choices = character(0),
          selected = character(0)
        )
      ),
      column(
        width = 3,
        checkboxInput("renalFnReportAnonymised", "Anonymise Report", value = TRUE),
        checkboxInput("renalFnReportIndividualPtPlots", "Individual Patient Plots", value = FALSE),
        div(
          class = "report-buttons",
          actionButton(ns("buttonRunRenalFnReport"), "Generate Report", class = "btn-primary"),
          downloadButton(ns("buttonRenalFnReportToPDF"), "Report to PDF"),
          downloadButton(ns("buttonRenalFnReportToDoc"), "Report to Doc")
        )
      )
    )),
    wellPanel(
      style = "background: white",
      fluidRow(fluidPage(
        htmlOutput("summaryRenalFnReport")
      ))
    )
  )
}

renalFunctionReportServer <- function(input, output, session, api, plots)
{
  rmdRenalFnReportFile <- c(Sys.getenv("RENAL_FUNCTION_REPORT_RMD"))
  mdRenalFnReportFile <- c(Sys.getenv("RENAL_FUNCTION_REPORT_MD"))
  currentReportParams <- reactiveVal(NULL)
  
  subtypeChoices <- reactive({
    req(input$RenalFnReportSelectedDiagnosisType)
    req(input$RenalFnReportSelectedOrgans)
    
    all_choices <- switch(
      input$RenalFnReportSelectedDiagnosisType,
      "All"       = c("All"),
      "Primary"   = api$diagnosis_1o_Factors,
      "Secondary" = api$diagnosis_2o_Factors,
      "1o & 2o"   = unique(c(api$diagnosis_1o_Factors, api$diagnosis_2o_Factors)),
      "Unknown"   = api$diagnosis_un_Factors,
      c("All")
    )
    
    organ <- tolower(trimws(input$RenalFnReportSelectedOrgans))
    if (organ %in% c("liver", "kidney", "lung")) {
      prefixed <- all_choices[grepl(paste0("^", organ, "\\s*:"), tolower(all_choices))]
      if (length(prefixed) > 0) return(prefixed)
    }
    all_choices
  })
  
  selectedSubtypes <- reactive({
    subtypeChoices()
  })
  
  modalityInitialised <- reactiveVal(FALSE)
  observe({
    if (modalityInitialised()) return()
    req(api$modalityFactors)
    modalityChoices <- setdiff(api$modalityFactors, "All")
    updateCheckboxGroupInput(
      session,
      "RenalFnReportSelectedModality",
      choices  = c("All", modalityChoices),
      selected = "All"
    )
    modalityInitialised(TRUE)
  })
  
  observeEvent(list(input$RenalFnReportSelectedDiagnosisType, input$RenalFnReportSelectedOrgans), {
    updateCheckboxGroupInput(
      session,
      "RenalFnReportSelectedSubtypes",
      choices  = subtypeChoices(),
      selected = selectedSubtypes()
    )
  }, ignoreInit = FALSE)
  
  finalRefReportInput <- reactive({
    tryCatch(
      {
        # Clear the old HTML file if it exists...
        if (file.exists(mdRenalFnReportFile)) {
          showNotification(paste("Cleaning out the old HTML file '", mdRenalFnReportFile, "'", sep = ""))
          file.remove(mdRenalFnReportFile)
        }
        
        # This does the knitting bit ready to make the HTML by running the knit function
        # This makes the MD file which is basically just HTML in a file, which we then return as includeMarkdown()
        sapply(rmdRenalFnReportFile, knit, quiet = T)
        htmltools::includeMarkdown(mdRenalFnReportFile)
      },
      error = function(errorMessage) {
        logger(conditionMessage(errorMessage), T)
        showNotification(conditionMessage(errorMessage))
        # Choose a return value in case of error
        NA
      }
    )
  })
  
  observeEvent(api$cancerOrganFactors, {
    req(!is.null(api$cancerOrganFactors))
    req(length(api$cancerOrganFactors) > 0)
    
    updateSelectInput(
      session,
      "RenalFnReportSelectedOrgans",
      choices = c("All", api$cancerOrganFactors),
      selected = "All"
    )
  })
  
  # Builds the report params list straight from the current input values.
  # Used both when "Cancer Outcomes" is clicked, and as a fallback in the
  # PDF/Doc download handlers so they work even if that button hasn't been
  # clicked yet. Wrapped in isolate() so it's safe to call from a
  # non-reactive context (e.g. inside a downloadHandler's content function).
  buildRenalFnReportParamsFromInputs <- function() {
    isolate(list(
      report_start_date               = input$RenalFnReportDate1,
      report_end_date                 = input$RenalFnReportDate2,
      report_organs                   = input$RenalFnReportSelectedOrgans,
      report_diagnosis_type           = input$RenalFnReportSelectedDiagnosisType,
      report_subtypes                 = input$RenalFnReportSelectedSubtypes,
      report_modality                 = input$RenalFnReportSelectedModality,
      report_min_tumour_size          = input$RenalFnReportTumourSizeRange[1],
      report_max_tumour_size          = input$RenalFnReportTumourSizeRange[2],
      report_min_months_followup      = input$minMonthsFollowup,
      report_max_months_followup      = input$maxMonthsFollowup,
      report_include_aki_bin          = input$renalFnReportAKIBin,
      report_renal_fn_anonymised      = input$renalFnReportAnonymised,
      report_renal_fn_pt_plots        = input$renalFnReportIndividualPtPlots
      
    ))
  }
  
  observe({
    sizes <- as.numeric(api$cancerPerPatientData$MaxTumourSize)
    sizes <- sizes[!is.na(sizes)]
    if (length(sizes) > 0) {
      dataMin <- floor(min(sizes))
      dataMax <- ceiling(max(sizes))
      updateSliderInput(
        session,
        "RenalFnReportTumourSizeRange",
        min   = dataMin,
        max   = dataMax,
        value = c(dataMin, dataMax)
      )
    }
  })
  
  observeEvent(input$buttonRunRenalFnReport, {
    currentReportParams(buildRenalFnReportParamsFromInputs())
    plots$activePlot <- NA
  })
  
  output$summaryRenalFnReport <- renderUI({
    req(input$buttonRunRenalFnReport > 0)
    params <- currentReportParams()
    req(params)
    
    shinyCatch(
      message(paste0("Generating the report from file '", Sys.getenv("RENAL_FUNCTION_REPORT_RMD"), "'")),
      prefix = ""
    )
    
    tempReportDir <- tempdir()
    tempRmd <- file.path(tempReportDir, "report.Rmd")
    file.copy(rmdRenalFnReportFile, tempRmd, overwrite = TRUE)
    
    # Render to HTML
    render_result <- tryCatch({
      rmarkdown::render(tempRmd,
                        output_format = "html_document",
                        output_dir = tempReportDir,
                        params = params,
                        envir = new.env(parent = globalenv()),
                        quiet = TRUE)
    }, error = function(e) { 
      logger(conditionMessage(e), TRUE); 
      showNotification(conditionMessage(e)); 
      return(NULL)
    })
    
    if (is.null(render_result) || !file.exists(render_result)) {
      return(tags$div("Failed to render report - see notifications for details."))
    }
    
    # Read the rendered HTML and embed in an iframe via srcdoc (avoids includeHTML warning)
    html_text <- paste(readLines(render_result, warn = FALSE), collapse = "\n")
    
    tags$iframe(
      srcdoc = html_text,
      width = "100%",
      height = "900px",
      style = "border:0;")
  })
  
  output$buttonRenalFnReportToDoc <- downloadHandler(
    filename = Sys.getenv("RENAL_FUNCTION_REPORT_PATHWAY_DOC"),
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReportDir <- tempdir()
      
      tempReport <- file.path(tempReportDir, "report.Rmd")
      file.copy(rmdRenalFnReportFile, tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document.
      # If "Renal Function" hasn't been clicked yet, currentReportParams()
      # will still be NULL - fall back to building params from the current
      # filter inputs so the download works regardless.
      params <- isolate(currentReportParams())
      if (is.null(params)) {
        params <- buildRenalFnReportParamsFromInputs()
      }
      req(params)
      
      shinyCatch(
        message(paste(
          "Generating the report from file '", Sys.getenv("RENAL_FUNCTION_REPORT_RMD"),
          sep = ""
        )),
        prefix = ""
      )
      
      rmarkdown::render(tempReport,
                        output_format = "html_document",
                        output_dir = tempReportDir,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
      
      shinyCatch(
        message("Converting to docx"),
        prefix = ""
      )
      pandoc::pandoc_convert(file = file.path(tempReportDir, "report.html"), from = "html", to = "docx", output = file)
      shinyCatch(
        message(paste("Created '",Sys.getenv("RENAL_FUNCTION_REPORT_PATHWAY_DOC"),"'")),
        prefix = ""
      )
    }
  )
  
  output$buttonRenalFnReportToPDF <- downloadHandler(
    filename = Sys.getenv("RENAL_FUNCTION_REPORT_PATHWAY_PDF"),
    content = function(file) {
      tempReportDir <- tempdir()
      tempReport <- file.path(tempReportDir, "report.Rmd")
      file.copy(rmdRenalFnReportFile, tempReport, overwrite = TRUE)
      
      # If "Cancer Outcomes" hasn't been clicked yet, currentReportParams()
      # will still be NULL - fall back to building params from the current
      # filter inputs so the download works regardless.
      params <- isolate(currentReportParams())
      if (is.null(params)) {
        params <- buildRenalFnReportParamsFromInputs()
      }
      req(params)
      
      shinyCatch(message(paste0("Generating report from '", Sys.getenv("RENAL_FUNCTION_REPORT_RMD"), "'")), prefix = "")
      
      html_file <- rmarkdown::render(
        tempReport,
        output_format = "html_document",
        output_dir    = tempReportDir,
        params        = params,
        envir         = new.env(parent = globalenv())
      )
      
      shinyCatch(message("Converting to PDF via Chrome"), prefix = "")
      
      # pagedown uses Chrome's print engine - fully respects CSS @media print
      pagedown::chrome_print(
        input  = html_file,
        output = file,
        options = list(
          paperWidth  = 8.27,   # A4 portrait width
          paperHeight = 11.69,  # A4 portrait height
          marginTop    = 0.4,
          marginBottom = 0.4,
          marginLeft   = 0.4,
          marginRight  = 0.4,
          printBackground   = TRUE,
          preferCSSPageSize = TRUE
        )
      )
      shinyCatch(message(paste("Created '",Sys.getenv("RENAL_FUNCTION_REPORT_PATHWAY_PDF"),"'")), prefix = "")
    }
  )
}
# Audit of the time to treatment referral pathway
auditRxPathwayTab <- function(id = NULL) {
  # if using as a module call NS for ids; otherwise leave as-is
  ns <- if (!is.null(id)) shiny::NS(id) else identity
  
  list(
    tags$head(
      tags$link(rel = "stylesheet", href = "styles.css")
    ),
    fluidRow(tabPanel(
      "AuditPathway",
      column(
        width = 3,
        dateInput(
          "auditDate1",
          "Start Date:",
          format = "dd/mm/yyyy",
          value = Sys.Date() - 365
        ),
        dateInput(
          "auditDate2",
          "End Date:",
          format = "dd/mm/yyyy",
          value = Sys.Date()
        )
      ),
      column(
        width = 3,
        checkboxGroupInput(
          "organAuditCheckbox",
          "Organs to Audit",
          choices = organFactors,
          selected = organFactors
        ),
      ),
      column(
        width = 3,
        div(
          class = "report-buttons",
          actionButton(ns("buttonRunAuditReport"), "Run Audit Report", class = "btn-primary"),
          downloadButton(ns("buttonAuditToPDF"), "Report to PDF"),
          downloadButton(ns("buttonAuditToDoc"), "Report to Doc")
        )
      )
    )),
    wellPanel(
      style = "background: white",
      fluidRow(fluidPage(
        htmlOutput("summaryRefAudit")
      ))
    )
  )
}

auditServer <- function(input, output, session, api, plots) {
  rmdAuditFile <- c(Sys.getenv("AUDIT_PATHWAY_RMD"))
  mdAuditFile <- c(Sys.getenv("AUDIT_PATHWAY_MD"))

  finalRefAuditInput <- reactive({
    tryCatch(
      {
        # Clear the old HTML file if it exists...
        if (file.exists(mdAuditFile)) {
          showNotification(paste("Cleaning out the old HTML file '", mdAuditFile, "'", sep = ""))
          file.remove(mdAuditFile)
        }

        # This does the knitting bit ready to make the HTML by running the knit function
        # This makes the MD file which is basically just HTML in a file, which we then return as includeMarkdown()
        sapply(rmdAuditFile, knit, quiet = T)
        htmltools::includeMarkdown(mdAuditFile)
      },
      error = function(errorMessage) {
        logger(conditionMessage(errorMessage), T)
        showNotification(conditionMessage(errorMessage))
        # Choose a return value in case of error
        NA
      }
    )
  })

  observe({
    updateCheckboxGroupInput(session, "organAuditCheckbox", "Organs to Audit",
      choices = api$organFactors,
      selected = api$organFactors
    )
  })

  observeEvent(input$buttonRunAuditReport, {
  plots$activePlot <- NA

  output$summaryRefAudit <- renderUI({
    # Informational message (does not affect UI)
    shinyCatch(
      message(paste0("Generating the audit from file '", Sys.getenv("AUDIT_PATHWAY_RMD"), "'")),
      prefix = ""
    )

    # Render into a temporary dir so we don't overwrite app files
    tempReportDir <- tempdir()
    tempRmd <- file.path(tempReportDir, "report.Rmd")
    file.copy(rmdAuditFile, tempRmd, overwrite = TRUE)

    params <- list(
      audit_start_date = input$auditDate1,
      audit_end_date   = input$auditDate2,
      audit_organs     = input$organAuditCheckbox
    )

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
      return(tags$div("Failed to render audit - see notifications for details."))
    }

    # Read the rendered HTML and embed in an iframe via srcdoc (avoids includeHTML warning)
    html_text <- paste(readLines(render_result, warn = FALSE), collapse = "\n")

    tags$iframe(
      srcdoc = html_text,
      width = "100%",
      height = "900px",
      style = "border:0;")
    })
  })

  output$buttonAuditToDoc <- downloadHandler(
    filename = "targetuk_waiting_time_audit_report.doc",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReportDir <- tempdir()

      tempReport <- file.path(tempReportDir, "report.Rmd")
      file.copy(rmdAuditFile, tempReport, overwrite = TRUE)

      # Set up parameters to pass to Rmd document
      params <- list(
        audit_start_date = input$auditDate1,
        audit_end_date = input$auditDate2,
        audit_organs = input$organAuditCheckbox
      )

      shinyCatch(
        message(paste(
          "Generating the audit from file '", Sys.getenv("AUDIT_PATHWAY_RMD"),
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
        message("Created \"targetuk_waiting_time_audit_report.doc\""),
        prefix = ""
      )
    }
  )

  output$buttonAuditToPDF <- downloadHandler(
    filename = "targetuk_waiting_time_audit_report.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReportDir <- tempdir()

      tempReport <- file.path(tempReportDir, "report.Rmd")
      file.copy(rmdAuditFile, tempReport, overwrite = TRUE)

      # Set up parameters to pass to Rmd document
      params <- list(
        audit_start_date = input$auditDate1,
        audit_end_date = input$auditDate2,
        audit_organs = input$organAuditCheckbox
      )

      shinyCatch(
        message(paste(
          "Generating the audit from file '", Sys.getenv("AUDIT_PATHWAY_RMD"),
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
        message("Converting to pdf"),
        prefix = ""
      )
      pandoc::pandoc_convert(file = file.path(tempReportDir, "report.html"), from = "html", to = "pdf", output = file)
      shinyCatch(
        message("Created \"targetuk_waiting_time_audit_report.pdf\""),
        prefix = ""
      )
    }
  )
}

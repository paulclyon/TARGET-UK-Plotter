auditTab <- function() {
  list(
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
        actionButton(inputId = "buttonRunAuditReport", label = "Run Audit Report"),
        downloadButton(outputId = "buttonAuditToPDF", label = "Report to PDF"),
        downloadButton(outputId = "buttonAuditToDoc", label = "Report to Doc")
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
    updateCheckboxGroupInput(session, "organAuditCheckbox", "Organs to Chart",
      choices = api$organFactors,
      selected = api$organFactors
    )
  })

  observeEvent(input$buttonRunAuditReport, {
    plots$activePlot <- NA

    # Elegant way to regenerate the HTML such that is does allow dynamic refresh
    output$summaryRefAudit <- renderUI({
      shinyCatch(
        {
          message(paste(
            "Generating the audit from file '", Sys.getenv("AUDIT_PATHWAY_RMD"),
            "' to file '", Sys.getenv("AUDIT_PATHWAY_MD"), "'",
            sep = ""
          ))
        },
        prefix = ""
      )
      tryCatch(
        {
          # Could also set knit_root_dir parameter but it is probably more trouble than it is worth
          # The default is the directory of the input file .Rmd which works fine, and if specified,
          # is relative to that dir
          includeHTML(rmarkdown::render(rmdAuditFile,
            params = list(
              audit_start_date = input$auditDate1,
              audit_end_date = input$auditDate2,
              audit_organs = input$organAuditCheckbox
            ),
            envir = new.env(parent = globalenv()),
            output_dir = Sys.getenv("REPORT_OUTPUT_DIR")
          ))
        },
        error = function(errorMessage) {
          logger(conditionMessage(errorMessage), T)
          showNotification(conditionMessage(errorMessage))
          errorMessage # Return value in case of error
        }
      )
    })

    # This is another way to do the magic - embed the output into the observe event to allow refresh!
    # But doesn't allow multiple refreshes, see the more simple elegant solution I used
    # output$summaryRefAudit2 <- renderPrint({
    #  if (api$connected == T && api$loaded == T)
    #  {
    #    showNotification(paste("Generating the audit from file '",Sys.getenv("AUDIT_PATHWAY_RMD"),"'",sep=""))
    #    thisHTML <- finalRefAuditInput()
    #    showNotification(paste("Audit generation completed, see file '",Sys.getenv("AUDIT_PATHWAY_MD"),"'",sep=""))
    #  }
    #  else
    #  {
    #    thisHTML <-
    #      "There is no study data loaded at present - cannot run the audit"
    #  }
    #  thisHTML
    # })
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

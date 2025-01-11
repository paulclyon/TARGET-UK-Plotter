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
        actionButton(inputId = "buttonAuditToPDF",     label = "Report to PDF")
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
  
  finalRefAuditInput <- reactive({
    # This does the knitting bit ready to make the HTML by running the knit function
    sapply(rmdAuditFile, knit, quiet = T)

    # This makes the MD file which is basically just HTML in a file
    htmltools::includeMarkdown(Sys.getenv("AUDIT_PATHWAY_MD"))
  })

  observe({
      updateCheckboxGroupInput(session, "organAuditCheckbox", "Organs to Chart",
        choices = api$organFactors,
        selected = api$organFactors
      )
  })

  observeEvent(input$buttonRunAuditReport, {
    plots$activePlot <- NA

    # This is a bit of an ugly hack to allow markdown to see global vars but it doesn't appear to work FIXME
    audit_start_date <<- input$auditDate1
    audit_end_date   <<- input$auditDate2
    audit_organs     <<- input$organAuditCheckbox

    # This is the magic - embed the output into the observe event to allow refresh!
    # So simple but still not quite working - maybe make something reactive ... keep working Paul
    output$summaryRefAudit <- renderPrint({
      if (api$connected == T && api$loaded == T)
      {
        thisHTML <- finalRefAuditInput()
      }
      else
      {
        thisHTML <-
          "There is no study data loaded at present - cannot run the audit"
      }
      thisHTML
    })
  })
  
  observeEvent(input$buttonAuditToPDF, {
    if (isDocker == T)
    {
      shinyCatch({
        message("Sorry running in a Docker via Web interface therefore data export functions not available...")
      }, prefix = '')
    }
    else
    {
      exportFile <- NA
      shinyCatch({
        message("If this is a secure computer (patient IDs included), choose a PDF file to export to...")
      }, prefix = "")
      result = tryCatch({ exportFile <- svDialogs::dlg_save(title = "Save R script to", default="targetuk_waiting_time_audit_report.pdf") }, error = function(err) { logger(err,F) })
      if (length(exportFile$res >0) && !is.na(exportFile$res) && exportFile$res != "")
      {
        if (!endsWith(exportFile$res, ".pdf")) {
          exportFile <- paste(exportFile$res, ".pdf", sep = "")
        }
        shinyCatch({
          message(paste("Attempting to export PDF to file (make take some time if first PDF export)...", exportFile$res))
        }, prefix = "")
        rmarkdown::render(rmdAuditFile,"pdf_document", output_file=exportFile$res, quiet=FALSE)
        shinyCatch({
          message(paste("Exported PDF to file", exportFile$res))
        }, prefix = "")
      }
      else
      {
        shinyCatch({
          message(paste("No file selected to export to, no PDF export performed"))
        }, prefix = "")
      }
    }
  })
  
}

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

auditServer <- function(input, output, session, api, plots)
{
  rmdAuditFile <- c(Sys.getenv("AUDIT_PATHWAY_RMD"))
  mdAuditFile <- c(Sys.getenv("AUDIT_PATHWAY_MD"))
  
  finalRefAuditInput <- reactive({
    tryCatch(
    {
      # Clear the old HTML file if it exists...
      if (file.exists(mdAuditFile))
      {
        showNotification(paste("Cleaning out the old HTML file '",mdAuditFile,"'",sep=""))
        file.remove(mdAuditFile)
      }
      
      # This does the knitting bit ready to make the HTML by running the knit function
      # This makes the MD file which is basically just HTML in a file, which we then return as includeMarkdown()
      sapply(rmdAuditFile, knit, quiet = T)
      htmltools::includeMarkdown(mdAuditFile)
    },
    error = function(errorMessage) {
      logger(conditionMessage(errorMessage),T)
      showNotification(conditionMessage(errorMessage))
      # Choose a return value in case of error
      NA
    })
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

    # Elegant way to regenerate the HTML such that is does allow dynamic refresh
    output$summaryRefAudit <- renderUI({
      shinyCatch({
        message(paste("Generating the audit from file '",Sys.getenv("AUDIT_PATHWAY_RMD"),"' to file '",Sys.getenv("AUDIT_PATHWAY_MD"),"'",sep=""))
      }, prefix = "")
      tryCatch(
      {
        # Could also set knit_root_dir parameter but it is probably more trouble than it is worth
        # The default is the directory of the input file .Rmd which works fine, and if specified, is relative to that dir
        includeHTML(rmarkdown::render(rmdAuditFile, output_dir=Sys.getenv("REPORT_OUTPUT_DIR")))
      },
      error = function(errorMessage) {
        logger(conditionMessage(errorMessage),T)
        showNotification(conditionMessage(errorMessage))
        errorMessage # Return value in case of error
      })
    })
    
    # This is another way to do the magic - embed the output into the observe event to allow refresh!
    # But doesn't allow multiple refreshes, see the more simple elegant solution I used
    #output$summaryRefAudit2 <- renderPrint({
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
    #})
  })
  
  observeEvent(input$buttonAuditToPDF, {
    if (isDocker == T)
    {
      shinyCatch({
        message("Sorry running in a Docker via Web interface therefore audit PDF export function not available...")
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
        if (file.access(exportFile$res, mode=2) != -1)  # 0 success i.e. writable (mode 2) / -1 failure
        {
          # Could also set knit_root_dir parameter but it is probably more trouble than it is worth
          # The default is the directory of the input file .Rmd which works fine, and if specified, is relative to that dir
          rmarkdown::render(rmdAuditFile,"pdf_document", output_dir=Sys.getenv("REPORT_OUTPUT_DIR"), output_file=exportFile$res, quiet=FALSE)
          shinyCatch({
            message(paste("Exported PDF to file", exportFile$res))
          }, prefix = "")          
        }
        else
        {
          shinyCatch({
            message(paste("Cannot write access file '", exportFile$res,"' for writing to...",sep=""))
          }, prefix = "")   
        }
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

# Report of the referred patients waiting for treatment 
reportWaitingListTab <- function() {
  list(
    fluidRow(tabPanel(
      "ReportWaitingList",
      column(
        width = 3,
        dateInput(
          "reportDate0",
          "TCI Start:",
          format = "dd/mm/yyyy",
          value = Sys.Date()
        ),
        dateInput(
          "reportDate1",
          "Earliest Referral Cut-off:",
          format = "dd/mm/yyyy",
          value = Sys.Date() - 365
        ),
        dateInput(
          "reportDate2",
          "Latest Referral Cut-off:",
          format = "dd/mm/yyyy",
          value = Sys.Date() + 365
        )
      ),
      column(
        width = 3,
        checkboxGroupInput(
          "organReportCheckbox",
          "Organs to Report",
          choices = organFactors,
          selected = organFactors
        ),
      ),
      column(
        width = 3,
        actionButton(inputId = "buttonRunReport", label = "Run Report"),
        downloadButton(outputId = "buttonReportToPDF", label = "Report to PDF"),
        downloadButton(outputId = "buttonReportToDoc", label = "Report to Doc")
      )
    )),
    wellPanel(
      style = "background: white",
      fluidRow(fluidPage(
        htmlOutput("summaryWaitReport")
      ))
    )
  )
}

reportServer <- function(input, output, session, api, plots)
{
  rmdReportFile <- c(Sys.getenv("REPORT_WAITING_LIST_RMD"))
  mdReportFile <- c(Sys.getenv("REPORT_WAITING_LIST_MD"))

  finalRefReportInput <- reactive({
    tryCatch(
      {
        # Clear the old HTML file if it exists...
        if (file.exists(mdReportFile)) {
          showNotification(paste("Cleaning out the old HTML file '", mdReportFile, "'", sep = ""))
          file.remove(mdReportFile)
        }

        # This does the knitting bit ready to make the HTML by running the knit function
        # This makes the MD file which is basically just HTML in a file, which we then return as includeMarkdown()
        sapply(rmdReportFile, knit, quiet = T)
        htmltools::includeMarkdown(mdReportFile)
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
    updateCheckboxGroupInput(session, "organReportCheckbox", "Organs to Report",
      choices = api$organFactors,
      selected = api$organFactors
    )
  })

  observeEvent(input$buttonRunReport, {
    plots$activePlot <- NA

    # Elegant way to regenerate the HTML such that is does allow dynamic refresh
    output$summaryWaitReport <- renderUI({
      shinyCatch(
        {
          message(paste(
            "Generating the report from file '", Sys.getenv("REPORT_WAITING_LIST_RMD"),
            "' to file '", Sys.getenv("REPORT_WAITING_LIST_MD"), "'",
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

          includeHTML(rmarkdown::render(rmdReportFile,
            params = list(
              tci_start_date = input$reportDate0,
              first_ref_date = input$reportDate1,
              last_ref_date  =  input$reportDate2,
              report_organs  = input$organReportCheckbox
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
  })

  output$buttonReportToDoc <- downloadHandler(
    filename = "targetuk_waiting_list_report.doc",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReportDir <- tempdir()

      tempReport <- file.path(tempReportDir, "report.Rmd")
      file.copy(rmdReportFile, tempReport, overwrite = TRUE)

      # Set up parameters to pass to Rmd document
      params <- list(
        tci_start_date = input$reportDate0,
        first_ref_date = input$reportDate1,
        last_ref_date  = input$reportDate2,
        report_organs  = input$organReportCheckbox
      )

      shinyCatch(
        message(paste(
          "Generating the report from file '", Sys.getenv("REPORT_WAITING_LIST_RMD"),
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
        message(paste("Created '",file,"'"),sep=""),
        prefix = ""
      )
    }
  )

  output$buttonReportToPDF <- downloadHandler(
    filename = "targetuk_waiting_list_report.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReportDir <- tempdir()

      tempReport <- file.path(tempReportDir, "report.Rmd")
      file.copy(rmdReportFile, tempReport, overwrite = TRUE)

      # Set up parameters to pass to Rmd document
      params <- list(
        tci_start_date = input$reportDate0,
        first_ref_date = input$reportDate1,
        last_ref_date  = input$reportDate2,
        report_organs  = input$organReportCheckbox
      )

      shinyCatch(
        message(paste(
          "Generating the report from file '", Sys.getenv("REPORT_WAITING_LIST_RMD"),
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
      # Note the options are used to convert to landscape as the table has a lot of columns!
      # Getting the table to wrap is tough and so the columns 50 sets an upper bound of character length
      # Also note the hack to remove the title, as otherwise it duplicates twice as a header also and fills the page
      # The margin is minimal to fill the page with the referral table also
      rmarkdown::pandoc_convert(
                             file.path(tempReportDir, "report.html"),
                             from = "html", to = "pdf", output = file,
                             options = c("-V", "geometry:portrait", 
                                         "-V", "title=",
                                         "-V", "geometry:margin=0.5in"
                                         )
                             )
      shinyCatch(
        message(paste("Created '",file,"'"),sep=""),
        prefix = ""
      )
    }
  )
}

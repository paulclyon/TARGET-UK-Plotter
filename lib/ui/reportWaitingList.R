# Improved UI function (if used as a plain UI piece)
reportWaitingListTab <- function(id = NULL) {
  # if using as a module call NS for ids; otherwise leave as-is
  ns <- if (!is.null(id)) shiny::NS(id) else identity
  
  list(
    tags$head(
      tags$style(HTML("
        .box .content { padding: 6px 12px; }
        
        #organReportCheckbox > label {
          margin-bottom: 12px;
          display: block;
        }
        #organReportCheckbox .shiny-options-group {
          display: grid;
          grid-template-columns: repeat(2, 1fr);
          gap: 6px 12px;
          align-items: center;
        }
        #organReportCheckbox .shiny-options-group .checkbox { margin: 0; }
        #organReportCheckbox .shiny-options-group input[type='checkbox'] {
          vertical-align: middle; margin-right: 6px;
        }
        @media (max-width: 600px) {
          #organReportCheckbox .shiny-options-group { grid-template-columns: 1fr; }
        }
      "))
    ),
    
    fluidRow(
      column(
        width = 3,
        dateInput(ns("reportDate0"), "TCI Start:", format = "dd/mm/yyyy", value = Sys.Date()),
        dateInput(ns("reportDate1"), "From Earliest Referral:", format = "dd/mm/yyyy", value = Sys.Date() - 365),
        dateInput(ns("reportDate2"), "Until Latest Referral:", format = "dd/mm/yyyy", value = Sys.Date() + 365)
      ),
      column(
        width = 6,
        checkboxGroupInput(ns("organReportCheckbox"), "Organs to Report",
                           choices = NULL, selected = NULL) # choices set by server (api)
      ),
      column(
        width = 3,
        actionButton(inputId = ns("buttonRunReport"), label = "Run Report"),
        downloadButton(outputId = ns("buttonReportToPDF"), label = "Save PDF"),
        downloadButton(outputId = ns("buttonReportToHTML"), label = "Save HTML")
      )
    ),
    
    fluidRow(
      wellPanel(style = "background: white",
                htmlOutput(ns("summaryWaitReport"))
      )
    )
  )
}

# Server: reportServer (improved)
reportServer <- function(input, output, session, api, plots)
{
  # ensure env vars: single path strings
  rmdReportFile <- Sys.getenv("REPORT_WAITING_LIST_RMD")
  mdReportFile  <- Sys.getenv("REPORT_WAITING_LIST_MD")
  outputDir     <- Sys.getenv("REPORT_OUTPUT_DIR", unset = tempdir())
  
  # defensive checks
  if (!nzchar(rmdReportFile) || !file.exists(rmdReportFile)) {
    warning("REPORT_WAITING_LIST_RMD not set or file not found: ", rmdReportFile)
  }
  
  # keep checkbox choices in sync if api has organFactors
  observe({
    if (!is.null(api) && !is.null(api$organFactors)) {
      updateCheckboxGroupInput(session, "organReportCheckbox", "Organs to Report",
                               choices = api$organFactors, selected = api$organFactors)
    }
  })
  
  # Create a reactive that only updates when button is clicked
  # eventReactive does the heavy lifting and shows progress while doing it
  report_html <- eventReactive(input$buttonRunReport, {
    req(input$reportDate0, input$reportDate1, input$reportDate2, rmdReportFile)
    
    params_now <- make_params()
    
    # isolated temp dir for this run
    tempReportDir <- tempfile("report_render_")
    dir.create(tempReportDir, recursive = TRUE)
    # ensure cleanup when this reactive is re-run / session ends
    # note: on.exit inside eventReactive will run when this call finishes,
    # so we schedule cleanup via a session$onSessionEnded handler as a backup
    session$onSessionEnded(function() if (dir.exists(tempReportDir)) unlink(tempReportDir, recursive = TRUE))
    
    tempReport <- file.path(tempReportDir, "report.Rmd")
    file.copy(rmdReportFile, tempReport, overwrite = TRUE)
    
    # show progress during the render
    withProgress(message = "Rendering report...", value = 0, {
      incProgress(0.05, detail = "Preparing files...")
      Sys.sleep(0.05) # tiny pause to let UI draw (optional)
      
      incProgress(0.15, detail = "Rendering R Markdown (this may take a moment)...")
      result_html <- tryCatch({
        rmarkdown::render(tempReport,
                          output_format = "html_document",
                          output_dir = tempReportDir,
                          params = params_now,
                          envir = new.env(parent = globalenv()),
                          quiet = TRUE)
      }, error = function(e) {
        # surface error to UI log and return NULL
        showNotification(paste("Report rendering failed:",conditionMessage(e)), type = "error")
        FALSE
      })
      
      incProgress(0.7, detail = "Finalising output...")
      Sys.sleep(0.05)
      
      # return a list with the path and the temp dir so renderUI can include it
      list(html_file = result_html, dir = tempReportDir)
    })
  }, ignoreNULL = TRUE)
  
  # UI consumer — uses the static result from the eventReactive
  output$summaryWaitReport <- renderUI({
    res <- report_html()
    req(res, res$html_file, file.exists(res$html_file))
    includeHTML(res$html_file)
  })
  
  # Helper to build params list (avoids duplication)
  make_params <- function() {
    list(
      tci_start_date = input$reportDate0,
      first_ref_date = input$reportDate1,
      last_ref_date  = input$reportDate2,
      report_organs  = input$organReportCheckbox
    )
  }
  
  # Common pattern for download handlers: render to a temp dir then copy/move to 'file'
  output$buttonReportToHTML <- downloadHandler(
    filename = function() {
      paste0("WaitingListReport_", Sys.Date(), ".html")
    },
    contentType = "text/html",
    content = function(file) {
      # 1) create temp HTML (render to tmp)
      tmp_html <- tempfile(fileext = ".html")
      withProgress(message = "Rendering HTML report ...", value = 0, {
        incProgress(0.2)
      rmarkdown::render(
        input = rmdReportFile,
        output_format = rmarkdown::html_document(self_contained = TRUE),
        output_file = tmp_html,
        params = make_params(),
        envir = new.env(parent = globalenv()),
        quiet = TRUE
      )
      if (!file.exists(tmp_html)) showNotification(paste("Could not create temporary HTML file: ", tmp_html), type="error")
      incProgress(0.8)
      # 2) Copy into the location Shiny expects (this preserves Shiny's headers)
      ok <- file.copy(tmp_html, file, overwrite = TRUE)
      if (!ok) showNotification("Failed to copy temporary HTML file -> output file", type="error")
      incProgress(1.0)
      })
    }
  )
  
  output$buttonReportToPDF <- downloadHandler(
      filename = function() {
        paste0("WaitingListReport_", Sys.Date(), ".pdf")
      },
      content = function(file) {
      req(rmdReportFile)
      tempReportDir <- tempdir()
      tempReport <- file.path(tempReportDir, "tci_report.Rmd")
      file.copy(rmdReportFile, tempReport, overwrite = TRUE)
      params <- make_params()
      withProgress(message = "Rendering PDF report...", value = 0, {
        incProgress(0.2)
        # prefer native PDF render; fallback to HTML->pagedown
        ok <- tryCatch({
          rmarkdown::render(input = tempReport,
                            output_format = rmarkdown::pdf_document(
                              latex_engine = "xelatex",
                              pandoc_args = c("--variable=geometry:margin=1in")
                            ),
                            output_file = basename(file),
                            output_dir = dirname(file),
                            params = params,
                            envir = new.env(parent = globalenv()),
                            quiet = TRUE)
          incProgress(0.6)
          TRUE
        }, error = function(e) {
          showNotification(paste("Direct PDF rendering failed:",conditionMessage(e)), type = "error")
          FALSE
        })
        if (!ok) {
          # fallback to html -> pagedown
          showNotification("Going to try HTML > Pagedown instead", type = "message")
          html_out <- rmarkdown::render(tempReport,
                                        output_format = "html_document",
                                        output_dir = tempReportDir,
                                        params = params,
                                        envir = new.env(parent = globalenv()),
                                        quiet = TRUE)
          incProgress(0.6)
          # attempt chrome_print to create the PDF at `file`
          tryCatch({
            pagedown::chrome_print(html_out, output = file)
          }, error = function(e) {
            # as a final fallback use pandoc_convert on the html -> pdf
            showNotification(paste("Pagedown rendering failed:",conditionMessage(e), ". Trying pandoc.."), type = "error")
            rmarkdown::pandoc_convert(html_out, from = "html", to = "pdf", output = file,
                                      options = c("-V", "geometry:portrait", "-V", "title=", "-V", "geometry:margin=0.5in"))
          })
        }
        incProgress(0.8)
        incProgress(1.0)
      })
    }
  )
}
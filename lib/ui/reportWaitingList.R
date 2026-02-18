# Improved UI function (if used as a plain UI piece)
reportWaitingListTab <- function(id = NULL) {
  # if using as a module call NS for ids; otherwise leave as-is
  ns <- if (!is.null(id)) shiny::NS(id) else identity
  
  list(
    tags$head(
      tags$link(rel = "stylesheet", href = "print.css")
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
    generateTheHTML()
  },
  ignoreNULL = TRUE)
  
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
  
  # Helper function to generate the HTML for rendering, however we render it
  # Returns a list of directory and filename for ui to render
  generateTheHTML <- function()
  {
    params_now <- make_params()
    
    # isolated temp dir for this run
    tempReportDir <- tempdir()
    tempReport <- file.path(tempReportDir, "report.Rmd")
    file.copy(rmdReportFile, tempReport, overwrite = TRUE)
    
    # show progress during the render
    withProgress(message = "Rendering report...", value = 0, {
      incProgress(0.05, detail = "Preparing files...")
      Sys.sleep(0.05) # tiny pause to let UI draw (optional)
      
      incProgress(0.15, detail = "Rendering R Markdown (this may take a moment)...")
      result_html <- tryCatch({
        
        # Ensure print.css lives in the same dir as html_out (pagedown serves that dir)
        app_print_css <- file.path("www", "print.css")  # your app copy of the css
        if (file.exists(app_print_css)) {
          file.copy(app_print_css, file.path(tempReportDir, "print.css"), overwrite = TRUE)
        } else {
          warning("print.css not found at www/print.css; print styling may be missing. See print.css for recommended rules.  [oai_citation:1‡print.css](sediment://file_000000005cf872468eea29cf6ac2e56f)")
        }
        
        rmarkdown::render(tempReport,
                          output_format = rmarkdown::html_document(css = "print.css", self_contained = TRUE),
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
    })
    # return a list with the path and the temp dir so renderUI can include it
    return(list(html_file = result_html, dir = tempReportDir))
  }
  
  output$buttonReportToHTML <- downloadHandler(
    filename = function() {
      paste0("WaitingListReport_", Sys.Date(), ".html")
    },
    contentType = "text/html",
    content = function(file) {
    html_out <- generateTheHTML()[1]
    html_out <- as.character(html_out)
    if (!file.exists(html_out)) {
      # HTML render failed — stop and surface error (or you could fallback to LaTeX here)
      stop("HTML render failed; cannot produce PDF.")}
    
      # Copy the generated HTML into the path Shiny expects
      ok <- file.copy(from = html_out, to = file, overwrite = TRUE)
      if (!ok) stop("Failed to copy generated HTML to Shiny download location.")
    }
  )
  
  output$buttonReportToPDF <- downloadHandler(
    filename = function() {
      paste0("WaitingListReport_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      req(rmdReportFile)
      
      # 1) Get the HTML path produced by the generator, remember this is a (filename,directory) list
      html_out <- generateTheHTML()
      if (is.null(html_out) || length(html_out) == 0) {
        stop("generateTheHTML() returned NULL or empty result.")
      }
      html_out <- as.character(html_out[1])
      if (!file.exists(html_out)) {
        stop("HTML render failed or html_out does not exist; cannot produce PDF.")
      }
      
      # Check Chrome availability (helpful debug)
      chrome_path <- pagedown::find_chrome()
      if (is.null(chrome_path)) {
        stop("Chrome/Chromium not found by pagedown::find_chrome(). Install Chrome or make it available in PATH.")
      }
      
      # Convert HTML -> PDF using pagedown (primary attempt)
      withProgress(message = "Rendering HTML > PDF report...", value = 0, {
        incProgress(0.4, detail = "Converting HTML to PDF with pagedown...")
        tryCatch({
          pagedown::chrome_print(input = html_out, output = file, format = "pdf", extra_args = c("--print-background"))
        }, error = function(e) {
          showNotification(paste("pagedown::chrome_print failed:", conditionMessage(e)), type = "error")
          # final fallback: pandoc html->pdf (less likely to preserve print CSS/backgrounds)
          tryCatch({
            rmarkdown::pandoc_convert(html_out, from = "html", to = "pdf", output = file,
                                      options = c("-V", "geometry:portrait", "-V", "title=", "-V", "geometry:margin=1in"))
          }, error = function(e2) {
            stop("All HTML->PDF conversion attempts failed: ", conditionMessage(e2))
          })
        })
        incProgress(0.8, detail = "Finishing up...")
        incProgress(1.0)
      })
    }
  )
}
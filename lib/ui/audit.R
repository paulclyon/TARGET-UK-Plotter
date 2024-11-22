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
        actionButton(inputId = "runAuditReport", label = "Run Audit Report")
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
  finalRefAuditInput <- reactive({
    # This does the knitting bit ready to make the HTML by running the knit function
    rmdAuditFiles <- c(Sys.getenv("AUDIT_PATHWAY_RMD"))

    sapply(rmdAuditFiles, knit, quiet = T)

    # This makes the MD file which is basically just HTML in a file
    htmltools::includeMarkdown(Sys.getenv("AUDIT_PATHWAY_MD"))
  })

  observeEvent(input$runAuditReport, {
    logger(paste("Running audit for dates: ", input$auditDate1,"-", input$auditDate2, sep=""))
    logger(paste("Running audit for organs:", input$organAuditCheckbox))
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
}

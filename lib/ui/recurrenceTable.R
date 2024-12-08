recurrenceTab <- function() {
  list(fluidRow(box(
    width = 12,
    column(
      width = 4,
      actionButton("buttonPasteRecurrenceData", "Copy Data to Clipboard"),
      actionButton("buttonSaveRecurrenceData",  "Save Data to File")
    )
  )),
  fluidRow(box(
    width = 12,
    DT::dataTableOutput("tableRecurrence")
  )))
}

recurrenceTableServer <- function(input, output, session) {
  observeEvent(input$buttonSaveRecurrenceData, {
    shinyCatch({
      message("Not yet implemented!")
    }, prefix = '')
  })

  observeEvent(input$buttonPasteRecurrenceData, {
    shinyCatch({
      message("Not yet implemented!")
    }, prefix = '')
  })

  output$tableRecurrence <- DT::renderDataTable({
    DT::datatable(recurrenceData)
  })
}

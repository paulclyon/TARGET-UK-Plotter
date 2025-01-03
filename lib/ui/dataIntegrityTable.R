dataIntegrityTab <- function() {
  list(fluidRow(box(
    width = 12,
    column(
      width = 4,
      actionButton("buttonPasteIntegrityData", "Copy Data to Clipboard"),
      actionButton("buttonSaveIntegrityData",  "Save Data to File")
    )
  )),
  fluidRow(box(
    width = 12,
    DT::dataTableOutput("tableDataIntegrity")
  )))
}

dataIntegrityTableServer <- function(input, output, session) {
  observeEvent(input$buttonPasteIntegrityData, {
    copyDataToClipboard(dataIntegrity.df)
    shinyCatch({
      message("Copied data to the clipboard, please paste into Excel")
    }, prefix = '')
  })

  observeEvent(input$buttonSaveIntegrityData, {
    shinyCatch({
      message("Choose a file to export to...")
    }, prefix = '') # DOESNT WORK IN A DOCKER
    exportFile = file.choose(new = TRUE)
    if (!endsWith(exportFile, ".csv"))
    {
      exportFile = paste(exportFile, ".csv", sep = "")
    }
    shinyCatch({
      message(paste("Attempting to export data to file", exportFile))
    }, prefix = '')
    write.csv(dataIntegrity.df, exportFile, row.names = TRUE)
    shinyCatch({
      message(paste("Exported data to file", exportFile))
    }, prefix = '')
  })

  output$tableDataIntegrity <- DT::renderDataTable({
    DT::datatable(dataIntegrity.df)
  })
}

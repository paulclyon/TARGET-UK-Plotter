survivalTab <- function() {
  list(fluidRow(box(
    width = 12,
    column(
      width = 4,
      actionButton("buttonPasteSurvivalData", "Copy Data to Clipboard"),
      actionButton("buttonSaveSurvivalData",  "Save Data to File")
    )
  )),
  fluidRow(box(
    width = 12,
    DT::dataTableOutput("tableSurvival")
  )))
}

survivalTableServer <- function(input, output, session) {
  observeEvent(input$buttonPasteSurvivalData, {
    copyDataToClipboard(survivalData)
    shinyCatch({
      message("Copied data to the clipboard, please paste into Excel")
    }, prefix = '')
  })

  observeEvent(input$buttonSaveSurvivalData, {
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
    write.csv(survivalData, exportFile, row.names = TRUE)
    shinyCatch({
      message(paste("Exported data to file", exportFile))
    }, prefix = '')
  })

  output$tableSurvival <- DT::renderDataTable({
    DT::datatable(survivalData)
  })
}

dataIntegrityTab <- function() {
  list(fluidRow(box(
    width = 12,
    column(
      width = 4,
      actionButton("buttonPasteIntegrityData", "Copy Data to Clipboard")
    ),
    column(
      width = 4,
      actionButton("buttonSaveIntegrityData",  "Save Data to File")
    ),
    column(
      width = 4,
      actionButton("buttonRefresh",  "Refresh Data")
    ),
  )),
  fluidRow(box(
    width = 12,
    DT::dataTableOutput("tableDataIntegrity")
  )))
}

dataIntegrityTableServer <- function(input, output, session, isDocker)
{
  observeEvent(input$buttonPasteIntegrityData, {
    if (isDocker == T)
    {
      shinyCatch({
        message("Sorry running in a Docker via Web interface therefore data export functions not available...")
      }, prefix = '')
    }
    else
    {
      copyDataToClipboard(dataIntegrity.df)
      shinyCatch({
        message("Copied data to the clipboard, please paste into app such as Microsoft Excel on a secure computer (patient IDs included).")
      }, prefix = '')
    }
  })

  observeEvent(input$buttonSaveIntegrityData, {
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
        message("If this is a secure computer (patient IDs included), choose a file to export to...")
      }, prefix = '')
      result = tryCatch({ exportFile <- file.choose(new = TRUE) }, error = function(err) { logger(err,F) })
      if (!is.na(exportFile) && exportFile != "")
      {
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
      }
      else
      {
        shinyCatch({
          message(paste("No file selected to export to, no data export performed"))
        }, prefix = "")
      }
    }
  })

  observeEvent(input$buttonRefresh, {
    output$tableDataIntegrity <- DT::renderDataTable({DT::datatable(dataIntegrity.df)})
  })
  
  output$tableDataIntegrity <- DT::renderDataTable({
    DT::datatable(dataIntegrity.df)
  })
}

loggerTab <- function() {
  list(fluidRow(box(
    width = 12,
    column(
      width = 4,
      actionButton("buttonPasteSystemLogs", "Copy System Logs to Clipboard"),
    ),
    column(
      width = 4,
      actionButton("buttonSaveSystemLogs",  "Save System Logs to File")
    ),
    column(
      width = 4,
      actionButton(inputId = "buttonRefresh", label = "Refresh Logs")
    )
  )),
  fluidRow(box(
    width = 12,
    DT::dataTableOutput("tableLogger"), style = "overflow-y: scroll;overflow-x: scroll;"
  )))
}

loggerTableServer <- function(input, output, session, isDocker) {
  observeEvent(input$buttonPasteSystemLogs, {
    if (isDocker == T)
    {
      shinyCatch({
        message("Sorry running in a Docker via Web interface therefore data export functions not available...")
      }, prefix = '')
    }
    else
    {
      copyDataToClipboard(logger.df)
      shinyCatch({
        message("Copied logs to the clipboard, please paste into app such as Microsoft Excel on a secure computer (patient IDs included).")
      }, prefix = '')
    }
  })

  observeEvent(input$buttonSaveSystemLogs, {
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
        message(paste("If this is a secure computer (patient IDs included), choose a file to export to..."),isDocker)
      }, prefix = '')
      result = tryCatch({ exportFile <- file.choose(new = TRUE) }, error = function(err) { logger(err,F) })
      if (!is.na(exportFile) && exportFile != "")
      {
        if (!endsWith(exportFile, ".csv"))
        {
          exportFile <- paste(exportFile, ".csv", sep = "")
        }
        shinyCatch({
          message(paste("Attempting to export system logs to file", exportFile))
        }, prefix = '')
        write.csv(logger.df, exportFile, row.names = TRUE)
        shinyCatch({
          message(paste("Exported system logs to file", exportFile))
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
    output$tableLogger <- DT::renderDataTable({DT::datatable(logger.df)})
  })
  
  output$tableLogger <- DT::renderDataTable({
    DT::datatable(logger.df)
  })
}

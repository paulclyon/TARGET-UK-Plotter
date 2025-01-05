pathwayTab <- function() {
  list(
    fluidRow(box(
      width = 12,
      column(
        width = 4,
        radioButtons(
          "rxTimesTableRadio",
          "Pathway Table Type",
          c("Treated" = "rxdoneTable", "Waiting" = "rxwaitTable")
        )
      ),
      column(
        width = 4,
        actionButton("buttonPasteRxTimesData", "Copy Data to Clipboard"),
        actionButton("buttonSaveRxTimesData", "Save Data to File")
      )
    )),
    fluidRow(box(
      width = 12,
      DT::dataTableOutput("tableRxPathway")
    ))
  )
}

pathwayTableServer <- function(input, output, session, isDocker)
{
  finalRxTableDataInput <- reactive({
    switch(input$rxTimesTableRadio,
      "rxdoneTable" = rxDoneData,
      "rxwaitTable" = rxWaitData
    )
  })

  observeEvent(input$buttonPasteRxTimesData, {
    if (isDocker == T)
    {
      shinyCatch({
        message("Sorry running in a Docker via Web interface therefore data export functions not available...")
      }, prefix = '')
    }
    else
    {
      copyDataToClipboard(finalRxTableDataInput())
      shinyCatch(
        {
          message("Copied data to the clipboard, please paste into app such as Microsoft Excel on a secure computer (patient IDs included).")
        },
        prefix = ""
      )
    }
  })

  observeEvent(input$buttonSaveRxTimesData, {
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
        }, prefix = "")
      result = tryCatch({ exportFile <- file.choose(new = TRUE) }, error = function(err) { logger(err,F) })
      if (!is.na(exportFile) && exportFile != "")
      {
        if (!endsWith(exportFile, ".csv")) {
          exportFile <- paste(exportFile, ".csv", sep = "")
        }
        shinyCatch({
            message(paste("Attempting to export data to file", exportFile))
          }, prefix = "")
        write.csv(finalRxTableDataInput(), exportFile, row.names = TRUE)
        shinyCatch({
            message(paste("Exported data to file", exportFile))
        }, prefix = "")
      }
      else
      {
        shinyCatch({
            message(paste("No file selected to export to, no data export performed"))
        }, prefix = "")
      }
    }
  })

  output$tableRxPathway <- DT::renderDataTable({
    DT::datatable(finalRxTableDataInput())
  })
}

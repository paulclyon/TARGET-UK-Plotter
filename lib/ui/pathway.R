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

pathwayServer <- function(input, output, session, plots) {
  finalRxTableDataInput <- reactive({
    switch(input$rxTimesTableRadio,
      "rxdoneTable" = rxDoneData,
      "rxwaitTable" = rxWaitData
    )
  })

  observeEvent(input$buttonPasteRxTimesData, {
    copyDataToClipboard(finalRxTableDataInput())
    shinyCatch(
      {
        message("Copied data to the clipboard, please paste into Excel")
      },
      prefix = ""
    )
  })

  observeEvent(input$buttonSaveRxTimesData, {
    shinyCatch(
      {
        message("Choose a file to export to...")
      },
      prefix = ""
    ) # DOESNT WORK IN A DOCKER
    exportFile <- NA
    try(
      exportFile <- file.choose(new = TRUE)
    )
    if (!is.na(exportFile)) {
      if (!endsWith(exportFile, ".csv")) {
        exportFile <- paste(exportFile, ".csv", sep = "")
      }
      shinyCatch(
        {
          message(paste("Attempting to export data to file", exportFile))
        },
        prefix = ""
      )
      write.csv(finalRxTableDataInput(), exportFile, row.names = TRUE)
      shinyCatch(
        {
          message(paste("Exported data to file", exportFile))
        },
        prefix = ""
      )
    } else {
      shinyCatch(
        {
          message(paste("No file selected to export to, no data export performed"))
        },
        prefix = ""
      )
    }
  })

  output$tableRxPathway <- DT::renderDataTable({
    DT::datatable(finalRxTableDataInput())
  })
}

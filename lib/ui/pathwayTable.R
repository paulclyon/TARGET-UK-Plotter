pathwayTab <- function() {
  list(
    fluidRow(box(
      width = 12,
      column(
        width = 3,
        radioButtons(
          "rxTimesTableRadio",
          "Pathway Table Type",
          c(
            "Treated" = "rxdoneTable",
            "Current Waiting" = "rxwaitTable",
            "Monthly Waiting List" = "monthlyWaitTable"
          )
        )
      ),
      column(
        width = 3,
        actionButton("buttonPasteRxTimesData", "Copy Data to Clipboard")
      ),
      column(
        width = 3,
        actionButton("buttonSaveRxTimesData", "Save Data to File")
      ),
      column(
        width = 3,
        actionButton("buttonRefreshTimesData", "Refresh")
      )
    )),
    fluidRow(box(
      width = 12,
      DT::dataTableOutput("tableRxPathway"), style = "overflow-y: scroll;overflow-x: scroll;"
    ))
  )
}

pathwayTableServer <- function(input, output, session, isDocker) {
  finalRxTableDataInput <- reactive({
    switch(input$rxTimesTableRadio,
      "rxdoneTable"      = rxDoneData,
      "rxwaitTable"      = rxWaitData,
      "monthlyWaitTable" = monthlyRxWaitData
    )
  })

  observeEvent(input$buttonPasteRxTimesData, {
    if (isDocker == T) {
      shinyCatch(
        {
          message("Sorry running in a Docker via Web interface therefore data export functions not available...")
        },
        prefix = ""
      )
    } else {
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
    if (isDocker == T) {
      shinyCatch(
        {
          message("Sorry running in a Docker via Web interface therefore data export functions not available...")
        },
        prefix = ""
      )
    } else {
      exportFile <- NA
      shinyCatch(
        {
          message("If this is a secure computer (patient IDs included), choose a file to export pathway data to...")
        },
        prefix = ""
      )
      result <- tryCatch(
        {
          exportFile <- file.choose(new = TRUE)
        },
        error = function(err) {
          logger(err, F)
        }
      )
      if (!is.na(exportFile) && exportFile != "") {
        if (!endsWith(exportFile, ".csv")) {
          exportFile <- paste(exportFile, ".csv", sep = "")
        }
        shinyCatch(
          {
            message(paste("Attempting to export pathway data to file", exportFile))
          },
          prefix = ""
        )
        write.csv(finalRxTableDataInput(), exportFile, row.names = TRUE)
        shinyCatch(
          {
            message(paste("Exported pathway data to file", exportFile))
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
    }
  })

  observeEvent(input$buttonRefreshTimesData, {
    output$tableRxPathway <- DT::renderDataTable({
      DT::datatable(finalRxTableDataInput())
    })
  })

  output$tableRxPathway <- DT::renderDataTable({
    DT::datatable(finalRxTableDataInput())
  })
}

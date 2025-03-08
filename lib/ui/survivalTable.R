survivalTab <- function() {
  list(
    fluidRow(box(
      width = 12,
      column(
        width = 4,
        actionButton("buttonPasteSurvivalData", "Copy Data to Clipboard")
      ),
      column(
        width = 4,
        actionButton("buttonSaveSurvivalData", "Save Data to File")
      ),
      column(
        width = 4,
        actionButton("buttonRefreshSurvivalData", "Refresh Data")
      )
    )),
    fluidRow(box(
      width = 12,
      DT::dataTableOutput("tableSurvival"), style = "overflow-y: scroll;overflow-x: scroll;"
    ))
  )
}

survivalTableServer <- function(input, output, session, isDocker) {
  observeEvent(input$buttonPasteSurvivalData, {
    if (isDocker == T) {
      shinyCatch(
        {
          message("Sorry running in a Docker via Web interface therefore data export functions not available...")
        },
        prefix = ""
      )
    } else {
      copyDataToClipboard(survivalData)
      shinyCatch(
        {
          message("Copied data to the clipboard, please paste into app such as Microsoft Excel on a secure computer (patient IDs included).")
        },
        prefix = ""
      )
    }
  })

  observeEvent(input$buttonSaveSurvivalData, {
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
          message("If this is a secure computer (patient IDs included), choose a file to export to...")
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
            message(paste("Attempting to export data to file", exportFile))
          },
          prefix = ""
        )
        write.csv(survivalData, exportFile, row.names = TRUE)
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
    }
  })

  observeEvent(input$buttonRefreshSurvivalData, {
    output$tableSurvival <- DT::renderDataTable({
      DT::datatable(survivalData)
    })
  })

  output$tableSurvival <- DT::renderDataTable({
    DT::datatable(survivalData)
  })
}

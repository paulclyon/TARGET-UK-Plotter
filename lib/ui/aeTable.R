aeTab <- function() {
  list(
    fluidRow(box(width = 12,
      column(
          width = 4,
          checkboxGroupInput(
            "aeGradesCheckbox",
            "CCTAE Grades",
            choices =  cctaeGradeFactors,
            selected = cctaeGradeFactors
          ),
        ),
        column(
          width = 4,
          dateInput(
            "aeTabStartDate",
            "Start Date:",
            format = "dd/mm/yyyy",
            value = Sys.Date() - 365
          ),
          dateInput(
            "aeTabEndDate",
            "End Date:",
            format = "dd/mm/yyyy",
            value = Sys.Date()
          )
        ),
        column(
          width = 4,
          br(),
          actionButton("buttonPasteAEData", "Copy Data to Clipboard"),
          br(),br(),
          actionButton("buttonSaveAEData",  "Save Data to File")
        )
      )
    ),
    fluidRow(box(
      width = 12,
      DT::dataTableOutput("tableAE"))
    )
  )
}

aeTableServer <- function(input, output, session, api) {
  observeEvent(input$buttonPasteAEData, {
    copyDataToClipboard(aeData)
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
    write.csv(aeData, exportFile, row.names = TRUE)
    shinyCatch({
      message(paste("Exported data to file", exportFile))
    }, prefix = '')
  })

  output$tableAE <- DT::renderDataTable({
    aeData.filtered <- filter(aeData, Grade %in% input$aeGradesCheckbox)
    startDate <- asDateWithOrigin(input$aeTabStartDate)
    endDate <-   asDateWithOrigin(input$aeTabEndDate)
    if (nrow(aeData.filtered) > 0)
    {
      # The dates in the table are just strings of format dd-mm-yyyy, convert to Date objects
      aeData.filtered$DateofOnset      <- as.Date(aeData.filtered$DateofOnset,"%d-%m-%Y")
      aeData.filtered$DateofResolution <- as.Date(aeData.filtered$DateofResolution,"%d-%m-%Y")
      if (!is.na(startDate) && nrow(aeData.filtered) >0 )
      {
        aeData.filtered <- aeData.filtered %>% filter(DateofOnset > startDate)
      }
      if (!is.na(endDate) && nrow(aeData.filtered) >0 )
      {
        aeData.filtered <- aeData.filtered %>% filter(DateofOnset < endDate)
      }
    }    
    DT::datatable(aeData.filtered)
  })
}

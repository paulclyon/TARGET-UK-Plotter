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
          actionButton("buttonPasteAEData", "Copy Data to Clipboard"),
          br(),br(),
          actionButton("buttonSaveAEData",  "Save Data to File"),
          br(),br(),
          actionButton("buttonRefresh",     "Refresh Data")
        )
      )
    ),
    fluidRow(box(
      width = 12,
      DT::dataTableOutput("tableAE"))
    )
  )
}

aeTableServer <- function(input, output, session, isDocker)
{
  observeEvent(input$buttonPasteAEData, {
    if (isDocker == T)
    {
      shinyCatch({
        message("Sorry running in a Docker via Web interface therefore data export functions not available...")
      }, prefix = '')
    }
    else
    {
      copyDataToClipboard(aeData)
      shinyCatch({
        message("Copied data to the clipboard, please paste into app such as Microsoft Excel on a secure computer (patient IDs included).")
      }, prefix = '')
    }
  })
  
  observeEvent(input$buttonSaveSurvivalData, {
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
      }
    }
  })

  filterData <- function()
  {
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
    return(aeData.filtered)
  }
  
  observeEvent(input$buttonRefresh, {
    output$tableAE <- DT::renderDataTable({DT::datatable(filterData())})
  })
  
  output$tableAE <- DT::renderDataTable({
    DT::datatable(filterData())
  })
}

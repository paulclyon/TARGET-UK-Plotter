pathwayPlotTab <- function() {
  list(
    fluidRow(
      tabPanel("RxPathwayPlots",
        column(
          width = 3,
          radioButtons(
            "rxTimesPlotRadio",
            "Pathway Plot Type",
            c(
              "Treated Plot"             = "rxdonePlot",
              "Treatment Times SPC Plot" = "spcRxTimePlot",
              "Waiting Plot"             = "rxwaitPlot",
              "Monthly Waiting List"     = "monthlyWaitingPlot"
            )
          ),
          br(),br(),
          actionButton(inputId = "refreshRxPlot", label = "Refresh Plot"),
        ),
        column(
          width = 3,
          dateInput(
            "rxPlotStartDate",
            "Start Date:",
            format = "dd/mm/yyyy",
            value = Sys.Date() - 365
          ),
          dateInput(
            "rxPlotEndDate",
            "End Date:",
            format = "dd/mm/yyyy",
            value = Sys.Date()
          )
        ),
        column(
          width = 3,
          checkboxGroupInput(
            "rxPlotSelectedOrgans",
            "Organs to Plot",
            choices = organFactors,
            selected = organFactors
          )
        ),
        column(
          width = 3,
          div(style=
              "background-color: lightgrey;
               border: 5px solid blue;
               padding: 5px;
               margin: 5px;",
          textOutput("informationalPathwayPlot"))
        )
      )
    ),
    fluidRow(box(
      width = 12,
      height = "100%",
      plotlyOutput("plotRxPathway", height = "100%")
    )),
    detectHeightJS("rxpathwayplots", "plotRxPathway")
  )
}

pathwayPlotServer <- function(input, output, session, api, plots)
{
  finalRxPlotInput <- reactive({
    # Update the plots based on radiobuttons etc
    # This is an efficient way of doing things so that the plots are only made as the radiobuttons are updated
    p <- switch(input$rxTimesPlotRadio,
      "rxdonePlot"         = makeRxDonePlot(     input$rxPlotStartDate, input$rxPlotEndDate, input$rxPlotSelectedOrgans),
      "rxwaitPlot"         = makeRxWaitPlot(     input$rxPlotStartDate, input$rxPlotEndDate, input$rxPlotSelectedOrgans),
      "spcRxTimePlot"      = makeRxDonePlot(     input$rxPlotStartDate, input$rxPlotEndDate, input$rxPlotSelectedOrgans), # FIXME over to you Andy
      "monthlyWaitingPlot" = makeWaitingListPlot(input$rxPlotStartDate, input$rxPlotEndDate, input$rxPlotSelectedOrgans)
    )
    p
  })

  finalRxTableDataInput <- reactive({
    returnData <- switch(input$rxTimesPlotRadio,
      "rxdonePlot"         = rxDoneData,
      "rxwaitPlot"         = rxWaitData,
      "monthlyWaitingPlot" = monthlyRxWaitData
    )
    returnData
  })
  
  observeEvent(input$plotRxPathwaySize, {
    plots$activePlot <- ggplot()
  })

  filteredPlot <- reactive(
  {
    filteredRxData <- finalRxTableDataInput()
    p <- finalRxPlotInput() # FIXME Need a fix if empty ie. if (!is.list(filteredRxData))
    p
  })

  # Plot the Rx pathway plot using the date range and organ filters
  # Note plotly vs. plot gives you the tool tip text
  output$plotRxPathway <- renderPlotly({
    height <- detectedHeight(input, "plotRxPathway")
    plots$activePlot <- ggplotly(filteredPlot(), height = height)
    plots$activePlot
  })

  observe({
    updateCheckboxGroupInput(session, "rxPlotSelectedOrgans", "Organs to Plot",
      choices = api$organFactors,
      selected = api$organFactors
    )
  })

  # When we hit refresh button we want to reset the plot
  # This works to a point in that it resets the scale but it doesn't reload the data
  # Not really sure how this works at all if I am honest! I don't assign it to a real plot, weird
  observeEvent(input$refreshRxPlot, {
    plots$activePlot <- ggplot()
  })
  
  informationalText <- reactive({
    headerText <- "Informational:\n"
    rxDoneText <- ""
    if(notForRxButRxdCount > 0)
    {
      rxDoneText <- paste(notForRxButRxdCount,"/",noRefsProcessed," treatment dates have been excluded from the analysis due to referral data integrity errors; see Data Integrity Table.",sep="")
    }
    switch(input$rxTimesPlotRadio,
           "rxdonePlot" = paste(headerText,rxDoneText, sep=""),
           "rxwaitPlot" = paste(headerText,nrow(rxWaitData)," on waiting list of ",noRefsProcessed," processed referrals, of which ",sum(!is.na(rxWaitData$RefDate))," have a valid referral date.", sep=""),
           "monthlyWaitingPlot" = paste(headerText,"Up to the last specified date, there are ",dplyr::last(monthlyRxWaitData$OnWaitingList)," patients on waiting list for the selected organs.", sep="")
    )
  })
  
  output$informationalPathwayPlot <- renderText({ informationalText() })
}

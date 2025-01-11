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
              "Treated Plot" = "rxdonePlot",
              "Waiting Plot" = "rxwaitPlot"
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
    switch(input$rxTimesPlotRadio,
      "rxdonePlot" = rxdonePlot,
      "rxwaitPlot" = rxwaitPlot
    )
  })

  finalRxTableDataInput <- reactive({
    switch(input$rxTimesPlotRadio,
      "rxdonePlot" = rxDoneData,
      "rxwaitPlot" = rxWaitData
    )
  })
  
  observeEvent(input$plotRxPathwaySize, {
    plots$activePlot <- ggplot()
  })

  filteredPlot <- reactive({
    filteredRxData <- finalRxTableDataInput()
    if (!is.list(filteredRxData)) {
      return(plot.new())
    }

    p <- finalRxPlotInput() +
      scale_x_date(limits = as.Date(
        c(input$rxPlotStartDate, input$rxPlotEndDate), format = "%d/%m/%Y")) +
        theme(legend.position = "bottom")
    p <- p %+% subset(finalRxTableDataInput(), Organs %in% input$rxPlotSelectedOrgans)
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
    logger(paste('count',notForRxButRxdCount))
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
           "rxwaitPlot" = paste(headerText,nrow(rxWaitData)," on waiting list of ",noRefsProcessed," processed referrals.", sep="")
    )
  })
  
  output$informationalPathwayPlot <- renderText({ informationalText() })
}

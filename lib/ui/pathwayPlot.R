pathwayPlotTab <- function() {
  list(
    fluidRow(
      tabPanel(
        "RxPathwayPlots",
        column(
          width = 3,
          radioButtons(
            "rxTimesPlotRadio",
            "Pathway Plot Type",
            c(
              "Treated Plot" = "rxdonePlot",
              "Waiting Plot" = "rxwaitPlot"
            )
          )
        ),
        column(
          width = 3,
          dateInput(
            "rxPlotDate1",
            "Start Date:",
            format = "dd/mm/yyyy",
            value = Sys.Date() - 365
          ),
          dateInput(
            "rxPlotDate2",
            "End Date:",
            format = "dd/mm/yyyy",
            value = Sys.Date()
          )
        ),
        column(
          width = 6,
          checkboxGroupInput(
            "organRxPlotCheckbox",
            "Organs to Plot",
            choices = organFactors,
            selected = organFactors
          ),
        ),
        column(
          width = 3,
          actionButton(inputId = "refreshRxPlot", label = "Refresh Plot")
        )
      )
    ),
    fluidRow(box(
      width = 12,
      plotlyOutput("plotRxPathway")
    ))
  )
}

pathwayPlotServer <- function(input, output, session, api, plots) {
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

  # Plot the Rx pathway plot using the date range and organ filters
  # Note plotly vs. plot gives you the tool tip text
  output$plotRxPathway <- renderPlotly({
    if (!is.list(finalRxTableDataInput())) {
      p <- plot.new()
    } else {
      p <- finalRxPlotInput() +
        scale_x_date(limits = as.Date(c(
          input$rxPlotDate1, input$rxPlotDate2
        ), format = "%d/%m/%Y")) +
        theme(legend.position = "bottom")
      p <- p %+% subset(finalRxTableDataInput(), Organs %in% input$organRxPlotCheckbox)
    }
    plots$activePlot <- p
    plots$activePlot
  })

  observe({
    updateCheckboxGroupInput(session, "organRxPlotCheckbox", "Organs to Plot",
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
}

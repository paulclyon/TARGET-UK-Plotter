pathwayPlotsTab <- function() {
  list(fluidRow(
    tabPanel(
      "RxPathwayPlots",
      column(
        width = 3,
        radioButtons(
          "rxTimesPlotRadio",
          "Pathway Plot Type",
          c("Treated Plot" = "rxdonePlot",
            "Waiting Plot" = "rxwaitPlot")
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
  )))
}

pathwayPlotsServer <- function(input, output, server, plots) {
  finalRxPlotInput <- reactive({
    switch(input$rxTimesPlotRadio,
           "rxdonePlot" = rxdonePlot,
           "rxwaitPlot" = rxwaitPlot)
  })

  # Plot the Rx pathway plot using the date range and organ filters
  # Note plotly vs. plot gives you the tool tip text
  output$plotRxPathway <- renderPlotly({
    if (!is.list(finalRxTableDataInput()))
    {
      p <- plot.new()
    }
    else
    {
      p <- finalRxPlotInput() +
        scale_x_date(limits = as.Date(c(
          input$rxPlotDate1, input$rxPlotDate2
        ), format = "%d/%m/%Y")) +
        theme(legend.position = "bottom")
      p <- p %+% subset(finalRxDataInput(), Organs %in% input$organRxPlotCheckbox)
    }
    plots$activePlot <- p
    plots$activePlot
  })
}

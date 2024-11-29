pathwayPieTab <- function() {
  list(fluidRow(
    tabPanel(
      "RxPathwayPies",
      column(
        width = 3,
        radioButtons(
          "rxTimesPieRadio",
          "Pathway Pie Chart Type",
          c("Treated Pie"  = "rxdonePie",
            "Waiting Pie"  = "rxwaitPie")
        )
      ),
      column(
        width = 3,
        dateInput(
          "rxPieDate1",
          "Start Date:",
          format = "dd/mm/yyyy",
          value = Sys.Date() - 365
        ),
        dateInput(
          "rxPieDate2",
          "End Date:",
          format = "dd/mm/yyyy",
          value = Sys.Date()
        )
      ),
      column(
        width = 3,
        checkboxGroupInput(
          "organPieCheckbox",
          "Organs to Chart",
          choices = organFactors,
          selected = organFactors
        ),
      ),
      column(
        width = 3,
        actionButton(inputId = "refreshRxPie", label = "Refresh Pie Chart")
      )
    )
  ),
  fluidRow(box(
    width = 12,
    plotOutput("pieRxPathway")
  )))
}

pathwayPieServer <- function(input, output, session, api, plots) {
  finalRxPieInput <- reactive({
    switch(input$rxTimesPieRadio,
           "rxdonePie" = rxdonePie,
           "rxwaitPie" = rxwaitPie)
  })

  finalRxTableDataInput <- reactive({
    switch(input$rxTimesTableRadio,
           "rxdoneTable" = rxDoneData,
           "rxwaitTable" = rxWaitData)
  })

  output$pieRxPathway <- renderPlot(
    {
      if (!is.list(finalRxTableDataInput()))
      {
        p <- plot.new()
      }
      else
      {
        makeRxDonePie(input$rxPieDate1,
                      input$rxPieDate2,
                      input$organPieCheckbox)
        makeRxWaitPie(input$rxPieDate1,
                      input$rxPieDate2,
                      input$organPieCheckbox)
        p <- finalRxPieInput()
      }
      plots$activePlot <- p
      plots$activePlot
    })

  observe({
    updateCheckboxGroupInput(session, "organPieCheckbox", "Organs to Chart",
      choices = api$organFactors,
      selected = api$organFactors
    )
  })

  observeEvent(input$refreshRxPie, {
    plots$activePlot <- ggplot()
  })
}

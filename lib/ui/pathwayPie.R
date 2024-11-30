pathwayPieTab <- function() {
  list(
    fluidRow(
      tabPanel(
        "RxPathwayPies",
        column(
          width = 3,
          radioButtons(
            "rxTimesPieRadio",
            "Pathway Pie Chart Type",
            c(
              "Treated Pie" = "treatedPie",
              "Waiting Pie" = "waitingPie"
            )
          )
        ),
        column(
          width = 3,
          dateInput(
            "pieStartDate",
            "Start Date:",
            format = "dd/mm/yyyy",
            value = Sys.Date() - 365
          ),
          dateInput(
            "pieEndDate",
            "End Date:",
            format = "dd/mm/yyyy",
            value = Sys.Date()
          )
        ),
        column(
          width = 3,
          checkboxGroupInput(
            "pieSelectedOrgans",
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
    )),
    detectHeightJS("rxpathwaypies", "pieRxPathway")
  )
}

pathwayPieServer <- function(input, output, session, api, plots) {
  selectedPiePlot <- reactive({
    switch(input$rxTimesPieRadio,
      "treatedPie" = treatedPie(),
      "waitingPie" = waitingPie()
    )
  })

  selectedData <- reactive({
    switch(input$rxTimesPieRadio,
      "treatedPie" = rxDoneData,
      "waitingPie" = rxWaitData
    )
  })

  treatedPie <- reactive(
    makeRxDonePie(
      input$pieStartDate,
      input$pieEndDate,
      input$pieSelectedOrgans
    )
  )

  waitingPie <- reactive(
    makeRxWaitPie(
      input$pieStartDate,
      input$pieEndDate,
      input$pieSelectedOrgans
    )
  )

  height <- reactive(detectedHeight(input, "pieRxPathway"))

  output$pieRxPathway <- renderPlot(
    {
      if (!is.list(selectedData())) {
        return(plot.new())
      }

      plots$activePlot <- selectedPiePlot()
      plots$activePlot
    },
    height = height
  )

  observe({
    updateCheckboxGroupInput(session, "pieSelectedOrgans", "Organs to Chart",
      choices = api$organFactors,
      selected = api$organFactors
    )
  })

  observeEvent(input$refreshRxPie, {
    plots$activePlot <- ggplot()
  })
}

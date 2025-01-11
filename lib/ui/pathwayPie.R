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
          ),
          br(),br(),
          actionButton(inputId = "refreshRxPie", label = "Refresh Pie Chart")
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
          )
        ),
        column(
          width = 3,
          div(style=
              "background-color: lightgrey;
               border: 5px solid blue;
               padding: 5px;
               margin: 5px;",
          textOutput("informationalPathwayPie"))
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
  
  
  informationalText <- reactive({
    headerText <- "Informational:\n"
    if(notForRxButRxdCount > 0)
    {
      rxDoneText <- paste(headerText,"\n",notForRxButRxdCount,"/",noRefsProcessed," treatment dates have been excluded from the analysis due to referral data integrity errors; see Data Integrity Table.",sep="")
    }
    switch(input$rxTimesPieRadio,
           "treatedPie" = paste(headerText,nrow(rxDoneData),"referrals which have completed treatment analysed in total"),
           "waitingPie" = paste(headerText,nrow(rxWaitData),"without treatment dates in total, ",length(which(sapply(is.na(rxWaitData$RefDate),isTRUE))),"of which omitted due to no valid referral date.")
    )
  })
  
  output$informationalPathwayPie <- renderText({ informationalText() })
}

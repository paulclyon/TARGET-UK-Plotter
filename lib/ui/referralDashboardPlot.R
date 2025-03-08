referralDashboardPlotTab <- function() {
  list(
    fluidRow(
      tabPanel(
        "ReferralDashboard",
        column(
          width = 3,
          dateInput(
            "referralDashboardPlotStart",
            "Start Date:",
            format = "dd/mm/yyyy",
            value = Sys.Date() - 365
          ),
          dateInput(
            "referralDashboardPlotEnd",
            "End Date:",
            format = "dd/mm/yyyy",
            value = Sys.Date()
          ),
        ),
        column(
          width = 3,
          checkboxGroupInput(
            "referralDashboardPlotOrganCheckbox",
            "Organs to Plot",
            choices = organFactors,
            selected = organFactors
          ),
        ),
        column(
          width = 3,
          radioButtons(
            "referralDashboardPlotDurationRadio",
            "Duration of Plot",
            c(
              "Weekly" = "week",
              "2 Weekly" = "2 weeks",
              "Monthly" = "month",
              "Quarterly" = "quarter",
              "Yearly" = "year"
            ),
            selected = "month"
          ),
          radioButtons(
            "referralDashboardPlotTypeRadio",
            "Type of Plot",
            c(
              "Mean" = "mean",
              "Counts" = "counts"
            ),
            selected = "mean"
          )
        ),
        column(
          width = 3,
          actionButton(inputId = "referralDashboardPlotRefresh", label = "Refresh Plot")
        )
      )
    ),
    fluidRow(
      box(
        width = 4,
        plotlyOutput("plotReferralDashboardRefToDTT")
      ),
      box(
        width = 4,
        plotlyOutput("plotReferralDashboardDTTToRx")
      ),
      box(
        width = 4,
        plotlyOutput("plotReferralDashboardRefToRx")
      ),
    ),
    detectHeightJS("referralDashboard", "plotReferralDashboardRefToDTT"),
    detectHeightJS("referralDashboard", "plotReferralDashboardDTTToRx"),
    detectHeightJS("referralDashboard", "plotReferralDashboardRefToRx")
  )
}

referralDashboardPlotServer <- function(input, output, session, api, plots) {
  observe({
    updateCheckboxGroupInput(session, "referralDashboardPlotOrganCheckbox", "Organs to Plot",
      choices = api$organFactors,
      selected = api$organFactors
    )
  })

  referralTimes <- reactive(processReferralTimesPerPeriod(
    rxDoneData %>% filter(Organs %in% input$referralDashboardPlotOrganCheckbox),
    rxWaitData %>% filter(Organs %in% input$referralDashboardPlotOrganCheckbox),
    input$referralDashboardPlotStart,
    input$referralDashboardPlotEnd,
    input$referralDashboardPlotDurationRadio
  ))


  output$plotReferralDashboardRefToDTT <- renderPlotly({
    p <- ggplot()
    if (input$referralDashboardPlotTypeRadio == "mean") {
      p <- refToDTTMeanPlot(referralTimes(), input$referralDashboardPlotDurationRadio)
    } else {
      p <- refToDTTCountPlot(referralTimes(), input$referralDashboardPlotDurationRadio)
    }

    height <- detectedHeight(input, "plotReferralDashboardRefToDTT")

    p <- ggplotly(p, height = height)
    plots$activePlot <- p
    p
  })

  output$plotReferralDashboardRefToRx <- renderPlotly({
    p <- ggplot()
    if (input$referralDashboardPlotTypeRadio == "mean") {
      p <- refToRxMeanPlot(referralTimes(), input$referralDashboardPlotDurationRadio)
    } else {
      p <- refToRxCountPlot(referralTimes(), input$referralDashboardPlotDurationRadio)
    }

    height <- detectedHeight(input, "plotReferralDashboardRefToRx")

    p <- ggplotly(p, height = height)
    plots$activePlot <- p
    p
  })

  output$plotReferralDashboardDTTToRx <- renderPlotly({
    p <- ggplot()
    if (input$referralDashboardPlotTypeRadio == "mean") {
      p <- dttToRxMeanPlot(referralTimes(), input$referralDashboardPlotDurationRadio)
    } else {
      p <- dttToRxCountPlot(referralTimes(), input$referralDashboardPlotDurationRadio)
    }

    height <- detectedHeight(input, "plotReferralDashboardDTTToRx")

    p <- ggplotly(p, height = height)
    plots$activePlot <- p
    p
  })


  observeEvent(input$referralDashboardPlotRefresh, {
    plots$activePlot <- ggplot()
  })
}

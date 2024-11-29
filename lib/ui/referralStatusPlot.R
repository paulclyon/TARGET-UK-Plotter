referralStatusPlotTab <- function() {
  list(
    fluidRow(
      tabPanel(
        "ReferralStatus",
        column(
          width = 3,
          dateInput(
            "referralStatusPlotStart",
            "Start Date:",
            format = "dd/mm/yyyy",
            value = Sys.Date() - 365
          ),
          dateInput(
            "referralStatusPlotEnd",
            "End Date:",
            format = "dd/mm/yyyy",
            value = Sys.Date()
          ),
        ),
        column(
          width = 3,
          checkboxGroupInput(
            "referralStatusPlotOrganCheckbox",
            "Organs to Plot",
            choices = organFactors,
            selected = organFactors
          ),
        ),
        column(
          width = 3,
          radioButtons(
            "referralStatusPlotDurationRadio",
            "Duration of Plot",
            c(
              "Weekly" = "week",
              "Monthly" = "month",
              "Quarterly" = "quarter",
              "Yearly" = "year"
            ),
            selected = "month"
          )
        ),
        column(
          width = 3,
          actionButton(inputId = "referralStatusPlotRefresh", label = "Refresh Plot")
        )
      )
    ),
    fluidRow(box(
      width = 12,
      plotlyOutput("plotReferralStatus")
    ))
  )
}

referralStatusPlotServer <- function(input, output, session, api, plots) {
  observe({
    updateCheckboxGroupInput(session, "referralStatusPlotOrganCheckbox", "Organs to Plot",
      choices = api$organFactors,
      selected = api$organFactors
    )
  })

  referralStatuses <- reactive(processReferralStatusPerPeriod(
    rxDoneData %>% filter(Organs %in% input$referralStatusPlotOrganCheckbox),
    rxWaitData %>% filter(Organs %in% input$referralStatusPlotOrganCheckbox),
    input$referralStatusPlotStart,
    input$referralStatusPlotEnd,
    input$referralStatusPlotDurationRadio
  ))
  output$plotReferralStatus <- renderPlotly({
    print(referralStatuses())
    p <- makeReferralStatusPlot(referralStatuses())

    if (input$referralStatusPlotDurationRadio == "week") {
      p <- p + scale_x_date(date_breaks = "1 week", date_labels = "%e %b %y") +
        scale_y_continuous(breaks = seq(0, 100, by = 1))
    } else if (input$referralStatusPlotDurationRadio == "month") {
      p <- p + scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
        scale_y_continuous(breaks = seq(0, 100, by = 2))
    } else if (input$referralStatusPlotDurationRadio == "year") {
      p <- p + scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
        scale_y_continuous(breaks = seq(0, 100, by = 5))
    }

    p <- ggplotly(p)
    plots$activePlot <- p
    p
  })

  observeEvent(input$referralStatusPlotRefresh, {
    plots$activePlot <- ggplot()
  })
}

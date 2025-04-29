waitTimesDashboardPlotTab <- function() {
  list(
    fluidRow(
      column(
        width = 5,
        radioButtons(
          "waitTimesDashboardPlotTypeRadio",
          "Plot Style",
          list("Mean" = "mean","Counts" = "counts","Box Plot" = "boxplot"),
          inline = TRUE,
          selected = "mean"
        )
      ),
      column(
        width = 7,
        div(
          style =
            "background-color: lightgrey;
               border: 5px solid blue;
               padding: 5px;
               margin: 5px;",
          textOutput("informationalWaitTimePlot")
        )
      )
    ),
    fluidRow(
      tabPanel(
        "waitTimesDashboard",
        column(
          width = 3,
          dateInput(
            "waitTimesDashboardPlotStart",
            "Start Date:",
            format = "dd/mm/yyyy",
            value = Sys.Date() - 365
          ),
          dateInput(
            "waitTimesDashboardPlotEnd",
            "End Date:",
            format = "dd/mm/yyyy",
            value = Sys.Date()
          ),
        ),
        column(
          width = 3,
          checkboxGroupInput(
            "waitTimesDashboardPlotOrganCheckbox",
            "Organs to Plot",
            choices = organFactors,
            selected = organFactors
          ),
        ),
        column(
          width = 3,
          radioButtons(
            "waitTimesDashboardPlotDurationRadio",
            "Duration of Plot",
            list("Weekly" = "week",
                 "2 Weekly" = "2 weeks",
                 "Monthly" = "month",
                 "Quarterly" = "quarter",
                 "Yearly" = "year"),
            selected = "month"
          )
        ),
        column(
          width = 3,
          radioButtons(
            "waitTimesDashboardPlotGroupRadio",
            "Plot Group...",
            list("By Ablation Date" = "Ablation Date",
                 "By DTT (or Ablation) Date" = "Performed",
                 "Waiting" = "Waiting",
                 "All" = "All"),
            selected = "Ablation Date"
          ),
        )
      )
    ),
    fluidRow(
      box(
        width = 4,
        plotlyOutput("plotWaitTimesDashboardRefToDTT")
      ),
      box(
        width = 4,
        plotlyOutput("plotWaitTimesDashboardDTTToRx")
      ),
      box(
        width = 4,
        plotlyOutput("plotWaitTimesDashboardRefToRx")
      ),
    ),
    detectHeightJS("waitTimesDashboard", "plotWaitTimesDashboardRefToDTT"),
    detectHeightJS("waitTimesDashboard", "plotWaitTimesDashboardDTTToRx"),
    detectHeightJS("waitTimesDashboard", "plotWaitTimesDashboardRefToRx")
  )
}

waitTimesDashboardPlotServer <- function(input, output, session, api, plots) {
  observe({
    updateCheckboxGroupInput(session, "waitTimesDashboardPlotOrganCheckbox", "Organs to Plot",
      choices = api$organFactors,
      selected = api$organFactors
    )
  })
  
  informationalText <- reactive({
    headerText <- "Informational:\n"
    waitTimeText <- switch(input$waitTimesDashboardPlotGroupRadio,
           "Ablation Date" = paste(headerText, "Plot only patients treated, dated by Ablation date", sep = ""),
           "Performed" = paste(headerText, "Plot by DTT date only if specified (Ref-to-DTT plot). Plotted by Ablation date for Rx plots.", sep = ""),
           "Waiting" = paste(headerText, "Plot just those waiting in each time period, across each of the three plots.", sep = ""),
           "All" = paste(headerText, "Plot all patients, both treated and waiting, in each period.", sep = "")
    )
  })
  
  waitTimesTimes <- reactive(processWaitTimesPerPeriod(
    rxDoneData %>% filter(Organs %in% input$waitTimesDashboardPlotOrganCheckbox),
    rxWaitData %>% filter(Organs %in% input$waitTimesDashboardPlotOrganCheckbox),
    input$waitTimesDashboardPlotStart,
    input$waitTimesDashboardPlotEnd,
    input$waitTimesDashboardPlotDurationRadio
  ))


  output$plotWaitTimesDashboardRefToDTT <- renderPlotly({
    p <- doWaitPlot(
      "RefToDTT",
      input$waitTimesDashboardPlotTypeRadio,
      waitTimesTimes(),
      input$waitTimesDashboardPlotDurationRadio,
      input$waitTimesDashboardPlotGroupRadio
    )

    height <- detectedHeight(input, "plotWaitTimesDashboardRefToDTT")

    p <- ggplotly(p, height = height)
    plots$activePlot <- p
    p
  })

  output$plotWaitTimesDashboardRefToRx <- renderPlotly({
    p <- doWaitPlot(
      "RefToRx",
      input$waitTimesDashboardPlotTypeRadio,
      waitTimesTimes(),
      input$waitTimesDashboardPlotDurationRadio,
      input$waitTimesDashboardPlotGroupRadio
    )

    height <- detectedHeight(input, "plotWaitTimesDashboardRefToRx")

    p <- ggplotly(p, height = height)
    plots$activePlot <- p
    p
  })

  output$plotWaitTimesDashboardDTTToRx <- renderPlotly({
    p <- doWaitPlot(
      "DTTToRx",
      input$waitTimesDashboardPlotTypeRadio,
      waitTimesTimes(),
      input$waitTimesDashboardPlotDurationRadio,
      input$waitTimesDashboardPlotGroupRadio
    )

    output$informationalWaitTimePlot <- renderText({
      informationalText()
    })
    
    height <- detectedHeight(input, "plotWaitTimesDashboardDTTToRx")

    p <- ggplotly(p, height = height)
    plots$activePlot <- p
    p
  })
}

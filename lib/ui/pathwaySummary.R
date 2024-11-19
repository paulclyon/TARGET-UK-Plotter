pathwaySummaryTab <- function() {
  fluidRow(
    tabPanel("Waiting List Summary",
             verbatimTextOutput("summaryWaitData")),
    tabPanel("Treated List Summary",
             verbatimTextOutput("summaryRxData"))
  )
}

pathwaySummaryServer <- function(input, output, session) {
  output$summaryWaitData <- renderPrint({
    summary(rxWaitData)
  })
  output$summaryRxData <- renderPrint({
    summary(rxDoneData)
  })
}

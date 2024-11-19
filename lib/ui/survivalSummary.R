survivalSummaryTab <- function() {
  verbatimTextOutput("summarySurvivalData")
}

survivalSummaryServer <- function(input, output, session) {
  output$summarySurvivalData <- renderPrint({
    paste(print(summary(survivalFitSex)), "\n", print(summary(survivalFitOrgan)), sep = "")
  })
}

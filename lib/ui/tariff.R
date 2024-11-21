tariffComponent <- \() uiOutput("totalTariff")

tariffServer <- function(input, output, session, api) {
  tariff <- reactiveValues(theTotalTariff = 0)

  # This is the income generated from the volume of ablations which have taken place on the volume plot
  output$totalTariff <- renderUI({
    if (tariff$theTotalTariff > 1000000) {
      totalTariffText <- paste("£", round(tariff$theTotalTariff / 1000000, 2), "M", sep = "")
    } else if (tariff$theTotalTariff > 10000) {
      totalTariffText <- paste("£", round(tariff$theTotalTariff / 1000, 2), "K", sep = "")
    } else {
      totalTariffText <- paste("£", tariff$theTotalTariff, sep = "")
    }

    if (api$connected && api$loaded) {
      summaryBox2(
        "HRG-coded Total Tariff",
        totalTariffText,
        width = 6,
        icon = "fas fa-pound-sign",
        style = "success"
      )
    } else {
      summaryBox2(
        "Load Data for Income",
        "£0",
        width = 5,
        icon = "fas fa-pound-sign",
        style = "primary"
      )
    }
  })

  tariff
}

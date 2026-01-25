referralTciCalendarTab <- function() {
  list(
    fluidRow(
      tabPanel(
        "TCIPlots",
        column(
          width = 3,
          dateInput(
            "calendarStartDate",
            "Start Date:",
            format = "dd/mm/yyyy",
            value = Sys.Date()
          )
        ),
        column(
          width = 3,
          checkboxInput(
            "tciIncludeTreatedCheckBox",
            "Include Treated",
            TRUE
          )
        ),
        column(
          width = 3,
          actionButton(inputId = "refreshReferralTciCalendar", label = "Refresh Plot")
        )
      )
    ),
    fluidRow(box(
      width = 12,
      calendarOutput("theCalendar")
    )),
    detectHeightJS("tciplots", "theCalendar")
  )
}

calendarServer <- function(input, output, session, api, plots)
{
    output$theCalendar <- renderCalendar({
      makeTciCalendar(
        input$calendarStartDate, input$tciIncludeTreatedCheckBox)
      })
    
    observeEvent(input$refreshReferralTciCalendar, {
      plots$activePlot <- calendar()
      logger("Refresh!")
    })
}




referralTciCalendarTab <- function() {
  list(
    fluidRow(
      tabPanel(
        "TCIPlots",
        column(
          width = 3,
          dateInput(
            "calendarStartDate",
            "From Earliest Referral:",
            format = "dd/mm/yyyy",
            value = Sys.Date()-365
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
      uiOutput("calendar_ui")
    )),
    detectHeightJS("tciplots", "theCalendar")
  )
}

calendarServer <- function(input, output, session, api, plots)
{
  # trigger used to force re-render on demand
  refresh_trigger <- reactiveVal(0)
  
  # Dynamically compute the desired height (pixels). detectedHeight(input) should return a numeric px value.
  calendar_height <- reactive({
    h <- NA_real_
    # defensive: call your detectedHeight safely
    try({
      h <- as.numeric(detectedHeight(input))
    }, silent = TRUE)
    
    # fallback and safety minimum
    if (is.na(h) || !is.finite(h)) h <- 800
    # ensure not too small
    h <- max(400, round(h))
    paste0(h, "px")
  })
  
  # Render the UI for the calendar output (reactive height)
  output$calendar_ui <- renderUI({
    # calendar_height() is reactive so this UI will re-render when it changes
    calendarOutput(
      "theCalendar",
      width  = "100%",
      height = calendar_height()
    )
  })
  
  # Render the calendar itself; make it depend on the refresh trigger so Refresh forces redraw
  output$theCalendar <- renderCalendar({
    # read trigger so render depends on it
    trigger <- refresh_trigger()
    
    # create and return the widget (makeTciCalendar should return an htmlwidget)
    makeTciCalendar(
      input$calendarStartDate,
      input$tciIncludeTreatedCheckBox,
      TRUE
    )
  })
  
  observeEvent(input$refreshReferralTciCalendar, {
    # bump the trigger to force re-render
    refresh_trigger(isolate(refresh_trigger()) + 1)
    
    plots$activePlot <- list(
      type = "tciCalendar",
      start = input$calendarStartDate,
      includeTreated = input$tciIncludeTreatedCheckBox,
      refreshed = Sys.time()
    )
    logger("Refresh!")
  })
  
  # If you want to nudge a resize when inputs change, you can still do so (optional)
  observeEvent(list(input$calendarStartDate, input$tciIncludeTreatedCheckBox), {
  })
}
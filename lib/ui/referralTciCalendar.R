referralTciCalendarTab <- function() {
  list(
    # small CSS to make the calendar and htmlwidget expand to wrapper height
    tags$head(
      tags$style(HTML("
        /* wrapper provides a real height reference */
        #calendar-wrapper { height: calc(100vh - 140px); overflow: hidden; }

        /* ensure the output and tui/full-calendar widget fill the wrapper */
        #calendar-wrapper .shiny-html-output,
        #calendar-wrapper .htmlwidget,
        #calendar-wrapper .tui-full-calendar,
        #calendar-wrapper .tui-calendar {
          height: 100% !important;
        }

        /* If container inside box adds padding, make sure widget uses full box */
        .box .content { padding: 6px 12px; }
        
        /* two-column grid for the organ checkboxes */
        #organTciCalCheckbox .shiny-options-group {
          display: grid;
          grid-template-columns: repeat(2, 1fr); /* two columns */
          gap: 6px 12px;                          /* row gap / column gap */
          align-items: center;                    /* vertically center label + box */
        }
      
        /* remove default margins that cause misalignment */
        #organTciCalCheckbox .shiny-options-group .checkbox {
          margin: 0;
        }
      
        /* make the input and label sit nicely */
        #organTciCalCheckbox .shiny-options-group input[type='checkbox'] {
          vertical-align: middle;
          margin-right: 6px;
        }
      
        /* responsive: 1 column on small screens */
        @media (max-width: 600px) {
          #organTciCalCheckbox .shiny-options-group {
            grid-template-columns: 1fr;
          }
        }
      "))
    ),
    
    fluidRow(
      tabPanel(
        "TCIPlots",
        column(
          width = 3,
          dateInput(
            "calendarStartDate",
            "From Earliest Referral:",
            format = "dd/mm/yyyy",
            value = Sys.Date() - 365
          ),
          checkboxInput(
            "tciIncludeTreatedCheckBox",
            "Include Treated",
            TRUE
          )
        ),
        column(
          width = 6,
          checkboxGroupInput(
            "organTciCalCheckbox",
            "Organs to Display",
            choices = organFactors,
            selected = organFactors,
            inline = TRUE
          ),
        ),
        column(
          width = 3,
          actionButton(inputId = "refreshReferralTciCalendar", label = "Refresh Plot")
        )
      )
    ),
    
    fluidRow(
      box(
        width = 12,
        getTciCalendarKey(),
        
        # wrapper that provides a height for the calendar to fill
        div(
          id = "calendar-wrapper",
          uiOutput("calendar_ui")
        )
      )
    ),
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
    div(
      id = "calendar-wrapper",
      style = "height: calc(100vh - 150px);",
      toastui::calendarOutput("theCalendar")
    )
    
    #calendarOutput(
    #  "theCalendar",
    #  width  = "100%",
    #  height = calendar_height()
    #)
  })

  
  # Render the calendar itself; make it depend on the refresh trigger so Refresh forces redraw
  output$theCalendar <- renderCalendar({
    # read trigger so render depends on it
    trigger <- refresh_trigger()

    # create and return the widget (makeTciCalendar should return an htmlwidget)
    makeTciCalendar(
      input$calendarStartDate,
      input$organTciCalCheckbox,
      input$tciIncludeTreatedCheckBox,
      TRUE
    )
  })
  
  observe({
    updateCheckboxGroupInput(session, "organTciCalCheckbox", "Organs to Display",
                             choices = api$organFactors,
                             selected = api$organFactors
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
    logger(paste("Refresh!",input$organTciCalCheckbox))
  })
  
  # If you want to nudge a resize when inputs change, you can still do so (optional)
  observeEvent(list(input$calendarStartDate, input$tciIncludeTreatedCheckBox), {
  })
}
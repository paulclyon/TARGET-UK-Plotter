referralMapTab <- function() {
  fluidRow(
    wellPanel(fluidRow(
      column(
        3,
        dateInput("refMapDate1", "Start Date:",
          format = "dd/mm/yyyy",
          value = Sys.Date() - 365 * 5
        )
      ),
      column(
        3,
        dateInput("refMapDate2", "End Date:",
          format = "dd/mm/yyyy",
          value = Sys.Date()
        )
      ),
      column(
        5,
        actionButton(inputId = "RefreshReferralMap", label = "Generate New Map Between Dates", style = "height: 60px")
      )
    )),
    tabPanel("ReferralMap", leafletOutput("plotReferralMap")),
    leafletResizeJS()
  )
}

leafletResizeJS <- function() {
  shinyjs::extendShinyjs(text = "
    resizeLeaflet = function() {
      let mapEl = document.getElementById('plotReferralMap');
      if (mapEl && mapEl.checkVisibility && mapEl.checkVisibility()) {
        let bb = mapEl.getBoundingClientRect();
        let wh = window.innerHeight;
        mapEl.style.height = (wh - bb.top - 20) + 'px';
      }
    }

    window.addEventListener('resize', debounce(resizeLeaflet));

    $('#plotReferralMap').on('shiny:value', function (e) {
      resizeLeaflet();
    });

    $(document).on('shiny:connected', function() {
      $('a[data-toggle=\"tab\"][data-value=\"referralmaps\"]').on('shown.bs.tab', function (e) {
        resizeLeaflet();
      });
    });
  ", functions = c())
}

# This is a common function to generate the referral treated map to avoid duplicating code
# See :    https://stackoverflow.com/questions/36679944/mapview-for-shiny
# Refresh: https://stackoverflow.com/questions/67725408/how-i-can-reload-my-leaflet-map-in-shiny-r
generateMapEvent <- function(input, plots) {
  if (!is.Date(input$refMapDate1) || !is.Date(input$refMapDate2)) {
    showNotification("Invalid start/end dates")
    return()
  }

  if (!is.data.frame(rxDoneData) || nrow(rxDoneData) == 0) {
    showNotification("No treatment data available to generate map - please reload data.")
    return()
  }

  showNotification("Generating/Refreshing treatment map ...")

  progress <- shiny::Progress$new()
  tags$head(tags$style(HTML(".progress-bar {background-color: green;}")))

  # Close the progress when this reactive exits (even if there's an error)
  on.exit(progress$close())

  # TODO: does this really need to be global?
  referralMap <<- makeReferralMap(rxDoneData, input$refMapDate1, input$refMapDate2, progress)

  showNotification("Completed update of treatment map.")

  showNotification("Completed map generation from registry.")

  if (!is.null(referralMap)) plots$activePlot <- referralMap@map

  plots$activePlot
}

referralsMapServer <- function(input, output, session, plots) {
  observeEvent(input$RefreshReferralMap, {
    output$plotReferralMap <- renderLeaflet(generateMapEvent(input, plots))
  })
}

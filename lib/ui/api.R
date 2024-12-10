apiTab <- function() {
  fluidRow(
    box(
      width = 12,
      fluidRow(div(img(src='TARGETPlotterLogo.png', height="60%", width="60%"), style="text-align: center;")),
      fluidRow(tags$br()),
      textInput("inputCastorKey", "User Key",
        value = Sys.getenv("CASTOR_USER_KEY"),
        width = NULL,
        placeholder = NULL
      ),
      textInput("inputCastorSecret", "Secret",
        value = Sys.getenv("CASTOR_SECRET"),
        width = NULL,
        placeholder = NULL
      ),
      column(
        width = 12,
        fluidRow(
          actionButton(inputId = "connectAPI", label = "Connect to API") |> restyleButton(),
          hidden(actionButton(inputId = "disconnectAPI", label = "Disconnect API") |> restyleButton(variant = "secondary")),
        ),
      ),
      tagAppendAttributes(br(), style = "clear:both;"),
      tagAppendAttributes(hr(), style = "clear:both;"),
      hidden(tagAppendAttributes(
        selectInput(inputId = "studyDropdown", label = "Choose TARGET-compatible Study To Load", choices = c()),
        id = "studyDropdownGroup"
      )),
      column(
        width = 4,
        br(),
        hidden(actionButton(
          inputId = "reloadData", label = "No API for Loading",
          icon("remove-circle", lib = "glyphicon"),
        ) |> restyleButton()),
      ),
      column(
        width = 8,
        uiOutput("apiStatus")
      ),
      br()
    )
  )
}

renderAPIStatus <- function(api) {
  renderUI({
    if (api$connected) {
      if (api$loaded) {
        summaryBox2("Connected & Loaded", "API",
          width = 5,
          icon = "fas fa-clipboard-list",
          style = "success"
        )
      } else {
        summaryBox2("Connected, Unloaded", "API",
          width = 5,
          icon = "fas fa-clipboard-list",
          style = "primary"
        )
      }
    } else {
      summaryBox2("Disconnected", "API",
        width = 5,
        icon = "fas fa-clipboard-list",
        style = "danger"
      )
    }
  })
}

connectAPIEvent <- function(input, api) {
  shinyjs::hide("studyDropdownGroup")
  shinyjs::hide("reloadData")
  shinyjs::hide("disconnectAPI")

  # First check we have up to date version (at least 2.x.x)
  castorLibMajorVersion <- strtoi(substr(toString(packageDescription("castoRedc")$Version), 1, 1))
  showNotification(paste("Version of castoRedc Library = ", packageVersion("castoRedc")))
  logger(paste("Version of castoRedc Library = ", packageVersion("castoRedc")))

  if (castorLibMajorVersion < 2) {
    showNotification("Version of castoRedc library incompatiable: Please upgrade")
    logger(paste("Version of castoRedc is incompatiable despite attempted upgrade; expected >2.x.x, got", packageVersion("castoRedc")), stderr = TRUE)
    return()
  }

  # Disconnect the old before connecting
  disconnectCastorAPI()

  api$connected <- F
  api$loaded <- F

  Sys.setenv(CASTOR_USER_KEY = input$inputCastorKey)
  Sys.setenv(CASTOR_SECRET = input$inputCastorSecret)

  connectCastorAPI()

  if (is.environment(castor_api)) {
    api$connected <- T

    shinyjs::show("disconnectAPI")

    showNotification("Castor API Connected.")
  } else {
    api$connected <- F

    shinyjs::hide("disconnectAPI")
    shinyjs::hide("studyDropdownGroup")
    shinyjs::hide("reloadData")

    showNotification("Could not connect to Castor API with those settings")
  }
}

disconnectAPIEvent <- function(api) {
  disconnectCastorAPI()

  api$connected <- F
  api$loaded <- F

  shinyjs::hide("studyDropdownGroup")
  shinyjs::hide("disconnectAPI")
  shinyjs::hide("reloadData")

  showNotification("Castor API Disconnected.")
}

apiConnectedEvent <- function(session, api) {
  if (!api$connected) {
    logger("No API connection")

    studyNames <<- c()

    shinyjs::hide("studyDropdownGroup")
    shinyjs::hide("reloadData")

    updateActionButton(session,
      inputId = "reloadData",
      label = "No API for Loading",
      icon("remove-circle", lib = "glyphicon")
    )

    updateSelectInput(session, "studyDropdown",
      choices = c("No API Connection"),
      selected = NULL
    )

    return()
  }

  logger("Getting study names via Castor API...")

  studyNames <<- getStudyNames()

  if (is.null(studyNames)) {
    showNotification("No studies found! Is Castor API connecting?")
    return()
  }

  logger(paste("API Connected: Found ", length(studyNames[[1]]), " studies", sep = ""))

  updateActionButton(session,
    inputId = "reloadData",
    label = "Load Study Data",
    icon("link", lib = "glyphicon")
  )

  updateSelectInput(session, "studyDropdown",
    choices = studyNames,
    selected = NULL
  )

  shinyjs::show("studyDropdownGroup")
  shinyjs::show("reloadData")
}

reloadStudyEvent <- function(input, output, session, api) {
  if (length(castor_api) == 0) {
    logger("Nothing to do without API Connection...")
    return()
  }

  # Create a Progress object
  progress <- shiny::Progress$new()
  progress$set(message = "Loading & Computing study data...", value = 0.0)

  # Close the progress when this reactive exits (even if there's an error)
  on.exit(progress$close())

  progress$set(message = "Initialising...", value = 0.2)
  initialiseGlobals()

  progress$set(message = "Loading study data...", value = 0.4)
  reloadStudyData(input$studyDropdown)

  if (!ifValidTARGETStudy()) {
    api$loaded <- F
    showNotification("Study data is not valid, either not TARGET study or no patients...")
    logger(paste("Study data '", input$studyDropdown, "' is not valid, either not TARGET study or no patients..."))

    return()
  }

  api$loaded <- T
  showNotification("Valid TARGET study loaded...")
  logger(paste("Valid study data for study '", input$studyDropdown, "'..."))

  progress$set(message = "Processing study data...", value = 0.7)
  processData() # TODO: This should return values which we can then attach to the api reactive value

  ## TODO: Then all of these can be bound to the reactive api processed data value outside of this function

  ## The first step is just move the organFactors and genderFactors to the api
  api$organFactors <- organFactors
  api$genderFactors <- genderFactors

  # Make sure our Organ tick list matches the data...
  updateSelectInput(session, "operatorPlotDropdown", "Operators to Plot",
    choices = operator1Factors
  )
  updateSelectInput(session, "anaesthetistPlotDropdown", "Anaesthetists to Plot",
    choices = anaesthetist1Factors
  )
  updateCheckboxGroupInput(session, "operatorNameCheckbox", "Operator Names",
    choices = operatorAllFactors
  )
  updateCheckboxGroupInput(session, "anaesthetistNameCheckbox", "Anaesthetist Names",
    choices = anaesthetistAllFactors
  )

  # Show the data sidebar items
  # TODO: these should be reactive based on the status of the data
  sapply(c("chartsMenuItem", "tablesMenuItem", "validationMenuItem", "auditMenuItem", "summaryMenuItem", "pathwaySummaryMenuItem"), shinyjs::show)

  # Make the pathway plots...
  # TODO: make reactive based on the status of the processed Data
  makeRxPathwayPlots()
  makeTreatmentVolumePlot(rxDoneData, input$volumePlotDurationRadio)

  progress$set(message = "Completed loading & processing.", value = 1.0)
  showNotification("Completed data processing, plot/tables should now be available to view...")
}

apiServer <- function(input, output, session) {
  api <- reactiveValues(connected = FALSE, loaded = FALSE, organFactors = NULL, genderFactors = NULL)

  output$apiStatus <- renderAPIStatus(api)

  observeEvent(input$connectAPI, disableReenable("connectAPI", connectAPIEvent, input, api))

  observeEvent(input$disconnectAPI, disableReenable("disconnectAPI", disconnectAPIEvent, api))

  # Update the Load Data button
  observe({
    (api$connected)
    apiConnectedEvent(session, api)
  })

  observeEvent(input$reloadData, disableReenable("reloadData", reloadStudyEvent, input, output, session, api))

  api
}

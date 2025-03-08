# Set the environment up
source("lib/__init__.R", .GlobalEnv)

# Clear out any old global variables
initialiseGlobals()
Sys.setenv(CASTOR_USER_KEY = "?")
Sys.setenv(CASTOR_SECRET = "?")
Sys.setenv(CASTOR_DEFAULT_STUDY = NA)
Sys.setenv(CASTOR_URL = "https://uk.castoredc.com")
Sys.setenv(DEBUG_MODE = TRUE)
Sys.setenv(DATE_FORMAT = "%d-%m-%Y")
Sys.setenv(AUDIT_PATHWAY_RMD = "audit/audit-pathway.rmd")
Sys.setenv(AUDIT_PATHWAY_MD = "audit-pathway.md")
Sys.setenv(REPORT_OUTPUT_DIR = "reports")
Sys.setenv(USERKEY_TXT = "../userkey.txt")
Sys.setenv(SECRET_TXT = "../secret.txt")
Sys.setenv(DEFAULT_STUDY_TXT = "../defaultstudy.txt")
theTotalTariff <- 0

if (file.exists(Sys.getenv("SECRET_TXT"))) {
  # Load from the secret password file if present.
  tryCatch(
    Sys.setenv(CASTOR_SECRET = readChar(
      Sys.getenv("SECRET_TXT"), file.info(Sys.getenv("SECRET_TXT"))$size
    ))
  )
}
if (file.exists(Sys.getenv("USERKEY_TXT"))) {
  # Load from the userkey.txt if present.
  tryCatch(
    Sys.setenv(CASTOR_USER_KEY = readChar(
      Sys.getenv("USERKEY_TXT"), file.info(Sys.getenv("USERKEY_TXT"))$size
    ))
  )
}
if (file.exists(Sys.getenv("DEFAULT_STUDY_TXT"))) {
  # Load from the defaultstudy.txt if present.
  tryCatch(
    Sys.setenv(CASTOR_DEFAULT_STUDY = readChar(
      Sys.getenv("DEFAULT_STUDY_TXT"), file.info(Sys.getenv("DEFAULT_STUDY_TXT"))$size
    ))
  )
}

# This should be outside of initialiseGlobals otherwise its always going to be empty at processing
castor_api <<- new.env()

# Initialise these global variables as required to render to UI - FIXME some of these are in initializeGlobals others are not - should they be?
organFactors <<- c()
genderFactors <<- c()
modalityFactors <<- c("Microwave", "Cryotherapy", "Radiofrequency")
operator1Factors <<- c()
anaesthetist1Factors <<- c()
operatorAllFactors <<- c()
anaesthetistAllFactors <<- c()
studyNames <<- c()
referralMap <<- c()

theme <- bslib::bs_theme(version = 4)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "TARGET-UK Plotter dashboard"),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarID",
      menuItem("API Connection", tabName = "api", icon = icon("cog")),
      hidden(tagAppendAttributes(menuItem(
        "Charts",
        id = "chartsID",
        tabName = "charts",
        icon = icon("chart-simple"),
        menuSubItem("Pathway Plots", tabName = "rxPathwayPlots"),
        menuSubItem("Pathway Pies", tabName = "rxPathwayPies"),
        menuSubItem("Wait Times Dashboard", tabName = "waitTimesDashboard"),
        menuSubItem("Operator Plot", tabName = "operatorPlots"),
        menuSubItem("Volume Plot", tabName = "volumePlots"),
        menuSubItem("Recurrence Plot", tabName = "recurrencePlot"),
        menuSubItem("Survival Plot", tabName = "survivalPlot"),
        menuSubItem("Referral Status Plot", tabName = "referralStatus"),
        menuSubItem("Referral Maps", tabName = "referralMaps")
      ), id = "chartsMenuItem")),
      hidden(tagAppendAttributes(menuItem(
        "Data Tables",
        id = "tablesID",
        tabName = "tables",
        icon = icon("table"),
        menuSubItem("Pathway Table", tabName = "rxPathwayTab"),
        menuSubItem("Adverse Events Table", tabName = "aeTab"),
        menuSubItem("Recur./Survival Table", tabName = "survivalTab")
      ), id = "tablesMenuItem")),
      hidden(tagAppendAttributes(menuItem(
        "Data Validation",
        id = "validationID",
        tabName = "validation",
        icon = icon("table"),
        menuSubItem("Operator Names", tabName = "operatorNames"),
        menuSubItem("Anaesthetists Names", tabName = "anaesthetistNames"),
        menuSubItem("Data Integrity Table", tabName = "dataIntegrityTab")
      ), id = "validationMenuItem")),
      hidden(tagAppendAttributes(menuItem(
        "Audit Reports",
        id = "auditID",
        tabName = "audit",
        icon = icon("clipboard-list"),
        expandedName = "AUDIT",
        menuSubItem("Referral Audit Report", tabName = "auditPathway")
      ), id = "auditMenuItem")),
      hidden(tagAppendAttributes(menuItem(
        "Summary Data",
        id = "summaryID",
        tabName = "summary",
        icon = icon("clipboard-list"),
        expandedName = "SUMMARY",
        menuSubItem("Pathway Summary Data", tabName = "rxPathwaySummary"),
        menuSubItem("Recurrence Summary Data", tabName = "recurrenceSummary"),
        menuSubItem("Survival Summary Data", tabName = "survivalSummary")
      ), id = "summaryMenuItem")),
      hidden(tagAppendAttributes(menuItem(
        "Pathway Summary",
        id = "pathwaySummaryID",
        tabName = "summary",
        icon = icon("clipboard-list")
      ), id = "pathwaySummaryMenuItem")),
      menuItem("Diagnostics",
        tabName = "diagnostics", icon = icon("code"),
        menuSubItem("System Logs", tabName = "loggerTab")
      ),
      menuItem("Change Log", tabName = "changeLog", icon = icon("list")),
      menuItem("About", tabName = "about", icon = icon("address-card"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    tabItems(
      tabItem(
        tabName = "api",
        initialiseDetectHeightJS(),
        apiTab()
      ),
      tabItem("rxPathwayPlots", pathwayPlotTab()),
      tabItem("operatorPlots", operatorPlotTab()),
      tabItem("volumePlots", volumePlotTab()),
      tabItem("rxPathwayPies", pathwayPieTab()),
      tabItem("recurrencePlot", recurrencePlotTab()),
      tabItem("survivalPlot", survivalPlotTab()),
      tabItem("referralStatus", referralStatusPlotTab()),
      tabItem("waitTimesDashboard", waitTimesDashboardPlotTab()),
      tabItem("referralMaps", referralMapTab()),
      tabItem("rxPathwayTab", pathwayTab()),
      tabItem("aeTab", aeTab()),
      tabItem("survivalTab", survivalTab()),
      tabItem("dataIntegrityTab", dataIntegrityTab()),
      tabItem("loggerTab", loggerTab()),
      tabItem("auditPathway", auditTab()),
      tabItem("rxPathwaySummary", pathwaySummaryTab()),
      tabItem("recurrenceSummary", "Recurrence Summary Data work in progress!"),
      tabItem("survivalSummary", survivalSummaryTab()),
      tabItem(
        tabName = "test",
        actionButton("msg", "msg"),
        actionButton("warn", "warn"),
        actionButton("err", "err"),
      ),
      tabItem(
        tabName = "changeLog",
        fluidRow(column(
          12, uiOutput("changeLog")
        ))
      ),
      tabItem(
        tabName = "about",
        fluidRow(column(
          12, uiOutput("about")
        ))
      ),
      tabItem(
        tabName = "operatorNames",
        fluidRow(
          tabPanel(
            "OperatorsNames",
            column(
              width = 5,
              checkboxGroupInput(
                "operatorNameCheckbox",
                "Select the variations of same Operator name for name change:",
                choices = operatorAllFactors
              )
            ),
            column(
              width = 5,
              textInput(
                "operatorsNewName",
                "Operators's New Name (e.g. Surname 'Smith'):"
              ),
              box(
                title = "Rename Warning", width = NULL, solidHeader = TRUE, status = "warning",
                "Warning! This cannot be reversed; ensure you really want to do this and have selected correctly before clicking\n"
              ),
              actionButton(
                inputId = "updateOperatorNames", label = "!!",
                style = "color: white;
                       background-color: #EE4B2B;
                       position: relative;
                       left: 3%;
                       height: 35px;
                       width: 35px;
                       text-align:center"
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "anaesthetistNames",
        fluidRow(
          tabPanel(
            "AnaesthetistsNames",
            column(
              width = 5,
              checkboxGroupInput(
                "anaesthetistNameCheckbox",
                "Select the variations of same Anaesthetist name for name change:",
                choices = anaesthetistAllFactors
              )
            ),
            column(
              width = 5,
              textInput(
                "anaesthetistNewName",
                "Anaesthetist New Name (e.g. Surname 'Smith'):"
              ),
              box(
                title = "Rename Warning", width = NULL, solidHeader = TRUE, status = "warning",
                "Warning! This cannot be reversed; ensure you really want to do this and have selected correctly before clicking\n"
              ),
              actionButton(
                inputId = "updateAnaesthetistNames", label = "!!",
                style = "color: white;
                       background-color: #EE4B2B;
                       position: relative;
                       left: 3%;
                       height: 35px;
                       width: 35px;
                       text-align:center"
              )
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  plots <- reactiveValues(activePlot = NULL)
  api <- apiServer(input, output, session)
  tariff <- tariffServer(input, output, session, api)
  pathwayPlotServer(input, output, session, api, plots)
  pathwayPieServer(input, output, session, api, plots)
  pathwayTableServer(input, output, session, isDocker)
  operatorPlotServer(input, output, session, plots)
  volumePlotServer(input, output, session, api, tariff, plots)
  aeTableServer(input, output, session, isDocker)
  recurrencePlotServer(input, output, session, api, plots)
  survivalPlotServer(input, output, session, api, plots)
  survivalTableServer(input, output, session, isDocker)
  dataIntegrityTableServer(input, output, session, isDocker)
  loggerTableServer(input, output, session, isDocker)
  referralStatusPlotServer(input, output, session, api, plots)
  waitTimesDashboardPlotServer(input, output, session, api, plots)
  referralsMapServer(input, output, session, plots)
  pathwaySummaryServer(input, output, session)
  survivalSummaryServer(input, output, session)
  auditServer(input, output, session, api, plots)
  output$tableWait <- DT::renderDataTable({
    DT::datatable(rxWaitData)
  })
  output$tableRx <- DT::renderDataTable({
    DT::datatable(rxDoneData)
  })
  observeEvent(input$msg, {
    logger("Message: ", paste(input$msg))
    shinyCatch(
      {
        message("a message")
      },
      prefix = ""
    )
  })
  observeEvent(input$warn, {
    logger("Warning: ", paste(input$warn))
    shinyCatch(
      {
        warning("a warning")
      },
      prefix = ""
    )
  })
  observeEvent(input$err, {
    logger("Error: ", paste(input$err))
    shinyCatch(
      {
        stop("an error")
      },
      prefix = ""
    )
  })
  output$about <- renderUI({
    htmltools::includeMarkdown("www/TARGETPlotterAbout.md")
  })
  output$changeLog <- renderUI({
    htmltools::includeMarkdown("www/TARGETPlotterChangeLog.md")
  })

  observeEvent(input$updateAnaesthetistNames, {
    updateAnaesthetistNames(
      input$anaesthetistNameCheckbox,
      input$anaesthetistNewName
    )
    if (length(input$anaesthetistNameCheckbox) == 0) {
      shinyCatch(
        {
          message("No anaesthetists selected to update")
        },
        prefix = ""
      )
    } else if (is.na(input$anaesthetistNewName) || input$anaesthetistNewName == "") {
      shinyCatch(
        {
          message("No new anaesthetist name input to update to")
        },
        prefix = ""
      )
    } else {
      warningString <- paste("Warning: You are about to update anaesthetists names '", paste(input$anaesthetistNameCheckbox, collapse = ","), "' to '", input$anaesthetistNewName, "'. This change is not reversible. You must use unique name otherwise your data will be diluted. To proceed with the change type: 'Proceed' (case-sensitive) otherwise press Esc.", sep = "")
      shinyalert(
        warningString,
        type = "input",
        callbackR = updateAnaesthetistNamesCallback,
        showCancelButton = TRUE
      )
    }
  })

  # This is the callback to ensure we are definately going to update the anaesthetist data
  updateAnaesthetistNamesCallback <- function(textEntered) {
    if (!is.na(textEntered) && textEntered == "Proceed") {
      # Create a Progress object
      progress <- shiny::Progress$new()
      progress$set(message = "Updating Castor Data...", value = 0.5)

      # Close the progress when this reactive exits (even if there's an error)
      on.exit(progress$close())

      updateAnaesthetistNames(
        targetStudyID,
        input$anaesthetistNameCheckbox,
        input$anaesthetistNewName
      )
      progress$set(message = "Completed Data Update", value = 1.0)
      shinyalert(title = "Completed Data Update: Please reload data to see reflected changes.", type = "success")
    } else {
      if (is.na(textEntered)) {
        textEntered <- ""
      }
      # Do nothing
      errMsg <- paste("The anaesthetists were not updated as the user typed '", textEntered, "'", sep = "")
      logger(errMsg)
      shinyalert(title = errMsg, type = "error")
    }
  }

  observeEvent(input$updateOperatorNames, {
    if (length(input$operatorNameCheckbox) == 0) {
      shinyCatch(
        {
          message("No operators selected to update")
        },
        prefix = ""
      )
    } else if (is.na(input$operatorsNewName) || input$operatorsNewName == "") {
      shinyCatch(
        {
          message("No new operator name input to update to")
        },
        prefix = ""
      )
    } else {
      warningString <- paste("Warning: You are about to update operator names '", paste(input$operatorNameCheckbox, collapse = ","), "' to '", input$operatorsNewName, "'. This change is not reversible. You must use unique name otherwise your data will be diluted. To proceed with the change type: 'Proceed' (case-sensitive) otherwise press Esc.", sep = "")
      shinyalert(
        warningString,
        type = "input",
        callbackR = updateOperatorNamesCallback,
        showCancelButton = TRUE
      )
    }
  })

  # This is the callback to ensure we are definately going to update the operator data
  updateOperatorNamesCallback <- function(textEntered) {
    if (!is.na(textEntered) && textEntered == "Proceed") {
      # Create a Progress object
      progress <- shiny::Progress$new()
      progress$set(message = "Updating Castor Data...", value = 0.5)

      # Close the progress when this reactive exits (even if there's an error)
      on.exit(progress$close())

      updateOperatorNames(
        targetStudyID,
        input$operatorNameCheckbox,
        input$operatorsNewName
      )

      progress$set(message = "Completed Data Update", value = 1.0)
      shinyalert(title = "Completed Data Update: Please reload data to see reflected changes.", type = "success")
    } else {
      if (is.na(textEntered)) {
        textEntered <- ""
      }
      # Do nothing
      errMsg <- paste("The operators were not updated as the user typed '", textEntered, "'", sep = "")
      logger(errMsg)
      shinyalert(title = errMsg, type = "error")
    }
  }
}

# Open the GUI
shinyApp(ui = ui, server = server)

# Useful stuff
# print (studyData)
# print colnames(studyData)
# print (studyData[]$ref_date_recd_1)

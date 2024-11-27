# Set the environment up
source("lib/__init__.R", .GlobalEnv)

# Clear out any old global variables
initialiseGlobals()

Sys.setenv(CASTOR_USER_KEY   = "?")
Sys.setenv(CASTOR_SECRET     = "?")
Sys.setenv(CASTOR_URL        = "https://uk.castoredc.com")
Sys.setenv(DEBUG_MODE        = TRUE)
Sys.setenv(DATE_FORMAT       = "%d-%m-%Y")
Sys.setenv(AUDIT_PATHWAY_RMD = "audit/audit-pathway.rmd")
Sys.setenv(AUDIT_PATHWAY_MD  = "audit-pathway.md")
theTotalTariff <- 0

if (file.exists("secret.txt")) {
  # Load from the secret.txt if present.
  tryCatch(
    Sys.setenv(CASTOR_SECRET = readChar(
      "secret.txt", file.info("secret.txt")$size
    ))
  )
}
if (file.exists("userkey.txt")) {
  # Load from the userkey.txt if present.
  tryCatch(
    Sys.setenv(CASTOR_USER_KEY = readChar(
      "userkey.txt", file.info("userkey.txt")$size
    ))
  )
}

# This should be outside of initialiseGlobals otherwise its always going to be empty at processing
castor_api      <<- new.env()

# Initialise these global variable as required to render to UI
organFactors           <<- c()
modalityFactors        <<- c('Microwave','Cryotherapy','Radiofrequency')
operator1Factors       <<- c()
anaesthetist1Factors   <<- c()
operatorAllFactors     <<- c()
anaesthetistAllFactors <<- c()
studyNames             <<- c()
referralMap            <<- c()

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
        menuSubItem("Pathway Plots",   tabName = "rxpathwayplots"),
        menuSubItem("Pathway Pies",    tabName = "rxpathwaypies"),
        menuSubItem("Operator Plot",   tabName = "operatorplots"),
        menuSubItem("Volume Plot",     tabName = "volumeplots"),
        menuSubItem("Recurrence Plot", tabName = "recurrenceplot"),
        menuSubItem("Survival Plot",   tabName = "survivalplot"),
        menuSubItem("Referral Status Plot", tabName = "referralstatus"),
        menuSubItem("Referral Maps",   tabName = "referralmaps")
      ), id='chartsMenuItem')),
      hidden(tagAppendAttributes(menuItem(
        "Data Tables",
        id = "tablesID",
        tabName = "tables",
        icon = icon("table"),
        menuSubItem("Pathway Table",    tabName = "rxpathwaytab"),
        menuSubItem("Recurrence Table", tabName = "recurrencetab"),
        menuSubItem("Survival Table",   tabName = "survivaltab")
      ), id='tablesMenuItem')),
      hidden(tagAppendAttributes(menuItem(
        "Data Validation",
        id = "validationID",
        tabName = "validation",
        icon = icon("table"),
        menuSubItem("Operator Names",      tabName = "operatorNames"),
        menuSubItem("Anaesthetists Names", tabName = "anaesthetistNames")
      ), id='validationMenuItem')),
      hidden(tagAppendAttributes(menuItem(
        "Audit Reports",
        id = "auditID",
        tabName = "audit",
        icon = icon("clipboard-list"),
        expandedName = "AUDIT",
        menuSubItem("Referral Audit Report",   tabName = "audit-pathway")
      ), id='auditMenuItem')),
      hidden(tagAppendAttributes(menuItem(
        "Summary Data",
        id = "summaryID",
        tabName = "summary",
        icon = icon("clipboard-list"),
        expandedName = "SUMMARY",
        menuSubItem("Pathway Summary Data",    tabName = "rxpathwaysummary"),
        menuSubItem("Recurrence Summary Data", tabName = "recurrencesummary"),
        menuSubItem("Survival Summary Data",   tabName = "survivalsummary")
      ), id='summaryMenuItem')),
      hidden(tagAppendAttributes(menuItem(
        "Pathway Summary",
        id = "pathwaySummaryID",
        tabName = "summary",
        icon = icon("clipboard-list")
      ), id='pathwaySummaryMenuItem')),
      menuItem("Test", tabName = "test", icon = icon("code")),
      menuItem("Change Log", tabName = "changeLog", icon = icon("list")),
      menuItem("About", tabName = "about", icon = icon("address-card"))
    )
  ),

  dashboardBody(
    useShinyjs(),
    tabItems(
      tabItem(tabName = "api", apiTab()),
      tabItem(tabName = "rxpathwayplots", pathwayPlotTab()),
      tabItem(tabName = "operatorplots", operatorPlotTab()),
      tabItem(tabName = "volumeplots", volumePlotTab()),
      tabItem(tabName = "rxpathwaypies", pathwayPieTab()),
      tabItem("recurrenceplot", recurrencePlotTab()),
      tabItem("survivalplot", survivalPlotTab()),
      tabItem("referralstatus", referralStatusPlotTab()),
      tabItem("referralmaps", referralMapTab()),
      tabItem("rxpathwaytab", pathwayTab()),
      tabItem("recurrencetab", recurrenceTab()),
      tabItem("survivaltab", survivalTab()),
      tabItem("audit-pathway", auditTab()),
      tabItem("rxpathwaysummary", pathwaySummaryTab()),
      tabItem("recurrencesummary", "Recurrence Summary Data work in progress!"),
      tabItem("survivalsummary", survivalSummaryTab()),

      tabItem(
        tabName = "test",
        actionButton("msg", "msg"),
        actionButton("warn", "warn"),
        actionButton("err", "err"),
      ),

      tabItem(tabName = "changeLog",
              fluidRow(column(
                12, uiOutput('changeLog')
              ))),

      tabItem(tabName = "about",
              fluidRow(column(
                12, uiOutput('about')
              ))),

      tabItem(tabName = "operatorNames",
              fluidRow(
                tabPanel(
                  "OperatorsNames",
                  column(
                    width = 5,
                    checkboxGroupInput(
                      "operatorNameCheckbox",
                      "Select the variations of same Operator name for name change:",
                      choices = operatorAllFactors
                    )),
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
                    actionButton(inputId = "updateOperatorNames", label = "!!",
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

      tabItem(tabName = "anaesthetistNames",
              fluidRow(
                tabPanel(
                  "AnaesthetistsNames",
                  column(
                    width = 5,
                    checkboxGroupInput(
                      "anaesthetistNameCheckbox",
                      "Select the variations of same Anaesthetist name for name change:",
                      choices = anaesthetistAllFactors
                    )),
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
                    actionButton(inputId = "updateAnaesthetistNames", label = "!!",
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
  pathwayServer(input, output, session, plots)
  pathwayPlotServer(input, output, session, plots)
  operatorPlotServer(input, output, session, plots)
  pathwayPieServer(input, output, session, plots)
  volumePlotServer(input, output, session, tariff, plots)
  referralStatusPlotServer(input, output, session, api, plots)
  survivalPlotServer(input, output, session, plots)
  referralsMapServer(input, output, session, plots)
  recurrenceServer(input, output, session)
  survivalServer(input, output, session)
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
    shinyCatch({
      message("a message")
    }, prefix = '')
  })
  observeEvent(input$warn, {
    shinyCatch({
      warning("a warning")
    }, prefix = '')
  })
  observeEvent(input$err, {
    shinyCatch({
      stop("an error")
    }, prefix = '')
  })
  output$about <- renderUI({
    htmltools::includeMarkdown('www/TARGETPlotterAbout.md')
  })
  output$changeLog <- renderUI({
    htmltools::includeMarkdown('www/TARGETPlotterChangeLog.md')
  })

  observeEvent(input$updateAnaesthetistNames, {
    updateAnaesthetistNames(input$anaesthetistNameCheckbox,
      input$anaesthetistNewName)
    if (length(input$anaesthetistNameCheckbox)==0)
    {
      shinyCatch({
        message("No anaesthetists selected to update")
      }, prefix = '')
    }
    else if(is.na(input$anaesthetistNewName) || input$anaesthetistNewName == "")
    {
      shinyCatch({
        message("No new anaesthetist name input to update to")
      }, prefix = '')
    }
    else
    {
      warningString = paste("Warning: You are about to update anaesthetists names '",paste(input$anaesthetistNameCheckbox,collapse=','),"' to '",input$anaesthetistNewName,"'. This change is not reversible. You must use unique name otherwise your data will be diluted. To proceed with the change type: 'Proceed' (case-sensitive) otherwise press Esc.",sep="")
      shinyalert(
        warningString, type = "input",
        callbackR = updateAnaesthetistNamesCallback,
        showCancelButton = TRUE
      )
    }
  })

  # This is the callback to ensure we are definately going to update the anaesthetist data
  updateAnaesthetistNamesCallback <- function(textEntered)
  {
    if (!is.na(textEntered) && textEntered == 'Proceed')
    {
      # Create a Progress object
      progress <- shiny::Progress$new()
      progress$set(message = "Updating Castor Data...", value = 0.5)

      # Close the progress when this reactive exits (even if there's an error)
      on.exit(progress$close())

      updateAnaesthetistNames(targetStudyID,
                          input$anaesthetistNameCheckbox,
                          input$anaesthetistNewName)
      progress$set(message = "Completed Data Update", value = 1.0)
      shinyalert(title = "Completed Data Update: Please reload data to see reflected changes.", type = "success")
    }
    else
    {
      if (is.na(textEntered))
      {
        textEntered <- ""
      }
      # Do nothing
      errMsg <- paste("The anaesthetists were not updated as the user typed '",textEntered,"'",sep="")
      logger(errMsg)
      shinyalert(title = errMsg, type = "error")
    }
  }

  observeEvent(input$updateOperatorNames, {
    if (length(input$operatorNameCheckbox)==0)
    {
      shinyCatch({
        message("No operators selected to update")
      }, prefix = '')
    }
    else if(is.na(input$operatorsNewName) || input$operatorsNewName == "")
    {
      shinyCatch({
        message("No new operator name input to update to")
      }, prefix = '')
    }
    else
    {
      warningString = paste("Warning: You are about to update operator names '",paste(input$operatorNameCheckbox,collapse=','),"' to '",input$operatorsNewName,"'. This change is not reversible. You must use unique name otherwise your data will be diluted. To proceed with the change type: 'Proceed' (case-sensitive) otherwise press Esc.",sep="")
      shinyalert(
        warningString, type = "input",
        callbackR = updateOperatorNamesCallback,
        showCancelButton = TRUE
      )
    }
  })

  # This is the callback to ensure we are definately going to update the operator data
  updateOperatorNamesCallback <- function(textEntered)
  {
    if (!is.na(textEntered) && textEntered == 'Proceed')
    {
      # Create a Progress object
      progress <- shiny::Progress$new()
      progress$set(message = "Updating Castor Data...", value = 0.5)

      # Close the progress when this reactive exits (even if there's an error)
      on.exit(progress$close())

      updateOperatorNames(targetStudyID,
                          input$operatorNameCheckbox,
                          input$operatorsNewName)

      progress$set(message = "Completed Data Update", value = 1.0)
      shinyalert(title = "Completed Data Update: Please reload data to see reflected changes.", type = "success")
    }
    else
    {
      if (is.na(textEntered))
      {
        textEntered <- ""
      }
      # Do nothing
      errMsg <- paste("The operators were not updated as the user typed '",textEntered,"'",sep="")
      logger(errMsg)
      shinyalert(title = errMsg, type = "error")
    }
  }
}

# Open the GUI
shinyApp(ui = ui, server = server)

# Useful stuff
#print (studyData)
#print colnames(studyData)
#print (studyData[]$ref_date_recd_1)

# Install the required packages if not already installed
required_pkgs <- c(
  "remotes",
  "httr",
  "rapiclient",
  "dplyr",
  "jsonlite",
  "shiny",
  "shinyjs",
  "shinydashboard",
  "shinyalert",
  "markdown",
  "DT",
  "ggplot2",
  "plotly",
  "ggrepel",
  "tidyverse",
  "rmarkdown",
  "here",
  "survival",
  "survminer",
  "knitr",
  "kableExtra",
  "RColorBrewer",
  "sf",           # for mapping
  "mapview",
  "PostcodesioR",
  "leaflet" # to render the mapview
  
  ) # Consider "tinytex" to generate pdf?

for (pkg in required_pkgs) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Install github packages
# Please note that the castorRedc library updated form 1.1.0 to 2.1.0 from 2024...
githubPkgs <- c("castoredc/castoRedc", "deepanshu88/summaryBox")

for (pkg in githubPkgs) {
  if (!require(sub(".*/", "", pkg), character.only = TRUE)) {
    remotes::install_github(pkg, quiet = FALSE,  host = "api.github.com")
  }
  library(sub(".*/", "", pkg), character.only = TRUE)
}

# Check we have the latest Castor library, if old then force update it!
castorLibMajorVersion <- strtoi(substr(toString(packageDescription("castoRedc")$Version),1,1))
if (castorLibMajorVersion < 2)
{
  write(paste("Attempting update of castoRedc library (currently version ", packageVersion("castoRedc"),")..."),stderr())
  detach(package:castoRedc, unload=TRUE)
  remove.packages("castoRedc")
  remotes::install_github("castoredc/castoRedc", host = "api.github.com" , upgrade="always")
}

# If we ever need to back date a library e.g. compatability, this is how we do it:
#remove.packages("yaml")
#install.packages("https://cran.r-project.org/src/contrib/Archive/yaml/yaml_2.3.8.tar.gz", repos = NULL, type = "source")


# Set the environment up
source("lib/__init__.R")
source("targetPlotterLib.R") # This can't be in lib folder as it sources all .R files in there

# Clear out any old global variables
rm(list = ls())
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

# Knit the audit scripts, currently only one
rmdAuditFiles <- c(Sys.getenv("AUDIT_PATHWAY_RMD"))

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
        expandedName = "CHARTS",
        menuSubItem("Pathway Plots",   tabName = "rxpathwayplots"),
        menuSubItem("Pathway Pies",    tabName = "rxpathwaypies"),
        menuSubItem("Operator Plot",   tabName = "operatorplots"),
        menuSubItem("Volume Plot",     tabName = "volumeplots"),
        menuSubItem("Recurrence Plot", tabName = "recurrenceplot"),
        menuSubItem("Survival Plot",   tabName = "survivalplot"),
        menuSubItem("Referral Maps",   tabName = "referralmaps")
      ), id='chartsMenuItem')),
      hidden(tagAppendAttributes(menuItem(
        "Data Tables",
        id = "tablesID",
        tabName = "tables",
        icon = icon("table"),
        expandedName = "TABLES",
        menuSubItem("Pathway Table",    tabName = "rxpathwaytab"),
        menuSubItem("Recurrence Table", tabName = "recurrencetab"),
        menuSubItem("Survival Table",   tabName = "survivaltab")
      ), id='tablesMenuITem')), 
      hidden(tagAppendAttributes(menuItem(
        "Data Validation",
        id = "validationID",
        tabName = "validation",
        icon = icon("table"),
        expandedName = "VALIDATION",
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
      menuItem("About", tabName = "about", icon = icon("address-card")),
      
      hidden(menuItem("hiddenCharts",  tabName = "hiddenCharts")),
      hidden(menuItem("hiddenTables",  tabName = "hiddenTables")),
      hidden(menuItem("hiddenSummary", tabName = "hiddenSummary"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    tabItems(
      tabItem(tabName = "api",
              fluidRow(
                box(
                  width = 12,
                  
                  textInput(
                    "inputCastorKey",
                    "User Key",
                    value = Sys.getenv("CASTOR_USER_KEY"),
                    width = NULL,
                    placeholder = NULL
                  ),
                  textInput(
                    "inputCastorSecret",
                    "Secret",
                    value = Sys.getenv("CASTOR_SECRET"),
                    width = NULL,
                    placeholder = NULL
                  ),
                  
                  hidden(tagAppendAttributes(selectInput(
                    inputId = "studyDropdown",
                    label = "Choose TARGET-compatible Study To Load",
                    choices = c()
                  ), id="studyDropdownGroup")),
                  hr(),
                  
                  column(
                    width = 4,
                    actionButton(inputId = "connectAPI",   label = "Connect to API"),
                    br(),
                    br(),
                    actionButton(inputId = "disconnectAPI", label = "Disconnect API"),
                    br(),
                    br(),
                    hidden(actionButton(
                      inputId = "reloadData",
                      label = "No API for Loading",
                      icon("remove-circle", lib = "glyphicon"),
                      style = "color: #ddd; background-color: #337ab7; border-color: #2e6da4"
                    )),
                    br(),
                  ),
                  column(width = 8,
                         uiOutput("apiStatus")),
                  br()
                  
                )
              )),
      
      tabItem(tabName = "rxpathwayplots",
              fluidRow(
                tabPanel(
                  "RxPathwayPlots",
                  column(
                    width = 3,
                    radioButtons(
                      "rxTimesPlotRadio",
                      "Pathway Plot Type",
                      c("Treated Plot" = "rxdonePlot",
                        "Waiting Plot" = "rxwaitPlot")
                    )
                  ),
                  column(
                    width = 3,
                    dateInput(
                      "rxPlotDate1",
                      "Start Date:",
                      format = "dd/mm/yyyy",
                      value = Sys.Date() - 365
                    ),
                    dateInput(
                      "rxPlotDate2",
                      "End Date:",
                      format = "dd/mm/yyyy",
                      value = Sys.Date()
                    )
                  ),
                  column(
                    width = 6,
                    checkboxGroupInput(
                      "organRxPlotCheckbox",
                      "Organs to Plot",
                      choices = organFactors,
                      selected = organFactors
                    ),
                  ),
                  column(
                    width = 3,
                    actionButton(inputId = "refreshRxPlot", label = "Refresh Plot")
                  )
                )
              ),
              fluidRow(box(
                width = 12,
                plotlyOutput("plotRxPathway")
              ))),
      
      tabItem(tabName = "operatorplots",
              fluidRow(
                tabPanel(
                  "OperatorPlots",
                  column(
                    width = 3,
                    dateInput(
                      "operatorPlotDate1",
                      "Start Date:",
                      format = "dd/mm/yyyy",
                      value = Sys.Date() - 365
                    ),
                    dateInput(
                      "operatorPlotDate2",
                      "End Date:",
                      format = "dd/mm/yyyy",
                      value = Sys.Date()
                    )
                  ),
                  column(
                    width = 6,
                    selectInput(
                      "operatorPlotDropdown",
                      "Operators to Plot",
                      choices = operator1Factors
                    ),
                    selectInput(
                      "anaesthetistPlotDropdown",
                      "Anaesthetists to Plot",
                      choices = anaesthetist1Factors
                    ),
                  ),
                  column(
                    width = 3,
                    actionButton(inputId = "refreshOperatorPlot", label = "Refresh Plot")
                  )
                )
              ),
              fluidRow(box(
                width = 12,
                plotlyOutput("plotOperators")
              ))),
      
      tabItem(tabName = "volumeplots",
              fluidRow(
                tabPanel(
                  "VolumePlots",
                  column(
                    width = 3,
                    dateInput(
                      "volumePlotDate1",
                      "Start Date:",
                      format = "dd/mm/yyyy",
                      value = Sys.Date() - 365
                    ),
                    dateInput(
                      "volumePlotDate2",
                      "End Date:",
                      format = "dd/mm/yyyy",
                      value = Sys.Date()
                    ),
                    actionButton(inputId = "refreshVolumePlot", label = "Refresh Plot")
                  ),
                  column(
                    width = 3,
                    checkboxGroupInput(
                      "organVolumePlotCheckbox",
                      "Organs to Plot",
                      choices = organFactors,
                      selected = organFactors
                    ),
                  ),
                  column(
                    width = 3,
                    checkboxGroupInput(
                      "modalityVolumePlotCheckbox",
                      "Modalities to Plot",
                      choices = modalityFactors,
                      selected = modalityFactors
                    ),
                  ),
                  
                  column(
                    width = 3,
                    radioButtons(
                      "volumePlotDurationRadio",
                      "Duration of Plot",
                      c("Weekly"  = "week",
                        "Monthly" = "month",
                        "Yearly"  = "year")
                    )
                  ),
                  column(
                    width = 3,
                  ),
                  column(
                    width = 6,
                     uiOutput("totalTariff"),
                  )
                )
              ),
              fluidRow(box(
                width = 12,
                plotlyOutput("plotVolume")
              ))),
      
      tabItem(tabName = "rxpathwaypies",
              fluidRow(
                tabPanel(
                  "RxPathwayPies",
                  column(
                    width = 3,
                    radioButtons(
                      "rxTimesPieRadio",
                      "Pathway Pie Chart Type",
                      c("Treated Pie"  = "rxdonePie",
                        "Waiting Pie"  = "rxwaitPie")
                    )
                  ),
                  column(
                    width = 3,
                    dateInput(
                      "rxPieDate1",
                      "Start Date:",
                      format = "dd/mm/yyyy",
                      value = Sys.Date() - 365
                    ),
                    dateInput(
                      "rxPieDate2",
                      "End Date:",
                      format = "dd/mm/yyyy",
                      value = Sys.Date()
                    )
                  ),
                  column(
                    width = 3,
                    checkboxGroupInput(
                      "organPieCheckbox",
                      "Organs to Chart",
                      choices = organFactors,
                      selected = organFactors
                    ),
                  ),
                  column(
                    width = 3,
                    actionButton(inputId = "refreshRxPie", label = "Refresh Pie Chart")
                  )
                )
              ),
              fluidRow(box(
                width = 12,
                plotOutput("pieRxPathway")
              ))),
      
      tabItem("recurrenceplot",
              fluidRow(
                box(
                  width = 12,
                  radioButtons(
                    "recurrencePlotRadio",
                    "Recurrence Analysis",
                    c("By Sex" = "recurrencePlotSex", "By Organ" =
                        "recurrencePlotOrgan")
                  ),
                  actionButton(inputId = "refreshRecurrencePlot", label = "Refresh Plot")
                  
                ),
                tabPanel("RecurrencePlot",
                         plotOutput("plotRecurrenceCurve"))
              )),
      tabItem("survivalplot",
              fluidRow(
                box(
                  width = 12,
                  radioButtons(
                    "survivalPlotRadio",
                    "Survival Analysis",
                    c("By Sex" = "survivalPlotSex", "By Organ" =
                        "survivalPlotOrgan")
                  ),
                  actionButton(inputId = "refreshSurvivalPlot", label = "Refresh Plot"),
                  
                ),
                tabPanel("SurvivalPlot",
                         plotOutput("plotSurvivalCurve"))
              )),
      
      tabItem("referralmaps",
              wellPanel(fluidRow(
                column(3,
                  dateInput(
                    "refMapDate1",
                    "Start Date:",
                    format = "dd/mm/yyyy",
                    value = Sys.Date() - 365*5
                  )),
                column(3,
                  dateInput(
                    "refMapDate2",
                    "End Date:",
                    format = "dd/mm/yyyy",
                    value = Sys.Date()
                  )),
                column(5,
                  actionButton(inputId = "RefreshReferralMap", label = "Generate New Map Between Dates", style = "height: 60px")
                )
              )),
              fluidRow(
                tabPanel("ReferralMap",
                         leafletOutput("plotReferralMap", height = "550px")
                ))
      ),
      
      tabItem("rxpathwaytab",
              fluidRow(box(
                width = 12,
                column(
                  width = 4,
                  radioButtons(
                    "rxTimesTableRadio",
                    "Pathway Table Type",
                    c("Treated" = "rxdoneTable", "Waiting" = "rxwaitTable")
                  )
                ),
                column(
                  width = 4,
                  actionButton("buttonPasteRxTimesData", "Copy Data to Clipboard"),
                  actionButton("buttonSaveRxTimesData",  "Save Data to File")
                )
              )),
              fluidRow(box(
                width = 12,
                DT::dataTableOutput("tableRxPathway")
              ))),
      
      tabItem("recurrencetab",
              fluidRow(box(
                width = 12,
                column(
                  width = 4,
                  actionButton("buttonPasteRecurrenceData", "Copy Data to Clipboard"),
                  actionButton("buttonSaveRecurrenceData",  "Save Data to File")
                )
              )),
              
              fluidRow(box(
                width = 12,
                DT::dataTableOutput("tableRecurrence")
              ))),
      
      tabItem("survivaltab",
              fluidRow(box(
                width = 12,
                column(
                  width = 4,
                  actionButton("buttonPasteSurvivalData", "Copy Data to Clipboard"),
                  actionButton("buttonSaveSurvivalData",  "Save Data to File")
                )
              )),
              
              fluidRow(box(
                width = 12,
                DT::dataTableOutput("tableSurvival")
              ))),
      
      tabItem(
        "audit-pathway",
        fluidRow(tabPanel(
          "AuditPathway",
          
          column(
            width = 3,
            dateInput(
              "auditDate1",
              "Start Date:",
              format = "dd/mm/yyyy",
              value = Sys.Date() - 365
            ),
            dateInput(
              "auditDate2",
              "End Date:",
              format = "dd/mm/yyyy",
              value = Sys.Date()
            )
          ),
          column(
            width = 3,
            checkboxGroupInput(
              "organAuditCheckbox",
              "Organs to Audit",
              choices = organFactors,
              selected = organFactors
            ),
          ),
          column(
            width = 3,
            actionButton(inputId = "runAuditReport", label = "Run Audit Report")
          )
        )),
        wellPanel(style = "background: white",
                  fluidRow(fluidPage(
                    htmlOutput("summaryRefAudit")
                  )))
      ),
      
      tabItem("rxpathwaysummary",
              fluidRow(
                tabPanel("Waiting List Summary",
                         verbatimTextOutput("summaryWaitData")),
                tabPanel("Treated List Summary",
                         verbatimTextOutput("summaryRxData"))
              )),
      tabItem("recurrencesummary", "Recurrence Summary Data work in progress!"),
      tabItem(
        "survivalsummary",
        verbatimTextOutput("summarySurvivalData")
      ),
      
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
  api   <- reactiveValues(connected = FALSE, loaded = FALSE)
  tariff <- reactiveValues(theTotalTariff = 0)
  
  output$apiStatus <- renderUI(
  {
    if (api$connected)
    {
      if (api$loaded)
      {
        summaryBox2(
          "Connected & Loaded",
          "API",
          width = 5,
          icon = "fas fa-clipboard-list",
          style = "success"
        )
      }
      else
      {
        summaryBox2(
          "Connected, Unloaded",
          "API",
          width = 5,
          icon = "fas fa-clipboard-list",
          style = "primary"
        )
      }
    }
    else
    {
      summaryBox2(
        "Disconnected",
        "API",
        width = 5,
        icon = "fas fa-clipboard-list",
        style = "danger"
      )
    }
  })
    
  # This is the income generated from the volume of ablations which have taken place on the volume plot
  output$totalTariff <- renderUI(
  {
    if (tariff$theTotalTariff > 1000000)
    {
      totalTariffText <- paste("£", round(tariff$theTotalTariff/1000000,2), "M", sep="")
    }
    else if (tariff$theTotalTariff > 10000)
    {
      totalTariffText <- paste("£", round(tariff$theTotalTariff/1000,2), "K", sep="")
    }
    else
    {
      totalTariffText <- paste("£", tariff$theTotalTariff, sep="")
    }
    
    if (api$connected && api$loaded)
    {
      summaryBox2(
        "HRG-coded Total Tariff",
        totalTariffText,
        width = 6,
        icon = "fas fa-pound-sign",
        style = "success"
      )
    }
    else
    {
      summaryBox2(
        "Load Data for Income",
        "£0",
        width = 5,
        icon = "fas fa-pound-sign",
        style = "primary"
      )
    }
  })

  # Keeps sidebar Charts submenu expanded rather than instant collapse
  observeEvent(input$sidebarItemExpanded, {
    if (input$sidebarItemExpanded == "CHARTS") {
      updateTabItems(session, "sidebarID", selected = "hiddenCharts")
    }
  })
  # Keeps sidebar Tables submenu expanded rather than instant collapse
  observeEvent(input$sidebarItemExpanded, {
    if (input$sidebarItemExpanded == "TABLES") {
      updateTabItems(session, "sidebarID", selected = "hiddenTables")
    }
  })
  # Keeps sidebar Summaries submenu expanded rather than instant collapse
  observeEvent(input$sidebarItemExpanded, {
    if (input$sidebarItemExpanded == "SUMMARY") {
      updateTabItems(session, "sidebarID", selected = "hiddenSummary")
    }
  })
  
  # Update the Load Data button
  observe({
    (api$connected)
    if (api$connected)
    {
      logger("Getting study names via Castor API...")
      studyNames <<- getStudyNames()
      if (is.null(studyNames))
      {
        showNotification("No studies found! Is Castor API connecting?")
      }
      else
      {
        logger(paste("API Connected: Found ",length(studyNames[[1]])," studies",sep=""))
        shinyjs::show('studyDropdownGroup')
        shinyjs::show('reloadData')
        updateActionButton(session,
                           inputId = "reloadData",
                           label = "Load Study Data",
                           icon("link", lib = "glyphicon"))
        updateSelectInput(session,
                          "studyDropdown",
                          choices = studyNames,
                          selected = NULL)
      }
    }
    else
    {
      logger("No API connection")
      studyNames <<- c()
      updateActionButton(
        session,
        inputId = "reloadData",
        label = "No API for Loading",
        icon("remove-circle", lib = "glyphicon")
      )
      updateSelectInput(
        session,
        "studyDropdown",
        choices = c("No API Connection"),
        selected = NULL
      )
      shinyjs::hide('studyDropdownGroup')
      shinyjs::hide('reloadData')
    }
  })
  
  finalRxPlotInput <- reactive({
    switch(input$rxTimesPlotRadio,
           "rxdonePlot" = rxdonePlot,
           "rxwaitPlot" = rxwaitPlot)
  })
  
  finalOperatorPlotInput <- reactive({
     operatorPlot
  })
  
  finalVolumePlotInput <- reactive({
    volumePlot
  })
  
  finalRxPieInput <- reactive({
    switch(input$rxTimesPieRadio,
           "rxdonePie" = rxdonePie,
           "rxwaitPie" = rxwaitPie)
  })
  
  finalRxDataInput <- reactive({
    switch(input$rxTimesPlotRadio,
           "rxdonePlot" = rxDoneData,
           "rxwaitPlot" = rxWaitData)
  })
  
  finalRxTableDataInput <- reactive({
    switch(input$rxTimesTableRadio,
           "rxdoneTable" = rxDoneData,
           "rxwaitTable" = rxWaitData)
  })
  
  finalSurvivalPlotInput <- reactive({
    switch(
      input$survivalPlotRadio,
      "survivalPlotSex" = survivalPlotSex,
      "survivalPlotOrgan" = survivalPlotOrgan
    )
  })
  
  finalRecurrencePlotInput <- reactive({
    recurrencePlotOrgan
  })
  
  regenerateReferralMap <- function(force = F)
  {
    showNotification("Generating/Refreshing treatment map ...")
    progress <- shiny::Progress$new()
    tags$head(tags$style(HTML('.progress-bar {background-color: green;}')))
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())
    makeReferralMap(rxDoneData,input$refMapDate1,input$refMapDate2,progress)
    showNotification("Completed update of treatment map.")
  }
  
  finalRefAuditInput <- reactive({
    # This does the knitting bit ready to make the HTML by running the knit function
    sapply(rmdAuditFiles, knit, quiet = T)

    # This makes the MD file which is basically just HTML in a file
    htmltools::includeMarkdown(Sys.getenv("AUDIT_PATHWAY_MD"))
    
    # There seem to be many ways to skin this particular cat...
    
    # This makes PDF (badly)
    #rmarkdown::render(Sys.getenv("AUDIT_PATHWAY_MD"), params = list(audit_start_date=input$auditDate1))
    
    # This doesn't seem to make a file...
    #markdownToHTML(Sys.getenv("AUDIT_PATHWAY_MD", 'test.html'))
    
    # This falls over with app within an app
    #rmarkdown::run(Sys.getenv("AUDIT_PATHWAY_MD"))
    
    # This generates a PDF but it doesn't have same format at HTML and has missing plots etc...
    # The refresh doesn't seem to work either... sigh
    #markdown::render(Sys.getenv("AUDIT_PATHWAY_MD"), output_format = "pdf_document")
    
    # This is for PDFs rather than HTML
    #rmarkdown::render(
    #  Sys.getenv("AUDIT_PATHWAY_MD"),
    #  params = list(audit_start_date = input$auditDate1, audit_end_date = input$auditDate2),
    #  envir = parent.frame()
    #)
    #})
  })
  
  # Plot the Rx pathway plot using the date range and organ filters
  # Note plotly vs. plot gives you the tool tip text
  output$plotRxPathway <- renderPlotly({
    if (!is.list(finalRxTableDataInput()))
    {
      p <- plot.new()
    }
    else
    {
      p <- finalRxPlotInput() +
        scale_x_date(limits = as.Date(c(
          input$rxPlotDate1, input$rxPlotDate2
        ), format = "%d/%m/%Y")) +
        theme(legend.position = "bottom")
      p <- p %+% subset(finalRxDataInput(), Organs %in% input$organRxPlotCheckbox)
    }
    plots$activePlot <- p
    plots$activePlot
  })
  
  # Note plotly vs. plot gives you the tool tip text
  output$plotOperators <- renderPlotly({
    filteredRxDoneData <- rxDoneData
    if (input$operatorPlotDropdown != 'ALL')
    {
      filteredRxDoneData <- filteredRxDoneData %>% filter(Operator1 %in% input$operatorPlotDropdown)
    }
    if (input$anaesthetistPlotDropdown != 'ALL')
    {
      filteredRxDoneData <- filteredRxDoneData %>% filter(Anaesthetist1 %in% input$anaesthetistPlotDropdown)
    }
    p <- finalOperatorPlotInput()
    p <- p %+% subset(filteredRxDoneData)
    
    # We need to round up to get the bin to include the full month otherwise it looses treatmnet data
    p <- p + scale_x_date(date_breaks = "1 month", date_labels = "%b %y",
                          limits = as.Date(c(input$operatorPlotDate1, ceiling_date(input$operatorPlotDate2,"month"))))
    
    plots$activePlot <- p
    plots$activePlot
  })
  
  # Note plotly vs. plot gives you the tool tip text
  output$plotVolume <- renderPlotly({
    filteredRxDoneData <- rxDoneData %>% filter(Organs %in% input$organVolumePlotCheckbox)
    filteredRxDoneData <- filteredRxDoneData %>% filter(Modality %in% input$modalityVolumePlotCheckbox)
    filteredRxDoneData <- filteredRxDoneData %>% filter(RxDate >= input$volumePlotDate1)
    filteredRxDoneData <- filteredRxDoneData %>% filter(RxDate <= input$volumePlotDate2)
    
    # We need to call this as if the duration radiobutton changes, it otherwise doesn't trigger a replot
    p <- makeRxVolumePlot(filteredRxDoneData, input$volumePlotDurationRadio)
    p <- p %+% subset(filteredRxDoneData)
    
    # Work out the tariff
    tariff$theTotalTariff <<- calculateTotalTariff(filteredRxDoneData)

    # We need to round up to get the bin to include the full month otherwise it looses treatmnet data
    #p <- p + scale_x_date(date_breaks = "1 month", date_labels = "%b %y",
    #                      limits = as.Date(c(input$volumePlotDate1, ceiling_date(input$volumePlotDate2,"month"))))

    if (input$volumePlotDurationRadio == "week")
    {
      p <- p + scale_x_date(date_breaks = "1 week", date_labels = "%e %b %y")
    }
    else if (input$volumePlotDurationRadio == "month")
    {
      p <- p + scale_x_date(date_breaks = "1 month", date_labels = "%b %y")
    }
    else if (input$volumePlotDurationRadio == "year")
    {
      p <- p + scale_x_date(date_breaks = "1 year", date_labels = "%Y")
    }
    plots$activePlot <- p
    plots$activePlot
  })
  
  # This is another method to make a pie chart, but from memory I had similar refresh issues
  observeEvent(input$refreshRxPieAnotherWay, {
    logger("FIMXE Refresh Pie Another Example Method")
    output$pieRxPathwayAnotherWay <- renderPieChart(div_id="pie", data=data.frame(name=c("A","B","C"),value=c(1,2,3)))
    plots$activePlot
  })
  
  # Chart the Rx pathway pie using the date range and organ filters
  # If you put the output$ in the observeEvent it won't refresh on loading, only when hit refresh button
  observeEvent(input$refreshRxPie, {
   logger("FIXME Refresh Pie")
  })
  
  output$pieRxPathway <- renderPlot(
    {
      if (!is.list(finalRxTableDataInput()))
      {
        p <- plot.new()
      }
      else
      {
        makeRxDonePie(input$rxPieDate1,
                      input$rxPieDate2,
                      input$organPieCheckbox)
        makeRxWaitPie(input$rxPieDate1,
                      input$rxPieDate2,
                      input$organPieCheckbox)
        p <- finalRxPieInput()
      }
      plots$activePlot <- p
      plots$activePlot
    })
  
  output$plotRecurrenceCurve <- renderPlot({
    # See this for dynmaic survival curves in shiny
    #    https://stackoverflow.com/questions/61273513/issue-with-r-shiny-app-interactive-survival-plots
    p <- finalRecurrencePlotInput()
    plots$activePlot <- p
    plots$activePlot
  })
  
  output$plotSurvivalCurve <- renderPlot({
    # See this for dynmaic survival curves in shiny
    #    https://stackoverflow.com/questions/61273513/issue-with-r-shiny-app-interactive-survival-plots
    p <- finalSurvivalPlotInput()
    plots$activePlot <- p
    plots$activePlot
  })
  
  # This is the one when we hit the refresh button!
  observeEvent(input$RefreshReferralMap, {
    output$plotReferralMap <- renderLeaflet({
      if (is.Date(input$refMapDate1) && is.Date(input$refMapDate2))
      {
        generateMap()
        if (!is.null(referralMap)) plots$activePlot <- referralMap@map
      }
      plots$activePlot
    })
  })
  
  # This is a common function to generate the referral treated map to avoid duplicating code
  generateMap <- function(force = F)
  {
    # See :    https://stackoverflow.com/questions/36679944/mapview-for-shiny
    # Refresh: https://stackoverflow.com/questions/67725408/how-i-can-reload-my-leaflet-map-in-shiny-r
    if (is.data.frame(rxDoneData) && nrow(rxDoneData>0))
    {
      regenerateReferralMap(force)
      showNotification("Completed map generation from registry.")
    }
    else
    {
      showNotification("No treatment data available to generate map- please reload data.")
    }
  }
  
  output$summaryWaitData <- renderPrint({
    summary(rxWaitData)
  })
  output$summaryRxData <- renderPrint({
    summary(rxDoneData)
  })
  output$summarySurvivalData <- renderPrint({
    paste(print(summary(survivalFitSex)), "\n", print(summary(survivalFitOrgan)), sep = "")
  })
  output$tableRxPathway <- DT::renderDataTable({
    DT::datatable(finalRxTableDataInput())
  })
  output$tableWait <- DT::renderDataTable({
    DT::datatable(rxWaitData)
  })
  output$tableRx <- DT::renderDataTable({
    DT::datatable(rxDoneData)
  })
  output$tableSurvival <- DT::renderDataTable({
    DT::datatable(survivalData)
  })
  output$tableRecurrence <- DT::renderDataTable({
    DT::datatable(recurrenceData)
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
  observeEvent(input$connectAPI,
  {
    shinyjs::hide('studyDropdownGroup')
    shinyjs::hide('reloadData')
    # First check we have up to date version (at least 2.x.x)
    castorLibMajorVersion <- strtoi(substr(toString(packageDescription("castoRedc")$Version),1,1))
    showNotification(paste("Version of castoRedc Library = ",packageVersion("castoRedc")))
    logger(paste("Version of castoRedc Library = ",packageVersion("castoRedc")))
    if (castorLibMajorVersion < 2)
    {
      showNotification("Version of castoRedc library incompatiable: Please upgrade")
      logger(paste("Version of castoRedc is incompatiable despite attempted upgrade; expected >2.x.x, got", packageVersion("castoRedc")),stderr=TRUE)
    }
    else
    {
      # Disconnect the old before connecting
      disconnectCastorAPI()
      api$connected = F
      api$loaded = F
      Sys.setenv(CASTOR_USER_KEY = input$inputCastorKey)
      Sys.setenv(CASTOR_SECRET   = input$inputCastorSecret)
      connectCastorAPI()
      if (is.environment(castor_api))
      {
        api$connected = T
        showNotification("Castor API Connected.")
      }
      else
      {
        api$connected = F
        shinyjs::hide('studyDropdownGroup')
        shinyjs::hide('reloadData')
        showNotification("Could not connect to Castor API with those settings")
      }
    }
  })
  observeEvent(input$disconnectAPI, {
    disconnectCastorAPI()
    api$connected = F
    api$loaded = F
    shinyjs::hide('studyDropdownGroup')
    shinyjs::hide('reloadData')
    showNotification("Castor API Disconnected.")
  })
  observeEvent(input$reloadData, {
    if (length(castor_api) != 0)
    {
      # Create a Progress object
      progress <- shiny::Progress$new()
      progress$set(message = "Loading & Computing study data...", value = 0.0)
      
      # Close the progress when this reactive exits (even if there's an error)
      on.exit(progress$close())
      
      progress$set(message = "Initialising...", value = 0.2)
      initialiseGlobals()
      
      progress$set(message = "Loading study data...", value = 0.4)
      reloadStudyData(input$studyDropdown)
      
      if (ifValidTARGETStudy())
      {
        api$loaded = T
        showNotification("Valid TARGET study loaded...")
        logger(paste(
          "Valid study data for study '",
          input$studyDropdown,
          "'..."
        ))
        
        progress$set(message = "Processing study data...", value = 0.7)
        processData()

        # Make sure our Organ tick list matches the data...
        updateCheckboxGroupInput(
          session,
          "organRxPlotCheckbox",
          "Organs to Plot",
          choices = organFactors,
          selected = organFactors
        )
        # Make sure our Organ tick list matches the data...
        updateCheckboxGroupInput(
          session,
          "organVolumePlotCheckbox",
          "Organs to Plot",
          choices = organFactors,
          selected = organFactors
        )
        updateSelectInput(
          session,
          "operatorPlotDropdown",
          "Operators to Plot",
          choices = operator1Factors
        )
        updateSelectInput(
          session,
          "anaesthetistPlotDropdown",
          "Anaesthetists to Plot",
          choices = anaesthetist1Factors
        )
        updateCheckboxGroupInput(
          session,
          "operatorNameCheckbox",
          "Operator Names",
          choices = operatorAllFactors
        )
        updateCheckboxGroupInput(
          session,
          "anaesthetistNameCheckbox",
          "Anaesthetist Names",
          choices = anaesthetistAllFactors
        )
        updateCheckboxGroupInput(
          session,
          "organPieCheckbox",
          "Organs to Chart",
          choices = organFactors,
          selected = organFactors
        )
        updateCheckboxGroupInput(
          session,
          "organAuditCheckbox",
          "Organs to Chart",
          choices = organFactors,
          selected = organFactors
        )
        sapply(c('chartsMenuItem', 'tablesMenuItem', 'validationMenuItem', 'auditMenuItem', 'summaryMenuItem', 'pathwaySummaryMenuItem'), shinyjs::show)
        # Make the pathway plots...
        makeRxPathwayPlots()
        makeRxVolumePlot(rxDoneData, input$volumePlotDurationRadio)
        
        progress$set(message = "Completed loading & processing.", value = 1.0)
        showNotification("Completed data processing, plot/tables should now be available to view...")
      }
      else
      {
        api$loaded = F
        showNotification("Study data is not valid, either not TARGET study or no patients...")
        logger(
          paste(
            "Study data '",
            input$studyDropdown,
            "' is not valid, either not TARGET study or no patients..."
          )
        )
      }
    }
    else
    {
      logger("Nothing to do without API Connection...")
    }
  })
  
  # When we hit refresh button we want to reset the plot
  # This works to a point in that it resets the scale but it doesn't reload the data
  # Not really sure how this works at all if I am honest! I don't assign it to a real plot, weird
  observeEvent(input$refreshRxPlot, {
    plots$activePlot <- ggplot()
  })
  observeEvent(input$refreshOperatorPlot, {
    plots$activePlot <- ggplot()
  })
  observeEvent(input$refreshVolumePlot, {
    plots$activePlot <- ggplot()
  })
  observeEvent(input$refreshRxPie, {
    plots$activePlot <- ggplot()
  })
  observeEvent(input$refreshRecurrencePlot, {
    plots$activePlot <- ggplot()
  })
  observeEvent(input$refreshSurvivalPlot, {
    plots$activePlot <- ggplot()
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
  
  observeEvent(input$runAuditReport, {
    logger(paste("Running audit for dates: ", input$auditDate1,"-", input$auditDate2, sep=""))
    logger(paste("Running audit for organs:", input$organAuditCheckbox))
    plots$activePlot <- NA
    
    # This is a bit of an ugly hack to allow markdown to see global vars but it doesn't appear to work FIXME
    audit_start_date <<- input$auditDate1
    audit_end_date   <<- input$auditDate2
    audit_organs     <<- input$organAuditCheckbox
    
    # This is the magic - embed the output into the observe event to allow refresh!
    # So simple but still not quite working - maybe make something reactive ... keep working Paul
    output$summaryRefAudit <- renderPrint({
      if (api$connected == T && api$loaded == T)
      {
        thisHTML <- finalRefAuditInput()
      }
      else
      {
        thisHTML <-
          "There is no study data loaded at present - cannot run the audit"
      }
      thisHTML
    })
  })
  
  observeEvent(input$buttonSaveRxTimesData, {
    shinyCatch({
      message("Choose a file to export to...")
    }, prefix = '') # DOESNT WORK IN A DOCKER
    exportFile = NA
    try (
      exportFile <- file.choose(new = TRUE)
    )
    if (!is.na(exportFile))
    {
      if (!endsWith(exportFile, ".csv"))
      {
        exportFile = paste(exportFile, ".csv", sep = "")
      }
      shinyCatch({
        message(paste("Attempting to export data to file", exportFile))
      }, prefix = '')
      write.csv(finalRxTableDataInput(), exportFile, row.names = TRUE)
      shinyCatch({
        message(paste("Exported data to file", exportFile))
      }, prefix = '')
    }
    else
    {
      shinyCatch({
        message(paste("No file selected to export to, no data export performed"))
      }, prefix = '')
    }
  })
  
  observeEvent(input$buttonPasteRxTimesData, {
    copyDataToClipboard(finalRxTableDataInput())
    shinyCatch({
      message("Copied data to the clipboard, please paste into Excel")
    }, prefix = '')
  })
  
  observeEvent(input$buttonSaveRecurrenceData, {
    shinyCatch({
      message("Not yet implemented!")
    }, prefix = '')
  })
  observeEvent(input$buttonPasteRecurrenceData, {
    shinyCatch({
      message("Not yet implemented!")
    }, prefix = '')
  })
  
  observeEvent(input$buttonSaveSurvivalData, {
    shinyCatch({
      message("Choose a file to export to...")
    }, prefix = '') # DOESNT WORK IN A DOCKER
    exportFile = file.choose(new = TRUE)
    if (!endsWith(exportFile, ".csv"))
    {
      exportFile = paste(exportFile, ".csv", sep = "")
    }
    shinyCatch({
      message(paste("Attempting to export data to file", exportFile))
    }, prefix = '')
    write.csv(survivalData, exportFile, row.names = TRUE)
    shinyCatch({
      message(paste("Exported data to file", exportFile))
    }, prefix = '')
  })
  
  observeEvent(input$buttonPasteSurvivalData, {
    copyDataToClipboard(survivalData)
    shinyCatch({
      message("Copied data to the clipboard, please paste into Excel")
    }, prefix = '')
  })
}

# Open the GUI
shinyApp(ui = ui, server = server)

# Useful stuff
#print (studyData)
#print colnames(studyData)
#print (studyData[]$ref_date_recd_1)

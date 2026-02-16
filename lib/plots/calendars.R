library(htmltools)
library(knitr)

getTciData <- function(startDate, reportOrgans, includeTreated)
{
  # Get first date of the month
  firstDateOfMonth <- format(ymd(startDate),"%Y-%m-01")
  
  # Make a data table which we can use to populate the calendar
  tciData <- subset(rxWaitData, select = c(ID, TciDate, TciStatus, Organs))
  
  # Filter to just the organs of interest and other stuff
  if (nrow(tciData)>0)
  {
    if (!("All Organs" %in% reportOrgans)) {
      tciData <-tciData |>
        dplyr::filter(Organs %in% reportOrgans)
    }
    tciData <- dplyr::filter(tciData, !is.na(TciDate)) # Remove the nulls
    tciData <- dplyr::filter(tciData, TciDate >= firstDateOfMonth) # Remove anything before the 1st day of month
  }
  data.table::setnames(tciData, c("title","start","status","body"))
  tciData <- tciData %>% mutate(calendarId = paste(body," (",status,")",sep=""))
  tciData <- tciData %>% mutate(body = paste(body," (",status,")",sep=""))
  tciData <- tciData %>% mutate(end = start) # Make a new column called End which is a copy of Start
  tciData <- tciData %>% mutate(category = "allday") # Make a new column called category which is allday as oppose to time option
  
  # If we are to include the treated patients too...
  if (includeTreated)
  {
    rxData <- subset(rxDoneData, select = c(ID, RxDate, Organs))
    if (!("All Organs" %in% reportOrgans)) {
      rxData <- rxData |>
        dplyr::filter(Organs %in% reportOrgans)
    }
    rxData <- dplyr::filter(rxData, RxDate >= firstDateOfMonth) # Remove anything before the 1st day of month
    data.table::setnames(rxData, c("title","start","body"))
    rxData <- rxData %>% mutate(body = paste(body,"*",sep=""))
    rxData <- rxData %>% mutate(status = "Treated") # Make a new column called status
    rxData <- rxData %>% mutate(calendarId = paste(body," (Treated)",sep=""))
    rxData <- rxData %>% mutate(end = start) # Make a new column called End which is a copy of Start
    rxData <- rxData %>% mutate(body = paste(body," (Treated)",sep=""))
    rxData <- rxData %>% mutate(category = "allday") # Make a new column called category which is allday as oppose to time option
    tciData <- merge(tciData,rxData, all=T) # Merge both tables together (the column order slightly differs so we need merge)
  }
  tciData
}


# Put this in your setup chunk (include=FALSE)
library(htmltools)
library(knitr)

getTciCalendarKey <- function() {
  
  if (knitr::is_latex_output()) {
    
    latex <- paste0(
      "\\begingroup\n",
      "\\setlength{\\fboxsep}{4pt}\n",
      "\\definecolor{lung}{HTML}{00A0FF}\n",
      "\\definecolor{kidney}{HTML}{FFDB58}\n",
      "\\definecolor{liver}{HTML}{C4A484}\n",
      "\\definecolor{bone}{HTML}{FF4B33}\n",
      "\\definecolor{multiple}{HTML}{F5A9A9}\n",
      "\\definecolor{other}{HTML}{2FA9A9}\n",
      "\\definecolor{completed}{HTML}{808080}\n",
      "\\definecolor{canceled}{HTML}{FF0000}\n",
      "\\definecolor{deferred}{HTML}{FFC0CB}\n",
      "\\definecolor{confirmed}{HTML}{90EE90}\n",
      "\\definecolor{prov}{HTML}{FF8C00}\n",
      "\\definecolor{treated}{HTML}{00008B}\n",
      "\n",
      "\\noindent\\textbf{Key:}\\quad ",
      "\\colorbox{completed}{\\strut Completed}\\quad ",
      "\\colorbox{lung}{\\strut Lung}\\quad ",
      "\\colorbox{kidney}{\\strut Kidney}\\quad ",
      "\\colorbox{liver}{\\strut Liver}\\quad ",
      "\\colorbox{bone}{\\strut Bone}\\quad ",
      "\\colorbox{multiple}{\\strut Multiple}\\quad ",
      "\\colorbox{other}{\\strut Other/Unspecified}\\\\[6pt]\n",
      "\\noindent\\textbf{TCI Status:}\\quad ",
      "\\colorbox{canceled}{\\strut C}\\, Cancelled\\quad ",
      "\\colorbox{deferred}{\\strut T}\\, Deferred\n",
      "\\colorbox{confirmed}{\\strut C}\\, Confirmed\\quad ",
      "\\colorbox{prov}{\\strut P}\\, Provisional\\quad ",
      "\\colorbox{treated}{\\strut T}\\, Treated\n",
      "\\endgroup\n"
    )
    
    # IMPORTANT: asis_output so knitr emits raw LaTeX directly
    return(knitr::asis_output(latex))
  }
  
  # HTML / Shiny: return htmltools tag (badges)
  badge <- function(label, bg, color = NULL) {
    style <- paste0("display:inline-block; padding:4px 8px; margin:0 6px 6px 0;",
                    "border-radius:4px; font-size:0.9em;")
    if (!is.null(color)) style <- paste0(style, "color:", color, ";")
    style <- paste0(style, "background-color:", bg, ";")
    tags$span(style = style, label)
  }
  
  tags$div(
    style = "border:2px solid #000; border-radius:6px; padding:10px; margin-bottom:20px; background:#fff; width:100%;",
    tags$b("Key: "),
    badge("Completed",         "lightgray", "gray"),
    tags$b("Upcoming: "),
    badge("Lung",              "#00A0FF"),
    badge("Kidney",            "#FFDB58"),
    badge("Liver",             "#C4A484"),
    badge("Bone",              "#FF4B33"),
    badge("Multiple",          "#F5A9A9"),
    badge("Other/Unspecified", "#2FA9A9"),
    tags$br(),
    tags$b("TCI Status:         "),
    tags$span(style = "display:inline-block; padding:2px 6px; margin-right:6px; border-radius:3px; background:#FF0000; color:white;", "."),
    tags$span(" Cancelled "),
    tags$span(style = "display:inline-block; padding:2px 6px; margin-right:6px; border-radius:3px; background:#FFC0CB; color:white;", "D"),
    tags$span(" Deferred "),
    tags$span(style = "display:inline-block; padding:2px 6px; margin:0 6px 0 12px; border-radius:3px; background:#90EE90;", "C"),
    tags$span(" Confirmed "),
    tags$span(style = "display:inline-block; padding:2px 6px; margin:0 6px 0 12px; border-radius:3px; background:#FF8C00;", "P"),
    tags$span(" Provisional "),
    tags$span(style = "display:inline-block; padding:2px 6px; margin:0 6px 0 12px; border-radius:3px; background:#00008b; color:white;", "T"),
    tags$span(" Treated ")
  )
}


# Make a colour transparent
makeTransparent<-function(someColor, alpha=100)
{
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                              blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}

makeTciCalendar <- function(startDate, reportOrgans, includeTreated = TRUE, withNavigation = FALSE)
{
  # Because its a calendar you often get the last week of the previous month included, so make sure we go back to start of that month
  tciData     <- getTciData(asDateWithOrigin(startDate)-months(1), reportOrgans, includeTreated)
  colLiver    <- "#C4A484"
  colLung     <- "#00A0FF"
  colKidney   <- "#FFDB58"
  colBone     <- "#FF4B33"
  colMulti    <- "#F5A9A9"
  colOther    <- "#2FA9A9"
  colTrLiver  <- makeTransparent(colLiver, 50)
  colTrLung   <- makeTransparent(colLung,  50)
  colTrKidney <- makeTransparent(colKidney,50)
  colTrBone   <- makeTransparent(colBone,  50)
  colTrMulti  <- makeTransparent(colMulti, 50)
  colTrOther  <- makeTransparent(colOther, 50)
  
  # These are used to colour the calendar entries, note using organ name for the ID rather than another lookup
  calendarProperties <- data.frame(col1 = c("Liver",   "Lung",    "Kidney",  "Bone",    "Multiple", "Other/Unspecified", "Liver*",   "Lung*",   "Kidney*",   "Bone*",   "Multiple*", "Other/Unspecified*"),
                                   col2 = c("Liver",   "Lung",    "Kidney",  "Bone",    "Multiple", "Other/Unspecified", "Liver*",   "Lung*",   "Kidney*",   "Bone*",   "Multiple*", "Other/Unspecified*"),
                                   col3 = c("#555555", "#555555", "#555555", "#555555", "#555555",  "#555555",           "gray",     "gray",    "gray",      "gray",    "gray",      "gray"             ),
                                   col4 = c(colLiver,  colLung,   colKidney, colBone,   colMulti,   colOther,            colTrLiver, colTrLung, colTrKidney, colTrBone, colTrMulti,  colTrOther          ),
                                   col5 = c("#000000", "#000000", "#000000", "#000000", "#000000",  "#000000",           "#000000",  "#000000", "#000000",   "#000000", "#000000",   "#000000"           ))
  data.table::setnames(calendarProperties, c("id","name","color","backgroundColor","borderColor"))
   
  # Make a unique status for each organ type, so we can colour it in a traffic light system, otherwise its black if not known status
  statusLevels <- unique(as.character(tciData$status))
  if (length(statusLevels) == 0) statusLevels <- c("Unknown")
  calendarPropertiesStatus <- data.frame()
  for (status in statusLevels)
  {
    newCalendarProperties <- calendarProperties %>% mutate(id = paste(id," (",status,")",sep=""))
  
    if (status == "Cancelled")
    {
      newCalendarProperties <- newCalendarProperties %>% mutate(borderColor = "#FF0000")
    }
    else if (status == "Deferred")
    {
      newCalendarProperties <- newCalendarProperties %>% mutate(borderColor = "#FFC0CB")
    }
    else if (status == "Confirmed")
    {
      newCalendarProperties <- newCalendarProperties %>% mutate(borderColor = "#90EE90")
    }
    else if (status == "Provisional")
    {
      newCalendarProperties <- newCalendarProperties %>% mutate(borderColor = "#FF8C00")
    }
    else if (status == "Treated")
    {
      newCalendarProperties <- newCalendarProperties %>% mutate(borderColor = "#00008b")
    }
    calendarPropertiesStatus <- rbind(calendarPropertiesStatus, newCalendarProperties)
  }

  # If we have found some records in the TCI table...
  if (nrow(tciData) > 0)
  {
    # The visibleEventCount is set to the max events on any day, fallback to 3 if can't compute it
    visible_count <- tryCatch({
      max((tciData$start %>% duplicate_count())$frequency)
    }, error = function(e) {
      warning("visibleEventCount calc failed; using fallback = 3: ", conditionMessage(e))
      3L
    })
    if (is.infinite(visible_count) || is.na(visible_count) || visible_count <= 0) visible_count <- 3L
  }
  else
  {
    visible_count <- 3L
  }

  # Create the widget (no global assignment) ---
  w <- toastui::calendar( tciData,
                          view = "month",
                          useDetailPopup = TRUE,
                          useCreationPopup = FALSE,
                          isReadOnly = TRUE,
                          navigation = withNavigation,
                          width = "100%",
                          height = "100%",
                          visibleEventCount = visible_count,
                          defaultDate = asDateWithOrigin(startDate)  # Start the calendar on the date provided
  ) %>%
    cal_month_options(startDayOfWeek = 1, narrowWeekend = TRUE) %>%
    cal_props(calendarPropertiesStatus)
  
  # Attach onRender to force the inner widget to match the wrapper height (helps knits & webshot)
  w <- htmlwidgets::onRender(w, htmlwidgets::JS("
    function(el,x){
      try {
        var wrapper = document.getElementById('calendar-wrapper') || el.closest('.shiny-html-output') || el.parentElement || el;
        if (!wrapper) wrapper = el;
        var h = Math.max(wrapper.clientHeight||0, wrapper.scrollHeight||0, 600);
        el.style.height = h + 'px'; el.style.minHeight = h + 'px';
        var inner = el.querySelector('.tui-full-calendar, .tui-calendar, .htmlwidget') || el;
        try { inner.style.height = h + 'px'; inner.style.minHeight = h + 'px'; } catch(e){}
        if (window.HTMLWidgets && typeof HTMLWidgets.getInstance === 'function') {
           try { var inst = HTMLWidgets.getInstance(el); if (inst && inst.widget && typeof inst.widget.resize === 'function') inst.widget.resize(); } catch(e){}
        }
        try { window.dispatchEvent(new Event('resize')); } catch(e){}
      } catch(e){ console.warn('fit error', e); }
    }
  "))
    
  # optional: keep a global reference if other code expects refTciCalendar
  try({ refTciCalendar <<- w }, silent = TRUE)
  refTciCalendar
}

# Make a heatmap calendar of the year of both Treated and TCI so we can see capacity
# Note that TCI status is ignored 
makeCalendarHeatmap <- function(startDate, reportOrgans = NA, includeTreated)
{
  firstDateOfYear <- format(ymd(startDate),"%Y-01-01")
  
  tciData <- getTciData(firstDateOfYear, reportOrgans, includeTreated)
  calendarYear <- year(startDate)
  
  # Vector of NA of the same length of the number of days of the year
  events <- rep(0, 365)
  
  # Set the corresponding events
  #for (i in 1 : nrow(tciData))
    #{
    #thisTciYearDay <- yday(tciData$start[i])
    #if (tciData$status[i] != "Cancelled")
      #{
      #events[thisTciYearDay] <- events[thisTciYearDay] + 1
      #}
    #}
  
  # Remember not to include the TCIs if the status is cancelled
  events <- tciData |>
    filter(status != "Cancelled") |>
    mutate(day = yday(start)) |>
    count(day) |>
    complete(day = 1:365, fill = list(n = 0)) |>
    arrange(day) |>
    pull(n)
                    
  # Creating the calendar with a legend
  calendR(year = calendarYear,
          special.days = events,
          low.col = "white",
          special.col = "#FF0000",
          gradient = TRUE,
          orientation = "landscape",
          legend.pos = "bottom") # Legend to the right
}
getTciData <- function(startDate, includeTreated)
{
  # Get first date of the month
  firstDateOfMonth <- format(ymd(startDate),"%Y-%m-01")
  
  # Make a data table which we can use to populate the calendar
  tciData <- subset(rxWaitData, select = c(ID, TciDate, TciStatus, Organs))
  tciData <- dplyr::filter(tciData, !is.na(TciDate)) # Remove the nulls
  tciData <- dplyr::filter(tciData, TciDate >= firstDateOfMonth) # Remove anything before the 1st day of month
  data.table::setnames(tciData, c("title","start","status","body"))
  tciData <- tciData %>% mutate(calendarId = paste(body," (",status,")",sep=""))
  tciData <- tciData %>% mutate(body = paste(body," (",status,")",sep=""))
  tciData <- tciData %>% mutate(end = start) # Make a new column called End which is a copy of Start
  tciData <- tciData %>% mutate(category = "allday") # Make a new column called category which is allday as oppose to time option
  
  # If we are to include the treated patients too...
  if (includeTreated)
  {
    rxData <- subset(rxDoneData, select = c(ID, RxDate, Organs))
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

makeTciCalendar <- function(startDate, includeTreated, withNavigation = FALSE)
{
  # Because its a calendar you often get the last week of the previous month included, so make sure we go back to start of that month
  tciData <- getTciData(asDateWithOrigin(startDate)-months(1), includeTreated)
  
  # These are used to colour the calendar entries, note using organ name for the ID rather than another lookup
  calendarProperties <- data.frame(col1 = c("Liver",   "Lung",    "Kidney",  "Bone",   "Other/Unspecified", "Liver*",   "Lung*",    "Kidney*",  "Bone*",   "Other/Unspecified*"),
                                   col2 = c("Liver",   "Lung",    "Kidney",  "Bone",   "Other/Unspecified", "Liver*",   "Lung*",    "Kidney*",  "Bone*",   "Other/Unspecified*"),
                                   col3 = c("#000000", "#000000", "#000000", "#000000", "#000000",          "white",    "white",    "white",    "white",   "white"           ),
                                   col4 = c("#F5A9A9", "#00A0FF", "#F539A9", "#FFDB58", "purple",           "grey",     "grey",     "grey",     "grey",    "grey"              ),
                                   col5 = c("#000000", "#000000", "#000000", "#000000", "#000000",          "#000000",  "#000000",  "#000000",  "#000000", "#000000"           ))
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
      newCalendarProperties <- newCalendarProperties %>% mutate(borderColor = "red")
    }
    else if (status == "Confirmed")
    {
      newCalendarProperties <- newCalendarProperties %>% mutate(borderColor = "green")
    }
    else if (status == "Provisional")
    {
      newCalendarProperties <- newCalendarProperties %>% mutate(borderColor = "orange")
    }
    else if (status == "Treated")
    {
      newCalendarProperties <- newCalendarProperties %>% mutate(borderColor = "darkblue")
    }
    calendarPropertiesStatus <- rbind(calendarPropertiesStatus, newCalendarProperties)
  }

  # The visibleEventCount is set to the max events on any day, fallback to 3 if can't compute it
  visible_count <- tryCatch({
    max((tciData$start %>% duplicate_count())$frequency)
  }, error = function(e) {
    warning("visibleEventCount calc failed; using fallback = 3: ", conditionMessage(e))
    3L
  })
  if (is.infinite(visible_count) || is.na(visible_count) || visible_count <= 0) visible_count <- 3L
  
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
                          defaultDate = asDateWithOrigin(Sys.Date())  # Start the calendar on today
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
makeCalendarHeatmap <- function(startDate, includeTreated)
{
  firstDateOfYear <- format(ymd(startDate),"%Y-01-01")
  
  tciData <- getTciData(firstDateOfYear, includeTreated)
  calendarYear <- year(startDate)
    
  # Vector of NA of the same length of the number of days of the year
  events <- rep(0, 365)
  
  # Set the corresponding events
  for (i in 1 : nrow(tciData))
  {
    thisTciYearDay <- yday(tciData$start[i])
    # Remember not to include the TCIs if the status is cancelled
    if (tciData$status[i] != "Cancelled")
    {
      events[thisTciYearDay] <- events[thisTciYearDay] + 1
    }
  }
                    
  # Creating the calendar with a legend
  calendR(year = calendarYear,
          special.days = events,
          low.col = "white",
          special.col = "#FF0000",
          gradient = TRUE,
          orientation = "landscape",
          legend.pos = "bottom") # Legend to the right
}
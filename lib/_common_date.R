# _common_date.R contains common date functions

# Checks if a provided string can be converted to a date using the default or a specified format
isConvertibleToDate <- function(mydate, date.format=Sys.getenv("DATE_FORMAT"))
{
  # Check if field is a date using as.Date that looks for unambiguous dates
  #   Assumes date format so NA returned not Character error. 
  #   Why? with no date format, R tries two defaults then gives error. 
  #   BUT With a dateformat R returns NA
  # Args
  #   Suspected date and optional date format string
  # Returns
  #   TRUE if thinbks it is a date
  tryCatch(!is.na(as.Date(mydate, format = date.format, origin = '1970-01-01', tz = 'UTC')),  
           error = function(err) {FALSE})  
}



isConvertibleToDateNotWorking <- function(x)
{
  if (!is.na(x))
  {
    theDate <- (!is.na(as.Date(as.character(x), origin='1970-01-01')))
  }
  else
  {
    theDate <- FALSE
  }
  return(theDate)
}

# convertToDate converts a provided string to a date
# Note this returns a Date object not a string
# The date object is basically an integer number which corresponds to the day
# To display it can be cast to a string with convertToDateStr() or it does this automatically when adding to data.frame
convertToDate <- function(x)
{
  if (isConvertibleToDate(x))
  {
    r=as.Date(x,format=Sys.getenv("DATE_FORMAT"), origin='1970-01-01')
  }
  else
  {
    r=NA
  }
  return (r)
}

# This is redundant currently
convertToDateStr <- function(x) {
  return (as.character(convertToDate(x)))
}

# This is a wrapper for the as.Date function but it sets the origin
# This is because on some OS e.g. Linux, R may be more strict and fall over if origin is not set
asDateWithOrigin <- function(thisDate)
{
  as.Date(thisDate, origin = '1970-01-01')
}

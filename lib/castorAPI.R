## Functions to load the castor API

# Connect to the Castor API...
connectCastorAPI <- function()
{
  # Please note that the castorRedc library updated form 1.1.0 to 2.1.0 from 2024...
  # This changes the methods available on the environment() variable and getRecords() only works on 1.1.0
  #logger("   Attempting to open Castor API...") 
  castor_api <<- tryCatch(
    { CastorData$new(key =      Sys.getenv("CASTOR_USER_KEY"),
                     secret =   Sys.getenv("CASTOR_SECRET"),
                     base_url = Sys.getenv("CASTOR_URL"))
    },
    
    error=function(e) {
      logger(paste("   An error occurred: Could not access Castor API..."))
      logger(e)
      castor_api <<- NA # Its an environment so we can't use is.na() function, but we can use is.environment()
    },
    
    #if a warning occurs, tell me the warning
    warning=function(w) {
      logger(paste("A warning occurred during access to Castor API"))
      logger(w)
    }
  )
  if (!is.environment(castor_api))
  {
    logger("Failed to access API with those settings")
  }
  else
  {
    logger("Accessed Castor API successfully")
  }
  return(castor_api)
}

# Disconnect from the Castor API...
disconnectCastorAPI <- function()
{
  castor_open_api <<- new.env()
}

# We can use the openAPI to write data, in fact its the only way I can work out how to do it!
# Largely borrowed from:
# https://git.lumc.nl/egjvonasmuth/castor-api-tutorial/-/blob/main/example_rapiclient.R?ref_type=heads
connectCastorOpenAPI <- function()
{
  base_url <- Sys.getenv("CASTOR_URL")
  tryCatch(
    { 
      # Authenticate using oauth
      app <- oauth_app("CastorEDC", key = Sys.getenv("CASTOR_USER_KEY"), secret = Sys.getenv("CASTOR_SECRET"))
      endpoint <- oauth_endpoint(request = NULL, base_url = paste0(base_url, "/oauth"), access = "token", "authorize")
      token <- httr::oauth2.0_token(endpoint, app, client_credentials = TRUE, cache = FALSE)
      castor_open_api <<- get_api(paste0(base_url, "/api/swagger"), httr::config(token = token))
    },
    
    error=function(e) {
      logger(paste("An error occurred: Could not access Castor Open API..."))
      logger(e)
      castor_open_api <<- NA # Its an special rapiclient environment which is essentially a list, so we can't use is.na() function or is.environment()
    },
    
    #if a warning occurs, tell me the warning
    warning=function(w) {
      logger(paste("A warning occurred during access to Castor Open API"))
      logger(w)
    }
  )
  
  # If its not a list it hasn't worked...
  if (typeof(castor_open_api) != "list") 
  {
    logger("Failed to access Castor Open API with those settings")
  }
  else
  {
    # As a global, retrieve the possible operations which is needed to then do the magic later on!
    openAPIoperations <<- get_operations(castor_open_api)
    
    # ...And the possible schemas; we use schemas again below
    openAPIschemas <<- get_schemas(castor_open_api)
    
    # Castor API uses labels like descriptions so this results in awful labels
    # We can use the path instead of the label
    # Need the <<- so it makes permanent change globally otherwise change is temporary
    names(openAPIoperations) <<- sapply(openAPIoperations, function(x) 
      paste(attributes(x)$definition$action, attributes(x)$definition$path))
    
    # A lot easier - do we need this too not sure?
    #openAPIoperations$`get /user`() |> content(as = "text") |> fromJSON()
    
    logger("Accessed Castor Open API successfully")
  }
  return(castor_open_api)
}

# Generic method to fetch any data specified by the URL endpoint provided which should start 'api/...'
# Original code for this method by Maximiliano of Castor Support (thanks!),
# but he must have based it on a different library version,
# as for example he referred to data$data which doesn't exist
# Fixed and modified for TARGET-UK by Paul Lyon
# Returns a data frame of all the data returned by URL endpoint
# Works with the standard (non-open) API but doens't work with open API handle
# I've no idea where the GET() method is called from... but it works!
fetchDataWithURL <- function(castorAPI, apiURL)
{
  page <- 1
  limit <- 100
  all_data <- list()
  
  # Sort out the authorisation headers
  headers <- add_headers(Authorization = paste('Bearer',
                                               castorAPI$oauth_token$credentials$access_token))
  
  # Keep getting pages of data until the data returned is empty, so we know we have got it all
  # Each time through we increment page by 1 which is embedded in the URL
  logger(paste("Reading data page by page via URL ('",apiURL,"')...",sep=""))
  repeat {
    final_api_url <- paste0(apiURL, "?page=", page, "&limit=", limit)
    response <- GET((final_api_url), headers)
    # If we get the normal 200 response code then keep going..
    if (response$status_code == 200)
    {
      # Use charset 'utf-8' if not specified in response headers, avoiding the 'No encoding supplied:..' warning
      charset <- "utf-8"
      if("content-type" %in% names(response$headers)) {
        media <- parse_media(response$headers[['content-type']])
        if("params" %in% names(media) && "charset" %in% names(media$params)) {
          charset <- media$params$charset
        }
      }
      data <- fromJSON(content(response, as = "text", encoding = "utf-8"), flatten = TRUE)
      # This gets to the crux of the data, and its name varies within Castor e.g. could be item or fields hence first embedded field only [[1]]
      all_data <- rbind.data.frame(all_data, data$`_embedded`[[1]]) 
      page <- page + 1
    }
    else
    {
      # Otherwise either we have run out of data, which is fine, or there is a problem...
      if (response$status_code == 409)
      {
        # This is the standard response code when we have run out of data, everything is fine
        break
      }
      else
      {
        # Otherwise log the error code, but don't necessarily terminate the thread
        logger (paste("Error reading endpoint after attempted ",page," page(s) of data, response code = ",response$status_code,sep=""))
        break
      }
    }
  }
  return(all_data)
}

# FIXME this is just an example but you can see how it might work if we added ?page=.. to the URL and looped like the other method
fetchDataWithURLOpenAPI <- function(castorOpenAPI, apiURL)
{
  # Connect to the API here
  if (!is.environment(castorOpenAPI))
  {
    castorOpenAPI <<- connectCastorOpenAPI()
  }
  
  if (typeof(studyDataOpenAPI) != "list")
  {
    logger("Connected to Open API...")
    studyDataOpenAPI <<- openAPIoperations$`get /study/{study_id}/data-points/study`(studyID) |>
      content(as = "text") |>
      fromJSON()
  }  
}

# We can get the field names with open API but also easy to do with the traditional API so this code is redundant currently
# but given I have coded it up I've left it in here! We may move only to OpenAPI in the future!
getFieldNamesWithOpenAPI <- function(castorOpenAPI)
{
  # If have haven't previously pulled in the Open API field data, pull it in now
  # FIXME: this only gives a page_size of 25 - i.e. only first 25 column or field names, how do I increase this or get next page?
  if (typeof(fieldDataOpenAPI) != "list")
  {
    fieldDataOpenAPI <<- openAPIoperations$`get /study/{study_id}/field`(studyID) |>
      content(as = "text") |>
      fromJSON()
  }
  
  # Find the fieldID for the fieldName
  # This seems like an ugly hack but until Castor dev team spill the beans on how to do this...
  i <- which(fieldDataOpenAPI$`_embedded`$fields$field_variable_name == fieldName)
  if (is_empty(i) || i==0)
  {
    msg <- paste("Field name '", fieldName, "' not found in the field data for the study data! Stopping", sep = '')
    logger(msg)
    stop(msg)
  }
  
  fieldID <- fieldDataOpenAPI$`_embedded`$fields$id[i]
  logger(paste("Field name '", fieldName, "' found in field data with ID='",fieldID,"'", sep = ))
}

# Write actions using OpenAPI interface - this is the only way I can work out how to write using any API...
# The Open API method to get the study data, which is in a different format...
# Its the bare bones of : https://git.lumc.nl/egjvonasmuth/castor-api-tutorial/-/blob/main/example_rapiclient.R?ref_type=heads
# To see how to use it just type 'operations$' into the Console of R and it will show you all available methods
updateStudyDataOpenAPI <- function(studyID, patientID, fieldName, newData, reasonForUpdate="Unspecified Reason")
{
  # First get the field name
  fieldID <- getFieldIDForName(castor_api,studyID,fieldName)

  # To update data within Castor we need to use the Castor Open API as I don't know how to do it any other way!
  # Certainly there is no API Wrapper function
  castorOpenAPI <- connectCastorOpenAPI()
  
  # Do the magic to update
  openAPIoperations$`post /study/{study_id}/participant/{participant_id}/data-points/study`
  formals(openAPIoperations$`post /study/{study_id}/participant/{participant_id}/data-points/study`)
  write_result_openapi <- openAPIoperations$`post /study/{study_id}/participant/{participant_id}/data-points/study`(
    study_id = studyID,
    participant_id = patientID,
    common = openAPIschemas$DataPointCollectionCommonParameters(
      change_reason = reasonForUpdate,
      confirmed_changes = "true"
    ),
    data = list(
      openAPIschemas$StudyDataPointCollectionFieldValueParameter(
        field_id = fieldID,
        field_value = newData
      )
    )
  )
  write_result_openapi
  content(write_result_openapi, as = "text") |> fromJSON()
  logger("OpenAPI write completed without error")
}

# Get field ID for field name 
# The meta-data used to be secretly hidden in with R in the Castor data.frame, but this was taken out from castoRedc v2.x
# So instead I use the API to get the fields, find the right field and return the field ID
getFieldIDForName <- function(castorAPI,studyID,fieldName)
{
  # If have haven't previously pulled in the field data, pull it in now
  if (typeof(fieldData) != "list")
  {
    logger("Getting the field data via standard Castor API...")
    base_url <- Sys.getenv("CASTOR_URL")
    apiURL <- paste(base_url,"/api/study/",studyID,"/field",sep='')
    
    # This works with the standard API, but not with castorOpenAPI
    # Note default page size is 500, but this uses a method which can pull all the data using fetchDatWithURL
    fieldData <<- fetchDataWithURL(castorAPI, apiURL)
  }
  
  # Find the fieldID for the fieldName provided using fieldData
  # This seems like a slightly ugly method but it works and there is no castor api doc I can find for more elegant solution,..
  i <- which(fieldData$field_variable_name == fieldName)
  if (is_empty(i) || i==0)
  {
    msg <- paste("Field name '", fieldName, "' not found in the field data for the study data! Stopping", sep = '')
    logger(msg)
    stop(msg)
  }
  fieldID <- fieldData$field_id[i]
  logger(paste("Field name '", fieldName, "' found in open API field data with ID='",fieldID,"'", sep = ""))
  return(fieldID)
}


# An alternative R wrapper example which was provided by Castor level 2 support Maximiliano Senestrari (and he wrote, not on web)
#R wrapper:
#  
#  Authentication: Ensure you are authenticated using your Castor API credentials.
#Identify the Study and Record: Obtain the study ID and record ID for the data you wish to update.
#Prepare the Data: Structure the data payload you need to update in the required format.
#Send the Request: Use the POST or PUT method to update the data field in the specified study/record.
#Here is an example of how you might set up a function to update a data field using the
#
#castor_api
# Function to update a data field
#update_data_field <- function(api, study_id, record_id, field_id, new_value)
#{ url <- paste0(api$base_url, "/study/", study_id, "/record/", record_id, "/data-point")
  # Structure the data payload
  #payload <- list( "field_id" = field_id, "field_value" = new_value )
  # Convert the payload to JSON
  #json_payload <- jsonlite::toJSON(payload)
  # Send the POST request 
  #response <- httr::POST( url, body = json_payload, encode = "json", httr::add_headers("Authorization" = paste("Bearer", api$token)) )
  #return(response)
#} 
# Example usage api <- castor_api$new("your_api_base_url", "your_api_token")
#study_id <- "your_study_id"
#record_id <- "your_record_id"
#field_id <- "your_field_id"
#new_value <- "new_value"
#response <- update_data_field(api, study_id, record_id, field_id, new_value)
#if (response$status_code == 200)
#{
#  print("Data field updated successfully!")
#}
#else
#{
#  print("Failed to update data field.")
#}

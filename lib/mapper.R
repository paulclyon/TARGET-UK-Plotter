# Mapping and MAPVIEW info
# https://r-spatial.github.io/mapview/
# https://map-rfun.library.duke.edu/01_georeference.html
# https://stackoverflow.com/questions/67154658/how-to-extract-longitude-and-latitude-using-postal-code-and-ggmap-function
# https://stackoverflow.com/questions/77947837/getting-distance-between-2-uk-postcodes-in-r 


# Return a list of lattiude and longitude for a post code
# If only the district level postcode is provided, then the first post code in the district is used
getGeoForPostcode <- function(postcode)
{
  postcodeFull <- NA
  if (length(strsplit(postcode," ")[[1]]) == 1)
  {
    # There is no space in the string so assume it is a district code only
    # Get a random postcode from the district
    postcodeFull <- random_postcode(postcode)[[1]][1]
  }
  else
  {
    # There is a space in the string so assume we have a full post code
    postcodeFull <- postcode
  }
  
  # Now we have a full post code
  postcodeGeo <- PostcodesioR::postcode_lookup(postcodeFull)
  return(c(postcodeGeo$latitude,postcodeGeo$longitude))
}


makeReferralMap <- function(rxDoneData, inputStartDate, inputEndDate)
{
  # Initialise some stuff
  startDate = as.Date(inputStartDate, format = "%d/%m/%Y")
  endDate   = as.Date(inputEndDate, format = "%d/%m/%Y")
  
  if (is.data.frame(rxDoneData) && nrow(rxDoneData>0))
  {
    # Filter just the dates we need from rxDoneData
    rxDoneData.filtered <<- rxDoneData %>% filter(between(RxDate, as.Date(inputStartDate, format = "%d/%m/%Y"), 
                                                                  as.Date(inputEndDate,   format = "%d/%m/%Y")))
    ID <- c()
    latitude <- c()
    longitude <- c()
    postcode <- c()
    rxdate <- c()
    organ <- c()
    geoRx <- NA
    
    for (i in 1:nrow(rxDoneData.filtered))
    {
      thisPostcode <- rxDoneData.filtered$Postcode[i]
      if (is.na(thisPostcode))
      {
        # Don't add to geoRx if no postcode, as we can't plot anything, just skip!
      }
      else
      {
        geo <- getGeoForPostcode(thisPostcode)
        ID <- c(ID, rxDoneData.filtered$ID)
        latitude  <- c(latitude, geo[1])
        longitude <- c(longitude, geo[2])
        postcode <- c(postcode, thisPostcode)
        rxdate <- c(rxdate, format(rxDoneData.filtered$RxDate, format = "%d %B, %Y"))
        organ <- c(organ, rxDoneData.filtered$Organs)
      }
    }
    
    # These will all appear in the tooltip text if you click on a dot on the generated map...
    geoRx <- data.frame(
      ID = ID,
      Latitude  = latitude,
      Longitude = longitude,
      Postcode  = postcode,
      TreatDate = rxdate,
      Organ     = organ
    )
  }
  logger(paste("Generating the referral map from ",inputStartDate,"-",inputEndDate,"...",sep=""))
  mapviewOptions(basemaps = "OpenStreetMap")
  referralMap <<- mapview(geoRx, xcol = "Longitude", ycol = "Latitude", crs = 4269, grid = FALSE)
  logger("Completed the referral map.")
  
  return(referralMap)
}


demo <- function()
{
  # https://www.infoworld.com/article/2271343/astonishingly-easy-mapping-in-r-with-mapview.html
  library(tigris)
  library(mapview)
  library(dplyr)
  library(sf)
  us_geo <- tigris::states(cb = TRUE, resolution = '20m')
  pop_data <- readr::read_csv("state_population_data.csv")
  all_data <- inner_join(us_geo, pop_data, by = c("GEOID" = "GEOID")) 
  mapview(all_data, zcol = "PctChange10_20")
}
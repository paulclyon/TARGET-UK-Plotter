# __init__.R contains common functions that other function files may require.
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
  "purrr",
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
  "sf", # for mapping
  "spsComps", # for shinyCatch
  "tools",
  "mapview",
  "PostcodesioR",
  "leaflet", # to render the mapview
  "lubridate",
  "withr",
  "svDialogs",
  "tinytex"
)

# Install tinyytex if not installed already
if (tinytex::check_installed("framed") == FALSE) {
  if (length(suppressWarnings(tryCatch(
    tinytex:::kpsewhich("framed.sty", stdout = TRUE), error = function(e) ""))) == 0) {
    tinytex::install_tinytex()
  }
}

# FIXME Install LaTeX packages to make PDF from HTML
#install.packages("rmarkdown")
#install.packages("knitr")
#install.packages("tinytex")
#tinytex::install_tinytex()
#install.packages("here")
#install.packages("tidyverse")
#install.packages("broom")
#install.packages("fs")
#install.packages("usethis")


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
    remotes::install_github(pkg, quiet = FALSE, host = "api.github.com")
    library(sub(".*/", "", pkg), character.only = TRUE)
  }
}

# logger function if stderr = TRUE then it goes to stderr, otherwise stdout, may want to update this to use the ShinyApp
logger <- function(msg, stderr = FALSE) {
  if (Sys.getenv("DEBUG_MODE") == T) {
    if (!is.na(stderr) && stderr != T) {
      write(toString(msg), stdout())
    } else {
      write(toString(msg), stderr())
    }
  }
  if (!exists('logger.df')) {
    logger.df <<- data.frame()
  }
  logger.df <<- rbind(logger.df,data.frame(TimeStamp=format(Sys.time(), "%a %b %d %X %Y"), IsError=stderr, Message=paste0(msg, collapse="|")))
  return()
}

# Check we have the latest Castor library, if old then force update it!
castorLibMajorVersion <- strtoi(substr(toString(packageDescription("castoRedc")$Version), 1, 1))
if (castorLibMajorVersion < 2) {
  write(paste("Attempting update of castoRedc library (currently version ", packageVersion("castoRedc"), ")..."), stderr())
  detach(package:castoRedc, unload = TRUE)
  remove.packages("castoRedc")
  remotes::install_github("castoredc/castoRedc", host = "api.github.com", upgrade = "always")
  library("castoRedc", character.only = TRUE)
}

#' Source all files in the current directory or provided directory
#'
#' These will be listed in an alphanumerical order - files starting with "_" will be read first.
#' Recursion is prevented by removing any files already included in the frame list
#'
#' @param dir the directory to load, if not provided uses the directory of the current file
sourceDirectory <- function(dir = NULL) {
  frameFiles <- sys.frames() |>
    rev() |>
    map(purrr::pluck("ofile")) |>
    purrr::discard(is.null) |>
    map(normalizePath)

  currentFile <- frameFiles[[1]]
  inDirectory <- normalizePath(dirname(currentFile))

  if (!is.null(dir)) {
    if (fs::is_absolute_path(dir)) {
      inDirectory <- dir
    } else {
      inDirectory <- normalizePath(paste(dirname(currentFile), dir, sep = "/"))
    }
  }

  # list all the files in the chosen directory in an alphanumerical order - files starting with "_" will be read first)
  file.sources <- list.files(
    inDirectory,
    pattern = "*.R",
    full.names = T,
    ignore.case = T
  ) |> purrr::map(normalizePath)


  # Discard any filenames that are already in the call frame to prevent recursion!
  file.sources <- purrr::discard(file.sources, \(x) x %in% frameFiles)

  sapply(file.sources, source, .GlobalEnv)

  directories <- list.dirs(inDirectory) |> purrr::discard(\(x) x == inDirectory)
  sapply(directories, sourceDirectory)
  # knitr cache.extra option no longer works since knitr 0.9 so this has been removed.
}

sourceDirectory()

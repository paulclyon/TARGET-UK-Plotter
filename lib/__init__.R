# __init__.R contains common functions that other function files may require.
required_base_pkgs <- c(
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
  "toastui",
  "purrr",
  "plotly",
  "ggrepel",
  "tidyverse",
  "rmarkdown",
  "here",
  "survival",
  "ggsurvfit",
  "survminer",
  "knitr",
  "webshot2",
  "htmlwidgets",
  "htmltools",
  "calendR",
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
  "scrutiny",
  "svDialogs"
)

# FIXME Install LaTeX packages to make PDF from HTML
required_latex_pkgs <- c(
  "pandoc",
  "rmarkdown",
  "knitr",
  "tinytex",
  "here",
  "tidyverse",
  "broom",
  "fs",
  "usethis"
)

# Set default CRAN mirror so user doesn't have to select
local({r <- getOption("repos")
r["CRAN"] <- "https://cran.r-project.org"
options(repos=r)})

# Install the packages
for (pkg in c(required_base_pkgs,required_latex_pkgs)) {
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

# Install tinyytex if not installed already, which installs differently
if (tinytex::check_installed("framed") == FALSE) {
  if (length(suppressWarnings(tryCatch(
    tinytex:::kpsewhich("framed.sty", stdout = TRUE), error = function(e) ""))) == 0) {
    tinytex::install_tinytex()
  }
}

# logger function if stderr = TRUE then it goes to stderr, otherwise stdout, may want to update this to use the ShinyApp
logger <- function(msg, stderr = FALSE, logOnlyAsDebug = FALSE)
{
  debugMode <- isTRUE(as.logical(Sys.getenv("DEBUG_MODE")))
  
  if (!logOnlyAsDebug || debugMode)
  {
    if (isTRUE(stderr)) {
      write(paste(msg, collapse = "|"), stderr())
    } else {
      write(paste(msg, collapse = "|"), stdout())
    }
  }
  
  if (!exists("logger.df")) {
    logger.df <<- data.frame()
  }
  
  logger.df <<- rbind(
    logger.df,
    data.frame(
      TimeStamp = format(Sys.time(), "%a %b %d %X %Y"),
      IsError   = stderr,
      Message   = paste(msg, collapse = "|")
    )
  )
  
  invisible(NULL)
}

# Add data integrity to the data integrity table and log it
addDataIntegrityError <- function(ptID = NA, refID = NA, date = NA, organs = NA, errorStr = NA)
{
  logger(paste(" > Data integrity issue Pt=",ptID," Ref=",refID," Date=",date," (",organs,")\n > ",errorStr, sep=""), FALSE)
  dataIntegrity.df <<- rbind(dataIntegrity.df,data.frame(PtID=ptID, RefID=refID, Date=date, Organs=organs, Error=errorStr))
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

# ChatGTP method to check if Google Chrome is installed on any OS
# This is useful as we cannot use webshot2 to convert calendar HTML to PDF without Google Chrome (Easily!)
isChromeInstalled <- function(executable = FALSE) {
  os <- Sys.info()[["sysname"]]
  chrome_paths <- switch(
    os,
    "Darwin" = c(
      "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
      "/Applications/Google Chrome Canary.app/Contents/MacOS/Google Chrome Canary"
    ),
    "Windows" = c(
      Sys.getenv("ProgramFiles"),
      Sys.getenv("ProgramFiles(x86)"),
      Sys.getenv("LocalAppData")
    ) |> 
      file.path("Google", "Chrome", "Application", "chrome.exe"),
    "Linux" = c(
      "/usr/bin/google-chrome",
      "/usr/bin/google-chrome-stable",
      "/usr/bin/chromium",
      "/snap/bin/chromium"
    ),
    character(0)
  )
  chrome_paths <- chrome_paths[file.exists(chrome_paths)]
  if (length(chrome_paths) == 0) {
    return(FALSE)
  }
  if (executable && os != "Windows") {
    chrome_paths <- chrome_paths[file.access(chrome_paths, 1) == 0]
    return(length(chrome_paths) > 0)
  }
  TRUE
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

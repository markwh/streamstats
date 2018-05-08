# streamstats.R
# Mark Hagemann
# 1/23/2016
# Functions for accessing the streamstats API
# Following https://cran.r-project.org/web/packages/httr/vignettes/api-packages.html
# Also hijacks code from rnoaa package.


# Convention Helper functions ---------------------------------------------

#' @importFrom httr GET timeout
#' @importFrom httr timeout
sstat_get <- function(service, arglist = list(), check = TRUE) {

  dots <- arglist[!sapply(arglist, is.null)]
  url0 <- paste0('https://streamstats.usgs.gov/streamstatsservices/',
                 service)
  append <- sstat_makeArgs(dots)

  url1 <- paste(url0, append, sep = "?")
  cat(url1)
  res <- GET(url1, timeout(seconds = getOption("timeout")))

  if (!check)
    return(res)

  tt <- sstat_check(res)
  out <- tt # was envir_makeDF(tt) but here don't want data.frames.

  attr(out, "url") <- res$url
  out
}

sstat_compact <- function(l) {
  Filter(Negate(is.null), l)
}

#' @importFrom httr content stop_for_status
sstat_check <- function(x) {
  if (!x$status_code == 200) {
    stnames <- names(content(x))
    if (!is.null(stnames)) {
      if ("developerMessage" %in% stnames | "message" %in%
          stnames) {
        stop(sprintf("Error: (%s) - %s", x$status_code,
                        sstat_compact(list(content(x)$developerMessage,
                                          content(x)$message))))
      }
      else {
        stop(sprintf("Error: (%s)", x$status_code))
      }
    }
    else {
      stop_for_status(x)
    }
  }
  else {
    stopifnot(x$headers$`content-type` == "application/json")
    res <- content(x, as = "text", encoding = "UTF-8")
    out <- jsonlite::fromJSON(res, simplifyVector = FALSE)
    if (!"results" %in% names(out)) {
      if (length(out) == 0) {
        warning("Sorry, no data found")
      }
    }
    else {
      if (class(try(out$results, silent = TRUE)) == "try-error" |
          is.null(try(out$results, silent = TRUE)))
        warning("Sorry, no data found")
    }
    return(out)
  }
}

sstat_parse <- function(req) {
  text <- content(req, as = "text")
  if (identical(text, "")) stop("No output to parse", call. = FALSE)
  jsonlite::fromJSON(text, simplifyVector = FALSE)
}

#' @importFrom magrittr "%>%"
sstat_makeArgs <- function(arglist) {
  if (length(arglist) == 0)
    return(NULL)

  # names(arglist) = toupper(names(arglist))
  arglens <- vapply(arglist, length, numeric(1))

  # stopifnot(all(sapply(arglist, is.character)))
  # stopifnot(all(sapply(arglens, `<`, 3)))


  # separate operators
  al2 <- lapply(arglist, paste, collapse = "&")

  urlArgs <- Map(paste, names(al2), unlist(al2), sep = "=") %>%
    unlist() %>%
    # c("", .) %>%
    paste(collapse = "&") %>%
    URLencode()

  urlArgs
}



#' Copied from http://stackoverflow.com/a/8751965
#'
#' library(sp)
#' library(maps)
#' library(maptools)
#' The single argument to this function, pointsDF, is a data.frame in which:
#' - column 1 contains the longitude in degrees (negative in the US)
#' - column 2 contains the latitude in degrees
#' @importFrom maps map

latlon2state <- function(lat, lon) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per state (plus DC, minus HI & AK)
  data("stateMapEnv", package = "maps")
  states <- maps::map('state', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- maptools::map2SpatialPolygons(states, IDs=IDs,
                                             proj4string=sp::CRS("+proj=longlat +datum=WGS84"))
  pointsDF <- data.frame(longitude = lon, latitude = lat)
  # Convert pointsDF to a SpatialPoints object
  pointsSP <- sp::SpatialPoints(pointsDF,
                                proj4string=sp::CRS("+proj=longlat +datum=WGS84"))

  # Use 'over' to get _indices_ of the Polygons object containing each point
  indices <- sp::over(pointsSP, states_sp)

  # Return the state names of the Polygons object containing each point
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  out <- vapply(stateNames[indices], FUN = getStateAb, character(1))
}

getStateAb <- function(statename) {
  stateind <- which(tolower(state.name) == tolower(statename))
  if(length(stateind) != 1)
    stop("State name not identified.")
  state.abb[stateind]
}

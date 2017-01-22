#' Delineate watershed based on location specification
#'
#' Uses StreamStats API http://streamstatsags.cr.usgs.gov/streamstatsservices/#
#'
#' @param xlocation X location of the most downstream point of desired study area.
#' @param ylocation Y location of the most downstream point of desired study area.
#' @param rcode 2-3 character code that identifies the Study Area (either a
#'  State or a Regional Study)
#' @param includeparameters	string	Comma separated list of region
#'  parameters to compute. Default: true, will return all parameters for region
#' @param includeflowtypes	string	Not yet implemented
#' @param includefeatures	string	true	Comma separated list of features to
#'  include in response. See Feature resource for more information.
#'  Default: true, returns delineated basin and pourpoint
#' @param crs	string ESPSG spatial reference code. The function rgdal::showEPSG
#'  might come in handy.
#' @param simplify	boolean	Whether to simplify returned result, defaut: true
#' @examples
#' \dontrun{
#' # Delineate a watershed and extract parameters using WGS83
#' ws1 <- delineateWatershed(xlocation = -72.9249, ylocation = 42.3170, crs = 4326,
#' includeparameters = "true"
#' }
#'
#' @export

delineateWatershed <- function(xlocation, ylocation, rcode = NULL,
                               includeparameters = c("false", "true"),
                               includeflowtypes = c("false", "true"),
                               includefeatures = c("true", "false"),
                               crs, simplify = NULL) {
  includeparameters <- match.arg(includeparameters)
  includeflowtypes <- match.arg(includeflowtypes)
  includefeatures <- match.arg(includefeatures)
  if (is.null(rcode))
    rcode <- latlon2state(lat = ylocation, lon = xlocation)
  args <- list(rcode = rcode, xlocation = xlocation,
               ylocation = ylocation, includeparameters = includeparameters,
               includeflowtypes = includeflowtypes,
               includefeatures = includefeatures,
               crs = crs, simplify = simplify)
  ret1 <- sstat_get("watershed.geojson", args)
  # attr(ret1, "class") <- "watershed"

  if (includeparameters) {
    ret1$parameters <- fs_toDf(ret1$parameters)
  }

  ret1 <- structure(ret1, class = "watershed")
}

#' Returns a watershed based on "workspace" info
#' Currently untested.
#' @param workspaceID	string		Service workspace received from watershed
#'  service result
#' @param rcode 2-3 character code that identifies the Study Area (either a
#'  State or a Regional Study)
#' @param includeparameters	string	Comma separated list of region
#'  parameters to compute. Default: true, will return all parameters for region
#' @param includeflowtypes	string	Not yet implemented
#' @param includefeatures	string	true	Comma separated list of features to
#'  include in response. See Feature resource for more information.
#'  Default: true, returns delineated basin and pourpoint
#' @param crs	string ESPSG spatial reference code. The function rgdal::showEPSG
#'  might come in handy.
#' @param simplify	boolean	Whether to simplify returned result, defaut: true
#' Uses StreamStats API http://streamstatsags.cr.usgs.gov/streamstatsservices/#
#' @export
watershedByWorkspace <- function(workspaceID,
                                 rcode,
                                 includeparameters = c("false", "true"),
                                 includeflowtypes = c("false", "true"),
                                 includefeatures = c("true", "false"),
                                 crs, simplify = NULL) {
  stopifnot(is(workspaceID, "character") && length(workspaceID) == 1)
  includeparameters <- match.arg(includeparameters)
  includeflowtypes <- match.arg(includeflowtypes)
  includefeatures <- match.arg(includefeatures)
  args <- list(rcode = rcode, workspaceID = workspaceID,
               includeparameters = includeparameters,
               includeflowtypes = includeflowtypes,
               includefeatures = includefeatures,
               crs = crs, simplify = simplify)
  ret1 <- sstat_get("watershed.geojson", args)

  if (includeparameters) {
    ret1$parameters <- fs_toDf(ret1$parameters)
  }

  attr(ret1, "class") <- "watershed"
  ret1
}

#' Create a leaflet map of a watershed
#' Uses an object returned by delineateWatershed() and displays the watershed
#' boundary as well as the pour point.
#' @param watershed an object of class "watershed", as returned by
#' delineateWatershed()
#' @importFrom leaflet leaflet addTiles addGeoJSON addMarkers
#' @export
leafletWatershed <- function(watershed) {
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("leaflet needed for this function to work. Please install it.",
         call. = FALSE)
  }

  pointLalo <- extractLaLo(watershed)

  if (is.null(pointLalo$ID))
    pointLalo$ID = NA

  # params = watershed$parameters
  leaflet() %>%
    addTiles() %>%
    addGeoJSON(watershed$featurecollection[[2]]$feature) %>%
    addMarkers(lng = ~lon, lat = ~lat, data = pointLalo, popup = ~ID)
}

#' Write part of a `watershed` object to file in geoJSON format.
#' @param watershed an object of class "watershed", as returned by
#' delineateWatershed()
#' @param file a character given the file to which to write the resulting
#'  geojson
#' @param what Either "boundary" or "pourpoint" describing what part of the
#'  watershed object to write.
#' @export
writeGeoJSON <- function(watershed, file, what = c("boundary", "pourpoint")) {
  what <- match.arg(what)
  if (!(grepl("\\.geojson$", file, ignore.case = TRUE) ||
         grepl("\\.json$", file, ignore.case = TRUE)))
    warning("file should probably have a .json or .geojson extension...")
  elem <- ifelse(what == "boundary", 2, 1)

  piece <- pullFeatureCollection(watershed, what = what)
  outstr <- jsonlite::toJSON(piece, auto_unbox = TRUE)
  writeLines(outstr, con = file)
  invisible(outstr)
}

#' extract part of watershed list corresponding to an individual feature collection
#' Returns a FeatureCollection.
#' @export

pullFeatureCollection <- function(ws, what = c("boundary", "pourpoint")) {
  what <- match.arg(what)
  fc <- setNames(ws$featurecollection,
                 vapply(ws$featurecollection, `[[`, character(1), "name"))
  elem <- ifelse(what == "boundary", "globalwatershed", "globalwatershedpoint")

  out <- fc[[elem]]$feature
  if (!is.list(out) || length(out$features) == 0)
    out <- NA

  out
}



#' Convert watershed to sp
#' Returns either SpatialPointsDataFrame or SpatialPolygonsDataFrame
#' @param watershed an object of class "watershed", as returned by
#' delineateWatershed(), or combineWatersheds()
#' @param what Either "boundary" or "pourpoint" describing what part of the
#'  watershed object to write.
#' @export
#' @importFrom rgdal readOGR
#'

toSp <- function(watershed, what = c("boundary", "pourpoint")) {

  if (!requireNamespace("rgdal", quietly = TRUE))
    stop("rgdal needed for this functionto work. Please install it.",
         call. = FALSE)
  what <- match.arg(what)
  tpf <- tempfile(fileext = ".geojson")
  writeGeoJSON(watershed, file = tpf, what = what)

  out <- readOGR(tpf, "OGRGeoJSON")
  unlink(tpf)
  out
}

#' Convert watershed to ESRI shapefile
#' @param watershed an object of class "watershed", as returned by
#'  delineateWatershed(), or combineWatersheds()
#' @param layer Name of layer in resulting shapefile
#' @param dir Directory where shapefile files should be written.
#' @param what Either "boundary" or "pourpoint" describing what part of the
#'  watershed object to write.
#' @export
#' @importFrom rgdal writeOGR

writeShapefile <- function(watershed, layer, dir = ".", what = c("boundary", "pourpoint")) {
  what <- match.arg(what)
  sp <- toSp(watershed, what = what)

  writeOGR(sp, dir, layer = layer, driver = "ESRI Shapefile")
}

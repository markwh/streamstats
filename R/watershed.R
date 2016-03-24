#' Delineate watershed based on location specification
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
#' @export

delineateWatershed <- function(xlocation, ylocation, rcode = NULL,
                               includeparameters = c("false", "true"),
                               includeflowtypes = c("false", "true"),
                               includefeatures = c("true", "false"),
                               crs = NULL, simplify = NULL) {
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
  attr(ret1, "class") <- "watershed"
  ret1
}

#' Returns a watershed based on "workspace" info
#' Currently untested.
#' @param xlocation X location of the most downstream point of desired study area.
#' @param ylocation Y location of the most downstream point of desired study area.
#' @param rcode 2-3 character code that identifies the Study Area (either a
#'  State or a Regional Study)
#' @param workspaceID	string		Service workspace received from watershed
#'  service result
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
watershedByWorkspace <- function(rcode = NULL,
                               workspaceID = NULL,
                               includeparameters = NULL,
                               includeflowtypes = NULL,
                               includefeatures = NULL,
                               crs = NULL, simplify = NULL) {
  args <- list(rcode = rcode, workspaceID = workspaceID,
               ylocation = ylocation, includeparameters = includeparameters,
               includeflowtypes = includeflowtypes,
               includefeatures = includefeatures,
               crs = crs, simplify = simplify)
}

#' Create a leaflet map of a watershed
#' Uses an object returned by delineateWatershed() and displays the watershed
#' boundary as well as the pour point.
#' @param watershed an object of class "watershed", as returned by
#' delineateWatershed()
#' @importFrom leaflet leaflet addTiles addGeoJSON
#' @export
leafletWatershed <- function(watershed) {
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("leaflet needed for this function to work. Please install it.",
         call. = FALSE)
  }

  leaflet() %>%
    addTiles() %>%
    addGeoJSON(watershed$featurecollection[[2]]$feature) %>%
    addGeoJSON(watershed$featurecollection[[1]]$feature)
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
         grepl("\\.json$", file, ignore.case = TURE)))
    warning("file should probably have a .json or .geojson extension...")
  elem <- ifelse(what == "boundary", 2, 1)

  piece <- watershed$featurecollection[[elem]]$feature
  outstr <- jsonlite::toJSON(piece, auto_unbox = TRUE)
  writeLines(outstr, con = file)
  invisible(outstr)
}

#
# ws1 <- westfield
# ws2 <- delineateWatershed(xlocation = -71.9588, ylocation = 42.4152, crs = 4326)
# ws1$featurecollection
# ws2$featurecollection[[1]]$feature$features
#
#
# wslist <- list(ws1, ws2)

#' Combine watershed objects into single geojson-structured list
#'
#' Returns a watershed object with elements of wslist combined into a single
#' FeatureCollection
#'
#' @param wslist list of watershed objects
#' @param id vector of identifiers to identify each watershed
#' @examples
#'   data(westfield)
#'   data(pommoqusset)
#'   combo <- combineWatersheds(list(westfield, pommoqusset), id = c("wf", "pom"))
#'   leafletWatershed(combo)
#' @export
combineWatersheds <- function(wslist, id) {
  stopifnot(all(sapply(wslist, is, "watershed")))

  # objects to be filled with all parts later on
  bnd <- wslist[[1]]$featurecollection[[2]]
  ppt <- wslist[[1]]$featurecollection[[1]]

  # extract watershed boundaries
  boundaries <- lapply(wslist, pullFeatureCollection, what = "boundary") %>%
    lapply(function(fc) fc$features[[1]]) %>%
    Map(f = addID, ftr = ., id = id)
  # extract pourpoints
  pourpoints <- lapply(wslist, pullFeatureCollection, what = "pourpoint") %>%
    lapply(function(fc) fc$features[[1]]) %>%
    Map(f = addID, ftr = ., id = id)

  # combine into FeaturecCllections
  bnd$feature$features <- boundaries
  ppt$feature$features <- pourpoints

  out <- structure(list(featurecollection = list(pourpoints = ppt,
                                                 boundaries = bnd)),
                   class = "watershed")
  out
}

addID <- function(ftr, id) {
  ftr$properties$ID <- id
  ftr
}

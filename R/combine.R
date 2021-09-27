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
#' @importFrom assertthat assert_that
#' @importFrom stats setNames
#' @export
combineWatersheds <- function(wslist, id) {
  stopifnot(all(sapply(wslist, is, "watershed")))
  wslist <- unname(wslist)
  # Make sure all used the same coordinate system.
  crslist <- lapply(wslist, function(x)
    x$featurecollection[[1]]$feature$crs)
  crscodes <- vapply(crslist, function(x) x$properties$code, numeric(1)) %>%
    unique()
  assert_that(length(crscodes) == 1)

  # extract watershed boundaries
  boundaries0 <- lapply(wslist, pullFeatureCollection, what = "boundary")
  nobounds <- is.na(boundaries0)

  if (sum(nobounds) > 0)
    warning(paste0("The following id's had watersheds with no boundary information: ",
                   paste(id[nobounds], collapse = ", ")))

  boundaries <- lapply(boundaries0[!nobounds], function(fc) fc$features[[1]]) %>%
    Map(f = addID, ftr = ., id = id[!nobounds])

  # extract pourpoints
  pourpoints0 <- lapply(wslist, pullFeatureCollection, what = "pourpoint")
  nopours <- is.na(pourpoints0)

  if (sum(nopours) > 0)
    warning(paste0("The following id's had watersheds with no pourpoint information: ",
                   paste(id[nopours], collapse = ", ")))

  pourpoints <- lapply(pourpoints0[!nopours], function(fc) fc$features[[1]]) %>%
    Map(f = addID, ftr = ., id = id[!nopours])

  # combine into FeaturecCllections
  bnd <- list(name = "globalwatershed",
              feature = list(type = "FeatureCollection",
                             crs = crslist[[1]],
                             features = boundaries))
  ppt <- list(name = "globalwatershedpoint",
              feature = list(type = "FeatureCollection",
                             crs = crslist[[1]],
                             features = pourpoints))

  out <- structure(list(featurecollection = list(pourpoints = ppt,
                                                 boundaries = bnd)),
                   class = "watershed")

  # combine parameters
  hasparams <- vapply(wslist, function(ws) !is.null(ws$parameters), logical(1))
  if (any(hasparams)) {
    paramIDs <- id[hasparams]
    allparams <- wslist[hasparams] %>%
      lapply(`[[`, "parameters") %>%
      setNames(make.names(id[hasparams])) %>%
      bind_rows(.id = "ID")

    out$parameters <- allparams
  }

  out
}

addID <- function(ftr, id) {
  ftr$properties$ID <- id
  ftr
}




# features.R
# list of spatial features computed for the study area

#' List available features for a given workspace
#' returns a returns a collection of spatial feature names available for the
#' workspace.
#' @param workspaceID	Service workspace received from watershed
#'  service result
#' @export
availFeatures <- function(workspaceID) {
  stopifnot(is(workspaceID, "character") && length(workspaceID) == 1)
  args <- list(workspaceID = workspaceID)
  ret1 <- sstat_get("features.json", args)
  ret1$features <- ret1$featurecollection %>%
    lapply(as.data.frame, stringsAsFactors = FALSE) %>%
    dplyr::bind_rows()
  out <- ret1$features

  out
}

#' Get features for a given workspace
#' returns a returns a collection of spatial feature names available for the
#' workspace.
#' @param workspaceID	Service workspace received from watershed
#'  service result
#' @param includefeatures	string	true	Comma separated list of features to
#'  include in response. See Feature resource for more information.
#'  Default: true, returns delineated basin and pourpoint
#' @param crs	string ESPSG spatial reference code. The function rgdal::showEPSG
#'  might come in handy.
#' @param simplify	boolean	Whether to simplify returned result, defaut: true
#' @export
getFeatures <- function(workspaceID, rcode, crs,
                        features = c("globalwatershedpoint",
                                            "globalwatershed"),
                        simplify = "true") {
  stopifnot(is(workspaceID, "character") && length(workspaceID) == 1)
  features <- match.arg(features, several.ok = TRUE) %>%
    paste(collapse = ",")
  args <- list(rcode = rcode, workspaceID = workspaceID, crs = crs,
               includefeatures = features, simplify = simplify)
  ret1 <- sstat_get("features.geojson", args)
  ret1$flowstatistics <- ret1$flowstatistics %>%
    lapply(as.data.frame, stringsAsFactors = FALSE) %>%
    dplyr::bind_rows()
  ret1
  attr(ret1, "class") <- "watershed"
  ret1
}

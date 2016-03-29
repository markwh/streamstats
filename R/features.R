# features.R
# list of spatial features computed for the study area

#' List available features for a given workspace
#' returns a returns a collection of spatial feature names available for the
#' workspace.
#' @param workspaceID	Service workspace received from watershed
#'  service result
#' @export
availFeatures <- function(workspaceID) {
  args <- list(workspaceID = workspaceID)
  ret1 <- sstat_get("features.json", args)
  ret1$features <- ret1$features %>%
    lapply(as.data.frame) %>%
    dplyr::bind_rows()
  ret1
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
getFeatures <- function(workspaceID, crs,
                        includefeatures = c("globalwatershedpoint",
                                            "globalwatershed"),
                        simplify = "true") {
  includefeatures <- match.arg(includefeatures, several.ok = TRUE)
  args <- list(rcode = rcode)
  ret1 <- sstat_get("features.geojson", args)
  ret1$flowstatistics <- ret1$flowstatistics %>%
    lapply(as.data.frame) %>%
    dplyr::bind_rows()
  ret1
  attr(ret1, "class") <- "watershed"
  ret1
}

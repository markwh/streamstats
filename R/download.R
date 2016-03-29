# download.R
# download workspace contents as a geodatabase or shapefiles

#' Download watershed resource based on workspace
#' returns a zip file containing the workspace contents, in either a
#' geodatabase or shape files
#'
#' @param workspaceID	string		Service workspace received from watershed
#'  service result
#' @param format either "geodatabase" or "shapefile"
#' @importFrom magrittr "%>%"
availChars <- function(workspaceID, format = c("geodatabase", "shapefile")) {
  format <- match.arg(format)
  format <- ifelse(format == "shapefile", "SHAPE", "")
  args <- list(workspaceID = workspaceID, format = format)
  ret1 <- sstat_get("download", args)
}

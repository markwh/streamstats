# download.R
# download workspace contents as a geodatabase or shapefiles

#' Download watershed resource based on workspace
#' returns a zip file containing the workspace contents, in either a
#' geodatabase or shape files
#'
#' @param workspaceID	string		Service workspace received from watershed
#'  service result
#' @param format either "geodatabase" or "shapefile"
#' @param file Where to save the data, should be .zip
#' @importFrom magrittr "%>%"
#' @importFrom httr content
downloadGIS <- function(workspaceID, file, format = c("geodatabase", "shapefile")) {

  if(!grepl("\\.zip$", file))
    warning("file should be a .zip file.")

  format <- match.arg(format)
  format <- ifelse(format == "shapefile", "SHAPE", "")
  args <- list(workspaceID = workspaceID, format = format)
  ret1 <- content(sstat_get("download", args, check = FALSE))
  writeBin(ret1, con = file)
}

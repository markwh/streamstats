# flowstats.R
# flow statistics computed for the study area.

#' Get available flow statistics for a region
#' returns a list of flowstatistics available to be computed in the selected
#' region.
#' @param rcode 2-3 character code that identifies the Study Area (either a
#'  State or a Regional Study)
#'  @export
availFlowStats <- function(rcode) {
  args <- list(rcode = rcode)
  ret1 <- sstat_get("flowstatistics.json", args)
  ret1$flowstatistics <- ret1$flowstatistics %>%
    lapply(as.data.frame, stringsAsFactors = FALSE) %>%
    dplyr::bind_rows()
  ret1
}



#' Compute flow statistics
#' Returns the computed flow statistic values based on the request configuration.
#' Currently does *not* return all of the information retrieved by the API call.
#' References cited include
#' @param rcode 2-3 character code that identifies the Study Area (either a
#'  State or a Regional Study)
#' @param workspaceID	Service workspace received from watershed
#'  service result
#' @param includeflowtypes Comma separated list of flow types
#'   to compute. Default: true, will return all flow types available for region
#' @export
computeFlowStats <- function(workspaceID, rcode,
                             includeparameters = c("true", "false")) {
  stopifnot(is(workspaceID, "character") && length(workspaceID) == 1)
  args <- list(rcode = rcode, workspaceID = workspaceID,
               includeparameters = includeparameters)
  ret1 <- sstat_get("flowstatistics.json", args)
  flowstats <- ret1$Statisitcs$Streamstats$STREAMFLOWS$STREAMFLOW
  fsnames <- vapply(flowstats, `[[`, character(1), "@name")
  fsdfs <- lapply(flowstats, formatFlowStats) %>%
    setNames(make.names(fsnames))

  out <- fsdfs

  out
}

#' @importFrom dplyr bind_rows
formatFlowStats <- function(fslist) {
  nss <- fslist$NSSproject$NSSScenario$NSSRegion %>% unlist %>%
    setNames(make.names(names(.)))
  params <- fs_toDf(fslist$REGIONS$REGION$PARAMETERS$PARAMETER)
  flow <- fs_toDf(fslist$FLOWS$FLOWTYPE$FLOW)
  out <- list(nss = nss, params = params, flow = flow)

}

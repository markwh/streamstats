# flowstats.R
# flow statistics computed for the study area.

#' Get available flow statistics for a region
#'
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
#'
#' Returns the computed flow statistic values based on the request configuration.
#' Currently does *not* return all of the information retrieved by the API call.
#' References cited include
#' @param rcode 2-3 character code that identifies the Study Area (either a
#'  State or a Regional Study)
#' @param workspaceID	Service workspace received from watershed
#'  service result
#' @param simplify if TRUE, combine results across regression regions using an area-weighted average.
#'   This will also remove the `error` column.
#' @export
computeFlowStats <- function(workspaceID, rcode, simplify = FALSE) {
  stopifnot(is(workspaceID, "character") && length(workspaceID) == 1)
  args <- list(rcode = rcode, workspaceID = workspaceID)
  ret1 <- sstat_get("flowstatistics.json", args)
  out <- parse_stats(ret1, simplify = simplify)

  out
}

#' Parse the results of `sstat_get()` into a tidy  data.frame
#'
#' @param statslist A list as returned by `sstat_get()`
#' @importFrom dplyr bind_rows
parse_stats <- function(statslist, simplify = FALSE) {
  outlist <- purrr::map(statslist, ~parse_statsgroup(., simplify = simplify))
  out <- bind_rows(outlist)
  out
}

#' Parse a statistics group
#'
#' @param statsgroup a single compoent of a list returned by `sstat_get()`
#' @inheritParams computeFlowStats
#' @importFrom dplyr mutate group_by summarize
parse_statsgroup <- function(statsgroup, simplify = FALSE) {
  regionslist <- statsgroup$RegressionRegions
  nregions <- length(regionslist)
  if (nregions > 1) regionslist <- regionslist[1:(nregions - 1)] # Last region is weighted average

  parsed_regions <- purrr::map(regionslist, ~parse_region(.)) %>%
    bind_rows()

  if (simplify) {
    parsed_regions <- parsed_regions %>%
      group_by(var_name, var_code, var_desc) %>%
      # mutate(scaled_value = value * region_wt / sum(region_wt)) %>%
      summarize(value = sum(value * region_wt / sum(region_wt)), .groups = "drop")
  }

  out <- parsed_regions %>%
    mutate(group_name = statsgroup$StatisticGroupName,
           group_id = statsgroup$StatisticGroupID)
  out
}

#' Parse a region's results
#'
#' @param region a single component of a single `sstat_get()` sublist
parse_region <- function(region) {
  out <- purrr::map(region$Results, ~parse_result(.)) %>%
    bind_rows() %>%
    mutate(region_name = region$Name,
           region_id = region$ID,
           region_code = region$Code,
           region_wt = region$PercentWeight / 100)
  out
}

#' Parse the results component of region's result list
#'
#' @param result a single component of a `sstat_get()` sub-sublist
parse_result <- function(result) {
  out <- with(result, data.frame(
    var_name = Name,
    var_code = code,
    var_desc = Description,
    value = Value
  ))
  out$error <- list(result$Errors)
  out
}

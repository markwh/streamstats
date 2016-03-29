context("workspace functions")

# These will take several minutes to run.

test_that("Functions to get, use watershed workspace work", {
  ws1 <- delineateWatershed(xlocation = -72.9249, ylocation = 42.3170, crs = 4326)
  expect_is(ws1, "watershed")

  # watershed
  ws2 <- watershedByWorkspace(ws1$workspaceID, "MA", crs = 4326)
  expect_is(ws2, "watershed")
  expect_identical(ws1$featurecollection, ws2$featurecollection)

  # characteristics
  ch1 <- computeChars(workspaceID = ws1$workspaceID, rcode = "MA")
  expect_is(ch1$parameters, "data.frame")

  # flowstats
  fs1 <- computeFlowStats(workspaceID = ws1$workspaceID, rcode = "MA")
  expect_is(fs1, "list")

  # features
  ft1 <- availFeatures(workspaceID = ws1$workspaceID)
  expect_is(ft1, "data.frame")

  ft2 <- getFeatures(workspaceID = ws1$workspaceID, rcode = "MA", crs = 4326)
  expect_is(ft2, "watershed")
  ftll <- leafletWatershed(ft2)
  expect_is(ftll, "leaflet")

  # download
  tmpf <- tempfile(fileext = ".zip")
  foo = downloadGIS(workspaceID = ws1$workspaceID, file = tmpf)
  expect_true(file.exists(tmpf))


})

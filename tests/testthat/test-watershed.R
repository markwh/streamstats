# Tests for watershed functions

context("Watershed extraction and inspection")

test_that("westfield dataset is available", {
  data(westfield)
  expect_is(westfield, "watershed")
})

test_that("leaflet plotting works for waterhsed objects", {
  data(westfield)
  ll <- leafletWatershed(westfield)
  ll
  expect_is(ll, "leaflet")
})

test_that("writing to geojson file works", {
  data(westfield)
  f1 <- paste0(tempfile(), ".geojson")
  f2 <- paste0(tempfile(), ".geojson")
  writeGeoJSON(watershed = westfield, file = f1)
  writeGeoJSON(watershed = westfield, file = f2, what = "pourpoint")
  expect_true(file.exists(f1))
  expect_true(file.exists(f2))

  ll <- leaflet() %>%
    addTiles() %>%
    addGeoJSON(readLines(f1)) %>%
    addGeoJSON(readLines(f2))

  expect_is(ll, "leaflet")
})


test_that("flowstats and such are returned when requested", {
  ws1 <- delineateWatershed(xlocation = -72.9249, ylocation = 42.3170, crs = 4326,
                            includeparameters = "true",
                            includeflowtypes = "true")
  expect_is(ws1$parameters, "data.frame")
})

test_that("watersheds can be combined", {
  data(westfield)
  data(pommoqusset)

  combo <- combineWatersheds(list(westfield, pommoqusset), c("wf", "pom"))
  expect_is(combo, "watershed")

  l1 <- leafletWatershed(combo)
  expect_is(l1, "leaflet")
  l1

})

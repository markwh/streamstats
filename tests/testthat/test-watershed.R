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

test_that("watersheds can be combined", {
  data(westfield)
  data(pommoqusset)

  combo <- combineWatersheds(list(westfield, pommoqusset), c("wf", "pom"))
  expect_is(combo, "watershed")

  l1 <- leafletWatershed(combo)
  expect_is(l1, "leaflet")
  l1

})


test_that("combined watersheds have combined parameter datasets", {

  data(westfield)
  data(pommoqusset)

  combo <- combineWatersheds(list(westfield, pommoqusset), c("wf", "pom"))

  expect_is(combo$parameters, "data.frame")

})


test_that("conversion to sp and shapefile works", {
  data("westfield")
  data("pommoqusset")
  combo <- combineWatersheds(list(westfield, pommoqusset), c("wf", "pom"))

  expect_is(toSp(combo, "boundary"), "SpatialPolygonsDataFrame")
  expect_is(toSp(combo, "pourpoint"), "SpatialPointsDataFrame")

  td <- tempdir()
  writeShapefile(combo, "bndry", td)
  writeShapefile(combo, "pourpts", td, what = "pourpoint")

  expect_is(readOGR(td, layer = "bndry"), "SpatialPolygonsDataFrame")
  expect_is(readOGR(td, layer = "pourpts"), "SpatialPointsDataFrame")
  unlink(list.files(tempdir(), full.names = TRUE))
})

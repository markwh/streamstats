context("data retrieval from server")


test_that("flowstats and such are returned when requested", {
  ws1 <- delineateWatershed(xlocation = -72.9249, ylocation = 42.3170, crs = 4326,
                            includeparameters = "true",
                            includeflowtypes = "true")
  expect_is(ws1$parameters, "data.frame")
})

test_that("features not returned when not requested", {
  ws1 <- delineateWatershed(xlocation = -72.9249, ylocation = 42.3170, crs = 4326,
                            includeparameters = "true",
                            includefeatures = "false")
  expect_equal(length(ws1$featurecollection), 0)
})

context("API status")

# create test status codes
status_test_200 <- list(status_code = 200)
status_test_401 <- list(status_code = 401)
status_test_402 <- list(status_code = 402)
status_test_300 <- list(status_code = 300)

test_that("api status", {
  expect_true(apiStatusHERE(api_call_object = status_test_200))
  expect_false(apiStatusHERE(api_call_object = status_test_300))
  expect_warning(apiStatusHERE(api_call_object = status_test_300),
                 "API request status: 300 . Please review results, if any, carefully!")
  expect_warning(apiStatusHERE(api_call_object = status_test_401),
                 "Unauthorized: Invalid authentication.")
  expect_false(apiStatusHERE(api_call_object = status_test_401))
  expect_warning(apiStatusHERE(api_call_object = status_test_402),
                 "Forbidden: Incorrect app_code or app_id in the request.")
  expect_false(apiStatusHERE(api_call_object = status_test_402))

})

context("extractDist")

test_route_obj <- list(response.route.summary.distance = 100.25)

test_that("Check inputs", {
  expect_error(extractDist(),
               "No route object specified.")
  expect_error(extractDist(route_object = test_route_obj, unit = "m"),
               "No valid measure for distance specified. Only km or mi is allowed.")
})

test_that("Check outputs", {
  expect_equal(extractDist(route_object = test_route_obj, unit = "km") * .621371,
               extractDist(route_object = test_route_obj, unit = "mi"))
  expect_true(extractDist(route_object = test_route_obj)%%1==0)
  expect_false(extractDist(route_object = test_route_obj, rnd = FALSE)%%1==0)
})

context("createWaypoint")

test_that("Check inputs", {
  expect_error(createWaypoint(lon = 16.37208),
               "Add a lattitude in the lat argument.")
  expect_error(createWaypoint(lat = 48.20849),
               "Add a longitude in the lon argument.")
})

test_that("Check outputs", {
  expect_equal(createWaypoint(48.20849, 16.37208),
               "48.20849,16.37208")
})

context("setURL")

test_that("Check in and output", {
  expect_equal(setURL(type = "routing"),
               "https://route.api.here.com/routing/7.2/calculateroute.json?")
  expect_equal(setURL(type = "geocoding"),
               "https://geocoder.api.here.com/6.2/")
  expect_equal(setURL(type = "batch geocode"),
               "https://batch.geocoder.api.here.com/6.2/")
  expect_error(setURL(type = "xxxx"),
               "Type can only be routing or geocoding.")
  expect_error(setURL(),
               "A valid type must be selected. This can only be routing or geocoding.")
})

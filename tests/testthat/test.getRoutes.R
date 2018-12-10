context("setRouteURL")

test_that("Check outputs", {
  expect_equal(setRouteURL(), "https://route.api.here.com/routing/7.2/calculateroute.json?")
  expect_equal(setRouteURL(version = "xx"), "https://route.api.here.com/routing/xx/calculateroute.json?")
  expect_equal(setRouteURL(url = "https://test.com/"), "https://test.com/7.2/calculateroute.json?")
  expect_equal(setRouteURL(url = "https://test.com/", version = "xx"), "https://test.com/xx/calculateroute.json?")
})

context("getRoute")

test_that("Check inputs", {
  expect_message(getRoute(app_id = "x", app_code = "x", waypoint0 = "52.5,13.45", waypoint1 = "52.5,13.45"),
                 "No url in the input. Using the the standard url from setRouteURL().")
  expect_error(getRoute(app_code = "x"),
               "No app id in the input.")
  expect_error(getRoute(app_id = "x"),
               "No app code in the input.")
  expect_error(getRoute(app_id = "x", app_code = "x"),
               "No waypoint 0 in the input.")
  expect_error(getRoute(app_id = "x", app_code = "x", waypoint0 = "52.5,13.45"),
               "No waypoint 1 in the input.")
  expect_message(getRoute(app_id = "x", app_code = "x", waypoint0 = "x", waypoint1 = "x", vehicle = "car"),
                 "Calculation route for: car.")
  expect_message(getRoute(app_id = "x", app_code = "x", waypoint0 = "x", waypoint1 = "x", vehicle = "truck"),
                 "Calculation route for: truck.")
  expect_message(getRoute(app_id = "x", app_code = "x", waypoint0 = "x", waypoint1 = "x"),
                 "Calculation route for: truck.")
  expect_error(getRoute(app_id = "x", app_code = "x", waypoint0 = "x", waypoint1 = "x", vehicle = "x"),
               "x is no valid vehicle type selected. Only truck or car is allowed.")
})

context("diffDistTruckCar")

test_that("Check output", {
  expect_error(diffDistTruckCar(app_id = app_id, app_code = "xxx", waypoint0 = "52.5,13.4", waypoint1 = "54.5,13", rnd = F))
  expect_true(is.list(diffDistTruckCar(app_id = "xxx", app_code = "xxx", waypoint0 = "52.5,13.4", waypoint1 = "54.5,13", rnd = F)))
})



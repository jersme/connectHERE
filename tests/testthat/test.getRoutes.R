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

context("diffDistTruckCarBatch")

test_that("Check input", {
  expect_error(diffDistTruckCarBatch(app_id = "xxx", app_code = "xxx"),
               "Select the data frame containing waypoint 0 and 1")
  expect_error(diffDistTruckCarBatch(df = "x", app_id = "xxx", app_code = "xxx"),
               "Dataframe selected is not of type data.frame")
})

test_that("Check output" ,{
  expect_equal(diffDistTruckCarBatch(df = data.frame(waypoint0 = "xxx", waypoint1 = "yyy"), app_id = "xxx", app_code = "xxx")[1, 1],
               as.factor("xxx"))
  expect_equal(diffDistTruckCarBatch(df = data.frame(waypoint0 = "xxx", waypoint1 = "yyy"), app_id = "xxx", app_code = "xxx")[1, 2],
               as.factor("yyy"))
  expect_equal(diffDistTruckCarBatch(df = data.frame(waypoint0 = "xxx", waypoint1 = "yyy"), app_id = "xxx", app_code = "xxx")[1, 3],
               99999)
  expect_equal(diffDistTruckCarBatch(df = data.frame(waypoint0 = "xxx", waypoint1 = "yyy"), app_id = "xxx", app_code = "xxx")[1, 4],
               99999)
  expect_equal(nrow(diffDistTruckCarBatch(df = data.frame(waypoint0 = c("xxx", "yyy"), waypoint1 = c("yyy", "xxx")), app_id = "xxx", app_code = "xxx")),
               2)
})



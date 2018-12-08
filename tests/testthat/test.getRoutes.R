context("setRouteURL")

test_that("Check outputs", {
  expect_equal(setRouteURL(), "https://route.api.here.com/routing/7.2/calculateroute.json?")
  expect_equal(setRouteURL(version = "xx"), "https://route.api.here.com/routing/xx/calculateroute.json?")
  expect_equal(setRouteURL(url = "https://test.com/"), "https://test.com/7.2/calculateroute.json?")
  expect_equal(setRouteURL(url = "https://test.com/", version = "xx"), "https://test.com/xx/calculateroute.json?")
})

context("API status")

test_object_true <- numeric(0)
test_object_true$status_code <- 200
test_object_true <- as.data.frame(test_object_true)

test_object_401 <- numeric(0)
test_object_401$status_code <- 401
test_object_401 <- as.data.frame(test_object_401)

test_object_402 <- numeric(0)
test_object_402$status_code <- 402
test_object_402 <- as.data.frame(test_object_402)

test_object_false <- numeric(0)
test_object_false$status_code <- 300
test_object_false <- as.data.frame(test_object_false)

test_that("api status", {
  expect_true(apiStatusHERE(api_call_object = test_object_true))
  expect_false(apiStatusHERE(api_call_object = test_object_false))
  expect_warning(apiStatusHERE(api_call_object = test_object_false),
                 "API request status: 300 . Please review results, if any, carefully!")
  expect_warning(apiStatusHERE(api_call_object = test_object_401),
                 "Unauthorized: Invalid authentication.")
  expect_false(apiStatusHERE(api_call_object = test_object_401))
  expect_warning(apiStatusHERE(api_call_object = test_object_402),
                 "Forbidden: Incorrect app_code or app_id in the request.")
  expect_false(apiStatusHERE(api_call_object = test_object_402))

})

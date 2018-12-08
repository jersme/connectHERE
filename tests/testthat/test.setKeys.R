context("setID")

test_that("Check inputs", {
  expect_error(setID(), "Please fill in your HERE REST App ID.")
  expect_warning(setID(id = "xxx"), "Non standard key length.")
  expect_message(setID(id = "xxxxxxxxxxxxxxxxxxxx"), "Key length correct.")
})

test_that("Check outputs", {
  expect_equal(setID(id = "xxxxxxxxxxxxxxxxxxxx"), "xxxxxxxxxxxxxxxxxxxx")
})

context("setCode")

test_that("Check inputs", {
  expect_error(setCode(), "Please fill in your HERE REST App Code.")
  expect_warning(setCode(code = "xxx"), "Non standard key length.")
  expect_message(setCode(code = "xxxxxxxxxxxxxxxxxxxxxx"), "Key length correct.")
})

test_that("Check outputs", {
  expect_equal(setCode(code = "xxxxxxxxxxxxxxxxxxxxxx"), "xxxxxxxxxxxxxxxxxxxxxx")
})

context("setID")

test_that("Check inputs", {
  expect_error(setID(), "Please fill in your HERE REST App ID.")
  expect_warning(setID(id = "xxx"), "Non standard key length.")
  expect_message(setID(id = "xxxxxxxxxxxxxxxxxxxxxx"), "Key length correct.")
})

test_that("Check outputs", {
  expect_equal(setID(id = "xxxxxxxxxxxxxxxxxxxxxx"), "xxxxxxxxxxxxxxxxxxxxxx")
})

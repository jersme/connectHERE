context("getBatchGeocodes")

test_that("createBatchJob input", {
  expect_warning(createBatchJob(app_id = "xxx", app_code = "xxx", indelim = "|", outdelim = "|", body = "xxx", action = "run", mailto = "test@test.com", outputcombined = FALSE, outcols = "xxx"),
               "No URL specified, using the standard url from setURL().")
  expect_error(createBatchJob(url = "xxx", app_code = "xxx", indelim = "|", outdelim = "|", body = "xxx", action = "run", mailto = "test@test.com", outputcombined = FALSE, outcols = "xxx"),
               "Please specify app id in function.")
  expect_error(createBatchJob(url = "xxx", app_id = "xxx", indelim = "|", outdelim = "|", action = "run",body = "xxx",  mailto = "test@test.com", outputcombined = FALSE, outcols = "xxx"),
               "Please specify app code in function.")
  expect_warning(createBatchJob(url = "xxx", app_id = "xxx", app_code = "xxx", outdelim = "|", body = "xxx", action = "run", mailto = "test@test.com", outputcombined = FALSE, outcols = "xxx"),
                 "No in delimeter specified, using | as delimeter.")
  expect_warning(createBatchJob(url = "xxx", app_id = "xxx", app_code = "xxx", indelim = "|", body ="xxx", action = "run", mailto = "test@test.com", outputcombined = FALSE, outcols = "xxx"),
                 "No out delimeter specified, using | as delimeter.")
  expect_warning(createBatchJob(url = "xxx", app_id = "xxx", app_code = "xxx", indelim = "|",  outdelim = "|", body = "xxx", mailto = "test@test.com", outputcombined = FALSE, outcols = "xxx"),
                 "No out action specified, the default action is run. This means that the batch encoding will take place and will use a HERE credit for each line in the body!")
  expect_message(createBatchJob(url = "xxx", app_id = "xxx", app_code = "xxx", indelim = "|", outdelim = "|", action = "run", body = "xxx", outputcombined = FALSE, outcols = "xxx"),
                 "No out email address specified; please check status manually via checkBatchStatus().")
  expect_error(createBatchJob(url = "xxx", app_id = "xxx", app_code = "xxx", indelim = "|", outdelim = "|", action = "run", body = "xxx", mailto = "test@test.com", outputcombined = FALSE),
               "Please specify the outcols.")
  expect_warning(createBatchJob(url = "xxx", app_id = "xxx", app_code = "xxx", indelim = "|", outdelim = "|", action = "run", body = "xxx", mailto = "test@test.com", outcols = "xxx"),
               "No ouputcombined specified, using FALSE.")
  expect_error(createBatchJob(url = "xxx", app_id = "xxx", app_code = "xxx", indelim = "|", outdelim = "|", action = "run", mailto = "test@test.com", outcols = "xxx", outputcombined = FALSE),
                 "No body present, please add body to the request.")

})

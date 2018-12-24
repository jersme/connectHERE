createBatchJob <- function(url = setURL(type = "batch geocode"), app_id, app_code, indelim = "|" , outdelim = "|", action = "run", mailto, outcols, outputcombined = FALSE) {

  # set url
  if (methods::hasArg(url) == FALSE) {
    warning("No URL specified, using the standard url from setURL()")
  }

  # check for app_id and app_code
  if (methods::hasArg(app_id) == FALSE) {
    stop("Please specify app id in function.")
  }
  if (methods::hasArg(app_code) == FALSE) {
    stop("Please specify app code in function.")
  }

  # check in and output delimters
  if (methods::hasArg(indelim) == FALSE) {
    warning("No in delimeter specified, using | as delimeter.")
  }
  if (methods::hasArg(outdelim) == FALSE) {
    warning("No out delimeter specified, using | as delimeter.")
  }

  # check for action
  if (methods::hasArg(action) == FALSE) {
    warning("No out action specified, the default action is run. This means that the batch encoding will take place and will use a HERE credit for each line in the body!")
  }

  # check for mailto
  if (methods::hasArg(mailto) == FALSE) {
    message("No out email address specified; please check status manually via checkBatchStatus().")
  }

  # check for outcols
  if (methods::hasArg(outcols) == FALSE) {
    stop("Please specify the outcols.")
  }

  # check for combined output
  if (methods::hasArg(outputcombined) == FALSE) {
    warning("No ouputcombined specified, using FALSE.")
  }



  req_url <- paste0(url, "jobs?")
  print(req_url)

}

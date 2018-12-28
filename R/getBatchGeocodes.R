#' Batch geocode request
#'
#' @param url URL to call the API. If no URL is specified the standarc URL
#' will be used.
#' @param app_id App id for the HERE REST API call. See the vignette: Getting
#' Started how to get an App Id.
#' @param app_code App Code for the HERE REST API call. See the vignette: Getting
#' Started how to get an App Code.
#' @param indelim Field delimeter in the input data.
#' @param outdelim Field delimeter in the output data.
#' @param body Txt file with the data for which the geocodes need te be added.
#' @param action Type of request, run is the standard,
#' @param mailto Email address where completion notification is sent.
#' @param outcols List of columns to return in the output.
#' @param outputcombined If true, the output of successful and unsuccessful
#' (empty response) Geocoder queries are combined into a single
#' result_YYYYMMDD-HH-MM__out.txt file. If false, they are returned in
#' two separate files ; result_YYYYMMDD-HH-MM__out.txt and
#' result_YYYYMMDD-HH-MM__err.txt.
#'
#' @return HERE RequestID
#' @export
#'
#' @examples
#' \dontrun{
#' createBatchJob(url = "xxx", app_id = "xxx", app_code = "xxx", indelim = "|", outdelim = "|", body = "xxx", action = "run", mailto = "test@test.com", outcols = "xxx", outputcombined = FALSE)
#' }
createBatchJob <- function(url = setURL(type = "batch geocode"), app_id, app_code, indelim = "|" , outdelim = "|", body, action = "run", mailto, outcols, outputcombined = FALSE) {

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

  # check for body
  if (methods::hasArg(body) == FALSE) {
    stop("No body present, please add body to the request.")
  }

  # get the locations in the body
  #req_body <- readr::read_delim(file = body, delim = indelim)
  #print(req_body)



  req_url <- paste0(url, "jobs?")
  return(req_url)

}

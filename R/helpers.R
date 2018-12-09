#' Checks HERE REST API status
#'
#' @param api_call_object The API status object as returned from the httr
#' package.
#'
#' @return The API status in TRUE or FALSE
#' @export
#'
#' @examples
#' \dontrun{
#' apiStatusHERE()
#' }
apiStatusHERE <- function (api_call_object) {

  # check the status
  if (api_call_object$status_code == 200) {
    message("API request status: ok.")
    status <- TRUE
  } else if (api_call_object$status_code == 401) {
    warning("Unauthorized: Invalid authentication.")
    status <- FALSE
  } else if (api_call_object$status_code == 402) {
    warning("Forbidden: Incorrect app_code or app_id in the request.")
    status <- FALSE
  } else {
    warning(paste("API request status:", api_call_object$status_code, ". Please review results, if any, carefully!"))
    status <- FALSE
  }

  # return the status code
  return(status)

}

#' Extracts the distance from a getRoute() result.
#'
#' @param route_object The object as otputted by the getRoute() function.
#' @param unit The unit of measure of the outputted distance object. Currently
#' only "km" and "mi" are an option.
#' @param rnd Rounds the result. TRUE is the standard setting.
#'
#' @return The distance from a getRoute() object in km or mi.
#' @export
#'
#' @examples
#' route_obj <- list(response.route.summary.distance = 100.25)
#' extractDist(route_object = route_obj)
extractDist <- function(route_object, unit = "km", rnd = TRUE) {

  # check for route object
  if (methods::hasArg(route_object) == FALSE) {
    stop("No route object specified.")
  }

  # get the distance from route object.
  dist_km <- route_object$response.route.summary.distance / 1000

  # calculate miles
  if (unit == "mi") {
    dist <- dist_km * .621371
  } else if (unit == "km") {
    dist <- dist_km
  } else {
    stop("No valid measure for distance specified. Only km or mi is allowed.")
  }

  # round
  if (rnd == TRUE) {
    dist <- round(dist)
  }

  # returns the distance value
  return(dist)
}

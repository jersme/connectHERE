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

#' Helper function to create a waypoint from a lattitude and a
#' longitude.
#'
#' @param lat Lattitude coordinates
#' @param lon Longitude coordinates
#'
#' @return A waypoint geocoordinates object as use is the HERE
#' routing API>
#' @export
#'
#' @examples createWaypoint(48.20849, 16.37208)
createWaypoint <- function(lat , lon) {

  # check if lat and lon are present
  if (methods::hasArg(lat) == FALSE) {
    stop("Add a lattitude in the lat argument.")
  }

  if (methods::hasArg(lon) == FALSE) {
    stop("Add a longitude in the lon argument.")
  }

  # create the correct waypoint format
  wp <- paste0(lat, ",", lon)

  # return the waypoint
  return(wp)

}


#' Helper function to set the url in the API calls to the production
#' standards.
#'
#' @param type The type of the API call. Can only be "routing" or "geocoding".
#'
#' @return The standard production url for the selected type.
#' @export
#'
#' @examples
#' setURL(type = "routing")
#' setURL(type = "geocoding")
setURL <- function(type) {

  # check for input
  if (methods::hasArg(type) == FALSE) {
    stop("A valid type must be selected. This can only be routing or geocoding.")
  }

  # routing url
  if (type == "routing") {

    # product standards settings
    route_url <-  "https://route.api.here.com/routing/"
    version <-  "7.2"
    last_part <- "/calculateroute.json?"

    url <- paste0(route_url, version, last_part)

  } else if (type == "geocoding") {

    # product standards settings
    geocode_url <- "https://geocoder.api.here.com/"
    version <-  "6.2/"

    url <- paste0(geocode_url, version)

  } else if (type == "batch geocode") {

    # product standards settings
    batch_geocode_url <- "https://batch.geocoder.api.here.com/"
    version <-  "6.2/"

    url <- paste0(batch_geocode_url, version)

  } else {

    stop("Type can only be routing or geocoding.")

  }

  # return the url
  return(url)

}

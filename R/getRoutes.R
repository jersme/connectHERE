#' Set routing url
#'
#' @param url The base url for here API. Standard is
#' "https://route.api.here.com/routing/"
#' @param version The version of the API to be called.
#' Standard is version 7.2
#'
#' @return The desired url for the API call.
#' @export
#'
#' @examples setRouteURL()
setRouteURL <- function(url = "https://route.api.here.com/routing/", version = "7.2") {

  # create url
  last_part <- "/calculateroute.json?"
  route_url <- paste0(url, version, last_part)

  # return the url
  return(route_url)

}

#' Get route information
#'
#' @param url The base url as required to connect to the HERE API. If no value
#' is passed in this parameter the standard parameter from the setRouteURL() is
#' used. See the documentation for the setRouteURL() for more information about
#' the base url.
#' @param app_id App id for the HERE REST API call. See the vignette: Getting
#' Started how to get an App Id.
#' @param app_code App Code for the HERE REST API call. See the vignette: Getting
#' Started how to get an App Code.
#' @param waypoint0 Starting point as a position with in geocoordinates.
#' @param waypoint1 End point as a position with in geocoordinates.
#' @param vehicle Vehicle type for which to calculate the route. Only two
#' options are possible; "truck" or "car". The paramters defaults to "truck".
#'
#' @return A data.frame with all the information about the requested route.
#' @export
#'
#' @examples
#' \dontrun{
#' getRoute(app_id = "xxx", app_code = "xxx", waypoint0 = "52.5,13.45", waypoint1 = "53.5,13.45")
#' }
getRoute <- function(url = setRouteURL(), app_id, app_code, waypoint0, waypoint1, vehicle = "truck") {

  # check for standard arguments
  if (methods::hasArg(url) == FALSE) {
    message("No url in the input. Using the the standard url from setRouteURL().")
  }

  # check for app id and code
  if (methods::hasArg(app_id) == FALSE) {
    stop("No app id in the input.")
  }

  if (methods::hasArg(app_code) == FALSE) {
    stop("No app code in the input.")
  }

  # check for waypoints
  if (methods::hasArg(waypoint0) == FALSE) {
    stop("No waypoint 0 in the input.")
  }

  if (methods::hasArg(waypoint1) == FALSE) {
    stop("No waypoint 1 in the input.")
  }

  # check for vehicle type
  if (vehicle == "truck" | vehicle == "car") {
    message(paste0("Calculation route for: ", vehicle, "."))
  } else {
    stop(paste(vehicle, "is no valid vehicle type selected. Only truck or car is allowed."))
  }

  # call api
  mode_param <- paste0("&mode=fastest;",
                       vehicle,
                       ";traffic:disabled")
  url_call <- paste0(url,
                     "app_id=", app_id,
                     "&app_code=", app_code,
                     "&waypoint0=geo!", waypoint0,
                     "&waypoint1=geo!", waypoint1,
                     mode_param,
                     "&limitedWeight=30.5&height=4.25&shippedHazardousGoods=harmfulToWater")
  tryCatch({
    api_call <- httr::GET(url = url_call)
  })


  # check call status
  call_status <- apiStatusHERE(api_call_object = api_call)

  # get the results out if the API
  results <- as.data.frame(jsonlite::fromJSON(httr::content(api_call, "text"), flatten = TRUE))

  # return the results
  return(results)

}

#' Return delta in distance between truck and car routes
#'
#' @param url The base url as required to connect to the HERE API. If no value
#' is passed in this parameter the standard parameter from the setRouteURL() is
#' used. See the documentation for the setRouteURL() for more information about
#' the base url.
#' @param app_id App id for the HERE REST API call. See the vignette: Getting
#' Started how to get an App Id.
#' @param app_code App Code for the HERE REST API call. See the vignette: Getting
#' Started how to get an App Code.
#' @param waypoint0 Starting point as a position with in geocoordinates.
#' @param waypoint1 End point as a position with in geocoordinates.
#' @param unit The unit of measure of the outputted distance object. Currently
#' only "km" and "mi" are an option.
#' @param rnd Rounds the result. TRUE is the standard setting.
#'
#' @return A list with the truck and car distance
#' @export
#'
#' @examples
#' \dontrun{
#' diffDistTruckCar(
#' app_id = "xxx",
#' app_code = "xxx",
#' waypoint0 = "52.5,13.4",
#' waypoint1 = "54.5,13")
#' }
diffDistTruckCar <- function(url = setRouteURL(), app_id, app_code, waypoint0, waypoint1, unit = "km", rnd = TRUE) {

  # car/truck distance
  tryCatch({
    car_dist <- extractDist(route_object = getRoute(url = url, app_id = app_id, app_code = app_code, waypoint0 = waypoint0, waypoint1 = waypoint1, vehicle = "car"), unit = unit, rnd = rnd)
    truck_dist <- extractDist(route_object = getRoute(url = url, app_id = app_id, app_code = app_code, waypoint0 = waypoint0, waypoint1 = waypoint1, vehicle = "truck"), unit = unit, rnd = rnd)
  })


  # create results list
  results <- list(car = car_dist, truck = truck_dist)

  # return(results)
  return(results)

}

#' Batch input (data.frame) for the diffDistTruckCar() function.
#'
#' @param url The base url as required to connect to the HERE API. If no value
#' is passed in this parameter the standard parameter from the setRouteURL() is
#' used. See the documentation for the setRouteURL() for more information about
#' the base url.
#' @param app_id App id for the HERE REST API call. See the vignette: Getting
#' Started how to get an App Id.
#' @param app_code App Code for the HERE REST API call. See the vignette: Getting
#' Started how to get an App Code.
#' @param df Dataframe where for the distances will be calculated based on the
#' geocoordinates.
#' @param waypoint0 Name of the column for the waypoint 0.
#' @param waypoint1 Name of the column for the waypoint 0.
#'
#' @return The truck and car distances for each each row in the inputted
#' dataframe.
#' @export
#'
#' @examples
#' diffDistTruckCarBatch(
#' df = data.frame(waypoint0 = c("xxx", "yyy"), waypoint1 = c("yyy", "xxx")),
#' app_id = "xxx",
#' app_code = "xxx"
#' )
diffDistTruckCarBatch <- function(url = setRouteURL(),app_id, app_code, df, waypoint0 = "waypoint0", waypoint1 = "waypoint1") {

  # check the inputs
  if (methods::hasArg(df) == FALSE ) {
    stop("Select the data frame containing waypoint 0 and 1")
  }

  if (is.data.frame(df) == FALSE) {
    stop("Dataframe selected is not of type data.frame")
  }

  # create an empty data.frame to add the results later
  distance_all <- data.frame()

  # create a progress bar
  pb <- progress::progress_bar$new(total = nrow(df))

  # run the api call over every function
  for (i in seq(1, nrow(df))) {

    # api call
    result <- tryCatch(diffDistTruckCar(app_id = app_id, app_code = app_code, waypoint0 = df[i,waypoint0], waypoint1 = df[i,waypoint1]),
                       warning = function(err_list) {list(car = 99999, truck = 99999)
                       })

    # create the single row data.frame and catch error if the api returns an error code
    distance <- tryCatch(data.frame(car = result$car, truck = result$truck),
                         error = function(err_df){data.frame(car = 99999, truck = 99999)
                      })

    # bind the for loop rows to the data frame
    distance_all <- rbind(distance_all, distance)

    # progress bar tick
    pb$tick()
  }

  distances <- dplyr::bind_cols(df, distance_all)

  # return  the data frame
  return(distances)
}






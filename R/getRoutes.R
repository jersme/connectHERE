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

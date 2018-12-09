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

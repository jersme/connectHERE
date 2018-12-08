#' Set HERE App ID
#'
#' @param id The project HERE App ID.
#'
#' @return The checked App ID.
#' @export
#'
#' @examples setID(id = "xxxxxxxxxxxxxxxxxxxxxx")
setID <- function(id) {

  # check id argument
  if (methods::hasArg(id) == FALSE) {
    stop("Please fill in your HERE REST App ID.")
  }

  # check for id length
  if (nchar(id) == 22) {
    message("Key length correct.")
  } else if (nchar(id) != 22) {
    warning("Non standard key length.")
  } else {
    stop("Function error. Please contact package maintainer.")
  }

  # return the keylength
  return(id)
}

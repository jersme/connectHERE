#' Set HERE App ID
#'
#' @param id The project HERE App ID.
#'
#' @return The checked App ID.
#' @export
#'
#' @examples setID(id = "xxxxxxxxxxxxxxxxxxxx")
setID <- function(id) {

  # check id argument
  if (methods::hasArg(id) == FALSE) {
    stop("Please fill in your HERE REST App ID.")
  }

  # check for id length
  if (nchar(id) == 20) {
    message("Key length correct.")
  } else if (nchar(id) != 20) {
    warning("Non standard key length.")
  } else {
    stop("Function error. Please contact package maintainer.")
  }

  # return the keylength
  return(id)
}

#' Set HERE App ID
#'
#' @param code The project HERE App Code.
#'
#' @return The checked App Code.
#' @export
#'
#' @examples setCode(code = "xxxxxxxxxxxxxxxxxxxxxx")
setCode <- function(code) {
  # check id argument
  if (methods::hasArg(code) == FALSE) {
    stop("Please fill in your HERE REST App Code.")
  }

  # check for id length
  if (nchar(code) == 22) {
    message("Key length correct.")
  } else if (nchar(code) != 22) {
    warning("Non standard key length.")
  } else {
    stop("Function error. Please contact package maintainer.")
  }

  # return the keylength
  return(code)
}

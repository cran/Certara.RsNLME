#' Return theta names and values
#'
#' Returns named character vector of theta values by parsing PML fixed effect statements
#'
#' @param model    PK/PD model
#'
#' @examples
#' \donttest{
#' getThetas(pkpdmodel)
#' }
#' @return Character vector of theta names defined in model
#' @export
getThetas <- function(model) {

  splittedFixefsStrings <- .get_fixefStrings(model)
  thetas <- .transform_FixefsStringsToThetas(splittedFixefsStrings)

  thetas
}

.transform_FixefsStringsToThetas <- function(splittedFixefsStrings) {
  thetas <- list()
  for (fixefString in splittedFixefsStrings) {
    if (length(fixefString) < 4) {
      warning(
        "Cannot parse the following modelInfo string:\n",
        paste(fixefString)
      )
      next()
    }
    name <- fixefString[1]
    value <- fixefString[4]
    thetas[[name]] <- value
  }

  thetas
}

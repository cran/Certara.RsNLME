#' Return covariate names
#'
#' Use to return character vector of covariate names available in model object.
#'
#' @param model Model object
#'
#' @examples
#' model <- pkmodel(columnMap = FALSE,
#'                  workingDir = tempdir())
#' model <- addCovariate(model, covariate = "BW", effect = "V")
#' model <- addCovariate(model, covariate = "Age", effect = "Cl")
#'
#' covariateNames(model)
#'
#' @return Character vector of covariate names defined in model
#' @export
covariateNames <- function(model) {
  covariateArray <- sapply(
    model@covariateList,
    function(x) {
      x@name
    }
  )

  covariateArray <-
    covariateArray[!is.null(covariateArray) & covariateArray != ""]

  return(covariateArray)
}

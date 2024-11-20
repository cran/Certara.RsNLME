#' NLME Profile variable
#'
#' Class initializer for an NLME profile perturbation variable
#'
#' @param effectName Name of fixed effect
#' @param initialvalue Initial value
#' @param pertubateValues Values to perturbate by (either delta or percentage)
#' @export ProfileVar
#' @keywords internal
#' @examples
#' ProfileVar("tvV", 9.95, "-2,-1,0,1,2")
ProfileVar <- setClass(
  "ProfileVar",
  slots = c(
    effectName = "character",
    initialValue = "numeric",
    pertubateValues = "character"
  )
)


setMethod("initialize", "ProfileVar",
  function(.Object, effectName, initialValue, pertubateValues) {
    .Object@effectName <- effectName
    .Object@initialValue <- initialValue
    .Object@pertubateValues <- pertubateValues
    .Object
  }
)

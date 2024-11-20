PARAM_INTRAVENOUS <- 1

PARAM_EXTRAVASCULAR <- 2

PARAM_GAMMA <- 3

PARAM_WEIBULL <- 4

PARAM_INVERSEGAUSSIAN <- 5

Intravenous <- 1

Extravascular <- 2

Gamma <- 3

Weibull <- 4

InverseGaussian <- 5



#' Class represents an NLME/PML model absorption
#'
#' Class represents an NLME/PML model absorption
#'
#' @param absorpType   Intravenous|Extravascular
#'
#' @examples
#' \donttest{
#' NlmeModelAbsorption(PARAM_EXTRAVASCULAR)
#' }
#' @keywords internal
setClass(
  "NlmeModelAbsorption",
  slots = c(absorpType = "numeric"),
  prototype = list(absorpType = PARAM_EXTRAVASCULAR)
) -> NlmeModelAbsorption

setMethod("initialize", "NlmeModelAbsorption",
  function(.Object,
           absorpType = PARAM_EXTRAVASCULAR) {
    if (absorpType < PARAM_INTRAVENOUS ||
        absorpType > PARAM_INVERSEGAUSSIAN) {
      warning(paste("absorpType", absorpType, " is not supported!"))
      absorpType <- PARAM_EXTRAVASCULAR
    }
    .Object@absorpType <- absorpType
    .Object
  }
)

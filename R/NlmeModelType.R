PARAM_PK_LINEAR <- 1
PkLinear <- 1
PARAM_PK <- 2
Pk <- 2
PARAM_EMAX <- 3
Emax <- 3
PARAM_PK_EMAX <- 4
PkEmax <- 4
PARAM_PK_INDIRECT <- 5
PkIndirect <- 5
PARAM_LINEAR <- 6
Linear <- 6
Blank <- 7
PhoenixModel <- 8

ModelTypeNames <- c(
  "PK_LINEAR",
  "PK",
  "EMAX",
  "PK_EMAX",
  "PK_INDIRECT",
  "LINEAR",
  "BLANK",
  "PHOENIX"
)
#' Class represents an NLME/PML model type
#'
#' Class represents an NLME/PML model type
#'
#' @param modelType Model type as numeric value from \code{1:7}
#'
#' @examples
#' \donttest{
#' NlmeModelType(PK)
#' }
#' @keywords internal
setClass(
  "NlmeModelType",
  slots = c(modelType = "numeric"),
  prototype = list(modelType = PARAM_PK)
) -> NlmeModelType

setMethod("initialize", "NlmeModelType",
  function(.Object,
           modelType = PARAM_PK) {
    if (modelType < PARAM_PK_LINEAR || modelType > PhoenixModel) {
      warning(paste("modelType", modelType, " is not supported!"))
      modelType <- PARAM_PK
    }
    .Object@modelType <- modelType
    # .Object
    validObject(.Object)
    return(.Object)
  }
)

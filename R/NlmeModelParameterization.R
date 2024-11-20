LIMITED_STIM <- 1
#LimitedStimulation <- 1
INFINITE_STIM <- 2
InfiniteStimulation <- 2
LIMITED_INHIB <- 3
# LimitedInhibition <- 3
INVERSE_INHIB <- 4
InverseInhibition <- 4
LINEAR_STIM <- 5
# LinearStimulation <- 5
LOG_LINEAR_STIM <- 6
# LogLinearStimulation <- 6

.indirectTypeNames <-
  c(
    "LimitedStimulation",
    "InfiniteStimulation",
    "LimitedInhibition",
    "InverseInhibition",
    "LinearStimulation",
    "LogLinearStimulation"
  )

LinearAlpha <- 1
LinearBeta <- 2
LinearGamma <- 3

PARAM_MICRO <- 1
Micro <- 1
PARAM_CLEARANCE <- 2
Clearance <- 2
PARAM_MACRO <- 3
Macro <- 3
PARAM_MACRO1 <- 4
Macro1 <- 4

#' Class represents an NLME/PML model parameterization
#'
#' Class represents an NLME/PML model parameterization
#'
#' @param paramType    One of Micro|Clearance|Macro|Macro1
#'
#' @examples
#' \donttest{
#' NlmeModelParameterization(PARAM_CLEARANCE)
#' }
#' @keywords internal
setClass(
  "NlmeModelParameterization",
  slots = c(paramType = "numeric"),
  prototype = list(paramType = PARAM_CLEARANCE)
) -> NlmeModelParameterization

setMethod("initialize", "NlmeModelParameterization",
  function(.Object,
           paramType = PARAM_CLEARANCE) {
    if (paramType < PARAM_MICRO || paramType > PARAM_MACRO1) {
      warning(paste("paramType", paramType, " is not supported!"))
    }
    .Object@paramType <- paramType
    .Object
  }
)

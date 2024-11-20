#' Class represents an NLME/PML Emax model parameters
#'
#' Class represents an NLME/PML Emax model parameters
#'
#' @param checkBaseline    Model has a baseline response
#' @param checkFractional  Model is fractional
#' @param checkInhibitory  Model is inhibitory
#' @param checkSigmoid     Model is sigmoidal
#'
#' @keywords internal
setClass(
  "NlmeEmaxParameters",
  slots = c(
    checkBaseline = "logical",
    checkFractional = "logical",
    checkInhibitory = "logical",
    checkSigmoid = "logical",
    hasEffectsCompartment = "logical",
    frozen = "logical"
  )
) -> NlmeEmaxParameters


setMethod("initialize", "NlmeEmaxParameters",
  function(.Object,
           checkBaseline = FALSE,
           checkFractional = FALSE,
           checkInhibitory = FALSE,
           checkSigmoid = FALSE,
           hasEffectsCompartment = FALSE,
           frozen = FALSE) {
    .Object@checkBaseline <- checkBaseline
    .Object@checkFractional <- checkFractional
    .Object@checkInhibitory <- checkInhibitory
    .Object@checkSigmoid <- checkSigmoid
    .Object@hasEffectsCompartment <- hasEffectsCompartment
    .Object@frozen <- frozen
    .Object
  }
)

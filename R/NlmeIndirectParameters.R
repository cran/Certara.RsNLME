LINEAR_ALPHA_TYPE <- 1
LINEAR_BETA_TYPE <- 2
LINEAR_GAMMA_TYPE <- 3

ModelLinearNames <- c("E = EAlpha ", "E = EAlpha + EBeta*C ", "E = EAlpha + EBeta*C + EGam*C^2")

#' Class represents an NLME/PML Indirect PD model parameters
#'
#' Class represents an NLME/PML Indirect PD model parameters
#'
#' @param type                   Indirect model type
#' @param hasEffectsCompartment  Is there data available for an effects compartment?
#' @param isBuildup              Is the response formation (TRUE) or
#'                                    degradation (FALSE) concentration dependent?
#' @param isExponent             Is there an exponent in the effect statement?
#' @param frozen                 Freeze standard deviation to prevent estimation
#'                                    of the PK part of the model
#' @keywords internal
setClass(
  "NlmeIndirectParameters",
  slots = c(
    type = "numeric",
    hasEffectsCompartment = "logical",
    isBuildup = "logical",
    isExponent = "logical",
    frozen = "logical"
  )
) -> NlmeIndirectParameters


setMethod("initialize", "NlmeIndirectParameters",
  function(.Object,
           type = LIMITED_STIM,
           hasEffectsCompartment = FALSE,
           isBuildup = TRUE,
           isExponent = FALSE,
           frozen = FALSE) {
    .Object@type <- type
    .Object@hasEffectsCompartment <- hasEffectsCompartment
    .Object@isBuildup <- isBuildup
    .Object@isExponent <- isExponent
    .Object@frozen <- frozen
    .Object
  }
)

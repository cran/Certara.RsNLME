#' Class represents an NLME/PML PK model parameters
#'
#' Class represents an NLME/PML PK model parameters
#'
#' @param parameterization    Taken from NlmeModelParameterization
#' @param absorption          Taken from NlmeModelAbsorption
#' @param numCompartments     Number of compartments
#' @param isTlag              Does dose have a time lag?
#' @param hasEliminationComp  Is there data available for an elimination compartment?
#' @param isFractionExcreted  Does compartment include a fraction excreted parameter?
#' @param isSaturating        Is the elimination rate equal to the absorption rate?
#' @param infusionAllowed     Is infusion allowed?
#' @param isDuration          Is duration of infusion measured (TRUE) or rate (FALSE)?
#' @param isSequential        Is model part of a PK/PD model that is being fitted sequentially?
#' @param isClosedForm        Is model closed-form algebraic (TRUE) or differential equation (FALSE)?
#'
#'
#' @keywords internal
setClass(
  "NlmePkParameters",
  slots = c(
    parameterization = "NlmeModelParameterization",
    absorption = "NlmeModelAbsorption",
    numCompartments = "numeric",
    isTlag = "logical",
    hasEliminationComp = "logical",
    isFractionExcreted = "logical",
    isSaturating = "logical",
    infusionAllowed = "logical",
    isDuration = "logical",
    isSequential = "logical",
    isPkFrozen = "logical",
    isClosedForm = "logical"
  ),
  prototype = list(
    numCompartments = 1,
    isTlag = FALSE,
    hasEliminationComp = FALSE,
    isFractionExcreted = FALSE,
    isSaturating = FALSE,
    infusionAllowed = FALSE,
    isDuration = FALSE,
    isClosedForm = TRUE,
    isSequential = FALSE,
    isPkFrozen = FALSE
  )
) -> NlmePkParameters


setMethod("initialize", "NlmePkParameters",
  function(.Object,
           parameterization = NlmeModelParameterization(),
           absorption = NlmeModelAbsorption(),
           numCompartments = 1,
           isTlag = FALSE,
           hasEliminationComp = FALSE,
           isFractionExcreted = FALSE,
           isSaturating = FALSE,
           infusionAllowed = FALSE,
           isDuration = FALSE,
           isClosedForm = TRUE,
           isSequential = FALSE,
           isPkFrozen = FALSE) {
    .Object@parameterization <- parameterization
    .Object@absorption <- absorption
    .Object@numCompartments <- numCompartments
    .Object@hasEliminationComp <- hasEliminationComp
    .Object@isFractionExcreted <- isFractionExcreted
    .Object@isSaturating <- isSaturating
    .Object@infusionAllowed <- infusionAllowed
    .Object@isDuration <- isDuration
    .Object@isSequential <- isSequential
    .Object@isPkFrozen <- isPkFrozen
    .Object@isTlag <- isTlag
    .Object@isClosedForm <- isClosedForm
    if (.Object@isSaturating) {
      .Object@isClosedForm <- FALSE
    }
    .Object
  }
)

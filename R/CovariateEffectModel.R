#' NLME covariate effects model object class
#'
#' Class represents an NLME covariate effects model
#'
#' @slot numCovariates     Number of covariate effects
#' @slot covariateList     Comma separated list of covariate effects names
#' @slot scenarioNames     Comma separated list of scenario names
#' @slot isDefault         Comma separated list of flags
#' @slot degreesOfFreedom  Comma separated list of degrees of freedom
#' @keywords NLME internal
#' @export CovariateEffectModel
#' @examples
#' CovariateEffectModel(numCovariates = 2,
#'                      covariateList = "V-Age,Cl-BW",
#'                      scenarioNames = "S,S",
#'                      isDefault = "1,1",
#'                      degreesOfFreedom = "1,1")
CovariateEffectModel <-
  setClass(
    "CovariateEffectModel",
    slots = c(
      numCovariates = "numeric",
      covariateList = "character",
      scenarioNames = "character",
      isDefault = "character",
      degreesOfFreedom = "character"
    )
  )

setMethod("initialize", "CovariateEffectModel",
          function(.Object,
                   covariateList,
                   scenarioNames,
                   isDefault,
                   degreesOfFreedom,
                   ...) {
            .Object@covariateList <- covariateList
            .Object@numCovariates <-
              length(unlist(strsplit(.Object@covariateList, ",")))
            .Object@scenarioNames <- scenarioNames
            .Object@isDefault <- isDefault
            .Object@degreesOfFreedom <- degreesOfFreedom
            .Object
          })

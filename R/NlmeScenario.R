#' Class initializer for NlmeScenario
#'
#' Creates NlmeScenario class object
#'
#' @param scenarioName   Name of the scenario
#' @param covariatesList Comma separated indices of covariate effects to use for this scenario
#'
#' @export NlmeScenario
#' @examples
#' CovariateEffectNames <- c("dVdBodyWeight",  "dCldBodyWeight")
#' #   Do not use any covariate effects
#' scenario1 <- NlmeScenario("no_covariates", "")
#' #   Use covariate effect index 1
#' scenario2 <- NlmeScenario("dVdBodyWeight", "1")
#' #   Use second covariate effect in the model
#' scenario3 <- NlmeScenario("dCldBodyWeight", "2")
#' #   Use 1st and 2nd covariate effect
#' scenario4 <- NlmeScenario("dVdBodyWeight_dCldBodyWeight", "1,2")
#' @keywords internal
NlmeScenario <-
  setClass(
    "NlmeScenario",
    slots = c(
      scenarioName = "character",
      covariatesList = "character",
      annotation = "character"
    )
  )

setMethod("initialize", "NlmeScenario",
  function(.Object, scenarioName, covariatesList, annotation = "") {
    .Object@scenarioName <- scenarioName
    .Object@covariatesList <- covariatesList
    .Object@annotation <- annotation
    .Object
  }
)

getScenarioNames <- function(scenarios) {
  names <- ""
  for (s in scenarios) {
    names <- paste(names, attr(s, "scenarioName"))
  }
  return(names)
}


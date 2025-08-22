#' Return model variable names
#'
#' Return a vector of model variable names from model object
#'
#' @param model Model object
#'
#' @examples
#' \donttest{
#' model <- pkmodel(columnMap = FALSE,
#'                  workingDir = tempdir())
#' modelVariableNames(model)
#' }
#'
#' @return Character vector of required model variable names
#' @export modelVariableNames
#'
modelVariableNames <- function(model) {
  ids <- c()
  if (model@isPopulation) {
    ids <- c("id")
  }

  if (model@isTimeBased) {
    ids <- c(ids, "time")
  }

  modelType <- model@modelType@modelType
  paramType <- model@pkModelAttrs@parameterization@paramType

  on <- observationNames(model)
  en <- observationExtraNames(model)
  cn <- covariateNames(model)

  # Add A1 Strip covariate for pkmodels with Macro Parameterization
  if (modelType %in% c(PARAM_PK, PARAM_PK_EMAX, PARAM_PK_INDIRECT, PARAM_PK_LINEAR) && paramType == Macro) {
    cn <- c(cn, "A1Strip")
  }

  dn <- doseNames(model)
  edn <- extraDoseNames(model)
  names <- c(ids, dn, on, en, cn, edn)
  if (model@hasResetInfo) {
    names <- c(names, "Reset")
  }
  return(names)
}

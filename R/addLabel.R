#' Add levels and labels to categorical or occasion covariate
#'
#' Allows users to specify the name and the associated value for each category/occasion of a
#' categorical/occasion covariate in a textual model object. Only applicable to the case where
#' the corresponding input data column of a categorical/occasion covariate is of class character.
#'
#' @param .Object     Model object
#' @param covariate   Existing covariate name
#' @param levels      Unique values of categorical or occasion covariate column specified as numeric vector
#' @param labels      Unique values specifying corresponding label names for levels of categorical or occasion covariate
#' column in data specified as character vector.
#'
#' @return Modified \code{NlmePmlModel} object
#' @examples
#' \donttest{
#' model <- pkmodel(columnMap = FALSE,
#'                  isPopulation = FALSE,
#'                  workingDir = tempdir())
#'
#' model <- suppressWarnings(addCovariate(model,
#'                       covariate = "Gender",
#'                       type = "Categorical",
#'                       effect = c("V"),
#'                       levels = c(0, 1)))
#' model@isTextual <- TRUE
#' model <- addLabel(model, "Gender", c(1, 2), c("male", "female"))
#' }
#'
#' @export
#'
addLabel <- function(.Object, covariate, levels, labels) {
  if (length(levels) != length(labels)) {
    stop("Error adding covariate labels: levels and labels have different length")
  }

  if (length(.Object@covariateList) < 1) {
    stop("Error adding covariate labels: covariate list is empty")
  }

  if (any(unlist(lapply(labels, function(x) {
    grepl("\\s", x) ||
      x == "" || x == "NA" || x == "na" || x == "."
  })))) {
    stop(
      "Error adding covariate labels: labels cannot contain spaces or missing value('NA', 'na', '.')"
    )
  }

  checkLabels(
    .Object,
    labels,
    .Object@columnMapping@mapping[[covariate]]@columnName,
    covariate
  )

  cov_list <-
    sapply(.Object@covariateList, function(x) {
      x@name == covariate
    })
  covarParam <- .Object@covariateList[which(cov_list)]

  if (length(covarParam) == 0) {
    stop(paste0(
      "The specified covariate ",
      covariate,
      " does not exist in the model."
    ))
  }

  if (length(covarParam) > 1) {
    stop("Several covariates with the same name found in the model.")
  }


  if (covarParam[[1]]@type %in% c(COVAR_CATEGORY, COVAR_OCCASION)) {
    covarParam[[1]]@covarItems <- list()
    for (i in 1:length(labels)) {
      value <- levels[i]
      name <- labels[i]
      catItem <- NlmeCovarItem(name, value)
      covarParam[[1]]@covarItems <- c(covarParam[[1]]@covarItems, catItem)
    }
  } else {
    stop(
      "Not applicable to the case where the corresponding data column of a covariate is of class numeric or integer."
    )
  }

  .Object@covariateList[which(cov_list)] <- covarParam

  .Object
}

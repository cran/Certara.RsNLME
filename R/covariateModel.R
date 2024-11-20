#' Creates set of covariate effects
#'
#' Use to create set of covariate effects to be checked during Stepwise or Shotgun
#' covariate search
#'
#' @param model Model object with covariates and covariate effects specified
#'
#' @return CovariateEffectModel class object
#'
#' @examples
#' \donttest{
#' # Define the model
#' model <- pkmodel(
#'   numCompartments = 2,
#'   data = pkData,
#'   ID = "Subject",
#'   Time = "Act_Time",
#'   A1 = "Amount",
#'   CObs = "Conc"
#' )
#'
#' # Add Gender covariate of type categorical
#' model <- addCovariate(model,
#'   covariate = "Gender",
#'   type = "Categorical",
#'   effect = c("V2", "Cl2"),
#'   levels = c(0, 1),
#'   labels = c("Female", "Male")
#' )
#'
#' # Add Bodyweight covariate of type continuous
#' model <- addCovariate(model,
#'   covariate = "BodyWeight",
#'   type = "Continuous",
#'   direction = "Backward",
#'   center = "Mean",
#'   effect = c("V", "Cl")
#' )
#' covariateModel(model)
#' }
#' @export
#' @keywords internal
covariateModel <- function(model) {
  if (missing(model)) {
    stop("model argument is required for run.")
  } else {
    stopifnot(inherits(model, "NlmePmlModel"))
  }

  # supportive array with the indexes in enable() statements
  enableFlagArray <- c()

  covariateList <- model@covariateList
  if (length(covariateList) == 0) {
    stop("No covariates found for the current model")
  }

  StParms <- model@structuralParams
  splittedFixefsStrings <- .get_fixefStrings(model)
  maxEnable <-
    max(sapply(splittedFixefsStrings, function(x) {
      as.numeric(x[2])
    }), na.rm = TRUE)
  if (maxEnable < 0) {
    stop(
      "To create the covariate model the user should add enable(N) flag for ",
      "covariate-related fixed effects to check."
    )
  }

  covFixefs <- lapply(rep(1:(maxEnable + 1)), function(x) {
    list()
  })
  # main vector with the info about all effects enabled
  for (fixefString in splittedFixefsStrings) {
    if (length(fixefString) < 4) {
      warning(
        "Cannot parse the following modelInfo string:\n",
        paste(fixefString)
      )
      next()
    }

    enableFlag <- as.integer(fixefString[2])
    if (is.na(enableFlag) || enableFlag < 0) {
      next()
    }

    covFixefName <- fixefString[1]

    # trying to figure out covariate and StParm
    for (StParm in StParms) {
      if (!grepl(paste0("^d", StParm@name), covFixefName)) {
        next()
      }

      for (Cov in covariateList) {
        pattern <- paste0("^d", StParm@name, "d", Cov@name, "\\d*")
        if (Cov@type == 2) {
          # categorical covariate: should be a number after
          pattern <- paste0(pattern, "\\d+")
        }

        if (!grepl(pattern, covFixefName)) {
          next()
        }

        covFixefName <- paste0(StParm@name, "-", Cov@name)
      }
    }

    newEnable <- length(covFixefs[[enableFlag + 1]]) == 0
    if (newEnable) {
      covFixefs[[enableFlag + 1]]$name <- covFixefName
      covFixefs[[enableFlag + 1]]$df <- 1
      covFixefs[[enableFlag + 1]]$enable <- enableFlag
      enableFlagArray <- c(enableFlagArray, enableFlag)
    } else {
      covFixefNameRepeated <-
        grepl(paste0("(^|&)", covFixefName, "($|&)"), covFixefs[[enableFlag + 1]]$name)
      if (length(covFixefNameRepeated) == 0 ||
          !covFixefNameRepeated) {
        covFixefs[[enableFlag + 1]]$name <-
          paste(covFixefs[[enableFlag + 1]]$name, covFixefName, sep = "&")
      }

      covFixefs[[enableFlag + 1]]$df <-
        covFixefs[[enableFlag + 1]]$df + 1
    }
  }

  covFixefs <- covFixefs[lengths(covFixefs) > 0]
  covFixefs <- covFixefs[order(enableFlagArray)]
  scenarios <- rep("S", length(covFixefs))
  isDefaultList <- rep("1", length(covFixefs))
  cvEffects <- c()
  df <- c()
  for (covFixef in covFixefs) {
    cvEffects <- c(cvEffects, covFixef$name)
    df <- c(df, covFixef$df)
  }

  cm <-
    CovariateEffectModel(
      paste(cvEffects, collapse = ","),
      paste(scenarios, collapse = ","),
      paste(isDefaultList, collapse = ","),
      paste(df, collapse = ",")
    )

  return(cm)
}

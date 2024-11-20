checkPKParams <-
  function(hasEliminationComp,
           isClosedForm,
           numCompartments,
           parameterization,
           isSaturating,
           isFractionExcreted,
           absorption) {
    if (hasEliminationComp && isClosedForm) {
      stop("Must specify isClosedForm = FALSE if hasEliminationComp = TRUE")
    }

    if (numCompartments > 3) {
      stop("numCompartments cannot be more than 3")
    }
    if (numCompartments %% 1 != 0) {
      stop("numCompartments cannot be a fraction")
    }
    if (numCompartments < 1) {
      stop("numCompartments cannot be less than 1")
    }

    if (parameterization != Clearance && isSaturating) {
      stop("Must use parameterization = 'Clearance' if isSaturating = TRUE")
    }

    if (parameterization == Macro || parameterization == Macro1) {
      assertthat::assert_that(hasEliminationComp == FALSE,
                              msg = "Must specify hasEliminationComp = FALSE if using 'Macro' or 'Macro1' parameterization")
      assertthat::assert_that(isFractionExcreted == FALSE,
                              msg = "Must specify isFractionExcreted = FALSE if using 'Macro' or 'Macro1' parameterization")
      assertthat::assert_that(isClosedForm == TRUE,
                              msg = "Must specify isClosedForm = TRUE if using 'Macro' or 'Macro1' parameterization")
    }

    if (absorption == Gamma ||
        absorption == Weibull || absorption == InverseGaussian) {
      assertthat::assert_that(parameterization == Micro ||
                                parameterization == Clearance,
                              msg = "Must use parametrization Micro or Clearance with absorption = 'Gamma', 'Weibull' or 'InverseGaussian'")
      assertthat::assert_that(isClosedForm == FALSE,
                              msg = "Cannot use closed form models with 'Gamma', 'Weibull' or 'InverseGaussian'")
    }
  }


#' Create a textual model object
#'
#' Use to create an empty model object and optionally supply location of .mdl file to initialize model with PML statements.
#'
#' @param  modelName    Model name to create subdirectory for model output in current working directory.
#' @param  workingDir   Working directory to run the model. Current working directory will be used if \code{workingDir} not specified.
#' @param  data         Input dataset
#' @param  mdl          File path specifying location of test.mdl file
#'
#' @return \code{NlmePmlModel} object
#' @examples
#' \donttest{
#' model <- textualmodel(data = pkData)
#' }
#'
#' @export
textualmodel <- function(modelName = "",
                         workingDir = "",
                         data,
                         mdl = NULL) {
  .Object <- NlmePmlModel(modelType = Blank,
                          modelInfo = NlmePmlModelInfo(modelName, workingDir))

  initColMapping(.Object) <- data

  if (!is.null(mdl)) {
    if (file.exists(mdl)) {
      statements <- readLines(mdl, warn = FALSE)
      .Object@statements <- as.list(statements)
      .Object@isTextual <- TRUE
      .Object <- parsePMLColMap(.Object)
      .Object@columnMapping@mapping <-
        .map_exactTerms(.Object@columnMapping@mapping, data)
    } else {
      stop(".mdl file not found")
    }
  }

  return(.Object)
}

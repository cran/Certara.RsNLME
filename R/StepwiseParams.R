#' Class initializer for NLME StepwiseParams
#'
#' Class represents an NLME Stepwise search parameters
#'
#' @param addPValue     Numeric. Threshold for adding a covariate effect
#' @param removePValue  Numeric. Threshold for removing a covariate effect
#' @param method        -2LL|AIC|BIC; could be abbreviated.
#' @export StepwiseParams
#' @examples
#' StepwiseParams(0.001, 0.001, "BIC")
#' @keywords internal
StepwiseParams <-
  setClass(
    "StepwiseParams",
    slots = c(
      addPValue = "numeric",
      removePValue = "numeric",
      method = "character"
    )
  )

setMethod("initialize", "StepwiseParams",
  function(.Object,
           addPValue = 0.01,
           removePValue = 0.001,
           method = "-2LL") {
    .Object@addPValue <- addPValue
    .Object@removePValue <- removePValue
    applicablemethods <- c("-2LL", "AIC", "BIC")
    tryCatch({
      .Object@method <- match.arg(toupper(method), applicablemethods)
    },
    error = function(cond) {
      stop("Current method:\n", method, "\nis not applicable for stewise search.",
           " Applicable methods are:\n", paste(applicablemethods, collapse = " "),
           call. = FALSE)
    })
    .Object
  }
)

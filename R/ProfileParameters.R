#' Class initializer for ProfileParameters
#'
#' Class represents an NLME profile perturbation variable
#'
#' @slot howToPertubate How to apply profile variables. Options are \code{USE_DELTA} or \code{USE_PERCENTAGE}
#' @slot profileVars List of profile variables
#' @export ProfileParameters
#' @keywords internal
#' @examples
#' profile1 <- ProfileVar("tvV", 9.548, "-2,-1,0,1,2")
#' profile2 <- ProfileVar("tvCl", 3.219, "-1,0,1")
#' profiles <- ProfileParameters("USE_DELTA", c(profile1, profile2))
ProfileParameters <-
  setClass("ProfileParameters",
           slots = c(howToPertubate = "character",
                     profileVars = "list"))

setMethod("initialize", "ProfileParameters",
  function(.Object, howToPertubate,
           profileVars) {
    .Object@howToPertubate <- howToPertubate
    .Object@profileVars <- profileVars
    .Object
  }
)

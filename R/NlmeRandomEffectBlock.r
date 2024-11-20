

Diagonal <- 1
Block <- 2

#' Random effect block definition
#'
#' Random effect block definition
#'
#' @slot  type          Diagonal or Block
#' @slot  effectNames   Character or character vector specifying names of random effects
#' @slot  frozen        Logical; Are random effects frozen?
#'
#' @examples
#' \donttest{
#' NlmeRandomEffectBlock(Diagonal, list("nCl", "nV"), FALSE)
#' }
#'
#' @name  NlmeRandomEffectBlock
#' @rdname NlmeRandomEffectBlock
#' @keywords internal

NlmeRandomEffectBlock <- setClass(
  "NlmeRandomEffectBlock",
  slots = c(
    type = "numeric",
    effectNames = "list",
    frozen = "logical"
  )
)


setMethod("initialize", "NlmeRandomEffectBlock",
  function(.Object,
           type,
           effectNames,
           frozen = FALSE) {
    .Object@type <- type
    .Object@effectNames <- effectNames
    .Object@frozen <- frozen
    .Object
  }
)


#' Random effect values matrix
#'
#' Random effect values matrix
#'
#' @param numEffects    Number of random effects
#' @param effectNames   Names of random effects
#' @param values        Matrix of random effect values
#'
#' @examples
#' \donttest{
#' # initialize with default effectValues ()
#' NlmeRandomEffectValues(list("nCl", "nV"))
#' # set diagonals
#' NlmeRandomEffectValues(list("Cl", "V"), effectValues = c(0.1, 0.2))
#' # values matrix overrides effectValues
#' NlmeRandomEffectValues(list("Cl", "V"),
#'   values = matrix(c(1,0.01,0.01,1), ncol = 2,
#'     dimnames = list(c("Cl", "V"), c("Cl", "V"))))
#' }
#' @keywords internal
NlmeRandomEffectValues <- setClass(
  "NlmeRandomEffectValues",
  slots = c(
    effectNames = "list",
    effectValues = "list",
    values = "matrix"
  )
)


setMethod("initialize", "NlmeRandomEffectValues",
  function(.Object,
           effectNames,
           effectValues = list(),
           values = NULL) {
    numEffects <- length(effectNames)
    .Object@effectNames <- effectNames
    if (is.null(values)) {
      values <- matrix(
        data = 1,
        nrow = numEffects,
        ncol = numEffects,
        dimnames = list(effectNames, effectNames)
      )
      for (r in 1:numEffects) {
        for (c in 1:numEffects) {
          if (c != r) {
            values[r, c] <- 0
          } else {
            if (length(effectValues) != 0) {
              values[r, c] <- effectValues[[c]]
            }
          }
        }
      }
    }

    .Object@values <- values

    .Object
  }
)

#' Return random effect names in model
#'
#' Use to return character vector of random effect names (if available) in model object
#'
#' @param model Model object
#'
#' @examples
#' model <- pkmodel(columnMap = FALSE)
#' getRandomEffectNames(model)
#'
#' @export
#' @return Characters vector of random effect names
getRandomEffectNames <- function(model) {
  names <- c()

  sps <- model@structuralParams

  for (sp in sps) {
    if (sp@hasRandomEffect == TRUE) {
      names <- c(names, sp@randomEffName)
    }
  }

  names
}






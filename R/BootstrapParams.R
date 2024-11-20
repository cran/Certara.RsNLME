#' Represents parameters for a bootstrap run
#'
#' Class initializer for BootstrapParams that represents parameters for a bootstrap run.
#'
#' @slot numReplicates Number of bootstrap replicates to run
#' @slot initialEstimates Get model final estimates to run bootstrap (T|F)
#' @slot numRetries Number of times to retry a failed replicated
#' @slot randomNumSeed Seed for random number generator
#' @slot confidenceLevel Confidence level
#' @slot stratifyColumns What columns to stratify on(comma separated)
#'
#' @export BootstrapParams
#' @keywords internal
#' @examples
#' # same object as BootstrapParams()
#' boot <- BootstrapParams(
#'   numReplicates = 100,
#'   initialEstimates = FALSE,
#'   numRetries = 2,
#'   randomNumSeed = 1234,
#'   confidenceLevel = 95
#' )
#'
BootstrapParams <-
  setClass(
    "BootstrapParams",
    slots = c(
      numReplicates = "numeric",
      initialEstimates = "logical",
      numRetries = "numeric",
      randomNumSeed = "numeric",
      confidenceLevel = "numeric",
      stratifyColumns = "character"
    )
  )

setMethod("initialize", "BootstrapParams",
          function(.Object,
                   numReplicates = 100,
                   initialEstimates = FALSE,
                   numRetries = 2,
                   randomNumSeed = 1234,
                   confidenceLevel = 95,
                   stratifyColumns = "",
                   ...) {
            .Object@numReplicates <- numReplicates
            .Object@initialEstimates <- initialEstimates
            .Object@numRetries <- numRetries
            .Object@randomNumSeed <- randomNumSeed
            .Object@confidenceLevel <- confidenceLevel
            .Object@stratifyColumns <- stratifyColumns
            .Object
          })

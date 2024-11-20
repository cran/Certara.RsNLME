#' Used for simulation parameters generation for pyDarwin run
#' @noRd
#' @keywords internal
simParamsBlock <- setClass(
  "simParamsBlock",
  slots = c(
    numReplicates = "numeric",
    seed = "numeric",
    sort = "logical",
    ODE = "character",
    rtolODE = "numeric",
    atolODE = "numeric",
    maxStepsODE = "numeric"
  )
)

setMethod("initialize", "simParamsBlock",
          function(.Object,
                   numReplicates = 100L,
                   seed = 1234L,
                   sort = FALSE,
                   ODE = "MatrixExponent",
                   rtolODE = 1e-6,
                   atolODE = 1e-6,
                   maxStepsODE = 50000L) {
            .Object@numReplicates <- numReplicates
            .Object@seed <- seed
            .Object@sort <- sort
            .Object@ODE <- ODE
            .Object@rtolODE <- rtolODE
            .Object@atolODE <- atolODE
            .Object@maxStepsODE <- maxStepsODE

            .Object
          })

setGeneric(
  name = "output",
  def = function(.Object, ...) standardGeneric("output")
)

setMethod(
  "output",
  "simParamsBlock",
  definition = function(.Object) {
    simString <- paste(
      "-predn",
      .Object@numReplicates,
      "-pcseed",
      .Object@seed,
      "/o",
      .assignODEMethod(.Object@ODE),
      "-rtol",
      .Object@rtolODE,
      "-atol",
      .Object@atolODE,
      "-nmxstep",
      .Object@maxStepsODE
    )

    if (.Object@sort) {
      simString <- paste(simString, "-sort")
    }

    simString
  }
)

#' @export
print.simParamsBlock <- function(x, ...) {
  cat("\n Simulation Parameters \n ------------------------------------------- \n")
  cat(paste("Number of replicates        : ", x@numReplicates), fill = TRUE)
  cat(paste("Random number generator seed: ", x@seed), fill = TRUE)
  cat(paste("Sort input data             : ", x@sort), fill = TRUE)
  cat(paste("ODE solver                  : ", x@ODE), fill = TRUE)
  cat(paste("ODE relative tolerance      : ", x@rtolODE), fill = TRUE)
  cat(paste("ODE absolute tolerance      : ", x@atolODE), fill = TRUE)
  cat(paste("ODE max steps               : ", x@maxStepsODE), fill = TRUE)
}

setMethod(
  f = "show",
  signature = "simParamsBlock",
  definition = function(object) {
    print(object)
  }
)

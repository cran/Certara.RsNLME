#' Wrapper around NlmeTableDef/NlmeSimTableDef-classes initializers.
#'
#' @inheritParams NlmeTableDef-class
#' @param forSimulation logical. Defining whether the table is for simulation
#' purposes or for postprocessing after fit. Default is \code{FALSE}.
#'
#' @return
#' \code{NlmeTableDef} object if \code{forSimulation} is \code{FALSE},
#' \code{NlmeSimTableDef} object otherwise.
#'
#' @examples
#' Table1 <- tableParams(
#'   name = "Table1.csv",
#'   timesList = seq(0, 24, 2),
#'   whenObs = c("CObs"),
#'   variablesList = "C",
#'   IRES = TRUE,
#'   IWRES = TRUE,
#'   Weight = TRUE)
#'
#' SimTable1 <- tableParams(
#'   name = "SimTable1.csv",
#'   variablesList = "CL, V",
#'   keepSource = TRUE,
#'   forSimulation = TRUE)
#'
#' @export
tableParams <- function(name = "",
                        timesList = numeric(0),
                        covrSet = "",
                        whenDose = "",
                        whenObs = "",
                        variablesList = "",
                        keepSource = FALSE,
                        timeAfterDose = FALSE,
                        IRES = FALSE,
                        Weight = FALSE,
                        IWRES = FALSE,
                        mode = "all",
                        forSimulation = FALSE) {
  if (hasArg(IRES) || hasArg(Weight) || hasArg(IWRES) || hasArg(mode)) {
    if (forSimulation) {
      message("Arguments 'IRES', 'Weight', 'IWRES', 'mode' ",
              "are not applicable for simulation tables and will be ignored.")
    }
  }

  if (forSimulation) {
    NlmeSimTableDef(
      name = name,
      timesList = timesList,
      covrSet = covrSet,
      whenDose = whenDose,
      whenObs = whenObs,
      variablesList = variablesList,
      keepSource = keepSource,
      timeAfterDose = timeAfterDose
    )
  } else {
    NlmeTableDef(
      name = name,
      timesList = timesList,
      covrSet = covrSet,
      whenDose = whenDose,
      whenObs = whenObs,
      variablesList = variablesList,
      keepSource = keepSource,
      timeAfterDose = timeAfterDose,
      IRES = IRES,
      Weight = Weight,
      IWRES = IWRES,
      mode = mode
    )
  }
}

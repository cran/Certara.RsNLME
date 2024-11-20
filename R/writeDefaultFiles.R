#' Writes out data/column mapping and engine parameter files for the engine
#'
#' Writes out data/column mapping and engine parameter files for the engine
#'
#' @param model     Model object
#' @param dataset   Dataset to run simulation/fitting on
#' @param simParams Simulation Parameters (simulation tables are written to coldef)
#' @param Tables    NlmeTableDef class object(s)
#' @param sortColumns SortColumns class object used for proper id mapping during
#' individual modeling
#'
#' @export
#' @keywords internal
#' @return \code{NULL}
writeDefaultFiles <-
  function(model,
           dataset,
           simParams,
           Tables,
           sortColumns = NULL) {
    mf <- dataset@modelFile
    cf <- dataset@colDefFile
    df <- dataset@dataFile
    writeInputData(model, df)
    writeColumnMapping(
      model = model,
      filename = cf,
      workingDir = model@modelInfo@workingDir,
      sortColumns = sortColumns
    )
    writeModelStatements(model, mf)

    if (!missing(Tables)) {
      addTablesToColumnMapping(model, Tables, cf, forSim = FALSE)
    }

    if (!missing(simParams)) {
      addTablesToColumnMapping(model, simParams@simulationTables, cf, forSim = TRUE)
    }

    if (length(model@doseMapping@mapping) != 0) {
      writeDoseMapping(model, dataset)
    }
    if (length(model@paramsMapping@mapping) != 0) {
      writeParamsMapping(model, dataset)
    }
  }

#' Writes the model statements to a file
#'
#' Writes the model statements to a file
#'
#' @param model      A PK/PD model class instance
#' @param filename   Name of the file
#' @param workingDir Directory where the file should be written.
#' If it is not given, the file is written to the model working directory
#'
#' @examples
#' \donttest{
#' writeModelStatements(model, filename)
#' }
#' @keywords internal
#' @noRd
writeModelStatements <- function(model, filename, workingDir) {
  if (missing(workingDir)) {
    workingDir <- model@modelInfo@workingDir
  }

  workingDir <- .prepare_wd(workingDir)

  fullPath <- file.path(workingDir, filename)

  modelStatements <- gsub("\r\n", "\n", unlist(model@statements))
  cat(paste(unlist(modelStatements), collapse = "\n"), file = fullPath)
}

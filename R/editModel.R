#' Directly edit PML text in model object
#'
#' Allows user to edit PML text in model object using internal text editor and return a new textual model
#' containing the edited PML statements.
#'
#' @param .Object     Model object
#'
#' @return Modified \code{NlmePmlModel} object
#' @examples
#' \donttest{
#' model <- pkmodel(columnMap = FALSE,
#'                  workingDir = tempdir())
#'
#' if (FALSE) {
#'   # will open an additional window with the model text:
#'   newModel <- editModel(model)
#' }
#' }
#'
#' @export
#'
editModel <- function(.Object) {
  stopifnot(inherits(.Object, "NlmePmlModel"))
  wd <- file.path(tempdir(TRUE), .Object@modelInfo@modelName)
  if (!dir.exists(wd)) {
    wd <- .prepare_wd(wd)
  }

  mf <- attr(.Object@dataset, "modelFile")
  mf_path <- file.path(wd, mf)
  initialmdl <- unlist(.Object@statements)
  cat(paste(initialmdl, collapse = "\n"), file = mf_path)

  utils::file.edit(name = mf_path)
  editedmdl <- readLines(mf_path, warn = FALSE)

  if (!identical(initialmdl, editedmdl)) {
    ModelInfoPath <- file.path(wd, "ModelInfo.txt")
    if (file.exists(ModelInfoPath)) {
      file.remove(ModelInfoPath)
    }
    .Object@isTextual <- TRUE
    .Object@statements <- as.list(editedmdl)
    .Object <- parsePMLColMap(.Object)
  }

  return(.Object)
}

editColMaps <- function(.Object) {
  stopifnot(inherits(.Object, "NlmePmlModel"))

  dataset <- .Object@dataset
  wd <- dataset@workingDir
  if (!dir.exists(wd)) {
    wd <- .prepare_wd(wd)
  }

  cf <- attr(dataset, "colDefFile")
  cf_path <- file.path(wd, cf)
  writeColumnMapping(model = .Object, filename = cf)

  initialcf <- readLines(cf_path, warn = FALSE)

  utils::file.edit(name = cf_path)

  editedcf <- as.list(readLines(cf_path, warn = FALSE))

  .Object@colStatements <- editedcf

  return(.Object)
}

#' Save model object to .rda file
#'
#' Saves the model, engine, and host objects to a single \code{model.rda} file
#' in model's working directory. If no working directory exists it will be
#' created by default.
#'
#' @param model    NlmePmlModel object
#' @param engine   Optional engine parameters to save with model
#' @param host     Optional host to save with model
#'
#' @examples
#' \donttest{
#' model <- emaxmodel(
#'   checkBaseline = TRUE,
#'   checkFractional = TRUE,
#'   checkInhibitory = TRUE,
#'   data = pkpdData,
#'   ID = "ID",
#'   C = "CObs",
#'   EObs = "EObs",
#'   workingDir = tempdir()
#' )
#'
#' saveModel(model)
#' }
#'
#' @export
#' @keywords internal
#' @return \code{NULL}
saveModel <- function(model,
                      engine = NULL,
                      host = NULL) {
  # deparse(substitute(modelpk))
  stopifnot(inherits(model, "NlmePmlModel"))

  if (!is.null(engine)) {
    stopifnot(inherits(engine, "NlmeEngineExtraParams"))
  }

  if (!is.null(host)) {
    stopifnot(inherits(host, "NlmeParallelHost"))
  }

  wd <- model@modelInfo@workingDir
  m_name <- model@modelInfo@modelName

  wd <- .prepare_wd(wd)

  # Uncomment below if we want to append model name to model.rda file
  # m_file <- paste0("model_", m_name, ".rda")
  m_file <- "model.rda"

  m_path <- file.path(wd, m_file)

  new_m_name <- paste0("model_", m_name)
  assign(new_m_name, model)

  if (is.null(engine) && is.null(host)) {
    save(list = new_m_name, file = m_path)
  }

  if (!is.null(engine) && is.null(host)) {
    new_e_name <- paste0("engine_", m_name)
    assign(new_e_name, engine)
    save(list = c(new_m_name, new_e_name), file = m_path)
  }

  if (is.null(engine) && !is.null(host)) {
    new_h_name <- paste0("host_", m_name)
    assign(new_h_name, host)
    save(list = c(new_m_name, new_h_name), file = m_path)
  }

  if (!is.null(engine) && !is.null(host)) {
    new_e_name <- paste0("engine_", m_name)
    assign(new_e_name, engine)
    new_h_name <- paste0("host", m_name)
    assign(new_h_name, host)
    save(
      list = c(new_m_name, new_e_name, new_h_name),
      file = m_path
    )
  }

  invisible(NULL)
}


#' Load model.rda file
#'
#' Loads a previously saved model from disk.
#'
#' @param directory    Directory where the model was saved
#'
#' @details
#' Note, the names of model, engine, and host objects will
#' be appended with name of model directory when reloading \code{model.rda} file
#' to global environment.
#'
#' @examples
#' \donttest{
#' TempDir <- tempdir()
#' model <- emaxmodel(
#'   checkBaseline = TRUE,
#'   checkFractional = TRUE,
#'   checkInhibitory = TRUE,
#'   data = pkpdData,
#'   ID = "ID",
#'   C = "CObs",
#'   EObs = "EObs",
#'   modelName = "model",
#'   workingDir = TempDir
#' )
#'
#' saveModel(model)
#'
#' loadModel(TempDir)
#' }
#'
#' @export
#' @return No value is returned. Model object is loaded in global environment.
#' @keywords internal
loadModel <- function(directory) {
  m_name <- "model.rda"
  m_path <- file.path(directory, m_name)

  stopifnot(file.exists(m_path))

  load(m_path, envir = .GlobalEnv)
}

#' Copy model object to iterate over base model
#'
#' Copies previously executed model into a new object and optionally accept all estimates returned from model execution.
#' A new working directory is created and all files from base model are copied into it.
#'
#' @param model             Model object to be copied
#' @param acceptAllEffects  Set to \code{TRUE} to accept all effects, update PML statements, and test.mdl file from original model run
#' @param modelName 	      New model name for subdirectory created for model output. Subdirectory is created in current working directory.
#' @param workingDir        Working directory to run the model. Current working directory will be used if workingDir not specified.
#'
#' @return Modified \code{NlmePmlModel} object
#' @examples
#' \donttest{
#' # Create initial model
#' model <- pkmodel(
#'   parameterization = "Clearance",
#'   absorption = "Intravenous",
#'   numCompartments = 2,
#'   data = pkData,
#'   ID = "Subject",
#'   A1 = "Amount",
#'   CObs = "Conc",
#'   Time = "Act_Time",
#'   modelName = "pk_model"
#' )
#'
#' # Fit Model
#' job <- fitmodel(model)
#'
#' # Copy model and accept all effects from the original model run
#' vpcModel <- copyModel(model, acceptAllEffects = TRUE, modelName = "vpc_model")
#' }
#'
#' @export
copyModel <- function(model,
                      acceptAllEffects = FALSE,
                      modelName = "",
                      workingDir = "") {
  i <- integer(0)
  if (missing(workingDir)) {
    if (modelName != "" &&
        modelName != basename(model@modelInfo@workingDir)) {
      workingDir <-
        file.path(dirname(model@modelInfo@workingDir), modelName)
    } else {
      workingDir <- paste0(model@modelInfo@workingDir, "_copy")
      if (dir.exists(workingDir)) {
        i <- 1
        while (!dir.exists(workingDir)) {
          workingDir <- paste0(workingDir, i)
          i <- i + 1
        }
      }
    }
  }

  if (!dir.exists(workingDir)) {
    workingDir <- .prepare_wd(workingDir)
  }

  if (missing(modelName)) {
    modelName <- paste0(model@modelInfo@modelName, "_copy", i)
    if (dir.exists(workingDir)) {
      i <- 1
      while (!dir.exists(workingDir)) {
        workingDir <- paste0(workingDir, i)
      }
    }
  }

  if (acceptAllEffects) {
    newModel <- acceptAllEffects(model)
  } else {
    newModel <- model
  }

  newModel@modelInfo <- NlmePmlModelInfo(modelName, workingDir)
  newModel
}

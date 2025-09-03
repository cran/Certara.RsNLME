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
#' \dontrun{
#' model <- pkmodel(
#'   parameterization = "Clearance",
#'   numCompartments = 2,
#'   data = pkData,
#'   ID = "Subject",
#'   Time = "Act_Time",
#'   A1 = "Amount",
#'   CObs = "Conc",
#'   workingDir = tempdir()
#'   )
#'  host <- hostParams(sharedDirectory = tempdir(),
#'                     parallelMethod = "None",
#'                     hostName = "local",
#'                     numCores = 1)
#' job <- fitmodel(model,
#'                 numIterations = 3,
#'                 hostPlatform = host)
#'
#' finalModelVPC <- copyModel(model,
#'                            acceptAllEffects = TRUE,
#'                            modelName = "model_VPC",
#'                            workingDir = tempdir())
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

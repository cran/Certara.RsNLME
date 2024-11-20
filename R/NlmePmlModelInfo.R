#'
#' @keywords internal
NlmePmlModelInfo <-
  setClass("NlmePmlModelInfo",
           slots = c(
             modelName = "character",
             workingDir = "character"
           )
  )


setMethod("initialize", "NlmePmlModelInfo",
  function(.Object,
           modelName = "",
           workingDir = "") {
    if (modelName == "") {
      modelName <- paste0("Model", format(Sys.time(), "_%y_%m_%d_%H_%M"))
    }

    if (workingDir == "") {
      workingDir <- file.path(normalizePath(".", winslash = "/", mustWork = FALSE), modelName)
    }

    .Object@workingDir <- workingDir
    .Object@modelName <- modelName
    .Object
  }
)

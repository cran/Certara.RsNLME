set_Xchmod <- function() {
  if (.Platform$OS.type != "windows") {
    Sys.chmod(
      list.files(
        path = Sys.getenv("INSTALLDIR"),
        pattern = "*.sh|TDL5$",
        full.names = TRUE,
        recursive = TRUE
      ),
      mode = "777"
    )
  } else {
    TRUE
  }
}

#' Parse the model and get the list of terms
#'
#' Calls TDL5 to parse the model and get the list of terms
#'
#' @param model Model object
#' @param ForceRun Set to \code{TRUE} to force run
#'
#' @examples
#' \dontrun{
#'   model <- pkmodel(columnMap = FALSE,
#'                    workingDir = tempdir())
#'   createModelInfo(model)
#' }
#'
#' @export
#' @return List of model information
createModelInfo <- function(model, ForceRun = FALSE) {
  wd <- tempdir(TRUE)
  modelName <- model@modelInfo@modelName
  mf <- model@dataset@modelFile
  ModelFilePath <- file.path(wd, modelName, mf)
  ModelInfoPath <- file.path(wd, modelName, "ModelInfo.txt")
  if (!file.exists(ModelInfoPath) || ForceRun) {
    tempDir <- file.path(wd, modelName)
    if (!dir.exists(tempDir)) {
      dir.create(tempDir)
      if (!dir.exists(tempDir)) {
        stop(paste(
          "Cannot create the directory to create ModelInfo.txt:\n",
          tempDir
        ))
      }
    }

    cat(paste(unlist(model@statements), collapse = "\n"), file = ModelFilePath)

    installdir <- path.expand(Sys.getenv("INSTALLDIR"))
    if (!Certara.NLME8::checkInstallDir(installdir)) {
      stop("Cannot create the modelinfo file, NLME executables not found")
    }

    set_Xchmod()

    suppressWarnings(
      TDL5_run <-
        system2(
          file.path(installdir, Sys.getenv("PML_BIN_DIR"), "TDL5"),
        paste("-i", shQuote(ModelFilePath, type = "cmd"), shQuote(tempDir, type = "cmd")),
          stdout = TRUE
        )
    )

    if (!file.exists(ModelInfoPath)) {
      stop(
        paste0(
          "Cannot create diagnostic file:\n",
          ModelInfoPath,
          "\nTDL5 output:\n",
          paste0(TDL5_run, collapse = "\n")
        )
      )
    } else {
      # note that in ModelBuilder ModelInfo file could exist from the good model
      # and we will use it
      if (length(attr(TDL5_run, "status")) > 0) {
        message(
          paste0(
            "TDL5 output:\n",
            paste0(TDL5_run, collapse = "\n")
          )
        )
      }
    }
  }

  ModelInfo <- readLines(ModelInfoPath, warn = FALSE)
  ModelInfo
}

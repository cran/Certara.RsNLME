#' @include StepwiseParams.R
#'
#' @slot stepParam Class represents an NLME Stepwise search parameters
StepwiseNlmeJob <-
  setClass(
    "StepwiseNlmeJob",
    slots =
      c(stepParam = "StepwiseParams"),
    contains = "SimpleNlmeJob"
  )

setMethod("initialize", "StepwiseNlmeJob",
          function(.Object, ..., stepParam = "") {
  message("Stepwise Job\n")
  callNextMethod(.Object, ..., stepParam = stepParam)
})

setMethod("generateScript", "StepwiseNlmeJob",
          definition = function(.Object) {
            modelFile <- .Object@argsList[[5]]
            extraArgsFile <- .Object@argsList[[6]]
            filesToCopy <- .Object@argsList[[7]]
            numCovariates <- .Object@argsList[[8]]
            covariateList <- .Object@argsList[[9]]
            stepwiseMethod <- .Object@argsList[[10]]
            addPValue <- .Object@argsList[[11]]
            removePValue <- .Object@argsList[[12]]
            numCores <- .Object@argsList[[13]]
            workflow <- .Object@argsList[[14]]

            script <- .paste_hashToScript("", .Object@host@hostType )
            script <-
              .paste_instsharedDirsToScript(script,
                                            .Object@host@hostType,
                                            .Object@host@installationDirectory,
                                            .Object@host@sharedDirectory)

            cmd_args <- paste(.Object@remoteDir,
                              modelFile,
                              extraArgsFile,
                              shQuote(filesToCopy, type = "cmd"),
                              numCovariates,
                              shQuote(covariateList, type = "cmd"),
                              shQuote(stepwiseMethod, type = "cmd"),
                              addPValue,
                              removePValue,
                              numCores,
                              workflow)

            if (.Object@host@hostType == "windows") {
              scriptName <- file.path(.Object@remoteDir, "cmd.ps1")

              cmd <- paste(
                "powershell -noninteractive -executionpolicy remotesigned -File $INSTALLDIR/phx_stepwise_covarsrch.ps1",
                .Object@host@parallelMethod@method,
                "$INSTALLDIR",
                "$shared_directory",
                cmd_args)

              script <- paste0(script, "\n", cmd)
              script <- gsub("/", "\\", script, fixed = TRUE)
            } else {
              scriptName <- file.path(.Object@localDir, "cmd.sh")
              trap <- .paste_trap(.Object)
              script <- .paste_shRlocToScript(script,
                                              .Object@host@rLocation,
                                              .Object@host@scriptPath)

              cmd <-
                  paste(shQuote("${INSTALLDIR}/phx_stepwise_covarsrch.sh", type = "cmd"),
                        .Object@host@parallelMethod@method,
                        shQuote("$INSTALLDIR", type = "cmd"),
                        "${shared_directory}",
                        cmd_args)

              script <-
                paste0(trap, "\n", script, "\n", cmd)
            }

            con <- file(scriptName, open = "wb")
            on.exit(close(con))
            cat(script, file = con, sep = "\n")
            return(scriptName)
          }
)

setMethod("executeJob", "StepwiseNlmeJob",
          definition = function(.Object) {
            runInBackground <- .Object@runInBackground

            removeCommandFile(.Object@localDir)
            if (.Object@host@isLocal == TRUE) {
              if (runInBackground == TRUE) {
                message("Run will be performed in parallel R process; the information about execution is limited.",
                        "\nPlease check the model directory (", .Object@localDir, ") for results.")
                parallel::mcparallel(Certara.NLME8::performStepwiseCovarSearch(
                  as.character(.Object@argsList)))
                print(.Object)
              } else {
                Certara.NLME8::performStepwiseCovarSearch(
                  as.character(.Object@argsList), reportProgress = TRUE)
                .remove_mappedDrive()
              }
            } else {
              .execute_remote(.Object)
            }
          }
)

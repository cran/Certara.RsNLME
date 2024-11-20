ShotgunNlmeJob <- setClass("ShotgunNlmeJob",
                           representation(),
                           contains = "SimpleNlmeJob")

setMethod("initialize", "ShotgunNlmeJob", function(.Object, ...) {
  message("Shotgun Job")
  callNextMethod(.Object, ...)
})


setMethod("generateScript", "ShotgunNlmeJob",
          definition = function(.Object) {
            controlFile <- .Object@argsFile

            script <- .paste_hashToScript("", .Object@host@hostType )
            script <-
              .paste_instsharedDirsToScript(script,
                                            .Object@host@hostType,
                                            .Object@host@installationDirectory,
                                            .Object@host@sharedDirectory)

            cmd_args <- paste(.Object@remoteDir,
                              controlFile,
                              .Object@host@numCores,
                              .Object@workflow)

            if (.Object@host@hostType == "windows") {
              scriptName <- file.path(.Object@remoteDir, "cmd.ps1")

              cmd <- paste("powershell -noninteractive -executionpolicy remotesigned -File $INSTALLDIR/phx_shotgun_covarsrch.ps1",
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

              cmd <- paste(shQuote("${INSTALLDIR}/phx_shotgun_covarsrch.sh", type = "cmd"),
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

setMethod("executeJob", "ShotgunNlmeJob",
          definition = function(.Object) {
            runInBackground <- .Object@runInBackground

            removeCommandFile(.Object@localDir)
            if (.Object@host@isLocal == TRUE) {
              if (runInBackground == TRUE) {
                message("Run will be performed in parallel R process; the information about execution is limited.",
                        "\nPlease check the model directory (", .Object@localDir, ") for results.")
                parallel::mcparallel(Certara.NLME8::performShotgunCovarSearch(
                  as.character(.Object@argsList)))

              } else {
                Certara.NLME8::performShotgunCovarSearch(as.character(.Object@argsList),
                                                         reportProgress = TRUE)
                .remove_mappedDrive()
              }
            } else {
              .execute_remote(.Object)
            }
          }
)


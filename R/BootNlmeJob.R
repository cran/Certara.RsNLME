#' @include SimpleNlmeJob.R
#' @include BootstrapParams.R
#' @slot boot an instance of NlmeBootstrapParams class
BootNlmeJob <- setClass("BootNlmeJob",
                        slots = c(boot = "BootstrapParams"),
                        contains = "SimpleNlmeJob")


setMethod("initialize", "BootNlmeJob",
          function(.Object, ..., boot = "") {
  message("Boot Job")
  callNextMethod(.Object, ..., boot = boot)
})

setMethod("generateScript", "BootNlmeJob",
          definition = function(.Object) {

            script <- .paste_hashToScript("", .Object@host@hostType )
            script <-
              .paste_instsharedDirsToScript(script,
                                            .Object@host@hostType,
                                            .Object@host@installationDirectory,
                                            .Object@host@sharedDirectory)

            cmd_args <- paste(
              shQuote(.Object@remoteDir),
              .Object@argsList$engine,
              .Object@argsList$num_iterations,
              .Object@argsList$num_samples,
              .Object@argsList$max_tries,
              .Object@argsList$model_file,
              .Object@argsList$column_def_file,
              .Object@argsList$data_file,
              .Object@argsList$start_seed,
              .Object@argsList$extra_args_file,
              shQuote(.Object@argsList$files_to_copy, type = "cmd"),
              .Object@host@numCores,
              .Object@argsList$ConfidenceLevel,
              .Object@argsList$gridDirectory)

            if (.Object@host@hostType == "windows") {
              scriptName <- file.path(.Object@remoteDir, "cmd.ps1")

              cmd <- paste(
                "powershell -noninteractive -executionpolicy remotesigned -File $INSTALLDIR/phx_bootstrap.ps1",
                .Object@host@parallelMethod@method,
                "$INSTALLDIR",
                "$shared_directory",
                cmd_args)

              script <- paste0(script,
                               "\n$NLME_SKIP_INITIAL_BOOTSTRAP_RUN=")
              script <-
                paste0(script, "\n", cmd)

              script <- gsub("/", "\\", script, fixed = TRUE)

            } else {
              scriptName <- file.path(.Object@localDir, "cmd.sh")
              trap <- .paste_trap(.Object)
              script <- .paste_shRlocToScript(script,
                                              .Object@host@rLocation,
                                              .Object@host@scriptPath)

              cmd <- paste(shQuote("${INSTALLDIR}/phx_bootstrap.sh", type = "cmd"),
                           .Object@host@parallelMethod@method,
                           shQuote("${INSTALLDIR}", type = "cmd"),
                           "${shared_directory}",
                           cmd_args)


              script <- paste0(script,
                               "\nexport NLME_SKIP_INITIAL_BOOTSTRAP_RUN=", !.Object@boot@initialEstimates)

              script <-
                paste0(trap, "\n", script, "\n", cmd)
            }

            con <- file(scriptName, open = "wb")
            on.exit(close(con))
            cat(script, file = con, sep = "\n")
            return(scriptName)
          }
)

setMethod("executeJob", "BootNlmeJob",
          definition = function(.Object) {
            runInBackground <- .Object@runInBackground

            removeCommandFile(.Object@localDir)
            if (.Object@host@isLocal == TRUE) {
              Sys.setenv("NLME_SKIP_INITIAL_BOOTSTRAP_RUN" = !.Object@boot@initialEstimates)
              if (runInBackground == TRUE) {
                message("Run will be performed in parallel R process; the information about execution is limited.",
                        "\nPlease check the model directory (", .Object@localDir, ") for results.")
                parallel::mcparallel(Certara.NLME8::performBootstrap(unlist(.Object@argsList)))
                print(.Object)
              } else {
                Certara.NLME8::performBootstrap(unlist(.Object@argsList),
                                                reportProgress = TRUE)
                .remove_mappedDrive()
              }
            } else {
              .execute_remote(.Object)
            }

            .Object
          }
)



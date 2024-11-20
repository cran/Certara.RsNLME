#' @include SortColumns.R
#'
#' @slot sortColumns Class represents an NLME Sort columns
#' @slot scenarios List of scenarios
SortByNlmeJob <-
  setClass(
    "SortByNlmeJob",
    slots = c(
      sortColumns = "SortColumns",
      scenarios = "list"),
    contains = "SimpleNlmeJob"
  )


setMethod("initialize", "SortByNlmeJob",
          function(.Object, ...,
                   sortColumns,
                   scenarios = list()) {
            message("SortByNlmeJob Job")
            .Object@sortColumns <- sortColumns
            .Object@scenarios <- scenarios
            callNextMethod(.Object, ..., sortColumns = sortColumns, scenarios = scenarios)
          })


setMethod("generateScript", "SortByNlmeJob",
	definition = function(.Object) {
		sortColumnList <- gsub(",", " ", .Object@sortColumns@sortColumnList)
		sortColumnList <- trimws(sortColumnList)
		sortColumnList <- shQuote(sortColumnList, type = "cmd")

		script <- .paste_hashToScript("", .Object@host@hostType )
		script <-
		  .paste_instsharedDirsToScript(script,
		                                .Object@host@hostType,
		                                .Object@host@installationDirectory,
		                                .Object@host@sharedDirectory)

		cmd_args <- paste(.Object@remoteDir,
		                  .Object@argsFile,
		                  .Object@sortColumns@numSortColumns,
		                  sortColumnList,
		                  .Object@host@numCores,
		                  .Object@workflow)

    if (.Object@host@hostType == "windows") {
      scriptName <- file.path(.Object@remoteDir, "cmd.ps1")
      cmd <- paste("powershell -noninteractive -executionpolicy remotesigned -File $INSTALLDIR/phx_sortcol_estimation.ps1",
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
          paste(shQuote("${INSTALLDIR}/phx_sortcol_estimation.sh", type = "cmd"),
                .Object@host@parallelMethod@method,
                shQuote("${INSTALLDIR}", type = "cmd"),
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

setMethod("executeJob", "SortByNlmeJob",
          definition = function(.Object) {
            runInBackground <- .Object@runInBackground

            removeCommandFile(.Object@localDir)
            if (.Object@host@isLocal == TRUE) {
              if (runInBackground == TRUE) {
                message("Run will be performed in parallel R process; the information about execution is limited.",
                        "\nPlease check the model directory (", .Object@localDir, ") for results.")
                parallel::mcparallel(Certara.NLME8::performEstimationOnSortColumns(as.character(.Object@argsList)))
                print(.Object)
              } else {
                Certara.NLME8::performEstimationOnSortColumns(as.character(.Object@argsList),
                                                              reportProgress = TRUE)
                .remove_mappedDrive()
              }
            } else {
              .execute_remote(.Object)
            }
            .Object
          }
)



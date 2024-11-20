#' @include SimpleNlmeJob.R
#' @include ProfileParameters.R
#' @include SortColumns.R
#'
#' @slot profiles Class represents an NLME profile pertubation variable
#' @slot sortColumns Class represents an NLME Sort columns
#' @slot scenarios List of scenarios
ProfileNlmeJob <-
  setClass(
    "ProfileNlmeJob",
    slots = c(
      profiles = "ProfileParameters",
      sortColumns = "SortColumns",
      scenarios = "list"
    ),
    contains = "SimpleNlmeJob"
  )

setMethod("initialize", "ProfileNlmeJob",
          function(.Object,
                   ...,
                   profiles,
                   sortColumns,
                   scenarios = list()) {
            message("ProfileNlmeJob Job")
            .Object@profiles <- profiles
            .Object@sortColumns <- sortColumns
            .Object@scenarios <- scenarios
            callNextMethod(.Object, ...)
          })


setMethod(
  "executeJob",
  "ProfileNlmeJob",
  definition = function(.Object) {
    runInBackground <- .Object@runInBackground

    removeCommandFile(.Object@localDir)
    if (.Object@host@isLocal == TRUE) {
      if (runInBackground == TRUE) {
        message(
          "Run will be performed in parallel R process; the information about execution is limited.",
          "\nPlease check the model directory (",
          .Object@localDir,
          ") for results."
        )
        parallel::mcparallel(Certara.NLME8::performProfileEstimation(as.character(.Object@argsList)))
        print(.Object)
      } else {
        Certara.NLME8::performProfileEstimation(as.character(.Object@argsList), reportProgress = TRUE)
        .remove_mappedDrive()
      }
    } else {
      .execute_remote(.Object)
    }
  }
)


setMethod(
  "generateScript",
  "ProfileNlmeJob",
  definition = function(.Object) {
    script <- .paste_hashToScript("", .Object@host@hostType)
    script <-
      .paste_instsharedDirsToScript(
        script,
        .Object@host@hostType,
        .Object@host@installationDirectory,
        .Object@host@sharedDirectory
      )

    sortColumnList <-
      gsub(",", " ", .Object@sortColumns@sortColumnList)
    sortColumnList <- trimws(sortColumnList)
    sortColumnList <- shQuote(sortColumnList, type = "cmd")

    profiles <- getProfilesString(.Object@profiles)
    profiles <- shQuote(profiles, type = "cmd")

    cmd_args <- paste(
      .Object@remoteDir,
      .Object@argsFile,
      .Object@sortColumns@numSortColumns,
      sortColumnList,
      profiles,
      .Object@profiles@howToPertubate,
      .Object@host@numCores,
      .Object@workflow
    )

    if (.Object@host@hostType == "windows") {
      scriptName <- file.path(.Object@remoteDir, "cmd.ps1")

      cmd <- paste(
        "powershell -noninteractive -executionpolicy remotesigned -File $INSTALLDIR/phx_profile_estimation.ps1",
        .Object@host@parallelMethod@method,
        "$INSTALLDIR",
        "$shared_directory",
        cmd_args
      )

      script <- paste0(script, "\n", cmd)
      script <- gsub("/", "\\", script, fixed = TRUE)

    } else {
      scriptName <- file.path(.Object@localDir, "cmd.sh")
      trap <- .paste_trap(.Object)
      script <- .paste_shRlocToScript(script,
                                      .Object@host@rLocation,
                                      .Object@host@scriptPath)

      cmd <-
        paste(
          shQuote("${INSTALLDIR}/phx_profile_estimation.sh", type = "cmd"),
          .Object@host@parallelMethod@method,
          shQuote("$INSTALLDIR", type = "cmd"),
          "${shared_directory}",
          cmd_args
        )

      script <-
        paste0(trap, "\n", script, "\n", cmd)
    }

    con <- file(scriptName, open = "wb")
    on.exit(close(con))
    cat(script, file = con, sep = "\n")
    return(scriptName)
  }
)

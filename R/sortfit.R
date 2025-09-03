#' Executes an NLME simple estimation with sort keys and given scenarios
#'
#' Executes an NLME simple estimation with sort keys and given scenarios
#'
#' @inheritParams fitmodel
#' @param  sortColumns List of sort columns. See \code{\link{SortColumns}}.
#' If \code{missing}, empty sort columns argument is used and NLME dataset is used as is.
#' @param  scenarios List of scenarios with different sets of covariates.
#' See \code{\link{NlmeScenario}}
#' If \code{missing}, all covariates effects are considered as enabled.
#'
#' @inherit fitmodel return
#'
#' @details All the results in tabular format have scenario column and sorts columns appended.
#' The resulted logs (nlme7engine.log, err1.txt, dmp.txt, out.txt) are appended with a row delimiter
#' where the name of the Scenario and sort values are specified.
#'
#' @inheritSection fitmodel Non-loaded but returned files
#'
#' @seealso \code{\link{hostParams}, \link{engineParams},  \link{SortColumns},
#' \link{NlmeScenario}, \link{tableParams}}
#'
#' @examples
#' \dontrun{
#' input_data <- pkData
#'
#' model <-
#'   pkmodel(numCompartments = 2,
#'           data = input_data,
#'           ID = "Subject",
#'           Time = "Act_Time",
#'           A1 = "Amount",
#'           CObs = "Conc",
#'           workingDir = tempdir())
#'
#' model <-
#'   addCovariate(model,
#'                covariate = "BodyWeight",
#'                direction = "Backward",
#'                center = "Mean",
#'                effect = c("V", "Cl"))
#'
#' # multicore
#' multicoreHost <-
#'    hostParams(parallelMethod = "Multicore",
#'               hostName = "multicore",
#'               numCores = 4,
#'               sharedDirectory = tempdir())
#'
#' # specify scenarios
#' CovariateEffectNames <- listCovariateEffectNames(model)
#' combinations <-
#'   combn(c("", CovariateEffectNames),
#'         length(CovariateEffectNames),
#'         simplify = FALSE)
#'
#' scenarioNames <-
#'   lapply(combinations,
#'          function(x) {paste(x, collapse = " ")})
#'
#' scenarios <-
#'   lapply(scenarioNames,
#'          function(x, CovariateEffectNames) {
#'            CovariateCombinations <- unlist(strsplit(x, " ", fixed = TRUE))
#'            scenarioIndex <-
#'              paste(which(CovariateEffectNames %in% CovariateCombinations,
#'                          arr.ind = TRUE),
#'                          collapse = ", ")
#'            NlmeScenario(trimws(x), scenarioIndex)
#'          },
#'          CovariateEffectNames)
#'
#' res <-
#'   sortfit(model,
#'           hostPlatform = multicoreHost,
#'           params = engineParams(model, numIterations = 5, fastOptimization = TRUE),
#'           sortColumns = SortColumns("Gender"),
#'           scenarios = scenarios)
#'
#' }
#' @export
#'
sortfit <- function(model,
                    hostPlatform = NULL,
                    params,
                    sortColumns,
                    scenarios = list(),
                    simpleTables,
                    runInBackground = FALSE,
                    filesToReturn = "*",
                    ...) {
  if (missing(model)) {
    stop("model argument is required for run.")
  } else {
    stopifnot(inherits(model, "NlmePmlModel"))
  }

  ellipsisArgs <- list(...)
  hostPlatform <-
    .load_hostPlatform(hostPlatform,
                       ellipsisArgs,
                       model = model,
                       mode = "multicore")
  params <- .load_engineParams(model, params, ellipsisArgs)


  if (missing(sortColumns)) {
    message("\nsortColumns argument is not given. ",
            "Running with no sorts")
    sortColumns <- SortColumns("")
  } else {
    stopifnot(inherits(sortColumns, "SortColumns"))
  }

  if (!missing(scenarios)) {
    stopifnot(inherits(scenarios, "list"))
    lapply(scenarios, function(x) {
      stopifnot(inherits(x, "NlmeScenario"))
    })
  }

  if (!missing(simpleTables)) {
    if (length(simpleTables) == 1 &&
        inherits(simpleTables, "NlmeTableDef")) {
      simpleTables <- list(simpleTables)
    } else {
      stopifnot(inherits(simpleTables, "list"))
    }
  }

  if (hostPlatform@hostType == "Windows" && runInBackground) {
    warning("`runInBackground = TRUE` is not available on Windows. Setting argument to `FALSE`.")
    runInBackground <- FALSE
  } else {
    stopifnot(is.logical(runInBackground))
  }

  model@modelInfo@workingDir <-
    .prepare_wd(model@modelInfo@workingDir)
  workingDir <- model@modelInfo@workingDir
  stopifnot(.remove_oldResults(workingDir))

  writeDefaultFiles(
    model = model,
    dataset = model@dataset,
    Tables = simpleTables,
    sortColumns = sortColumns
  )

  workFlow <- "WorkFlow"

  argsFile <- GenerateControlfile(
    dataset = model@dataset,
    params = params,
    workFlow = workFlow,
    scenarios = scenarios,
    workingDir = workingDir,
    filesToReturn = filesToReturn
  )

  argsList <- list(
    method = hostPlatform@parallelMethod@method,
    install_directory = hostPlatform@installationDirectory,
    shared_directory = hostPlatform@sharedDirectory,
    localWorkingDir = workingDir,
    nlmeArgsFile = names(argsFile),
    numColumns = sortColumns@numSortColumns,
    ColumnNames = gsub(",", " ", sortColumns@sortColumnList),
    NumProc = hostPlatform@numCores
  )

  if (length(scenarios) == 0) {
    argsList <- c(argsList, workflowName = workFlow)
  } else {
    argsList <- c(argsList, workflowName = getScenarioNames(scenarios))
  }

  job <- SortByNlmeJob(
    jobType = "Sort_By_Column",
    localDir = workingDir,
    remoteDir = workingDir,
    host = hostPlatform,
    argsList = argsList,
    argsFile = names(argsFile),
    sortColumns = sortColumns,
    scenarios = scenarios,
    workflow = workFlow,
    runInBackground = runInBackground
  )

  .log_Execution(Model = model,
                 EngineParams = params,
                 RunMode = "Scenarios",
                 Host = hostPlatform)

  status <- executeJob(job)

  if (runInBackground) {
    .report_BackgroundJob(hostPlatform@isLocal,
                          LocalWorkingDir = workingDir,
                          RemoteDir = hostPlatform@sharedDirectory)

    status
  } else {
    result_list <-
      .get_resultList(
        workingDir,
        model@dataset@colDefFile,
        params@method,
        model@structuralParams,
        model@secondaryParameters
      )

    result_list
  }
}

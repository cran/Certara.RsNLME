getProfilesString <- function(profiles) {
  strList <- ""
  for (p in profiles@profileVars) {
    if (strList == "") {
      strList <- paste(p@effectName,
                       ",",
                       p@initialValue,
                       ",",
                       p@pertubateValues,
                       sep = "")
    } else {
      strList <- paste(
        strList,
        paste(
          p@effectName,
          ",",
          p@initialValue,
          ",",
          p@pertubateValues,
          sep = ""
        ),
        sep = " "
      )
    }
  }
  return(strList)
}

#' Executes an NLME profile perturbation
#'
#' Executes an NLME profile perturbation
#'
#' @param  hostPlatform How to execute the run(NlmeParallelHost)
#' @param  params Engine parameters(NlmeEngineExtraParams)
#' @param  profiles Profiles to perturbate(ProfileParameters)
#' @param  model PK/PD model
#' @param  sortColumns Optional list of columns to sort and fit(SortColumns)
#' @param  scenarios Optional list of scenarios to fit(NlmeScenario)
#' @param  runInBackground Set to \code{TRUE} to run in background and return prompt(Bool)
#'
#' @export
#'
#' @examples
#' \donttest{
#'   model <- pkmodel(
#'     parameterization = "Clearance",
#'     numCompartments = 2,
#'     data = pkData,
#'     ID = "Subject",
#'     Time = "Act_Time",
#'     A1 = "Amount",
#'     CObs = "Conc",
#'     workingDir = tempdir()
#'   )
#'
#'   params <- NlmeEngineExtraParams(
#'     method = 3,
#'     numIterations = 1
#'   )
#'
#'   host <- hostParams(
#'     sharedDirectory = tempdir(),
#'     parallelMethod = "MULTICORE",
#'     hostName = "local",
#'     numCores = 4
#'   )
#'
#'   profile1 <- ProfileVar(
#'     "tvV",
#'     9.548,
#'     "-2,2"
#'   )
#'
#'   profile2 <- ProfileVar(
#'     "tvCl",
#'     0.919,
#'     "-0.5,1.5"
#'   )
#'
#'   profiles <- ProfileParameters(
#'     "USE_DELTA",
#'     c(profile1, profile2)
#'   )
#'
#'   job <- profilePertubate(
#'     hostPlatform = host,
#'     params = params,
#'     profiles = profiles,
#'     model = model
#'   )
#' }
#' @keywords internal
#' @return List of results from NLME execution.
profilePertubate <- function(hostPlatform,
                             params = NULL,
                             profiles,
                             model = NULL,
                             sortColumns = SortColumns(""),
                             scenarios = list(),
                             runInBackground = FALSE) {
  if (!is.null(model)) {
    model@modelInfo@workingDir <-
      .prepare_wd(model@modelInfo@workingDir)
    Certara.RsNLME::writeDefaultFiles(model = model, dataset = model@dataset)
    workingDir <- model@modelInfo@workingDir
  } else {
    workingDir <- normalizePath(".", winslash = "/", mustWork = FALSE)
  }
  return(
    RunProfilePertubation(
      hostPlatform,
      model@dataset,
      params,
      profiles,
      sortColumns,
      scenarios,
      runInBackground,
      workingDir = workingDir
    )
  )
}


#' Execute an NLME profile perturbation
#'
#' Execute an NLME profile perturbation
#'
#' @param  hostPlatform How to execute the run(NlmeParallelHost)
#' @param  dataset Dataset and model information(NlmeDataset)
#' @param  params Engine parameters(NlmeEngineExtraParams)
#' @param  profiles Profiles to perturbate(ProfileParameters)
#' @param  sortColumns Optional list of columns to sort and fit(SortColumns)
#' @param  scenarios Optional list of scenarios to fit(NlmeScenario)
#' @param  runInBackground TRUE will run in background and return prompt(Bool)
#' @param  workingDir Directory in which to run the job. Current working directory is used if \code{NULL}
#'
#' @examples
#' \donttest{
#'   model <- pkmodel(
#'     parameterization = "Clearance",
#'     numCompartments = 2,
#'     data = pkData,
#'     ID = "Subject",
#'     Time = "Act_Time",
#'     A1 = "Amount",
#'     CObs = "Conc",
#'     workingDir = tempdir()
#'   )
#'
#'   params <- NlmeEngineExtraParams(
#'     method = 3,
#'     numIterations = 1
#'   )
#'
#'   host <- hostParams(
#'     sharedDirectory = tempdir(),
#'     parallelMethod = "None",
#'     hostName = "Local",
#'     numCores = 4
#'   )
#'
#'   profile1 <- ProfileVar(
#'     "tvV",
#'     9.548,
#'     "-2,0,2"
#'   )
#'
#'   profile2 <- ProfileVar(
#'     "tvCl",
#'     0.919,
#'     "-0.5,0,1.5"
#'   )
#'
#'   profiles <- ProfileParameters(
#'     "USE_DELTA",
#'     c(profile1, profile2)
#'   )
#'
#'   job <- profilePertubate(
#'     hostPlatform = host,
#'     params = params,
#'     profiles = profiles,
#'     model = model
#'   )
#' }
#' @export RunProfilePertubation
#' @keywords internal
#' @return Object of class \code{ProfileNlmeJob}
RunProfilePertubation <- function(hostPlatform,
                                  dataset,
                                  params,
                                  profiles,
                                  sortColumns,
                                  scenarios = list(),
                                  runInBackground = FALSE,
                                  workingDir = NULL) {
  workFlow <- "WorkFlow"

  if (hostPlatform@hostType == "Windows" && runInBackground) {
    warning("`runInBackground = TRUE` is not available on Windows. Setting argument to `FALSE`.")
    runInBackground <- FALSE
  } else {
    stopifnot(is.logical(runInBackground))
  }

  if (is.null(workingDir)) {
    cwd <- normalizePath(".", winslash = "/", mustWork = FALSE)
  } else {
    cwd <- workingDir
  }

  argsFile <- GenerateControlfile(
    dataset = dataset,
    params = params,
    workFlow = workFlow,
    scenarios = scenarios,
    workingDir = cwd
  )

  argsList <- list(
    method = hostPlatform@parallelMethod@method,
    install_directory = hostPlatform@installationDirectory,
    shared_directory = hostPlatform@sharedDirectory,
    localWorkingDir = cwd,
    nlmeArgsFile = names(argsFile),
    numColumns = sortColumns@numSortColumns,
    ColumnNames = gsub(",", " ", sortColumns@sortColumnList),
    profileArray = getProfilesString(profiles),
    profilePercentFlag = profiles@howToPertubate,
    NumProc = hostPlatform@numCores
  )

  if (length(scenarios) == 0) {
    argsList <- c(argsList, workFlow)
  } else {
    argsList <- c(argsList, getScenarioNames(scenarios))
  }

  job <- ProfileNlmeJob(
    jobType = "Profile_Pertubation",
    localDir = cwd,
    remoteDir = cwd,
    host = hostPlatform,
    argsList = argsList,
    argsFile = names(argsFile),
    profiles = profiles,
    sortColumns = sortColumns,
    scenarios = scenarios,
    workflow = workFlow,
    runInBackground = runInBackground
  )

  .log_Execution(Model = model,
                 EngineParams = params,
                 RunMode = "Profile",
                 Host = hostPlatform)

  status <- executeJob(job)
  return(job)
}

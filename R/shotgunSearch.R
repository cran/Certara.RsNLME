#' Executes an NLME shotgun covariate search
#'
#' Executes an NLME shotgun covariate search
#'
#' @inheritParams fitmodel
#' @param  hostPlatform Host definition for model execution. See \code{\link{hostParams}}.
#' If \code{missing}, multicore local host with 4 threads is used.
#' @param  covariateModel Covariate Effects Model providing the relationship
#' between covariates and structural parameters to test (\code{covariateModel(model)}).
#'
#' @return if \code{runInBackground = FALSE}, a data frame is returned with
#' shotgun (all combinations given the covariate model) search results,
#' i.e. "Overall" comma separated file.
#' Otherwise the \code{ShotgunNlmeJob} class object is returned.
#'
#' @seealso \code{\link{hostParams}, \link{engineParams}}
#'
#' @examples
#' \donttest{
#' # Define the model
#' model <- pkmodel(numCompartments = 2,
#'                  data = pkData,
#'                  ID = "Subject",
#'                  Time = "Act_Time",
#'                  A1 = "Amount",
#'                  CObs = "Conc")
#'
#' # Add Gender covariate of type categorical
#' model <- addCovariate(model,
#'                       covariate = "Gender",
#'                       type = "Categorical",
#'                       effect = c("V2", "Cl2"),
#'                       levels = c(0, 1),
#'                       labels = c("Female", "Male"))
#'
#' # Add Bodyweight covariate of type continuous
#' model <- addCovariate(model,
#'              covariate = "BodyWeight",
#'              type = "Continuous",
#'              direction = "Backward",
#'              center = "Mean",
#'              effect = c("V", "Cl"))
#'
#' # Define the host
#' host <- hostParams(parallelMethod = "None",
#'                    hostName = "local",
#'                    numCores = 1)
#'
#' # Define the engine parameters
#' params <- engineParams(model)
#'
#' # Define covariate model
#' cp <- covariateModel(model)
#'
#' # Perform shotgun search
#' OverallDF <-  shotgunSearch(model = model,
#'                             hostPlatform = host,
#'                             params = params,
#'                             covariateModel = cp,
#'                             runInBackground = FALSE)
#' }
#'
#' @export
shotgunSearch <- function(model,
                          hostPlatform = NULL,
                          params,
                          covariateModel,
                          runInBackground = FALSE,
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

  dataset <- model@dataset

  if (missing(covariateModel)) {
    message("\ncovariateModel argument is not given. Processing model object to make it.")
    covariateModel <- Certara.RsNLME::covariateModel(model)
  } else {
    stopifnot(inherits(covariateModel, "CovariateEffectModel"))
  }


  model@modelInfo@workingDir <-
    .prepare_wd(model@modelInfo@workingDir)
  cwd <- model@modelInfo@workingDir
  writeDefaultFiles(model = model,
                    dataset = dataset)

  workFlow <- "WorkFlow"

  if (hostPlatform@hostType == "Windows" && runInBackground) {
    warning("`runInBackground = TRUE` is not available on Windows. Setting argument to `FALSE`.")
    runInBackground <- FALSE
  } else {
    stopifnot(is.logical(runInBackground))
  }

  argsFile <-
    GenerateControlfile(
      dataset = dataset,
      params = params,
      workFlow = workFlow,
      workingDir = cwd
    )
  argsFileLines <- unlist(strsplit(argsFile, "\n"))

  modelFile <- model@dataset@modelFile
  extraArgsFile <- get_extraArgsFile(argsFileLines)

  filesToCopy <- argsFileLines[2]

  nlmeControlFile <- "newNlmeControlFile.txt"
  nlmeArgsFile <- "newNlmeargsCombined.txt"

  filesToCopy <-
    paste0(filesToCopy, " ", nlmeControlFile, " ", nlmeArgsFile)

  generateCovarSearchArgsFile(
    nlmeControlFile,
    nlmeArgsFile,
    dataset@modelFile,
    extraArgsFile,
    filesToCopy,
    covariateModel@numCovariates,
    gsub(",", " ", covariateModel@covariateList,
         fixed = TRUE),
    workingDir = cwd
  )

  argsList <- list(
    jobType = "COVAR_SEARCH",
    parallelMethod = hostPlatform@parallelMethod@method,
    install_dir = hostPlatform@installationDirectory,
    shared_directory = hostPlatform@sharedDirectory,
    localWorkingDir = cwd,
    controlFile = nlmeControlFile,
    NumProc = hostPlatform@numCores,
    workflow_name = workFlow
  )

  job <- ShotgunNlmeJob(
    jobType = "Shotgun",
    localDir = cwd,
    remoteDir = cwd,
    host = hostPlatform,
    argsList = argsList,
    argsFile = nlmeControlFile,
    workflow = workFlow,
    runInBackground = runInBackground
  )

  job@argsFile <- file.path(job@localDir, job@argsFile)


  .log_Execution(Model = model,
                 EngineParams = params,
                 RunMode = "Cov Shotgun",
                 Host = hostPlatform)

  status <- executeJob(job)

  if (runInBackground) {
    .report_BackgroundJob(hostPlatform@isLocal,
                          LocalWorkingDir = cwd,
                          RemoteDir = hostPlatform@sharedDirectory)
    status
  } else {
    OverallPath <- file.path(cwd, "Overall.csv")
    if (file.exists(OverallPath)) {
      OverallDF <- read.csv(OverallPath, check.names = FALSE)
      OverallDF
    } else {
      warning("File with CovSearch results\n",
              OverallPath,
              "\n not found.")
      job
    }
  }
}

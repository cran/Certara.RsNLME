#' Executes an NLME stepwise covariate search
#'
#' Executes an NLME stepwise covariate search
#'
#' @inheritParams fitmodel
#' @param  hostPlatform Host definition for model execution. See \code{\link{hostParams}}.
#' If \code{missing}, multicore local host with 4 threads is used.
#' @param  covariateModel Covariate Effects Model providing the relationship
#' between covariates and structural parameters to test (\code{covariateModel(model)}).
#' @param  stepwiseParams Stepwise parameters defining decision tree.
#' See \code{\link{StepwiseParams}}
#'
#' @return if \code{runInBackground = FALSE}, a data frame is returned with
#' stepwise search results, i.e. "Overall" comma separated file.
#' Otherwise the \code{StepwiseNlmeJob} class object is returned.
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
#' defaultHost <- hostParams(parallelMethod = "None",
#'                    hostName = "local",
#'                    numCores = 1)
#'
#' # Define the engine parameters
#' params <- engineParams(model)
#'
#' # Define covariate model
#' cp <- covariateModel(model)
#'
#' # Define the stepwise parameters
#' sp <- StepwiseParams(0.01, 0.001, "-2LL")
#'
#' # Perform stepwise search
#' OverallDF <-  stepwiseSearch(model = model,
#'                       hostPlatform = defaultHost,
#'                       params = params,
#'                       covariateModel = cp,
#'                       stepwiseParams = sp,
#'                       runInBackground = FALSE)
#' }
#'
#' @export
#'
stepwiseSearch <- function(model,
                           hostPlatform = NULL,
                           params,
                           covariateModel,
                           stepwiseParams,
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

  if (missing(stepwiseParams)) {
    stepwiseParams <- Certara.RsNLME::StepwiseParams(addPValue = 0.01,
                                                     removePValue = 0.001,
                                                     method = "-2LL")
    message(
      "\nstepwiseParams argument is not given. ",
      "\nUsing Default: ",
      "\nThresholds for adding/removing a covariate effect: ",
      paste(
        stepwiseParams@addPValue,
        stepwiseParams@removePValue,
        sep = " / "
      ),
      "\nMethod: ",
      stepwiseParams@method
    )
  } else {
    stopifnot(inherits(stepwiseParams, "StepwiseParams"))
  }

  if (missing(covariateModel)) {
    message("\ncovariateModel argument is not given. Processing model object to make it.")
    covariateModel <- Certara.RsNLME::covariateModel(model = model)
  } else {
    stopifnot(inherits(covariateModel, "CovariateEffectModel"))
  }

  dataset <- model@dataset
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
  extraArgsFile <- get_extraArgsFile(argsFileLines)

  filesToCopy <- argsFileLines[2]

  StepwiseParamsString <-
    paste0(stepwiseParams@method,
           ":",
           covariateModel@degreesOfFreedom)

  argList <- list(
    method = hostPlatform@parallelMethod@method,
    install_directory = hostPlatform@installationDirectory,
    shared_directory = hostPlatform@sharedDirectory,
    localWorkingDir = cwd,
    modelFile = dataset@modelFile,
    nlmeArgsFile = extraArgsFile,
    listOfFilesToCopy = filesToCopy,
    numCovariates = covariateModel@numCovariates,
    CovariateNames = gsub(",", " ", covariateModel@covariateList, fixed = TRUE),
    NCriteria = StepwiseParamsString,
    addPValue = stepwiseParams@addPValue,
    removePValue = stepwiseParams@removePValue,
    NumProc = hostPlatform@numCores,
    workflowName = workFlow
  )

  job <- StepwiseNlmeJob(
    jobType = "Stepwise",
    localDir = cwd,
    remoteDir = cwd,
    host = hostPlatform,
    argsList = argList,
    argsFile = names(argsFile),
    workflow = workFlow,
    runInBackground = runInBackground,
    stepParam = stepwiseParams
  )

  .log_Execution(Model = model,
                 EngineParams = params,
                 RunMode = "Cov Stepwise",
                 Host = hostPlatform)

  results <- executeJob(job)

  if (runInBackground) {
    .report_BackgroundJob(hostPlatform@isLocal,
                          LocalWorkingDir = cwd,
                          RemoteDir = hostPlatform@sharedDirectory)
    results
  } else {
    OverallPath <- file.path(cwd, "Overall.csv")
    if (file.exists(OverallPath)) {
      OverallDF <- read.csv(OverallPath, check.names = FALSE)
      StepwisePath <- file.path(cwd, "Stepwise.txt")
      if (file.exists(StepwisePath)) {
        StepwiseText <- readLines(StepwisePath)
        ScenarioToUse <- grepl("Scenario to use =", StepwiseText)
        if (any(ScenarioToUse)) {
          ScenarioToUseTextSplit <-
            strsplit(StepwiseText[ScenarioToUse], split = " ")[[1]]
          cstepPos <- grep("cstep[0-9]+", ScenarioToUseTextSplit)
          if (length(cstepPos) > 0) {
            cstep <- ScenarioToUseTextSplit[cstepPos][1]
            BestScenario <- grepl(cstep, OverallDF$Scenario)
            if (any(BestScenario)) {
              OverallDF$BestScenario <- BestScenario
            }
          }
        }
      }
      OverallDF
    } else {
      warning("File with CovSearch results\n",
              OverallPath,
              "\n not found.")
      results
    }
  }
}

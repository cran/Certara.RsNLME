GenerateControlfile <- function(dataset,
                                params,
                                workFlow,
                                bootStratify = "",
                                vpcOption = NULL,
                                simOption = NULL,
                                scenarios = c(),
                                workingDir = NULL,
                                filesToReturn = "*") {
  controlFilename <- "jobControlFile.txt"
  argsFilename <- "jobArgsCombined.txt"
  if (!is.null(workingDir)) {
    controlFilename <- file.path(workingDir, controlFilename)
    argsFileFullPath <- file.path(workingDir, argsFilename)
  } else {
    argsFileFullPath <- argsFilename
  }

  GenerateParamsfile(argsFileFullPath,
                     dataset,
                     params,
                     bootStratify,
                     vpcOption,
                     simOption,
                     scenarios = scenarios)
  controlFile <-
    paste(
      dataset@modelFile,
      paste(
        argsFilename,
        dataset@dataFile,
        dataset@colDefFile,
        dataset@modelFile
      ),
      sep = "\n"
    )

  if (dataset@estimatesDefFile != "") {
    controlFile <- paste(controlFile,
                         dataset@estimatesDefFile,
                         dataset@estimatesDataFile)
  }

  if (dataset@doseDefFile != "") {
    controlFile <- paste(controlFile,
                         dataset@doseDefFile,
                         dataset@doseDataFile)
  }

  if (dataset@ranEffectDefFile != "") {
    controlFile <- paste(controlFile,
                         dataset@ranEffectDefFile,
                         dataset@ranEffectDataFile)
  }

  # we don't need .dat in bootstrap and covsearch
  ParentFunction <-
    paste0(deparse(sys.calls()[[sys.nframe() - 1]]), collapse = "")

  controlFile <- paste(controlFile,
                       "progress.xml log.txt compilelog.txt", sep = "\n")

  if (!is.null(vpcOption) ||
      (!is.null(simOption) && simOption@yVariables == "")) {
    outputFilename <- dataset@predoutFilename
  } else if (!is.null(simOption) && simOption@yVariables != "") {
    # guessing individual mode
    outputFilename <- dataset@simoutFilename
  } else {
    outputFilename <- "out.txt"
  }

  servicingFiles <-
    c("err1.txt",
      "err2.txt",
      "ps1err.txt",
      "fort.27",
      "integration_errors.txt")
  servicingFilesString <- paste(servicingFiles, collapse = " ")

  defaultReturnedNLMEFiles <-
    c("nlme7engine.log", "out.txt", "dmp.txt")

  returnedNLMEFilesString <- paste(defaultReturnedNLMEFiles, collapse = " ")

  filesToReturnString <- paste(filesToReturn, collapse = " ")

  numTodo <- length(scenarios)
  done <- FALSE
  current <- 1
  if (numTodo == 0) {
    controlFile <- paste(controlFile,
                         "1", sep = "\n")
  } else {
    controlFile <- paste(controlFile,
                         numTodo, sep = "\n")
  }

  while (!done) {
    if (numTodo != 0) {
      workFlow <- attr(scenarios[[current]], "scenarioName")
    }

    controlFile <- paste(
      controlFile,
      sprintf(
        #"%s,%s:%d,,%s,*.csv *.txt *.log *.dat *.LOG,dmp.txt nlme7engine.log",
        "%s,%s:%d,%s,%s,%s %s, ",
        workFlow,
        argsFilename,
        current,
        filesToReturnString,
        outputFilename,
        servicingFilesString,
        returnedNLMEFilesString
      ),
      sep = "\n"
    )

    if (numTodo == 0) {
      done <- TRUE
    } else {
      current <- current + 1
      if (current > numTodo) {
        done <- TRUE
      }
    }
  }

  names(controlFile) <- controlFilename
  cat(controlFile, "\n", file = controlFilename, append = FALSE)
  return(controlFile)
}

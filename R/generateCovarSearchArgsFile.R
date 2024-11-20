generateCovarSearchArgsFile <- function(controlFilename, nlmeArgsFile, modelFilename, nlmeArgsFilename, inputFileArray, numCovariates, covarNamesArray, workingDir) {
  controlFilenameFullpath <- file.path(workingDir, controlFilename)
  nlmeArgsFileFullpath <- file.path(workingDir, nlmeArgsFile)
  appendFlag <- FALSE
  nlmeArgLines <- readLines(file.path(workingDir, nlmeArgsFilename))
  numTodo <- as.integer(numCovariates)
  submodels <- rep(FALSE, numTodo)

  cat(modelFilename,
      paste(inputFileArray, collapse = " "),
      "*.csv *.txt *.log *.LOG",
      2^numTodo,
      sep = "\n", file = controlFilenameFullpath)

  nxtScenario <- 0
  scenarioBase <- "cshot"
  outputBase <- "out"
  done <- FALSE
  while (TRUE) {
    nxtScenarioName <- sprintf("%s%03d", scenarioBase, nxtScenario)
    suffix <- ""
    nxtScenarioDescription <- ""
    for (i in 1:numTodo) {
      if (submodels[i] == TRUE) {
        suffix <- paste0(suffix, "1")
      } else {
        suffix <- paste0(suffix, "0")
      }
    }
    argFlag <- ""
    na <- unlist(strsplit(covarNamesArray, split = " "))

    for (i in 1:numTodo) {
      if (submodels[i] == TRUE) {
        nxtScenarioDescription <- paste0(nxtScenarioDescription, " ", na[i])
        argFlag <- paste0(argFlag, "_", (i - 1))
      }
    }
    argFlag <- paste0(argFlag, "_")
    outputFile <- paste0(outputBase, suffix, ".txt")
    for (i in 1:numTodo) {
      submodels[i] <- !submodels[i]
      if (submodels[i]) {
        break
      }
    }

    cat(paste(paste0(nxtScenarioName, nxtScenarioDescription),
              sprintf("%s:%d", nlmeArgsFile, nxtScenario + 1),
              "",
              outputFile,
              outputFile,
              "progress.txt", sep = ","),
              file = controlFilenameFullpath, append = TRUE, sep = "\n")

    # cat(nxtScenarioName, file = controlFilenameFullpath, sep = "", append = TRUE)
    # cat(nxtScenarioDescription, file = controlFilenameFullpath, sep = ",", append = TRUE)
    # cat(sprintf(",%s:%d,", nlmeArgsFile, nxtScenario + 1), file = controlFilenameFullpath, sep = ",", append = TRUE)
    # cat(sprintf(",%s,%s,progress.txt", outputFile, outputFile), file = controlFilenameFullpath, sep = "\n", append = TRUE)

    cat(sprintf("/xe %s", argFlag), file = nlmeArgsFileFullpath, sep = "\n", append = appendFlag)
    appendFlag <- TRUE
    for (l in nlmeArgLines) {
      l <- gsub("out.txt", "", l, fixed = TRUE) # get read of out.txt if there is any
      cat(l, file = nlmeArgsFileFullpath, sep = "\n", append = appendFlag)
    }
    cat(paste(" /out_file", outputFile),
        file = nlmeArgsFileFullpath, sep = "\n", append = appendFlag)
    if (done == TRUE) {
      break
    }
    nxtScenario <- nxtScenario + 1
    n <- sum(submodels)

    if (n == numTodo) {
      done <- TRUE
    }
  }
}

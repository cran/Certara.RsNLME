.modify_s4class <- function(s4classInst, ellipsisArgs) {
  AddNames <- intersect(slotNames(s4classInst), names(ellipsisArgs))
  for (ParamsName in AddNames) {
    attr(s4classInst, ParamsName) <- ellipsisArgs[[ParamsName]]
  }

  s4classInst
}

# helper function to collect results from files to the list
.get_resultList <-
  function(workingDir,
           colDefFile,
           engine,
           stparms,
           secondaries) {
    result_list <- list()
    fitmodeldfNames <-
      paste0(
        c(
          "Overall",
          "theta",
          "residuals",
          "Secondary",
          "thetaCorrelation",
          "thetaCovariance",
          "Covariance",
          "ConvergenceData",
          "nonParSupportResult",
          "nonParStackedResult",
          "nonParEtaResult",
          "nonParOverallResult",
          "StrCovariate",
          "StrCovariateCat",
          "posthocStacked"
        ),
        ".csv"
      )

    if (engine == 6) {
      fitmodeldfNames <- c(fitmodeldfNames, paste0(c("ParDer"), ".csv"))
    } else {
      fitmodeldfNames <- c(fitmodeldfNames, paste0(
        c(
          "Eta",
          "EtaStacked",
          "EtaEta",
          "EtaCov",
          "EtaCovariate",
          "EtaCovariateCat",
          "omega",
          "omega_stderr"
        ),
        ".csv"
      ))

      fitmodeldfNames <- c(fitmodeldfNames, "bluptable.dat")
    }

    if (dirname(colDefFile) == ".") {
      colDefFile <- file.path(workingDir, colDefFile)
    } else {
      colDefFile <- file.path(workingDir, basename(colDefFile))
    }

    TableNames <-
      strsplit(Certara.NLME8::getTableNames(colDefFile, simtbl = F), " +")[[1]]
    fitmodeldfNames <-
      c(fitmodeldfNames, TableNames[TableNames != ""])

    for (fitmodeldfName in fitmodeldfNames) {
      fitmodeldfPath <- file.path(workingDir, fitmodeldfName)
      if (!file.exists(fitmodeldfPath) ||
          readLines(fitmodeldfPath, n = 1L) == "")
        next

      fitmodeldf <- data.table::data.table()
      try(fitmodeldf <-
            data.table::fread(fitmodeldfPath, check.names = FALSE, fill = TRUE),
          silent = TRUE)
      if (nrow(fitmodeldf) == 0) {
        result_list[[tools::file_path_sans_ext(fitmodeldfName)]] <- NA
      }  else if (fitmodeldfName == "omega.csv") {
        # omega
        omegaList <- .get_omega_DFs(fitmodeldf)
        if (!all(is.na(omegaList))) {
          result_list <- c(result_list, omegaList)
        }

      } else if (fitmodeldfName == "omega_stderr.csv") {
        # omega SE
        OmegaSE_DF <- .get_omegaSE_DF(fitmodeldf)
        if (!all(is.na(OmegaSE_DF))) {
          result_list[[tools::file_path_sans_ext(fitmodeldfName)]] <-
            OmegaSE_DF
        }
      } else if (grepl("^nonPar", fitmodeldfName)) {
        # get rid of Result in the name
        fitmodeldfName <- gsub("Result", "", fitmodeldfName)
        result_list[[tools::file_path_sans_ext(fitmodeldfName)]] <-
          fitmodeldf
      } else if (fitmodeldfName == "Covariance.csv") {
        # remove rows where all data column values are zero
        if (ncol(fitmodeldf) > 2) {
          dataColumns <-
            (which(colnames(fitmodeldf) == "Var Name") + 1):ncol(fitmodeldf)
          zeroRowsToRetain <-
            apply(fitmodeldf[, ..dataColumns], 1, sum, na.rm = TRUE) != 0
          # remove columns where all data are zero
          zeroColsToRetain <-
            sapply(fitmodeldf, function(x) {
              if (is.character(x))
                TRUE
              else
                sum(x, na.rm = TRUE) != 0
            })
          fitmodeldf <-
            fitmodeldf[zeroRowsToRetain, zeroColsToRetain, with = FALSE]
        }

        result_list[[tools::file_path_sans_ext(fitmodeldfName)]] <-
          fitmodeldf
      } else if (fitmodeldfName == "Secondary.csv") {
        # impute units from the model if given in Secondary
        fitmodeldf <-
          .add_unitsFixefSecondary(
            fitmodeldf = fitmodeldf,
            modelList = secondaries,
            nameColumn = "Secondary",
            fitmodeldfPath = fitmodeldfPath
          )

        result_list[[tools::file_path_sans_ext(fitmodeldfName)]] <-
          fitmodeldf

      } else if (fitmodeldfName == "theta.csv") {
        # impute units from the model if given in StructuralParameter
        fitmodeldf <-
          .add_unitsFixefSecondary(
            fitmodeldf = fitmodeldf,
            modelList = stparms,
            nameColumn = "Parameter",
            fitmodeldfPath = fitmodeldfPath
          )

        result_list[[tools::file_path_sans_ext(fitmodeldfName)]] <-
          fitmodeldf

      } else {
        result_list[[tools::file_path_sans_ext(fitmodeldfName)]] <-
          fitmodeldf
      }
    }

    nlme7engineFile <- file.path(workingDir, "nlme7engine.log")
    if (file.exists(nlme7engineFile)) {
      result_list[["nlme7engine.log"]] <- readLines(nlme7engineFile)
    }

    dmp.txtFile <- file.path(workingDir, "dmp.txt")
    if (file.exists(dmp.txtFile)) {
      tryCatch({
        # need to get rid of posthoc and residuals
        dmpLines <- readLines(dmp.txtFile)
        dmpLines <-
          .remove_dmptxtParts(partName = "residuals", dmpLines)
        dmpLines <-
          .remove_dmptxtParts(partName = "posthoc", dmpLines)
        dmpLines <-
          gsub("(=\\W-*nan)|(=\\W-*1.#IND)", "= NA", dmpLines)

        if (as.list(sys.call(-1))[[1]] == "fitmodel") {
          source(textConnection(paste(dmpLines, collapse = '\n')), local = TRUE)
          result_list[["dmp.txt"]] <- dmp.txt
        } else {
          result_list[["dmp.txt"]] <- dmpLines
        }
      }, error = function(cond) {
        message("Cannot load dmp.txt structure into model object.")
        message("Error reported: ", cond)
      })
    }

    result_list
  }

.add_unitsFixefSecondary <-
  function(fitmodeldf,
           modelList,
           nameColumn,
           fitmodeldfPath) {
    unitsImputed <- FALSE
    if (length(modelList) > 0) {
      if (nameColumn == "Secondary") {
        NamesToImpute <- sapply(modelList, function(x)
          x@name)
        UnitsToImpute <-
          sapply(modelList, function(x)
            slot(x, "unit"))
      } else {
        UnitsToImpute <- sapply(modelList, function(x)
          slot(x, "units"))
        NamesToImpute <- names(UnitsToImpute)
      }
    }

    NamesInDF <- unique(fitmodeldf[[nameColumn]])
    for (Name in NamesInDF) {
      if (!Name %in% NamesToImpute)
        next
      UnitsInDF <-
        fitmodeldf$Units[which(Name == fitmodeldf[[nameColumn]])]
      UnitToImpute <-
        UnitsToImpute[which(Name == NamesToImpute)]

      if (UnitToImpute != "") {
        if (any(!is.na(UnitsInDF))) {
          warning(
            "Units for ",
            Name,
            " given in the model will override the units ",
            "calculated from the units in the dataset."
          )
        }

        # substitute units in original dataset
        fitmodeldf$Units[which(Name == fitmodeldf[[nameColumn]])] <-
          UnitToImpute
        unitsImputed <- TRUE
      }
    }

    # overwrite the original file if units are imputed
    if (unitsImputed)
      utils::write.csv(
        fitmodeldf,
        fitmodeldfPath,
        row.names = FALSE,
        na = "",
        quote = FALSE
      )

    fitmodeldf
  }

.remove_dmptxtParts <- function(partName, dmpLines) {
  partNameQuoted <- paste0("\"", partName, "\"")
  partStart <- grep(paste(partNameQuoted, "="), dmpLines)
  if (length(partStart) != 0) {
    partEnd <- grep(paste("end of", partName), dmpLines)
    namesStart <- grep(" names =", dmpLines)
    dmpLines[namesStart] <-
      gsub(paste(",", partNameQuoted), "", dmpLines[namesStart], fixed = TRUE)
    if (length(partStart) == length(partEnd)) {
      for (chunk in length(partStart):1) {
        dmpLines <- dmpLines[-c(partStart[chunk]:partEnd[chunk])]
      }
    }
  }

  dmpLines
}

# helper function to transform the working directory
.prepare_wd <- function(workingDir, showWarnings = FALSE) {
  workingDir <- trimws(workingDir)
  if (workingDir == "")
    return(normalizePath(".", winslash = "/", mustWork = FALSE))

  workingDir <-
    normalizePath(workingDir, winslash = "/", mustWork = FALSE)
  if (dir.exists(workingDir))
    return(workingDir)

  dirCreated <-
    dir.create(workingDir, recursive = TRUE, showWarnings = showWarnings)
  if (!dir.exists(workingDir)) {
    errMsg <- paste("Cannot create the directory to run:\n", workingDir)
    if (.Platform$OS.type == "windows" &&
        nchar(workingDir) > 250) {
      errMsg <-
        paste(
          errMsg,
          "\nPossible reason: Current directory path is too long.",
          "Consider to shrink the path for execution."
        )
    }

    stop(errMsg, call. = FALSE)
  }

  workingDir
}

.remove_oldResults <- function(workingDir) {
  csvNames <-
    paste0(
      c(
        "ConvergenceData",
        "doses",
        "EtaStacked",
        "Overall",
        "omega",
        "omega_stderr",
        "Secondary",
        "Eta",
        "EtaEta",
        "IdCarry",
        "iniest",
        "ParDer",
        "predout",
        "posthoc",
        "res",
        "residuals",
        "theta",
        "thetaCovariance",
        "thetaCorrelation"
      ),
      ".csv"
    )

  datNames <-
    paste0(c("bluptable", "LLbysub"), ".dat")
  txtNames <-
    paste0(
      c(
        "err1",
        "err2",
        "out",
        "mpilog",
        "IniCarry",
        "dmp",
        "EtaCov",
        "EtaEta",
        "EtaShrinkageBySubject",
        "IdEta",
        "IniCovr",
        "log",
        "mpilog",
        "MultCovr",
        "ps1err",
        "ps1log",
        "StatusWindow",
        "StrCov",
        "progress"
      ),
      ".txt"
    )
  xmlNames <-
    paste0(c("progress"), ".xml")

  logNames <-
    paste0(c("nlme7engine"), ".log")

  JobNames <- ".*\\.Job\\d+"

  statusNames <- ".*\\.status"

  ascNames <- ".*\\.asc"

  filesToDelete <-
    list.files(workingDir,
               pattern = paste(
                 c(
                   csvNames,
                   datNames,
                   txtNames,
                   xmlNames,
                   logNames,
                   JobNames,
                   statusNames,
                   ascNames
                 ),
                 collapse = "|"
               ),
               full.names = TRUE)

  Value <- unlink(filesToDelete)
  status <- !Value
}

.prepare_sharedDirectory <- function(argsList, isLocal) {
  if (argsList$shared_directory == "") {
    message("sharedDirectory is not given in the host class instance.")
    sharedDirectory <- Sys.getenv("NLME_ROOT_DIRECTORY")
    if (dir.exists(sharedDirectory)) {
      message(paste0(
        "Using NLME_ROOT_DIRECTORY env.variable instead:\n",
        sharedDirectory
      ))
      argsList$shared_directory <- sharedDirectory
    } else {
      sharedDirectory <- normalizePath(".", winslash = "/", mustWork = FALSE)
      message(
        paste0(
          "Valid NLME_ROOT_DIRECTORY is not given, using current working directory:\n",
          sharedDirectory
        )
      )
      argsList$shared_directory <- sharedDirectory

    }
  }

  argsList$shared_directory <-
    .prepare_wd(argsList$shared_directory, showWarnings = FALSE)

  if (.Platform$OS.type != "windows" ||
      !isLocal ||
      !grepl("^(\\\\)|(//)", argsList$shared_directory)) {
    return(argsList$shared_directory)
  }

  # try to catch UNC path on Windows
  tryCatch({
    getLocation <-
      system2(
        "powershell",
        args = paste0("('", argsList$shared_directory, "'| %{$_.Drive}) -eq $NUL"),
        stdout = TRUE,
        stderr = TRUE
      )
    if (!is.null(attr(getLocation, "status"))) {
      stop(
        "Cannot use powershell to identify UNC path on Windows, errors reported: ",
        getLocation
      )
    } else if (getLocation != "True") {
      return(argsList$shared_directory)
    }

    Drive <-
      system2("powershell",
              args = "ls function:[d-z]: -n | ?{ !(test-path $_) } | select -First 1",
              stdout = TRUE,
              stderr = TRUE)

    if (!is.null(attr(Drive, "status")) ||
        !grepl("^[A-Z]:$", Drive)) {
      stop("Cannot use powershell to identify UNC path on Windows, errors reported: ",
           Drive)
    }

    DriveWOColon <- substr(Drive, 1, 1)

    setDrive <-
      system2(
        "powershell",
        args = paste0(
          "New-PSDrive -Name '",
          DriveWOColon,
          "' -PSProvider FileSystem -Root (Get-Item -LiteralPath '",
          argsList$shared_directory,
          "').FullName -Persist"
        ),
        stdout = TRUE,
        stderr = TRUE
      )

    if (!is.null(attr(setDrive, "status"))) {
      stop("Cannot use powershell to identify UNC path on Windows, errors reported: ",
           setDrive)
    }

    # will be used in unmapping
    Sys.setenv("NLME_ROOT_DIRECTORY_UNCDRIVE" = DriveWOColon)

    message(
      "Since shared directory to be used: ",
      argsList$shared_directory,
      "\nidentified as UNC path, R mapped that folder to drive ",
      Drive,
      "\nWhen execution ends, R will unmap that drive automatically"
    )

    argsList$shared_directory <- paste0(Drive, "/")
  }, error = function(cond) {
    message(cond)
  })

  argsList$shared_directory
}

.remove_mappedDrive <- function() {
  DriveWOColon <- Sys.getenv("NLME_ROOT_DIRECTORY_UNCDRIVE")
  if (DriveWOColon == "")
    return(TRUE)
  tryCatch({
    testPath <- .test_path(paste0(DriveWOColon, ":"))
    if (!is.null(attr(testPath, "status")) || testPath != "True") {
      stop(
        "Cannot use powershell to unmap drive ",
        DriveWOColon,
        " since Test-Path claims it is already gone."
      )
    }

    netUse <-
      system2(
        "powershell",
        args = paste0("net use ", DriveWOColon, ": /delete"),
        stdout = TRUE,
        stderr = TRUE
      )
    # testing again
    testPath <- .test_path(paste0(DriveWOColon, ":"))
    if (testPath == "True") {
      RemovePSDrive <-
        system2(
          "powershell",
          args = paste0("Remove-PSDrive -Name '", DriveWOColon, ":'"),
          stdout = TRUE,
          stderr = TRUE
        )

      testPath <- .test_path(paste0(DriveWOColon, ":"))
      if (testPath == "True") {
        stop(
          "Cannot use powershell to unmap drive ",
          DriveWOColon,
          "errors reported:",
          RemovePSDrive,
          netUse
        )
      }
    }
  }, error = function(cond) {
    warning(cond)
    return(FALSE)
  }, finally = Sys.unsetenv("NLME_ROOT_DIRECTORY_UNCDRIVE"))

  return(TRUE)
}

.test_path <- function(path) {
  testPath <-
    system2(
      "powershell",
      args = paste0("Test-Path -Path '", path, "'"),
      stdout = TRUE,
      stderr = TRUE
    )
  testPath
}

.report_BackgroundJob <- function(isLocal, LocalWorkingDir, RemoteDir) {
  if (isLocal) {
    message(
      "\nSince running in background,",
      "the results will be available later in ",
      LocalWorkingDir,
      "\n"
    )
  } else {
    message(
      "\nSince running in background,",
      "the results will be available later for download in ",
      RemoteDir,
      " subfolder.",
      "\n"
    )
  }
}

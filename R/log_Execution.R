.log_Execution <- function(Model, EngineParams, RunMode, Host) {
  LogDebug <- Sys.getenv("LogDebug") != ""
  tryCatch({
    product_submodule_version <-
      paste(strsplit(as.character(utils::packageVersion("Certara.RsNLME")), "\\.")[[1]][1:3], collapse = ".")

    if (Model@isTextual) {
      structure_type <-
        residual_error <-
        "textual"
    } else {
      structure_type <-
        switch(Model@modelType@modelType,
               "PK/Linear",
               "PK",
               "Emax",
               "PK/Emax",
               "PK/Indirect",
               "Linear")

      residual_error <-
        switch(Model@errorModel@effectsList[[1]]@errorType,
               "Additive",
               "Log-additive",
               "Multiplicative",
               "Add+Mult",
               "Mix Ratio",
               "Power",
               "Custom")
    }

    number_of_covariates <- length(Model@covariateList)
    if (number_of_covariates > 0) {
      LeftBounds <- c(0, 1, 4, 11, 33, Inf)
      BoundNumber <- sum(number_of_covariates >= LeftBounds)
      number_of_covariates <-
        paste0("[", LeftBounds[BoundNumber], "-", LeftBounds[BoundNumber + 1] - 1, "]")
    }

    Cols1 <- unlist(Model@userDefinedExtraDefs)
    if (length(Cols1) > 0) {
      DoseAddlStatements <- c("addlcol",
                              "addl")
      RowsAddlStartWith <-
        paste0("(", DoseAddlStatements, "\\()", collapse = "|")
      ADDL <- any(grepl(paste0("(\\W|^)", RowsAddlStartWith), Cols1))

      DoseSsStatements <- c("sscol",
                            "ss")
      RowsSsStartWith <-
        paste0("(", DoseSsStatements, "\\()", collapse = "|")

      steady_state <- any(grepl(paste0("(\\W|^)", RowsSsStartWith), Cols1))
    } else {
      ADDL <- steady_state <- FALSE
    }

    algorithm <-
      switch(EngineParams@method,
             "QRPEM",
             "IT2S-EM",
             "FOCE L-B",
             "FO",
             "FOCE ELS",
             "Naive Pooled")

    if (!is.null(algorithm) &&
        EngineParams@xfocehess == 0) {
      algorithm <- "Laplacian"
    } else if (length(algorithm) == 0) {
      algorithm <- NA
    }

    max_ODE <-
      switch(EngineParams@odeToUse,
             "auto-detect",
             "stiff",
             "non-stiff DVERK",
             "auto-detect",
             "auto-detect",
             "matrix-exp",
             "non-stiff DOPRI5",
             "Higham")

    if (length(max_ODE) == 0) max_ODE <- NA

    if (EngineParams@xstderr == 2) {
      stderr <- "Forward Diff"
    } else if (EngineParams@xstderr == 1) {
      stderr <- "Central Diff"
    } else {
      stderr <- "none"
      stderr_method <- NA
    }

    if (stderr != "none") {
      if (nchar(EngineParams@sand) > 0) {
        stderr_method <- "Sandwich"
      } else if (nchar(EngineParams@fisher) > 0) {
        stderr_method <- "Fisher Score"
      } else if (nchar(EngineParams@autodetect) > 0) {
        stderr_method <- "Auto-detect"
      } else {
        stderr_method <- "Hessian"
      }
    }

    InstallDir <- Sys.getenv("INSTALLDIR")
    PML_BIN_DIR <- Sys.getenv("PML_BIN_DIR")
    WorkingDir <- path.expand(file.path(InstallDir, PML_BIN_DIR))
    LicensingTool <- file.path(WorkingDir, "cadlicensingtool")

    CADOutput <-
      .runCmd_LicensingTool(LicensingTool = LicensingTool,
                            WorkingDir = WorkingDir,
                            CmdArgs = "license get")

    UserID <- CADOutput[grepl("Issued for: ", CADOutput, fixed = TRUE)]
    UserID <-
      unlist(regmatches(UserID, gregexpr("(?<=\\().*?(?=\\))", text = UserID, perl = TRUE)))

    Issuer <- CADOutput[grepl("Issuer: ", CADOutput, fixed = TRUE)]
    organization <-
      gsub("Issuer: ", "", Issuer, fixed = TRUE)

    MACs <- .get_MACAddresses()

    if (.Platform$OS.type == "windows") {
      os_type <- .Platform$OS.type
    } else {
      LSB <-
        readLines(list.files(path = "/etc/", pattern = "*ease", full.names = TRUE)[1])
      if (any(grepl("Ubuntu", LSB, ignore.case = TRUE))) {
        os_type <- "Ubuntu"
        if (any(grepl("22.04", LSB, fixed = TRUE))) {
          os_type <- paste0(os_type, "2204")
        }
      } else if (any(grepl("Red Hat", LSB, ignore.case = TRUE))) {
        os_type <- "RHEL"
        if (any(grepl("8.", LSB, fixed = TRUE))) {
          os_type <- paste0(os_type, "8")
        }
      } else {
        os_type <- .Platform$OS.type
      }
    }

    Log <- list(
      eventType = "track",
      eventName = "Maximum Likelihood Model Executed",
      props = list(
        product_submodule = "RsNLME",
        product_submodule_version =
          product_submodule_version,
        os_type = os_type,
        population = Model@isPopulation,
        ui_mode = ifelse(Model@isTextual, "textual", "builtin"),
        remote_run = !Host@isLocal,
        control_mode = Host@parallelMethod@method,
        structure_type = structure_type,
        residual_error = residual_error,
        number_of_covariates = number_of_covariates,
        steady_state = steady_state,
        ADDL = ADDL,
        algorithm = algorithm,
        max_ODE = max_ODE,
        stderr = stderr,
        stderr_method = stderr_method,
        run_mode = RunMode,
        analytic_gradient = EngineParams@anagrad,
        organization = organization,
        product = "Phoenix",
        phoenixEnvironment = .guess_EnvType(MACs),
        `machine name` = Sys.info()["nodename"],
        `mac addresses` = paste(MACs, collapse = ","),
        `User Id` = UserID,
        `Client Event Time` = format(Sys.time(), "%x %X")
      )
    )

    LogJSON <- jsonlite::toJSON(Log, auto_unbox = TRUE)

    CADOutput <-
      .runCmd_LicensingTool(LicensingTool = LicensingTool,
                            WorkingDir = WorkingDir,
                            CmdArgs = paste0("event report --json=",
                                             shQuote(LogJSON, type = "cmd")),
                            wait = LogDebug)
  },
  error = function(cond) {
    if (LogDebug) {
      stop(conditionMessage(cond))
    }
  },
  warning = function(cond) {
    if (LogDebug) {
      warning(conditionMessage(cond))
    }
  })
}

.get_MACAddresses <- function() {
  MACPattern <-
    "((([0-9a-fA-F]{2})[ :-]){5}[0-9a-fA-F]{2})|(([0-9a-fA-F]){6}[:-]([0-9a-fA-F]){6})|([0-9a-fA-F]{12})"

  if (.Platform$OS.type == "windows") {
    MACOutputWindows <-
      system(paste(
        Sys.getenv("COMSPEC"),
        "/c getmac"
      ),
      intern = TRUE)

    MACOutputWindows <-
      unlist(strsplit(MACOutputWindows, ' |"'))

    MACs <-
      MACOutputWindows[grepl(paste0("^", MACPattern, "$"),
                             MACOutputWindows)]

  } else {
    MACOutputLinux <-
      system(paste(
        "ip link"
      ),
      intern = TRUE)

    MACs <-
      regmatches(MACOutputLinux,
                 gregexpr(paste0("(?<=link/ether )", MACPattern), MACOutputLinux, perl = TRUE))
  }

  unique(unlist(MACs[lengths(MACs) > 0 & MACs != ""]))
}

.guess_EnvType <- function(MACs) {
  if (is.null(MACs)) {
    return("Desktop Deployment")
  }

  MACs <- gsub("\\W", "", MACs)

  VirtualEnvs <- c(
    "005056",
    "000C29",
    "000569",
    "001C14",
    "00155D",
    "080027",
    "0003FF",
    "00163E",
    "525400",
    "545200",
    "020F4F"
  )

  VirtualPattern <- paste0("^", paste0("(", VirtualEnvs, ")", collapse = "|"))

  if (any(grepl(VirtualPattern, MACs))) {
    "Virtual Deployment"
  } else {
    "Desktop Deployment"
  }
}


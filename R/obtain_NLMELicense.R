#' Obtain NLME License
#'
#' This function attempts to authenticate and obtain an NLME license using the
#' specified installation directory and licensing tool.
#'
#' @param InstallDir A character string specifying the directory
#'   where the NLME Engine is installed e.g., \code{INSTALLDIR} environment
#'   variable. The \code{cadlicensingtool} executable is expected to be located
#'   within this directory, or within a subdirectory specified by the
#'   \code{PML_BIN_DIR} environment variable.
#' @param ForceAuth A logical value indicating whether to force
#'   re-authentication even if already authenticated. Default is \code{FALSE}.
#' @param ForceLicenseGet A logical value indicating whether to force obtaining
#'   the license even if already licensed. Default is \code{FALSE}.
#' @param verbose A logical value indicating whether to print verbose output.
#'   Default is \code{getOption("verbose")}.
#' @return A logical value indicating whether the license was successfully
#'   obtained.
#' @details This function checks for the presence of the necessary
#'   \code{appsettings.json} file as indicated by the \code{CAD_CONFIG_FILE} environment variable,
#'   runs the licensing tool to authenticate the user, and attempts to obtain
#'   an NLME license. It prints detailed messages if the \code{verbose}
#'   parameter is set to \code{TRUE}.
#' @examples
#' \donttest{
#' INSTALLDIR <- Sys.getenv("INSTALLDIR")
#' if (INSTALLDIR == "") INSTALLDIR <- "C:/Program Files/Certara/NLME_Engine"
#' result <- obtain_NLMELicense(INSTALLDIR, verbose = TRUE)
#' if (result) {
#'   message("License obtained successfully!")
#' } else {
#'   message("Failed to obtain license.")
#' }
#' }
#' @export
obtain_NLMELicense <-
  function(InstallDir = Sys.getenv("INSTALLDIR"),
           ForceAuth = FALSE,
           ForceLicenseGet = FALSE,
           verbose = getOption("verbose")) {
    stopifnot(Certara.NLME8::checkInstallDir(InstallDir))

    PML_BIN_DIR <- Sys.getenv("PML_BIN_DIR")
    if (.Platform$OS.type == "windows" &&
        verbose &&
        PML_BIN_DIR != "") {
      cat("PML_BIN_DIR env.variable is set and will be used.\n")
    }

    if (verbose &&
        Sys.getenv("CAD_OPENID_URL") != "") {
      cat("CAD_OPENID_URL env.variable is set and will be used.\n")
    }

    WorkingDir <- path.expand(file.path(InstallDir, PML_BIN_DIR))

    if (!.check_CAD_CONFIG_FILE(WorkingDir)) {
      return(FALSE)
    }

    LicensingTool <- file.path(WorkingDir, "cadlicensingtool")
    if (.Platform$OS.type != "windows") {
      Sys.chmod(LicensingTool, mode = "777")
    }

    CmdOutputAuthPrint <-
      .runCmd_LicensingTool(LicensingTool = LicensingTool,
                            WorkingDir = WorkingDir,
                            CmdArgs = "auth print")

    if (verbose) {
      cat("auth print output:", CmdOutputAuthPrint, sep = "\n")
    }

    AuthPresented <-
      .check_Auth(CmdOutputAuthPrint)

    # if authenticated/licensed, there should be 2 rows
    if (AuthPresented &&
        ForceAuth &&
        verbose) {
      cat("The user is already logged in, but logging in again due to ForceAuth flag:",
          sep = "\n")
    }

    if (!AuthPresented ||
        ForceAuth) {
      CmdOutputAuthLogin <-
        .runCmd_LicensingTool(
          LicensingTool = LicensingTool,
          WorkingDir = WorkingDir,
          CmdArgs = "auth login"
        )

      if (length(CmdOutputAuthLogin) == 0 &&
          length(attr(CmdOutputAuthLogin, "errmsg")) > 0) {
        CmdOutputAuthLogin <- attr(CmdOutputAuthLogin, "errmsg")
      }

      if (verbose) {
        cat("auth login output:", CmdOutputAuthLogin,
            sep = "\n")
      }
    } else {
      CmdOutputAuthLogin <- CmdOutputAuthPrint
    }

    AuthSuccessful <-
      .check_Auth(CmdOutputAuthLogin)

    if (!AuthSuccessful) {
      if (verbose) {
        WarningMessage <- "Cannot obtain the license."
      } else {
        WarningMessage <-
          paste(
            "Cannot obtain the license. The cadlicensingtool reported:",
            CmdOutputAuthLogin,
            collapse = "\n"
          )
      }

      warning(WarningMessage, immediate. = TRUE)

      return(FALSE)
    }

    CmdOutputLicensePrint <-
      .runCmd_LicensingTool(LicensingTool = LicensingTool,
                            WorkingDir = WorkingDir,
                            CmdArgs = "license print")

    if (verbose) {
      cat("license print output:\n",
          CmdOutputLicensePrint,
          sep = "\n")
    }

    NLMELicensed <-
      .check_Licensed(CmdOutputLicensePrint)

    if (!NLMELicensed ||
        ForceLicenseGet) {
      if (NLMELicensed && ForceLicenseGet) {
        message("`ForceLicenseGet` flag forced license update.")
      }

      CmdOutputLicenseGet <-
        .runCmd_LicensingTool(
          LicensingTool = LicensingTool,
          WorkingDir = WorkingDir,
          CmdArgs = "license get --now"
        )

      if (verbose) {
        cat("license get output:\n", CmdOutputLicenseGet, sep = "\n")
      }
    } else {
      CmdOutputLicenseGet <- CmdOutputLicensePrint
    }

    LicenseGetSuccessful <-
      .check_Licensed(CmdOutputLicenseGet)

    if (LicenseGetSuccessful) {
      LicenseStatus <-
        CmdOutputLicenseGet[grepl("License status", CmdOutputLicenseGet)]
      UserEmail <-
        CmdOutputLicenseGet[grepl("@", CmdOutputLicenseGet, fixed = TRUE)]
      UserEmail <- strsplit(UserEmail, " ")[[1]]
      UserEmail <- UserEmail[grepl("@", UserEmail, fixed = TRUE)]
      message(LicenseStatus,
              "\nUser's Email: ", UserEmail)
      message(CmdOutputLicenseGet[grepl("Issuer: ", CmdOutputLicenseGet, fixed = TRUE)])

      ExpireDateTime <-
        CmdOutputLicenseGet[grepl("- key: nlme, expiration:", CmdOutputLicenseGet, fixed = TRUE)]
      ExpireDateTime <- strsplit(ExpireDateTime, " ")[[1]]
      ExpireDateTime <-
        strsplit(ExpireDateTime[length(ExpireDateTime)], "T")[[1]][1]
      ExpireDateTime <-
        gsub("Expires at: ", "", ExpireDateTime, fixed = TRUE)

      RefreshDateTime <-
        CmdOutputLicenseGet[grepl("Expires at:", CmdOutputLicenseGet)]
      RefreshDateTime <-
        gsub("Expires at: ", "", RefreshDateTime, fixed = TRUE)

      message(
        "License expires: ",
        ExpireDateTime,
        "\nRefresh until: ",
        RefreshDateTime,
        "\nCurrentDate: ",
        Sys.Date()
      )
    } else {
      warning(
        "Cannot obtain the license. current output is:\n",
        paste(CmdOutputLicenseGet, collapse = "\n"),
        immediate. = TRUE
      )
    }

    LicenseGetSuccessful
  }

#' Remove NLME License
#'
#' This function attempts to remove an NLME license using the specified
#' installation directory and licensing tool.
#'
#' @inheritParams obtain_NLMELicense
#' @return A logical value indicating whether the license information was
#'   successfully removed.
#' @details The function checks for the presence of the necessary
#' `appsettings.json` file in the specified directory or the CAD config file
#' specified by the `CAD_CONFIG_FILE` environment variable, runs the licensing
#' tool to log out the user, and attempts to remove the NLME license.
#' @examples
#' \donttest{
#' INSTALLDIR <- Sys.getenv("INSTALLDIR")
#' if (INSTALLDIR == "") INSTALLDIR <- "C:/Program Files/Certara/NLME_Engine"
#' if (FALSE) { # to prevent unintended logout
#'   result <- remove_NLMELicense(INSTALLDIR)
#' }
#' }
#' @export
remove_NLMELicense <-
  function(InstallDir = Sys.getenv("INSTALLDIR")) {
    stopifnot(Certara.NLME8::checkInstallDir(InstallDir))

    PML_BIN_DIR <- Sys.getenv("PML_BIN_DIR")

    WorkingDir <- path.expand(file.path(InstallDir, PML_BIN_DIR))

    if (!.check_CAD_CONFIG_FILE(WorkingDir)) {
      return(FALSE)
    }

    LicensingTool <- file.path(WorkingDir, "cadlicensingtool")
    if (.Platform$OS.type != "windows") {
      Sys.chmod(LicensingTool, mode = "777")
    }

    CmdOutputAuthPrint <-
      .runCmd_LicensingTool(
        LicensingTool = LicensingTool,
        WorkingDir = WorkingDir,
        CmdArgs = "auth print"
      )

    AuthFound <- !"Access token: missing" %in% CmdOutputAuthPrint
    if (!AuthFound) {
      message("Authentification information is not found.")
    }

    CmdOutputLicensePrint <-
      .runCmd_LicensingTool(
        LicensingTool = LicensingTool,
        WorkingDir = WorkingDir,
        CmdArgs = "license print"
      )

    LicenseFound <- !"License status: Missing" %in% CmdOutputLicensePrint
    if (!LicenseFound) {
      message("License information is not found.")
    }

    if (!AuthFound && !LicenseFound) {
      message("Authentificaiton/License information is not found. Nothing to remove.")
      return(FALSE)
    }

    CmdOutputAuthLogout <-
      .runCmd_LicensingTool(
        LicensingTool = LicensingTool,
        WorkingDir = WorkingDir,
        CmdArgs = "auth logout"
      )

    if (any(grepl("License status: Missing", CmdOutputAuthLogout))) {
      message(
        "Authentification/License information removed successfully. If a complete cleanup is required, clear the browser cache."
      )
    } else {
      warning(
        "Failed to remove license information. Error reported:\n",
        paste(CmdOutputAuthLogout, collapse = "\n"),
        immediate. = TRUE
      )
      FALSE
    }

    TRUE
  }

.check_Auth <- function(CmdOutput) {
  any(grepl("Access token: expires at", CmdOutput, fixed = TRUE)) &&
    any(grepl("Refresh token: expires at", CmdOutput, fixed = TRUE))
}

.check_Licensed <- function(CmdOutput) {
  length(CmdOutput) > 2 &&
    any(grepl(
      "(License status: OK)|(License status: Disconnected)",
      CmdOutput
    )) &&
    any(
      grepl(
        "(\\- key: nlme, expiration: )\\d+\\-\\d+\\-\\d+T\\d+\\:\\d+\\:\\d+\\.\\d+Z",
        CmdOutput
      )
    )
}

.runCmd_LicensingTool <-
  function(LicensingTool,
           WorkingDir,
           CmdArgs,
           wait = TRUE) {
    CmdArgs <- paste(CmdArgs, collapse = " ")

    if (.Platform$OS.type == "windows") {
      system(
        paste(
          Sys.getenv("COMSPEC"),
          "/c pushd",
          shQuote(WorkingDir),
          "&&",
          shQuote(LicensingTool),
          CmdArgs
        ),
        intern = wait,
        wait = wait,
        ignore.stdout = !wait,
        ignore.stderr = !wait
      )
    } else {
      system(
        paste(
          "(cd",
          shQuote(WorkingDir),
          ";",
          shQuote(LicensingTool),
          CmdArgs,
          ";)"
        ),
        intern = wait,
        wait = wait,
        ignore.stdout = !wait,
        ignore.stderr = !wait
      )
    }
  }

.check_CAD_CONFIG_FILE <- function(WorkingDir) {
  CAD_CONFIG_FILE <- Sys.getenv("CAD_CONFIG_FILE")
  if (CAD_CONFIG_FILE != "") {
    CAD_CONFIG_FILE_exists <- file.exists(CAD_CONFIG_FILE)
    if (!CAD_CONFIG_FILE_exists) {
      warning(
        "The specified env.var. CAD_CONFIG_FILE does not point to the valid config:\n",
        CAD_CONFIG_FILE,
        immediate. = TRUE,
        call. = TRUE
      )
    }
  } else {
    CAD_CONFIG_FILE <- file.path(WorkingDir, "appsettings.json")
    CAD_CONFIG_FILE_exists <- file.exists(CAD_CONFIG_FILE)
    if (!CAD_CONFIG_FILE_exists) {
      warning(
        "The 'appsettings.json' file containing CAD connection information is missing in ",
        WorkingDir,
        ". Please check the installation directory.",
        immediate. = TRUE
      )
    }
  }


  CAD_CONFIG_FILE_exists
}

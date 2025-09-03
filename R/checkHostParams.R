#' Check Host Parameters
#'
#' Checks NLME Parallel Host object for correct settings for GCC, NLME Installation, MPI and Root directories.
#'
#' @param obj NLME Parallel Host to be checked
#' @return \code{TRUE} if all checks are successful, otherwise \code{FALSE}
#' @examples
#' \dontrun{
#'
#' # multicore
#' multicoreHost <- hostParams(
#'   parallelMethod = "Multicore",
#'   hostName = "local_multicore",
#'   numCores = 4
#' )
#' checkHostParams(multicoreHost)
#' }
#'
#' @keywords NLME internal
#' @export
checkHostParams <- function(obj, verbose = FALSE) {
  if (!obj@isLocal) {
    warning("please start the check on remote host directly")
    return(FALSE)
  }

  checkGCC_res <- Certara.NLME8::checkGCC()
  checkInstallDir_res <-
    Certara.NLME8::checkInstallDir(obj@installationDirectory)
  checkRootDir_res <- Certara.NLME8::checkRootDir(obj)
  if (checkInstallDir_res) {
    checkLicenseFile_res <-
      Certara.NLME8::checkLicenseFile(installDir = obj@installationDirectory,
                                      verbose = verbose)
  } else {
    warning(
      "license is not checked since the directory with NLME Executables is not correct",
      immediate. = TRUE
    )

    checkLicenseFile_res <- FALSE
  }

  checkMPISettings_res <- Certara.NLME8::checkMPISettings(obj)

  if (verbose) {
    successfullChecks <- c()
    if (checkGCC_res) {
      successfullChecks <-
        c(successfullChecks, "GCC directory is set correctly.\n")
    }
    if (checkInstallDir_res) {
      successfullChecks <-
        c(successfullChecks,
          "NLME installation directory is set correctly.\n")
    }
    if (checkRootDir_res) {
      successfullChecks <-
        c(successfullChecks,
          "Shared execution directory is set and accessible.\n")
    }
    if (checkLicenseFile_res) {
      successfullChecks <-
        c(successfullChecks,
          "Licensing checks passed successfully.\n")
    }
    if (checkMPISettings_res) {
      successfullChecks <-
        c(successfullChecks,
          "MPI directory is set and available for job execution.\n")
    }

    cat(successfullChecks, sep = "")
  }

  return(
    all(
      checkGCC_res,
      checkInstallDir_res,
      checkRootDir_res,
      checkLicenseFile_res,
      checkMPISettings_res
    )
  )

}

#' Create an NLME Parallel Host Configuration
#'
#' This helper function simplifies the creation and configuration of an
#' `NlmeParallelHost` object, which defines the environment for running NLME
#' jobs.
#'
#' @param sharedDirectory `character`. The directory where temporary run folders
#'   are created. Defaults to the current working directory.
#' @param installationDirectory `character`. The directory containing NLME
#'   libraries/scripts. Defaults to the `INSTALLDIR` environment variable.
#' @param hostName `character`. A display name for the host. Defaults to the
#'   system's network name (from `Sys.info()[["nodename"]]`).
#' @param machineName `character`. The IP address or network name of the host.
#'   Defaults to `"127.0.0.1"`.
#' @param hostType `character`. The host operating system. Defaults to the
#'   current OS (`Sys.info()[["sysname"]]`). While `"windows"` or `"linux"` are
#'   valid, for remote Linux hosts the following are officially supported:
#'   `"RHEL"` (for RHEL 8 and 9) and `"UBUNTU"` (for Ubuntu 22.04 and 24.04).
#'   Specifying one of these values correctly sets the `PML_BIN_DIR` variable.
#' @param numCores `numeric`. The number of CPU cores to utilize. Defaults to
#'   `4`.
#' @param parallelMethod `character`. The parallel execution method. Options
#'   include: `"None"`, `"Multicore"`, `"LOCAL_MPI"`, `"SGE"`, `"SGE_MPI"`,
#'   `"TORQUE"`, `"TORQUE_MPI"`, `"LSF"`, `"LSF_MPI"`, `"SLURM"`, `"SLURM_MPI"`.
#'   Defaults to `"LOCAL_MPI"`.
#' @param userName `character`. The username for remote host authentication.
#' @param privateKeyFile `character`. The path to an SSH private key file for
#'   remote authentication. See `ssh::ssh_connect()` for more details.
#' @param userPassword `character` or `function`. The password or a callback
#'   function for remote authentication. See `ssh::ssh_connect()` for details.
#' @param scriptPath `character`. The path to a script to run on a remote host
#'   before the main job starts. Ignored for local runs.
#' @param rLocation `character`. The path to the `Rscript` executable on a
#'   remote host. Ignored for local runs.
#' @param isLocal `logical`. Set to `TRUE` for a local host or `FALSE` for a
#'   remote host. Defaults to `TRUE`.
#'
#' @return An `NlmeParallelHost` object configured with the specified
#'   parameters.
#'
#' @examples
#' host <- hostParams(sharedDirectory = tempdir(),
#'                    parallelMethod = "LOCAL_MPI",
#'                    hostName = "Local",
#'                    numCores = 4)
#'
#' @keywords NLME NlmeParallelHost
#' @md
#' @export hostParams
#'
hostParams <- function(sharedDirectory,
                       installationDirectory = Sys.getenv("INSTALLDIR"),
                       hostName = Sys.info()[["nodename"]],
                       machineName = "127.0.0.1",
                       hostType = Sys.info()[["sysname"]],
                       numCores = 4,
                       parallelMethod = "LOCAL_MPI",
                       userName = "",
                       privateKeyFile = NULL,
                       userPassword = "",
                       scriptPath = "",
                       rLocation = "",
                       isLocal = TRUE) {
  NlmeUserAuthenticationArg <- NlmeUserAuthentication(userName,
                                                      privateKeyFile,
                                                      userPassword)

  parallelMethodArg <- NlmeParallelMethod(parallelMethod)

  if (missing(sharedDirectory)) {
    sharedDirectory <-
      normalizePath(".", winslash = "/", mustWork = FALSE)
  }

  host <- NlmeParallelHost(
    sharedDirectory = sharedDirectory,
    installationDirectory = installationDirectory,
    hostName = hostName,
    machineName = machineName,
    hostType = hostType,
    numCores = numCores,
    parallelMethod = parallelMethodArg,
    userAuthentication = NlmeUserAuthenticationArg,
    scriptPath = scriptPath,
    isLocal = isLocal
  )

  host
}

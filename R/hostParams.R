#' Initialize for NlmeParallelHost
#'
#' @param sharedDirectory Directory where temporary NLME run folder is created
#' during execution. If missing, the current working directory will be used.
#' @param installationDirectory Directory containing NLME libraries/scripts
#' @param hostName Visual name of the host (default A name by which
#'   the machine is known on the network)
#' @param machineName IP address or name of the host(default
#'   127.0.0.1)
#' @param hostType `windows` or `linux`. Current OS by default.
#' For remote runs it is possible to point
#' the distro suppported, i.e. `RHEL8` or `UBUNTU2204`. In such case the
#' corresponding `PML_BIN_DIR` variable will be created and NLME Engine libraries
#' will be looked in `installationDirectory/{$PML_BIN_DIR}`.
#' @param numCores Integer; Number of compute cores. 4 by default
#' @param parallelMethod String; Options are:
#'   `None`|`Multicore`|`LOCAL_MPI`|`SGE`|`SGE_MPI`|
#'   `TORQUE`|`TORQUE_MPI`|`LSF`|`LSF_MPI`|`SLURM_SLURM_MPI`.
#' @param userName String; How the user is identified to the remote
#'   system
#' @param privateKeyFile Path to private key file, see
#'   [ssh::ssh_connect()] for details
#' @param userPassword Either a string or a callback function for
#'   password prompt, see [ssh::ssh_connect()] for details
#' @param scriptPath a path to the script to be executed before
#'   starting Rscript within Certara.NLME8 package on the remote host. Ignored
#'   when running locally.
#' @param rLocation Path to Rscript executable on remote host;
#'   ignored on local host
#' @param isLocal Is this a local `TRUE` or remote `FALSE` host?
#'
#' @return NlmeParallelHost class instance
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
    sharedDirectory <- normalizePath(".", winslash = "/", mustWork = FALSE)
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

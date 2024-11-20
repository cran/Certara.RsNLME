#' Class initializer for NlmeRemoteExecutor
#'
#' Creates Remote Executor object class
#'
#' @slot sharedDirectory        The directory in which the run executes
#' @slot installationDirectory  Directory containing NLME libraries/scripts
#' @slot hostName               IP or name of remote host
#' @slot userAuthentication     Credential for user to log into remote system
#'
#' @export NlmeRemoteExecutor
#' @keywords internal
NlmeRemoteExecutor <-
  setClass(
    "NlmeRemoteExecutor",
    slots = c(
      sharedDirectory = "character",
      installationDirectory = "character",
      machineName = "character",
      userAuthentication = "ANY",
      session = "ANY"
    )
  )

setMethod("initialize", "NlmeRemoteExecutor",
          function(.Object,
                   sharedDirectory = "",
                   installationDirectory = "",
                   machineName = "",
                   userAuthentication = NULL) {
            .Object@sharedDirectory <- sharedDirectory
            .Object@installationDirectory <- installationDirectory
            .Object@machineName <- machineName
            .Object@userAuthentication <- userAuthentication
            .Object@session <- NULL
            .Object
          })

openSession <- function(.Object) {
  session <- ssh::ssh_connect(
    host = paste0(
      .Object@userAuthentication@userName,
      "@",
      .Object@machineName
    ),
    keyfile = .Object@userAuthentication@privateKeyFile,
    passwd = .Object@userAuthentication@userPassword,
    verbose = 0
  )
  .Object@session <- session
  .Object
}


mkremotedir <- function(.Object, jobtype) {
  dir <- file.path(.Object@sharedDirectory,
                   paste0(substr(jobtype, 1, 5),
                          format(Sys.time(), "_%y%m%d%H%M%OS2")))
  out <- ssh::ssh_exec_internal(.Object@session,
                                paste0("mkdir -p ", dir))

  dir
}

uploadFiles <- function(.Object, remoteDir, files) {
  message(paste("\nUploading files to RemoteDir", remoteDir, "\n"))
  files <- unique(files)
  ret <- ssh::scp_upload(.Object@session, files, to = remoteDir)
  ret
}


downloadFiles <-
  function(.Object,
           localDir,
           remoteDir,
           files,
           hostType) {
    # The new separate 'session' has been created just to perform the 'scp_download' call to ensure data are synchronized
    session <- ssh::ssh_connect(
      host = paste0(
        .Object@userAuthentication@userName,
        "@",
        .Object@machineName
      ),
      keyfile = .Object@userAuthentication@privateKeyFile,
      passwd = .Object@userAuthentication@userPassword,
      verbose = 0
    )

    if (hostType != "windows") {
      filesToGetRaw <-
        ssh::ssh_exec_internal(session, command = paste('ls', remoteDir))
    } else {
      filesToGetRaw <-
        ssh::ssh_exec_internal(session, command = paste('dir', remoteDir))
    }

    filesToGet <-
      unlist(strsplit(rawToChar(as.raw(
        strtoi(filesToGetRaw$stdout, 16L)
      )), "\n"))

    filesToGet <- filesToGet[!grepl("(status$)|(.sh$)", filesToGet)]

    filesExist <-
      filesToGet[grepl(paste0("(", files, ")", collapse = "|"), filesToGet, perl = TRUE)]

    for (f in filesExist) {
      message("\nStart loading ", f, " from remote host")
      ret <-
        ssh::scp_download(session,
                          file.path(remoteDir, f),
                          to = localDir,
                          verbose = FALSE)
    }

    # wait a moment to download everything
    Sys.sleep(0.1)
    try(if (hostType != "windows") {
      ssh::ssh_exec_internal(session, command = paste('rm -rf', remoteDir))
    } else {
      filesToGetRaw <-
        ssh::ssh_exec_internal(session, command = paste('rmdir /s', remoteDir))
    },
    silent = TRUE)

    ssh::ssh_disconnect(session)
    message("\n")
  }

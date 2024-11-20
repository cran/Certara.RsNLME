.get_slotHostPlatfrom <-
  function(ellipsisArgs,
           hostPlatform,
           slotName,
           default) {
    if (!is.null(ellipsisArgs[[slotName]])) {
      slotValue <- ellipsisArgs[[slotName]]
    } else if (.hasSlot(hostPlatform, slotName)) {
      slotValue <- slot(hostPlatform, slotName)
    } else {
      slotValue <- default
    }

    slotValue
  }

.load_hostPlatform <-
  function(hostPlatform,
           ellipsisArgs,
           model = NULL,
           mode = "multicore") {
    if (length(hostPlatform) > 0 &&
        length(intersect(class(hostPlatform), names(ellipsisArgs))) > 0) {
      warning(
        "Host arguments are supplied through both 'hostPlatform' argument and additional argument.",
        "\nThe ones defined through 'hostPlatform' argument will be ignored.",
        call. = FALSE,
        immediate. = TRUE
      )
    }

    sharedDirectory <-
      .get_slotHostPlatfrom(
        ellipsisArgs,
        hostPlatform,
        "sharedDirectory",
        Sys.getenv("NLME_ROOT_DIRECTORY")
      )
    installationDirectory <-
      .get_slotHostPlatfrom(ellipsisArgs,
                            hostPlatform,
                            "installationDirectory",
                            Sys.getenv("INSTALLDIR"))

    machineName <-
      .get_slotHostPlatfrom(ellipsisArgs, hostPlatform, "machineName", Sys.info()["nodename"])
    hostType <-
      .get_slotHostPlatfrom(ellipsisArgs, hostPlatform, "hostType", Sys.info()["sysname"])
    numCores <-
      .get_slotHostPlatfrom(ellipsisArgs,
                            hostPlatform,
                            "numCores",
                            ifelse(mode != "vpc", 4, 1))

    if (!is.null(ellipsisArgs$parallelMethod)) {
      if (inherits(ellipsisArgs$parallelMethod, "NlmeParallelMethod")) {
        parallelMethod <- ellipsisArgs$parallelMethod
      } else {
        parallelMethod <- NlmeParallelMethod(ellipsisArgs$parallelMethod)
      }
    } else if (!is.null(hostPlatform)) {
      parallelMethod <- hostPlatform@parallelMethod
    } else {
      if (mode == "fit") {
        parallelMethod <- NlmeParallelMethod("local_mpi")
      } else if (mode == "vpc") {
        parallelMethod <- NlmeParallelMethod("none")
      } else {
        parallelMethod <- NlmeParallelMethod("multicore")
      }
    }

    isLocal <-
      .get_slotHostPlatfrom(ellipsisArgs, hostPlatform, "isLocal", TRUE)
    rLocation <-
      .get_slotHostPlatfrom(ellipsisArgs, hostPlatform, "rLocation", "")
    scriptPath <-
      .get_slotHostPlatfrom(ellipsisArgs, hostPlatform, "scriptPath", "")
    userAuthentication <-
      .get_slotHostPlatfrom(ellipsisArgs,
                            hostPlatform,
                            "userAuthentication",
                            NlmeUserAuthentication())


    hostPlatform <-
      NlmeParallelHost(
        sharedDirectory = sharedDirectory,
        installationDirectory = installationDirectory,
        machineName = machineName,
        hostType = hostType,
        numCores = numCores,
        isLocal = isLocal,
        rLocation = rLocation,
        scriptPath = scriptPath,
        userAuthentication = userAuthentication,
        parallelMethod = parallelMethod,
      )

    hostMsg <- ""
    if (mode == "fit" && hostPlatform@isLocal) {
      suppressWarnings(MPIpresent <-
                         Certara.NLME8::checkMPISettings(hostPlatform))

      if (grepl(".*MPI$", parallelMethod@method, ignore.case = TRUE) &&
          MPIpresent &&
          model@isPopulation &&
          numCores > 1) {
        hostMsg <- paste0("Using MPI host with ",
                          numCores,
                          " cores")
      } else {
        if (!MPIpresent &&
            model@isPopulation  &&
            numCores > 1) {
          hostMsg <- paste0("MPI not found on the system.\n",
                            "Using localhost without parallelization.")
        } else if (!model@isPopulation) {
          hostMsg <- paste0("Using localhost without parallelization.")
        }

        hostPlatform <-
          NlmeParallelHost(
            sharedDirectory = sharedDirectory,
            installationDirectory = installationDirectory,
            machineName = machineName,
            hostType = hostType,
            numCores = 1,
            isLocal = isLocal,
            rLocation = rLocation,
            scriptPath = scriptPath,
            userAuthentication = userAuthentication,
            parallelMethod = NlmeParallelMethod("none"),
          )
      }


      if (hostMsg != "") {
        message(hostMsg)
      }
    } else if (mode == "vpc") {
      message("Using localhost without parallelization.")
    }

    hostPlatform
  }

#' Class initializer for NlmeParallelHost
#'
#' NLME Parallel Host object class. Class represents an NLME parallel host which
#' can either be local or remote.
#'
#' @include NlmeUserAuthentication.R
#' @include NlmeParallelMethod.R
#' @slot sharedDirectory Directory in which the run happens. If it is
#'   given as UNC path on Windows, a PS Drive will be mapped within powershell
#'   command to any free Disk letter on local machine where execution performed
#'   (not applicable to remote executions). After execution PS Drive will be
#'   removed. A warning will be given if removal is unsuccessful.
#' @slot installationDirectory Directory containing NLME libraries/scripts
#' @slot hostName Visual name of the host(default local)
#' @slot machineName IP address or name of the host(default local)
#' @slot hostType `windows` or `linux`. For remote runs it is possible to point
#' the distro suppported, i.e. `RHEL8` or `UBUNTU2204`. In such case the
#' corresponding `PML_BIN_DIR` variable will be created and NLME Engine libraries
#' will be looked in `installationDirectory/{$PML_BIN_DIR}`.
#' @slot numCores Number of compute cores
#' @slot isLocal Is this a local `TRUE` or remote `FALSE` host?
#' @slot rLocation Path to Rscript executable on remote host;
#'   ignored on local host
#' @slot scriptPath a path to the script to be executed before
#'   starting Rscript within Certara.NLME8 package on the remote host. Ignored
#'   when running locally.
#' @slot userAuthentication User credential for remote system. See
#'   [NlmeUserAuthentication()]
#' @slot parallelMethod Options are:
#'   `None|Multicore|LOCAL_MPI|SGE|SGE_MPI|TORQUE|`
#'   `TORQUE_MPI|LSF|LSF_MPI|SLURM|SLURM_MPI`. Supply argument using
#'   `NlmeParallelMethod("LOCAL_MPI")` for example.
#'
#' @examples
#' host <- hostParams(
#'   parallelMethod = "LOCAL_MPI",
#'   hostName = "local_mpi",
#'   numCores = 4
#' )
#'
#' @md
#' @keywords NLME NlmeParallelHost internal
#'
#' @export NlmeParallelHost
#'
NlmeParallelHost <-
  setClass(
    "NlmeParallelHost",
    slots = c(
      sharedDirectory = "character",
      installationDirectory = "character",
      hostName = "character",
      machineName = "character",
      hostType = "character",
      numCores = "numeric",
      isLocal = "logical",
      rLocation = "character",
      scriptPath = "character",
      userAuthentication = "NlmeUserAuthentication",
      remoteExecutor = "ANY",
      parallelMethod = "NlmeParallelMethod"
    )
  )

setMethod("initialize", "NlmeParallelHost",
          function(.Object,
                   sharedDirectory = "",
                   installationDirectory = "",
                   hostName = "",
                   machineName = "",
                   hostType = "",
                   numCores = "",
                   isLocal = TRUE,
                   rLocation = "",
                   scriptPath = "",
                   userAuthentication = NlmeUserAuthentication(),
                   remoteExecutor = NULL,
                   parallelMethod = NlmeParallelMethod()) {
            if (isLocal) {
              installationDirectory <-
                gsub("\\", "/", installationDirectory, fixed = TRUE)
              sharedDirectory <-
                gsub("\\", "/", sharedDirectory, fixed = TRUE)

              if (machineName == "") {
                machineName <- Sys.info()[["nodename"]]
              }
              if (hostType == "") {
                hostType <- Sys.info()[["sysname"]]
              }
            } else {
              if (missing(sharedDirectory)) {
                warning("With empty sharedDirectory argument the remote host may not work properly.")
              }

              if (grepl("windows", hostType, ignore.case = TRUE)) {
                hostType <- "windows"
              } else if (grepl("(linux)|(unix)", hostType, ignore.case = TRUE)) {
                hostType <- "linux"
              }
            }

            if (numCores == "") {
              numCores <- 1
            }


            .Object@sharedDirectory <- sharedDirectory
            .Object@installationDirectory <- installationDirectory
            .Object@hostName <- hostName
            .Object@machineName <- machineName
            .Object@hostType <- hostType
            .Object@numCores <- as.integer(numCores)
            .Object@parallelMethod <- parallelMethod
            .Object@rLocation <- rLocation
            .Object@scriptPath <- scriptPath
            .Object@userAuthentication <- userAuthentication
            .Object@isLocal <- isLocal
            .Object
          })

#' Print method for NlmeParallelHost class
#'
#' This method prints the information of an NlmeParallelHost object.
#'
#' @param x An NlmeParallelHost object.
#' @param ... Additional arguments passed to the print function.
#'
#'
#' @examples
#' host <- NlmeParallelHost(
#'   sharedDirectory = "~/shared/",
#'   installationDirectory = "~/nlme/",
#'   hostName = "my_host",
#'   machineName = "192.168.1.100",
#'   hostType = "RHEL8",
#'   numCores = 8,
#'   isLocal = FALSE,
#'   rLocation = "/usr/bin/R",
#'   scriptPath = "/path/to/script.R",
#'   userAuthentication = NlmeUserAuthentication(userName = "myuser", userPassword = "mypassword"),
#'   parallelMethod = NlmeParallelMethod("SGE_MPI")
#' )
#'
#' print(host)
#'
#' @md
#' @keywords internal
#' @return `NULL`
#' @export
#'
print.NlmeParallelHost <- function(x, ...) {
  cat("\n NLME Parallel Host \n ------------------------------------------- \n")
  sharedDirectory <- x@sharedDirectory
  if (x@isLocal) {
    if (x@sharedDirectory == "") {
      if (Sys.getenv("NLME_ROOT_DIRECTORY") == "") {
        cat(
          "\nShared Directory slot is empty, as well as NLME_ROOT_DIRECTORY;",
          "current working directory will be used.\n"
        )
        sharedDirectory <- normalizePath(".", winslash = "/", mustWork = FALSE)
      } else {
        cat("\nNLME_ROOT_DIRECTORY environment variable will be used.\n")
        sharedDirectory <- Sys.getenv("NLME_ROOT_DIRECTORY")
      }
    }
  } else {
    if (x@sharedDirectory == "") {
      cat(
        "\nWith empty sharedDirectory slot the remote host may not work",
        "if it is not given there as environment variable (NLME_ROOT_DIRECTORY)\n"
      )
    }
  }

  cat(paste("Shared Directory            : ", sharedDirectory), fill = TRUE)

  installationDirectory <- x@installationDirectory
  if (installationDirectory == "") {
    if (x@isLocal && Sys.getenv("INSTALLDIR") == "") {
      cat(
        "\nWith empty installationDirectory slot the host may not work",
        "if INSTALLDIR environment variable is also empty.\n"
      )
    } else if (!x@isLocal && Sys.getenv("INSTALLDIR") == "") {
      cat(
        "\nWith empty installationDirectory slot the remote host may not work",
        "if INSTALLDIR environment variable there is also empty.\n"
      )
    } else if (x@isLocal && Sys.getenv("INSTALLDIR") != "") {
      cat(
        "\nINSTALLDIR environment variable will be used since",
        "installationDirectory slot is empty.\n"
      )
      installationDirectory <- Sys.getenv("INSTALLDIR")
    }
  }

  cat(paste("NLME Executables Directory  : ", installationDirectory),
      fill = TRUE)

  cat(paste("Name of the host            : ", x@hostName), fill = TRUE)
  cat(paste("Address of the host         : ", x@machineName), fill = TRUE)
  cat(paste("Host System                 : ", x@hostType), fill = TRUE)
  cat(paste("Number of cores             : ", x@numCores), fill = TRUE)
  cat(paste("Is host local               : ", x@isLocal), fill = TRUE)
  cat(paste("R Location                  : ", x@rLocation), fill = TRUE)
  cat(paste("Remote script path          : ", x@scriptPath), fill = TRUE)
  cat(paste("Parallel method             : ", x@parallelMethod@method), fill = TRUE)

  print(x@userAuthentication)

  cat("\n ------------------------------------------- ")
}

setMethod(
  "show",
  "NlmeParallelHost",
  definition = function(object) {
    print(object)
  }
)

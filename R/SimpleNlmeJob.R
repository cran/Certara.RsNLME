#' @include NlmeParallelHost.R
#'
#' @slot jobType GENERIC/ESTIMATION_RUN/COVAR_SEARCH/PROFILE_RUN/STEPWISE_SEARCH/BOOTSTRAP/Sort_By_Column
#' @slot localDir where the data/model files are taken and the results are loaded for the local and remote runs
#' @slot remoteDir where the data/model files are taken and the results are loaded on remote host
#' @slot host local/remote job parallelization type
#' @slot argsList list of arguments for run
#' @slot argsFile file for arguments for run
#' @slot scriptFile initial script generation
#' @slot workflow workflow name
#' @slot runInBackground should the job be executed in background or not

SimpleNlmeJob <-
  setClass(
    "SimpleNlmeJob",
    slots = c(
      jobType = "character",
      localDir = "character",
      remoteDir = "character",
      host = "NlmeParallelHost",
      argsList = "list",
      argsFile = "character",
      scriptFile = "character",
      workflow = "character",
      runInBackground = "logical"
    )
  )

setMethod("initialize", "SimpleNlmeJob",
          function(.Object,
                   ...,
                   jobType = "",
                   localDir = "",
                   remoteDir = "",
                   host = NlmeParallelHost(),
                   argsList = list(),
                   argsFile = "",
                   workflow = "",
                   runInBackground = FALSE) {
            message("\nNLME Job")
            if (host@isLocal) {
              if (Certara.NLME8::checkInstallDir(host@installationDirectory)) {
                Sys.setenv("INSTALLDIR" = path.expand(host@installationDirectory))
              } else {
                warning(
                  "Given host installation directory is not valid,",
                  "\n trying to use INSTALLDIR environment variable",
                  call. = FALSE,
                  immediate. = TRUE
                )
                if (!Certara.NLME8::checkInstallDir(Sys.getenv("INSTALLDIR"))) {
                  stop("The execution without NLME executables is not possible",
                       call. = FALSE)
                } else {
                  warning(
                    "Using INSTALLDIR environment variable instead",
                    call. = FALSE,
                    immediate. = TRUE
                  )
                  argsList$install_dir <- Sys.getenv("INSTALLDIR")
                }
              }

              argsList$shared_directory <-
                .prepare_sharedDirectory(argsList, host@isLocal)

              copyTemplateFiles(host, localDir)
              .Object@localDir <- localDir
              .Object@remoteDir <- localDir
              .Object@host <- host
              .Object@argsFile <- argsFile
              .Object@argsList <- argsList
              .Object@workflow <- workflow
              .Object@scriptFile <- generateScript(.Object)
            } else {
              remoteExecutor <-
                NlmeRemoteExecutor(
                  sharedDirectory = host@sharedDirectory,
                  installationDirectory = host@installationDirectory,
                  machineName = host@machineName,
                  userAuthentication = host@userAuthentication
                )

              host@remoteExecutor <- openSession(remoteExecutor)

              .Object@localDir <- localDir
              tmpdir <- mkremotedir(host@remoteExecutor, jobType)
              .Object@remoteDir <- tmpdir
              .Object@host <- host
              .Object@argsFile <- basename(argsFile)
              .Object@argsList <- argsList
              .Object@workflow <- workflow
              .Object@scriptFile <- generateScript(.Object)
            }

            callNextMethod(
              .Object,
              ...,
              jobType = jobType,
              remoteDir = .Object@remoteDir,
              localDir = localDir,
              host = host,
              argsList = argsList,
              argsFile = argsFile,
              workflow = workflow,
              runInBackground = runInBackground
            )
          })

removeCommandFile <- function(localDir) {
  commandFile <- file.path(localDir, "nlme.cmd")
  if (file.exists(commandFile)) {
    file.remove(commandFile)
  }
}

setGeneric(
  name = "executeJob",
  def = function(.Object) {
    standardGeneric("executeJob")
  }
)

setMethod(
  "executeJob",
  "SimpleNlmeJob",
  definition = function(.Object) {
    runInBackground <- .Object@runInBackground

    removeCommandFile(.Object@localDir)
    if (.Object@host@isLocal == TRUE) {
      if (runInBackground == TRUE) {
        message(
          "Run will be performed in parallel R process; the information about execution is limited.",
          "\nPlease check the model directory (",
          .Object@localDir,
          ") for results."
        )
        parallel::mcparallel(Certara.NLME8::performParallelNLMERun(as.character(.Object@argsList)))
        print(.Object)
      } else {
        Certara.NLME8::performParallelNLMERun(as.character(.Object@argsList), reportProgress = TRUE)
        .remove_mappedDrive()
      }
    } else {
      .execute_remote(.Object)
    }
    .Object
  }
)

.execute_remote <- function(.Object) {
  Certara.NLME8::checkInstallDir(Sys.getenv("INSTALLDIR"))
  # need to check NLME Engine files
  LinuxExecsGeneral <- c("TDL5",
                         "cadlicensingtool")

  LinuxExecs <-
    list.files(
      path = Sys.getenv("INSTALLDIR"),
      pattern = paste0("^", LinuxExecsGeneral, "$", collapse = "|"),
      full.names = TRUE,
      recursive = TRUE
    )

  LinuxLibs <- c(
    "libBLAS_linux.a",
    "libcrlibm_linux.a",
    "libLAPACK_linux.a",
    "libMPI_STUB_linux.a",
    "libNLME7_FORT_linux.a",
    "libNLME7_linux.a",
    "libcrlibm_linux.a",
    "libcadlicensingclient.so.0.1.0"
  )

  LinuxLibs <-
    list.files(
      path = Sys.getenv("INSTALLDIR"),
      pattern = paste0("^", LinuxLibs, "$", collapse = "|"),
      full.names = TRUE,
      recursive = TRUE
    )

  requiredScripts <- list.files(
    path = Sys.getenv("INSTALLDIR"),
    pattern = "*\\.sh$",
    full.names = TRUE,
    recursive = TRUE
  )

  requiredHeaders <-
    list.files(
      path = Sys.getenv("INSTALLDIR"),
      pattern = "(*\\.h$)|(*\\.r$)|(*\\.tmpl)|(*\\.crt$)",
      full.names = TRUE,
      recursive = TRUE
    )


  requiredFiles <-
    c(LinuxExecs, LinuxLibs, requiredScripts, requiredHeaders)
  requiredFilesWOInstalldir <-
    gsub(Sys.getenv("INSTALLDIR"), "", requiredFiles, fixed = TRUE)
  # if [ -f lservrc ] && [ -f TDL5 ] ; then echo TRUE; else echo FALSE; fi
  Request <-
    paste("[ -f",
          shQuote(
            file.path(
              .Object@host@installationDirectory,
              requiredFilesWOInstalldir
            ),
            type = "sh"
          ),
          "]",
          collapse = " && ")
  Request <-
    paste("if",
          Request,
          "; then echo TRUE; else echo FALSE; fi")
  stat <-
    ssh::ssh_exec_internal(.Object@host@remoteExecutor@session,
                           Request)

  .check_SSHExecStatusDir(
    CreateRetValue = stat,
    relativePath = "",
    .Object = .Object
  )

  if (rawToChar(stat$stdout) == "FALSE\n") {
    message(
      "A directory ",
      .Object@host@installationDirectory,
      " not found on the server.",
      "\nWill try to create it."
    )

    # creating InstallDirNLME and subfolders
    CreateInstallDir <-
      ssh::ssh_exec_internal(.Object@host@remoteExecutor@session,
                             paste0(
                               "mkdir -p ",
                               shQuote(.Object@host@installationDirectory, type = "sh")
                             ))

    .check_SSHExecStatusDir(
      CreateRetValue = CreateInstallDir,
      relativePath = "",
      .Object = .Object
    )

    for (Directory in unique(dirname(requiredFilesWOInstalldir))) {
      requiredFilesCurrentDir <-
        requiredFilesWOInstalldir[dirname(requiredFilesWOInstalldir) == Directory]
      if (!Directory %in% c(".", "/")) {
        CreateDirectory <-
          ssh::ssh_exec_internal(.Object@host@remoteExecutor@session,
                                 paste0("mkdir -p ",
                                        shQuote(
                                          file.path(.Object@host@installationDirectory, Directory),
                                          type = "sh"
                                        )))

        .check_SSHExecStatusDir(
          CreateRetValue = CreateDirectory,
          relativePath = Directory,
          .Object = .Object
        )
      }

      ret <- uploadFiles(
        .Object = .Object@host@remoteExecutor,
        remoteDir = file.path(.Object@host@installationDirectory, Directory),
        files = file.path(Sys.getenv("INSTALLDIR"), requiredFilesCurrentDir)
      )
    }

    # make scripts and run files executable
    cmds <- paste(
      " find ",
      shQuote(.Object@host@installationDirectory, type = "sh"),
      " -name ",
      c(LinuxExecsGeneral, '"*.sh"'),
      " -exec chmod -R +x {} +",
      "|| true"
    )

    for (cmd in cmds) {
      stat <-
        ssh::ssh_exec_internal(.Object@host@remoteExecutor@session, cmd)
    }

  }

  # licensing
  if (!Certara.NLME8::checkLicenseFile(Sys.getenv("INSTALLDIR"),
                                       outputGenericInfo = FALSE)) {
    message(
      "This system currently lacks the necessary NLME Engine license",
      " for execution. The process assumes the NLME Engine is already",
      " licensed on the remote system and will utilize that license."
    )
  } else {
    if (Sys.getenv("CAD_AUTH_PATH") != "") {
      LicenseFilesPath <- Sys.getenv("CAD_AUTH_PATH")
    } else if (.Platform$OS.type == "windows") {
      LicenseFilesPath <-
        file.path(Sys.getenv("APPDATA"), "Certara/Auth/wnl/")
    } else {
      LicenseFilesPath <- path.expand("~/.certara/auth/wnl/")
    }

    LicenseFiles <-
      list.files(LicenseFilesPath, full.names = TRUE)

    RemoteAuthPath <- "~/.certara/auth/wnl/"
    CreateDirectory <-
      ssh::ssh_exec_internal(.Object@host@remoteExecutor@session,
                             paste("mkdir -p", RemoteAuthPath))

    .check_SSHExecStatusDir(
      CreateRetValue = CreateDirectory,
      relativePath = Directory,
      .Object = .Object
    )

    # need to get the absolute path
    stat <-
      ssh::ssh_exec_internal(
        .Object@host@remoteExecutor@session,
        paste("realpath", RemoteAuthPath)
      )

    RemoteAuthPath <- gsub("\n", "", rawToChar(stat$stdout))

    ret <- uploadFiles(
      .Object = .Object@host@remoteExecutor,
      remoteDir = RemoteAuthPath,
      files = LicenseFiles
    )
  }

  files <- getFilesToTransfer(.Object@localDir, .Object@argsFile)
  allFiles <- c(files, .Object@scriptFile, .Object@argsFile)
  # uploading files required for the model run
  stat <-
    uploadFiles(.Object@host@remoteExecutor, .Object@remoteDir, allFiles)
  scriptFileFullPath <-
    file.path(.Object@remoteDir, basename(.Object@scriptFile))

  cmds <- c(
    paste("chmod +x", shQuote(scriptFileFullPath, type = "sh")),
    paste0(
      "cd ",
      shQuote(.Object@remoteDir, type = "sh"),
      " ; nohup ",
      scriptFileFullPath,
      " > NlmeRemote.LOG 2>&1 &"
    )
  )

  for (cmd in cmds) {
    stat <-
      ssh::ssh_exec_internal(.Object@host@remoteExecutor@session, cmd)
  }

  if (.Object@runInBackground == FALSE) {
    waitForJobToFinish(.Object, verbose = TRUE)
    ret <- retrieveJobResults(.Object)
    ssh::ssh_disconnect(.Object@host@remoteExecutor@session)
    message("Resulted files are retrieved from remote server ",
            .Object@host@machineName,
            "\n")
  } else {
    print(.Object)
  }
}

.check_SSHExecStatusDir <-
  function(CreateRetValue, relativePath, .Object) {
    if (CreateRetValue$status != 0 ||
        length(CreateRetValue$stderr) != 0) {
      message(
        "Cannot proceed with ",
        file.path(.Object@host@installationDirectory, relativePath),
        "\nStatus code is ",
        CreateRetValue$status,
        "\nstdout output is ",
        rawToChar(CreateRetValue$stdout),
        "\nstderr output is ",
        rawToChar(CreateRetValue$stderr)
      )
      FALSE
    } else {
      TRUE
    }
  }

#' Generic function for cancelling a job
#'
#' @param .Object A job object that you want to cancel
#'
#' @return Depends on the specific methods
#'
#' @name cancelJob
#' @rdname cancelJob
#' @export
setGeneric(
  name = "cancelJob",
  def = function(.Object) {
    standardGeneric("cancelJob")
  }
)

#' @describeIn cancelJob Method for cancelling a job of the 'SimpleNlmeJob' class
#'
#' This method attempts to cancel a job of the 'SimpleNlmeJob' class.
#' If the job is running on a local host or is not running in the background,
#' it throws an error and does nothing. Otherwise, it uploads a 'STOP' command
#' to the host's remote executor.
#'
#' @param .Object A 'SimpleNlmeJob' object that you want to cancel
#'
#' @return Prints the 'SimpleNlmeJob' object after attempting to cancel the job.
#' No return value.
#'
#' @export
setMethod(
  "cancelJob",
  "SimpleNlmeJob",
  definition = function(.Object) {
    runInBackground <- .Object@runInBackground

    runInBackground <- .Object@host@hostType != "windows"

    if (.Object@host@isLocal == TRUE ||
        runInBackground == FALSE) {
      warning("Error : Cannot cancel local jobs")
      return()
    }

    stat <- NlmeJobStatus(job)

    if (stat == "InProgress") {
      cat("STOP",
          file = file.path(.Object@localDir, "nlme.cmd"),
          append = FALSE)
      files <- c("nlme.cmd")
      stat <-
        uploadFiles(.Object@host@remoteExecutor, .Object@remoteDir,
                    files)
    }
    print(.Object)
  }
)


waitForJobToFinish <- function(.Object, verbose = TRUE) {
  stat <- NlmeJobStatus(.Object)
  if (!length(stat) == 0 & stat != "") {
    message(stat)
  }
  while (stat %in% c("InProgress", "", "Running")) {
    tryCatch({
      Sys.sleep(2)
      if (verbose == TRUE) {
        print(.Object)
      }
      Sys.sleep(2)
      message("Updating NlmeJobStatus from remote host:")
      stat <- NlmeJobStatus(.Object)
    },
    error = function(ex) {
      stat <- ""
    })
  }
  stat
}


retrieveJobResults <- function(.Object) {
  files <- "^((?!.status).)*$"
  stat <- downloadFiles(
    .Object@host@remoteExecutor,
    .Object@localDir,
    .Object@remoteDir,
    files,
    .Object@host@hostType
  )

  Sys.sleep(0.1)
  try(ssh::ssh_exec_internal(.Object@host@remoteExecutor@session,
                             paste("rm -rf", .Object@remoteDir)),
      silent = TRUE)

  stat
}

setGeneric(
  name = "generateScript",
  def = function(.Object) {
    standardGeneric("generateScript")
  }
)


setMethod(
  "generateScript",
  "SimpleNlmeJob",
  definition = function(.Object) {
    {
      script <- .paste_hashToScript("", .Object@host@hostType)
      script <-
        .paste_instsharedDirsToScript(
          script,
          .Object@host@hostType,
          .Object@host@installationDirectory,
          .Object@host@sharedDirectory
        )

      cmd_args <- paste(.Object@remoteDir,
                        .Object@argsFile,
                        .Object@host@numCores,
                        .Object@workflow)

      if (.Object@host@hostType == "windows") {
        scriptName <- file.path(.Object@remoteDir, "cmd.ps1")

        cmd <-
          paste(
            "powershell -noninteractive -executionpolicy remotesigned -File $INSTALLDIR/generic_run.ps1",
            .Object@host@parallelMethod@method,
            "$INSTALLDIR",
            "$shared_directory",
            cmd_args
          )

        script <-
          paste0(script, "\n", cmd)

        script <- gsub("/", "\\", script, fixed = TRUE)
      } else {
        scriptName <- file.path(.Object@localDir, "cmd.sh")

        trap <- .paste_trap(.Object)
        script <- .paste_shRlocToScript(script,
                                        .Object@host@rLocation,
                                        .Object@host@scriptPath)

        cmd <- paste(
          shQuote("${INSTALLDIR}/generic_run.sh", type = "cmd"),
          .Object@host@parallelMethod@method,
          shQuote("${INSTALLDIR}", type = "cmd"),
          "${shared_directory}",
          cmd_args
        )

        script <-
          paste0(trap, "\n", script, "\n", cmd)
      }

      con <- file(scriptName, open = "wb")
      on.exit(close(con))
      cat(script, file = con, sep = "\n")
    }
    return(scriptName)
  }
)

#' Print generic for class Simple.NlmeJob
#'
#' Reads progress file and prints out its contents
#'
#' @param  x Handle to an NLME job
#' @inheritParams ellipsis::dots_used
#'
#' @examples
#' \donttest{
#'   print(jobHandle)
#' }
#' @export
#' @return \code{NULL}
#' @keywords internal
print.SimpleNlmeJob <- function(x, ...) {
  tryCatch({
    localDir <- attr(x, "localDir")
    xmlFile <- file.path(localDir, "progress.xml")
    if (x@host@isLocal == FALSE) {
      stat <- downloadFileWithCat(x, "progress.xml")
      con <- file(xmlFile, "w")
      writeLines(stat, con)
      close(con)
    }
    if (file.exists(xmlFile)) {
      #stuff <- XML::xmlToList(XML::xmlParse(xmlFile))
      stuff <- xml2::as_list(xml2::read_xml(xmlFile))
      message(
        "\nJob Type                 : ",
        x@jobType,
        "\nLocal Directory          : ",
        x@localDir,
        "\nParallel Protocol        : ",
        stuff$ParallelProtocol,
        "\nStatus                   : ",
        stuff$Status,
        "\nNum Jobs/Completed/Failed:",
        paste(
          stuff$NumOfSamples,
          stuff$NumOfSamplesCompleted,
          stuff$NumOfSamplesFailed,
          sep = "/"
        ),
        paste("\n", DetailInfoLines, collapse = "\n"),
        "\n------------------------------------------------\n"
      )
    }
  },
  error = function(ex) {
    return("")
  })
}


copyTemplateFiles <- function(host, localDir) {
  installDirectory <- Sys.getenv("INSTALLDIR")
  templateFile <- ""
  parallelModeString <- host@parallelMethod@method
  if (grepl("TORQUE", parallelModeString)) {
    templateFile <- "batchtools.torque.tmpl"
  } else if (grepl("SGE", parallelModeString)) {
    templateFile <- "batchtools.sge.tmpl"
  } else if (grepl("LSF", parallelModeString)) {
    templateFile <- "batchtools.lsf.tmpl"
  } else if (grepl("SLURM", parallelModeString)) {
    templateFile <- "batchtools.slurm.tmpl"
  }

  if (templateFile != "") {
    file.copy(file.path(installDirectory, templateFile), localDir)
  }
}

.paste_hashToScript <- function(script, platform) {
  hash <- Sys.getenv("NLME_HASH")
  if (hash == "") {
    return(script)
  }

  if (platform == "windows") {
    script <- paste0(script,
                     "/n$env:NLME_HASH = ", hash)
  } else {
    script <- paste0(script,
                     "/nexport NLME_HASH=", hash)
  }

  script
}

.paste_instsharedDirsToScript <- function(script,
                                          platform,
                                          installationDirectory,
                                          sharedDirectory) {
  if (platform == "windows") {
    if (installationDirectory != "") {
      script <-
        paste0(script,
               paste0("\n$INSTALLDIR=\"", installationDirectory, "\""))
    }

    script <-
      paste0(
        script,
        "\nif($INSTALLDIR -eq \"\" -or $INSTALLDIR -eq $null)",
        "\n{",
        "\n  \"Installation directory is not specified\"",
        "\n  exit 1",
        "\n}"
      )

    if (sharedDirectory != "") {
      script <-
        paste0(script, "\n$shared_directory=\"", sharedDirectory, "\"")
    }
  } else {
    if (installationDirectory != "") {
      script <- paste0(script,
                       "\nexport INSTALLDIR=\"",
                       installationDirectory,
                       "\"")
    }

    script <-
      paste0(
        script,
        '\nif [ \"${INSTALLDIR}\"X == \"X\" ]',
        '\nthen',
        '\n  echo "Installation directory is not specified"',
        '\n  exit 1',
        '\nfi'
      )

    if (platform != "linux") {
      script <- paste0(script,
                       "\nexport PML_BIN_DIR=", platform)

    }


    if (sharedDirectory != "") {
      script <-
        paste0(script,
               "\nexport shared_directory=\"",
               sharedDirectory,
               "\"")
    }
  }

  script
}

.paste_shRlocToScript <- function(script, rLocation, scriptPath) {
  if (scriptPath != "") {
    script <- paste0(
      script,
      "\nif [ -e ",
      shQuote(scriptPath, type = "cmd"),
      " ]",
      "\nthen",
      "\n  . ",
      shQuote(scriptPath, type = "cmd"),
      "\nfi"
    )
  }

  if (rLocation != "") {
    script <- paste0(script,
                     "\nexport PATH=", rLocation, ":$PATH")
  } else {
    script <- paste0(script,
                     "\nexport PATH=/usr/bin:$PATH")
  }

  script
}

.paste_trap <- function(.Object) {
  script <- paste0(
    "#!/bin/bash",
    "\nStartTime=$(date '+%b %Y %d %r')",
    "\ntrap 'catch $?' EXIT",
    "\ncatch() {",
    '\n  if [ "$1" != "0" ]; then',
    "\n    ErrorTime=$(date '+%b %Y %d %r')",
    "\n    { error=$(Error: 3>&2 2>&1 1>&3); }  2>&1",
    "\n    error=${error##*::}",
    '\n    DetailInfoLine1="Error $1 occurred on executing $BASH_COMMAND"',
    '\n    echo "<progress>',
    "\n          <MachineName>",
    .Object@host@machineName,
    "</MachineName>",
    "\n          <ParallelProtocol>",
    .Object@host@parallelMethod@method,
    "</ParallelProtocol>",
    "\n          <ModelName>",
    .Object@argsList[[1]],
    "</ModelName>",
    "\n          <StartTime>$StartTime</StartTime>",
    "\n          <EndTime>$ErrorTime</EndTime>",
    "\n          <Status>Finished</Status>",
    "\n          <NumOfSamples>1</NumOfSamples>",
    "\n          <NumOfSamplesCompleted>0</NumOfSamplesCompleted>",
    "\n          <NumOfSamplesFailed>1</NumOfSamplesFailed>",
    "\n          <DetailInfoLine1>$DetailInfoLine1</DetailInfoLine1>",
    "\n          <DetailInfoLine2>$error</DetailInfoLine2>",
    '\n    </progress>" > progress.xml',
    "\n  fi",
    "\n}"
  )
  script
}

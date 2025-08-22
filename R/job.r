getFilesToTransfer <- function(dirName, argsFile) {
  if (dirname(argsFile) == ".") {
    lines <- readLines(file.path(dirName, argsFile))
  } else {
    lines <- readLines(argsFile)
  }
  filesLine <- lines[2]
  files <- unlist(strsplit(filesLine, split = " "))
  if (dirName != ".") {
    for (indx in 1:length(files)) {
      dn <- dirname(files[indx])
      if (dn == ".") {
        files[indx] <- file.path(dirName, files[indx])
      }
    }
  }
  files
}

downloadFileWithCat <- function(job, filename) {
  ret <- ssh::ssh_exec_internal(job@host@remoteExecutor@session,
                                c(
                                  paste0(
                                    "if  [ -e ",
                                    file.path(job@remoteDir, filename),
                                    " ]; ",
                                    "then cat ",
                                    file.path(job@remoteDir, filename),
                                    "; fi"
                                  )
                                ))
  stat <- rawToChar(ret$stdout)
  return(stat)
}

#' Reads progress file and returns the status of a job
#'
#' Reads progress file and returns the status of a job
#'
#' @param job NLME job object
#'
#' @examples
#' \donttest{
#'   model <- pkmodel(
#'     parameterization = "Clearance",
#'     numCompartments = 2,
#'     data = pkData,
#'     ID = "Subject",
#'     Time = "Act_Time",
#'     A1 = "Amount",
#'     CObs = "Conc",
#'     workingDir = tempdir()
#'   )
#'
#'   params <- NlmeEngineExtraParams(
#'     method = 3,
#'     numIterations = 1
#'   )
#'
#'   host <- hostParams(
#'     sharedDirectory = tempdir(),
#'     parallelMethod = "LOCAL_MPI",
#'     hostName = "local_mpi",
#'     numCores = 4
#'   )
#'
#'   profile1 <- ProfileVar(
#'     "tvV",
#'     9.548,
#'     "-2,0"
#'   )
#'
#'   profile2 <- ProfileVar(
#'     "tvCl",
#'     0.919,
#'     "-0.5,0"
#'   )
#'
#'   profiles <- ProfileParameters(
#'     "USE_DELTA",
#'     c(profile1, profile2)
#'   )
#'
#'   job <- profilePertubate(
#'     hostPlatform = host,
#'     params = params,
#'     profiles = profiles,
#'     model = model
#'   )
#'
#'   status <- NlmeJobStatus(job)
#' }
#'
#' @export
#' @return Character. Job status messages.
#' @keywords internal
NlmeJobStatus <- function(job) {
  localDir <- job@localDir

  tryCatch({
    xmlFile <- file.path(localDir, "progress.xml")
    if (job@host@isLocal == FALSE) {
      if (file.exists(xmlFile)) {
        file.remove(xmlFile)
      }

      stat <- downloadFileWithCat(job, "progress.xml")
      if (stat != "") {
        con <- file(xmlFile, "w")
        writeLines(stat, con)
        close(con)
      }
    }

    if (file.exists(xmlFile)) {
      stuff <-
        unlist(xml2::as_list(xml2::read_xml(xmlFile)), recursive = FALSE)
      message(
        "\nNum Jobs/Completed/Failed:",
        paste(
          stuff$progress.NumOfSamples[[1]],
          stuff$progress.NumOfSamplesCompleted[[1]],
          stuff$progress.NumOfSamplesFailed[[1]],
          sep = "/"
        )
      )

      DetailInfoLines <-
        sapply(1:3,
               function(x, DetailInfoLineList) {
                 if (length(DetailInfoLineList[[x]]) > 0) {
                   return(unlist(DetailInfoLineList[[x]], recursive = TRUE))
                 } else {
                   return("")
                 }
               },
               list(
                 stuff$progress.DetailInfoLine1,
                 stuff$progress.DetailInfoLine2,
                 stuff$progress.DetailInfoLine3
               ))

      if (any(DetailInfoLines != "")) {
        output_InfoStatus(DetailInfoLines)
      } else {
        message("")
      }

      return(stuff$progress.Status[[1]])
    } else {
      if (job@host@isLocal == FALSE) {
        NlmeRemote <- file.path(localDir, "NlmeRemote.LOG")
        if (file.exists(NlmeRemote)) {
          file.remove(NlmeRemote)
        }

        stat <- downloadFileWithCat(job, basename(NlmeRemote))
        if (stat != "") {
          message("NlmeRemote.LOG output: ", stat)
        }
      }
    }
  },
  error = function(ex) {
    message("Error in NlmeJobStatus:")
    message(ex)
  })

  ""
}

output_InfoStatus <- function(DetailInfoLines) {
  DetailInfoLines <-
    DetailInfoLines[!is.na(DetailInfoLines) &
                      DetailInfoLines != "NA"]
  if (length(DetailInfoLines) < 1) {
    return("")
  }

  Info <- ""
  DF <- data.frame()
  DFnames <- ""
  for (InfoLine in DetailInfoLines) {
    if (grepl("\\W+\\t", InfoLine)) {
      DFInfoLine <- t(trimws(unlist(strsplit(
        InfoLine, split = "\\t"
      ))))
      # first "\t" does not have anything
      DFInfoLine <- DFInfoLine[,-c(1)]
      if (length(DFnames) == 1) {
        DFnames <- DFInfoLine
      } else {
        DF <- rbind.data.frame(DF, DFInfoLine)
      }
    } else {
      Info <- paste0(Info, "\n", InfoLine)
    }
  }

  message("Current status:\n",
      Info)

  if (nrow(DF) > 0) {
    colnames(DF) <- DFnames
    print(DF, row.names = FALSE, max = 10000)
  } else if (length(DFnames) > 1) {
    cat(DFnames, "\n", sep = "\t")
  }


}

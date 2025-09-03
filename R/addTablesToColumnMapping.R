#' Add tables to the mapping
#'
#' Add tables to the mapping
#'
#' @param model       Model object
#' @param Tables      Tables class objects e.g., \code{NlmeTableDef} or \code{NlmeSimTableDef}
#' @param filename    Name of column definition file
#' @param forSim      Logical; Set to \code{TRUE} if simulation tables are provided
#'
#' @examples
#' \dontrun{
#' addTablesToColumnMapping(model, simParams, Tables, filename)
#' }
#'
#' @return Table class objects specified to \code{Tables} argument.
#' @keywords internal
#' @noRd
addTablesToColumnMapping <- function(model, Tables, filename, forSim = TRUE) {
  if (length(Tables) == 0) {
    return(NULL)
  }

  workingDir <- model@modelInfo@workingDir

  filename <- file.path(workingDir, filename)

  ObsInTables <- unlist(lapply(c(Tables), function(x) {
    x@whenObs
  }))
  if (any(ObsInTables != "")) {
    if (model@isTextual) {
      # some responses could be here
      ObsTypes <- sapply(
        model@columnMapping@mapping,
        function(x) {
          name <- x@variableType$type
          name
        }
      )
      ObsTypes <- ObsTypes[!is.null(ObsTypes) & ObsTypes != ""]
      on <- names(ObsTypes[ObsTypes == "observation"])
    } else {
      on <- observationNames(model)
    }

    for (Obs in unique(ObsInTables)) {
      if (Obs != "" && !Obs %in% on) {
        warning(
          "\n'When observe' block has not valid observable:\n",
          Obs, "\nPlease check tables syntax.",
          call. = TRUE
        )
      }
    }
  }

  CovInTables <- unlist(lapply(c(Tables), function(x) {
    x@covrSet
  }))
  if (any(CovInTables != "")) {
    cn <- covariateNames(model)
    # workaround for Emax
    if (model@modelType@modelType == 3 && !model@isTimeBased && !model@isTextual) {
      cn <- c(cn, "C")
    }

    for (Cov in unique(CovInTables)) {
      if (Cov != "" && !Cov %in% cn) {
        warning(
          "\n'When covariate set' block has not valid covariate:\n",
          Cov, "\nPlease check tables syntax.",
          call. = TRUE
        )
      }
    }
  }

  DoseInTables <- unlist(lapply(c(Tables), function(x) {
    x@whenDose
  }))
  if (any(DoseInTables != "")) {
    dn <- doseNames(model)
    for (Dose in unique(DoseInTables)) {
      if (Dose != "" && !Dose %in% dn) {
        warning(
          "\n'When Dose set' block has not valid Dose:\n",
          Dose, "\nPlease check simulation tables syntax.",
          call. = TRUE
        )
      }
    }
  }

  for (Table in c(Tables)) {
    if (forSim) {
      tblString <- paste0("simtbl(file=\"", Table@name, "\"")
    } else {
      tblString <- paste0("table(file=\"", Table@name, "\"")
    }

    tString <- Table@timesList
    if (length(tString) > 0 && any(tString != "") && !Table@keepSource) {
      tString <- tString[tString != ""]
      tblString <- c(
        tblString,
        paste0("time(", paste0(tString, collapse = ", "), ")")
      )
    }

    covrSet <- Table@covrSet
    if (any(covrSet != "")) {
      covrSet <- covrSet[covrSet != ""]
      tblString <- c(
        tblString,
        paste0("covr(", paste0(covrSet, collapse = ", "), ")")
      )
    }

    whenDose <- Table@whenDose
    if (any(whenDose != "")) {
      whenDose <- whenDose[whenDose != ""]
      tblString <- c(
        tblString,
        paste0("dose(", paste0(whenDose, collapse = ", "), ")")
      )
    }

    whenObs <- Table@whenObs
    if (any(whenObs != "")) {
      whenObs <- whenObs[whenObs != ""]
      tblString <- c(
        tblString,
        paste0("obs(", paste0(whenObs, collapse = ", "), ")")
      )
    }

    variablesList <- Table@variablesList
    if (any(variablesList != "")) {
      variablesList <- variablesList[variablesList != ""]
      tblString <- c(
        tblString,
        paste0(variablesList, collapse = ", ")
      )
    }

    if (forSim && Table@timeAfterDose) {
      tblString <- c(
        tblString,
        "specvar(TAD)"
      )
    } else if (!forSim) {
      specVars <- c("TAD", "IRES", "Weight", "IWRES")
      specVarsPresent <- c(Table@timeAfterDose, Table@IRES, Table@Weight, Table@IWRES)
      if (any(specVarsPresent)) {
        tblString <- c(
          tblString,
          paste0("specvar(", paste0(specVars[specVarsPresent], collapse = ", "), ")")
        )
      }
    }

    if (Table@keepSource) {
      tblString <- c(
        tblString,
        "mode = keep"
      )
    } else if (!forSim &&
               !is.null(model@columnMapping@mapping[["time"]]) &&
               model@columnMapping@mapping[["time"]]@variableType$type == "time" &&
               Table@mode != "all") {
      # time based model should not have mode specified
      warning("mode slot is ignored for the table ", Table@name,
              " since the model is time-based.",
              call. = FALSE
      )
    } else if (!forSim &&
               any(covrSet != "") &&
               any(whenObs != "") &&
               Table@mode != "all") {
      # In the case where 'whenObs' is also specified along with 'covrSet',
      # neither mode = 'unique' nor mode = 'first' work
      warning("mode slot is ignored for the table ", Table@name,
              " since 'whenObs' is also specified along with 'covrSet'.",
              call. = FALSE
      )
    } else if (!forSim &&
               any(covrSet == "") &&
               Table@mode != "all") {
      warning("mode slot is ignored for the table ", Table@name,
              " since 'covrSet' is not specified.",
              call. = FALSE
      )
    } else if (!forSim &&
               !(!is.null(model@columnMapping@mapping[["time"]]) &&
                 model@columnMapping@mapping[["time"]]@variableType$type == "time") &&
               Table@mode != "all") {
      # mode is applicable for simple non-timebased tables only
      tblString <- c(
        tblString,
        paste0("mode = ", Table@mode)
      )
    }

    tblStringCollapsed <- paste0(tblString, collapse = ", ")

    tblStringCollapsed <- paste0(tblStringCollapsed, ")")
    cat(tblStringCollapsed, file = filename, sep = "\n", append = TRUE)
  }

  return(Tables)
}

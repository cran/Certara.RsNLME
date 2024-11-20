#' Get observation names
#'
#' Get observation model variables names.
#'
#' @param model Model object
#'
#' @examples
#' model <- pkemaxmodel(columnMap = FALSE)
#' obsnames <- observationNames(model)
#'
#' @export
#' @return Character vector of observation model variables names.
#' @keywords internal
observationNames <- function(model) {
  if (!model@isTextual) {
    ObsNames <- sapply(
      model@errorModel@effectsList,
      function(x) {
        name <- x@observeName # Here is where we get CObs from
        name
      }
    )

    ObsNames <- ObsNames[!is.null(ObsNames) & ObsNames != ""]
  } else {
    ObsNames <- list()
  }
  return(ObsNames)
}

observationExtraNames <- function(model) {
  ExtraNames <- sapply(
    model@errorModel@effectsList,
    function(x) {
      if (x@isBQL) {
        return(paste0(x@observeName, "BQL"))
      } else {
        return("")
      }
    }
  )

  ExtraNames <- ExtraNames[ExtraNames != ""]
  return(ExtraNames)
}

#' Get structural parameter names
#'
#' Returns character vector of structural parameter names for model object.
#'
#' @param model Object of class \code{NlmePmlModel}
#' @param omitEmpties Set to \code{TRUE} to omit empty names
#'
#' @examples
#' model <- pkemaxmodel(columnMap = FALSE)
#' stparms <- structuralParameterNames(model)
#'
#' @return Character vector of structural parameter names defined in model
#' @export
structuralParameterNames <- function(model, omitEmpties = TRUE) {
  stparms <- sapply(model@structuralParams, function(stparm) {
    stparmname <- stparm@name
  })

  stparms <- unlist(stparms)

  if (omitEmpties) {
    stparms <- stparms[!stparms %in% c(NA, "")]
  }

  stparms
}


#' Get secondary parameter names
#'
#' Returns character vector of secondary parameter names for model object.
#'
#' @param model Object of class \code{NlmePmlModel}
#'
#' @examples
#' model <- pkemaxmodel(columnMap = FALSE)
#' secondaryparms <- secondaryParameterNames(model)
#'
#' @return Character vector of secondary parameter names defined in model
#' @export
secondaryParameterNames <- function(model) {
  secondaryNames <-
    sapply(model@secondaryParameters, function(secondary) {
      secondaryname <- secondary@name
    })
  secondaryNames <- unlist(secondaryNames)
  secondaryNames <- secondaryNames[!secondaryNames %in% c(NA, "")]

  secondaryNames
}


fixedParameterNames <- function(model) {
  names <- c()
  for (s in model@structuralParams) {
    name <- attr(s, "name")
    names <- c(names, paste0("tv", name))
  }
  return(names)
}


randParameterNames <- function(model) {
  if (model@pkModelAttrs@isSequential) {
    sps <- attr(model, "structuralParams")
    names <- c()
    for (s in sps) {
      if (s@isSequential) {
        name <- attr(s, "name")
        names <- c(names, paste0("n", name))
      }
    }
  } else {
    sps <- attr(model, "structuralParams")
    names <- c()
    for (s in sps) {
      name <- attr(s, "name")
      names <- c(names, paste0("n", name))
    }
  }
  return(names)
}



#' Return residual effect terms available in model
#'
#' Use to return character vector of residual effect names in model object
#'
#' @param model Object of class \code{NlmePmlModel}
#'
#' @examples
#'
#' model <- pkemaxmodel(columnMap = FALSE)
#' residualEffectNames(model)
#'
#' @return Character vector of residual effect names
#' @export
#'
residualEffectNames <- function(model) {
  names <- c()
  errorModel <- attr(model, "errorModel")
  numEffects <- attr(errorModel, "numberOfEffects")
  effects <- attr(errorModel, "effectsList")
  for (e in effects) {
    name <- attr(e, "effectName")
    names <- c(names, name)
  }
  return(names)
}

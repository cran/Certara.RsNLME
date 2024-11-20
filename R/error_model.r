

ERR_ADDITIVE = 1
Additive = 1
ERR_LOG_ADDITIVE = 2
LogAdditive = 2
ERR_MULTIPLICATIVE = 3
Multiplicative = 3
ERR_ADD_MULT = 4
AdditiveMultiplicative = 4
ERR_MIX_RATIO = 5
MixRatio = 5
ERR_POWER = 6
Power = 6
ERR_CUSTOM = 7
Custom = 7

ErrorTypeNames = c(
  "Additive",
  "LogAdditive",
  "Multiplicative",
  "AdditiveMultiplicative",
  "MixRatio",
  "Power",
  "Custom"
)

#' Class represents an NLME/PML residual error model
#'
#' Class represents an NLME/PML residual error model
#'
#' @param effectName   Name of the observed variable
#' @param observeName  Name to use for observation
#' @param epsilonName  Name to use for Epsilon
#' @param errorType    Additive|LogAdditive|Multiplicative
#'                     |AdditiveMultiplicative|MixRatio|Power|Custom
#' @param frozen       Is the standard deviation frozen? (Default:FALSE)
#' @param SD           Standard deviation value
#' @param definition   Definition of a custom type or Power value
#' @param isBQL        Are there BQL values? (Default:FALSE)
#' @param bqlStatic    Value LLOQ value
#' @param dobefore     Code to execute before the observation
#' @param doafter      Code to execute after the observation
#'
#'
#' @keywords internal
NlmeResidualEffect = setClass(
  "NlmeResidualEffect",
  slots = c(
    effectName = "character",
    observeName = "character",
    epsilonName = "character",
    errorType = "numeric",
    frozen = "logical",
    SD = "numeric",
    definition = "character",
    isBQL = "logical",
    bqlStaticValue = "character",
    dobefore = "character",
    doafter = "character"
  )
)


setMethod("initialize", "NlmeResidualEffect",
          function(.Object,
                   errorType = Additive,
                   effectName = "",
                   observeName = "",
                   epsilonName = "",
                   frozen = FALSE,
                   SD = 1,
                   definition = "",
                   isBQL = FALSE,
                   bqlStaticValue = "",
                   dobefore = "",
                   doafter = "") {
            if (errorType < ERR_ADDITIVE || errorType > ERR_CUSTOM) {
              warning(paste("errorType", errorType, " is not supported!"))
              errorType = ERR_ADDITIVE
            }
            if (errorType == ERR_POWER ||
                errorType == ERR_MULTIPLICATIVE || errorType == ERR_LOG_ADDITIVE) {
              SD = 0.1
            }
            if (effectName == "")
              effectName = "C"
            if (observeName == "")
              observeName = paste0(effectName, "Obs")
            if (epsilonName == "")
              epsilonName = paste0(effectName, "Eps")
            if (epsilonName == "")
              epsilonName = paste0(effectName, "Eps")
            if ((errorType == ERR_ADD_MULT) && (definition == ""))
              definition = paste0(effectName, "MultStdev")

            .Object@dobefore = dobefore
            .Object@doafter = doafter

            .Object@errorType = errorType
            .Object@effectName = effectName
            .Object@observeName = observeName
            .Object@epsilonName = epsilonName
            .Object@frozen = frozen
            .Object@SD = SD
            .Object@definition = definition
            .Object@isBQL = isBQL
            .Object@bqlStaticValue = bqlStaticValue

            .Object
          })


#' Class represents an NLME/PML error model
#'
#' Class represents an NLME/PML error model
#'
#' @param effectsList      List of residual effects to include in the error model
#' @param numberOfEffects  Number of effects being included in the error model
#'
#'
#' @keywords internal
NlmeErrorModel = setClass("NlmeErrorModel",
                          slots = c(effectsList = "list",
                                    numberOfEffects = "numeric"))

setMethod("initialize", "NlmeErrorModel",
          function(.Object,
                   effectsList = list()) {
            .Object@effectsList = effectsList
            .Object@numberOfEffects = length(effectsList)
            .Object
          })

#' @export
print.NlmeErrorModel <- function(x, ...) {
  if (x@numberOfEffects > 0) {
    cat("\n ------------------------------------------- \n")
    cat("Observations:\n")
    for (NlmeRes in x@effectsList) {
      ErrorType <- NA
      for (ErrorTypeText in ErrorTypeNames) {
        if (NlmeRes@errorType == eval(parse(text = ErrorTypeText))) {
          ErrorType <- ErrorTypeText
        }
      }

      cat("Observation Name : ", NlmeRes@observeName, fill = TRUE)
      cat("Effect Name      : ", NlmeRes@effectName, fill = TRUE)
      cat("Epsilon Name     : ", NlmeRes@epsilonName, fill = TRUE)
      cat("Epsilon Type     : ", ErrorType, fill = TRUE)
      cat("Epsilon frozen   : ", NlmeRes@frozen, fill = TRUE)
      cat("is BQL           : ", NlmeRes@isBQL, fill = TRUE)

      if (NlmeRes@dobefore != "") {
        cat("dobefore code  : ", NlmeRes@dobefore, fill = TRUE)
      }

      if (NlmeRes@doafter != "") {
        cat("doafter code   : ", NlmeRes@doafter, fill = TRUE)
      }

      cat(" ------------------------------------------- ", fill = TRUE)
    }
  }

  invisible(x)
}


# print.NlmeResidualEffect <-function(x, ...){
#   cat("Name             : ", x@effectName, fill = TRUE)
#   cat("Type             : ", ErrorTypeNames[x@errorType], fill = TRUE)
#   cat("Observation Name : ", x@observeName, fill = TRUE)
#   cat("Is Frozen        : ", x@frozen, fill = TRUE)
# }

#' Add to the NLME Error model
#'
#' Add to the NLME Error model
#'
#' @param model          Model object
#' @param effectsList    List of effects
#' @keywords internal
addToErrorModel <- function(model, effectsList) {
  errorModel = model@errorModel
  origList = errorModel@effectsList
  len = length(origList)
  for (i in 1:length(effectsList)) {
    origList[[len + i]] = effectsList[[i]]
  }
  errorModel@effectsList = origList
  errorModel@numberOfEffects = length(origList)
  model@errorModel = errorModel
  model
}



Logit = 1
Probit = 2
LogLog = 3
Cloglog = 4
Custom = 5


.map_BQL <-
  function(.Object,
           predName,
           predNameValue,
           isBQL,
           mappedBQLcol = NULL,
           staticLLOQ = NULL) {
    mdata <- as.data.frame(.Object@inputData)
    BQLterm <- paste0(predName, "ObsBQL")
    if (predName == predNameValue) {
      if (isBQL) {
        if (!is.null(mappedBQLcol)) {
          .check_column_mappings(BQLterm, data = mdata)
          if (!is.null(staticLLOQ)) {
            warning(
              "the value in the observation row corresponding to '",
              BQLterm,
              "' column having non-zero value will be used as the LLOQ and will override the static LLOQ"
            )
          }
        } else {
          mappedBQLcol <- "?"
          if (is.null(staticLLOQ)) {
            warning(
              "'staticLLOQ' argument not provided, specify' ",
              BQLterm,
              "' using `colMapping()`"
            )
          }
        }

        mappedColumn(.Object, BQLterm) <- mappedBQLcol
        .Object@columnMapping@mapping[[BQLterm]]@variableType$type <-
          "bql"
      } else {
        .Object@columnMapping@mapping[[BQLterm]] <- NULL
      }
    }
    .Object
  }

#' Assign residual error model to model object
#'
#' Use to change or update residual error model for model object
#'
#' @param .Object      Model object
#' @param predName  	 Name of the predicted variable as returned in \code{\link{residualEffectNames}}.
#' @param errorType	   Options are \code{"Additive"}, \code{"LogAdditive"}, \code{"Multiplicative"}, \code{"AdditiveMultiplicative"}, \code{"MixRatio"}, \code{"Power"}.
#' @param SD	         Value for the standard deviation of the residual error variable.
#' @param isFrozen     Set to \code{TRUE} to freeze the standard deviation to the value specified for \code{SD}.
#' @param isBQL        Set to \code{TRUE} if BQL values present in the observation data.
#' @param staticLLOQ   Optional LLOQ value if \code{isBQL = TRUE}
#' @param EObsBQL      Column mapping argument that represents the input dataset column that contains the BQL flag for observation values corresponding to \code{EObs}. Only applicable to \code{isBQL = TRUE}.
#' @param CObsBQL      Column mapping argument that represents the input dataset column that contains the BQL flag for observation values corresponding to \code{CObs}. Only applicable to \code{isBQL = TRUE}.
#' @param C1ObsBQL     Column mapping argument that represents the input dataset column that contains the BQL flag for observation values corresponding to \code{C1Obs}. Only applicable to \code{isBQL = TRUE}.
#' @param A0ObsBQL     Column mapping argument that represents the input dataset column that contains the BQL flag for observation values corresponding to \code{AObs}. Only applicable to \code{isBQL = TRUE}.
#' @param exponent     Value of exponent. Only applicable to \code{errorType = "Power"}.
#'
#' @return Modified \code{NlmePmlModel} object
#' @examples
#' model <- pkindirectmodel(indirectType = "LimitedInhibition", isBuildup = FALSE,
#'  data = pkpdData, ID = "ID", Time = "Time", A1 = "Dose", CObs = "CObs", EObs = "EObs")
#'
#' residualEffectNames(model)
#'
#' # Change error type to "Multiplicative" and value of SD to 0.1 for "E"
#' model <- residualError(model, predName = "E", errorType = "Multiplicative", SD = 0.1)
#'
#' # Change error type to "Power", value of SD to 0.15, and set exponent = 2 for "C"
#' model <- residualError(model, predName = "C", errorType = "Power", SD = 0.15, exponent = 2)
#'
#' @export
residualError <- function(.Object,
                          predName = "C",
                          errorType = NULL,
                          SD = NULL,
                          isFrozen = FALSE,
                          isBQL = FALSE,
                          staticLLOQ = NULL,
                          EObsBQL = NULL,
                          CObsBQL = NULL,
                          C1ObsBQL = NULL,
                          A0ObsBQL = NULL,
                          exponent = NULL) {
  if (!inherits(.Object, "NlmePmlModel")) {
    stop("Object must be of class 'NlmePmlmodel'")
  }

  if (.Object@isTextual) {
    stop("`residualError()` cannot be used for edited or textual models")
  }

  if (is.null(errorType)) {
    for (i in seq_along(.Object@errorModel@effectsList)) {
      if (predName == .Object@errorModel@effectsList[[i]]@effectName) {
        error <- .Object@errorModel@effectsList[[i]]@errorType
      }
    }
  } else {
    if (errorType == "Additive") {
      error <- Additive
    } else if (errorType == "LogAdditive") {
      error <- LogAdditive
    } else if (errorType == "Multiplicative") {
      error <- Multiplicative
    } else if (errorType == "AdditiveMultiplicative") {
      error <- AdditiveMultiplicative
    } else if (errorType == "MixRatio") {
      error <- MixRatio
    } else if (errorType == "Power") {
      error <- Power
    } else {
      stop(
        paste0(
          "'",
          errorType,
          "' not found, errorType must be one of 'Additive', 'LogAdditive', 'Multiplicative', 'AdditiveMulitiplicative', 'MixRatio', 'Power'"
        )
      )
    }
  }

  if (error == ERR_POWER ||
      error == ERR_MULTIPLICATIVE || error == ERR_LOG_ADDITIVE) {
    if (is.null(SD)) {
      SD = 0.1
    }
  }

  eMod <- .Object@errorModel@effectsList
  effectNames <- vector(mode = "list", length = length(eMod))

  for (i in seq_along(eMod)) {
    effectNames[[i]] <- eMod[[i]]@effectName
  }

  `%notin%` <- Negate(`%in%`)

  if (predName %notin% effectNames) {
    stop(paste0(
      "`predName` = '",
      predName,
      "' not found in existing error model"
    ))
  }

  if (error == Power) {
    residualEffect(.Object, predName) = c(
      errorType = error,
      SD = as.character(SD),
      frozen = isFrozen,
      isBQL = isBQL,
      definition = as.character(exponent),
      bqlStaticValue = as.character(staticLLOQ)
    )
  } else {
    residualEffect(.Object, predName) = c(
      errorType = error,
      SD = as.character(SD),
      frozen = isFrozen,
      isBQL = isBQL,
      bqlStaticValue = as.character(staticLLOQ)
    )
  }

  mtype <- .Object@modelType@modelType
  pktype <- .Object@pkModelAttrs@parameterization@paramType
  hasElimCpt <-  .Object@pkModelAttrs@hasEliminationComp

  mdata <- as.data.frame(.Object@inputData)

  if (length(mdata) > 0) {
    if (isBQL) {
      if (pktype > 2 &&
          is.null(C1ObsBQL) && is.null(staticLLOQ) && predName == "C1") {
        warning("missing `C1ObsBQL` argument for model parameterization 'Macro' or 'Macro1'")
      }
      if (pktype < 3 &&
          is.null(CObsBQL) && is.null(staticLLOQ) && predName == "C") {
        warning("missing `CObsBQL` argument for model parameterization 'Micro' or 'Clearance'")
      }
      if (mtype != Pk &&
          is.null(EObsBQL) && is.null(staticLLOQ) && predName == "E") {
        warning("missing `EObsBQL` argument specifying column in dataset")
      }
      if (pktype < 3 &&
          is.null(A0ObsBQL) &&
          hasElimCpt && is.null(staticLLOQ) && predName == "A0") {
        warning("missing `A0ObsBQL` argument specifying column in dataset")
      }
    }

    .Object <-
      .map_BQL(
        .Object,
        predName,
        predNameValue = "C1",
        isBQL,
        mappedBQLcol = C1ObsBQL,
        staticLLOQ
      )

    .Object <-
      .map_BQL(
        .Object,
        predName,
        predNameValue = "C",
        isBQL,
        mappedBQLcol = CObsBQL,
        staticLLOQ
      )

    .Object <-
      .map_BQL(
        .Object,
        predName,
        predNameValue = "E",
        isBQL,
        mappedBQLcol = EObsBQL,
        staticLLOQ
      )

    .Object <-
      .map_BQL(
        .Object,
        predName,
        predNameValue = "A0",
        isBQL,
        mappedBQLcol = A0ObsBQL,
        staticLLOQ
      )
  }


  #Check if any existing structural parameters exist for selected error model and remove
  structuralParams <- .Object@structuralParams


  if (predName == "C") {
    for (i in seq_along(structuralParams)) {
      if ("CMultStdev" %in% structuralParams[[i]]@name) {
        structuralParams[[i]] <- NULL
        break
      }
    }
    for (i in seq_along(structuralParams)) {
      if ("CMixRatio" %in% structuralParams[[i]]@name) {
        structuralParams[[i]] <- NULL
        break
      }
    }
  }

  if (predName == "C1") {
    for (i in seq_along(structuralParams)) {
      if ("C1MixRatio" %in% structuralParams[[i]]@name) {
        structuralParams[[i]] <- NULL
        break
      }
    }
    for (i in seq_along(structuralParams)) {
      if ("C1MultStdev" %in% structuralParams[[i]]@name) {
        structuralParams[[i]] <- NULL
        break
      }
    }
  }

  if (predName == "E") {
    for (i in seq_along(structuralParams)) {
      if ("EMixRatio" %in% structuralParams[[i]]@name) {
        structuralParams[[i]] <- NULL
        break
      }
    }
    for (i in seq_along(structuralParams)) {
      if ("EMultStdev" %in% structuralParams[[i]]@name) {
        structuralParams[[i]] <- NULL
        break
      }
    }
  }

  if (predName == "A0") {
    for (i in seq_along(structuralParams)) {
      if ("A0MultStdev" %in% structuralParams[[i]]@name) {
        structuralParams[[i]] <- NULL
        break
      }
    }
    for (i in seq_along(structuralParams)) {
      if ("A0MixRatio" %in% structuralParams[[i]]@name) {
        structuralParams[[i]] <- NULL
        break
      }
    }
  }



  .Object@structuralParams <- structuralParams

  if (error == ERR_ADD_MULT || error == ERR_MIX_RATIO) {
    if (error == ERR_ADD_MULT) {
      if (pktype == Micro || pktype == Clearance) {
        def <- "CMultStdev"
      } else {
        def <- "C1MultStdev"
      }

      if (mtype != Pk && predName == "E") {
        def <- "EMultStdev"
        for (i in seq_along(structuralParams)) {
          if (def %in% structuralParams[[i]]@name) {
            structuralParams[[i]] <- NULL
          }
        }
      }
    }

    if (error == ERR_MIX_RATIO) {
      if (pktype == Micro || pktype == Clearance) {
        def <- "CMixRatio"
      } else {
        def <- "C1MixRatio"
      }

      if (mtype != Pk && predName == "E") {
        def <- "EMixRatio"
        for (i in seq_along(structuralParams)) {
          if (def %in% structuralParams[[i]]@name) {
            structuralParams[[i]] <- NULL
          }
        }
      }
    }

    if (predName == "A0") {
      #added
      if (error == ERR_MIX_RATIO) {
        def <- "A0MixRatio"
      }
      if (error == ERR_ADD_MULT) {
        def <- "A0MultStdev"
      }
    }

    fixEffect <- paste0("tv", def)
    ranEffect <- paste0("n", def)

    param <- NlmeStructuralParameter(
      name = def,
      fixedEffName = fixEffect,
      randomEffName = ranEffect,
      hasRandomEffect = FALSE,
      style = STP_PRODUCT,
      initialValue = "1",
      lowerBound = "",
      upperBound = "",
      units = "",
      isFrozen = FALSE,
      isSequential = FALSE
    )

    structuralParams <- c(structuralParams, param)
    .Object@structuralParams <- structuralParams
  }
  .Object <- generatePMLModel(.Object)

  return(.Object)
}

#' Creates a mapping between model variable and dataset columns
#'
#' Creates a mapping between model variable and dataset columns
#'
#' @param model      Model object
#' @param inputData  Input data set
#'
#' @examples
#' \donttest{
#' createInitialMapping(model, InputData)
#' }
#' @keywords internal
#' @noRd
createInitialMapping <- function(model, inputData) {
  map <- list()

  if (model@isTextual) {
    map <- model@columnMapping@mapping
  } else {
    # id
    if (model@isPopulation) {
      map$id <- NlmeColumnMap(
        variableName = "id",
        columnName = "?",
        variableType = list(type = "id")
      )
    }

    # time
    if (model@isTimeBased) {
      map$time <- NlmeColumnMap(
        variableName = "time",
        columnName = "?",
        variableType = list(type = "time")
      )
    }

    # observations
    obsNames <- observationNames(model)
    for (obs in obsNames) {
      map[[obs]] <- NlmeColumnMap(
        variableName = obs,
        columnName = "?",
        variableType = list(type = "observation")
      )
    }

    # BQL - Not Required if Static LLOQ provided
    ErrorModel <- model@errorModel@effectsList
    for (e in seq_along(ErrorModel)) {
      if (ErrorModel[[e]]@isBQL == TRUE && ErrorModel[[e]]@bqlStaticValue == "") {
        map[[paste0(ErrorModel[[e]]@observeName, "BQL")]] <- NlmeColumnMap(
          variableName = paste0(ErrorModel[[e]]@observeName, "BQL"),
          columnName = "?",
          variableType = list(type = "bql")
        )
      }
    }

    # covariates
    covariateArray <- unlist(covariateNames(model))
    covariateTypeArray <- unlist(sapply(
      model@covariateList,
      function(x) {
        x@type
      }
    ))

    # for Emax model C is a special type of covariate, adding it here
    if (((model@modelType@modelType == PARAM_EMAX) || (model@modelType@modelType == PARAM_LINEAR)) &&
        model@isTextual == FALSE &&
        !("C" %in% covariateArray)) {
      covariateArray <- c(covariateArray, "C")
      covariateTypeArray <- c(covariateTypeArray, COVAR_CONTINUOUS)
    }

    for (i in seq_along(covariateArray)) {
      map[[covariateArray[i]]] <- NlmeColumnMap(
        variableName = covariateArray[i],
        columnName = "?",
        variableType = list(
          type = "covariate",
          covType = covariateTypeArray[i]
        )
      )
    }

    # doses
    for (dosepoint in model@dosePoints) {
      if (isTRUE(model@pkModelAttrs@infusionAllowed)) {
        if (model@pkModelAttrs@isDuration) {
          Infusion <- "Duration"
        } else {
          Infusion <- "Rate"
        }

        InfVariableName <- paste0(dosepoint, "_", Infusion)
        map[[InfVariableName]] <- NlmeColumnMap(
          variableName = InfVariableName,
          columnName = "?",
          variableType = list(
            type = "dosepointInf",
            Infusion = Infusion,
            DosepointN = 1,
            DosepointDouble = FALSE
          )
        )
      } else {
        Infusion <- NA
      }

      map[[dosepoint]] <- NlmeColumnMap(
        variableName = dosepoint,
        columnName = "?",
        variableType = list(
          type = "dosepoint",
          Infusion = Infusion,
          DosepointN = 1,
          DosepointDouble = FALSE
        )
      )
    }

    # Macro parameterization requires A1Strip
    if (model@pkModelAttrs@parameterization@paramType == Macro) {
      map[["A1Strip"]] <- NlmeColumnMap(
        variableName = "A1Strip",
        columnName = "?",
        variableType = list(type = "extraDoses")
      )
    }
  }

  # ADDL SS
  extraDosesList <- model@extraDoses
  if (length(extraDosesList) > 0) {
    for (extraDose in extraDosesList) {
      if (extraDose@doseType == SteadyStateDose) {
        variableName <- "SteadyState"
      } else {
        variableName <- "ADDL"
      }

      map[[variableName]] <- NlmeColumnMap(
        variableName = variableName,
        columnName = "?",
        variableType = list(type = "extraDoses")
      )

      # map[["II"]] <- NlmeColumnMap(variableName = "II",
      #                              columnName = "?",
      #                              variableType = list(type = "extraDoses"))
    }
  }

  # Reset
  if (model@hasResetInfo) {
    map[["Reset"]] <- NlmeColumnMap(
      variableName = "Reset",
      columnName = "?",
      variableType = list(type = "extraDoses")
    )
  }



  map <- .map_exactTerms(map, inputData)

  map
}

# map exact terms from data
# but only if they are not mapped yet
.map_exactTerms <- function(map, inputData) {
  colNames <- colnames(inputData)

  for (modelTermName in map) {
    # only first one is used
    colIndex <- colNames %in% modelTermName@variableName
    if (sum(colIndex)) {
      foundColumn <- colNames[colIndex]
    } else {
      colIndex <-
        tolower(colNames) %in% tolower(modelTermName@variableName)
      if (sum(colIndex) > 1) {
        warning(
          "Multiple columns found in the dataset:\n",
          paste(colNames[colIndex], collapse = ", "),
          "\nOnly first one will be used"
        )
        foundColumn <- colNames[colIndex][1]
      } else if (sum(colIndex) == 1) {
        foundColumn <- colNames[colIndex]
      }
    }

    if (sum(colIndex) > 0 &
        map[[modelTermName@variableName]]@columnName == "?") {
      map[[modelTermName@variableName]]@columnName <- foundColumn

      if (map[[modelTermName@variableName]]@variableType$type == "covariate") {
        checkCatCovariateMappingColumn(map, inputData, foundColumn, modelTermName@variableName)
      }
    }
  }

  map
}

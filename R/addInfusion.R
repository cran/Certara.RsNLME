#' Change existing dosing compartment to infusion
#'
#' Allows user to switch any dosing compartment to infusion
#'
#' @param .Object      Model object
#' @param doseCptName  Name of the compartment to which the dose is administered
#' @param isDuration   Set \code{TRUE} if duration is used to specify infusion information
#' @param isSecondDose Set \code{TRUE} if doseCptName is specified in the model through dosepoint2 statement
#' @param colName      Name of the input data column that represents the corresponding infusion rate. If not provided, \code{colName} must be mapped through \code{colMapping()}.
#'
#' @return Modified \code{NlmePmlModel} object
#' @examples
#' \donttest{
#' newModel <- addInfusion(model, "A1", FALSE, FALSE, "A1_1")
#' }
#'
#' @export
#'
addInfusion <-
  function(.Object,
           doseCptName,
           isDuration = FALSE,
           isSecondDose = FALSE,
           colName = NULL) {
    if (!doseCptName %in% .Object@dosePoints &&
        !paste0(doseCptName, "_1") %in% .Object@dosePoints) {
      stop(paste0("No dosepoint with the name '", doseCptName, "' found in the model"))
    }

    if (isSecondDose &&
        !paste0(doseCptName, "_2") %in% .Object@dosePoints) {
      stop(paste0(
        "No dosepoint2 statement associated with the specified dose compartment"
      ))
    }


    Infusion <- if (isDuration) {
      "Duration"
    } else {
      "Rate"
    }
    dosePointName <- doseCptName
    if (!doseCptName %in% .Object@dosePoints) {
      dosePointName <-
        if (isSecondDose) {
          paste0(doseCptName, "_2")
        } else {
          paste0(doseCptName, "_1")
        }
    }

    .Object@columnMapping@mapping[[dosePointName]]@variableType$Infusion <-
      Infusion

    InfVariableName <- paste0(dosePointName, "_", Infusion)

    if (!is.null(colName)) {
      if (!colName %in% colnames(.Object@inputData)) {
        stop(paste0("No column with the name '", colName, "' found in the dataset"))
      }
      .Object@columnMapping@mapping[[InfVariableName]] <-
        NlmeColumnMap(
          variableName = InfVariableName,
          columnName = colName,
          variableType = list(
            type = "dosepointInf",
            Infusion = Infusion,
            DosepointN = if (isSecondDose) {
              2
            } else {
              1
            },
            DosepointDouble = if (dosePointName == doseCptName) {
              FALSE
            } else {
              TRUE
            }
          )
        )
    } else {
      .Object@columnMapping@mapping[[InfVariableName]] <-
        NlmeColumnMap(
          variableName = InfVariableName,
          columnName = "?",
          variableType = list(
            type = "dosepointInf",
            Infusion = Infusion,
            DosepointN = if (isSecondDose) {
              2
            } else {
              1
            },
            DosepointDouble = if (dosePointName == doseCptName) {
              FALSE
            } else {
              TRUE
            }
          )
        )
    }
    .Object@pkModelAttrs@infusionAllowed <- TRUE
    if (isDuration) {
      .Object@pkModelAttrs@isDuration <- TRUE
    } else {
      .Object@pkModelAttrs@isDuration <- FALSE
    }

    .Object
  }

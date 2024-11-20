#' Create an Emax or Imax model
#'
#' Use to create an Emax or Imax model
#'
#' @inheritParams pkmodel
#' @param checkBaseline      Set to \code{TRUE} if the model contains a baseline response.
#' @param checkFractional    Set to \code{TRUE} to modify the default form for the model. Only applicable to models with \code{checkBaseline = TRUE}.
#' @param checkInhibitory    Set to \code{TRUE} to change the model from an Emax to an Imax model.
#' @param checkSigmoid       Set to \code{TRUE} to change the model to its corresponding signmoid form.
#' @inheritDotParams emaxmodel_MappingParameters
#'
#' @inheritSection pkmodel_MappingParameters Column mapping
#' @return \code{NlmePmlModel} object
#' @examples
#' model <- emaxmodel(data = pkpdData, ID = "ID", C = "CObs", EObs = "EObs")
#'
#' model <- emaxmodel(
#'   checkBaseline = TRUE,
#'   checkFractional = TRUE,
#'   checkInhibitory = TRUE,
#'   data = pkpdData,
#'   ID = "ID",
#'   C = "CObs",
#'   EObs = "EObs"
#' )
#'
#' # View PML Code
#' print(model)
#'
#' @export emaxmodel
emaxmodel <- function(isPopulation = TRUE,
                      checkBaseline = FALSE,
                      checkFractional = FALSE,
                      checkInhibitory = FALSE,
                      checkSigmoid = FALSE,
                      data = NULL,
                      columnMap = TRUE,
                      modelName = "",
                      workingDir = "",
                      ...) {
  # Check if missing data argument
  if (checkFractional) {
    checkBaseline <- TRUE
  }

  if (columnMap) {
    Columns <- .get_columnsVector(match.call()[[1]])

    # initialize mapping values
    for (Column in Columns) {
      eval(parse(text = paste(Column, "<- NULL")))
    }

    dotsPrepared <- .transform_EllipsisToVector(...)

    # note that current function initialize all model variables
    # to mapped columns in current envir
    columnNamesToInclude <- .prepare_dotargs(dotsPrepared, Columns)

    .check_data5ID(data, isPopulation, ID)

    .check_EmaxArgs(EObs, C)

    .check_column_mappings(columnNamesToInclude, data)
  }

  emaxParams <- NlmeEmaxParameters(
    checkBaseline = checkBaseline,
    checkFractional = checkFractional,
    checkInhibitory = checkInhibitory,
    checkSigmoid = checkSigmoid,
    frozen = FALSE
  )

  residual <- NlmeResidualEffect(errorType = ERR_ADDITIVE,
                                 effectName = "E")

  errorModel <- NlmeErrorModel(c(residual))

  model <- NlmePmlModel(
    modelType = PARAM_EMAX,
    isPopulation = isPopulation,
    emaxModelAttrs = emaxParams,
    errorModel = errorModel,
    isTimeBased = FALSE,
    modelInfo = NlmePmlModelInfo(modelName, workingDir)
  )

  model <- createEmaxStructuralParameters(model)
  model <- generatePMLModel(model)

  if (columnMap) {
    initColMapping(model) <- data

    # Perform column mapping
    modelColumnMapping(model) <- c(id = ID, EObs = EObs, C = C)
  } else if (!is.null(data)) {
    initColMapping(model) <- data
  }

  return(model)
}

#' Emax model mapping parameters
#'
#' Emax model mapping parameters
#'
#' @param ID Column mapping argument for input dataset column(s)
#' that identify individual data profiles. Only applicable to population models
#' \code{isPopulation = TRUE}.
#'
#' @param C Column mapping argument that represents
#' the input dataset column for the independent variable that is treated
#' as a covariate during the estimation/simulation process.
#'
#' @param EObs Column mapping argument that represents
#' the input dataset column for the observed drug effect
#' (i.e., the dependent variable).
#' @keywords internal
emaxmodel_MappingParameters <- function(ID = NULL,
                                        C = NULL,
                                        EObs = NULL)
  NULL

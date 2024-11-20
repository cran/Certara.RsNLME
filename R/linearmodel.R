#' Create linear model
#'
#' Use to create a constant, linear, or quadratic PD model
#'
#' @inheritParams pkmodel
#' @param type           Model type. Options are \code{"Constant"}, \code{"Linear"}, \code{"Quadratic"}.
#' @inheritDotParams linearmodel_MappingParameters
#'
#' @inheritSection pkmodel_MappingParameters Column mapping
#' @return \code{NlmePmlModel} object
#' @examples
#' model <- linearmodel(type = "Linear", data = pkpdData, ID = "ID", C = "CObs", EObs = "EObs")
#'
#' # View PML Code
#' print(model)
#'
#' @export linearmodel
linearmodel <- function(isPopulation = TRUE,
                        type = "Constant",
                        data = NULL,
                        columnMap = TRUE,
                        modelName = "",
                        workingDir = "",
                        ...
                        ) {
  type <- .check_linearType(type)

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

  residual <- NlmeResidualEffect(
    errorType = ERR_ADDITIVE,
    effectName = "E"
  )

  errorModel <- NlmeErrorModel(c(residual))

  model <- NlmePmlModel(
    modelType = PARAM_LINEAR,
    isPopulation = isPopulation,
    linearModelType = type,
    # check
    errorModel = errorModel,
    isTimeBased = FALSE,
    modelInfo = NlmePmlModelInfo(modelName, workingDir)
  )

  model <- createLinearStructuralParameters(model)
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

.check_linearType <- function(type) {
  if (type == "Constant") {
    type <- LinearAlpha
  } else if (type == "Linear") {
    type <- LinearBeta
  } else if (type == "Quadratic") {
    type <- LinearGamma
  } else {
    stop("type argument must be one of 'Constant', 'Linear', 'Quadratic'")
  }

  type
}

.check_EmaxArgs <- function(EObs, C) {
  # Check if missing EObs Column
  if (is.null(EObs)) {
    warning("`EObs` column not mapped")
  }

  if (is.null(C)) {
    stop("Must specify 'C'")
  }
}

#' Linear model mapping parameters
#'
#' Linear model mapping parameters
#'
#' @param ID Column mapping argument for input dataset column(s)
#' that identify individual data profiles. Only applicable to population models
#' \code{isPopulation = TRUE}.
#' @param C Column mapping argument that represents
#' the input dataset column for the independent variable that is treated
#' as a covariate during the estimation/simulation process.
#' @param EObs Column mapping argument that represents
#' the input dataset column for the observed drug effect
#' (i.e., the dependent variable).
#' @keywords internal
linearmodel_MappingParameters <- function(ID = NULL,
                                          C = NULL,
                                          EObs = NULL) NULL

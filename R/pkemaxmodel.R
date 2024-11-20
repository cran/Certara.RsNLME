#' Create a PK/Emax or PK/Imax model
#'
#' Use to create a PK/Emax or PK/Imax model
#'
#' @inheritParams pkindirectmodel
#' @param checkBaseline           Does Emax/Imax model have a baseline response?
#' @param checkFractional         Set to \code{TRUE} to modify the default form for the Emax/Imax model. Only applicable to models with \code{checkBaseline = TRUE}.
#' @param checkInhibitory         Set to \code{TRUE} to change the default Emax to Imax model.
#' @param checkSigmoid            Set to \code{TRUE} to change the Emax/Imax to its corresponding sigmoid form.
#' @param isEmaxFrozen            Set to \code{TRUE} to freeze PD fixed effects and remove the corresponding random effects as well as the PD observed variable from the model.
#' @inheritDotParams pkindirectmodel_MappingParameters
#'
#' @inheritSection pkmodel_MappingParameters Column mapping
#' @return \code{NlmePmlModel} object
#' @examples
#' model <- pkemaxmodel(
#'   parameterization = "Macro",
#'   data = pkpdData,
#'   Time = "Time",
#'   ID = "ID",
#'   A1 = "Dose",
#'   C1Obs = "CObs",
#'   EObs = "EObs"
#' )
#'
#' # View the model as well as its associated column mappings
#' print(model)
#'
#' @export pkemaxmodel
pkemaxmodel <- function(isPopulation = TRUE,
                        parameterization = "Clearance",
                        absorption = "Intravenous",
                        numCompartments = 1,
                        isClosedForm = TRUE,
                        isTlag = FALSE,
                        hasEliminationComp = FALSE,
                        isFractionExcreted = FALSE,
                        isSaturating = FALSE,
                        infusionAllowed = FALSE,
                        isDuration = FALSE,
                        isSequential = FALSE,
                        isPkFrozen = FALSE,
                        hasEffectsCompartment = FALSE,
                        checkBaseline = FALSE,
                        checkFractional = FALSE,
                        checkInhibitory = FALSE,
                        checkSigmoid = FALSE,
                        isEmaxFrozen = FALSE,
                        data = NULL,
                        columnMap = TRUE,
                        modelName = "",
                        workingDir = "",
                        ...) {
  if (isFractionExcreted) {
    hasEliminationComp <- TRUE
  }

  if (hasEliminationComp) {
    isClosedForm <- FALSE
  }

  if (isDuration) {
    infusionAllowed <- TRUE
  }

  parameterization <- .check_parameterization(parameterization)

  absorption <- .check_absorption(absorption)

  if (!hasArg(isClosedForm) &&
      (absorption %in% c(Gamma, Weibull, InverseGaussian))) {
    isClosedForm <- FALSE
  }

  checkPKParams(
    hasEliminationComp,
    isClosedForm,
    numCompartments,
    parameterization,
    isSaturating,
    isFractionExcreted,
    absorption
  )

  if (isSequential && isPkFrozen) {
    stop("isSequential and isPkFrozen cannot both be set to TRUE")
  }

  if (columnMap) {
    Columns <- .get_columnsVector(match.call()[[1]], isSequential)

    # initialize mapping values
    for (Column in Columns) {
      eval(parse(text = paste(Column, "<- NULL")))
    }

    dotsPrepared <- .transform_EllipsisToVector(...)

    # note that current function initialize all model variables
    # to mapped columns in current envir
    columnNamesToInclude <- .prepare_dotargs(dotsPrepared, Columns)

    .check_data5ID(data, isPopulation, ID)

    .check_PKArgs(
      absorption = absorption,
      parameterization = parameterization,
      hasEliminationComp = hasEliminationComp,
      infusionAllowed = infusionAllowed,
      isDuration = isDuration,
      isSequential = isSequential,
      Columns = Columns,
      ...
    )

    .check_SequentialParams(
      isSequential,
      isTlag,
      absorption,
      parameterization,
      isSaturating,
      isFractionExcreted,
      numCompartments,
      Columns = Columns,
      ...
    )

      # Check if missing EObs Column
      if (is.null(EObs)) {
        if (isEmaxFrozen == FALSE) {
          warning("`EObs` column not mapped")
        }
      }

    .check_column_mappings(columnNamesToInclude, data)
  }

  emaxParams <- NlmeEmaxParameters(
    checkBaseline = checkBaseline,
    checkFractional = checkFractional,
    checkInhibitory = checkInhibitory,
    checkSigmoid = checkSigmoid,
    hasEffectsCompartment = hasEffectsCompartment,
    frozen = isEmaxFrozen
  )

  pkParams <- NlmePkParameters(
    parameterization = NlmeModelParameterization(parameterization),
    absorption = NlmeModelAbsorption(absorption),
    numCompartments = numCompartments,
    isTlag = isTlag,
    hasEliminationComp = hasEliminationComp,
    isFractionExcreted = isFractionExcreted,
    isSaturating = isSaturating,
    infusionAllowed = infusionAllowed,
    isDuration = isDuration,
    isClosedForm = isClosedForm,
    isSequential = isSequential,
    isPkFrozen = isPkFrozen
  )

  if (!isEmaxFrozen) {
    pdresidual <- NlmeResidualEffect(
      errorType = ERR_ADDITIVE,
      effectName = "E"
    )
  } else {
    pdresidual <- NULL
  }


  if (!isPkFrozen) {
    if (parameterization == Macro1 || parameterization == Macro) {
      pkresidual <- NlmeResidualEffect(
        errorType = ERR_MULTIPLICATIVE,
        effectName = "C1",
        frozen = isSequential
      )
    } else {
      pkresidual <- NlmeResidualEffect(
        errorType = ERR_MULTIPLICATIVE,
        effectName = "C",
        frozen = isSequential
      )
    }
  } else {
    pkresidual <- NULL
  }

  if (hasEliminationComp) {
    a0residual <- NlmeResidualEffect(
      errorType = ERR_MULTIPLICATIVE,
      effectName = "A0",
      frozen = isSequential
    )
  } else {
    a0residual <- NULL
  }

  if (isSequential) {
    emod <- c(a0residual, pdresidual)
  } else {
    emod <- c(a0residual, pdresidual, pkresidual)
  }

  if (!is.null(emod)) {
    errorModel <- NlmeErrorModel(emod)
  } else {
    errorModel <- NlmeErrorModel()
  }

  model <- NlmePmlModel(
    modelType = PkEmax,
    isPopulation = isPopulation,
    emaxModelAttrs = emaxParams,
    pkModelAttrs = pkParams,
    hasEffectsCompartment = hasEffectsCompartment,
    errorModel = errorModel,
    modelInfo = NlmePmlModelInfo(modelName, workingDir)
  )


  model <- createPkStructuralParameters(model)
  model <- createEmaxStructuralParameters(model)
  model <- generatePMLModel(model)

  if (columnMap) {
      model <- .map_indirectModels(model = model,
                                   data = data,
                                   isSequential,
                                   Columns = Columns,
                                   ...)
  } else if (!is.null(data)) {
    initColMapping(model) <- data
    if (isSequential) {
      initRandParamsMapping(model) <- data
    }
  }

  return(model)
}

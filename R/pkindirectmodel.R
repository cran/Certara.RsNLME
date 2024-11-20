#' Create a PK/Indirect response model
#'
#' Use to create a PK/Indirect response model.
#'
#' @inheritParams pkmodel
#' @param isSequential            Set to \code{TRUE} to freeze PK fixed effects and convert
#' the corresponding random effects into covariates as well as remove the PK observed variable from the model.
#' @param isPkFrozen              Set to \code{TRUE} to freeze PK fixed effects and remove
#' the corresponding random effects as well as the PK observed variable from the model.
#' @param hasEffectsCompartment   Set to \code{TRUE} to include an effect compartment into the model.
#' @param indirectType            Type of drug actions for the indirect response model.
#' Options are \code{"LimitedStimulation"}, \code{"InfiniteStimulation"}, \code{"LimitedInhibition"},
#' \code{"InverseInhibition"}, \code{"LinearStimulation"}, or \code{"LogLinearStimulation"}.
#' @param isBuildup               Set to \code{FALSE} to have the drug actions affect
#'  the loss/degradation instead of the production.
#' @param isExponent              Set to \code{TRUE} to add an exponent parameter to the drug action term.
#' @param indirectFrozen          Set to \code{TRUE} to freeze PD fixed effects and remove
#' the corresponding random effects as well as the PD observed variable from the model.
#' @inheritDotParams pkindirectmodel_MappingParameters
#'
#' @inheritSection pkmodel_MappingParameters Column mapping
#' @return \code{NlmePmlModel} object
#' @examples
#' model <- pkindirectmodel(
#'   parameterization = "Micro",
#'   data = pkpdData,
#'   ID = "ID",
#'   Time = "Time",
#'   A1 = "Dose",
#'   CObs = "CObs",
#'   EObs = "EObs"
#' )
#'
#' # View PML Code
#' print(model)
#'
#' @export pkindirectmodel
pkindirectmodel <- function(isPopulation = TRUE,
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
                            indirectType = "LimitedStimulation",
                            isBuildup = TRUE,
                            isExponent = FALSE,
                            indirectFrozen = FALSE,
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

  # Check for indirect model type and assign value
  if (indirectType %in% .indirectTypeNames) {
    indirectType <-
      which(indirectType == .indirectTypeNames, arr.ind = TRUE)
  } else {
    stop(
      "indirectType argument must be one of 'LimitedStimulation', 'InfiniteStimulation', 'LimitedInhibition', 'InverseInhibition', 'LinearStimulation', 'LogLinearStimulation'"
    )
  }

  if (!hasArg(isClosedForm) &&
    (absorption == Gamma ||
      absorption == Weibull || absorption == InverseGaussian)) {
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

  # Errors related to columnMap
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
    if (is.null(EObs) && indirectFrozen == FALSE) {
      warning("'EObs' column not mapped")
    }

    .check_column_mappings(columnNamesToInclude, data)
  }

  indirectParams <- NlmeIndirectParameters(
    type = indirectType,
    hasEffectsCompartment = hasEffectsCompartment,
    isBuildup = isBuildup,
    isExponent = isExponent,
    frozen = indirectFrozen
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

  if (!indirectFrozen) {
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
    modelType = PkIndirect,
    isPopulation = isPopulation,
    indirectModelAttrs = indirectParams,
    pkModelAttrs = pkParams,
    hasEffectsCompartment = hasEffectsCompartment,
    errorModel = errorModel,
    modelInfo = NlmePmlModelInfo(modelName, workingDir)
  )

  model <- createPkStructuralParameters(model)
  model <- createIndirectStructuralParameters(model)
  model <- generatePMLModel(model)

  if (columnMap) {
    model <- .map_indirectModels(model,
      data,
      isSequential,
      Columns = Columns,
      ...
    )
  } else if (!is.null(data)) {
    initColMapping(model) <- data
    if (isSequential) {
      initRandParamsMapping(model) <- data
    }
  }

  return(model)
}

.map_indirectModels <- function(model,
                                data,
                                isSequential,
                                Columns,
                                ...) {
  # initialize mapping values
  for (Column in Columns) {
    eval(parse(text = paste(Column, "<- NULL")))
  }

  dotsPrepared <- .transform_EllipsisToVector(...)

  # note that current function initialize all model variables
  # to mapped columns in current envir
  columnNamesToInclude <- .prepare_dotargs(dotsPrepared, Columns)

  initColMapping(model) <- data

  modelColumnMapping(model) <- c(
    id = ID,
    A1 = A1,
    Aa = Aa,
    A = A,
    A0Obs = A0Obs,
    A1_Rate = A1_Rate,
    A1_Duration = A1_Duration,
    A1Strip = A1Strip,
    Aa_Rate = Aa_Rate,
    Aa_Duration = Aa_Duration,
    A_Rate = A_Rate,
    A_Duration = A_Duration,
    CObs = CObs,
    C1Obs = C1Obs,
    EObs = EObs,
    time = Time
  )

  if (isSequential) {
    # For now we are assuming randomParamterValues must be in primary dataset, will support alt data in GUI
    initRandParamsMapping(model) <- data

    modelRandParamsMapping(model) <- c(
      nV = nV,
      nV2 = nV2,
      nV3 = nV3,
      nA = nA,
      nCl = nCl,
      nCl2 = nCl2,
      nCl3 = nCl3,
      nAlpha = nAlpha,
      nKa = nKa,
      nKe = nKe,
      nB = nB,
      nBeta = nBeta,
      nC = nC,
      nGamma = nGamma,
      nK12 = nK12,
      nK21 = nK21,
      nK13 = nK13,
      nK31 = nK31,
      nKm = nKm,
      nVmax = nVmax,
      nFe = nFe,
      nMeanDelayTime = nMeanDelayTime,
      nShapeParam = nShapeParam,
      nShapeParamMinusOne = nShapeParamMinusOne,
      nTlag = nTlag
    )
  }

  model
}

.get_columnsVector <-
  function(ParentFunctionName, isSequential = FALSE) {
    ParentFunctionName <-
      as.character(ParentFunctionName)[length(ParentFunctionName)]
    PKColumns <- c(
      "ID",
      "Time",
      "A1",
      "Aa",
      "A",
      "A1_Rate",
      "A1_Duration",
      "Aa_Rate",
      "Aa_Duration",
      "A_Rate",
      "A_Duration",
      "A1Strip",
      "CObs",
      "C1Obs",
      "A0Obs"
    )

    EmaxColumns <- c("ID", "EObs", "C")

    sequentialColumns <- c(
      "nV",
      "nV2",
      "nV3",
      "nA",
      "nCl",
      "nCl2",
      "nCl3",
      "nAlpha",
      "nKa",
      "nKe",
      "nB",
      "nBeta",
      "nC",
      "nGamma",
      "nK12",
      "nK21",
      "nK13",
      "nK31",
      "nKm",
      "nVmax",
      "nFe",
      "nMeanDelayTime",
      "nShapeParam",
      "nShapeParamMinusOne",
      "nTlag"
    )

    if (ParentFunctionName == "pkmodel") {
      Columns <- PKColumns
    } else if (ParentFunctionName %in% c("emaxmodel", "linearmodel")) {
      Columns <- EmaxColumns
    } else {
      Columns <- union(PKColumns, EmaxColumns)
      if (isSequential) {
        Columns <- c(Columns, sequentialColumns)
      }
    }

    Columns
  }

.check_IndirectSequentialArg <- function(Arg, Condition) {
  if (is.null(Arg)) {
    if (Condition) {
      stop(
        "Must specify ",
        deparse(substitute(Arg, env = environment())),
        " if isSequential = TRUE"
      )
    }
  }
}

.check_SequentialParams <-
  function(isSequential,
           isTlag,
           absorption,
           parameterization,
           isSaturating,
           isFractionExcreted,
           numCompartments,
           Columns = Columns,
           ...) {
    # initialize mapping values
    for (Column in Columns) {
      eval(parse(text = paste(Column, "<- NULL")))
    }

    dotsPrepared <- .transform_EllipsisToVector(...)

    # note that current function initialize all model variables
    # to mapped columns in current envir
    columnNamesToInclude <- .prepare_dotargs(dotsPrepared, Columns)

    if (!isSequential) {
      return(TRUE)
    }
    .check_IndirectSequentialArg(nTlag, isTlag)

    .check_IndirectSequentialArg(
      nMeanDelayTime,
      absorption %in% c(Gamma, Weibull, InverseGaussian)
    )

    .check_IndirectSequentialArg(nShapeParam, absorption == InverseGaussian)

    .check_IndirectSequentialArg(nShapeParamMinusOne, absorption %in% c(Gamma, Weibull))

    .check_IndirectSequentialArg(nV, parameterization != Macro)

    .check_IndirectSequentialArg(nA, parameterization == Macro)

    .check_IndirectSequentialArg(nCl, parameterization == Clearance &&
      isSaturating == FALSE)

    .check_IndirectSequentialArg(nKm, isSaturating == TRUE)

    .check_IndirectSequentialArg(nVmax, isSaturating == TRUE)

    .check_IndirectSequentialArg(nKe, parameterization == Micro)

    .check_IndirectSequentialArg(nFe, isFractionExcreted)

    # nKa must be mapped for all parameterization if absorption = Extravascular
    .check_IndirectSequentialArg(nKa, absorption == Extravascular)

    .check_IndirectSequentialArg(nAlpha, parameterization == Macro1 ||
      parameterization == Macro)

    if (numCompartments >= 2) {
      .check_IndirectSequentialArg(nB, parameterization == Macro ||
        parameterization == Macro1)
      .check_IndirectSequentialArg(
        nBeta,
        parameterization == Macro ||
          parameterization == Macro1
      )

      .check_IndirectSequentialArg(nK12, parameterization == Micro)
      .check_IndirectSequentialArg(nK21, parameterization == Micro)

      .check_IndirectSequentialArg(nV2, parameterization == Clearance)
      .check_IndirectSequentialArg(nCl2, parameterization == Clearance)
    }

    if (numCompartments == 3) {
      .check_IndirectSequentialArg(nC, parameterization == Macro ||
        parameterization == Macro1)
      .check_IndirectSequentialArg(
        nGamma,
        parameterization == Macro ||
          parameterization == Macro1
      )

      .check_IndirectSequentialArg(nK13, parameterization == Micro)
      .check_IndirectSequentialArg(nK31, parameterization == Micro)

      .check_IndirectSequentialArg(nV3, parameterization == Clearance)
      .check_IndirectSequentialArg(nCl3, parameterization == Clearance)
    }
  }

#' PK Indirect model mapping parameters
#'
#' PK Indirect mapping parameters
#'
#' @param ID          Column mapping argument for input dataset column(s) that identify
#' individual data profiles. Only applicable to population models \code{isPopulation = TRUE}.
#'
#' @param Time        Column mapping argument that represents the input dataset column
#' for the relative time used in a study and only applicable to time-based models.
#'
#' @param A1          Column mapping argument that represents the input dataset column
#' for the amount of drug administered. Only applicable to the following types of models:
#' \itemize{
#'  \item Models with \code{absorption = "Intravenous"} and parameterization set
#'  to either \code{"Clearance"},\code{"Micro"}, or \code{"Macro"}
#'  \item Models with \code{absorption} set to either \code{"Gamma"}, \code{"InverseGaussian"},
#'  or \code{"Weibull"}
#'  }
#'
#'
#' @param Aa          Column mapping argument that represents the input dataset column
#' for the amount of drug administered and only applicable to models with \code{absorption = "FirstOrder"}.
#'
#' @param A           Column mapping argument that represents the input dataset column
#' for the amount of drug administered and only applicable to models with
#' \code{absorption = "Intravenous"} and \code{parameterization = "Macro1"}.
#'
#' @param A1_Rate     Column mapping argument that represents the input dataset column
#' for the rate of drug administered.  Only applicable to the following types of models:
#' \itemize{
#'  \item Models with \code{absorption = "Intravenous"}, \code{infusionAllowed = TRUE}
#'  and parameterization set to either \code{"Clearance"},\code{"Micro"} or \code{"Macro"}
#'  \item Models with \code{absorption} set to either \code{"Gamma"}, \code{"InverseGaussian"},
#'  or \code{"Weibull"} and \code{infusionAllowed = TRUE}
#'  }
#'
#' @param A1_Duration Column mapping argument that represents the input dataset column
#' for the duration of drug administered.  Only applicable to the following types of models:
#' \itemize{
#'  \item Models with \code{absorption = "Intravenous"}, \code{infusionAllowed = TRUE} with
#'   \code{isDuration = TRUE} and parameterization set to either \code{"Clearance"},\code{"Micro"}
#'   or \code{"Macro"}
#'  \item Models with \code{absorption} set to either \code{"Gamma"}, \code{"InverseGaussian"},
#'  or \code{"Weibull"} and \code{infusionAllowed = TRUE} with \code{isDuration = TRUE}
#'  }
#'
#' @param Aa_Rate     Column mapping argument that represents the input dataset column
#' for the rate of drug administered and only applicable to models with \code{absorption = "FirstOrder"},
#' \code{infusionAllowed = TRUE}.
#'
#' @param Aa_Duration Column mapping argument that represents the input dataset column
#' for the duration of drug administered and only applicable to models with \code{absorption = "FirstOrder"},
#' \code{infusionAllowed = TRUE}, and \code{isDuration = TRUE}.
#'
#' @param A_Rate      Column mapping argument that represents the input dataset column
#' for the rate of drug administered and only applicable to models with \code{absorption = "Intravenous"},
#' \code{infusionAllowed = TRUE}, and \code{parameterization = "Macro1"}.
#'
#' @param A_Duration  Column mapping argument that represents the input dataset column
#' for the duration of drug administered and only applicable to models with \code{absorption = "Intravenous"},
#' \code{infusionAllowed = TRUE}, \code{isDuration = TRUE}, and \code{parameterization = "Macro1"}.
#'
#' @param A1Strip     Column mapping argument that represents the input dataset column
#' for the stripping dose and only applicable to models with \code{parameterization = "Macro"}.
#'
#' @param CObs        Column mapping argument that represents the input dataset column
#' for the observations of drug concentration in the central compartment and only applicable
#' to models with \code{parameterization} being either set to either \code{"Clearance"} or \code{"Micro"}.
#'
#' @param C1Obs       Column mapping argument that represents the input dataset column
#' for the observations of drug concentration in the central compartment and only applicable
#' to models with \code{parameterization} being either set to either \code{"Macro"} or \code{"Macro1"}.
#'
#' @param A0Obs       Column mapping argument that represents the input dataset column
#' for the observed amount of drug in the elimination compartment. (\code{hasEliminationComp = TRUE}).
#'
#' @param EObs              Column mapping argument that represents the input dataset column
#' for the observed drug effect.
#'
#' @param nV                If \code{isSequential = TRUE}, mapped to the input dataset column
#' that lists the values for random effect \code{nV}.
#'
#' @param nV2                 If \code{isSequential = TRUE}, mapped to the input dataset column
#' that lists the values for random effect \code{nV2}.
#'
#' @param nV3                 If \code{isSequential = TRUE}, mapped to the input dataset column
#' that lists the values for random effect \code{nV3}.
#'
#' @param nCl                 If \code{isSequential = TRUE}, mapped to the input dataset column
#' that lists the values for random effect \code{nCl}.
#'
#' @param nCl2                If \code{isSequential = TRUE}, mapped to the input dataset column
#' that lists the values for random effect \code{nCl2}.
#'
#' @param nCl3                If \code{isSequential = TRUE}, mapped to the input dataset column
#' that lists the values for random effect \code{nCl3}.
#'
#' @param nKa                 If \code{isSequential = TRUE}, mapped to the input dataset column
#' that lists the values for random effect \code{nKa}.
#'
#' @param nA                  If \code{isSequential = TRUE}, mapped to the input dataset column
#' that lists the values for random effect \code{nA}.
#'
#' @param nAlpha              If \code{isSequential = TRUE}, mapped to the input dataset column
#' that lists the values for random effect \code{nAlpha}.
#'
#' @param nB                  If \code{isSequential = TRUE}, mapped to the input dataset column
#' that lists the values for random effect \code{nB}.
#'
#' @param nBeta               If \code{isSequential = TRUE}, mapped to the input dataset column
#' that lists the values for random effect \code{nBeta}.
#'
#' @param nC                  If \code{isSequential = TRUE}, mapped to the input dataset column
#' that lists the values for random effect \code{nC}.
#'
#' @param nGamma              If \code{isSequential = TRUE}, mapped to the input dataset column
#' that lists the values for random effect \code{nGamma}.
#'
#' @param nKe                 If \code{isSequential = TRUE}, mapped to the input dataset column
#' that lists the values for random effect \code{nKe}.
#'
#' @param nK12                If \code{isSequential = TRUE}, mapped to the input dataset column
#' that lists the values for random effect \code{nK12}.
#'
#' @param nK21                If \code{isSequential = TRUE}, mapped to the input dataset column
#' that lists the values for random effect \code{nK21}.
#'
#' @param nK13                If \code{isSequential = TRUE}, mapped to the input dataset column
#' that lists the values for random effect \code{nK13}.
#'
#' @param nK31                If \code{isSequential = TRUE}, mapped to the input dataset column
#' that lists the values for random effect \code{nK31}.
#'
#' @param nTlag	              If \code{isSequential = TRUE}, mapped to the input dataset column
#' that lists the values for random effect \code{nTlag}.
#'
#' @param nKm	                If \code{isSequential = TRUE}, mapped to the input dataset column
#' that lists the values for random effect \code{nKm}.
#'
#' @param nVmax	              If \code{isSequential = TRUE}, mapped to the input dataset column
#' that lists the values for random effect \code{nVmax}.
#'
#' @param nFe                 If \code{isSequential = TRUE} and \code{isFractionExcreted = TRUE},
#' mapped to the input dataset column that lists the values for random effect \code{nFe}.
#'
#' @param nMeanDelayTime      If \code{isSequential = TRUE}, mapped to the input dataset column
#' that lists the values for random effect \code{nMeanDelayTime}.
#'
#' @param nShapeParam         If \code{isSequential = TRUE}, mapped to the input dataset column
#' that lists the values for random effect \code{nShapeParam}.
#'
#' @param nShapeParamMinusOne If \code{isSequential = TRUE}, mapped to the input dataset column
#' that lists the values for random effect \code{nShapeParamMinusOne}.
#' @keywords internal
pkindirectmodel_MappingParameters <- function(ID = NULL,
                                              Time = NULL,
                                              A1 = NULL,
                                              Aa = NULL,
                                              A = NULL,
                                              A1_Rate = NULL,
                                              A1_Duration = NULL,
                                              Aa_Rate = NULL,
                                              Aa_Duration = NULL,
                                              A_Rate = NULL,
                                              A_Duration = NULL,
                                              A1Strip = NULL,
                                              CObs = NULL,
                                              C1Obs = NULL,
                                              A0Obs = NULL,
                                              EObs = NULL,
                                              nV = NULL,
                                              nV2 = NULL,
                                              nV3 = NULL,
                                              nCl = NULL,
                                              nCl2 = NULL,
                                              nCl3 = NULL,
                                              nKa = NULL,
                                              nA = NULL,
                                              nAlpha = NULL,
                                              nB = NULL,
                                              nBeta = NULL,
                                              nC = NULL,
                                              nGamma = NULL,
                                              nKe = NULL,
                                              nK12 = NULL,
                                              nK21 = NULL,
                                              nK13 = NULL,
                                              nK31 = NULL,
                                              nTlag = NULL,
                                              nKm = NULL,
                                              nVmax = NULL,
                                              nFe = NULL,
                                              nMeanDelayTime = NULL,
                                              nShapeParamMinusOne = NULL,
                                              nShapeParam = NULL) {
  NULL
}

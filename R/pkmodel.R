#' Creates a PK model
#'
#' Use to create a PK model
#' @param isPopulation        Is this a population model \code{TRUE} or individual model \code{FALSE}?
#' @param parameterization    Type of parameterization. Options are \code{"Clearance"}, \code{"Micro"},
#' \code{"Macro"}, or \code{"Macro1"}.
#' @param absorption          Type of absorption. Options are \code{"Intravenous"}, \code{"FirstOrder"},
#' \code{"Gamma"}, \code{"InverseGaussian"}, \code{"Weibull"} .
#' @param numCompartments     Value of either  \code{1}, \code{2}, or \code{3}.
#' @param isClosedForm        Set to \code{TRUE} to convert model from a differential equation to close form.
#' @param isTlag              Set to \code{TRUE} to add a lag time parameter to the model.
#' @param hasEliminationComp  Set to \code{TRUE} to add an elimination compartment to the model.
#' @param isFractionExcreted  Set to \code{TRUE} if elimination compartment (\code{hasEliminationComp = TRUE})
#' contains a fraction excreted parameter.
#' @param isSaturating        Set to \code{TRUE} to use Michaelis-Menten kinetics for elimination.
#' Only applicable to models with \code{paramteterization = "Clearance"}
#' @param infusionAllowed     Set to \code{TRUE} if infusions allowed.
#' @param isDuration          Set to \code{TRUE} if infusions use duration instead of rate
#' (must also set \code{infusionAllowed = TRUE}).
#' @param isStdevFrozen       Set to \code{TRUE} to freeze value of standard deviation of residual error variable.
#' @param data                Input dataset
#' @param columnMap           If \code{TRUE} (default) column mapping arguments are required.
#' Set to \code{FALSE} to manually map columns after defining model using \code{\link{colMapping}}.
#' @param modelName           Model name for subdirectory created for model output in current working directory.
#' @param workingDir          Working directory to run the model. Current working directory will be used
#' if \code{workingDir} not specified.
#' @inheritDotParams pkmodel_MappingParameters
#'
#' @inheritSection pkmodel_MappingParameters Column mapping
#'
#' @return \code{NlmePmlModel} object
#'
#' @examples
#' model <- pkmodel(
#'   parameterization = "Clearance",
#'   numCompartments = 2,
#'   data = pkData,
#'   ID = "Subject",
#'   Time = "Act_Time",
#'   A1 = "Amount",
#'   CObs = "Conc"
#' )
#'
#' # View the model as well as its associated column mappings
#' print(model)
#'
#' @export pkmodel
pkmodel <- function(isPopulation = TRUE,
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
                    isStdevFrozen = FALSE,
                    data = NULL,
                    columnMap = TRUE,
                    modelName = "",
                    workingDir = "",
                    ...) {
  if (isDuration) {
    infusionAllowed <- TRUE
  }

  if (isFractionExcreted) {
    hasEliminationComp <- TRUE
  }

  parameterization <- .check_parameterization(parameterization)

  absorption <- .check_absorption(absorption)

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

    .check_PKArgs(
      absorption = absorption,
      parameterization = parameterization,
      hasEliminationComp = hasEliminationComp,
      infusionAllowed = infusionAllowed,
      isDuration = isDuration,
      Columns = Columns,
      ...
    )

    .check_column_mappings(columnNamesToInclude, data)
  }

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
    isSequential = FALSE
  )

  if (parameterization == Macro1 || parameterization == Macro) {
    residual <- NlmeResidualEffect(
      errorType = ERR_MULTIPLICATIVE,
      effectName = "C1",
      frozen = isStdevFrozen
    )
  } else {
    residual <- NlmeResidualEffect(
      errorType = ERR_MULTIPLICATIVE,
      effectName = "C",
      frozen = isStdevFrozen
    )
  }

  if (hasEliminationComp) {
    a0residual <- NlmeResidualEffect(
      errorType = ERR_MULTIPLICATIVE,
      effectName = "A0",
      frozen = isStdevFrozen
    )
    errorModel <-
      NlmeErrorModel(c(residual, a0residual)) # Combine resiual with a0 residual if has elimination compartment
  } else {
    errorModel <- NlmeErrorModel(c(residual))
  }

  model <- NlmePmlModel(
    modelType = PARAM_PK,
    isPopulation = isPopulation,
    pkModelAttrs = pkParams,
    errorModel = errorModel,
    modelInfo = NlmePmlModelInfo(modelName, workingDir)
  )

  # look into this function, could also be incorrectly setup
  model <- createPkStructuralParameters(model)
  model <- generatePMLModel(model)

  if (columnMap) {
    initColMapping(model) <- data

    # Perform column mapping
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
      time = Time
    )
  } else if (!is.null(data)) {
    initColMapping(model) <- data
  }

  return(model)
}

.check_parameterization <- function(parameterization) {
  # Check for correct paremeterization and assign value
  if (parameterization %in% c("Clearance", "Macro", "Macro1", "Micro")) {
    parameterization <- eval(parse(text = parameterization))
  } else {
    stop("parameterization must be one of 'Clearance', 'Macro', 'Macro1', 'Micro'")
  }

  parameterization
}

.check_absorption <- function(absorption) {
  if (absorption == "FirstOrder") {
    absorption <- "Extravascular"
  }
  # Check for correct absorption type and assign a value
  if (absorption %in% c(
    "Intravenous",
    "Extravascular",
    "Gamma",
    "Weibull",
    "InverseGaussian"
  )) {
    absorption <- eval(parse(text = absorption))
  } else {
    stop(
      "absorption must be one of 'FirstOrder', 'Intravenous', 'Gamma', 'Weibull' or 'InverseGaussian'"
    )
  }

  absorption
}

.check_data5ID <- function(data, isPopulation, ID) {
  assertthat::assert_that(!is.null(data), msg = "Missing data argument")

  if (isPopulation == TRUE) {
    assertthat::assert_that(!is.null(ID),
      msg = "Must specify ID if 'isPopulation = TRUE'"
    )

    assertthat::assert_that(length(strsplit(ID, ",")[[1]]) <= 5,
      msg = "number of columns mapped to `ID` must be less than or equal to 5"
    )
  }
}

.check_PKArgs <-
  function(absorption,
           parameterization,
           hasEliminationComp,
           infusionAllowed,
           isDuration,
           isSequential = FALSE,
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

    assertthat::assert_that(!is.null(Time), msg = "'Time' column not mapped")

    if (absorption == Intravenous) {
      if (parameterization == Macro1) {
        assertthat::assert_that(!is.null(A),
          msg = "Must specify A if using absorption = 'Intravenous' and parameterization = 'Macro1'"
        )
      } else {
        assertthat::assert_that(!is.null(A1),
          msg = "Must specify A1 if using absorption = 'Intravenous','Gamma', 'Weibull' or 'InverseGaussian'"
        )
      }
    } else if (absorption == Extravascular) {
      assertthat::assert_that(!is.null(Aa),
        msg = "Must specify Aa if using absorption = 'FirstOrder'"
      )
    }

    if (parameterization == Clearance ||
      parameterization == Micro) {
      if (hasEliminationComp == TRUE && is.null(A0Obs)) {
        warning("A0Obs column not mapped")
      }
    }

    if (is.null(A1Strip) && parameterization == Macro) {
      warning("A1Strip column not mapped")
    } else if (parameterization != Macro && !is.null(A1Strip)) {
      stop("Must set parameterization = 'Macro' if setting value for A1Strip")
    }

    if (infusionAllowed == TRUE && absorption == Intravenous) {
      if (parameterization == Macro1) {
        if (isDuration) {
          assertthat::assert_that(!is.null(A_Duration),
            msg = "Must specify A_Duration if using parameterization = 'Macro1' and 'infusionAllowed = TRUE' and 'isDuration = TRUE'"
          )
        } else {
          assertthat::assert_that(!is.null(A_Rate),
            msg = "Must specify A_Rate if using parameterization = 'Macro1' and 'infusionAllowed = TRUE'"
          )
        }
      } else {
        if (isDuration == TRUE) {
          assertthat::assert_that(!is.null(A1_Duration),
            msg = "Must specify A1_Duration if using 'infusionAllowed = TRUE' and isDuration = TRUE"
          )
        } else {
          assertthat::assert_that(!is.null(A1_Rate),
            msg = "Must specify A1_Rate if using 'infusionAllowed = TRUE'"
          )
        }
      }
    }

    if (infusionAllowed == TRUE && absorption == Extravascular) {
      if (isDuration == TRUE) {
        assertthat::assert_that(!is.null(Aa_Duration),
          msg = "Must specify Aa_Duration if using 'infusionAllowed = TRUE' and isDuration = TRUE"
        )
      } else {
        assertthat::assert_that(!is.null(Aa_Rate),
          msg = "Must specify Aa_Rate if using 'infusionAllowed = TRUE'"
        )
      }
    }

    if (!isSequential) {
      if (parameterization == Macro1 || parameterization == Macro) {
        if (is.null(C1Obs)) {
          warning("C1Obs column not mapped")
        }
      } else {
        if (is.null(CObs)) {
          warning("CObs column not mapped")
        }
      }
    }
  }

.prepare_dotargs <- function(dotsPrepared, cols) {
  modelTermsInMapping <- names(dotsPrepared)
  if (any(duplicated(modelTermsInMapping))) {
    stop(
      "Duplicated model terms are recognized in mapping:",
      paste(modelTermsInMapping[duplicated(modelTermsInMapping)])
    )
  } else if (any(duplicated(dotsPrepared))) {
    stop(
      "Duplicated column names are recognized in mapping:",
      paste(dotsPrepared[duplicated(dotsPrepared)])
    )
  }

  modelTermsToInclude <- intersect(cols, modelTermsInMapping)
  columnNamesToInclude <- dotsPrepared[modelTermsInMapping %in% cols]

  if (length(modelTermsToInclude) != length(modelTermsInMapping)) {
    # some dots arguments are not recognized
    # try id and time
    if ("id" %in% modelTermsInMapping) {
      if ("ID" %in% modelTermsInMapping) {
        stop("Both 'ID' and 'id' are presented in mapping.")
      } else {
        idMapped <- dotsPrepared[modelTermsInMapping == "id"]
        names(idMapped) <- "ID"
        columnNamesToInclude <- c(columnNamesToInclude, idMapped)
        modelTermsToInclude <- names(columnNamesToInclude)
        modelTermsInMapping <-
          replace(modelTermsInMapping, modelTermsInMapping == "id", "ID")
      }
    }

    if ("time" %in% modelTermsInMapping && "Time" %in% cols) {
      if ("Time" %in% modelTermsInMapping) {
        stop("Both 'Time' and 'time' are presented in mapping.")
      } else {
        timeMapped <- dotsPrepared[modelTermsInMapping == "time"]
        names(timeMapped) <- "Time"
        columnNamesToInclude <- c(columnNamesToInclude, timeMapped)
        modelTermsToInclude <- names(columnNamesToInclude)
        modelTermsInMapping <-
          replace(modelTermsInMapping, modelTermsInMapping == "time", "Time")
      }
    }

    if (length(modelTermsToInclude) != length(modelTermsInMapping)) {
      warning(
        "Current model terms were not found and won't be mapped: ",
        paste(setdiff(modelTermsInMapping, modelTermsToInclude), collapse = ", ")
      )
    }
  }

  for (columnNameIndex in seq_along(columnNamesToInclude)) {
    columnName <- columnNamesToInclude[columnNameIndex]
    eval.parent(parse(text = paste0(names(columnName), " = '", columnName, "'")))
  }

  columnNamesToInclude
}

#' PK model mapping parameters
#'
#' PK model mapping parameters
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
#'  \item Models with \code{absorption} set to either \code{"Gamma"},
#'  \code{"InverseGaussian"}, or \code{"Weibull"}
#'  }
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
#'   and parameterization set to either \code{"Clearance"},\code{"Micro"} or \code{"Macro"}
#'  \item Models with \code{absorption} set to either \code{"Gamma"}, \code{"InverseGaussian"},
#'   or \code{"Weibull"} and \code{infusionAllowed = TRUE}
#'   }
#'
#' @param A1_Duration Column mapping argument that represents the input dataset column
#' for the duration of drug administered.  Only applicable to the following types of models:
#' \itemize{
#'  \item Models with \code{absorption = "Intravenous"},\code{infusionAllowed = TRUE}
#'   with \code{isDuration = TRUE} and parameterization set to either \code{"Clearance"},
#'   \code{"Micro"} or \code{"Macro"}
#'  \item Models with \code{absorption} set to either \code{"Gamma"}, \code{"InverseGaussian"},
#'   or \code{"Weibull"} and \code{infusionAllowed = TRUE} with \code{isDuration = TRUE}
#'   }
#'
#' @param Aa_Rate     Column mapping argument that represents the input dataset column
#' for the rate of drug administered and only applicable to models with \code{absorption = "FirstOrder"},
#' \code{infusionAllowed = TRUE}.
#'
#' @param Aa_Duration Column mapping argument that represents the input dataset column
#' for the duration of drug administered and only applicable to models with \code{absorption = "FirstOrder"},
#'  \code{infusionAllowed = TRUE}, and \code{isDuration = TRUE}.
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
#' @section Column mapping:
#' Note that quoted and unquoted column names are supported. Please see \code{\link{colMapping}}.
#' @keywords internal
pkmodel_MappingParameters <- function(ID = NULL,
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
                                      A0Obs = NULL) {
  NULL
}

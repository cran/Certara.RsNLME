COVAR_CONTINUOUS <- 1
Continuous <- 1
COVAR_CATEGORY <- 2
Category <- 2
COVAR_OCCASION <- 3
Occasion <- 3
CovarTypeNames <- c("Continuous", "Category", "Occasion")

COVAR_NUMBER <- 1
CovarNumber <- 1
COVAR_MEAN <- 2
CovarMean <- 2
COVAR_MEDIAN <- 3
CovarMedian <- 3
CovarEvalNames <- c("Number", "Mean", "Median")

COVAR_BACKWARD <- 1
Backward <- 1
COVAR_INTERPOLATE <- 2
Interpolate <- 2
COVAR_FORWARD <- 3
Forward <- 3

COVAR_EFF_YES <- 1
EnableEffect <- 1
COVAR_EFF_NO <- 2
DisableEffect <- 2
COVAR_EFF_PLUS_ONE <- 3
PlusOneEffect <- 3

#' Class represents an NLME Covariate parameter
#'
#' Class represents an NLME Covariate parameter
#'
#' @param name      Name of covariate
#' @param value     Covariate value
#' @keywords internal
NlmeCovarItem <- setClass(
  "NlmeCovarItem",
  slots = c(name = "character",
            value = "numeric"),
  prototype = list(name = "",
                   value = 0)
)


setMethod("initialize", "NlmeCovarItem",
          function(.Object,
                   name = "",
                   value = 0) {
            .Object@name <- name
            .Object@value <- as.integer(value)
            .Object
          })



#' NlmeCovariateParameter
#'
#' Class represents an NLME Covariate parameter
#'
#' @param name             Name of covariate parameter
#' @param type             Type of covariate parameter: Continuous|Category|Occasion
#' @param direction        Curve fitting method: Forward|Interpolate|Backward
#' @param isDiagonal       Is the structure diagonal (TRUE)| or box (FALSE)
#' @param centerValue      Covariate centering value
#' @param isPositive       Are the covariate values all positive
#' @param continuousType   Type of value to use for the centering value:
#'                              CovarNumber|CovarMean|CovarMedian
#' @param covarEffList     List of covariate effects
#' @param covarItems       List of covariate items
#'
#'
#' @keywords internal
NlmeCovariateParameter <- setClass(
  "NlmeCovariateParameter",
  slots = c(
    name = "character",
    type = "numeric",
    direction = "numeric",
    isDiagonal = "logical",
    centerValue = "character",
    # centerValue="list",
    isPositive = "logical",
    continuousType = "numeric",
    # continuousType="list",
    covarEffList = "list",
    catEffInitValues = "list",
    covarItems = "list"
  )
)


setMethod("initialize", "NlmeCovariateParameter",
          function(.Object,
                   name = "",
                   type = Continuous,
                   direction = Forward,
                   centerValue = "",
                   isDiagonal = TRUE,
                   isPositive = TRUE,
                   continuousType = CovarNumber) {
            .Object@name <- name
            .Object@isDiagonal <- isDiagonal
            .Object@type <- type
            .Object@direction <- direction
            .Object@centerValue <- centerValue
            .Object@isPositive <- isPositive
            .Object@continuousType <- continuousType
            .Object@catEffInitValues <- list()
            .Object
          })

#' Prints covariate parameter information
#'
#' Prints covariate parameter information
#'
#' @param x      Model covariate
#' @inheritParams ellipsis::dots_used
#'
#' @noRd
#' @keywords internal
print.NlmeCovariateParameter <- function(x, ...) {
  CovarDirectionNames <- c("Backward", "Interpolate", "Forward")
  type <- x@type
  if (type == COVAR_CONTINUOUS) {
    cat(paste(x@name, " : Continuous"), "\n")
    cat(paste("Direction : ", CovarDirectionNames[x@direction]), "\n")
    cat(paste("Center    : ", x@centerValue), "\n")
    cat(paste("Type      : ", CovarEvalNames[x@continuousType]), "\n")
    cat(paste("Positive? : ", x@isPositive), "\n")
  } else if (type %in% c(Category, COVAR_OCCASION)) {
    if (type == Category) {
      cat(paste(x@name, " : Category"), "\n")
    } else {
      cat(paste(x@name, " : Occasion"), "\n")
    }

    cat(paste("Direction : ", CovarDirectionNames[x@direction]), "\n")

    for (covarItem in x@covarItems) {
      cat(paste(covarItem@name, covarItem@value), "\n")
    }
  }

  cat(x@covarEffList, "\n")
}

#' Creates a string of all attributes for a covariate
#'
#' Creates a string of all attributes for a covariate
#'
#' @param obj       Model covariate
#'
#' @examples
#' \donttest{
#' covariateToString(weight)
#' }
#' @keywords internal
#' @noRd
covariateToString <- function(obj) {
  str <- ""
  type <- attr(obj, "type")
  if (type == COVAR_CONTINUOUS) {
    str <- sprintf("%s", attr(obj, "name"))
  } else if (type == Category) {
    str <- sprintf("%s(", attr(obj, "name"))
    l <- attr(obj, "covarItems")
    sep <- ""
    for (c in l) {
      str <- paste(str, sprintf("%s\"%s\"=%d", sep, attr(c, "name"),
                                attr(c, "value")))
      sep <- ","
    }
    str <- paste(str, ")")
  } else if (type == COVAR_OCCASION) {
    str <- sprintf("%s(", attr(obj, "name"))
    l <- attr(obj, "covarItems")
    sep <- ""
    for (c in l) {
      str <- paste(str, sprintf("%s\"%s\"=%d", attr(c, "name"),
                                attr(c, "value")))
      sep <- ";"
    }
  }
  return(str)
}

#' Creates string from each covariate attribute
#'
#' Creates string from each covariate attribute
#'
#' @param obj       Model covariate
#' @keywords internal
#'
covariatePartsString <- function(obj) {
  str <- ""
  type <- obj@type
  if (type == COVAR_CONTINUOUS) {
    str <- ""
  } else if (type %in% c(COVAR_CATEGORY, COVAR_OCCASION)) {
    str <- "("
    l <- obj@covarItems
    sep <- ""
    for (c in l) {
      if (c@name != "") {
        str <- paste0(str,
                      sprintf("%s\"%s\"=%d", sep, c@name, c@value))
        sep <- ", "
      }
    }
    str <- paste0(str, ")")
  }

  return(str)
}

.fill_covarCategories <-
  function(covar, categories, categoryNames) {
    if (length(categoryNames) > 0 &&
        length(categories) != length(categoryNames)) {
      stop(
        "Number of categories specified for covariate",
        name,
        "do not match with the number of labels:\n",
        "categories: ",
        paste(categories, collapse = " "),
        "\nlabels: ",
        paste(categoryNames, collapse = " ")
      )
    }

    if (length(categories) != 0) {
      covar@covarItems <-
        lapply(seq_along(categories),
               function(i, CovCategories, CovCatNames) {
                 if (length(categoryNames) == 0) {
                   NlmeCovarItem("", CovCategories[i])
                 } else {
                   NlmeCovarItem(CovCatNames[i], CovCategories[i])
                 }
               },
               categories, categoryNames)
    }

    covar
  }

#' Convenience method to create a continuous covariate
#'
#' Convenience method to create a continuous covariate
#'
#' @param name              Name of the covariate
#' @param centerValue       Covariate centering value
#' @param isPositive        Are the covariate values all positive?
#' @param direction         Curve fitting method: Forward|Interpolate|Backward
#' @param continuousType    Type of value to use for the centering value:
#'                               CovarNumber|CovarMean|CovarMedian
#'
#' @examples
#' \donttest{
#' continuousCovariate(
#'   name = "weight",
#'   centerValue = 0,
#'   isPositive = FALSE,
#'   direction = Backward,
#'   continuousType = CovarNumber
#' )
#' }
#'
#' @noRd
continuousCovariate <- function(name = "",
                                centerValue = 0,
                                isPositive = FALSE,
                                direction = Backward,
                                continuousType = CovarNumber) {
  covar <- NlmeCovariateParameter(
    name,
    type = Continuous,
    isPositive = isPositive,
    centerValue = centerValue,
    continuousType = continuousType
  )

  covar
}

#' Convenience method to create a categorical covariate
#'
#' Convenience method to create a categorical covariate
#'
#' @param name           Name of the covariate
#' @param categories     Numerical values for each category
#' @param categoryNames  Names of each category
#' @param direction      Curve fitting method: Forward|Backward|Interpolate
#'
#' @examples
#' \donttest{
#' sex <- categoricalCovariate("sex",
#'   c(1, 2),
#'   c("female", "male"),
#'   direction = Forward
#' )
#' }
#'
#' @noRd
categoricalCovariate <- function(name = "",
                                 categories = c(),
                                 categoryNames = c(),
                                 direction = Backward) {
  covar <- NlmeCovariateParameter(name,
                                  type = Category,
                                  direction = direction)

  covar <- .fill_covarCategories(covar, categories, categoryNames)

  covar
}

#' Convenience method to create an occasion covariate
#'
#' Convenience method to create an occasion covariate
#'
#' @param name           Name of the covariate
#' @param occasions      Numerical values for each occasion
#' @param occasionNames  Names of each occasion
#' @param direction      Curve fitting method: Forward|Backward|Interpolate
#' @param isDiagonal     Is the structure diagonal (TRUE) or box (FALSE)?
#'
#' @examples
#' \donttest{
#' occ <- occasionCovariate("OCC",
#'   c(1, 2),
#'   c("OCC1", "OCC2"),
#'   direction = Forward
#' )
#' }
#'
#' @noRd
occasionCovariate <- function(name = "",
                              occasions = c(),
                              occasionNames = c(),
                              direction = Backward,
                              isDiagonal = TRUE) {
  covar <- NlmeCovariateParameter(name,
                                  type = Occasion,
                                  isDiagonal = isDiagonal,
                                  direction = direction)

  covar <- .fill_covarCategories(covar, occasions, occasionNames)

  covar
}


#' Add covariate to model object
#'
#' Add a continuous, categorical, or occasion covariate to model object and set
#' covariate effect on structural parameters.
#'
#' @param .Object      Model object
#' @param covariate    Name of covariate. If the involved model has columns
#'   mapped (i.e. model with `columnMap = TRUE`) use named character if the name
#'   of the covariate is different from the corresponding column in the input
#'   dataset, for example, `covariate = c(BW = "BodyWeight")`, where `BW`
#'   denotes the name of the covariate, and `"BodyWeight"` is the name of the
#'   corresponding column in the input dataset.
#' @param effect       Name of structural parameter(s) on which the covariate
#'   has an effect.  Specify `effect` as character or character vector if the
#'   covariate has an effect on multiple structural parameters.
#' @param type         Type of covariate. Options are `"Continuous"`,
#'   `"Categorical"`, `"Occasion"`.
#' @param direction Direction of missing values propagation (if no covariate
#'   value is given). Options are `"Forward"`, `"Interpolate"`, `"Backward"`,
#'   where `"Interpolate"` is only applicable to `type = "Continuous"`.
#' @param option Options are `"Yes"`, `"PlusOne"`, or `"No"`, where
#'   `option = "No"` will remove the covariate effect from the specified
#'   structural parameter(s), but retain the covariate in the model. Note:
#'   `option = "PlusOne"` is only applicable to continuous and categorical
#'   covariates in the case where structural parameters have `style =
#'   "LogNormal"`. Multiple options are not supported (i.e. all covariate
#'   effects in the call are supposed to have the same `option`. If different
#'   `option`s are required for different covariate effects, sequential calls of
#'   current method could be done.
#' @param center       Centering method. Options are `"Mean"`, `"Median"`,
#'   `"Value"` or `"None"`. Only applicable to covariate `type = "Continuous"`.
#'   Must include argument `centerValue` if `center = "Value"`.
#' @param centerValue  Value used to center covariate. Only applicable if
#'   argument `center = "Value"` and `type = "Continuous"`.
#' @param levels Unique values of categorical or occasion covariate.  Only
#'   applicable to covariate `type = "Categorical"` or `type = "Occasion"`.
#' @param labels Label names (in the same order as levels) for unique levels of
#'   categorical or occasion covariate in data. Only applicable to covariate
#'   `type = "Categorical"`  or `type = "Occasion"` where its corresponding
#'   column in the input dataset has character type.
#' @param isDiagonal Set to `FALSE` if inter-occasion covariance matrix is not
#'   diagonal matrix. Only applicable to covariate `type = "Occasion"`.
#' @param values Initial values for the diagonal elements of the inter-occasion
#'   covariance matrix (if `isDiagonal = TRUE`) or initial values for the lower
#'   triangular elements (including diagonal elements) of inter-occasion
#'   covariance matrix (if `isDiagonal = FALSE`) in a row-wise order. Only
#'   applicable for covariate `type = "Occasion"`.
#' @param isPositive   Set to `FALSE` if covariate contains negative values.
#'   Only applicable to covariate `type = "Continuous"`.
#'
#' @details The following relationships are applicable for covariates:
#' * `direction = "Forward"` is equivalent to PML code 'fcovariate(CovName)';
#' * `direction = "Backward"` is equivalent to PML code 'covariate(CovName)';
#' * `direction = "Interpolate"` is equivalent to PML code 'interpolate(CovName)'.
#'
#'   If the structural parameter has `style = "LogNormal"`, the options are
#'   reflected in PML code as follows:
#' * `option = "Yes"` is equivalent to
#'   `stparm(V = tvV * wt^dVdwt * exp(dVdsex1*(sex==1)) * exp(nV))`;
#' * `option = "PlusOne` is equivalent to
#'   `stparm(V = tvV * (1+wt*dVdwt) * (1+dVdsex1*(sex==1)) * exp(nV))`.
#'
#' @return Modified \code{NlmePmlModel} object
#' @examples
#' model <- pkmodel(
#'   numCompartments = 2,
#'   data = pkData,
#'   ID = "Subject",
#'   Time = "Act_Time",
#'   A1 = "Amount",
#'   CObs = "Conc"
#' )
#'
#' # Add Gender covariate of type categorical
#' model <- addCovariate(model,
#'   covariate = "Gender",
#'   type = "Categorical",
#'   effect = c("V2", "Cl2"),
#'   levels = c(0, 1),
#'   labels = c("Female", "Male")
#' )
#'
#' # Add BodyWeight covariate of type continuous
#' model <- addCovariate(model,
#'   covariate = "BodyWeight",
#'   type = "Continuous",
#'   direction = "Backward",
#'   center = "Mean",
#'   effect = c("V", "Cl")
#' )
#' @import data.table
#' @md
#' @export
addCovariate <- function(.Object,
                         covariate,
                         effect = NULL,
                         type = c("Continuous", "Categorical", "Occasion"),
                         direction = c("Forward", "Interpolate", "Backward"),
                         option = c("Yes", "PlusOne", "No"),
                         center = NULL,
                         centerValue = NULL,
                         levels = NULL,
                         labels = NULL,
                         isDiagonal = TRUE,
                         values = NULL,
                         isPositive = TRUE) {
  if (!inherits(.Object, "NlmePmlModel")) {
    stop("Object must be of class 'NlmePmlmodel'")
  }

  if (.Object@isTextual) {
    stop("`addCovariate()` cannot be used for edited or textual models")
  }

  if (!missing(type) && length(type) > 1) {
    stop(
      "Vectorized type argument is not supported. ",
      "If different types are required, please use sequential addCovariate calls."
    )
  }

  type <- match.arg(type)

  if (!missing(direction) && length(direction) > 1) {
    stop(
      "Vectorized direction argument is not supported. ",
      "If different directions are required, please use sequential addCovariate calls."
    )
  }

  direction <- match.arg(direction)

  if (!missing(option) && length(option) > 1) {
    stop(
      "Vectorized option argument is not supported. ",
      "If different options are required, please use sequential addCovariate calls."
    )
  }

  option <- match.arg(option)

  if (is.null(names(covariate))) {
    names(covariate) <- covariate
  }

  cov <- names(covariate)
  covMappedColumn <- covariate

  stpnames <- structuralParameterNames(.Object)
  if (cov %in% stpnames) {
    stop(paste0("covariate '", cov, "' exists in structural parameter names"))
  }

  covAvail <- FALSE
  mdata <- NULL
  . <- NULL # R CMD CHECK

  if (!is.null(values)) {
    if (isDiagonal) {
      if (any(values <= 0)) {
        stop("supplied `values` must be positive")
      }
    } else {
      if (any(values < 0)) {
        stop(
          "`isDiagonal = FALSE` supplied `values` for block matrix must be greater than or equal to 0 "
        )
      }
    }
  }

  `%notin%` <- Negate(`%in%`)

  if (!is.null(.Object@inputData)) {
    mdata <- as.data.frame(.Object@inputData)

    # Check if covariate is missing from data or duplicated in data
    data_cols <- colnames(mdata)

    if (covMappedColumn %notin% data_cols && option != "No") {
      covAvail <- FALSE
      warning(
        paste0(
          "Column '",
          covMappedColumn,
          "' not found in data. Please manually map '",
          covMappedColumn,
          "' before model execution using `modelColumnMapping` function."
        )
      )
    } else {
      covAvail <- TRUE
    }

    if (covAvail &&
        option != "No" &&
        inherits(mdata[, covMappedColumn], "numeric") &&
        !is.null(labels)) {
      stop("`labels` argument specified for covariate type numeric")
    }
  }

  if (isPositive == FALSE && option == "Yes") {
    stop("`option` = 'Yes' only applicable if `isPositive` = TRUE")
  }

  structuralParams <- .Object@structuralParams

  for (p in seq_along(structuralParams)) {
    if (structuralParams[[p]]@name %in% effect &&
        option == "PlusOne") {
      if (type == "Occasion") {
        stop("`option` = 'PlusOne' is not applicable to occasion covariates")
      }

      if (structuralParams[[p]]@style != LogNormal) {
        stop(
          "`option` = 'PlusOne' only applicable to structural parameters with `style` = 'LogNormal', ",
          structuralParams[[p]]@name,
          " structural parameter has another 'style'."
        )
      }
    }
  }

  if (!is.null(center) && length(center) > 1) {
    stop("multiple values specified to argument `center`")
  }

  if (!is.null(centerValue)) {
    if (center != "Value") {
      stop("Must set argument center = 'Value' if supplying `centerValue` argument")
    }
    if (!is.numeric(centerValue)) {
      stop("`centerValue` must be of type numeric")
    }
  }

  if (!is.null(levels)) {
    if (any(levels < 0)) {
      stop("values specified to argument `levels` cannot be negative")
    }
    if (any(levels %% 1 != 0)) {
      stop("values specified to argument `levels` cannot be a fraction")
    }
    if (length(unique(levels)) == 1) {
      stop("values specified to argument `levels` must be unique")
    }
  }

  if (is.null(center) || center == "None") {
    center <- CovarNumber
    centerValue <- ""
    # centerValue <- as.character(mean(mdata[,covariate], na.rm = TRUE))
  } else if (center == "Median") {
    center <- CovarMedian
    centerValue <- ""
    # centerValue <- as.character(median(mdata[,covariate], na.rm = TRUE))
  } else if (center == "Value") {
    if (is.null(centerValue)) {
      stop("Must specify argument `centerValue` if using `center` = 'Value'")
    }
    center <- CovarNumber
    centerValue <- as.character(centerValue)
  } else if (center == "Mean") {
    center <- CovarMean
    centerValue <- ""
  } else {
    stop("Argument `center` must be one of 'Mean', 'Median', 'Value', or 'None'")
  }

  if (direction == "Forward") {
    direction <- Forward
  } else if (direction == "Backward") {
    direction <- Backward
  } else {
    # if (direction == "Interpolate")
    direction <- Interpolate
    if (type != "Continuous") {
      stop("argument `direction` = 'Interpolate' only applicable for `type` = 'Continuous'")
    }
  }

  if (!is.null(.Object@columnMapping@mapping$id) &&
      .Object@columnMapping@mapping$id@columnName != "?") {
    id_col_names <-
      gsub(" ", "", .Object@columnMapping@mapping$id@columnName)
  }
  dt <- data.table::setDT(.Object@inputData)
  if (type == "Continuous") {
    type <- Continuous
    if (!is.null(mdata) && covAvail) {
      if (!is.null(.Object@columnMapping@mapping$id) &&
          .Object@columnMapping@mapping$id@columnName != "?") {
        numCovariatesData <- dt[,
                                .(group_column_name = any(unlist(lapply(
                                  .SD, .is_numeric
                                )))),
                                by = c(id_col_names),
                                .SDcols = c(covMappedColumn)]

        df <-
          numCovariatesData[group_column_name == FALSE |
                              is.na(group_column_name),]
        if (nrow(df) > 0) {
          df$group_column_name <- NULL
          if (ncol(df) == 1) {
            stop(
              paste0(
                "Error: continuous covariate ",
                cov,
                " must have at least one numeric value assigned for each subject \n",
                "Following subject(s) does not have one: ",
                paste0(df[[.Object@columnMapping@mapping$id@columnName]], collapse = ", ")
              )
            )
          } else {
            sub_ids <- ""
            for (row in 1:nrow(df)) {
              sub_ids <-
                paste0(sub_ids,
                       paste0(colnames(df), "=", df[row,], collapse = " "),
                       ";")
            }
            stop(
              paste0(
                "Error: continuous covariate ",
                covariate,
                " must have at least one numeric value assigned for each subject \n",
                "Following subject(s) does not have one: ",
                sub_ids
              )
            )
          }
        }
      }
    }
  } else if (type == "Categorical") {
    type <- Category
    if (is.null(levels) && option != "No") {
      stop("must specify `levels` argument if `type` = 'Categorical' ")
    }

    if (center != CovarNumber) {
      stop("argument `center` not applicable to covariate `type` = 'Categorical'")
    }
  } else {
    # if (type == "Occasion")
    type <- Occasion
    if (is.null(levels) && option != "No") {
      stop("must specify `levels` argument if `type` = 'Occasion' ")
    }
    if (center != CovarNumber) {
      stop("argument `center` not applicable to covariate `type` = 'Occasion'")
    }
    if ("PlusOne" %in% option) {
      stop("argument `option` = 'PlusOne' is not applicable for `type` = 'Occasion'")
    }
  }

  if (type == Occasion || type == Category) {
    if (!is.null(mdata) && covAvail) {
      if (is.null(labels) && option != "No") {
        if (!is.null(.Object@columnMapping@mapping$id) &&
            .Object@columnMapping@mapping$id@columnName != "?") {
          numCovariatesData <- dt[,
                                  .(group_column_name = any(unlist(lapply(
                                    .SD, .is_numeric
                                  )))),
                                  by = id_col_names,
                                  .SDcols = c(covMappedColumn)]
          df <-
            numCovariatesData[group_column_name == FALSE |
                                is.na(group_column_name),]
          if (nrow(df) > 0) {
            df$group_column_name <- NULL
            if (ncol(df) == 1) {
              stop(
                paste0(
                  "Error: must specify `labels` argument for covariate ",
                  cov,
                  " or set numerical values for subject(s): ",
                  paste0(df[[.Object@columnMapping@mapping$id@columnName]], collapse = ", ")
                )
              )
            } else {
              sub_ids <- ""
              for (row in 1:nrow(df)) {
                sub_ids <-
                  paste0(sub_ids,
                         paste0(colnames(df), "=", df[row,], collapse = " "),
                         ";")
              }
              stop(
                paste0(
                  "Error: must specify `labels` argument for covariate ",
                  cov,
                  " or set numerical values for subject(s): ",
                  sub_ids
                )
              )
            }
          }
        }
      } else if (!is.null(labels)) {
        checkLabels(.Object, labels, covMappedColumn, cov)
      }
    }

    if (is.null(labels) && is.null(mdata)) {
      warning(
        "Please specify `labels` argument for covariate if corresponding column in the input dataset is of type `character`."
      )
    }
  }

  if (option == "Yes") {
    optionNum <- EnableEffect
  } else if (option == "PlusOne") {
    optionNum <- PlusOneEffect
  } else {
    # if (option == "No")
    optionNum <- DisableEffect
  }

  covList <- .Object@covariateList
  covName <- c()
  for (i in seq_along(covList)) {
    covName[i] <- covList[[i]]@name
  }

  if (covMappedColumn %notin% covName &&
      optionNum == DisableEffect) {
    stop(paste0(
      "covariate '",
      covMappedColumn,
      "' not found in existing covariate names"
    ))
  }

  if (is.null(effect)) {
    optionNum <- DisableEffect
  } else {
    for (i in seq_along(effect)) {
      if (effect[[i]] %notin% structuralParameterNames(.Object)) {
        stop(paste0(
          "effect '",
          effect[[i]],
          "' not found in structural parameters"
        ))
      }
    }
  }

  if (cov %in% covName) {
    # Check if Covariate already exists and update
    for (i in seq_along(covList)) {
      if (!covList[[i]]@name %in% cov)
        next

      # covList[[i]]@type <- type #Removed so user cannot change existing covar type
      covList[[i]]@direction <- direction
      covList[[i]]@isDiagonal <- isDiagonal
      covList[[i]]@isPositive <- isPositive
      covList[[i]]@centerValue <- centerValue
      covList[[i]]@continuousType <- center

      currentCovEffNames <- names(covList[[i]]@covarEffList)
      for (stParm in effect) {
        if (stParm %in% currentCovEffNames &&
            covList[[i]]@covarEffList[[stParm]] != optionNum) {
          message(
            "updating option argument for covariate effect of ",
            covariate,
            " in structural parameter ",
            stParm,
            " to '",
            option,
            "'"
          )
        }

        covList[[i]]@covarEffList[[stParm]] <- optionNum
      }
    }

    .Object@covariateList <- covList
  } else {
    # Add new covariate if covariate doesn't exist
    if (type == Category) {
      covName <-
        categoricalCovariate(
          name = cov,
          categories = levels,
          categoryNames = labels,
          direction = direction
        )
    } else if (type == Occasion) {
      covName <-
        occasionCovariate(
          name = cov,
          occasions = levels,
          occasionNames = labels,
          direction = direction,
          isDiagonal = isDiagonal
        )
    } else {
      # if (type == Continuous)
      covName <- NlmeCovariateParameter(
        name = cov,
        type = Continuous,
        continuousType = center,
        centerValue = as.character(centerValue),
        direction = direction,
        isPositive = isPositive,
        isDiagonal = isDiagonal
      )
    }

    coveffect <- covName@name
    coveffects <-
      vector(mode = "character", length = length(effect))
    for (i in seq_along(effect)) {
      coveffects[[i]] <- c(coveffect)
    }
    names(coveffects) <- effect

    .Object <-
      addCovariates(.Object,
                    covariates = c(covName),
                    effects = c(coveffects))

    if (!is.null(effect)) {
      for (i in seq_along(effect)) {
        covariateEffect(.Object, cov, effect[[i]]) <- optionNum
      }
    }
  }

  # Add random effect values specified for occasion covariate
  .Object <- generatePMLModel(.Object)
  if (type == Occasion && optionNum == EnableEffect) {
    if (is.null(values)) {
      if (isDiagonal) {
        values <- rep(1, length(effect))
      } else {
        diagMatrix <- diag(1, nrow = length(effect), ncol = length(effect))
        values <- diagMatrix[upper.tri(diagMatrix, diag = TRUE)]
      }
    }
    if (isDiagonal) {
      assertthat::assert_that(length(values) == length(effect), msg = "arguments `effect` and `values` are vectors of different lengths")
    } else {
      covMatrix <- matrix(NA, length(effect), length(effect))
      tryCatch({
        covMatrix[upper.tri(covMatrix, diag = TRUE)] <- values
      },
      error = function(e) {
        stop(
          "isDiagonal = FALSE; incorrect number of `values` specified for corresponding `effect` in block covariance matrix"
        )
      },
      warning = function(w) {
        stop(
          "isDiagonal = FALSE; incorrect number of `values` specified for corresponding `effect` in block covariance matrix"
        )
      })
      covMatrix[lower.tri(covMatrix)] <-
        t(covMatrix)[lower.tri(covMatrix)]
      if (!.is.positive.definite(covMatrix)) {
        stop(
          "block covariance matrix specified is not positive definite for supplied `values` argument"
        )
      }
    }
    initOccasionRandomEffect(.Object, covMappedColumn) <- values
  }

  if (covAvail) {
    # && optionNum != DisableEffect){
    .check_column_mappings(covMappedColumn, data = mdata)
    mappedColumn(.Object, cov) <- paste0(covMappedColumn)
    .Object@columnMapping@mapping[[cov]]@variableType <-
      list(type = "covariate",
           covType = type)
  } else {
    if (!is.null(.Object@columnMapping@mapping[[cov]])) {
      columnName <- .Object@columnMapping@mapping[[cov]]@columnName
    } else {
      columnName <- "?"
    }

    .Object@columnMapping@mapping[[cov]] <- NlmeColumnMap(
      variableName = cov,
      columnName = columnName,
      variableType = list(type = "covariate",
                          covType = type)
    )
  }

  return(.Object)
}

checkCatCovariateMappingColumn <-
  function(map,
           inputData,
           columnToMap,
           modelTermName) {
    if (!is.null(map$id) && map$id@columnName != "?") {
      id_col_names <- gsub(" ", "", map$id@columnName)
      dt <- data.table::setDT(inputData)

      numCovariatesData <- dt[,
                              .(group_column_name = any(unlist(lapply(
                                .SD, .is_numeric
                              )))),
                              by = id_col_names,
                              .SDcols = c(columnToMap)]
      df <-
        numCovariatesData[group_column_name == FALSE |
                            is.na(group_column_name),]
      if (nrow(df) > 0) {
        df$group_column_name <- NULL
        if (ncol(df) == 1) {
          if (map[[modelTermName]]@variableType$covType != COVAR_CONTINUOUS) {
            emptyCovariatesData <- dt[,
                                      .(group_column_name = any(unlist(lapply(.SD, function(x) {
                                        any(grepl("\\s", x)) ||
                                          any(x == "") ||
                                          any(x == "NA") || any(x == "na") || any(x == ".")
                                      })))),
                                      by = id_col_names,
                                      .SDcols = c(columnToMap)]
            df1 <-
              emptyCovariatesData[group_column_name == TRUE |
                                    is.na(group_column_name),]
            if (nrow(df1) == 0) {
              warning(
                paste0(
                  "The corresponding data column ",
                  columnToMap,
                  " for covariate ",
                  modelTermName,
                  " is of class character. Please specify the name and the associated value for each category through 'addLabel' function "
                )
              )
            } else {
              df1$group_column_name <- NULL
              warning(
                paste0(
                  "The corresponding data column ",
                  columnToMap,
                  " for covariate ",
                  modelTermName,
                  " does not contain any valid value for subject(s): ",
                  paste0(df1[[map$id@columnName]], collapse = ", ")
                )
              )
            }
          } else {
            warning(
              paste0(
                "The corresponding data column ",
                columnToMap,
                " for covariate ",
                modelTermName,
                " does not contain any valid value for subject(s): ",
                paste0(df[[map$id@columnName]], collapse = ", ")
              )
            )
          }
        } else {
          sub_ids <- ""
          for (row in 1:nrow(df)) {
            sub_ids <-
              paste0(sub_ids,
                     paste0(colnames(df), "=", df[row,], collapse = " "),
                     ";")
          }
          if (map[[modelTermName]]@variableType$covType != COVAR_CONTINUOUS) {
            emptyCovariatesData <- dt[,
                                      .(group_column_name = any(unlist(lapply(.SD, function(x) {
                                        grepl("\\s", x) || x == "" || x == "NA" || x == "na" || x == "."
                                      })))),
                                      by = id_col_names,
                                      .SDcols = c(columnToMap)]
            df1 <-
              emptyCovariatesData[group_column_name == TRUE |
                                    is.na(group_column_name),]
            if (nrow(df1) == 0) {
              warning(
                paste0(
                  "The corresponding data column ",
                  columnToMap,
                  " for covariate ",
                  modelTermName,
                  " is of class character. Please specify the name and the associated value for each category through 'addLabel' function "
                )
              )
            } else {
              warning(
                paste0(
                  "The corresponding data column ",
                  columnToMap,
                  " for covariate ",
                  modelTermName,
                  " does not contain any valid value for subject(s): ",
                  sub_ids
                )
              )
            }
          } else {
            warning(
              paste0(
                "The corresponding data column ",
                columnToMap,
                " for covariate ",
                modelTermName,
                " does not contain any valid value for subject(s): ",
                sub_ids
              )
            )
          }
        }
      }
    }
  }

checkLabels <- function(model, labels, covMappedColumn, cov) {
  if (!is.null(model@columnMapping@mapping$id) &&
      model@columnMapping@mapping$id@columnName != "?") {
    dt <- data.table::setDT(model@inputData)
    id_col_names <-
      gsub(" ", "", model@columnMapping@mapping$id@columnName)
    covLevelsData <-
      dt[, .(group_column_name = any(unlist(lapply(.SD, tolower)) %in% tolower(labels)) ||
               any(unlist(lapply(
                 .SD, .is_numeric
               )))),
         by = id_col_names,
         .SDcols = c(covMappedColumn)]

    df <-
      covLevelsData[group_column_name == FALSE |
                      is.na(group_column_name),]
    if (nrow(df) > 0) {
      df$group_column_name <- NULL
      if (ncol(df) == 1) {
        stop(
          paste0(
            "The corresponding data column ",
            covMappedColumn,
            " for covariate ",
            cov,
            " does not contain any valid value for subject(s)  ",
            paste0(df[[model@columnMapping@mapping$id@columnName]], collapse = ", ")
          )
        )
      } else {
        sub_ids <- ""
        for (row in 1:nrow(df)) {
          sub_ids <-
            paste0(sub_ids,
                   paste0(colnames(df), "=", df[row,], collapse = " "),
                   ";")
        }
        stop(
          paste0(
            "The corresponding data column ",
            covMappedColumn,
            " for covariate ",
            cov,
            " does not contain any valid value for subject(s)  ",
            sub_ids
          )
        )
      }
    }
  }
}


#' Remove covariate from structural parameters in a model object.
#'
#' Remove one or more covariates from structural parameters in a model object.
#'
#' @param .Object Model object
#' @param covariate Covariates to remove from model. If \code{NULL} all covariates will be removed from model.
#' @param paramName Structural parameters for which to remove covariate effect(s) from. If \code{NULL} covariate effect will be removed from all structural parameters.
#'
#' @return Modified \code{NlmePmlModel} object
#' @examples
#' model <- pkmodel(
#'   numCompartments = 2,
#'   data = pkData,
#'   ID = "Subject",
#'   Time = "Act_Time",
#'   A1 = "Amount",
#'   CObs = "Conc"
#' )
#'
#' # Add Gender covariate of type categorical
#' model <- addCovariate(model,
#'   covariate = "Gender",
#'   type = "Categorical",
#'   effect = c("V2", "Cl2"),
#'   levels = c(0, 1),
#'   labels = c("Female", "Male")
#' )
#'
#' # Add BodyWeight covariate of type continuous
#' model <- addCovariate(model,
#'   covariate = "BodyWeight",
#'   type = "Continuous",
#'   direction = "Backward",
#'   center = "Mean",
#'   effect = c("V", "Cl")
#' )
#'
#' # Remove all covariates from model
#' model <- removeCovariate(model)
#'
#' @export
removeCovariate <-
  function(.Object,
           covariate = NULL,
           paramName = NULL) {
    if (is.null(covariate) && is.null(paramName)) {
      .Object <- resetCovariateEffects(.Object)
      .Object@covariateList <- list()
      .Object <- generatePML(.Object)
      return(.Object)
    }

    covList <- .Object@covariateList
    covNames <- vector(mode = "list", length = length(covList))

    `%notin%` <- Negate(`%in%`)

    for (i in seq_along(covList)) {
      covNames[[i]] <- covList[[i]]@name
    }

    for (i in seq_along(covariate)) {
      if (covariate[[i]] %notin% covNames) {
        stop(paste0(
          "covariate '",
          covariate[[i]],
          "' not found in existing covariate names"
        ))
      }
    }

    if (!is.null(covariate) && is.null(paramName)) {
      for (i in seq_along(covList)) {
        if (any(covariate %in% covList[[i]]@name)) {
          .Object@columnMapping@mapping[[covList[[i]]@name]] <- NULL
          covList[[i]] <- list()
        }
      }
      covList <- covList[lapply(covList, length) > 0]
      .Object@covariateList <- covList
    }

    if (!is.null(covariate) && !is.null(paramName)) {
      for (i in seq_along(covList)) {
        if (any(covariate %in% covList[[i]]@name)) {
          for (j in seq_along(covList[[i]]@covarEffList)) {
            if (any(paramName %in% names(covList[[i]]@covarEffList[j]))) {
              covList[[i]]@covarEffList[j] <- NULL
            }
          }
        }
      }
    }

    .Object@covariateList <- covList

    .Object <- generatePML(.Object)
    .Object
  }

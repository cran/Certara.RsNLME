
SteadyStateDose <- 1

AddlDose <- 2

DoseTypeNames <- c("Steady State", "ADDL")

BolusDose <- 1
InfusionDose <- 2

DoseItemTypeNames <- c("Bolus", "Infusion")

ValueType <- 1

ColumnType <- 2


#' Creates an extra dose parameter
#'
#' Creates an extra dose parameter
#'
#' @param type       ValueType|ColumnType
#' @param value      Value of dosing parameter
#' @param column     Name of column containing dose information
#' @keywords internal
ExtraDoseDataType <- setClass("ExtraDoseDataType",
  slots = c(
    type = "numeric",
    value = "numeric",
    column = "character"
  )
)

setMethod("initialize", "ExtraDoseDataType",
  function(.Object,
           type = ValueType,
           value = 0,
           column = "") {
    .Object@type <- type
    .Object@value <- value
    .Object@column <- column
    .Object
  }
)

#' Defines an extra dose point
#'
#' Defines an extra dose point
#'
#' @slot name              Dose point name
#' @slot type              Character; Options are "Bolus" or "Infusion"
#' @slot amount            Optional; Character specifying corresponding column in data or numeric specifying dose amount
#' @slot rate              Optional; Character specifying corresponding column in data or numeric specifying dose rate
#' @slot deltaTime         Optional; Character specifying corresponding column in data or numeric specifying delta time
#' @slot isSecondDose      Use second dose point on compartment?
#' @slot data              Optional data frame. Must specify \code{data} if supplying column as character value to \code{amount, rate, deltaTime} arguments
#' @keywords internal
setClass("ExtraDoseItem",
  slots = c(
    name = "character",
    type = "numeric",
    amount = "ExtraDoseDataType",
    rate = "ExtraDoseDataType",
    deltaTime = "ExtraDoseDataType",
    duration = "ExtraDoseDataType",
    isSecondDose = "logical"
  )
) -> ExtraDoseItem


setMethod("initialize", "ExtraDoseItem",
  function(.Object,
           name = "",
           type = "Bolus",
           amount = NULL,
           rate = NULL,
           deltaTime = NULL,
           duration = NULL,
           isSecondDose = FALSE,
           data = NULL) {
    if (type == "Bolus") {
      type <- BolusDose
    } else if (type == "Infusion") {
      type <- InfusionDose
    } else {
      stop("argument `type` must be one of 'Bolus' or 'Infusion'")
    }

    .Object@type <- type
    .Object@name <- name
    .Object@isSecondDose <- isSecondDose

    if (length(data) > 0) {
      mdata <- as.data.frame(data)
    } else {
      mdata <- NULL
    }


    if (!is.null(amount)) {
      if (is.character(amount)) {
        if (!is.null(mdata)) {
          .check_column_mappings(amount, mdata)
        }
        .Object@amount <- ExtraDoseDataType(type = ColumnType, column = amount)
      } else if (is.numeric(amount)) {
        .Object@amount <- ExtraDoseDataType(type = ValueType, value = amount)
      } else {
        stop("argument `amount` is not of type `numeric` or `character`")
      }
    }

    if (!is.null(rate)) {
      if (is.character(rate)) {
        if (!is.null(mdata)) {
          .check_column_mappings(rate, mdata)
          if (any(mdata[!is.na(mdata[[rate]]), rate] <= 0)) {
            stop("value in `rate` column must be greater than 0")
          }
        }
        .Object@rate <- ExtraDoseDataType(type = ColumnType, column = rate)
      } else if (is.numeric(rate)) {
        if (rate <= 0) {
          stop("value supplied for `rate` must be greater than 0")
        }
        .Object@rate <- ExtraDoseDataType(type = ValueType, value = rate)
      } else {
        stop("argument `rate` is not of type `numeric` or `character`")
      }
    }

    if (!is.null(duration)) {
      if (is.character(duration)) {
        if (!is.null(mdata)) {
          .check_column_mappings(duration, mdata)
          if (any(mdata[!is.na(mdata[[duration]]), duration] <= 0)) {
            stop("value in `duration` column must be greater than 0")
          }
        }
        .Object@duration <- ExtraDoseDataType(type = ColumnType, column = duration)
      } else if (is.numeric(duration)) {
        if (duration <= 0) {
          stop("value supplied for `duration` must be greater than 0")
        }
        .Object@duration <- ExtraDoseDataType(type = ValueType, value = duration)
      } else {
        stop("argument `duration` is not of type `numeric` or `character`")
      }
    }

    if (!is.null(deltaTime)) {
      if (is.character(deltaTime)) {
        if (!is.null(mdata)) {
          .check_column_mappings(deltaTime, mdata)
        }
        .Object@deltaTime <- ExtraDoseDataType(type = ColumnType, column = deltaTime)
      } else if (is.numeric(deltaTime)) {
        .Object@deltaTime <- ExtraDoseDataType(type = ValueType, value = deltaTime)
      } else {
        stop("argument `II` is not of type `numeric` or `character`")
      }
    }

    .Object@isSecondDose <- isSecondDose
    .Object
  }
)

#' Prints extra dose information
#'
#' Prints extra dose information
#'
#' @noRd
#' @keywords internal
print.ExtraDoseItem <- function(x, ...) {
  name <- x@name
  type <- DoseItemTypeNames[x@type]
  cat(paste0(name), "\n")
  cat(paste0("Type       : ", type), "\n")

  cat("Amount     : ",  "\n")
  cat(x@amount,  "\n")
  if (type == InfusionDose) {
    cat("Rate       : ", "\n")
    cat(x@rate, "\n")
  }
  cat("Delta Time : ", "\n")
  cat(x@deltaTime, "\n")
}

#' Additional dosing information
#'
#' Additional dosing information
#'
#' @param name       Dose name
#' @param doseType   SteadyStateDose|AddlDose
#' @param doses      List of treatment doses
#'
#' @examples
#' \donttest{
#' dose <- ExtraDoseItem(
#'   type = BolusDose,
#'   amountColumn = "Aa",
#'   deltaTimeColumn = "ii"
#' )
#'
#' dose1 <- ExtraDoseItem(
#'   type = InfusionDose,
#'   amount = 12,
#'   rate = 4,
#'   deltaTime = 1,
#'   isSecondDose = TRUE
#' )
#'
#' dosePoint <- ExtraDoseOption(
#'   name = "SteadyState",
#'   doseType = SteadyStateDose,
#'   doses = c(dose, dose1)
#' )
#'
#' model <- addExtraDose(model, dosePoint)
#' }
#' @keywords internal
#' @noRd
setClass("ExtraDoseOption",
  slots = c(
    name = "character",
    doseType = "numeric",
    doses = "ANY"
  )
) -> ExtraDoseOption


setMethod("initialize", "ExtraDoseOption",
  function(.Object,
           name = "",
           doseType = SteadyStateDose,
           doses = NULL) {
    .Object@name <- name
    .Object@doseType <- doseType
    .Object@doses <- doses
    .Object
  }
)

#' Prints any additional information for extra dose
#'
#' Prints any additional information for extra dose
#'
#' @param x   ExtraDoseOption object
#' @inheritParams ellipsis::dots_used
#' @keywords internal
#' @noRd
print.ExtraDoseOption <- function(x, ...) {
  name <- attr(x, "name")
  type <- DoseTypeNames[attr(x, "doseType")]
  cat(paste0(name, "  ", type), "\n")
  for (d in x@doses) {
    print(d)
  }
}


#' Adds MDV extra column definition to model object
#'
#' Use to add MDV statement to \code{model@userDefinedExtraDefs}
#'
#' @param .Object Model object
#' @param MDV   Column mapping argument specifying corresponding "MDV" column in input data set
#'
#' @return Modified \code{NlmePmlModel} object
#' @examples
#' \donttest{
#' pkData1 <- pkData
#' pkData1$MDV <- 0
#' model <- pkmodel(data = pkData1,
#'                  ID = "Subject",
#'                  Time = "Act_Time",
#'                  A1 = "Amount",
#'                  CObs = "Conc",
#'                  workingDir = tempdir()
#'                  )
#' model <- addMDV(model, MDV = "MDV")
#' }
#' @export
addMDV <- function(.Object, MDV) {
  existing_def <- .Object@userDefinedExtraDefs

  mdata <- .Object@inputData

  if (missing(MDV)) {
    stop("Missing `MDV` column mapping argument")
  }

  if (!is.null(mdata)) {
    cols <- c(MDV)
    .check_column_mappings(cols, mdata)
  }

  mdv_def <- paste0("mdv(\"", MDV, "\")")

  defs <- c(existing_def, mdv_def)

  .Object@userDefinedExtraDefs <- unique(defs)
  return(.Object)
}

#' Adds ADDL extra column definition to model object
#'
#' Specify ADDL column definition in model object instead of specifying ADDL through \code{\link{addDoseCycle}}
#'
#' @param .Object Model object
#' @param ADDL Column mapping argument specifying corresponding "ADDL" column in input data set
#' @param II   Column mapping argument specifying corresponding "II" column in input data set
#'
#' @return Modified \code{NlmePmlModel} object
#' @examples
#' \donttest{
#' pkData1 <- pkData
#' pkData1$ii <- 0
#' pkData1$addl <- 0
#'  model <- pkmodel(numComp = 2,
#'                   absorption = "FirstOrder",
#'                   ID = "Subject",
#'                   Time = "Act_Time",
#'                   CObs = "Conc",
#'                   Aa = "Amount",
#'                   data = pkData1,
#'                   modelName = "PkModel",
#'                   workingDir = tempdir())
#'  model <- addADDL(model, ADDL = "addl", II = "ii")
#' }
#'
#' @export
addADDL <- function(.Object, ADDL, II) {
  existing_def <- .Object@userDefinedExtraDefs

  mdata <- .Object@inputData

  if (!is.null(mdata)) {
    cols <- c(ADDL, II)
    .check_column_mappings(cols, mdata)
  }

  userDefinedExtraDefinitions(.Object) <- c(
    paste0("addlcol(", ADDL, ")"),
    paste0("iicol(", II, ")")
  )


  current_def <- .Object@userDefinedExtraDefs
  defs <- c(existing_def, current_def)

  pos <- grep("iicol", defs)
  if (length(pos) > 1) {
    if (length(unique(defs[pos])) > 1) {
      stop("cannot use different 'II' columns for steady state and ADDL")
    }
  }

  .Object@userDefinedExtraDefs <- unique(defs)

  return(.Object)
}

#' Adds Steady State extra column definition to model object
#'
#' Use to add Steady State column definition statement to \code{model@userDefinedExtraDefs}
#'
#' @param .Object   Model object
#' @param SS        Column mapping argument specifying corresponding "SS" column in input data set
#' @param II        Column mapping argument specifying corresponding "II" column in input data set
#' @param SSOffset  Optional. Column mapping argument specifying corresponding "SSOffset" column in input data set
#'
#' @return Modified \code{NlmePmlModel} object
#' @examples
#' \donttest{
#' pkData1 <- pkData
#' pkData1$SS <- 0
#' pkData1$II <- 0
#' model <- pkmodel(data = pkData1,
#'                  ID = "Subject",
#'                  Time = "Act_Time",
#'                  A1 = "Amount",
#'                  CObs = "Conc",
#'                  workingDir = tempdir()
#'                  )
#' model <- addSteadyState(model, SS = "SS", II = "II")
#' }
#' @export
addSteadyState <- function(.Object, SS, II, SSOffset = NULL) {
  existing_def <- .Object@userDefinedExtraDefs

  mdata <- .Object@inputData

  if (!is.null(mdata)) {
    cols <- c(SS, II, SSOffset)
    .check_column_mappings(cols, mdata)
  }

  if (is.null(SSOffset)) {
    userDefinedExtraDefinitions(.Object) <- c(
      paste0("sscol(", SS, ")"),
      paste0("iicol(", II, ")")
    )
  } else {
    userDefinedExtraDefinitions(.Object) <- c(
      paste0("sscol(", SS, ")"),
      paste0("iicol(", II, ")"),
      paste0("ssoffcol(", SSOffset, ")")
    )
  }


  current_def <- .Object@userDefinedExtraDefs
  defs <- c(existing_def, current_def)

  pos <- grep("iicol", defs)
  if (length(pos) > 1) {
    if (length(unique(defs[pos])) > 1) {
      stop("cannot use different 'II' columns for steady state and ADDL")
    }
  }

  .Object@userDefinedExtraDefs <- unique(defs)

  # Not adding @mapping for v1 release
  # if(!is.null(mdata)){
  # variableTypeList <- list(type = "extraDoses",
  #                          ii = II)
  #
  #
  # .Object@columnMapping@mapping$SteadyState <-
  #   NlmeColumnMap(variableName = "SteadyState",
  #                 columnName = SS,
  #                 variableType = variableTypeList)
  # }

  return(.Object)
}

#' Adds a dosing cycle to model
#'
#' Add Steady State or ADDL dosing cycle to model object.
#'
#' @param .Object           Model object
#' @param type              Specification of dose type. Options are \code{"SteadyState"}and \code{"ADDL"}
#' @param name              Dose point name. See \code{\link{doseNames}}
#' @param administration    Mechanism for administering dose. Options are \code{"Bolus"} or \code{"Infusion"}
#' @param amount            Optional. Column mapping argument specifying corresponding "ADDL" column in input data, or
#'  numeric value specifiying dose amount.
#' @param II                Optional. Column mapping argument specifying corresponding "II" column in input data, or
#' numeric value specifying delta time.
#' @param rate              Optional. Column mapping argument specifying corresponding "Rate" column in input data,
#' or numeric specifying dose rate.
#' @param duration          Optional. Column mapping argument specifying corresponding "Duration" column in data,
#' or numeric specifying duration value.
#' @param isSecondDose      Use second dose point on compartment
#' @param colName           Column name in input data corresponding to column mapping for "SteadyState" or "ADDL" as supplied in \code{type} argument.
#' @seealso \code{\link{doseNames}}
#' @return Modified \code{NlmePmlModel} object
#' @examples
#' \donttest{
#' model <- addDoseCycle(pkmodel(columnMap = FALSE,
#'                               isPopulation = FALSE,
#'                               workingDir = tempdir()),
#'                       type = "SteadyState",
#'                       name = "A1",
#'                       amount = "Amount",
#'                       II = "II")
#' }
#'
#' @export
addDoseCycle <- function(.Object,
                         type = "SteadyState",
                         name,
                         administration = "Bolus", # type argument in ExtraDoseItem
                         amount = NULL,
                         II = NULL,
                         rate = NULL,
                         duration = NULL,
                         isSecondDose = FALSE,
                         colName = NULL) { # this is used to map either SteadyState or ADDL to the corresponding column in the input dataset (as commented above)


  # Check if name in existing dose names
  `%notin%` <- Negate(`%in%`)
  dnames <- doseNames(.Object)
  if (name %notin% dnames) {
    stop(paste0(name), " not found in existing dosepoint names")
  }

  if (length(dnames) < 2 && isSecondDose) {
    stop("isSecondDose = TRUE; not applicable for models without 'dosePoint2' statement")
  }

  mdata <- as.data.frame(.Object@inputData)

  if (!is.null(colName)) {
    if (is.null(mdata)) {
      stop("must specify input data using `initColMapping()`")
    } else if (colName %notin% colnames(mdata)) {
      stop(
        "Current column names are available for mapping:\n",
        paste(names(mdata), collapse = ", "),
        "\nColumn ", colName, " not found"
      )
    }
  }

  if (administration %notin% c("Infusion", "Bolus")) {
    stop("Administration should be Infusion or Bolus")
  }

  amountIICheckAvailable <- !sapply(list(amount, II), is.null)

  if (!all(amountIICheckAvailable)) {
    stopmessage <- paste0(
      "Following argument required: ",
      c("amount", "II")[!amountIICheckAvailable]
    )
  } else {
    stopmessage <- c()
  }

  rateDurationAvailable <- !sapply(list(rate, duration), is.null)

  if (administration == "Infusion") {
    if (all(rateDurationAvailable)) {
      stopmessage <- c(
        stopmessage,
        "Must specify 'rate' or 'duration', not both"
      )
    } else if (!any(rateDurationAvailable)) {
      stopmessage <- c(
        stopmessage,
        "Must specify either argument `rate` or `duration`"
      )
    }
  } else if (administration == "Bolus") {
    if (any(rateDurationAvailable)) {
      stopmessage <- c(
        stopmessage,
        paste0(
          "Following argument not applicable: ",
          c("rate", "duration")[rateDurationAvailable]
        )
      )
    }
  }

  if (!is.null(stopmessage)) {
    stop(paste(stopmessage,
      paste0("for `administration` = '", administration, "'"),
      collapse = "\n"
    ))
  }

  mapExtraDose <- ExtraDoseItem(
    type = administration,
    amount = amount,
    deltaTime = II,
    duration = duration,
    rate = rate,
    isSecondDose = isSecondDose,
    data = mdata
  )

  # need to check only the rows with SS/ADDL flag
  if (!is.null(colName)) {
    rowsToCheck <- mdata[[colName]] > 0
    rowsToCheck[is.na(rowsToCheck)] <- FALSE

    if (is.character(amount) && any(mdata[[amount]][rowsToCheck] < 0)) {
      stop("value in `amount` column cannot be negative")
    } else if (is.numeric(amount) && amount < 0) {
      stop("value supplied to `amount` argument cannot be negative")
    }

    if (is.character(II) && any(mdata[[II]][rowsToCheck] <= 0)) {
      stop("values in `II` column must be greater than 0")
    } else if (is.numeric(II) && II <= 0) {
      stop("value supplied to `II` argument must be greater than 0")
    }

    extype <- .Object@columnMapping@mapping[[type]]
    if (!is.null(extype) && extype@columnName != colName) {
      stop(paste0(colName, " differs from previous column specified in `addDoseCycle()` ", extype@columnName))
    }
  }


  if (type == "SteadyState") {
    .Object <- addExtraDose(.Object, name = name, doseType = type, doses = mapExtraDose, SteadyState = colName)
  } else {
    .Object <- addExtraDose(.Object, name = name, doseType = type, doses = mapExtraDose, ADDL = colName)
  }

  return(.Object)
}




#' Return dose names
#'
#' Use to return character vector of dose point names in model object.
#'
#' @param model Model object
#'
#' @examples
#' model <- pkmodel(columnMap = FALSE,
#'                  workingDir = tempdir())
#'
#' doses <- doseNames(model)
#'
#' @return Character vector of dose names defined in model
#' @export
doseNames <- function(model) {
  doseList <- model@dosePoints
  return(unlist(doseList))
}

#' Return extra dose names
#'
#' Use to return extra dose names for model object
#'
#' @param model Model object
#'
#' @examples
#' data <- pkData
#' data$II <- 24
#' data$ADDL <- 1
#'
#' model <-
#' pkmodel(
#'   parameterization = "Clearance",
#'   numCompartments = 2,
#'   data = data,
#'   ID = "Subject",
#'   Time = "Act_Time",
#'   A1 = "Amount",
#'   CObs = "Conc",
#'   workingDir = tempdir())
#' addDoseCycle(
#'   model,
#'   name = "A1",
#'   amount = 30000,
#'   II = 24,
#'   type = "ADDL",
#'   colName = "ADDL")
#'
#' extraDoseNames(model)
#'
#' @return Character vector of extra dose names
#' @export
extraDoseNames <- function(model) {
  extraDosesnames <- c()
  extraDosesList <- model@extraDoses
  if (length(extraDosesList) > 0) {
    for (extraDose in extraDosesList) {
      if (extraDose@doseType == SteadyStateDose) {
        name <- "SteadyState"
      } else {
        name <- "ADDL"
      }
      extraDosesnames <- c(extraDosesnames, name)
    }
  }

  if (model@pkModelAttrs@infusionAllowed) {
    dl <- model@dosePoints
    for (dosepoint in dl) {
      if (model@pkModelAttrs@isDuration) {
        name <- paste0(dosepoint, "_Duration")
      } else {
        name <- paste0(dosepoint, "_Rate")
      }
      extraDosesnames <- c(extraDosesnames, name)
    }
  }
  return(extraDosesnames)
}

#' Return extra dose lines
#'
#' Use to return extra dose lines for model object
#'
#' @param model Model object
#'
#' @return List of extra dose information
#' @examples
#' data <- pkData
#' data$II <- 24
#' data$ADDL <- 1
#'
#' model <-
#' pkmodel(
#'   parameterization = "Clearance",
#'   numCompartments = 2,
#'   data = data,
#'   ID = "Subject",
#'   Time = "Act_Time",
#'   A1 = "Amount",
#'   CObs = "Conc",
#'   workingDir = tempdir())
#' addDoseCycle(
#'   model,
#'   name = "A1",
#'   amount = 30000,
#'   II = 24,
#'   type = "ADDL",
#'   colName = "ADDL")
#'
#' extraDoseLines(model)
#'
#' @export
extraDoseLines <- function(model) {
  lines <- c()
  doseList <- attr(model, "extraDoses")
  colMap <- model@columnMapping@mapping
  if (length(doseList) > 0) {
    for (c in doseList) {
      name <- c@name
      doseType <- c@doseType
      if (doseType == SteadyStateDose) {
        cmd <- "ss"
        colName <- lookupColname(colMap, "SteadyState")
      } else {
        cmd <- "addl"
        colName <- lookupColname(colMap, "ADDL")
      }

      line <- paste0(cmd, "(\"", colName, "\", ")

      for (d in c@doses) {
        if (d@amount@type == ColumnType) {
          amount <- paste0("\"", d@amount@column, "\"")
        } else {
          amount <- d@amount@value
        }
        if (d@rate@type == ColumnType) {
          rate <- paste0("\"", d@rate@column, "\"")
        } else {
          rate <- d@rate@value
          if (rate == 0) {
            rate <- NULL
          }
        }
        if (d@duration@type == ColumnType) {
          duration <- paste0("\"", d@duration@column, "\"")
        } else {
          duration <- d@duration@value
          if (duration == 0) {
            duration <- NULL
          }
        }
        if (d@deltaTime@type == ColumnType) {
          deltaTime <- paste0("\"", d@deltaTime@column, "\"")
        } else {
          deltaTime <- d@deltaTime@value
        }
        if (d@isSecondDose) {
          second <- "2"
        } else {
          second <- ""
        }

        if (is.null(duration)) {
          dur_rate <- rate
        }
        if (is.null(rate)) {
          dur_rate <- paste0("dup / ", duration)
        }
        if (is.null(rate) && is.null(duration)) {
          dur_rate <- NULL
        }

        if (d@type == BolusDose) {
          if (doseType == SteadyStateDose) {
            line <- paste0(line, amount, dur_rate, " bolus", second, "(", name, ") ", deltaTime, " dt ")
          } else { # For ADDL
            line <- paste0(line, deltaTime, " dt ", amount, dur_rate, " bolus", second, "(", name, ") ")
          }
        } else {
          if (doseType == SteadyStateDose) {
            line <- paste0(line, amount, " ", dur_rate, " inf", second, "(", name, ") ", deltaTime, " dt")
          } else { # For ADDL
            line <- paste0(line, deltaTime, " dt ", amount, " ", dur_rate, " inf", second, "(", name, ") ")
          }
        }
      }
      line <- paste0(line, ")")
      lines <- c(lines, line)
    }
  }
  return(lines)
}

#' Class initializer for ResetColumnInfo
#'
#' Class initializer for ResetColumnInfo
#'
#' @param low    Lower value of reset range
#' @param hi     Upper value of reset range
#'
#' @export ResetColumnInfo
#' @keywords internal
setClass("ResetColumnInfo",
  slots = c(
    low = "numeric",
    hi = "numeric"
  )
) -> ResetColumnInfo



setMethod("initialize", "ResetColumnInfo",
  function(.Object,
           low,
           hi) {
    .Object@low <- low
    .Object@hi <- hi
    .Object
  }
)

#' Prints the data type of the extra dose
#'
#' Prints the data type of the extra dose
#' @keywords internal
#' @noRd
print.ExtraDoseDataType <- function(x, ...) {
  if (x@type == ValueType) {
    cat(x@value, "\n")
  } else {
    cat(x@column, "\n")
  }
}







No <- 1
RateDose <- 2
DurationDose <- 3

DosePointNames <- c("No", "RateDose", "DurationDose")

#' Defines a dosepoint for a compartment
#'
#' Defines a dosepoint for a compartment
#'
#' @param isZeroOrderAbsorption   One of No|RateDose|DurationDose
#' @param isBioavail              Does dose point has bioavailability?
#' @param bioavailExpression      Bioavailability expression
#' @param isTlag                  Does dose have time lag?
#' @param tlagExpression          Time lag expression
#' @param durationExpression      Optional Formula defines how to express duration in PML
#' @param rateExpression          Optional Formula defines how to express rate in PML
#' @param dobefore                Code to execute before dose is administrated
#' @param doafter                 Code to execute after dose is administrated
#'
#' @examples
#' \donttest{
#' dosePoint <- DosePoint(
#'   isZeroOrderAbsorption = DurationDose,
#'   durationExpression = "DUR",
#'   isTlag = FALSE,
#'   isBioavail = TRUE,
#'   bioavailExpression = "logitF1;1-ilogit(logitF1)"
#' )
#'
#' absDosePoint <- DosePoint(
#'   isZeroOrderAbsorption = NoDose,
#'   isBioavail = TRUE,
#'   bioavailExpression = "logitF1;ilogit(logitF1)"
#' )
#' }
#'
#' @noRd
#' @keywords internal
setClass("DosePoint",
  slots = c(
    isZeroOrderAbsorption = "numeric",
    isBioavail = "logical",
    bioavailExpression = "character",
    isTlag = "logical",
    tlagExpression = "character",
    isInfusion = "logical",
    isDuration = "logical",
    durationExpression = "character",
    rateExpression = "character",
    dobefore = "character",
    doafter = "character"
  )
) -> DosePoint


setMethod("initialize", "DosePoint",
  function(.Object,
           isZeroOrderAbsorption = NoDose,
           isBioavail = FALSE,
           bioavailExpression = "",
           isTlag = FALSE,
           tlagExpression = "",
           isInfusion = FALSE,
           isDuration = FALSE,
           durationExpression = "",
           rateExpression = "",
           dobefore = "",
           doafter = "") {
    .Object@isZeroOrderAbsorption <- isZeroOrderAbsorption
    .Object@isBioavail <- isBioavail
    .Object@bioavailExpression <- bioavailExpression
    .Object@isTlag <- isTlag
    .Object@tlagExpression <- tlagExpression
    .Object@durationExpression <- durationExpression
    .Object@rateExpression <- rateExpression
    .Object@dobefore <- dobefore
    .Object@doafter <- doafter
    .Object
  }
)

#' Class represents map between a model variable and a dataset column
#'
#' Class represents map between a model variable and a dataset column
#'
#' @param variableName   Model variable name
#' @param columnName     Corresponding column name in the dataset
#' @param variableType   Model variable type
#'
#' @keywords internal
NlmeColumnMap <- setClass(
  "NlmeColumnMap",
  slots = c(
    variableName = "character",
    columnName = "character",
    variableType = "list"
  )
)

setMethod("initialize", "NlmeColumnMap",
          function(.Object,
                   variableName = "",
                   columnName = "",
                   variableType = list()) {
            .Object@variableName <- variableName
            .Object@columnName <- columnName
            .Object@variableType <- variableType
            .Object
          })

#' Prints column mapping
#'
#' Prints column mapping
#'
#' @param x   Class created from \code{NlmeColumnMapping()}
#' @inheritParams ellipsis::dots_used
#'
#' @noRd
#' @keywords internal
print.NlmeColumnMap <- function(x, ...) {
  if (!is.null(x@variableType$type)) {
    cat(paste0(
      x@variableName,
      " => ",
      x@columnName,
      " (type: ",
      x@variableType$type,
      ")"
    ))
  } else {
    cat(paste0(x@variableName, " => ", x@columnName))
  }
}

#' Class represents mapping list between model variables and dataset columns
#'
#' Class represents mapping list between model variables and dataset columns
#'
#' @keywords internal
NlmeColumnMapping <-
  setClass("NlmeColumnMapping",
           slots = c(mapping = "list"))


setMethod("initialize", "NlmeColumnMapping",
          function(.Object, model, inputData) {
            .Object@mapping <- createInitialMapping(model, inputData)

            .Object
          })

#' Return column names
#'
#' Return column names
#'
#' @param mapping  Mapping list
#' @param vname    Variable name
#'
#' @keywords internal
#' @noRd
lookupColname <- function(mapping, vname) {
  for (m in mapping) {
    varName <- attr(m, "variableName")
    colName <- attr(m, "columnName")
    if (vname == varName) {
      return(colName)
    }
  }
  return("")
}

# removing comments from the model statements
.remove_comments <-
  function(statementsLines,
           type = "All",
           removeWhites = TRUE,
           collapse = "\n") {
    if (is.list(statementsLines)) {
      statementsLines <- unlist(statementsLines)
    }

    if (length(statementsLines) > 1) {
      statementsLines <- paste(statementsLines, collapse = collapse)
    }

    OneLine2Slash <- "(?:\\/\\/(?:\\\\\\n|[^\\n])*(?=$|\\n))"
    OneLineSharp <- "(?:#(?:\\\\\\n|[^\\n])*(?=$|\\n))"
    Asterisk <- "(?:\\/\\*[\\s\\S]*?\\*\\/)"

    if (type == "All") {
      pattern <- paste(OneLine2Slash,
                       OneLineSharp,
                       Asterisk,
                       sep = "|",
                       collapse = "|")
    } else if (type == "Asterisk") {
      pattern <- paste(Asterisk,
                       sep = "|", collapse = "|")
    } else if (type == "Sharp") {
      pattern <- paste(OneLineSharp,
                       sep = "|", collapse = "|")
    } else if (type == "OneLine") {
      pattern <- paste(OneLine2Slash,
                       OneLineSharp,
                       sep = "|",
                       collapse = "|")
    } else if (type == "C") {
      pattern <- paste(OneLine2Slash,
                       Asterisk,
                       sep = "|",
                       collapse = "|")
    } else {
      warning("type not recognized: ", type)
    }

    statementsLineswoComm <-
      gsub(pattern, "\n", statementsLines, perl = TRUE)

    if (removeWhites) {
      statementsLineswoComm <- gsub("[\r\n \t]|\\(\\)",
                                    "",
                                    paste0(statementsLineswoComm))
    }

    statementsLineswoComm
  }

#' Writes the input data for the model to a file
#'
#' Writes the input data for the model to a file
#'
#' @param model         A PK/PD model
#' @param datafileName  Input data file
#' @param workingDir Directory where the file should be written.
#' If it is not given, the file is written to the model working directory
#'
#' @examples
#' \donttest{
#'   writeInputData(model = model,
#'                  datafileName = model@dataset@dataFile,
#'                  workingDir = model@modelInfo@workingDir)
#' }
#' @keywords internal
#' @noRd
writeInputData <- function(model, datafileName, workingDir) {
  inputData <- model@inputData
  if (!model@isPopulation) {
    inputData <- cbind(zzzDummyId = 0, inputData)
  }

  if (missing(workingDir)) {
    workingDir <- model@modelInfo@workingDir
  }

  if (!dir.exists(workingDir)) {
    workingDir <- .prepare_wd(workingDir)
  }

  fullPath <- file.path(workingDir, datafileName)
  colnames <- colnames(inputData)
  header <- paste0("##", paste(colnames, collapse = ","))
  cat(header,
      file = fullPath,
      append = FALSE,
      sep = "\n")

  # optional units in attributes
  attrUnits <- attr(inputData, "Units")
  if (is.null(attrUnits))
    attrUnits <- attr(inputData, "units")
  if (!is.null(attrUnits)) {
    if (length(attrUnits) != ncol(inputData)) {
      stop(
        "inputData slot of the model contains units row. ",
        "The length of the units row is not the same as the data columns length: ",
        length(attrUnits),
        " != ",
        ncol(inputData)
      )
    }

    units <- paste0("#@", paste(attrUnits, collapse = ","))
    cat(units,
        file = fullPath,
        append = TRUE,
        sep = "\n")
  }

  commaCheck <- sapply(inputData, function(x) {
    any(grepl(",", x))
  })
  if (any(commaCheck)) {
    stop(
      "Cannot proceed with the data given due to commas inside the column(s): ",
      paste(names(which(commaCheck)), collapse = ", ")
    )
  }
  write.table(
    inputData,
    fullPath,
    row.names = FALSE,
    col.names = FALSE,
    sep = ",",
    quote = FALSE,
    append = TRUE
  )
}

# Internal function to check column mappings
.check_column_mappings <- function(cols, data) {
  data_cols <- colnames(data)

  `%notin%` <- Negate(`%in%`)

  # split multiple ID columns
  cols <- unlist(strsplit(cols, "\\s*,\\s*"))
  # Check if mapped column names are in dataset
  for (i in 1:length(cols)) {
    if (cols[[i]] == "?") {
      next
    }

    if (cols[[i]] %notin% data_cols) {
      # warning(paste0("Column '", cols[[i]], "' not found in data. Please manually map '", cols[[i]], "' before model execution using `modelColumnMapping` function."))
      stop(paste0("Column '", cols[[i]], "' not found in data"))
    }
  }

  # Check for any duplicated columns in data
  if (any(duplicated(cols))) {
    dup_num <- which(duplicated(cols))
    stop(
      paste0(
        "Column '",
        cols[[dup_num]],
        "' is duplicated, assign unique column names to model variables"
      )
    )
  }
}

#' Initialize input data for PK/PD model
#'
#' Used to initialize input data for PK/PD model
#'
#' @param .Object Model object
#' @param data   Input data of class \code{data.frame}.
#'
#' @examples
#' model <- pkmodel(columnMap = FALSE,
#'                  workingDir = tempdir())
#'
#' model <- dataMapping(model, pkData)
#'
#' @seealso \code{\link{colMapping}}
#' @return Modified \code{NlmePmlModel} object
#' @export dataMapping
dataMapping <- function(.Object, data) {
  stopifnot(inherits(.Object, "NlmePmlModel"))
  stopifnot(inherits(data, "data.frame"))

  .Object <- `initColMapping<-`(.Object, data)
}

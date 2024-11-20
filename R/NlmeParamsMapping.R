#' Class represents mapping list between model variables and data columns
#'
#' Class represents mapping list between model variables and data columns
#'
#' @keywords internal
NlmeParamsMapping <-
  setClass("NlmeParamsMapping",
           slots = c(mapping = "list")
  )


setMethod("initialize", "NlmeParamsMapping",
  function(.Object,
           model,
           paramsInput) {
    .Object@mapping <- createInitialParamsMapping(model, paramsInput)
    .Object
  }
)

#' Creates a mapping between model variable and parameter columns
#'
#' Creates a mapping between model variable and parameter columns
#'
#' @param model        A PK/PD model
#' @param paramsInput  Parameter column names
#'
#' @examples
#' \donttest{
#' createInitialParamsMapping(model, doseInput)
#' }
#' @keywords internal
#' @noRd
createInitialParamsMapping <- function(model, paramsInput) {
  map <- list()
  ids <- c("param", "init", "high", "low")
  pn <- fixedParameterNames(model)
  names <- ids
  for (p in pn) {
    names <- c(names, p)
  }
  colNames <- colnames(paramsInput)
  for (c in paramsInput$Parameter) {
    colNames <- c(colNames, c)
  }

  for (n in names) {
    # Check for exact match
    for (c in colNames) {
      if (toupper(c) == toupper(n)) {
        m <- NlmeColumnMap(n, c)
        map[[n]] <- m
      }
    }
  }
  for (n in names) {
    # Check for partial match
    if (length(map[[n]]) == 0) {
      for (c in colNames) {
        if (length(grep(toupper(n), toupper(c))) != 0) {
          m <- NlmeColumnMap(n, c)
          map[[n]] <- m
        }
      }
    }
  }
  for (n in names) {
    #  Mark as unassigned
    if (length(map[[n]]) == 0) {
      if (n == "param") {
        m <- NlmeColumnMap(n, "Parameter")
      } else if (n == "init") {
        m <- NlmeColumnMap(n, "Initial")
      } else if (n == "low") {
        m <- NlmeColumnMap(n, "Lower")
      } else if (n == "high") {
        m <- NlmeColumnMap(n, "Upper")
      } else {
        m <- NlmeColumnMap(n, "?")
      }
      map[[n]] <- m
    }
  }
  return(map)
}

#' Writes the mapping between model variables and parameter column names to a file
#'
#' Write the mapping between model variables and parameter column
#' names to a file
#'
#' @param model    A PK/PD model
#' @param dataset  Source of parameter data
#'
#' @examples
#' \donttest{
#' writeParamsMapping(model, dataset)
#' }
#' @keywords internal
#' @noRd
writeParamsMapping <- function(model, dataset) {
  colMap <- model@paramsMapping@mapping
  data <- model@fixedParamData

  lines <- c()
  modelType <- model@modelType@modelType

  cols <- c("param", "init", "high", "low")
  for (c in cols) {
    name <- lookupColname(colMap, c)
    if (name != "") {
      line <- paste0(c, "(\"", name, "\")")
      lines <- c(lines, line)
    }
  }
  for (m in colMap) {
    var <- m@variableName
    name <- m@columnName
    if ((var %in% cols) == FALSE) {
      if (name != "?" && name != "") {
        line <- paste0("map(\"", var, "\" <- \"", name, "\")")
        lines <- c(lines, line)
      }
    }
  }

  workingDir <- model@modelInfo@workingDir
  if (!dir.exists(workingDir)) {
    workingDir <- .prepare_wd(workingDir)
  }

  filename <- file.path(workingDir, dataset@estimatesDefFile)
  dataFilename <- file.path(workingDir, dataset@estimatesDataFile)
  appendFlag <- FALSE
  for (l in lines) {
    cat(l, file = filename, sep = "\n", append = appendFlag)
    appendFlag <- TRUE
  }

  header <- paste0("##", paste0(cols, collapse = ","))

  data[is.na(data)] <- "."
  cat(header, file = dataFilename, sep = "\n", append = FALSE)
  write.table(data, dataFilename,
              row.names = FALSE, col.names = FALSE, sep = ",",
              quote = FALSE, append = TRUE
  )
}

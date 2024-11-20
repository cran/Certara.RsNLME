#' Class represents mapping list between model variables and dose columns
#'
#' Class represents mapping list between model variables and dose columns
#'
#'
#' @keywords internal
NlmeDoseMapping <- setClass("NlmeDoseMapping",
                            slots = c(mapping = "list")
)


setMethod("initialize", "NlmeDoseMapping",
  function(.Object,
           model,
           doseInput) {
    .Object@mapping <- createInitialDoseMapping(model, doseInput)
    .Object
  }
)

#' Creates a mapping between model variable and dose columns
#'
#' Creates a mapping between model variable and dose columns
#'
#' @param model  A PK/PD model
#' @param doseInput   Column names in dose file
#'
#' @examples
#' \donttest{
#' createInitialDoseMapping(model, doseInput)
#' }
#' @keywords internal
#' @noRd
createInitialDoseMapping <- function(model, doseInput) {
  map <- list()
  ids <- c()
  if (model@isPopulation) {
    ids <- c("id")
  }
  if (model@isTimeBased) {
    ids <- c(ids, "time")
  }

  dn <- doseNames(model)
  edn <- extraDoseNames(model)
  names <- c(ids, dn, edn)
  if (model@hasResetInfo) {
    names <- c(names, "Reset")
  }
  colNames <- colnames(doseInput)

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
      m <- NlmeColumnMap(n, "?")
      map[[n]] <- m
    }
  }
  return(map)
}

#' Writes the mapping between model variables and dose column names to a file
#'
#' Writes the mapping between model variables and dose column names to a file
#'
#' @param model    A PK/PD model
#' @param dataset  Source of dose data
#'
#' @examples
#' \donttest{
#' writeDoseMapping(model, dataset)
#' }
#' @keywords internal
#' @noRd
writeDoseMapping <- function(model, dataset) {
  lines <- c()
  mapping <- model@doseMapping
  colMap <- mapping@mapping
  data <- model@doseData
  if (model@isPopulation == FALSE) {
    cbind(zzzDummyId = 0, data)
  }

  workingDir <- model@modelInfo@workingDir
  if (!dir.exists(workingDir)) {
    workingDir <- .prepare_wd(workingDir)
  }

  filename <- file.path(workingDir, dataset@doseDefFile)
  dataFilename <- file.path(workingDir, dataset@doseDataFile)

  if (model@isPopulation == FALSE) {
    lines <- c(lines, paste0("id(\"zzzDummyId\")"))
  }
  for (c in colMap) {
    varName <- c@variableName
    colName <- c@columnName
    if (colName != "?" && colName != "") {
      if (varName == "id" || varName == "time") {
        line <- paste0(varName, "(\"", colName, "\")")
      } else {
        line <- paste0("dose(", varName, "<-\"", colName, "\")")
      }
      lines <- c(lines, line)
    }
  }
  append <- FALSE
  for (l in lines) {
    cat(l, file = filename, sep = "\n", append = append)
    append <- TRUE
  }

  cols <- colnames(data)
  header <- paste0("##", paste0(cols, collapse = ","))

  cat(header, file = dataFilename, sep = "\n", append = FALSE)
  write.table(data, dataFilename,
              row.names = FALSE, col.names = FALSE, sep = ",",
              quote = FALSE, append = TRUE
  )
}

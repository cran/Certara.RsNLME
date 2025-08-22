#' Class represents mapping list between model variables and Random Effect
#'
#' Class represents mapping list between model variables and Random Effect
#'
#'
#' @keywords internal
#' @noRd
setClass("NlmeRandParamsMapping",
         slots = c(
           mapping = "list"
         )
) -> NlmeRandParamsMapping

setMethod("initialize", "NlmeRandParamsMapping",
  function(.Object,
           model,
           randEffectInput) {
    .Object@mapping <- createInitialRandEffectMapping(model, randEffectInput)
    .Object
  }
)

#' Creates a mapping between model variables and random effects columns
#'
#' Creates a mapping between model variables and random effects columns
#'
#' @param model            A PK/PD model
#' @param randEffectInput  Random effect data
#'
#' @examples
#' \donttest{
#' createInitialRandEffectMapping(model, randEffectInput)
#' }
#' @keywords internal
#' @noRd
createInitialRandEffectMapping <- function(model, randEffectInput) {
  map <- list()
  pn <- randParameterNames(model)
  names <- c()
  for (p in pn) {
    names <- c(names, p)
  }

  colNames <- colnames(randEffectInput)

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

#' Writes the mapping between model variables and random effect column
#'
#' Writes the mapping between model variables and random effect column
#' names to a file
#'
#' @param model    A PK/PD model
#' @param dataset  Source of random effect data
#'
#' @examples
#' \donttest{
#' writeRandParamsMapping(model, dataset)
#' }
#' @keywords internal
#' @noRd
writeRandParamsMapping <- function(model, dataset) {
  colMap <- model@randParamsMapping@mapping
  data <- model@randParamData

  subjectId <- model@columnMapping@mapping$id@columnName

  lines <- c()
  line <- paste0("id(\"", subjectId, "\")")
  lines <- c(lines, line)

  for (c in colMap) {
    varName <- attr(c, "variableName")
    colName <- attr(c, "columnName")
    if (colName != "?" && colName != "") {
      line <- paste0("covr(", varName, "<-\"", colName, "\")")
      lines <- c(lines, line)
    }
  }

  workingDir <- model@modelInfo@workingDir
  if (!dir.exists(workingDir)) {
    workingDir <- .prepare_wd(workingDir)
  }

  filename <- file.path(workingDir, dataset@ranEffectDefFile)
  dataFilename <- file.path(workingDir, dataset@ranEffectDataFile)
  appendFlag <- FALSE
  for (l in lines) {
    cat(l, file = filename, sep = "\n", append = appendFlag)
    appendFlag <- TRUE
  }

  cols <- colnames(data)
  header <- paste0("##", paste0(cols, collapse = ","))

  cat(header, file = dataFilename, sep = "\n", append = FALSE)
  write.table(data, dataFilename,
              row.names = FALSE, col.names = FALSE, sep = ",",
              quote = FALSE, append = TRUE
  )
}

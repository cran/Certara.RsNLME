#' Writes the mapping between model variables and column names to a file
#'
#' Writes the mapping between model variables and column names to a file
#'
#' @param model      A PK/PD model
#' @param filename   Name of the file
#' @param workingDir Directory where the file should be written.
#' If it is not given, the file is written to the model working directory
#' @param sortColumns SortColumns class object used for proper id mapping during
#' individual modeling
#'
#' @examples
#' \donttest{
#' writeColumnMapping(model, filename)
#' }
#' @keywords internal
#' @noRd
writeColumnMapping <- function(model, filename, workingDir, sortColumns = NULL) {
  if (missing(workingDir)) {
    workingDir <- model@modelInfo@workingDir
  }

  workingDir <- .prepare_wd(workingDir)

  fullPath <- file.path(workingDir, filename)

  colMap <- model@columnMapping@mapping
  modelType <- model@modelType@modelType
  on <- observationNames(model)
  # not typical observations are not included as LL count etc.
  # need to add them here
  for (mappedTerm in colMap) {
    if (.hasSlot(mappedTerm, "variableType") &&
        !is.null(mappedTerm@variableType$type) &&
        mappedTerm@variableType$type == "observation" &&
        !mappedTerm@variableName %in% on) {
      on <- c(on, mappedTerm@variableName)
    }
  }

  en <- observationExtraNames(model)
  cn <- covariateNames(model)
  dn <- doseNames(model)

  # RandomParam
  isSequential <- model@pkModelAttrs@isSequential
  if (isSequential) {
    rn <- randParameterNames(model) # JC
    rcolMap <- model@randParamsMapping@mapping
  }
  eDoseLines <- extraDoseLines(model)

  sspos <- grep("ss\\(", eDoseLines)
  ss_eDoseLines <- eDoseLines[sspos]

  addlpos <- grep("addl\\(", eDoseLines)
  addl_eDoseLines <- eDoseLines[addlpos]

  if (length(ss_eDoseLines) > 1) {
    s1 <- gsub(pattern = "\\)$", "", ss_eDoseLines[[1]])
    s2 <- vector(mode = "character", length = length(ss_eDoseLines) - 1)
    for (i in 2:length(ss_eDoseLines)) {
      s2[[i]] <- ss_eDoseLines[[i]]
      s2[[i]] <- gsub("\\)$", "", s2[[i]])
      s2[[i]] <- gsub(".*?,", "", s2[[i]])
    }
    s2 <- paste0(s2, collapse = " ")

    ss_eDoseLines <- paste0(s1, s2, ")")
  }

  if (length(addl_eDoseLines) > 1) {
    a1 <- gsub(pattern = "\\)$", "", addl_eDoseLines[[1]])
    a2 <- vector(mode = "character", length = length(addl_eDoseLines) - 1)
    for (i in 2:length(addl_eDoseLines)) {
      a2[[i]] <- addl_eDoseLines[[i]]
      a2[[i]] <- gsub("\\)$", "", a2[[i]])
      a2[[i]] <- gsub(".*?,", "", a2[[i]])
    }
    a2 <- paste0(a2, collapse = " ")

    addl_eDoseLines <- paste0(a1, a2, ")")
  }

  eDoseLines <- c(ss_eDoseLines, addl_eDoseLines)

  vars <- c()
  lines <- c()
  if (model@isPopulation) {
    idcolNames <- strsplit(lookupColname(colMap, "id"), split = ",")[[1]]
    # type gives doublequotes
    idcolNamesquo <- paste(shQuote(trimws(idcolNames), type = "cmd"), collapse = ", ")
    if (idcolNamesquo == "") {
      stop("Cannot create a column mapping file for the model in pop mode when no id column is mapped.\n",
           "Please use parsePMLColMap() to update the mapping and then map with colMapping().")
    }
    lines <- c(lines, paste0("id(", idcolNamesquo, ")"))
  } else {
    if (missing(sortColumns) |
        is.null(sortColumns) |
        !.hasSlot(sortColumns, "sortColumnList")) {
      # nothing to put into id
      lines <- c(lines, paste0("id(\"zzzDummyId\")"))
    } else if (sortColumns@numSortColumns <= 5) {
      # same as above; just cannot do all checks in one line
      lines <- c(lines, paste0("id(\"zzzDummyId\")"))
    } else if (sortColumns@numSortColumns > 5) {
      stop("Number of input data columns supplied ",
           "to 'SortColumns' cannot exceed 5 for individual models:\n",
           paste(sortColumns@sortColumnList, collapse = ", "),
           call. = FALSE
      )
    } else {
      idcolNamesquo <- paste0(shQuote(trimws(sortColumns@sortColumnList), type = "cmd"),
                              collapse = ", "
      )
      lines <- c(lines, paste0("id(", idcolNamesquo, ")"))
    }
  }

  if (model@isTimeBased) {
    timeCol <- lookupColname(colMap, "time")
    lines <- c(
      lines,
      paste0("time(\"", timeCol, "\")")
    )
  }

  for (doseName in dn) {
    colName <- lookupColname(colMap, doseName)
    if (colName %in% c("", "?")) {
      next()
    }

    extraDoseInfo <- ""

    if (!model@isTextual) {
      if (model@pkModelAttrs@infusionAllowed) {
        if (model@pkModelAttrs@isDuration) {
          infusionName <- paste0(doseName, "_Duration")
          dcolName <- lookupColname(colMap, infusionName)
          if (!dcolName %in% c("", "?")) {
            extraDoseInfo <- paste0(", duration=\"", dcolName, "\"")
          }
        } else {
          infusionName <- paste0(doseName, "_Rate")
          dcolName <- lookupColname(colMap, infusionName)
          if (!dcolName %in% c("", "?")) {
            extraDoseInfo <- paste0(", \"", dcolName, "\"")
          }
        }
      }
    } else if (.hasSlot(colMap[[doseName]], "variableType") &&
               !is.null(colMap[[doseName]]@variableType$Infusion)) {
      Infusion <- colMap[[doseName]]@variableType$Infusion
      if (!is.na(Infusion)) {
        infusionName <- paste0(doseName, "_", Infusion)
        dcolName <- lookupColname(colMap, infusionName)
        if (!dcolName %in% c("", "?")) {
          if (Infusion == "Duration") {
            DurSuffix <- "duration="
          } else {
            DurSuffix <- ""
          }

          extraDoseInfo <- paste0(", ", DurSuffix, "\"", dcolName, "\"")
        }
      }
    }

    doseN <- ifelse((colMap[[doseName]]@variableType$DosepointN == 1), "dose", "dose2")
    # if the dosepoint was doubled, need to get rid of index
    doseName <- ifelse(colMap[[doseName]]@variableType$DosepointDouble,
                       gsub("_\\d$", "", doseName),
                       doseName
    )

    lines <- c(
      lines,
      paste0(doseN, "(", doseName, "<-\"", colName, "\"", extraDoseInfo, ")")
    )
  }

  lines <- c(lines, eDoseLines)

  if (lookupColname(colMap, "MDV") != "?" && lookupColname(colMap, "MDV") != "") {
    lines <- c(lines, paste0("mdv(\"", lookupColname(colMap, "MDV"), "\")"))
  }

  covarList <- model@covariateList
  for (covariate in cn) {
    colName <- lookupColname(colMap, covariate)
    covObject <- covarList[sapply(
      covarList,
      function(x, covariate) {
        x@name == covariate
      },
      covariate
    )][[1]]
    lines <- c(
      lines,
      paste0("covr(", covariate, "<-\"", colName, "\"", covariatePartsString(covObject), ")")
    )
  }

  # are there some special covariates not in the list?
  for (mappedTerm in colMap) {
    if (.hasSlot(mappedTerm, "variableType") &&
        !is.null(mappedTerm@variableType$type) &&
        mappedTerm@variableType$type == "covariate" &&
        !mappedTerm@variableName %in% cn) {

      # we need cn to create posthoc file later
      cn <- c(cn, mappedTerm@variableName)
      colName <- mappedTerm@columnName
      if (mappedTerm@variableType$covType == COVAR_CATEGORY) {
        brackets <- "()"
      } else {
        brackets <- ""
      }
      lines <- c(
        lines,
        paste0("covr(", mappedTerm@variableName, "<-\"", colName, "\"", brackets, ")")
      )
    }
  }

  if (length(en) == 0) {
    for (obsName in on) {
      colName <- lookupColname(colMap, obsName)
      if (colName != "?") {
        lines <- c(
          lines,
          sprintf("obs(%s<-\"%s\")", obsName, colName)
        )
      }
    }
  } else {
    # there are BQLs
    statementsWOComments <- .remove_comments(model@statements)

    for (obsName in on) {
      colName <- lookupColname(colMap, obsName)
      if (colName != "?") {
        bqlName <- paste0(obsName, "BQL")
        if (any(sapply(en, grepl, bqlName)) &&
            bqlName %in% names(colMap) &&
            lookupColname(colMap, bqlName) != "?") {
          colBQL <- lookupColname(colMap, bqlName)
          # use BQL column only if it is mapped
          s <- sprintf("obs(%s<-\"%s\", bql <- \"%s\") ", obsName, colName, colBQL)
          # throw a warning if static LLOQ is used AND BQL is mapped
          greplStaticLLOQString <- paste0("(?<=observe\\(", obsName, "=)[^,]+,bql=")
          if (any(grepl(greplStaticLLOQString, statementsWOComments, perl = TRUE))) {
            warning("For observation ", obsName, " a static LLOQ value requested,",
                    "\nbut ", bqlName, " mapped; static LLOQ value will be ignored.",
                    call. = FALSE
            )
          }
        } else {
          s <- sprintf("obs(%s<-\"%s\")", obsName, colName)
        }
        lines <- c(lines, s)
      }
    }
  }

  # Check if A1strip Variable is mapped
  # and not added as a covariate
  if ("A1Strip" %in% names(colMap) &&
      !"A1Strip" %in% cn) {
    colName <- lookupColname(colMap, "A1Strip")
    if (colName != "?") {
      lines <- c(lines, paste0("covr(A1Strip", "<-\"", colName, "\"", ")"))
    }
  }

  if (isSequential) {
    for (r in rn) {
      colName <- lookupColname(rcolMap, r)
      lines <- c(lines, paste0("covr(", r, "<-\"", colName, "\"", ")"))
    }
  }

  if (model@isPopulation) {
    if (length(cn) > 0) {
      indVar <- paste0(", covr(", paste(cn, collapse = ", "), "), ")
    } else {
      # no covariates
      indVar <- ", "
    }

    stparms <- paste(structuralParameterNames(model), collapse = ", ")

    if (length(stparms) != 0) {
      table <- paste0(
        "table(file=\"posthoc.csv\"",
        indVar,
        stparms, ", mode=keep)"
      )

      lines <- c(lines, table)
    }
  }


  if (length(model@userDefinedExtraDefs) != 0) {
    for (l in model@userDefinedExtraDefs) {
      lines <- c(lines, l)
    }
  }

  writeLines(lines, fullPath)

  return(lines)
}



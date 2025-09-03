.add_covLabels <- function(NLMEmodel, MAP1_list) {
  covarList <- NLMEmodel@covariateList
  if (length(covarList) == 0) {
    return(NLMEmodel)
  }

  cn <- covariateNames(NLMEmodel)
  for (covariate in cn) {
    covObjectIndex <- which(sapply(covarList,
                                   function(x, covariate) {
                                     x@name == covariate
                                   },
                                   covariate))
    if (length(covObjectIndex) != 1)
      next()
    if (covarList[[covObjectIndex]]@type == Continuous)
      next()
    if (length(MAP1_list$covLabels_terms[[covariate]]) == 1)
      next()

    labels <- MAP1_list$covLabels_terms[[covariate]]
    covarItems <-
      lapply(seq_along(labels),
             function(i, labels) {
               NlmeCovarItem(names(labels)[i], labels[i])
             },
             labels)
    NLMEmodel@covariateList[[covObjectIndex]]@covarItems <-
      covarItems
  }

  NLMEmodel
}

# check if ADDL is presented in coldef slot of the model
.check_extradefADDL <- function(userDefinedExtraDefs) {
  KeyNames <- c("addl", "addlcol")
  KeyNamesCollapsed <- paste(KeyNames, collapse = "|")
  pattern <-
    paste0('(?<=^|\\n)\\s*(', KeyNamesCollapsed, ')\\s*\\([^\\n]+\\)')

  sum(grepl(pattern, userDefinedExtraDefs, perl = TRUE))
}

# check if SS is presented in coldef slot of the model
.check_extradefSS <- function(userDefinedExtraDefs) {
  KeyNames <- c("ss", "sscol")
  KeyNamesCollapsed <- paste(KeyNames, collapse = "|")
  pattern <-
    paste0('(?<=^|\\n)\\s*(', KeyNamesCollapsed, ')\\s*\\([^\\n]+\\)')

  sum(grepl(pattern, userDefinedExtraDefs, perl = TRUE))
}

# generate ss mdv reset or addl column definitions from the MAP block
.add_inputColdefsFromMAP <- function(NLMEmodel, parsedMetamodel) {
  map_vector <- parsedMetamodel$MAP1_list$col_terms
  names(map_vector) <- parsedMetamodel$MAP1_list$model_terms
  InputWords <- c("ADDL", "SS", "II", "SSOFFSET", "RESET" , "MDV")
  input_indices <-
    lapply(InputWords,
           function(InputWord, model_terms) {
             found_index <- which(grepl(paste0("^", InputWord, "$"),
                                        model_terms, ignore.case = TRUE))
             if (length(found_index) > 1) {
               stop("During the mapping more than one special input word '",
                    InputWord,
                    "' was found")
             } else if (length(found_index) == 1) {
               InputWord = found_index
             } else {
               return(NA)
             }
           },
           names(map_vector))

  names(input_indices) <- InputWords
  # MDV
  if (!is.na(input_indices$MDV)) {
    if (sum(
      grepl(
        '(?<=^|\\n)\\s*mdv\\s*\\([^\\n]+\\)',
        NLMEmodel@userDefinedExtraDefs,
        perl = TRUE
      )
    )) {
      warning("MDV map is already found in the columnd definition",
              "\n ignoring it in mapping")
    } else {
      userDefinedExtraDefinitions(NLMEmodel) <-
        c(unlist(NLMEmodel@userDefinedExtraDefs),
          paste0('mdv("', map_vector[input_indices$MDV], '")'))
    }
  }

  # ADDL
  if (!is.na(input_indices$ADDL)) {
    KeyNames <- c("addl", "addlcol")
    KeyNamesCollapsed <- paste(KeyNames, collapse = "|")
    pattern <-
      paste0('(?<=^|\\n)\\s*(',
             KeyNamesCollapsed,
             ')\\s*\\([^\\n]+\\)')

    if (sum(grepl(pattern, NLMEmodel@userDefinedExtraDefs, perl = TRUE))) {
      warning("ADDL map is already found in the column definition",
              "\n ignoring it in mapping")
    } else {
      if (is.na(input_indices$II)) {
        stop("ADDL mapping column is given, but delta time II is not")
      } else if (any(
        grepl(
          '(?<=^|\\n)\\s*iicol\\s*\\([^\\n]+\\)',
          NLMEmodel@userDefinedExtraDefs,
          perl = TRUE
        )
      )) {
        stop(
          "ADDL mapping column is given, but iicol is already presented in ",
          "\n column definitions"
        )
      }

      userDefinedExtraDefinitions(NLMEmodel) <-
        c(
          unlist(NLMEmodel@userDefinedExtraDefs),
          paste0('addlcol(', map_vector[input_indices$ADDL], ')'),
          paste0('iicol(', map_vector[input_indices$II], ')')
        )
    }
  }

  # SS
  if (!is.na(input_indices$SS)) {
    KeyNames <- c("ss", "sscol")
    KeyNamesCollapsed <- paste(KeyNames, collapse = "|")
    pattern <-
      paste0('(?<=^|\\n)\\s*(',
             KeyNamesCollapsed,
             ')\\s*\\([^\\n]+\\)')

    if (sum(grepl(pattern, NLMEmodel@userDefinedExtraDefs, perl = TRUE))) {
      warning(
        "SS map is already found in the columnd definition",
        "\n ignoring it in mapping",
        call. = TRUE
      )
    } else {
      if (is.na(input_indices$II)) {
        stop("SS mapping column is given, but delta time II is not")
      } else if (any(
        grepl(
          '(?<=^|\\n)\\s*iicol\\s*\\([^\\n]+\\)',
          NLMEmodel@userDefinedExtraDefs,
          perl = TRUE
        )
      ) &&
      is.na(input_indices$ADDL)) {
        stop(
          "SS mapping column is given, but iicol is already presented in ",
          "\n column definitions"
        )
      }

      userDefinedExtraDefinitions(NLMEmodel) <-
        c(unlist(NLMEmodel@userDefinedExtraDefs),
          paste0('sscol(', map_vector[input_indices$SS], ')'))

      # if iicol already added for ADDL,
      if (is.na(input_indices$ADDL)) {
        userDefinedExtraDefinitions(NLMEmodel) <-
          c(
            unlist(NLMEmodel@userDefinedExtraDefs),
            paste0('iicol(', map_vector[input_indices$II], ')')
          )
      }

      # add offset is presented
      if (!is.na(input_indices$SSOFFSET)) {
        userDefinedExtraDefinitions(NLMEmodel) <-
          c(
            unlist(NLMEmodel@userDefinedExtraDefs),
            paste0('ssoffcol(', map_vector[input_indices$SSOFFSET], ')')
          )
      }
    }
  }

  # reset
  if (!is.na(input_indices$RESET)) {
    if (sum(
      grepl(
        '(?<=^|\\n)\\s*reset\\s*\\([^\\n]+\\)',
        NLMEmodel@userDefinedExtraDefs,
        perl = TRUE
      )
    )) {
      warning(
        "reset map is already found in the columnd definition",
        "\n ignoring it in mapping",
        call. = FALSE,
        immediate. = TRUE
      )
    } else {
      NLMEmodel <-
        addReset(NLMEmodel, 4, 4, map_vector[input_indices$RESET])
    }
  }

  NLMEmodel
}

# the user can map the model terms Dosepoint_Rate or Dosepoint_Duration
# find it here and create mapping
.InfVarMap <- function(NLMEmodel, map_vector_woInputOpt) {
  for (dosepoint in NLMEmodel@dosePoints) {
    RateFound <-
      map_vector_woInputOpt[names(map_vector_woInputOpt) == paste0(dosepoint, "_", "Rate")]
    DurationFound <-
      map_vector_woInputOpt[names(map_vector_woInputOpt) == paste0(dosepoint, "_", "Duration")]
    if (length(RateFound) + length(DurationFound) > 1) {
      warning(
        "More than one Rate or Duration map are given for the dosepoint ",
        dosepoint,
        "\nIgnoring Rate/Duration for the current dosepoint.",
        call. = FALSE,
        immediate. = TRUE
      )
      next()
    } else if (length(RateFound) + length(DurationFound) == 0) {
      next()
    } else if (length(RateFound) == 1) {
      Infusion <- "Rate"
      NLMEmodel@columnMapping@mapping[[dosepoint]]@variableType$Infusion <-
        "Rate"
      InfcolumnName <- RateFound
    } else {
      # duration
      Infusion <- "Duration"
      NLMEmodel@columnMapping@mapping[[dosepoint]]@variableType$Infusion <-
        "Duration"
      InfcolumnName <- DurationFound
    }

    InfVariableName <- names(InfcolumnName)
    dosepointN <-
      NLMEmodel@columnMapping@mapping[[dosepoint]]@variableType$DosepointN
    DosepointDouble <-
      NLMEmodel@columnMapping@mapping[[dosepoint]]@variableType$DosepointDouble

    InfVarNlmeColumnMap <-
      NlmeColumnMap(
        variableName = InfVariableName,
        columnName = InfcolumnName,
        variableType = list(
          type = "dosepointInf",
          Infusion = Infusion,
          DosepointN = dosepointN,
          DosepointDouble = DosepointDouble
        )
      )

    NLMEmodel@columnMapping@mapping[[InfVariableName]] <-
      InfVarNlmeColumnMap
  }

  NLMEmodel
}

.quote_EngineArguments <- function(ShouldBeQuoted,
                                   Args,
                                   Values) {
  Quo <- "\\'"
  DoubleQuo <- '\\"'
  Quos <- paste0("[", Quo, DoubleQuo, "]")
  Pattern <- paste0("^", Quos, "[a-zA-Z0-9-]*", Quos, "$")

  QuotedInArgs <- Args %in% ShouldBeQuoted
  if (any(QuotedInArgs)) {
    if (!grepl(Pattern, Values[QuotedInArgs])) {
      # not quoted: quoting
      Values[QuotedInArgs] <-
        paste0('"', Values[QuotedInArgs], '"')
    }
  }

  Values
}

.add_SimSetupRow <- function(SimParams_Userargs) {
  if (SimParams_Userargs == "") {
    return("")
  }

  SimParams_UserMaps <- .find_map_pairs(SimParams_Userargs)
  Args <- SimParams_UserMaps$model_terms
  Values <- SimParams_UserMaps$col_terms

  ShouldBeQuoted <- "ODE"
  Values <- .quote_EngineArguments(ShouldBeQuoted = ShouldBeQuoted,
                                   Args = Args,
                                   Values = Values)

  ArgsRow <- paste0(paste0(Args, "=",
                           Values),
                    collapse = ", ")
  ArgsRow
}

.add_engineSetupRow <- function(engineParams_Userargs) {
  if (engineParams_Userargs == "") {
    return("")
  }

  engineParams_UserMaps <- .find_map_pairs(engineParams_Userargs)
  UserArgs <- engineParams_UserMaps$model_terms

  engineParams_argslist <- names(as.list(args(engineParams)))
  userArgs_in_funcArgs <-
    unlist(lapply(UserArgs,
                  function(term, engineParams_argslist) {
                    term_indx <- grepl(paste0("^", trimws(term), "$"),
                                       engineParams_argslist,
                                       ignore.case = TRUE)
                    if (sum(term_indx) > 1) {
                      warning(
                        paste0(
                          "More than one argument named \n ",
                          engineParams_argslist[term_indx][1],
                          "\nis found in the ESTARGS block"
                        )
                      )
                    } else if (sum(term_indx) == 0) {
                      return(NA)
                    } else {
                      return(engineParams_argslist[term_indx])
                    }
                  },
                  engineParams_argslist))

  if (any(is.na(userArgs_in_funcArgs))) {
    warning(paste(
      "The following engine arguments are not applicable:\n",
      paste(UserArgs[is.na(userArgs_in_funcArgs)], collapse = ", ")
    ))

    Args <- UserArgs[!is.na(userArgs_in_funcArgs)]
  } else {
    Args <- UserArgs
  }

  ShouldBeQuotedAll <-
    c("method", "stdErr", "ODE", "scramble", "impDist")

  Values <- engineParams_UserMaps$col_terms

  for (ShouldBeQuoted in ShouldBeQuotedAll) {
    Values <- .quote_EngineArguments(ShouldBeQuoted = ShouldBeQuoted,
                                     Args = Args,
                                     Values = Values)
  }

  ArgsRow <- paste0(paste0(Args, "=",
                           Values),
                    collapse = ", ")
  ArgsRow
}

# check if some coldef statements have valid column names/dose names
.check_ColDEFBlock <-
  function(colDefs,
           KeyNames,
           inputDataColNames,
           quoted = TRUE) {
    KeyNamesCollapsed <- paste(KeyNames, collapse = "|")
    pattern <- paste0("(?<=^|\\n)\\s*(", KeyNamesCollapsed, ")\\(")

    LinesToFindColNames <-
      colDefs[grepl(pattern, colDefs, perl = TRUE)]
    columnNotFound <- FALSE
    if (quoted) {
      gregexprPattern <- '(?<=\\")\\w+(?=\\")'
    } else {
      gregexprPattern <- '(?<=\\()\\s*\\w+\\s*(?=\\))'
    }

    for (colDefLine in LinesToFindColNames) {
      ColNamesInColDef <-
        unlist(regmatches(
          colDefLine,
          gregexpr(gregexprPattern, colDefLine, perl = TRUE)
        ))
      ColNamesInColDef <- noquote(ColNamesInColDef)
      colNamesNotFound <- c()
      for (ColName in ColNamesInColDef) {
        if (!ColName %in% inputDataColNames) {
          colNamesNotFound <- c(colNamesNotFound, ColName)
        }
      }

      if (length(colNamesNotFound) > 0) {
        warning(
          "Column name(s) ",
          paste(shQuote(colNamesNotFound), collapse = ", "),
          " found in COLDEF block, line\n",
          colDefLine,
          "\nnot presented in the input dataset.",
          call. = FALSE,
          immediate. = TRUE
        )
        columnNotFound <- TRUE
      }
    }

    columnNotFound
  }

.check_ColDEFDoses <- function(colDefs, KeyNames, Doses) {
  KeyNamesCollapsed <- paste(KeyNames, collapse = "|")
  pattern <- paste0("(?<=^|\\n)\\s*(", KeyNamesCollapsed, ")\\(")

  LinesToFindColNames <-
    colDefs[grepl(pattern, colDefs, perl = TRUE)]
  if (length(Doses) == 0 && length(LinesToFindColNames) > 0) {
    stop(
      "There are no doses in the model recognized, ",
      "\nbut there are lines in COLDEF block defining special input:\n",
      paste(LinesToFindColNames, sep = "\n", collapse = "\n")
    )
  } else if (length(Doses) == 0 &&
             length(LinesToFindColNames) == 0) {
    return(FALSE)
  }

  gregexprPattern <-
    '(((inf)|(inf2)|(bolus)|(bolus2))\\s*\\()\\K\\s*\\w+\\s*(?=\\))'
  DoseNotFound <- FALSE
  for (colDefLine in LinesToFindColNames) {
    DoseInColDef <-
      unlist(regmatches(
        colDefLine,
        gregexpr(gregexprPattern, colDefLine, perl = TRUE)
      ))
    DoseInColDef <- trimws(DoseInColDef)
    if (is.null(DoseInColDef)) {
      warning(
        "Dose not found in the COLDEF block, line\n",
        colDefLine,
        call. = FALSE,
        immediate. = TRUE
      )
      DoseNotFound <- TRUE
    } else if (!DoseInColDef %in% Doses) {
      warning(
        "Dose ",
        DoseInColDef,
        " not found in the model, but found in COLDEF block, line\n",
        colDefLine,
        call. = FALSE,
        immediate. = TRUE
      )
      DoseNotFound <- TRUE
    }
  }

  DoseNotFound
}

#' Use to create model object from parsed metamodel
#'
#' Define NlmePmlModel model class instance from metamodel blocks and creates
#' NlmePmlModel model class from the given parsed metamodel. See
#' \href{https://certara.github.io/R-RsNLME/articles/metamodels_overview.html}{Metamodel
#' overview}.
#' @param mmdlfile File with metamodel description
#' @param directoryToRun Directory where the results will be stored; if
#'   `missing`, a subfolder in the current directory with the model name given
#'   in `parsedMetamodel` will be created
#'
#' @return a list  with the resulted model class instance and engine parameters.
#'   If multiple ESTARGS/SIMARGS blocks are used, a list of estimation argument
#'   classes ([NlmeEngineExtraParams()]) and simulation argument classes
#'   (`simParamsBlock`) are returned.
#'
#' @seealso [run_metamodel()]
#'
#' @examples
#'  \dontrun{
#'    ModelParamsList <-
#'      create_model_from_metamodel(
#'        mmdlfile = system.file("extdata/mmdlNoTime", "test.mmdl",
#'                               package = "Certara.RsNLME"),
#'        directoryToRun = tempdir())
#'  }
#'
#' @md
#' @export
#' @keywords internal
create_model_from_metamodel <- function(mmdlfile,
                                        directoryToRun) {
  if (missing(mmdlfile)) {
    stop("`mmdlfile` argument should be provided.")
  }

  parsedMetamodel <- parse_metamodel(mmdlfile)

  model_name <- parsedMetamodel$model_name

  if (missing(directoryToRun)) {
    directoryToRun <- model_name
  }

  directoryToRun <- .prepare_wd(directoryToRun)

  modelInfo <- NlmePmlModelInfo(modelName = model_name,
                                workingDir = directoryToRun)
  # 7 is blank model
  NLMEmodel <-
    NlmePmlModel(modelType = 7,
                 modelInfo = modelInfo)

  NLMEmodel@statements <- as.list(parsedMetamodel$MODEL_block)
  NLMEmodel@isTextual <- TRUE
  map_vector <- parsedMetamodel$MAP1_list$col_terms
  names(map_vector) <- parsedMetamodel$MAP1_list$model_terms

  id_index <-
    grepl("^(id)\\d?$", names(map_vector), ignore.case = TRUE)
  ids <- paste0(map_vector[id_index], collapse = ", ")
  # if no id in mapping, assume individual model
  NLMEmodel@isPopulation <- nzchar(ids) | grepl("id\\(", parsedMetamodel$COLDEF_block)
  # removing input options, will map it manually
  spec_input_index <-
    grepl(
      "(^ADDL$)|(^SS$)|(^II$)|(^SSOFFSET$)|(^RESET$)|(^MDV$)",
      names(map_vector),
      ignore.case = TRUE
    )
  spec_input <- map_vector[spec_input_index]
  map_vector_woInputOpt <- map_vector[!id_index &
                                        !spec_input_index]


  initColMapping(NLMEmodel) <- parsedMetamodel$DATA1
  # we need create a set of variables inside mapping class
  NLMEmodel <- parsePMLColMap(NLMEmodel)

  # check if Rate or Duration are mapped
  NLMEmodel <- .InfVarMap(NLMEmodel, map_vector_woInputOpt)

  # if the flag _MAPFROMMODEL is not given, we do manual mapping for the model
  # and id terms
  if (!parsedMetamodel$MAP1_list$MAPFROMMODEL) {
    # remove not found model terms
    map_vector_woInputOpt <-
      map_vector_woInputOpt[names(map_vector_woInputOpt) %in%
                              names(NLMEmodel@columnMapping@mapping)]
    # remove automatic mapping
    NLMEmodel@columnMapping@mapping <-
      lapply(NLMEmodel@columnMapping@mapping,
             function(x) {
               x@columnName <- "?"
               x
             })

    if (ids != "" && !is.null(NLMEmodel@columnMapping@mapping$id)) {
      NLMEmodel@columnMapping@mapping$id@columnName <- ids
    }

    for (modelTerm in names(map_vector_woInputOpt)) {
      if (!modelTerm %in% names(NLMEmodel@columnMapping@mapping)) {
        stop(
          "Mapped model term",
          modelTerm,
          "not found in the model;",
          "\nplease check the MAP block",
          call. = FALSE
        )
      }

      columnToMap <-
        map_vector_woInputOpt[which(modelTerm == names(map_vector_woInputOpt))]
      if (!columnToMap %in% colnames(NLMEmodel@inputData)) {
        stop("Mapped column ", columnToMap, " not found in the dataset.")
      }

      NLMEmodel@columnMapping@mapping[[modelTerm]]@columnName <-
        columnToMap
    }

  } else {
    # otherwise we are trying to map it
    NLMEmodel@columnMapping@mapping <-
      .map_exactTerms(NLMEmodel@columnMapping@mapping, parsedMetamodel$DATA1)

    # map the retained part of mapping given in MAP row
    if (!grepl("^[[:blank:]]*$", map_vector_woInputOpt)) {
      suppressWarnings(modelColumnMapping(NLMEmodel) <-
                         map_vector_woInputOpt)
    }

    map_vector_from_model <-
      names(which("?" == unlist(
        lapply(NLMEmodel@columnMapping@mapping,
               function(x) {
                 x@columnName
               })
      )))

    names(map_vector_from_model) <- map_vector_from_model

    modelColumnMapping(NLMEmodel) <- map_vector_from_model
  }

  NLMEmodel <- .add_covLabels(NLMEmodel, parsedMetamodel$MAP1_list)

  unmappedTerms <-
    names(which("?" == unlist(
      lapply(NLMEmodel@columnMapping@mapping,
             function(x) {
               x@columnName
             })
    )))

  # checking COLDEF block
  colDefs <-
    unlist(strsplit(parsedMetamodel$COLDEF_block, "\\r?\\n"))
  # checking for non-existed columns
  QuotedColNamesStatements <-
    c("id",
      "covr",
      "obs",
      "time",
      "reset",
      "dose",
      "dose2",
      "addl",
      "ss")
  NotQuotedColNamesStatements <-
    c("mdv", "sscol", "iicol", "ssoffcol", "addlcol")
  checkQuoted <- .check_ColDEFBlock(
    colDefs,
    KeyNames = QuotedColNamesStatements,
    inputDataColNames = colnames(NLMEmodel@inputData),
    quoted = TRUE
  )
  checkUnquoted <- .check_ColDEFBlock(
    colDefs,
    KeyNames = NotQuotedColNamesStatements,
    inputDataColNames = colnames(NLMEmodel@inputData),
    quoted = FALSE
  )
  if (checkQuoted | checkUnquoted) {
    stop("COLDEF block has column names not found in the input data.")
  }

  if (.check_ColDEFDoses(colDefs, KeyNames = c("addl", "ss"), doseNames(NLMEmodel))) {
    stop("COLDEF block has Dose names not found in the input data.")
  }

  unmappedTermsNotFound <- unmappedTerms
  for (unmappedTerm in unmappedTerms) {
    # usual case
    pattern1 <- paste0('(\\(\\s*', unmappedTerm, '\\s*<-)')
    # time or id
    pattern2 <- paste0('(^', unmappedTerm, '\\(\\s*\\")')
    pattern <- paste(pattern1, pattern2, sep = "|")
    # dose2 is a special case where A1_1 or A1_2 are possible
    if (nchar(unmappedTerm) > 2 && grepl("_(1|2)$", unmappedTerm)) {
      pattern3 <- paste0('(^dose2?\\s*\\(\\s*',
                         substr(unmappedTerm, 1, nchar(unmappedTerm) - 2),
                         '\\s*<-)')
      pattern <- paste(pattern, pattern3, sep = "|")
    }

    matchedResult <- grep(pattern, colDefs)
    if (length(matchedResult) > 0) {
      columnNamesFound <- unlist(regmatches(
        colDefs[matchedResult[1]],
        gregexpr("(?<=\")(.\\w*?)(?=\")",
                 colDefs[matchedResult[1]],
                 perl = TRUE)
      ))
      columnNamesFound <- unique(columnNamesFound)
      if (length(columnNamesFound) == 1) {
        # if some term is found in coldef, map it from columnMapping and
        # remove from the vector of unmapped terms
        # if we just remove it from coldef, it is mapped again during writing cols1.txt
        NLMEmodel@columnMapping@mapping[[unmappedTerm]]@columnName <-
          columnNamesFound
        unmappedTermsNotFound <-
          setdiff(unmappedTermsNotFound, unmappedTerm)
        colDefs <- colDefs[-matchedResult]
      } else if (length(columnNamesFound) > 1 &&
                 any(grepl("^\\s*dose2?\\s*\\(", colDefs[matchedResult]))) {
        # there's a special case of dose where more than 1 column name could be mapped
        # i.e. rate or duration
        if (any(NLMEmodel@columnMapping@mapping[[unmappedTerm]]@columnName != "?")) {
          warning(
            "mapping in MAP block will be substituted to the mapping",
            "\n presented in the COLDEF block:\n",
            colDefs[matchedResult]
          )
        }
        NLMEmodel@columnMapping@mapping[[unmappedTerm]] <- NULL
        unmappedTermsNotFound <-
          setdiff(unmappedTermsNotFound, unmappedTerm)
      } else {
        warning("The current statement in COLDEF block does not look right:\n",
                colDefs[matchedResult[1]])
      }

    }

    if (length(unmappedTermsNotFound) == 0L) {
      break()
    }
  }

  if (length(unmappedTermsNotFound) > 0) {
    warning(
      "The following model terms are unmapped:\n",
      paste(unmappedTermsNotFound, collapse = ", "),
      call. = FALSE
    )
  }

  ExtraDefinitions <-
    paste(c(colDefs,
            parsedMetamodel$TABLES_BLOCK),
          collapse = "\n")

  userDefinedExtraDefinitions(NLMEmodel) <- ExtraDefinitions

  if (sum(spec_input_index) > 0) {
    NLMEmodel <- .add_inputColdefsFromMAP(NLMEmodel, parsedMetamodel)
  }

  dosingCycles <-
    unlist(strsplit(parsedMetamodel$DOSINGCYCLE_block, split = "\\r?\\n"))
  dosingCycles <- dosingCycles[dosingCycles != ""]

  if (length(dosingCycles) > 0) {
    NLMEmodel <- .add_input_dosingCycles(NLMEmodel, dosingCycles)
  }

  engineSetup <- list()
  for (engineParams_Userargs in parsedMetamodel$ESTARGS_block) {
    engineSetupRow <- .add_engineSetupRow(engineParams_Userargs)
    if (engineSetupRow == "") {
      oneEngineSetup <-
        eval(parse(text = paste0(
          "Certara.RsNLME::engineParams(NLMEmodel)"
        )))
    } else {
      oneEngineSetup <-
        eval(parse(
          text = paste0(
            "Certara.RsNLME::engineParams(NLMEmodel,",
            engineSetupRow,
            ")"
          )
        ))
    }

    # for back compatibility treat single ESTARGS as
    # NlmeEngineExtraParams class, not a list of classes
    if (length(parsedMetamodel$ESTARGS_block) == 1 &&
        all(parsedMetamodel$SIMARGS_block == "")) {
      engineSetup <- oneEngineSetup
    } else {
      engineSetup <- append(engineSetup, oneEngineSetup)
    }
  }

  for (simParams_Userargs in parsedMetamodel$SIMARGS_block) {
    engineSetupRow <- .add_SimSetupRow(simParams_Userargs)
    if (engineSetupRow == "") {
      next
    }

    oneEngineSetup <-
      eval(parse(text = paste0("simParamsBlock(", engineSetupRow, ")")))

    engineSetup <- append(engineSetup, oneEngineSetup)
  }

  return(list(model = NLMEmodel, params = engineSetup))
}

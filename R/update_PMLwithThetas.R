.update_PMLwithThetas <- function(model, newThetas) {
  Statements <- paste(unlist(model@statements), collapse = "\n")

  if (length(newThetas) == 0 ||
      any(is.null(names(newThetas))) ||
      any(names(newThetas) == "")) {
    warning("thetas in replacement are incorrect:\n",
            unlist(newThetas))
    return(Statements)
  }

  ModelInfoFixef <- .get_fixefStrings(model)
  Bounds <- .parse_Fixefs(Statements)
  ModelInfoFixefNames <- sapply(ModelInfoFixef, `[[`, 1)
  ModelInfoFixef <- stats::setNames(ModelInfoFixef, ModelInfoFixefNames)

  ModelDir <- .prepare_wd(file.path(tempdir(TRUE), model@modelInfo@modelName))
  ModelFilePath <- file.path(ModelDir, model@dataset@modelFile)
  cat(Statements, file = ModelFilePath, append = FALSE)

  # trying to get the header
  mdlheader <-
    regmatches(Statements, regexpr("\\w+", Statements, perl = TRUE))
  overrideText <- paste0("\noverride ", mdlheader, "(){\n")
  thetasNames <- names(newThetas)
  for (ThetaName in thetasNames) {
    if (ModelInfoFixef[[ThetaName]][3] == "1") {
      Freeze <- "(freeze)"
    } else {
      Freeze <- ""
    }

    EnableNumeric <- as.numeric(ModelInfoFixef[[ThetaName]][2])
    if (EnableNumeric >= 0) {
      Enable <- paste0("(enable=c(", EnableNumeric, "))")
    } else {
      Enable <- ""
    }

    if (Bounds[[ThetaName]][1] > -Inf) {
      Lower <- Bounds[[ThetaName]][1]
    } else {
      Lower <- ""
    }

    if (Bounds[[ThetaName]][3] < Inf) {
      Upper <- Bounds[[ThetaName]][3]
    } else {
      Upper <- ""
    }

    overrideText <- paste0(overrideText,
                           "  fixef(",
                           ThetaName,
                           Freeze,
                           Enable,
                           " = c(",
                           Lower,
                           ", ",
                           sprintf("%.15g", newThetas[[ThetaName]]),
                           ", ",
                           Upper,
                           "))\n")
  }

  overrideText <- paste0(overrideText, "} \n")

  cat(overrideText, file = ModelFilePath, append = TRUE)


  newModelFilePath <-
    paste0(substr(ModelFilePath, 1, nchar(ModelFilePath) - 1), "x")
  if (exists(newModelFilePath)) {
    unlink(newModelFilePath)
  }

  ArgsForMDLReplacement <-
    paste(" -r",
          shQuote(ModelFilePath, type = "cmd"),
          shQuote(ModelDir, type = "cmd"))

  if (.Platform$OS.type == "windows") {
    system2(file.path(Sys.getenv("INSTALLDIR"), "TDL5.exe"), ArgsForMDLReplacement)
  } else {
    set_Xchmod()

    system2(file.path(Sys.getenv("INSTALLDIR"), Sys.getenv("PML_BIN_DIR"), "TDL5"),
            ArgsForMDLReplacement)
  }

  if (!file.exists(newModelFilePath)) {
    warning("Model Statements were not updated due to the error")
  } else {
    Statements <- readLines(newModelFilePath)
  }

  Statements
}

.parse_Fixefs <- function(CustomCodeToSearch) {
  Pattern <-
    paste0("(?<=fixef)(\\((?:[^()]++|(?-1))*+\\))")

  # get rid of enable
  CustomCodeToSearch <-
    gsub("\\(\\W*enable\\W*\\=\\W*c\\(\\W*\\d+\\W*\\)\\W*\\)", "", CustomCodeToSearch)

  # get rid of freeze
  CustomCodeToSearch <- gsub("\\(\\W*freeze\\W*\\)", "", CustomCodeToSearch)

  ParenthesisRegexpr <-
    gregexpr(Pattern, CustomCodeToSearch, perl = TRUE)
  if (ParenthesisRegexpr[[1]][1] == -1)
    return(list())

  Fixefs <- list()

  InParents <-
    unlist(regmatches(CustomCodeToSearch, ParenthesisRegexpr))
  for (InParent in InParents) {
    InParentSplitted <- strsplit(InParent, "[^a-zA-Z0-9_\\-\\.]", perl = TRUE)[[1]]
    FixefNames <-
      unique(InParentSplitted[nchar(InParentSplitted) > 0])
    # removing special words
    FixefNames <- FixefNames[!FixefNames %in% c("c", "enable", "freeze")]
    # removing numbers
    FixefNames <-
      FixefNames[!grepl("^([-]?[0-9]+[.]?[0-9]*|[-]?[0-9]+[L]?|[-]?[0-9]+[.]?[0-9]*[eE][-]?[0-9]+)$", FixefNames)]
    FixefNamesPattern <-
      paste0("(", FixefNames, ")", collapse = "|")
    FixefNamesPattern <-
      paste0("(?<=\\W)(", FixefNamesPattern, ")\\W")
    FixefStatements <-
      strsplit(InParent, FixefNamesPattern, perl = TRUE)[[1]]
    # the first one is a parenthesis
    FixefStatements <- FixefStatements[FixefStatements != "("]
    # removing the last parenthesis
    FixefStatements[length(FixefStatements)] <-
      gsub(")$", "", FixefStatements[length(FixefStatements)])

    for (FixefNameIndex in seq_along(FixefNames)) {
      Upper <- Inf
      Lower <- -Inf
      if (grepl("c\\(.+\\)", FixefStatements[[FixefNameIndex]])) {
        # c(,,) format
        FixefValuesParsed <-
          strsplit(FixefStatements[[FixefNameIndex]], split = "(c\\(|\\,|\\))")[[1]]
        FixefValuesParsed <- trimws(FixefValuesParsed)
        if (FixefValuesParsed[2] != "") {
          Lower <- as.numeric(FixefValuesParsed[2])
        }

        Value <- as.numeric(FixefValuesParsed[3])

        if (FixefValuesParsed[4] != "") {
          Upper <- as.numeric(FixefValuesParsed[4])
        }

      } else {
        # value format
        Value <- gsub("(\\=|\\ )", "", FixefStatements[[FixefNameIndex]])
        Value <- as.numeric(Value)
      }

      Fixefs[[FixefNames[FixefNameIndex]]] <- c(Lower = Lower,
                                                Value = Value,
                                                Upper = Upper)
    }
  }

  Fixefs
}

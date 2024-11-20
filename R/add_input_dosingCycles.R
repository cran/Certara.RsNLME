.is_numeric <- function (Value) {
  grepl("^([-]?[0-9]+[.]?[0-9]*|[-]?[0-9]+[L]?|[-]?[0-9]+[.]?[0-9]*[eE][-]?[0-9]+)$", Value)
}

.add_input_dosingCycles <- function (NLMEmodel, dosingCycles) {
  dosingCycles <- trimws(dosingCycles)
  DosingCyclesStrings <- dosingCycles[grepl("^(SS|ADDL)", dosingCycles)]
  notSSorADDL <- setdiff(dosingCycles, DosingCyclesStrings)
  notSSorADDL <- notSSorADDL[grepl("[[:word:]]+", notSSorADDL, perl = TRUE)]
  if(length(notSSorADDL) > 0) {
    warning("in the DOSING CYCLE block the rows starting not with SS or ADDL ",
            "are ignored:\n",
            paste(notSSorADDL, collapse = "\n"),
            call. = FALSE)
  }

  obligatoryModelTerms <- c("DELTA", "DOSEPOINT", "AMOUNT")
  possibleModelTerms <- c("DURATION", "RATE", "SS", "ADDL")
  ssTable <- data.frame()
  addlTable <- data.frame()
  colMap <- NLMEmodel@columnMapping@mapping
  # create tables for SS and ADDL
  for(CyclesString in DosingCyclesStrings) {
    termPairs <- .find_map_pairs(CyclesString)
    termPairs$model_terms <- toupper(termPairs$model_terms)
    obligatoryModelTermsNotInString <- setdiff(obligatoryModelTerms, termPairs$model_terms)
    notRecognizedModelTermsInString <-
      setdiff(termPairs$model_terms,
              c(obligatoryModelTerms, possibleModelTerms))

    if(length(obligatoryModelTermsNotInString) > 0) {
      stop("In the DOSING CYCLE block the row\n",
           CyclesString,
           "\nis incomplete, following terms are missing:\n",
           paste(obligatoryModelTermsNotInString, collapse = "\n"))
    } else if (length(notRecognizedModelTermsInString) > 0) {
      stop("In the DOSING CYCLE block the row\n",
           CyclesString,
           "\nhas the following unrecognized terms:\n",
           paste(notRecognizedModelTermsInString, collapse = "\n"))
    }

    if(termPairs$model_terms[1] %in% c("SS", "ADDL")) {
      Compartment <- termPairs$col_terms[termPairs$model_terms == "DOSEPOINT"]
      if(is.null(colMap[[Compartment]])) {
        stop("Current compartment: ", Compartment,
             "\nnot found in the model column mapping.",
             "\nDosing cycle string affected:\n", CyclesString)
      }

      # transorm dosepoint2 to compartment
      if(.hasSlot(colMap[[Compartment]], "variableType") &&
         !is.null(colMap[[Compartment]]@variableType$type) &&
         colMap[[Compartment]]@variableType$type == "dosepoint") {
        if(colMap[[Compartment]]@variableType$DosepointN == 2){
          DosepointN <- "2"
        } else {
          DosepointN <- ""
        }

        if(colMap[[Compartment]]@variableType$DosepointDouble) {
          Compartment <- gsub("_\\d$", "", Compartment)
        }
      } else {
        stop("Current compartment: ", Compartment,
             "\nis not recognized as a dosepoint in the model column mapping.",
             "\nDosing cycle string affected:\n", CyclesString)
      }

      Row <- data.frame(Amount = termPairs$col_terms[termPairs$model_terms == "AMOUNT"],
                        Compartment = Compartment,
                        Delta = termPairs$col_terms[termPairs$model_terms == "DELTA"],
                        DosepointN = DosepointN)

      if(!.is_numeric(Row$Amount) && ! Row$Amount %in% colnames(NLMEmodel@inputData)) {
        stop("Amount column ", Row$Amount, " not found in the data column names.\n",
             "Dosing cycle string affected:\n", CyclesString)
      } else if(.is_numeric(Row$Amount) && Row$Amount <= 0) {
        stop("Amount given ", Row$Amount, " is not positive.\n",
             "Dosing cycle string affected:\n", CyclesString)
      } else if(!.is_numeric(Row$Delta) && ! Row$Delta %in% colnames(NLMEmodel@inputData)) {
        stop("Delta column ", Row$Delta, " not found in the data column names.\n",
             "Dosing cycle string affected:\n", CyclesString)
      } else if(.is_numeric(Row$Delta) && Row$Delta <= 0) {
        stop("Delta given ", Row$Delta, " is not positive.\n",
             "Dosing cycle string affected:\n", CyclesString)
      }

      if("DURATION" %in% termPairs$model_terms) {
        Row$Duration <- termPairs$col_terms[termPairs$model_terms == "DURATION"]
        if(!.is_numeric(Row$Duration) && ! Row$Duration %in% colnames(NLMEmodel@inputData)) {
          stop("Duration column ", Row$Duration, " not found in the data column names.\n",
               "Dosing cycle string affected:\n", CyclesString)
        } else if(.is_numeric(Row$Duration) && Row$Duration <= 0) {
          stop("Delta given ", Row$Duration, " is not positive.\n",
               "Dosing cycle string affected:\n", CyclesString)
        }
      } else {
        Row$Duration <- NA
      }

      if("RATE" %in% termPairs$model_terms) {
        Row$Rate <- termPairs$col_terms[termPairs$model_terms == "RATE"]
        if(!.is_numeric(Row$Rate) && ! Row$Rate %in% colnames(NLMEmodel@inputData)) {
          stop("Rate column ", Row$Rate, " not found in the data column names.\n",
               "Dosing cycle string affected:\n", CyclesString)
        } else if(.is_numeric(Row$Rate) && Row$Rate <= 0) {
          stop("Rate given ", Row$Rate, " is not positive.\n",
               "Dosing cycle string affected:\n", CyclesString)
        }
      } else {
        Row$Rate <- NA
      }

      if(!is.na(Row$Duration) && !is.na(Row$Rate)) {
        stop("Both Rate and Duration are specified for the Dosing cycle string:\n",
             CyclesString)
      }

      if(termPairs$model_terms[1] == "SS") {
        if(.check_extradefSS(unlist(userDefinedExtraDefinitions(NLMEmodel))) > 0) {
          stop("Cannot use SS defined in dosing cycle:\n", CyclesString,
               "\nsince SS is already defined in MAP or COLDEF block.")
        }

        Row$SS <- termPairs$col_terms[termPairs$model_terms == "SS"]
        if(.is_numeric(Row$SS)) {
          stop("SS definition in \n",
               CyclesString,
               "\ncannot be numeric, i.e. should point to the exact column name:\n")
        } else if (! Row$SS %in% colnames(NLMEmodel@inputData)) {
          stop("SS column ", Row$SS, " not found in the data column names.\n",
               "Dosing cycle string affected:\n", CyclesString)
        }

        ssTable <- rbind.data.frame(ssTable, Row)
      } else { # ADDL
        if(.check_extradefADDL(unlist(userDefinedExtraDefinitions(NLMEmodel))) > 0) {
          stop("Cannot use ADDL defined in dosing cycle:\n", CyclesString,
               "\nsince ADDL is already defined in MAP or COLDEF block.")
        }

        Row$ADDL <- termPairs$col_terms[termPairs$model_terms == "ADDL"]
        if(.is_numeric(Row$ADDL)) {
          stop("ADDL definition in \n",
               CyclesString,
               "\ncannot be numeric, i.e. should point to the exact column name:\n")
        } else if (! Row$ADDL %in% colnames(NLMEmodel@inputData)) {
          stop("ADDL column ", Row$ADDL, " not found in the data column names.\n",
               "Dosing cycle string affected:\n", CyclesString)
        }

        addlTable <- rbind.data.frame(addlTable, Row)
      }
    } else {
      stop("In the DOSING CYCLE block the row\n",
           CyclesString,
           "\nis not started with SS or ADDL.")
    }
  }

  # ss("SS",  "Amount" "Rate" inf(A1) "Delta" dt "Amount" dup / "Duration" inf(A1) "Delta" dt "Amount" bolus(A1) "Delta" dt)
  if(nrow(ssTable) > 0) {
    if(length(unique(ssTable$SS)) > 1) {
      stop("More than one SS column is specified. NLME does not support it.")
    }
    # ss column
    colDefs <- paste0("ss(", shQuote(ssTable$SS[1], type = "cmd"), ",")
    for(Row in 1:nrow(ssTable)) {
      # amount
      Amount <- ssTable$Amount[Row]
      if(!.is_numeric(Amount)) {
        Amount <- shQuote(Amount, type = "cmd")
      }
      colDefs <- paste(colDefs, Amount)

      # rate/duration
      Rate <- ssTable$Rate[Row]
      Duration <- ssTable$Duration[Row]
      if(!is.na(Rate)) {
        if(!.is_numeric(Rate)) {
          Rate <- shQuote(Rate, type = "cmd")
        }
        colDefs <- paste(colDefs,
                         Rate,
                         paste0("inf", ssTable$DosepointN[Row], "(", ssTable$Compartment[Row], ")"))
      } else if(!is.na(Duration)) {
        if(!.is_numeric(Duration)) {
          Duration <- shQuote(Duration, type = "cmd")
        }
        colDefs <- paste(colDefs,
                         "dup /", Duration,
                         paste0("inf", ssTable$DosepointN[Row], "(", ssTable$Compartment[Row], ")"))
      } else {
        colDefs <- paste(colDefs,
                         paste0("bolus", ssTable$DosepointN[Row], "(", ssTable$Compartment[Row], ")"))
      }
      # delta
      Delta <- ssTable$Delta[Row]
      if(!.is_numeric(Delta)) {
        Delta <- shQuote(Delta, type = "cmd")
      }

      colDefs <- paste(colDefs, Delta, "dt")
    }
    colDefs <- paste0(colDefs, ")\n")
  } else {
    colDefs <- ""
  }

  # addl("ADDL",  "Delta" dt "Amount" "Rate" inf(A1) "Delta" dt "Amount" dup / "Duration" inf(A1) "Delta" dt "Amount" bolus(A1))
  if(nrow(addlTable) > 0) {
    if(length(unique(addlTable$ADDL)) > 1) {
      stop("More than one ADDL column is specified. NLME does not support it.")
    }

    # addl column
    colDefs <- paste0(colDefs, "addl(", shQuote(addlTable$ADDL[1], type = "cmd"), ",")
    for(Row in 1:nrow(addlTable)) {
      # delta
      Delta <- addlTable$Delta[Row]
      if(!.is_numeric(Delta)) {
        Delta <- shQuote(Delta, type = "cmd")
      }

      colDefs <- paste(colDefs, Delta, "dt")
      # amount
      Amount <- addlTable$Amount[Row]
      if(!.is_numeric(Amount)) {
        Amount <- shQuote(Amount, type = "cmd")
      }
      colDefs <- paste(colDefs, Amount)

      Rate <- addlTable$Rate[Row]
      Duration <- addlTable$Duration[Row]

      if(!is.na(Rate)) {
        if(!.is_numeric(Rate)) {
          Rate <- shQuote(Rate, type = "cmd")
        }
        colDefs <- paste(colDefs,
                         Rate,
                         paste0("inf", addlTable$DosepointN[Row], "(", addlTable$Compartment[Row], ")"))
      } else if(!is.na(Duration)) {
        if(!.is_numeric(Duration)) {
          Duration <- shQuote(Duration, type = "cmd")
        }
        colDefs <- paste(colDefs,
                         "dup /", Duration,
                         paste0("inf", addlTable$DosepointN[Row], "(", addlTable$Compartment[Row], ")"))
      } else {
        colDefs <- paste(colDefs,
                         paste0("bolus", addlTable$DosepointN[Row], "(", addlTable$Compartment[Row], ")"))
      }
    }

    colDefs <- paste0(colDefs, ")\n")
  }

  userDefinedExtraDefinitions(NLMEmodel) <-
    c(unlist(NLMEmodel@userDefinedExtraDefs), colDefs)

  NLMEmodel
}

#' Initialize random effects statement
#'
#' Initializes the random effect statement. It should be used after all model components have been created.
#'
#' @param .Object    PK/PD model
#'
#' @include pml_model.r
#'
#' @examples
#' \donttest{
#' initRandomEffects(model) <- c(PMLStringForRanef)
#'
#' initRandomEffects(model) <- c(
#'   BlockOrDiagonal,
#'   isFrozen,
#'   listOfVariables,
#'   listOfValues
#' )
#'
#' initRandomEffects(model) <- c(
#'   Block,
#'   FALSE,
#'   "nV,nCl,nKa,nV2",
#'   "0.2, 0, 0.2, 0, 0, 0.2, 0, 0, 0, 0.1"
#' )
#'
#' initRandomEffects(model) <- c(Diagonal, FALSE, "nV,nCl", "0.1, 0.02")
#'
#' initRandomEffects(model) <- c(
#'   Diagonal, FALSE, "nV,nCl", "0.1, 0.02",
#'   Block, TRUE, "nCl2,nV2", "0.2, 0, 0.2"
#' )
#'
#' initRandomEffects(model) <- c("block(nCl, nV)(freeze) =
#'                                c(0.2, 0, 0.2), diag(nKa)(freeze) =
#'                                c(0.2), diag(nV2) =
#'                                c(0.1), diag(nV3, nCl2, nCl3) =
#'                                c(1, 1, 1)")
#' }
#' @keywords internal
#' @noRd
setGeneric(
  name = "initRandomEffects",
  def = function(.Object) {
    standardGeneric("initRandomEffects")
  }
)

setMethod(
  "initRandomEffects",
  "NlmePmlModel",
  definition = function(.Object) {
    if (.Object@isTextual) {
      pos <- grep("ranef\\(", .Object@statements)
      if (length(pos) != 0) {
        statement <- .Object@statements[[pos[[1]]]]
      }
    } else {
      statement <- randomBlockStatement(.Object)
    }
    statement
  }
)

#'
#' @keywords internal
setGeneric(
  name = "initRandomEffects<-",
  def = function(.Object, value) {
    standardGeneric("initRandomEffects<-")
  }
)

setMethod(
  "initRandomEffects<-",
  "NlmePmlModel",
  definition = function(.Object, value) {
    values <- value
    effectsParams <- .Object@effectsParams
    randomEffectsStatements <-
      .Object@randomEffectsStatements
    sps <- .Object@structuralParams
    if (.Object@isTextual) {
      if (length(values) == 0) {
        warning("thetas in replacement are incorrect:\n", unlist(values))
        return(statements)
      }

      INSTALLDIR <- Sys.getenv("INSTALLDIR")
      if (!Certara.NLME8::checkInstallDir(INSTALLDIR)) {
        stop("Cannot use NLME without valid NLME executables")
      }

      statements <-
        paste(unlist(.Object@statements), collapse = "\n")
      mdlheader <-
        regmatches(statements, regexpr("\\w+", statements, perl = TRUE))
      overrideText <-
        paste0("\noverride ", mdlheader, "(){\n")

      pos <- grep("ranef\\(", unlist(.Object@statements))

      blocks <- c()
      for (p in pos) {
        line <- unlist(.Object@statements)[p]
        line <- unlist(strsplit(line, "ranef\\("))[[2]]
        line <- gsub("\\s+", "", line)
        tokens <-
          unlist(strsplit(line, "\\)\\(|\\)=c\\(|\\(|\\)=c|\\),|\\)=|="))
        indx <- 1
        while (indx < length(tokens)) {
          typ <- tokens[[indx]]
          if (typ == "diag") {
            typ <- Diagonal
          } else {
            typ <- Block
          }
          indx <- indx + 1
          names <- tokens[[indx]]
          indx <- indx + 1
          nxt <- tokens[[indx]]
          indx <- indx + 1
          freeze <- FALSE
          if (nxt == "freeze") {
            vals <- tokens[[indx]]
            freeze <- TRUE
            indx <- indx + 1
          }
          block <- NlmeRandomEffectBlock(typ,
                                         as.list(unlist(strsplit(names, ","))),
                                         freeze)
          blocks <- c(blocks, block)
        }

        rv <-
          NlmeRandomEffectValues(as.list(dimnames(values)[[1]]),
                                 values =
                                   values)
        .Object@randomValues <- rv
      }
      .Object@randomBlocks <- as.list(blocks)

      overrideText <-
        paste0(overrideText,
               "ranef(block(",
               paste0(dimnames(values)[[1]], collapse = ","),
               ")=c(")
      for (i in 1:length(dimnames(values)[[1]])) {
        for (j in 1:i) {
          overrideText <- paste0(overrideText, values[i, j])
          if (i != length(dimnames(values)[[1]]) ||
              j != length(dimnames(values)[[1]])) {
            overrideText <- paste0(overrideText, ",")
          }
        }
      }
      overrideText <- paste0(overrideText, "))\n } \n")

      wd <- tempdir(TRUE)
      modelName <- model@modelInfo@modelName
      mf <- model@dataset@modelFile
      ModelDir <- file.path(wd, modelName)
      ModelFilePath <- file.path(ModelDir, mf)
      cat(statements, file = ModelFilePath, append = FALSE)
      cat(overrideText, file = ModelFilePath, append = TRUE)

      if (.Platform$OS.type == "windows") {
        ModelDir <- gsub("/", "\\", ModelDir, fixed = TRUE)
        ModelFilePath <-
          gsub("/", "\\", ModelFilePath, fixed = TRUE)
      }
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
        system2(file.path(INSTALLDIR, "TDL5.exe"),
                ArgsForMDLReplacement)
      } else {
        set_Xchmod()

        system2(file.path(INSTALLDIR, Sys.getenv("PML_BIN_DIR"), "TDL5"),
                ArgsForMDLReplacement)
      }

      if (!file.exists(newModelFilePath)) {
        warning("Model statements were not updated due to the error")
      } else {
        .Object@statements <- list(readLines(newModelFilePath))
      }
    } else {
      if (length(values) == 1) {
        .Object@randomValues@values <- values
      } else {
        .Object@randomValuesInitialized <- FALSE
        .Object <- initializeRandomEffectsBlock(.Object)
        randomEffectNames <- getRandomEffectNames(.Object)
        len <- length(values)
        usedVars <- c()
        if (len %% 4 != 0) {
          warning("Usage : initRandomEffects() wrong number of arguments")
          return(.Object)
        } else {
          .Object@randomBlocks <- list()
          str <- ""
          num <- len / 4
          for (indx in 1:num) {
            if (indx > 1) {
              str <- paste0(str, ", ")
            } else {
              str <- ""
            }
            what <- values[[(indx - 1) * 4 + 1]]
            frozen <-
              as.logical(values[[(indx - 1) * 4 + 2]])
            vars <- values[[(indx - 1) * 4 + 3]]
            names <- unlist(strsplit(vars, split = ","))
            for (n in names) {
              pos <- grep(paste0("^", trimws(n, "both"), "$"), randomEffectNames)
              if (length(pos) == 0) {
                stop(paste0(n, " : Is not a valid random effect name"))
              }
            }
            vals <- values[[(indx - 1) * 4 + 4]]
            typ <- Diagonal
            if (what == Block) {
              typ <- Block
            } else {
              typ <- Diagonal
            }
            block <- NlmeRandomEffectBlock(typ,
                                           as.list(names),
                                           frozen)
            .Object@randomBlocks[[indx]] <- block
            vals <- unlist(strsplit(vals, split = ","))
            usedVars <- c(usedVars, names)
            if (typ == Diagonal) {
              for (i in 1:length(vals)) {
                .Object@randomValues <- updateValue(.Object@randomValues,
                                                    names[[i]],
                                                    vals[[i]])
              }
            } else {
              indx <- 1
              for (i in 1:length(names)) {
                for (j in 1:i) {
                  .Object@randomValues <- updateValue2(.Object@randomValues,
                                                       names[[i]],
                                                       names[[j]],
                                                       vals[[indx]])
                  indx <- indx + 1
                }
              }
            }
          }
          usedVars <- unique(usedVars)
        }
        if (length(randomEffectNames) != length(usedVars)) {
          extraVars <- c()
          for (r in randomEffectNames) {
            pos <- grep(paste0("^", trimws(r, "both"), "$"), usedVars)
            if (length(pos) == 0) {
              extraVars <- c(extraVars, r)
            } else {
              if (length(extraVars) != 0) {
                block <- NlmeRandomEffectBlock(Diagonal,
                                               as.list(extraVars),
                                               FALSE) # JC
                .Object@randomBlocks[[length(.Object@randomBlocks) +
                                        1]] <- block
                extraVars <- c()
              }
            }
          }
          if (length(extraVars) != 0) {
            block <- NlmeRandomEffectBlock(Diagonal,
                                           as.list(extraVars),
                                           FALSE) # jc
            .Object@randomBlocks[[length(.Object@randomBlocks) + 1]] <-
              block
          }
        }
        .Object@randomEffectsStatements <- as.list(str)
      }
    }
    .Object <- generatePML(.Object)
    return(.Object)
  }
)

#' Initializes random effects structure from structural parameters
#'
#' Initializes random effects structure from structural parameters
#'
#' @param .Object   PK/PD model
#' @keywords internal
#' @noRd
#'
setGeneric(
  name = "initializeRandomEffectsBlock",
  def = function(.Object) {
    standardGeneric("initializeRandomEffectsBlock")
  }
)

setMethod(
  "initializeRandomEffectsBlock",
  "NlmePmlModel",
  definition = function(.Object) {
    if (.Object@randomValuesInitialized == FALSE) {
      structuralParams <- .Object@structuralParams
      names <- c()
      estimates <- c()
      if (length(structuralParams) > 0) {
        for (i in 1:length(structuralParams)) {
          stp <- structuralParams[[i]]
          name <- stp@name
          if (stp@isSequential) {
            randomEffName <- ""
            ranEffInitValue <- ""
          } else {
            randomEffName <- stp@randomEffName
            ranEffInitValue <- stp@ranEffInitValue
          }

          if (randomEffName != "") {
            estimates <- c(estimates, ranEffInitValue)
            names <- c(names, randomEffName)
          }
        }
      }

      if (length(names) > 0) {
        .Object@randomValues <-
          NlmeRandomEffectValues(as.list(names), effectValues = as.list(estimates))
        .Object@randomBlocks <-
          c(NlmeRandomEffectBlock(Diagonal, as.list(names)))
        .Object@randomValuesInitialized <- TRUE
      }
    }
    .Object
  }
)




lookupValue <- function(randomValues, effName) {
  pos <-
    grep(paste0("^", trimws(effName, "both"), "$"), colnames(randomValues@values))
  val <- randomValues@values[pos, pos]
  val
}

lookupValue2 <- function(randomValues, effName, effName2) {
  pos <-
    grep(paste0("^", trimws(effName, "both"), "$"), colnames(randomValues@values))
  pos2 <-
    grep(paste0("^", trimws(effName2, "both"), "$"), colnames(randomValues@values))
  val <- randomValues@values[pos, pos2]
  val
}

updateValue <- function(randomValues, effName, value) {
  pos <-
    grep(paste0("^", trimws(effName, "both"), "$"), colnames(randomValues@values))
  randomValues@values[pos, pos] <- value
  randomValues
}

updateValue2 <- function(randomValues, effName, effName2, value) {
  pos <-
    grep(paste0("^", trimws(effName, "both"), "$"), colnames(randomValues@values))
  pos2 <-
    grep(paste0("^", trimws(effName2, "both"), "$"), colnames(randomValues@values))
  randomValues@values[pos, pos2] <- value
  randomValues
}

#' Returns occasional random block statement
#'
#' @param .Object   PK/PD model
#'
#' @keywords internal
#' @noRd
setGeneric(
  name = "randomOccasionalBlockStatement",
  def = function(.Object) {
    standardGeneric("randomOccasionalBlockStatement")
  }
)

setMethod(
  "randomOccasionalBlockStatement",
  "NlmePmlModel",
  definition = function(.Object) {
    statement <- ""
    for (c in .Object@covariateList) {
      if (c@type == Occasion) {
        variables <- c()
        items <- c@covarItems
        effects <- c@covarEffList
        isEnabled <-
          effects == 1 # Added to generate ran eff statement for only effects that are enabled
        values <- c@catEffInitValues
        effects <- effects[isEnabled]
        values <- unlist(c@catEffInitValues[isEnabled])
        stParmNames <- names(effects)
        names(values) <- stParmNames
        new_values <- c()

        if (length(effects) > 0) {
          if (length(effects) == length(values)) {
            # diagonal
            for (eff_name in stParmNames) {
              name <- paste0("n", eff_name, "x", c@name, items[[1]]@value)
              variables <- c(variables, name)
              new_value <- lookupValue(.Object@randomValues, name)
              if (length(new_value) == 0) {
                # not found in the random matrix; will use the initial value
                new_value <- values[[eff_name]]
              }

              new_values <-
                c(new_values,
                  new_value)
            }

            occasion_ranef_statement <- paste0(
              "    ranef(diag(",
              paste(as.character(variables), collapse = ","),
              ") = c(",
              paste(as.character(new_values), collapse = ","),
              ")"
            )
          } else {
            # block
            if (all(stParmNames %in% colnames(.Object@randomValues@values))) {
              for (i in 1:length(stParmNames)) {
                name <- paste0("n", stParmNames[[i]], "x", c@name, items[[1]]@value)
                variables <- c(variables, name)
                for (j in 1:i) {
                  if (i == j) {
                    val <- lookupValue(.Object@randomValues, name)
                  } else {
                    name2 <- paste0("n", stParmNames[[j]], "x", items[[1]]@value)
                    val <-
                      lookupValue2(.Object@randomValues, name, name2)
                  }
                  new_values <- c(new_values, val)
                }
              }
            } else {
              # will use default since some (all) values are not in random matrix
              variables <- paste0("n", stParmNames, "x", c@name, items[[1]]@value)
              new_values <- values
            }

            occasion_ranef_statement <- paste0(
              "    ranef(block(",
              paste(as.character(variables), collapse = ","),
              ") = c(",
              paste(as.character(new_values), collapse = ","),
              ")"
            )
          }

          if (length(items) > 1) {
            for (i in 2:length(items)) {
              variables <- c()
              for (indx in 1:length(stParmNames)) {
                name <-
                  paste0("n", stParmNames[[indx]], "x", c@name, items[[i]]@value)
                variables <- c(variables, name)
              }
              occasion_ranef_statement <- paste0(
                occasion_ranef_statement,
                ", same(",
                paste(as.character(variables), collapse = ","),
                ")"
              )
            }
          }

          occasion_ranef_statement <-
            paste0(occasion_ranef_statement, ")")
          statement <-
            c(statement, occasion_ranef_statement)
        }
      }
    }
    statement
  }
)

#' Returns random block statement
#'
#' Returns random block statement
#'
#' @param .Object   PK/PD model
#'
#' @keywords internal
setGeneric(
  name = "randomBlockStatement",
  def = function(.Object) {
    standardGeneric("randomBlockStatement")
  }
)


setMethod(
  "randomBlockStatement",
  "NlmePmlModel",
  definition = function(.Object) {
    structuralParams <- .Object@structuralParams
    names <- c()
    estimates <- c()
    statement <- ""
    if (length(structuralParams) > 0) {
      for (i in 1:length(structuralParams)) {
        stp <- structuralParams[[i]]
        name <- attr(stp, "name")
        randomEffName <- attr(stp, "randomEffName")
        ranEffInitValue <- attr(stp, "ranEffInitValue")
        if (randomEffName != "") {
          estimates <- c(estimates, ranEffInitValue)
          names <- c(names, randomEffName)
        }
      }
    }
    if (length(.Object@randomBlocks) > 0) {
      statement <- ""
      firstBlock <- TRUE
      for (b in .Object@randomBlocks) {
        if (b@type == Diagonal) {
          s <- "diag("
        } else {
          s <- "block("
        }
        first <- TRUE
        for (v in b@effectNames) {
          if (first == FALSE) {
            s <- paste0(s, ",")
          }
          s <- paste0(s, v)
          first <- FALSE
        }
        s <- paste0(s, ")")
        if (b@frozen == TRUE) {
          s <- paste0(s, " (freeze) ")
        }
        s <- paste0(s, " = c(")
        first <- TRUE
        if (b@type == Diagonal) {
          for (i in 1:length(b@effectNames)) {
            v <- b@effectNames[[i]]
            val <- lookupValue(.Object@randomValues, v)
            if (first == FALSE) {
              s <- paste0(s, ",")
            }
            s <- paste0(s, val)
            first <- FALSE
          }
          s <- paste0(s, ")")
        } else {
          for (i in 1:length(b@effectNames)) {
            for (j in 1:i) {
              if (i == j) {
                v <- b@effectNames[[i]]
                val <- lookupValue(.Object@randomValues, v)
              } else {
                v <- b@effectNames[[i]]
                v2 <- b@effectNames[[j]]
                val <-
                  lookupValue2(.Object@randomValues, v, v2)
              }
              if (first == FALSE) {
                s <- paste0(s, ",")
              }
              s <- paste0(s, val)
              first <- FALSE
            }
          }
          s <- paste0(s, ")")
        }
        if (firstBlock == TRUE) {
          statement <- paste0(statement, s)
        } else {
          statement <- paste0(statement, ",", s)
        }
        firstBlock <- FALSE
      }
      statement <- paste0("    ranef(", statement, ")")
    }
    statement
  }
)

#' Class initializaer for NlmePmlModel object
#'
#' The following class represents an NLME/PML model object.
#'
#' @slot isPopulation                        Is this a population model (TRUE) or
#'                                                 individual (FALSE)?
#' @slot modelType                           Taken from NlmeModelType
#' @slot isTimeBased                         Is model time-based?
#' @slot linearModelType                     Type of linear model
#' @slot isLinearFrozen                      Is linear model frozen?
#' @slot pkModelAttrs                        Taken from NlmePkParameters
#' @slot indirectModelAttrs                  Taken from NlmeIndirectParameters
#' @slot emaxModelAttrs                      Taken from NlmeEmaxParameters
#' @slot hasEffectsCompartment               Is there data available for an effects compartment?
#' @slot errorModel                          Taken from NlmeErrorModel
#' @slot structuralParams                    List of structural parameters
#' @slot outputParams                        List of output parameters
#' @slot diffEquations                       List of differential equations
#' @slot statements                          List of PML statements
#' @slot dosePoints                          List of dosepoints
#' @slot covariateList                       List of covariates
#' @slot columnMapping                       Taken from NlmeColumnMapping
#' @slot doseMapping                         Taken from NlmeDoseMapping
#' @slot paramsMapping                       Taken from NlmeParamsMapping
#' @slot randParamsMapping                   Taken from NlmeRandParamsMapping
#' @slot inputData                           Input data source
#' @slot doseData                            Dose data source
#' @slot fixedParamData                      Fixed effect parameter data source
#' @slot randParamData                       Random effect parameter data source
#' @slot isTextual                           Is model textual (TRUE) or graphical (FALSE)?
#' @slot pmloutput                           List of PML output to generate
#' @slot modelInfo                           Taken from NlmePmlModelInfo
#' @slot objects                             deprecated
#' @slot objectsNeedRegenerating             deprecated
#' @slot randomEffectsStatements             Custom random effects statements
#' @slot randomOccasionalEffectsStatements   Custom random occasional effects statements
#' @slot userDefinedExtraDefs                Custom definition for extra column
#'                                                 and table generation
#'
#' @include NlmeRandomEffectBlock.r
#' @include error_model.r
#' @include NlmeColumnMapping.r
#' @include NlmeDataset.r
#' @examples
#' \donttest{
#' # helper class
#' setClass("NlmePmlModelInfo",
#'          slots = c(modelName = "character",
#'                    workingDir = "character"))
#' Model <-
#'   new("NlmePmlModel",
#'   modelInfo = new("NlmePmlModelInfo", modelName = "Model", workingDir = "."))
#' }
#'
#' @keywords internal
setClass(
  "NlmePmlModel",
  slots = c(
    isPopulation = "logical",
    modelType = "NlmeModelType",
    isTimeBased = "logical",
    linearModelType = "numeric",
    isLinearFrozen = "logical",
    pkModelAttrs = "NlmePkParameters",
    indirectModelAttrs = "NlmeIndirectParameters",
    emaxModelAttrs = "NlmeEmaxParameters",
    hasEffectsCompartment = "logical",
    errorModel = "NlmeErrorModel",
    structuralParams = "list",
    effectsParams = "list",
    outputParams = "list",
    diffEquations = "list",
    statements = "list",
    dosePoints = "list",
    covariateList = "list",
    columnMapping = "NlmeColumnMapping",
    colStatements = "list",
    doseMapping = "NlmeDoseMapping",
    paramsMapping = "NlmeParamsMapping",
    randParamsMapping = "NlmeRandParamsMapping",
    inputData = "ANY",
    doseData = "ANY",
    extraDoses = "list",
    resetInfo = "ANY",
    hasResetInfo = "logical",
    fixedParamData = "ANY",
    randParamData = "ANY",
    isTextual = "logical",
    pmloutput = "list",
    randomEffectsStatements = "list",
    randomOccasionalEffectsStatements = "list",
    modelInfo = "NlmePmlModelInfo",
    objects = "ANY",
    objectsNeedRegenerating = "logical",
    nextKey = "numeric",
    isObjectModel = "logical",
    userDefinedExtraDefs = "list",
    secondaryParameters = "list",
    dataset = "NlmeDataset",
    randomValues = "NlmeRandomEffectValues",
    randomBlocks = "list",
    randomValuesInitialized = "logical"
  ),
  prototype = list(
    isPopulation = TRUE,
    isTextual = FALSE,
    isTimeBased = TRUE,
    isLinearFrozen = FALSE,
    hasEffectsCompartment = FALSE,
    dataset = NULL,
    randomValues = NULL,
    randomBlocks = NULL
  )
) -> NlmePmlModel


setMethod("initialize", "NlmePmlModel",
          function(.Object,
                   isPopulation = TRUE,
                   isTextual = FALSE,
                   modelType = PARAM_PK,
                   isTimeBased = TRUE,
                   linearModelType = LINEAR_ALPHA_TYPE,
                   isLinearFrozen = FALSE,
                   pkModelAttrs = NlmePkParameters(),
                   indirectModelAttrs = NlmeIndirectParameters(),
                   emaxModelAttrs = NlmeEmaxParameters(),
                   hasEffectsCompartment = FALSE,
                   errorModel = NlmeErrorModel(),
                   modelInfo = ANY,
                   dataset = NULL,
                   randomValues = NULL,
                   randomBlocks = NULL) {
            .Object@modelInfo <- NlmePmlModelInfo()
            .Object@isPopulation <- isPopulation
            .Object@isTextual <- isTextual
            .Object@modelType <- NlmeModelType(modelType)
            .Object@isTimeBased <- isTimeBased
            .Object@linearModelType <- linearModelType
            .Object@isLinearFrozen <- isLinearFrozen
            .Object@pkModelAttrs <- pkModelAttrs
            .Object@indirectModelAttrs <- indirectModelAttrs
            .Object@emaxModelAttrs <- emaxModelAttrs
            .Object@hasEffectsCompartment <- hasEffectsCompartment
            .Object@errorModel <- errorModel
            .Object@modelInfo <- modelInfo
            .Object@hasResetInfo <- FALSE
            .Object@objects <- list()
            .Object@extraDoses <- list()
            .Object@randomEffectsStatements <- list()
            .Object@randomOccasionalEffectsStatements <- list()
            .Object@objectsNeedRegenerating <- TRUE
            .Object@nextKey <- 1
            .Object@isObjectModel <- FALSE
            .Object@userDefinedExtraDefs <- list()
            .Object@colStatements <- list()
            .Object@secondaryParameters <- list()
            if (is.null(dataset)) {
              dataset <- NlmeDataset(.Object@modelInfo@workingDir)
            }
            .Object@dataset <- dataset
            if (!is.null(randomValues)) {
              .Object@randomValues <- randomValues
            }
            if (!is.null(randomBlocks)) {
              .Object@randomBlocks <- randomBlocks
            }
            .Object@randomValuesInitialized <- FALSE
            .Object
          })

print_PKattributes <- function(x) {
  paramType <- x@pkModelAttrs@parameterization@paramType
  ModelParametrizationNames <-
    c("Micro", "Clearance", "Macro", "Macro1")
  paramName <- ModelParametrizationNames[paramType]

  absorpType <- x@pkModelAttrs@absorption@absorpType
  ModelAbsorptionNames <- c("Intravenous",
                            "FirstOrder",
                            "Gamma",
                            "Weibull",
                            "InverseGaussian")
  absorpName <- ModelAbsorptionNames[absorpType]

  cat("\n PK \n ------------------------------------------- \n")
  cat(paste("Parameterization  : ", paramName), fill = TRUE)
  cat(paste("Absorption        : ", absorpName), fill = TRUE)
  cat(paste("Num Compartments  : ", x@pkModelAttrs@numCompartments),
      fill = TRUE)
  cat(paste("Dose Tlag?        : ", x@pkModelAttrs@isTlag), fill = TRUE)
  flag <- x@pkModelAttrs@hasEliminationComp
  cat(paste("Elimination Comp ?: ", flag), fill = TRUE)
  if (flag == TRUE) {
    cat(paste("Fraction Excreted : ",
              x@pkModelAttrs@isFractionExcreted),
        fill = TRUE)
  }
  flag <- x@pkModelAttrs@infusionAllowed
  cat(paste("Infusion Allowed ?: ", flag), fill = TRUE)
  if (flag == TRUE) {
    cat(paste("Duration          : ", x@pkModelAttrs@isDuration), fill = TRUE)
  }
  cat(paste("Sequential        : ", x@pkModelAttrs@isSequential),
      fill = TRUE)
  cat(paste("Freeze PK         : ", x@pkModelAttrs@isPkFrozen), fill = TRUE)
}

print_EmaxAttributes <- function(x) {
  cat("\n PD \n ------------------------------------------- \n")
  cat(paste("Check Baseline    : ", x@emaxModelAttrs@checkBaseline),
      fill = TRUE)
  cat(paste("Check Inhibitory  : ", x@emaxModelAttrs@checkInhibitory),
      fill = TRUE)
  cat(paste("Check Sigmoid     : ", x@emaxModelAttrs@checkSigmoid),
      fill = TRUE)
  cat(paste("Check Fractional  : ", x@emaxModelAttrs@checkFractional),
      fill = TRUE)
  cat(paste(
    "Effect Compartment: ",
    x@emaxModelAttrs@hasEffectsCompartment
  ),
  fill = TRUE)
  cat(paste("Freeze PD         : ", x@emaxModelAttrs@frozen), fill = TRUE)
}

print_IndirectAttributes <- function(x) {
  name <- .indirectTypeNames[x@indirectModelAttrs@type]
  cat("\n Indirect \n ------------------------------------------- \n")
  cat(paste("Indirect Type     : ", name), fill = TRUE)
  cat(paste(
    "Effect Compartment: ",
    x@indirectModelAttrs@hasEffectsCompartment
  ),
  fill = TRUE)
  cat(paste("Buildup           : ", x@indirectModelAttrs@isBuildup),
      fill = TRUE)
  cat(paste("Exponent          : ", x@indirectModelAttrs@isExponent),
      fill = TRUE)
  cat(paste("Indirect Frozen   : ", x@indirectModelAttrs@frozen),
      fill = TRUE)
}

print_LinearAttributes <- function(x) {
  name <- ModelLinearNames[x@linearModelType]
  cat("\n Linear \n ------------------------------------------- \n")
  cat(paste("Linear Type       : ", name), fill = TRUE)
  cat(paste("Linear Frozen     : ", x@isLinearFrozen), fill = TRUE)
  cat(paste("Effect Compartment: ", attr(x, "hasEffectsCompartment")), fill = TRUE)
}

#' Print generic for class NlmePmlModel
#'
#' Prints model information, including PML and column mappings.
#'
#' @param x NlmePmlModel class instance
#' @inheritParams ellipsis::dots_used
#'
#' @examples
#' model <- pkmodel(columnMap = FALSE,
#'                  data = pkData,
#'                  workingDir = tempdir())
#' print(model)
#'
#' @return \code{NULL}
#' @export
print.NlmePmlModel <- function(x, ...) {
  cat("\n Model Overview \n ------------------------------------------- \n")

  cat(paste("Model Name        : ", x@modelInfo@modelName), fill = TRUE)
  cat(paste("Working Directory : ", x@modelInfo@workingDir), fill = TRUE)

  if (!x@isTextual) {
    modelType <- x@modelType@modelType
    modelTypeName <- ModelTypeNames[modelType]
    cat(paste("Is population     : ", x@isPopulation), fill = TRUE)
    cat(paste("Model Type        : ", modelTypeName), fill = TRUE)
    if (length(grep("PK", modelTypeName))) {
      print_PKattributes(x)
    }

    if (length(grep("EMAX", modelTypeName))) {
      print_EmaxAttributes(x)
    }

    if (length(grep("INDIRECT", modelTypeName))) {
      print_IndirectAttributes(x)
    }

    if (length(grep("LINEAR", modelTypeName))) {
      print_LinearAttributes(x)
    }
  } else {
    cat(paste("Model Type        : ", "Textual"), fill = TRUE)
  }

  cat("\n PML \n ------------------------------------------- \n")
  cat(unlist(x@statements), sep = "\n")
  cat(
    "\n Structural Parameters \n ------------------------------------------- \n",
    paste0(structuralParameterNames(x))
  )

  if (!x@isTextual) {
    errorModel <- x@errorModel
    print(errorModel)
  } else {
    cat("\n ------------------------------------------- \n")
  }

  cat(" Column Mappings \n ------------------------------------------- \n")
  cat("Model Variable Name : Data Column name\n")

  id_mapping <- ""
  id_mapped <- FALSE
  time_mapping <- ""
  dosepoint_mapping <- ""
  covariate_mapping <- ""
  observation_mapping <- ""
  random_effects_mapping <- ""

  for (mapping in x@columnMapping@mapping) {
    if (mapping@variableType$type == "id") {
      id_mapping <-
        paste0(
          id_mapping,
          sprintf("%-19s", mapping@variableName),
          " : ",
          mapping@columnName,
          "\n"
        )

      id_mapped <- TRUE

    } else if (mapping@variableType$type == "time") {
      time_mapping <-
        paste0(
          time_mapping,
          sprintf("%-19s", mapping@variableName),
          " : ",
          mapping@columnName,
          "\n"
        )
    } else if (mapping@variableType$type == "dosepoint" ||
               mapping@variableType$type == "dosepointInf" ||
               mapping@variableType$type == "extraDoses") {
      dosepoint_mapping <-
        paste0(
          dosepoint_mapping,
          sprintf("%-19s", mapping@variableName),
          " : ",
          mapping@columnName,
          "\n"
        )
    } else if (mapping@variableType$type == "covariate") {
      cov_levels <- ""
      printLvls <- FALSE
      for (cov in x@covariateList) {
        if (cov@name == mapping@variableName &&
            cov@type == 2 && length(cov@covarItems) > 0) {
          cov_levels <- "("
          for (cov_level in cov@covarItems) {
            if (cov_level@name != "") {
              printLvls <- TRUE
            }
            cov_levels <-
              paste0(cov_levels,
                     " ",
                     cov_level@name,
                     "=",
                     cov_level@value)
          }
          cov_levels <- paste0(cov_levels, " )")
        }
      }
      if (!printLvls) {
        cov_levels <- ""
      }
      covariate_mapping <-
        paste0(
          covariate_mapping,
          sprintf("%-19s", mapping@variableName),
          " : ",
          mapping@columnName,
          cov_levels,
          "\n"
        )
    } else {
      observation_mapping <-
        paste0(
          observation_mapping,
          sprintf("%-19s", mapping@variableName),
          " : ",
          mapping@columnName,
          "\n"
        )
    }
  }

  for (mapping in x@randParamsMapping@mapping) {
    random_effects_mapping <-
      paste0(
        random_effects_mapping,
        sprintf("%-19s", mapping@variableName),
        " : ",
        mapping@columnName,
        "\n"
      )
  }

  extraDefs <- ""
  for (colDef in x@userDefinedExtraDefs) {
    if (grepl("^(addl\\s?\\()|(addlcol\\s?\\()", colDef)) {
      extraDefs <-
        paste0(extraDefs,
               "ADDL                : ",
               sub(".*\\((.*)\\).*", "\\1", colDef, perl = TRUE),
               "\n")
    } else if (grepl("^(ss\\s?\\()|(sscol\\s?\\()", colDef)) {
      extraDefs <-
        paste0(
          extraDefs,
          "SteadyState         : ",
          sub(".*\\((.*)\\).*", "\\1", colDef, perl = TRUE),
          "\n"
        )
    } else if (grepl("^ssoffcol\\s?\\(", colDef)) {
      extraDefs <-
        paste0(extraDefs,
               "SSOffset            : ",
               sub(".*\\((.*)\\).*", "\\1", colDef, perl = TRUE),
               "\n")
    } else if (grepl("^mdv\\s?\\(", colDef)) {
      extraDefs <-
        paste0(extraDefs,
               "MDV                 : ",
               sub(".*\\(\"(.*)\"\\).*", "\\1", colDef, perl = TRUE),
               "\n")
    } else if (grepl("^iicol\\s?\\(", colDef)) {
      extraDefs <-
        paste0(extraDefs,
               "II                  : ",
               sub(".*\\((.*)\\).*", "\\1", colDef, perl = TRUE),
               "\n")
    } else if (grepl("^reset\\s?\\(", colDef)) {
      extraDefs <-
        paste0(
          extraDefs,
          "Reset               : ",
          sub(".*\\(\"(.*)\",.*", "\\1", colDef, perl = TRUE),
          "[",
          x@resetInfo@low,
          ",",
          x@resetInfo@hi,
          "]",
          "\n"
        )
    }
  }

  cat(
    paste0(
      id_mapping,
      time_mapping,
      dosepoint_mapping,
      covariate_mapping,
      observation_mapping,
      random_effects_mapping,
      extraDefs
    ),
    fill = TRUE
  )

  if (id_mapped && !x@isPopulation) {
    warning("`id` model term will be ignored when the model is in individual mode.",
            call. = FALSE)
  } else if (!id_mapped && x@isPopulation) {
    warning(
      "`id` model term is not found, but the model is in pop mode.\n",
      "Please use parsePMLColMap() to update the mapping and then map with colMapping().",
      call. = FALSE
    )
  }
}

setMethod(
  "show",
  "NlmePmlModel",
  definition = function(object) {
    print(object)
  }
)

setGeneric(
  name = "createIndirectStructuralParameters",
  def = function(.Object) {
    standardGeneric("createIndirectStructuralParameters")
  }
)

setMethod(
  "createIndirectStructuralParameters",
  "NlmePmlModel",
  definition = function(.Object) {
    structuralParams <- .Object@structuralParams
    dosePoints <- .Object@dosePoints
    dosePoints <- list()
    outputParams <- .Object@outputParams
    diffEquations <- attr(.Object, "diffEquations")
    attrs <- attr(.Object, "indirectModelAttrs")

    type <- attrs@type
    hasEffectsCompartment <- .Object@hasEffectsCompartment
    isBuildup <- attrs@isBuildup
    isExponent <- attrs@isExponent
    frozen <- attrs@frozen

    sCOutput <- "C"
    bFreeze <- FALSE
    if (frozen) {
      bFreeze <- TRUE
    }
    if (.Object@isPopulation == FALSE) {
      hasRandomEffect <- FALSE
    } else {
      hasRandomEffect <- !bFreeze
    }

    if (hasEffectsCompartment) {
      structuralParams <- addStructuralParameter(structuralParams,
                                                 "Ke0",
                                                 hasRandomEffect =
                                                   hasRandomEffect,
                                                 isFrozen = bFreeze)
    }

    structuralParams <- addStructuralParameter(structuralParams,
                                               "Kin",
                                               hasRandomEffect = hasRandomEffect,
                                               isFrozen = bFreeze)
    structuralParams <- addStructuralParameter(structuralParams,
                                               "Kout",
                                               hasRandomEffect = hasRandomEffect,
                                               isFrozen = bFreeze)

    sMax <- ""
    s50 <- ""

    if (type == LIMITED_STIM) {
      sMax <- "Emax"
      s50 <- "EC50"
    }
    if (type == INFINITE_STIM) {
      sMax <- ""
      s50 <- "EC50"
    }
    if (type == LIMITED_INHIB) {
      sMax <- "Imax"
      s50 <- "IC50"
    }
    if (type == INVERSE_INHIB) {
      sMax <- "Imax"
      s50 <- "IC50"
    }
    if (type == LINEAR_STIM) {
      sMax <- ""
      s50 <- "s"
    }
    if (type == LOG_LINEAR_STIM) {
      sMax <- ""
      s50 <- "s"
    }

    if (type %in% c(LIMITED_STIM, LIMITED_INHIB, INVERSE_INHIB)) {
      structuralParams <- addStructuralParameter(structuralParams,
                                                 sMax,
                                                 hasRandomEffect =
                                                   hasRandomEffect,
                                                 isFrozen = bFreeze)
    }

    structuralParams <- addStructuralParameter(structuralParams,
                                               s50,
                                               hasRandomEffect = hasRandomEffect,
                                               isFrozen = bFreeze)
    if (isExponent) {
      structuralParams <- addStructuralParameter(structuralParams,
                                                 "gam",
                                                 hasRandomEffect =
                                                   hasRandomEffect,
                                                 isFrozen = bFreeze)
    }

    if (bFreeze == FALSE) {
      outputParams <- c(outputParams, "E")
    }
    .Object@structuralParams <- structuralParams
    .Object@outputParams <- outputParams
    return(.Object)
  }
)

setGeneric(
  name = "modelColumnMapping",
  def = function(.Object) {
    standardGeneric("modelColumnMapping")
  }
)

setMethod(
  "modelColumnMapping",
  "NlmePmlModel",
  definition = function(.Object) {
    map <- .Object@columnMapping
    return(map)
  }
)

setGeneric(
  name = "modelColumnMapping<-",
  def = function(.Object, value) {
    standardGeneric("modelColumnMapping<-")
  }
)

setMethod(
  "modelColumnMapping<-",
  "NlmePmlModel",
  definition = function(.Object, value) {
    modelTermsToMap <- names(value)
    if (is.null(.Object@inputData)) {
      stop("missing input data, use `initColMapping()` to specify input data")
    }

    mcols <- modelColumnMapping(.Object)
    mcolsNames <- names(mcols@mapping)
    `%notin%` <- Negate(`%in%`)

    if (is.null(.Object@inputData)) {
      dt <- data.table::as.data.table(.Object@inputData)
    } else {
      dt <- data.table::setDT(.Object@inputData)
    }

    for (modelTerm in modelTermsToMap) {
      columnToMap <- value[[modelTerm]]
      if (modelTerm %notin% mcolsNames) {
        warning("'",
                modelTerm,
                "' does not exist in model variables and won't be mapped",
                call. = FALSE)
        next()
      }

      if (modelTerm == "id") {
        idcol <- unlist(strsplit(columnToMap, split = "(,)|(\\s+)"))
        idcol <- idcol[!is.na(idcol) & idcol != ""]
        if (length(idcol) > 5) {
          stop("number of columns specified for `id` cannot exceed 5")
        }
        .check_column_mappings(idcol, .Object@inputData)
      } else {
        .check_column_mappings(columnToMap, .Object@inputData)
      }

      if (.Object@columnMapping@mapping[[modelTerm]]@variableType$type == "covariate") {
        if ((
          .Object@columnMapping@mapping[[modelTerm]]@variableType$covType == COVAR_OCCASION ||
          .Object@columnMapping@mapping[[modelTerm]]@variableType$covType == COVAR_CATEGORY
        )) {
          checkCatCovariateMappingColumn(.Object@columnMapping@mapping,
                                         .Object@inputData,
                                         columnToMap,
                                         modelTerm)
        } else if (.Object@columnMapping@mapping[[modelTerm]]@variableType$covType == COVAR_CONTINUOUS) {
          if (!is.null(.Object@columnMapping@mapping$id) &&
              .Object@columnMapping@mapping$id@columnName != "?") {
            id_col_names <-
              gsub(" ",
                   "",
                   .Object@columnMapping@mapping$id@columnName)
            numCovariatesData <- dt[,
                                    .(group_column_name = any(unlist(lapply(
                                      .SD, .is_numeric
                                    )))),
                                    by = c(id_col_names),
                                    .SDcols = c(columnToMap)]

            df <-
              numCovariatesData[group_column_name == FALSE |
                                  is.na(group_column_name),]
            if (nrow(df) > 0) {
              df$group_column_name <- NULL
              if (ncol(df) == 1) {
                stop(
                  paste0(
                    "Error: continuous covariate ",
                    modelTerm,
                    " must have at least one numeric value assigned for each subject \n",
                    "Following subject(s) does not have one: ",
                    paste0(df[[.Object@columnMapping@mapping$id@columnName]], collapse = ", ")
                  )
                )
              } else {
                sub_ids <- ""
                for (row in 1:nrow(df)) {
                  sub_ids <-
                    paste0(sub_ids,
                           paste0(colnames(df), "=", df[row,], collapse = " "),
                           ";")
                }
                stop(
                  paste0(
                    "Error: continuous covariate ",
                    modelTerm,
                    " must have at least one numeric value assigned for each subject \n",
                    "Following subject(s) does not have one: ",
                    sub_ids
                  )
                )
              }
            }
          }
        }
      }

      .Object@columnMapping@mapping[[modelTerm]]@columnName <-
        columnToMap
    }

    mcols <- modelColumnMapping(.Object)
    reqcols <-
      c(
        "time",
        "id",
        "A",
        "A1",
        "Aa",
        "A_Duration",
        "A_Rate",
        "A1_Duration",
        "A1_Rate",
        "Aa_Duration",
        "Aa_Rate",
        "C"
      )

    for (modelTermMap in mcols@mapping) {
      if (any(reqcols %in% modelTermMap@variableName) &&
          modelTermMap@columnName == "?") {
        stop(paste0(
          "'",
          modelTermMap@variableName,
          "' requires column mapping"
        ))
      } else if (modelTermMap@columnName == "?") {
        warning(paste0("'", modelTermMap@variableName, "' not mapped"))
      }
    }
    return(.Object)
  }
)

setGeneric(
  name = "modelDoseMapping",
  def = function(.Object) {
    standardGeneric("modelDoseMapping")
  }
)

setMethod(
  "modelDoseMapping",
  "NlmePmlModel",
  definition = function(.Object) {
    map <- .Object@doseMapping
    return(map)
  }
)


setGeneric(
  name = "modelDoseMapping<-",
  def = function(.Object, value) {
    standardGeneric("modelDoseMapping<-")
  }
)

setMethod(
  "modelDoseMapping<-",
  "NlmePmlModel",
  definition = function(.Object, value) {
    names <- names(value)
    for (i in 1:length(names)) {
      n <- names[i]
      v <- value[[n]]
      mappedDose(.Object, n) <- v
    }
    return(.Object)
  }
)


#' Add an extra dose definition to the model
#'
#' Adds an extra dose definition to the model
#'
#' @param .Object     PK/PD model
#' @param name        Name of the compartment to which the dose is administered
#' @param doseType    Character. Options are "SteadyState" or "ADDL"
#' @param doses       List of treatment doses
#' @param SteadyState Character corresponding to SteadyState column in input data
#' @param ADDL        Character corresponding to ADDL column in input data
#' @keywords internal
#' @noRd
setGeneric(
  name = "addExtraDose",
  def = function(.Object,
                 name,
                 doseType,
                 doses,
                 SteadyState = NULL,
                 ADDL = NULL) {
    standardGeneric("addExtraDose")
  }
)

setMethod(
  "addExtraDose",
  "NlmePmlModel",
  definition = function(.Object,
                        name,
                        doseType = "SteadyState",
                        doses,
                        SteadyState = NULL,
                        ADDL = NULL) {
    if (doseType == "SteadyState") {
      doseType <- SteadyStateDose
    } else if (doseType == "ADDL") {
      doseType <- AddlDose
    } else {
      stop("argument `doseType` must be one of 'SteadyState' or 'ADDL'")
    }

    if (!is.vector(doses)) {
      doses <- c(doses)
    }

    dose <-
      ExtraDoseOption(name = name,
                      doseType = doseType,
                      doses = doses)
    .Object@extraDoses <- c(.Object@extraDoses, dose)

    if (!is.null(ADDL) && !is.null(SteadyState)) {
      stop("cannot provide both ADDL and SteadyState column mappings")
    }

    # if (!is.null(ADDL) || !is.null(SteadyState)) {
    mdata <- .Object@inputData
    # if (is.null(mdata)) {
    #  stop("input data not found, use `initColMapping()` to specify input data set")
    # } else {
    variableTypeList <- list(
      type = "extraDoses",
      dosepoint = name,
      amount = doses[[1]]@amount,
      ii = doses[[1]]@deltaTime,
      rate = doses[[1]]@rate,
      duration = doses[[1]]@duration,
      isSecondDose = doses[[1]]@isSecondDose
    )

    if (!is.null(SteadyState)) {
      if (length(mdata) == 0) {
        stop("input data not found, use `initColMapping()` to specify input data set")
      } else {
        .check_column_mappings(SteadyState, data = mdata)
        .Object@columnMapping@mapping$SteadyState <-
          NlmeColumnMap(
            variableName = "SteadyState",
            columnName = SteadyState,
            variableType = variableTypeList
          )
        # .Object@columnMapping@mapping$SteadyState@variableType <- variableTypeList
      }
    } else if (!is.null(ADDL)) {
      if (length(mdata) == 0) {
        stop("input data not found, use `initColMapping()` to specify input data set")
      } else {
        .check_column_mappings(ADDL, data = mdata)
        .Object@columnMapping@mapping$ADDL <-
          NlmeColumnMap(
            variableName = "ADDL",
            columnName = ADDL,
            variableType = variableTypeList
          )
      }
    }

    if (is.null(SteadyState) && doseType == SteadyStateDose) {
      .Object@columnMapping@mapping$SteadyState <-
        NlmeColumnMap(
          variableName = "SteadyState",
          columnName = "?",
          variableType = variableTypeList
        )
      # .Object@columnMapping@mapping$SteadyState@variableType <- variableTypeList
    } else if (is.null(ADDL) && doseType == AddlDose) {
      .Object@columnMapping@mapping$ADDL <-
        NlmeColumnMap(
          variableName = "ADDL",
          columnName = "?",
          variableType = variableTypeList
        )
    }



    .Object <- generatePML(.Object)
    return(.Object)
  }
)


setGeneric(
  name = "modelParamsMapping",
  def = function(.Object) {
    standardGeneric("modelParamsMapping")
  }
)

setMethod(
  "modelParamsMapping",
  "NlmePmlModel",
  definition = function(.Object) {
    map <- attr(.Object, "paramsMapping")
    return(map)
  }
)


setGeneric(
  name = "modelRandParamsMapping",
  def = function(.Object) {
    standardGeneric("modelRandParamsMapping")
  }
)

setMethod(
  "modelRandParamsMapping",
  "NlmePmlModel",
  definition = function(.Object) {
    map <- .Object@randParamsMapping
    return(map)
  }
)


setGeneric(
  name = "modelRandParamsMapping<-",
  def = function(.Object, value) {
    standardGeneric("modelRandParamsMapping<-")
  }
)

setMethod(
  "modelRandParamsMapping<-",
  "NlmePmlModel",
  definition = function(.Object, value) {
    if (is.null(.Object@inputData)) {
      stop("missing input data, use `initRandParamsMapping()` to specify input data")
    }

    mcols <- modelRandParamsMapping(.Object)
    mcolsNames <-
      vector(mode = "list", length = length(mcols@mapping))
    `%notin%` <- Negate(`%in%`)
    for (i in seq_along(mcols@mapping)) {
      mcolsNames[[i]] <- mcols@mapping[[i]]@variableName
    }
    dat <- .Object@inputData
    names <- names(value)
    for (i in 1:length(names)) {
      n <- names[i]
      v <- value[[n]]
      if (n %notin% mcolsNames) {
        warning(paste0("'", n, "' does not exist in random parameter mappings"))
      }
      .check_column_mappings(v, dat)
      mappedRandParams(.Object, n) <- v
    }
    mcols <- modelRandParamsMapping(.Object)

    for (i in seq_along(mcols@mapping)) {
      if (mcols@mapping[[i]]@columnName == "?") {
        stop(paste0("'", mcols@mapping[[i]]@variableName, "' not mapped"))
      }
    }
    return(.Object)
  }
)


setGeneric(
  name = "initColMapping<-",
  def = function(.Object, value) {
    standardGeneric("initColMapping<-")
  }
)

setMethod(
  "initColMapping<-",
  "NlmePmlModel",
  definition = function(.Object, value) {
    .Object@columnMapping <- NlmeColumnMapping(.Object, value)
    .Object@inputData <- value
    return(.Object)
  }
)

setGeneric(
  name = "initDoseColMapping<-",
  def = function(.Object, value) {
    standardGeneric("initDoseColMapping<-")
  }
)

setMethod(
  "initDoseColMapping<-",
  "NlmePmlModel",
  definition = function(.Object, value) {
    map <- NlmeDoseMapping(.Object, value)
    .Object@doseMapping <- map
    .Object@doseData <- value
    return(.Object)
  }
)

setGeneric(
  name = "initParamsMapping<-",
  def = function(.Object, value) {
    standardGeneric("initParamsMapping<-")
  }
)

setMethod(
  "initParamsMapping<-",
  "NlmePmlModel",
  definition = function(.Object, value) {
    map <- NlmeParamsMapping(.Object, value)
    .Object@paramsMapping <- map
    .Object@fixedParamData <- value
    return(.Object)
  }
)


setGeneric(
  name = "initRandParamsMapping<-",
  def = function(.Object, value) {
    standardGeneric("initRandParamsMapping<-")
  }
)

setMethod(
  "initRandParamsMapping<-",
  "NlmePmlModel",
  definition = function(.Object, value) {
    map <- NlmeRandParamsMapping(.Object, value)
    .Object@randParamsMapping <- map
    .Object@randParamData <- value
    return(.Object)
  }
)


setGeneric(
  name = "mappedColumn<-",
  def = function(.Object, varName, value) {
    standardGeneric("mappedColumn<-")
  }
)

setMethod(
  "mappedColumn<-",
  "NlmePmlModel",
  definition = function(.Object, varName, value) {
    if (.Object@isTextual) {
      map <- .Object@columnMapping
      map@mapping[[varName]] <- NlmeColumnMap(varName, value)
      .Object@columnMapping <- map
    } else {
      names <- modelVariableNames(.Object)
      if (!is.na(match(varName, names))) {
        # Removed check for textual models above
        map <- .Object@columnMapping
        map@mapping[[varName]] <-
          NlmeColumnMap(varName, value)
        .Object@columnMapping <- map
      }
    }
    return(.Object)
  }
)

setGeneric(
  name = "mappedDose<-",
  def = function(.Object, varName, value) {
    standardGeneric("mappedDose<-")
  }
)

setMethod(
  "mappedDose<-",
  "NlmePmlModel",
  definition = function(.Object, varName, value) {
    names <- doseNames(.Object)
    if (!is.na(match(varName, names))) {
      map <- attr(.Object, "doseMapping")
      cm <- attr(map, "mapping")
      n <- NlmeColumnMap(varName, value)
      cm[[varName]] <- n
      map@mapping <- cm
      .Object@doseMapping <- map
    }
    return(.Object)
  }
)


setGeneric(
  name = "mappedRandParams<-",
  def = function(.Object, varName, value) {
    standardGeneric("mappedRandParams<-")
  }
)

setMethod(
  "mappedRandParams<-",
  "NlmePmlModel",
  definition = function(.Object, varName, value) {
    names <- randParameterNames(.Object)
    if (!is.na(match(varName, names))) {
      map <- .Object@randParamsMapping
      cm <- map@mapping
      n <- NlmeColumnMap(varName, value)
      cm[[varName]] <- n
      map@mapping <- cm
      .Object@randParamsMapping <- map
    }
    return(.Object)
  }
)

#' Sets residual effect attributes
#'
#' Sets residual effect attributes
#'
#' @param .Object      PK/PD model object
#' @param effectName   Effect to lookup/set attributes for
#' @param value        A value to be set
#' @keywords internal
setGeneric(
  name = "residualEffect<-",
  def = function(.Object, effectName, value) {
    standardGeneric("residualEffect<-")
  }
)

setMethod(
  "residualEffect<-",
  "NlmePmlModel",
  definition = function(.Object, effectName, value) {
    obsCompIndx <- -1
    errorModel <- attr(.Object, "errorModel")
    effectsList <- attr(errorModel, "effectsList")


    if (length(effectsList) > 0) {
      for (indx in 1:length(effectsList)) {
        effect <- effectsList[[indx]]
        name <- attr(effect, "effectName")
        if (name == effectName) {
          if (!is.na(value["effectName"])) {
            effect@effectName <- value[["effectName"]]
          }
          if (!is.na(value["observeName"])) {
            effect@observeName <- value[["observeName"]]
          }
          if (!is.na(value["epsilonName"])) {
            effect@epsilonName <- value[["epsilonName"]]
          }
          if (!is.na(value["errorType"])) {
            effect@errorType <- as.integer(value[["errorType"]])
            if (effect@errorType == ERR_ADD_MULT) {
              effect@definition <- paste0(effect@effectName, "MultStdev")
            }
            if (effect@errorType == ERR_MIX_RATIO) {
              effect@definition <- paste0(effect@effectName, "MixRatio")
            }
            if (effect@errorType == ERR_POWER &&
                effect@definition == "") {
              effect@definition <- paste0("2")
            }
          }
          if (!is.na(value["frozen"])) {
            effect@frozen <- as.logical(value[["frozen"]])
          }
          if (!is.na(value["SD"])) {
            effect@SD <- as.numeric(value[["SD"]])
          }
          if (!is.na(value["definition"])) {
            effect@definition <- value[["definition"]]
          }
          if (!is.na(value["isBQL"])) {
            effect@isBQL <- as.logical(value[["isBQL"]])
          }
          if (!is.na(value["bqlStaticValue"])) {
            effect@bqlStaticValue <- value[["bqlStaticValue"]]
          }
          if (!is.na(value["dobefore"])) {
            effect@dobefore <- value[["dobefore"]]
          }
          if (!is.na(value["doafter"])) {
            effect@doafter <- value[["doafter"]]
          }
          effectsList[[indx]] <- effect
        }
      }
    }

    errorModel@effectsList <- effectsList
    .Object@errorModel <- errorModel
    .Object <- updateErrorModel(.Object)

    .Object <- generatePML(.Object)

    return(.Object)
  }
)


#' Sets style for a covariate/variable
#'
#' Sets style for a covariate/variable
#'
#' @param .Object           A PK/PD model
#' @param covariateName     Name of the covariate
#' @param parameterName     Name of the model variable
#' @param value             A value to set
#' @keywords internal
setGeneric(
  name = "covariateEffect<-",
  def = function(.Object,
                 covariateName,
                 parameterName,
                 value) {
    standardGeneric("covariateEffect<-")
  }
)

setMethod(
  "covariateEffect<-",
  "NlmePmlModel",
  definition = function(.Object,
                        covariateName,
                        parameterName,
                        value) {
    ModelCovNames <- covariateNames(.Object)
    if (length(ModelCovNames) > 0) {
      whichName <- which(ModelCovNames == covariateName)
      if (length(whichName) > 0) {
        .Object@covariateList[[whichName]]@covarEffList[[parameterName]] <-
          value
      } else {
        message("Covariate ", covariateName, " is not found in the model.")
      }
    }

    # covariateList <- .Object@covariateList
    # indx <- 1
    # if (length(covariateList) > 0) {
    #   for (indx in 1:length(covariateList)) {
    #     c <- covariateList[[indx]]
    #     name <- attr(c, "name")
    #     if (name == covariateName) {
    #       effectsList <- c@covarEffList
    #       effectsList[[parameterName]] <- value
    #       attrc@covarEffList <- effectsList
    #       covariateList[[indx]] <- c
    #     }
    #   }
    # }
    # .Object@covariateList <- covariateList
    # # Added generatePML function here as PML was not updated after specifying covariate effect
    # # .Object <- generatePMLModel(.Object) #Added to addCovariate
    return(.Object)
  }
)


#' Set initial values for an inter-occasion variability
#'
#' Set initial values for an inter-occasion variability
#'
#' @param .Object          A PK/PD model
#' @param covariateName    Name of the occasion covariate
#' @param value            A value to be set
#' @keywords internal
#' @noRd
initOccasionRandomEffect <-
  function(.Object,
           covariateName,
           values) {
    covariateList <- .Object@covariateList
    for (indx in seq_along(covariateList)) {
      CovariateInstance <- covariateList[[indx]]

      if (CovariateInstance@name == covariateName &&
          CovariateInstance@type == Occasion) {
        covariateList[[indx]]@catEffInitValues <- as.list(values)
      }
    }

    .Object@covariateList <- covariateList
    .Object
  }

#' Lists user defined extra column/table definition
#'
#' Lists user defined extra column/table definition
#'
#' @param .Object          A PK/PD model
#' @param userDefinedList  Character vector of extra column/table definitions
#'
#' @examples
#' \donttest{
#' userDefinedExtraDefinitions(model)
#' }
#' @keywords internal
#' @noRd
setGeneric(
  name = "userDefinedExtraDefinitions",
  def = function(.Object, userDefinedList) {
    standardGeneric("userDefinedExtraDefinitions")
  }
)

setMethod(
  "userDefinedExtraDefinitions",
  "NlmePmlModel",
  definition = function(.Object, userDefinedList) {
    userDefinedExtraDefs <- .Object@userDefinedExtraDefs

    userDefinedExtraDefs
  }
)

#' Sets user defined extra column/table definition
#'
#' Sets user defined extra column/table definition
#'
#' @param .Object          A PK/PD model
#' @param value            Character vector of extra column/table definitions
#'
#' @examples
#' \donttest{
#' userDefinedExtraDefinitions(model) <- c(
#'   "addlcol(ADDL)",
#'   " iicol(II)", "table(file=\"res.csv\",time(0),Ka,V,Cl,Tlg)"
#' )
#' }
#' @keywords internal
#' @noRd
setGeneric(
  name = "userDefinedExtraDefinitions<-",
  def = function(.Object, value) {
    standardGeneric("userDefinedExtraDefinitions<-")
  }
)


setMethod(
  "userDefinedExtraDefinitions<-",
  "NlmePmlModel",
  definition = function(.Object, value) {
    .Object@userDefinedExtraDefs <- as.list(value)

    return(.Object)
  }
)


#' Adds user defined extra column/table definitions to column definition file
#'
#' Adds user defined extra column/table definitions to column definition file
#'
#' @param .Object       PK/PD model
#' @param value         Character vector of extra column/table definitions
#'
#' @return Modified \code{NlmePmlModel} object
#' @examples
#' \donttest{
#' model <- pkmodel(columnMap = FALSE,
#'                  workingDir = tempdir())
#' model <- addExtraDef(model, c("addlcol(ADDL)",
#'                               "table(file=\"res.csv\",time(0),Ka,V,Cl,Tlg)"))
#' }
#'
#' @export
addExtraDef <- function(.Object, value) {
  stopifnot(inherits(.Object, "NlmePmlModel"))
  stopifnot(inherits(value, "character"))

  existing_def <- .Object@userDefinedExtraDefs

  userDefinedExtraDefinitions(.Object) <- value

  current_def <- .Object@userDefinedExtraDefs
  defs <- c(existing_def, current_def)


  .Object@userDefinedExtraDefs <- unique(defs)

  return(.Object)
}



#' Adds a secondary parameter to model definition
#'
#' @param .Object PK/PD model
#' @param name Name of the secondary parameter
#' @param definition Definition of secondary parameter
#' @param unit Optional units of the secondary parameter. The default is "".
#'
#' @return Depends on the specific methods
#'
#' @examples
#' \donttest{
#' model <- pkmodel(columnMap = FALSE,
#'                  absorption = "FirstOrder",
#'                  workingDir = tempdir())
#' model <- addSecondary(model, "Ke", "tvCl/tvV")
#' model <- addSecondary(
#'   model, "Tmax",
#'   "CalcTMax(tvA,tvCl/tvV)"
#' )
#' }
#'
#' @name addSecondary
#' @rdname addSecondary
#' @export
setGeneric(
  name = "addSecondary",
  def = function(.Object, name, definition, unit = "") {
    standardGeneric("addSecondary")
  }
)

#' @describeIn addSecondary Method for the 'NlmePmlModel' class
#'
#' This method adds a secondary parameter to the NlmePmlModel object.
#' It checks for duplicate parameter names, and if there is no duplicate,
#' it adds the new secondary parameter to the object and updates the PML model.
#'
#' @param .Object An 'NlmePmlModel' object to which you want to add a secondary parameter.
#' @param name Name of the secondary parameter.
#' @param definition Definition of secondary parameter.
#' @param unit Optional units of the secondary parameter. The default is "".
#'
#' @return Returns the 'NlmePmlModel' object with the added secondary parameter.
#'
#' @export
setMethod(
  "addSecondary",
  "NlmePmlModel",
  definition = function(.Object, name, definition, unit = "") {
    secondParams <- .Object@secondaryParameters
    found <- FALSE
    for (s in secondParams) {
      if (s@name == name) {
        found <- TRUE
        break
      }
    }
    if (found == TRUE) {
      warning(paste0("Duplicate name ", name, "is not allowed"))
    } else {
      param <- SecondaryParameter(name, definition, unit)
      secondParams[[length(secondParams) + 1]] <- param

      .Object@secondaryParameters <- secondParams
      .Object <- generatePML(.Object)
    }
    .Object
  }
)

setGeneric(
  name = "listSecondary",
  def = function(.Object) {
    standardGeneric("listSecondary")
  }
)

setMethod(
  "listSecondary",
  "NlmePmlModel",
  definition = function(.Object) {
    secondParams <- .Object@secondaryParameters
    return(secondParams)
  }
)


#' Deletes a secondary parameter from the model
#'
#' @param .Object PK/PD model
#' @param name Name of the secondary parameter to be deleted
#'
#' @return Depends on the specific methods
#'
#' @keywords internal
#' @name deleteSecondary
#' @rdname deleteSecondary
setGeneric(
  name = "deleteSecondary",
  def = function(.Object, name) {
    standardGeneric("deleteSecondary")
  }
)

#' @describeIn deleteSecondary Method for the 'NlmePmlModel' class
#'
#' This method deletes a secondary parameter from the NlmePmlModel object.
#' It searches for the parameter by name, and if it is found, it removes it
#' from the object and updates the PML model.
#'
#' @param .Object An 'NlmePmlModel' object from which you want to delete a secondary parameter.
#' @param name Name of the secondary parameter to be deleted.
#'
#' @return Returns the 'NlmePmlModel' object with the secondary parameter removed.
#'
#' @keywords internal
setMethod(
  "deleteSecondary",
  "NlmePmlModel",
  definition = function(.Object, name) {
    secondParams <- .Object@secondaryParameters
    found <- FALSE

    for (indx in 1:length(secondParams)) {
      s <- secondParams[[indx]]
      if (s@name == name) {
        secondParams[indx] <- NULL
        found <- TRUE
        break
      }
    }
    if (found == TRUE) {
      .Object@secondaryParameters <- secondParams
      .Object <- generatePML(.Object)
    } else {
      message("Secondary parameter ", name, " Not found")
    }
    .Object
  }
)

#' Method to set structural parameter attributes
#'
#' @param .Object          Model with the parameter
#' @param parameterNames   The names of structural parameters
#' @param value            A value of the fixed effect to be set
#'
#' @examples
#' \donttest{
#' structuralParam(model, "Cl") <- c(style = LogNormal, initialValue = "0.75")
#'
#' structuralParam(model, "Cl2") <- c(style = Custom, code = "stparm(V=10^(tvlog10V + nlog10V))")
#' }
#' @noRd
#' @keywords internal
setGeneric(
  name = "structuralParam<-",
  def = function(.Object, parameterNames, value) {
    standardGeneric("structuralParam<-")
  }
)

setMethod(
  "structuralParam<-",
  "NlmePmlModel",
  definition = function(.Object, parameterNames, value) {
    sps <- .Object@structuralParams
    for (parameterName in parameterNames) {
      stParmPos <-
        which(structuralParameterNames(.Object, omitEmpties = FALSE) == parameterName)
      sp <- sps[[stParmPos]]
      name <- sp@name

      if (!is.na(value["name"])) {
        sp@name <- value[["name"]]
      }
      if (!is.na(value["fixedEffName"])) {
        sp@fixedEffName <- value[["fixedEffName"]]
      }
      if (!is.na(value["randomEffName"])) {
        sp@randomEffName <- value[["randomEffName"]]
      }
      if (!is.na(value["hasRandomEffect"])) {
        sp@hasRandomEffect <- as.logical(value[["hasRandomEffect"]])
      }
      if (!is.na(value["hasCovariateEffect"])) {
        sp@hasCovariateEffect <-
          as.logical(value[["hasCovariateEffect"]])
      }
      if (!is.na(value["style"])) {
        sp@style <- as.integer(value[["style"]])
      }
      if (!is.na(value["initialValue"])) {
        sp@initialValue <- value[["initialValue"]]
      }
      if (!is.na(value["lowerBound"])) {
        sp@lowerBound <- value[["lowerBound"]]
      }
      if (!is.na(value["upperBound"])) {
        sp@upperBound <- value[["upperBound"]]
      }
      if (!is.na(value["units"])) {
        sp@units <- value[["units"]]
      }
      if (!is.na(value["isFrozen"])) {
        sp@isFrozen <- as.logical(value[["isFrozen"]])
      }
      if (!is.na(value["isSequential"])) {
        sp@isSequential <- as.logical(value[["isSequential"]])
      }
      if (!is.na(value["code"])) {
        sp@code <- value[["code"]]
      }
      extraCode <- c()
      for (i in 1:10) {
        key <- paste0("extraCode", i)
        if (!is.na(value[key])) {
          extraCode <- c(extraCode, value[[key]])
        }
      }
      if (length(extraCode) != 0) {
        sp@extraCode <- as.list(extraCode)
      }
      sps[[stParmPos]] <- sp
    }
    .Object@structuralParams <- sps
    .Object <- generatePMLModel(.Object)
    return(.Object)
  }
)

spExists <- function(structuralParams, paramName) {
  found <- FALSE
  for (s in structuralParams) {
    if (s@name == paramName) {
      found <- TRUE
    }
  }
  return(found)
}

addStructuralParameter <- function(structuralParams,
                                   paramName,
                                   hasRandomEffect = TRUE,
                                   hasCovariateEffect = FALSE,
                                   isFrozen = FALSE,
                                   isSequential = FALSE) {
  if (hasRandomEffect && isFrozen) {
    hasRandomEffect <- FALSE
  }

  sp <- NlmeStructuralParameter(
    paramName,
    hasRandomEffect = hasRandomEffect,
    hasCovariateEffect = hasCovariateEffect,
    isFrozen = isFrozen,
    isSequential = isSequential
  )
  indx <- length(structuralParams) + 1
  structuralParams[[indx]] <- sp
  return(structuralParams)
}

removeStructuralParameter <- function(structuralParams, paramName) {
  for (indx in 1:length(structuralParams)) {
    sp <- structuralParams[[indx]]

    if (attr(sp, "name") == paramName) {
      structuralParams[indx] <- NULL
    }
  }
  return(structuralParams)
}


#'
setGeneric(
  name = "modelStatements",
  def = function(.Object) {
    standardGeneric("modelStatements")
  }
)

setMethod(
  "modelStatements",
  "NlmePmlModel",
  definition = function(.Object) {
    statements <- .Object@statements
    return(statements)
  }
)

setGeneric(
  name = "modelStatements<-",
  def = function(.Object, value) {
    standardGeneric("modelStatements<-")
  }
)

setMethod(
  f = "modelStatements<-",
  "NlmePmlModel",
  definition = function(.Object, value) {
    .Object@statements <- value
    return(.Object)
  }
)


# setGeneric(
#   name = "errorModel",
#   def = function(.Object) {
#     standardGeneric("errorModel")
#   }
# )
#
# setMethod("errorModel", "NlmePmlModel",
#   definition = function(.Object) {
#     errorModel <- .Object@errorModel
#     return(errorModel)
#   }
# )
#
# setGeneric("errorModel<-", "NlmePmlModel",
#   def = function(.Object, value) {
#     standardGeneric("errorModel<-")
#   }
# )
#
# setMethod("errorModel<-", "NlmePmlModel",
#   definition = function(.Object, value) {
#     .Object@errorModel <- value
#     return(.Object)
#   }
# )


#' Updates error model
#'
#' Updates error model, recreating some structural parameters
#' that depend on how residual effects are used
#' @noRd
#' @keywords internal
setGeneric(
  name = "updateErrorModel",
  def = function(.Object) {
    standardGeneric("updateErrorModel")
  }
)

setMethod(
  "updateErrorModel",
  "NlmePmlModel",
  definition = function(.Object) {
    errorModel <- attr(.Object, "errorModel")
    effectsList <- attr(errorModel, "effectsList")
    structuralParams <- attr(.Object, "structuralParams")
    for (indx in 1:length(effectsList)) {
      effect <- effectsList[[indx]]
      definition <- attr(effect, "definition")
      errorType <- attr(effect, "errorType")
      if (errorType == ERR_ADD_MULT ||
          errorType == ERR_MIX_RATIO) {
        if (definition != "") {
          if (spExists(structuralParams, definition) == FALSE) {
            structuralParams <- addStructuralParameter(
              structuralParams,
              definition,
              hasRandomEffect =
                FALSE,
              hasCovariateEffect =
                FALSE
            )
          }
        }
      } else {
        if (definition != "") {
          structuralParams <- removeStructuralParameter(structuralParams,
                                                        definition)
        }
      }
    }
    if (length(structuralParams) != 0) {
      .Object@structuralParams <- structuralParams
    }
    return(.Object)
  }
)

#'
setGeneric(
  name = "createEmaxStructuralParameters",
  def = function(.Object) {
    standardGeneric("createEmaxStructuralParameters")
  }
)

setMethod(
  "createEmaxStructuralParameters",
  "NlmePmlModel",
  definition = function(.Object) {
    structuralParams <- attr(.Object, "structuralParams")
    dosePoints <- attr(.Object, "dosePoints")
    outputParams <- attr(.Object, "outputParams")
    diffEquations <- attr(.Object, "diffEquations")
    emaxModelAttrs <- attr(.Object, "emaxModelAttrs")
    checkInhibitory <- attr(emaxModelAttrs, "checkInhibitory")
    checkBaseline <- attr(emaxModelAttrs, "checkBaseline")
    checkSigmoid <- attr(emaxModelAttrs, "checkSigmoid")
    checkFractional <- attr(emaxModelAttrs, "checkFractional")
    isFrozen <- attr(emaxModelAttrs, "frozen")
    hasRandomEffect <- .Object@isPopulation

    # Add E to the output
    outputParams <- c(outputParams, "E")
    errorModel <- attr(.Object, "errorModel")
    effectsList <- attr(errorModel, "effectsList")
    # Figure out equation for Emax model based on other params
    s0 <- "E0"
    sMax <- ""
    s50 <- ""
    if (checkInhibitory == FALSE) {
      sMax <- "Emax"
      s50 <- "EC50"
    } else {
      sMax <- "Imax"
      s50 <- "IC50"
    }
    structuralParams <- addStructuralParameter(structuralParams,
                                               s50,
                                               hasRandomEffect = hasRandomEffect,
                                               isFrozen = isFrozen)
    if (checkSigmoid == TRUE) {
      structuralParams <- addStructuralParameter(structuralParams,
                                                 "Gam",
                                                 hasRandomEffect =
                                                   hasRandomEffect,
                                                 isFrozen = isFrozen)
    }
    if (checkBaseline == FALSE) {
      if (checkInhibitory == FALSE) {
        structuralParams <- addStructuralParameter(structuralParams,
                                                   sMax,
                                                   hasRandomEffect =
                                                     hasRandomEffect,
                                                   isFrozen = isFrozen)
      } else {
        structuralParams <- addStructuralParameter(structuralParams,
                                                   s0,
                                                   hasRandomEffect =
                                                     hasRandomEffect,
                                                   isFrozen = isFrozen)
      }
    } else {
      structuralParams <- addStructuralParameter(structuralParams,
                                                 s0,
                                                 hasRandomEffect =
                                                   hasRandomEffect,
                                                 isFrozen = isFrozen)
      structuralParams <- addStructuralParameter(structuralParams,
                                                 sMax,
                                                 hasRandomEffect =
                                                   hasRandomEffect,
                                                 isFrozen = isFrozen)
    }

    # Add standard deviation name to structural parameters

    if (length(effectsList) > 0) {
      definition <- attr(effectsList[[1]], "definition")
      errorType <- attr(effectsList[[1]], "errorType")
      if (errorType == ERR_ADD_MULT ||
          errorType == ERR_MIX_RATIO) {
        structuralParams <- addStructuralParameter(
          structuralParams,
          definition,
          hasRandomEffect =
            FALSE,
          hasCovariateEffect =
            FALSE,
          isFrozen = isFrozen
        )
      }
    }

    .Object@outputParams <- outputParams
    .Object@structuralParams <- structuralParams
    return(.Object)
  }
)

#'
setGeneric(
  name = "createLinearStructuralParameters",
  def = function(.Object) {
    standardGeneric("createLinearStructuralParameters")
  }
)

setMethod(
  "createLinearStructuralParameters",
  "NlmePmlModel",
  definition = function(.Object) {
    structuralParams <- attr(.Object, "structuralParams")
    dosePoints <- attr(.Object, "dosePoints")
    outputParams <- attr(.Object, "outputParams")
    diffEquations <- attr(.Object, "diffEquations")
    linearModelType <- .Object@linearModelType
    isLinearFrozen <- attr(.Object, "isLinearFrozen")
    cInput <- "C"

    hasRandomEffect <- .Object@isPopulation
    # Add E to the output
    outputParams <- c(outputParams, "E")
    errorModel <- attr(.Object, "errorModel")
    effectsList <- attr(errorModel, "effectsList")

    # Figure out equation for Linear model based on other params
    # Note: Statements have changed from Alpha, Beta, Gam, to Alpha0, Alpha1, Alpha2

    structuralParams <- addStructuralParameter(structuralParams,
                                               "EAlpha",
                                               hasRandomEffect = hasRandomEffect,
                                               isFrozen = isLinearFrozen)
    s <- "E = EAlpha"
    if (linearModelType >= LinearBeta) {
      s <- paste0(s, " + EBeta*", cInput)
      structuralParams <- addStructuralParameter(structuralParams,
                                                 "EBeta",
                                                 hasRandomEffect =
                                                   hasRandomEffect,
                                                 isFrozen = isLinearFrozen)
    }
    if (linearModelType >= LinearGamma) {
      s <- paste0(s, " + EGam*", cInput, "^2")
      structuralParams <- addStructuralParameter(structuralParams,
                                                 "EGam",
                                                 hasRandomEffect =
                                                   hasRandomEffect,
                                                 isFrozen = isLinearFrozen)
    }

    # Add standard deviation name to structural parameters

    if (length(effectsList) > 0) {
      definition <- attr(effectsList[[1]], "definition")
      errorType <- attr(effectsList[[1]], "errorType")
      if (errorType == ERR_ADD_MULT ||
          errorType == ERR_MIX_RATIO) {
        structuralParams <- addStructuralParameter(
          structuralParams,
          definition,
          hasRandomEffect =
            hasRandomEffect,
          hasCovariateEffect =
            FALSE
        )
      }
    }
    .Object@outputParams <- outputParams
    .Object@structuralParams <- structuralParams
    return(.Object)
  }
)

#'
setGeneric(
  name = "createPkStructuralParameters",
  def = function(.Object) {
    standardGeneric("createPkStructuralParameters")
  }
)

setMethod(
  "createPkStructuralParameters",
  "NlmePmlModel",
  definition = function(.Object) {
    structuralParams <- attr(.Object, "structuralParams")
    dosePoints <- attr(.Object, "dosePoints")
    dosePoints <- list()
    outputParams <- attr(.Object, "outputParams")
    diffEquations <- attr(.Object, "diffEquations")
    pkModelAttrs <- attr(.Object, "pkModelAttrs")

    parameterization <- attr(pkModelAttrs, "parameterization")
    paramType <- attr(parameterization, "paramType")
    absorption <- attr(pkModelAttrs, "absorption")
    absorpType <- attr(absorption, "absorpType")
    numCompartments <- attr(pkModelAttrs, "numCompartments")
    isTlag <- attr(pkModelAttrs, "isTlag")
    hasEliminationComp <- attr(pkModelAttrs, "hasEliminationComp")
    isFractionExcreted <- attr(pkModelAttrs, "isFractionExcreted")
    isSaturating <- attr(pkModelAttrs, "isSaturating")
    infusionAllowed <- attr(pkModelAttrs, "infusionAllowed")
    isDuration <- attr(pkModelAttrs, "isDuration")
    isSequential <- attr(pkModelAttrs, "isSequential")
    isPkFrozen <- attr(pkModelAttrs, "isPkFrozen")
    isClosedForm <- attr(pkModelAttrs, "isClosedForm")

    sKe <- ""
    sKa <- "Ka"
    sDose <- ""

    # Ka parameter
    sKe <- "Ke"

    hasRandomEffect <- .Object@isPopulation
    if (absorpType == PARAM_EXTRAVASCULAR) {
      structuralParams <- addStructuralParameter(
        structuralParams,
        "Ka",
        hasRandomEffect =
          hasRandomEffect,
        isFrozen = isPkFrozen,
        isSequential = isSequential
      )
    }

    if (absorpType == PARAM_WEIBULL ||
        absorpType == PARAM_GAMMA ||
        absorpType == PARAM_INVERSEGAUSSIAN) {
      structuralParams <- addStructuralParameter(
        structuralParams,
        "MeanDelayTime",
        hasRandomEffect =
          hasRandomEffect,
        isFrozen = isPkFrozen,
        isSequential = isSequential
      )
      if (absorpType == PARAM_INVERSEGAUSSIAN) {
        structuralParams <- addStructuralParameter(
          structuralParams,
          "ShapeParam",
          hasRandomEffect =
            hasRandomEffect,
          isFrozen = isPkFrozen,
          isSequential = isSequential
        )
      } else {
        structuralParams <- addStructuralParameter(
          structuralParams,
          "ShapeParamMinusOne",
          hasRandomEffect =
            hasRandomEffect,
          isFrozen = isPkFrozen,
          isSequential = isSequential
        )
      }
    }

    # Check to see if you need to add C.
    # logic is
    #  if ! ((bPKPD &&( bSequential || bFrozen ) ))
    #
    modelType <- attr(attr(.Object, "modelType"), "modelType")
    if (modelType != PARAM_PK) {
      sCOutput <- "C"
      outputParams <- c(outputParams, sCOutput)
    }

    if (paramType == PARAM_MACRO) {
      structuralParams <- addStructuralParameter(
        structuralParams,
        "A",
        hasRandomEffect =
          hasRandomEffect,
        isFrozen = isPkFrozen,
        isSequential = isSequential
      )
    } else {
      structuralParams <- addStructuralParameter(
        structuralParams,
        "V",
        hasRandomEffect =
          hasRandomEffect,
        isFrozen = isPkFrozen,
        isSequential = isSequential
      )
    }
    if (paramType == PARAM_MICRO) {
      structuralParams <- addStructuralParameter(
        structuralParams,
        sKe,
        hasRandomEffect =
          hasRandomEffect,
        isFrozen = isPkFrozen,
        isSequential = isSequential
      )
    }

    if (paramType == PARAM_MACRO1 || paramType == Macro) {
      structuralParams <- addStructuralParameter(
        structuralParams,
        "Alpha",
        hasRandomEffect =
          hasRandomEffect,
        isFrozen = isPkFrozen,
        isSequential = isSequential
      )
      if (numCompartments >= 2) {
        structuralParams <- addStructuralParameter(
          structuralParams,
          "B",
          hasRandomEffect =
            hasRandomEffect,
          isFrozen = isPkFrozen,
          isSequential = isSequential
        )
        structuralParams <- addStructuralParameter(
          structuralParams,
          "Beta",
          hasRandomEffect =
            hasRandomEffect,
          isFrozen = isPkFrozen,
          isSequential = isSequential
        )
      }
      if (numCompartments == 3) {
        structuralParams <- addStructuralParameter(
          structuralParams,
          "C",
          hasRandomEffect =
            hasRandomEffect,
          isFrozen = isPkFrozen,
          isSequential = isSequential
        )
        structuralParams <- addStructuralParameter(
          structuralParams,
          "Gamma",
          hasRandomEffect =
            hasRandomEffect,
          isFrozen = isPkFrozen,
          isSequential = isSequential
        )
      }
    }

    if (paramType == PARAM_CLEARANCE) {
      if (isSaturating == FALSE) {
        structuralParams <- addStructuralParameter(
          structuralParams,
          "Cl",
          hasRandomEffect =
            hasRandomEffect,
          isFrozen = isPkFrozen,
          isSequential = isSequential
        )
      } else {
        structuralParams <- addStructuralParameter(
          structuralParams,
          "Km",
          hasRandomEffect =
            hasRandomEffect,
          isFrozen = isPkFrozen,
          isSequential = isSequential
        )
        structuralParams <- addStructuralParameter(
          structuralParams,
          "Vmax",
          hasRandomEffect =
            hasRandomEffect,
          isFrozen = isPkFrozen,
          isSequential = isSequential
        )
      }
    }
    if (hasEliminationComp == TRUE &&
        isFractionExcreted == TRUE) {
      structuralParams <- addStructuralParameter(
        structuralParams,
        "Fe",
        hasRandomEffect =
          hasRandomEffect,
        isFrozen = isPkFrozen,
        isSequential = isSequential
      )
    }
    if (numCompartments >= 2) {
      for (c in 2:numCompartments) {
        if (paramType == PARAM_MICRO) {
          structuralParams <- addStructuralParameter(
            structuralParams,
            sprintf("K1%0d", c),
            hasRandomEffect =
              hasRandomEffect,
            isFrozen = isPkFrozen,
            isSequential = isSequential
          )
          structuralParams <- addStructuralParameter(
            structuralParams,
            sprintf("K%0d1", c),
            hasRandomEffect =
              hasRandomEffect,
            isFrozen = isPkFrozen,
            isSequential = isSequential
          )
        }
        if (paramType == PARAM_CLEARANCE) {
          structuralParams <- addStructuralParameter(
            structuralParams,
            sprintf("V%0d", c),
            hasRandomEffect =
              hasRandomEffect,
            isFrozen = isPkFrozen,
            isSequential = isSequential
          )
          structuralParams <- addStructuralParameter(
            structuralParams,
            sprintf("Cl%0d", c),
            hasRandomEffect =
              hasRandomEffect,
            isFrozen = isPkFrozen,
            isSequential = isSequential
          )
        }
      }
    }
    sDose <- paste0(sDose, ")")

    if (isTlag) {
      structuralParams <- addStructuralParameter(
        structuralParams,
        "Tlag",
        hasRandomEffect =
          hasRandomEffect,
        isFrozen = isPkFrozen,
        isSequential = isSequential
      )
    }

    errorModel <- attr(.Object, "errorModel")
    effectsList <- attr(errorModel, "effectsList")

    # Add standard deviation name to structural parameters

    if (length(effectsList) > 0) {
      definition <- attr(effectsList[[1]], "definition")
      errorType <- attr(effectsList[[1]], "errorType")
      if (errorType == ERR_ADD_MULT ||
          errorType == ERR_MIX_RATIO) {
        structuralParams <- addStructuralParameter(
          structuralParams,
          definition,
          hasRandomEffect =
            hasRandomEffect,
          hasCovariateEffect =
            FALSE,
          isFrozen = isPkFrozen,
          isSequential = isSequential
        )
      }
    }
    .Object@structuralParams <- structuralParams
    .Object@outputParams <- outputParams
    return(.Object)
  }
)

setGeneric(
  "generateLinearModel",
  def = function(.Object, scInput) {
    standardGeneric("generateLinearModel")
  }
)

setMethod(
  "generateLinearModel",
  "NlmePmlModel",
  definition = function(.Object, scInput) {
    statements <- attr(.Object, "statements")
    if (length(statements) == 0) {
      statements <- c(statements, "test(){")
    }
    structuralParams <- attr(.Object, "structuralParams")
    dosePoints <- attr(.Object, "dosePoints")
    outputParams <- attr(.Object, "outputParams")
    diffEquations <- attr(.Object, "diffEquations")
    linearModelType <- .Object@linearModelType

    # Add a C covariate if there is no input from a prior model
    if (length(dosePoints) == 0) {
      # FRED
      statement <- sprintf("    covariate(%s)", scInput)
      statements <- c(statements, statement)
    }
    # Add E to the output
    outputParams <- c(outputParams, "E")
    # Note: Statements have changed from Alpha, Beta, Gam, to Alpha0, Alpha1, Alpha2
    statement <- "    E = EAlpha"
    if (linearModelType >= LinearBeta) {
      statement <- paste0(statement, " + EBeta*", scInput)
    }
    if (linearModelType >= LinearGamma) {
      statement <- paste0(statement, " + EGam*", scInput, "^2")
    }
    statements <- c(statements, statement)

    .Object@statements <- statements
    .Object@dosePoints <- dosePoints
    .Object@outputParams <- outputParams
    return(.Object)
  }
)


setGeneric(
  "generateEffectsModel",
  def = function(.Object, scInput, frozen) {
    standardGeneric("generateEffectsModel")
  }
)

setMethod(
  "generateEffectsModel",
  "NlmePmlModel",
  definition = function(.Object, scInput, frozen) {
    statements <- attr(.Object, "statements")
    isPopulation <- attr(.Object, "isPopulation")
    structuralParams <- attr(.Object, "structuralParams")
    diffEquations <- attr(.Object, "diffEquations")
    if (spExists(structuralParams, "Ke0") == FALSE) {
      structuralParams <- addStructuralParameter(
        structuralParams,
        "Ke0",
        hasRandomEffect =
          isPopulation,
        hasCovariateEffect = !frozen,
        isFrozen = frozen
      )
    }
    statement <- paste0("    deriv(Ce = Ke0*(", scInput, " - Ce))")
    statements <- c(statements, statement)

    .Object@structuralParams <- structuralParams
    .Object@statements <- statements

    .Object
  }
)

setGeneric(
  "generateEmaxModel",
  def = function(.Object, scInput) {
    standardGeneric("generateEmaxModel")
  }
)

setMethod(
  "generateEmaxModel",
  "NlmePmlModel",
  definition = function(.Object, scInput) {
    statements <- attr(.Object, "statements")
    if (length(statements) == 0) {
      statements <- c(statements, "test(){")
    }
    structuralParams <- attr(.Object, "structuralParams")
    dosePoints <- attr(.Object, "dosePoints")
    outputParams <- attr(.Object, "outputParams")
    diffEquations <- attr(.Object, "diffEquations")
    emaxModelAttrs <- attr(.Object, "emaxModelAttrs")
    checkInhibitory <- attr(emaxModelAttrs, "checkInhibitory")
    checkBaseline <- attr(emaxModelAttrs, "checkBaseline")
    checkSigmoid <- attr(emaxModelAttrs, "checkSigmoid")
    checkFractional <- attr(emaxModelAttrs, "checkFractional")
    frozen <- emaxModelAttrs@frozen

    # Add a C covariate if there is no input from a prior model
    if (length(dosePoints) == 0) {
      # FRED
      statement <- sprintf("    covariate(%s)", scInput)
      statements <- c(statements, statement)
    }
    # Add E to the output
    # outputParams=c(outputParams,"E") #There is already an E, check?

    # Figure out equation for Emax model based on other params
    s0 <- "E0"
    sMax <- ""
    s50 <- ""
    if (checkInhibitory == FALSE) {
      sMax <- "Emax"
      s50 <- "EC50"
    } else {
      sMax <- "Imax"
      s50 <- "IC50"
    }

    sC <- ""
    sC50 <- ""

    if (checkSigmoid == FALSE) {
      sC <- scInput
      sC50 <- s50
    } else {
      sC <- paste0(scInput, "^Gam")
      sC50 <- paste0(s50, "^Gam")
    }

    sFrac <- paste0(sC, " / (", sC50, " + ", sC, ")")

    if (checkBaseline == FALSE) {
      if (checkInhibitory == FALSE) {
        s <- paste0(sMax, " * ", sFrac)
      } else {
        s <- paste0(s0, " * (1 - ", sFrac, ")")
      }
    } else {
      # If baseline ==TRUE
      if (checkFractional == FALSE &&
          checkInhibitory == TRUE) {
        s <- paste0(s0, " - ", sMax, " * ", sFrac)
      } else if (checkFractional == FALSE &&
                 checkInhibitory == FALSE) {
        s <- paste0(s0, " + ", "Emax * ", sFrac)
      } else if (checkFractional == TRUE &&
                 checkInhibitory == FALSE) {
        s <- paste0(s0, " * (1 + Emax * ", sFrac, ")")
      } else {
        s <- paste0(s0, " * (1 - ", sMax, " * ", sFrac, ")")
      }
    }


    # if ( checkBaseline == FALSE ) {
    #   if ( checkInhibitory == FALSE ) {
    #     s = paste0(sMax , " * ", sFrac )
    #   } else {
    #     s = paste0(s0  , " + ", sMax, " * ",sFrac  )
    #   }
    # } else  if ( checkFractional == FALSE && checkInhibitory == FALSE ) {
    #   s = paste0( s0, " + ", sMax, " * " , sFrac );
    # } else if ( checkFractional == FALSE && checkInhibitory == TRUE ) {
    #   s = paste0( s0, " - ", sMax, " * " , sFrac );
    # } else if ( checkFractional == TRUE && checkInhibitory == FALSE ) {
    #   s = paste0( s0, " * (1  +  ", sMax, " * " , sFrac ,")");
    # } else {
    #   s = paste0( s0, " * (1  -  ", sMax, " * " , sFrac ,")");
    # }


    statement <- sprintf("    E = %s", s)
    statements <- c(statements, statement)

    .Object@statements <- statements
    .Object@dosePoints <- dosePoints
    .Object@outputParams <- outputParams
    return(.Object)
  }
)


setGeneric(
  "generatePkModel",
  def = function(.Object) {
    standardGeneric("generatePkModel")
  }
)

setMethod(
  "generatePkModel",
  "NlmePmlModel",
  definition = function(.Object) {
    statements <- attr(.Object, "statements")
    if (length(statements) == 0) {
      statements <- c(statements, "test(){")
    }
    structuralParams <- attr(.Object, "structuralParams")
    dosePoints <- attr(.Object, "dosePoints")
    outputParams <- attr(.Object, "outputParams")
    #        diffEquations=attr(.Object,"diffEquations")
    diffEquations <- list()
    pkModelAttrs <- attr(.Object, "pkModelAttrs")

    parameterization <- attr(pkModelAttrs, "parameterization")
    paramType <- attr(parameterization, "paramType")
    absorption <- attr(pkModelAttrs, "absorption")
    absorpType <- attr(absorption, "absorpType")
    numCompartments <- attr(pkModelAttrs, "numCompartments")
    isTlag <- attr(pkModelAttrs, "isTlag")
    hasEliminationComp <- attr(pkModelAttrs, "hasEliminationComp")
    isFractionExcreted <- attr(pkModelAttrs, "isFractionExcreted")
    isSaturating <- attr(pkModelAttrs, "isSaturating")
    infusionAllowed <- attr(pkModelAttrs, "infusionAllowed")
    isDuration <- attr(pkModelAttrs, "isDuration")
    isSequential <- attr(pkModelAttrs, "isSequential")
    isClosedForm <- attr(pkModelAttrs, "isClosedForm")
    sDose <- ""
    sA <-
      "" # There was not an sA here before, which explains why not generated in PML
    sAa <- ""
    sA1 <- ""
    sA2 <- ""
    sA3 <- ""
    sA0 <- ""
    sFe <- "Fe"
    sA12 <- ""
    sA13 <- ""
    sA21 <- ""
    sA31 <- ""
    sAa1 <- ""
    sA10 <- ""
    sKe <- "Ke"
    sKa <- "Ka"
    sMicro <- ""
    sMacro <- ""
    sDoseStatment <- ""
    sDosepoint <- ""

    # if (iParm==parmMicro && iAbs==absExtravascular && bKaEqKe) sKe = "Ka";
    # XXXXXXXXXXX

    attr(.Object, "isTimeBased") <- TRUE

    if (paramType == PARAM_MICRO) {
      sKe <- "Ke"
    }

    if (absorpType == PARAM_INTRAVENOUS ||
        absorpType == PARAM_GAMMA ||
        absorpType == PARAM_WEIBULL ||
        absorpType == PARAM_INVERSEGAUSSIAN) {
      if (paramType == Macro1) {
        sDosepoint <- "A"
      } else {
        sDosepoint <- "A1"
      }
    } else {
      sDosepoint <- "Aa"
    }

    doseStatments <- c()

    sDoseStatment <- paste0("    dosepoint(", sDosepoint)

    sCOutput <- "C"

    # if (bPKPD && (bSequential || bFrozen)) : Do not add C to output

    outputParams <- c(outputParams, sCOutput)

    if (isTlag) {
      sDoseStatment <- paste0(sDoseStatment, ", tlag = Tlag")
    }

    if (paramType == Macro) {
      sDoseStatment <-
        paste0(sDoseStatment, ", idosevar = ", sDosepoint, "Dose")
    }

    sDoseStatment <- paste0(sDoseStatment, ")")

    dosePoints <- c(dosePoints, sDosepoint)
    doseStatments <- c(doseStatments, sDoseStatment)

    # Get absorption flow
    #        ???
    if (paramType == PARAM_MICRO &&
        absorpType == PARAM_EXTRAVASCULAR) {
      sAa1 <- paste0(sKa, " * Aa")
    } else {
      if (absorpType == PARAM_EXTRAVASCULAR) {
        sAa1 <- "Ka * Aa"
      }
    }

    #       if (iParm==parmMicro && iAbs==absExtravascular && bKaEqKe){
    #           sAa1 = sKe + " * Aa";
    #           ms.AddSParm(sKe, bFrozen, true, bSequential);
    #         } else if (iAbs==absExtravascular){
    #           sAa1 = "Ka * Aa";
    #           ms.AddSParm("Ka", bFrozen, true, bSequential);
    #         }
    # Concentration statement

    if (paramType == Macro1) {
      doseStatments <- c(doseStatments, "    C1 = A / V")
    } else if (paramType == Macro) {
      doseStatments <- doseStatments
    } else {
      doseStatments <- c(doseStatments, "    C = A1 / V")
    }

    # Is it closed form  - Micro Parameterization
    if (isClosedForm) {
      if (paramType == PARAM_MICRO) {
        sMicro <- paste0("    cfMicro(A1, ", sKe)
      }
      if (paramType == PARAM_CLEARANCE) {
        sMicro <- paste0("    cfMicro(A1,Cl/V")
      }
    }

    # Macro and Macro1 Paramterization
    if (paramType == PARAM_MACRO) {
      sMacro <-
        paste0("    cfMacro(", sDosepoint, ",C1,", sDosepoint, "Dose,A,Alpha")
    }

    if (paramType == PARAM_MACRO1) {
      sMacro <- paste0("    cfMacro1(A, Alpha")
    }

    macros <- c(", B, Beta", ", C, Gamma")
    if (numCompartments >= 2) {
      for (c in 2:numCompartments) {
        if (isClosedForm) {
          if (paramType == PARAM_MICRO) {
            sMicro <- paste0(sMicro, sprintf(", K1%0d, K%0d1", c, c))
          } else if (paramType == PARAM_CLEARANCE) {
            sMicro <- paste0(sMicro, sprintf(", Cl%0d/V, Cl%0d/V%0d", c, c, c))
          }
        } else {
          if (paramType == PARAM_CLEARANCE) {
            statements <-
              c(statements, sprintf("    C%0d = A%0d/V%0d", c, c, c))
          }
        }
        if (paramType == PARAM_MACRO ||
            paramType == PARAM_MACRO1) {
          # No distinction between macro and macro1
          sMacro <- paste0(sMacro, macros[c - 1])
        }
      }
    }
    if (sMicro != "") {
      if (absorpType == PARAM_EXTRAVASCULAR) {
        sMicro <- paste0(sMicro, ", first = (Aa = Ka)")
      }
      sMicro <- paste0(sMicro, ")")
      statements <- c(statements, sMicro)
    }

    if (sMacro != "") {
      if (paramType == PARAM_MACRO) {
        if (absorpType == PARAM_EXTRAVASCULAR) {
          sMacro <- paste0(sMacro, ",Ka")
        }
        sMacro <- paste0(sMacro, ",strip=A1Strip")
        statements <- c(statements, "    covariate(A1Strip)")
      } else {
        if (absorpType == PARAM_EXTRAVASCULAR &&
            paramType == PARAM_MACRO1) {
          sMacro <- paste0(sMacro, ",first = (Aa = Ka)")
        }
      }
      sMacro <- paste0(sMacro, ")")
      statements <- c(statements, sMacro)
    }

    for (d in doseStatments) {
      statements <- c(statements, d)
    }

    # Get elimination flow
    if (paramType == PARAM_CLEARANCE) {
      if (isSaturating) {
        sA10 <- "Vmax * C / (Km +C)"
      } else {
        sA10 <- "Cl * C"
      }
    }
    if (paramType == PARAM_MICRO) {
      sA10 <- paste0(sKe, " * A1")
    }
    # Compartment 2 elimination
    if (numCompartments >= 2) {
      if (paramType == PARAM_MICRO) {
        sA12 <- "K12 * A1"
        sA21 <- "K21 * A2"
      } else if (paramType == PARAM_CLEARANCE) {
        sA12 <- "Cl2 * (C - C2)"
      }
    }
    # Compartment 3 elimination
    if (numCompartments >= 3) {
      if (paramType == PARAM_MICRO) {
        sA13 <- "K13 * A1"
        sA31 <- "K31 * A3"
      } else if (paramType == PARAM_CLEARANCE) {
        sA13 <- "Cl3 * (C - C3)"
      }
    }

    # Get differential equation
    # Handle absorption
    if (sAa1 != "") {
      if (sAa != "") {
        sAa <- paste0(sAa, " ")
      }
      sAa <- paste(sAa, "- ", sAa1)

      if (sA1 != "") {
        sA1 <- paste0(sA1, " + ")
      }
      sA1 <- paste(sA1, sAa1)
    }
    # Handle elimination
    if (sA10 != "") {
      if (sA1 != "") {
        sA1 <- paste0(sA1, " ")
      }
      sA1 <- paste(sA1, "- ", sA10)
      if (sA0 != "") {
        sA0 <- paste0(sA0, " + ")
      }
      sA0 <- paste(sA0, sA10)
    }
    # Handle compartment 2 flow
    if (sA12 != "") {
      if (sA1 != "") {
        sA1 <- paste0(sA1, " ")
      }
      sA1 <- paste(sA1, "- ", sA12)
      if (sA2 != "") {
        sA2 <- paste0(sA2, " + ")
      }
      sA2 <- paste(sA2, sA12)
    }
    if (sA21 != "") {
      if (sA2 != "") {
        sA2 <- paste0(sA2, " ")
      }
      sA2 <- paste(sA2, "- ", sA21)
      if (sA1 != "") {
        sA1 <- paste0(sA1, " + ")
      }
      sA1 <- paste(sA1, sA21)
    }
    # Handle compartment 3 flow
    if (sA13 != "") {
      if (sA1 != "") {
        sA1 <- paste0(sA1, " ")
      }
      sA1 <- paste(sA1, "- ", sA13)
      if (sA3 != "") {
        sA3 <- paste0(sA3, " + ")
      }
      sA3 <- paste(sA3, sA13)
    }
    if (sA31 != "") {
      if (sA3 != "") {
        sA3 <- paste0(sA3, " ")
      }
      sA3 <- paste(sA3, "- ", sA31)
      if (sA1 != "") {
        sA1 <- paste0(sA1, " + ")
      }
      sA1 <- paste(sA1, sA31)
    }

    # Generate differential equations
    if (sAa != "") {
      ds <- paste0("    deriv(Aa = ", sAa, ")")
      diffEquations <- c(diffEquations, ds)
    }
    if (sA1 != "") {
      if (absorpType == PARAM_GAMMA ||
          absorpType == PARAM_WEIBULL ||
          absorpType == PARAM_INVERSEGAUSSIAN) {
        if (absorpType == PARAM_GAMMA) {
          paramName <- "ShapeParamMinusOne"
          dist <- "Gamma"
        } else if (absorpType == PARAM_WEIBULL) {
          paramName <- "ShapeParamMinusOne"
          dist <- "Weibull"
        } else if (absorpType == PARAM_INVERSEGAUSSIAN) {
          paramName <- "ShapeParam"
          dist <- "InverseGaussian"
        }
        ds <- paste0("    delayInfCpt(A1, MeanDelayTime, ",
                     paramName,
                     ", out = ",
                     sA1,
                     ", dist = ",
                     dist,
                     ")")
      } else {
        ds <- paste0("    deriv(A1 = ", sA1, ")")
      }
      diffEquations <- c(diffEquations, ds)
    }
    if (sA2 != "") {
      ds <- paste0("    deriv(A2 = ", sA2, ")")
      diffEquations <- c(diffEquations, ds)
    }
    if (sA3 != "") {
      ds <- paste0("    deriv(A3 = ", sA3, ")")
      diffEquations <- c(diffEquations, ds)
    }
    if (sA0 != "" && hasEliminationComp) {
      s <- paste0("    urinecpt(A0 = ", sA0)
      # FIX_ME            if ( bElimCptFe && sFe != "" )
      if (sFe != "" && isFractionExcreted == TRUE) {
        s <- paste0(s, ", fe=", sFe)
      }
      s <- paste0(s, ")")
      ds <- s
      outputParams <- c(outputParams, "A0")
      diffEquations <- c(diffEquations, ds)
    }
    if (isClosedForm == FALSE) {
      if (length(diffEquations) > 0) {
        for (de in diffEquations) {
          statements <- c(statements, de)
        }
      }
    }


    .Object@statements <- statements
    .Object@dosePoints <- dosePoints
    .Object@outputParams <- outputParams
    .Object@diffEquations <- diffEquations

    .Object
  }
)


#'
getContinuousEffectsString <-
  function(usageType,
           name,
           effName,
           effType,
           isPositive,
           centerValue,
           style) {
    if (isPositive == TRUE && style == LogNormal) {
      operation <- "/"
      operation2 <- "^"
    } else {
      operation <- "-"
      operation2 <- "*"
    }
    if (usageType == COVAR_EFF_PLUS_ONE) {
      operation <- "-"
      operation2 <- "*"
    }

    if (effType == COVAR_NUMBER) {
      if (centerValue != "") {
        name <- paste0("(", name, operation, centerValue, ")")
      }
      if (usageType == COVAR_EFF_YES) {
        name <- paste0("(", name, operation2, effName, ")")
      } else if (usageType == COVAR_EFF_PLUS_ONE) {
        name <- paste0("(1+", name, operation2, effName, ")")
      }
    } else if (effType == COVAR_MEDIAN) {
      name <- paste0("(", name, operation, "median(", name, "))")
      if (usageType == COVAR_EFF_YES) {
        name <- paste0("(", name, operation2, effName, ")")
      } else if (usageType == COVAR_EFF_PLUS_ONE) {
        name <- paste0("(1+", name, operation2, effName, ")")
      }
    } else if (effType == COVAR_MEAN) {
      name <- paste0("(", name, operation, "mean(", name, "))")
      if (usageType == COVAR_EFF_YES) {
        name <- paste0("(", name, operation2, effName, ")")
      } else if (usageType == COVAR_EFF_PLUS_ONE) {
        name <- paste0("(1+", name, operation2, effName, ")")
      }
    }

    name
  }


getCovariateEffNames <- function(model) {
  structuralParams <- model@structuralParams
  covariateList <- model@covariateList

  if (length(structuralParams) == 0) {
    return(c())
  }

  AllCovariateEffNames <- c()
  for (i in seq_along(structuralParams)) {
    StParmInstance <- structuralParams[[i]]

    if (StParmInstance@fixedEffName == "")
      next

    for (indx in seq_along(covariateList)) {
      CovariateEffNames <- generateCovariateNames(StParmInstance@name,
                                                  covariateList[[indx]],
                                                  StParmInstance@style)
      if (length(CovariateEffNames) == 0)
        next

      AllCovariateEffNames <-
        c(AllCovariateEffNames, unlist(CovariateEffNames))
    }
  }

  AllCovariateEffNames
}

#'
getCovariateEffDirection <- function(model) {
  direction <- c()
  curEnable <- 0
  structuralParams <- model@structuralParams
  covariateList <- model@covariateList
  if (length(structuralParams) > 0) {
    for (i in 1:length(structuralParams)) {
      stp <- structuralParams[[i]]
      name <- stp@name
      style <- stp@style
      fixedEffName <- stp@fixedEffName
      if (fixedEffName != "") {
        if (length(covariateList) > 0) {
          for (indx in 1:length(covariateList)) {
            strArray <- generateCovariateNames(name,
                                               covariateList[[indx]],
                                               style)

            if (length(strArray) != 0) {
              direction <- c(direction, rep(curEnable, length(strArray)))
              curEnable <- curEnable + 1
            }
          }
        }
      }
    }
  }

  direction
}

#'
getCategoryEffectString <-
  function(usageType, stpName, covarName, indx) {
    effectName <- paste0("d", stpName, "d", covarName, indx)
    if (usageType == COVAR_EFF_YES) {
      CategoryEffectString <-
        paste0(effectName, "*(", covarName, "==", indx, ")")
      ##        name=paste0(paste0("exp(",effectName,"*(",covarName,"==",indx,"))"))
    } else if (usageType == COVAR_EFF_PLUS_ONE) {
      CategoryEffectString <-
        paste0("(1+", effectName, "*(", covarName, "==", indx, "))")
    } else {
      stop("Invalid usageType")
    }
    #    return(paste0(effectName,"*(",covarName,"==",indx,")"))
    return(CategoryEffectString)
  }


getOccasionEffectString <-
  function(usageType, stpName, covarName, indx) {
    effectName <- paste0("n", stpName, "x", covarName, indx)
    if (usageType == COVAR_EFF_YES) {
      OccasionEffectString <-
        paste0(effectName, "*(", covarName, "==", indx, ")")
    } else if (usageType == COVAR_EFF_PLUS_ONE) {
      OccasionEffectString <-
        paste0("(1+", effectName, "*(", covarName, "==", indx, "))")
    } else {
      stop("Invalid usageType")
    }

    return(OccasionEffectString)
  }

generateCovariateEffects <-
  function(stpName, CovariateInstance, style) {
    covarEffList <- CovariateInstance@covarEffList
    usageType <- covarEffList[[stpName]]

    ret <- c()
    if (length(usageType) == 0 || usageType == COVAR_EFF_NO) {
      return(ret)
    }

    covarName <- CovariateInstance@name
    type <- CovariateInstance@type
    centerValue <- CovariateInstance@centerValue
    effType <- CovariateInstance@continuousType
    isPositive <- CovariateInstance@isPositive
    effName <- paste0("d", stpName, "d", covarName)
    if (type == COVAR_CONTINUOUS) {
      ContinuousEffectsString <- getContinuousEffectsString(usageType,
                                                            covarName,
                                                            effName,
                                                            effType,
                                                            isPositive,
                                                            centerValue,
                                                            style)
      return(ContinuousEffectsString)
    }

    items <- CovariateInstance@covarItems

    for (i in seq_along(items)) {
      if (type == Category && i == 1)
        next # reference category

      item <- items[[i]]
      val <- item@value
      if (type == Category) {
        EffectString <-
          getCategoryEffectString(usageType, stpName, covarName, val)
      } else if (type == COVAR_OCCASION) {
        EffectString <-
          getOccasionEffectString(usageType, stpName, covarName, val)
      }

      ret <- c(ret, EffectString)
    }

    ret
  }

generateCovariateNames <-
  function(stpName, CovariateInstance, style) {
    covarEffList <- CovariateInstance@covarEffList
    usageType <- covarEffList[[stpName]]

    if (length(usageType) == 0 || usageType == COVAR_EFF_NO) {
      return(c())
    }

    ret <- c()
    covarName <- CovariateInstance@name
    type <- CovariateInstance@type
    effName <- paste0("d", stpName, "d", covarName)

    if (type == COVAR_CONTINUOUS) {
      ret <- effName
    } else if (type == Category) {
      items <- CovariateInstance@covarItems
      for (i in 2:length(items)) {
        ret <-
          c(ret, paste0("d", stpName, "d", covarName, items[[i]]@value))
      }
    }

    ret
  }


generateCovariateStatement <- function(.Object) {
  covariates <- .Object@covariateList

  if (length(covariates) == 0) {
    return(.Object)
  }

  statements <- .Object@statements
  for (i in seq_along(covariates)) {
    covar <- covariates[[i]]
    name <- covar@name
    type <- covar@type
    direction <- covar@direction

    if (direction == COVAR_BACKWARD) {
      statement <- paste0("    covariate(", name)
    } else if (direction == COVAR_FORWARD) {
      statement <- paste0("    fcovariate(", name)
    } else if (direction == COVAR_INTERPOLATE) {
      statement <- paste0("    interpolate(", name)
    }

    if (type == COVAR_CATEGORY) {
      statement <- paste0(statement, "()")
    }

    statement <- paste0(statement, ")")
    statements <- c(statements, statement)
  }

  .Object@statements <- statements

  .Object
}


generateStparmStatement <- function(.Object) {
  statements <- .Object@statements
  structuralParams <- .Object@structuralParams
  covariateList <- .Object@covariateList
  # sort the covariates by type: Continuous, Category, Occasion
  # so the occasion is grouped within random effect
  CovariateTypes <- sapply(covariateList, function(x)
    x@type)
  covariateList <- covariateList[order(CovariateTypes)]

  if (length(structuralParams) == 0) {
    return(.Object)
  }

  for (i in seq_along(structuralParams)) {
    stp <- structuralParams[[i]]
    StParmName <- stp@name
    fixedEffName <- stp@fixedEffName
    randomEffName <- stp@randomEffName
    hasRandomEffect <- stp@hasRandomEffect
    hasCovariateEffect <- stp@hasCovariateEffect
    style <- stp@style
    closeParanthesis <- FALSE
    closeOuterParanthesis <- FALSE
    if (fixedEffName != "") {
      if (style == LogNormal) {
        statement <- paste0(StParmName, " = ", fixedEffName)
      } else if (style == Normal) {
        statement <- paste0(StParmName, " = ", fixedEffName)
      } else if (style == Combination) {
        statement <- paste0(StParmName, " = (", fixedEffName)
        closeParanthesis <- TRUE
      } else if (style == Log) {
        statement <- paste0(StParmName, " = exp(", fixedEffName)
        closeOuterParanthesis <- TRUE
      } else if (style == Logit) {
        statement <- paste0(StParmName, " = ilogit(", fixedEffName)
        closeOuterParanthesis <- TRUE
      } else {
        stop("unknown structural parameter style")
      }
    }

    # Generate covariate effects
    randomEffectUsed <- FALSE
    # JC Removed if ( stp@isFrozen == FALSE && length(covariateList) > 0 )
    # Replaced with
    for (indx in seq_along(covariateList)) {
      CovariateEffects <-
        generateCovariateEffects(StParmName, covariateList[[indx]], style)
      if (length(CovariateEffects) == 0)
        next
      covarEffList <- covariateList[[indx]]@covarEffList
      usageType <- covarEffList[[StParmName]]
      type <- covariateList[[indx]]@type
      if (type == Occasion && !randomEffectUsed) {
        randomEffectUsed <- TRUE

        if (style == Combination) {
          # add `)` at the end of the sum before random block
          statement <-
            paste0(statement,
                   ")")
          closeParanthesis <- FALSE
        }

        if (style %in% c(LogNormal, Combination)) {
          statement <-
            paste0(statement,
                   " * exp(",
                   ifelse(hasRandomEffect, randomEffName, ""))
        } else {
          statement <-
            paste0(statement,
                   " + ",
                   ifelse(hasRandomEffect, randomEffName, ""))
        }
      }

      CovariateEffects <- CovariateEffects[CovariateEffects != ""]
      for (CovariateEffect in CovariateEffects) {
        if (type == Occasion) {
          # `+` is required
          # 1. after main ranefname
          # 2. after first occ eff
          # 3. if `+` is not already there (after `)` and alphanumeric)
          PlusRequired <-
            hasRandomEffect ||
            CovariateEffect != CovariateEffects[[1]] ||
            !grepl("[^)a-zA-Z0-9\\s]\\s*$", statement)
          statement <-
            paste0(statement,
                   ifelse(PlusRequired,
                          " + ",
                          ""),
                   CovariateEffect)
          next
        }

        if (style == LogNormal) {
          if (usageType == COVAR_EFF_PLUS_ONE) {
            statement <- paste0(statement, " * ( ", CovariateEffect, ") ")
            next
          }

          # usageType != COVAR_EFF_PLUS_ONE
          if (type == Category) {
            statement <- paste0(statement, " * exp(", CovariateEffect, ")")
          } else {
            statement <- paste0(statement, " * ", CovariateEffect, "  ")
          }

        } else if (style == Normal) {
          statement <- paste0(statement, " + ", CovariateEffect)
        } else if (style == Combination) {
          statement <- paste0(statement, " + ", CovariateEffect)
        } else if (style == Log) {
          statement <- paste0(statement, " + ", CovariateEffect)
        } else if (style == Logit) {
          statement <- paste0(statement, " + ", CovariateEffect)
        } else {
          stop("unknown structural parameter style")
        }
      }
    }

    if (closeParanthesis && style == Combination) {
      # need to close parent of sum if not done yet
      statement <- paste0(statement, " )")
      closeParanthesis <- FALSE
    }

    # Generate random effect if there's no occasion
    ranEffName <- stp@randomEffName
    if (hasRandomEffect  && !randomEffectUsed && ranEffName != "") {
      style <- stp@style
      if (style == LogNormal) {
        statement <- paste0(statement, " * exp(", ranEffName, ")")
      } else if (style == Normal) {
        statement <- paste0(statement, " + ", ranEffName)
      } else if (style == Combination) {
        statement <- paste0(statement, "  * exp(", ranEffName, ")")
      } else if (style == Log) {
        statement <- paste0(statement, " + ", ranEffName, ")")
      } else if (style == Logit) {
        statement <- paste0(statement, " + ", ranEffName, ")")
      }

    }

    matches_open <- gregexpr("\\(", statement, fixed = FALSE)
    matches_close <- gregexpr("\\)", statement, fixed = FALSE)

    # Count: if the first element is -1, no matches were found. Otherwise, count matches.
    if (matches_open[[1]][1] == -1) {
      open_count <- 0
    } else {
      open_count <- length(matches_open[[1]])
    }

    if (matches_close[[1]][1] == -1) {
      close_count <- 0
    } else {
      close_count <- length(matches_close[[1]])
    }

    if (open_count > close_count) {
      # If the number of open and close parentheses is not equal, add a closing parenthesis
      statement <-
        paste0(statement, rep(")", open_count - close_count))
    } else if (open_count < close_count) {
      warning("Current stparm() is not correct:\n",
              statement)
    }

    statement <- paste0("    stparm(", statement, ")")
    structuralParams[[i]]@code <- statement
    statements <- c(statements, statement)

  }

  .Object@structuralParams <- structuralParams
  .Object@statements <- statements
  .Object

}

generateStparmExtraCode <- function(.Object) {
  structuralParams <- .Object@structuralParams
  for (Stparm in structuralParams) {
    for (extraCode in Stparm@extraCode) {
      .Object@statements <- c(.Object@statements, extraCode)
    }
  }

  .Object
}


actionString <- function(dobefore, doafter) {
  ret <- ""
  if (dobefore != "") {
    ret <- paste0(", dobefore = ", dobefore)
  }
  if (doafter != "") {
    ret <- paste0(ret, ", doafter = ", doafter)
  }

  ret
}

generateObserveErrorStatements <- function(effectsList) {
  statements <- c()
  if (length(effectsList) > 0) {
    for (i in 1:length(effectsList)) {
      effect <- effectsList[[i]]
      effectName <- attr(effect, "effectName")
      observeName <- attr(effect, "observeName")
      epsilonName <- attr(effect, "epsilonName")
      errorType <- attr(effect, "errorType")
      frozen <- attr(effect, "frozen")
      SD <- attr(effect, "SD")
      definition <- attr(effect, "definition")
      isBQL <- attr(effect, "isBQL")
      bqlStaticValue <- attr(effect, "bqlStaticValue")
      dobefore <- attr(effect, "dobefore")
      doafter <- attr(effect, "doafter")
      if (isBQL == TRUE) {
        bql <- ", bql"
        if (bqlStaticValue != "") {
          bql <- paste0(bql, "=", bqlStaticValue)
        }
      } else {
        bql <- ""
      }
      if (frozen == TRUE) {
        freeze <- "(freeze)"
      } else {
        freeze <- ""
      }


      observe <- ""
      error <-
        paste0("    error(", epsilonName, freeze, "=", SD, ")")
      bqlStaticValue <- attr(effect, "bqlStaticValue")

      if (errorType == ERR_ADDITIVE) {
        observe <- paste0(
          "    observe(",
          observeName,
          "=",
          effectName,
          " + ",
          epsilonName,
          bql,
          actionString(dobefore, doafter),
          ")"
        )
      }
      if (errorType == ERR_LOG_ADDITIVE) {
        observe <- paste0(
          "    observe(",
          observeName,
          "=",
          effectName,
          " * exp( ",
          epsilonName,
          ")",
          bql,
          actionString(dobefore, doafter),
          ")"
        )
      }
      if (errorType == ERR_MULTIPLICATIVE) {
        observe <- paste0(
          "    observe(",
          observeName,
          "=",
          effectName,
          " * ( 1 + ",
          epsilonName,
          ")",
          bql,
          actionString(dobefore, doafter),
          ")"
        )
      }
      if (errorType == ERR_ADD_MULT) {
        if (effectName == "C1") {
          definition <- "C1MultStdev"
        } else if (effectName == "C") {
          definition <- "CMultStdev"
        } else if (effectName == "E") {
          definition <- "EMultStdev"
        } else {
          definition <- definition
        }
        observe <- paste0(
          "    observe(",
          observeName,
          "=",
          effectName,
          " + ",
          epsilonName,
          " * sqrt(1 + ",
          effectName,
          "^2 * (",
          definition,
          "/sigma())^2)",
          bql,
          actionString(dobefore, doafter),
          ")"
        )
      }



      if (errorType == ERR_POWER) {
        observe <- paste0(
          "    observe(",
          observeName,
          "=",
          effectName,
          " + ",
          effectName,
          " ^(",
          definition,
          ") * ",
          epsilonName,
          bql,
          actionString(dobefore, doafter),
          ")"
        )
      }
      if (errorType == ERR_MIX_RATIO) {
        if (definition == "") {
          definition <- paste0(effectName, "MixRatio")
        }
        observe <- paste0(
          "    observe(",
          observeName,
          "=",
          effectName,
          " + ",
          epsilonName,
          " * (1 + ",
          effectName,
          " * ",
          definition,
          " )",
          bql,
          actionString(dobefore, doafter),
          ")"
        )
      }
      if (errorType == ERR_CUSTOM) {
        observe <- paste0(
          "    observe(",
          observeName,
          "=",
          definition,
          bql,
          actionString(dobefore, doafter),
          ")"
        )
      }

      statements <- c(statements, error)
      statements <- c(statements, observe)
    }
  }
  statements
}

setGeneric(
  name = "generateErrorStatment",
  def = function(.Object) {
    standardGeneric("generateErrorStatment")
  }
)

setMethod(
  "generateErrorStatment",
  "NlmePmlModel",
  definition = function(.Object) {
    statements <- attr(.Object, "statements")
    errorModel <- attr(.Object, "errorModel")
    effectsList <- attr(errorModel, "effectsList")
    errStatments <- generateObserveErrorStatements(effectsList)
    # Special Error formatting for emaxmodel to create dependency of EObs on C i.e. EObs(C)
    if (.Object@modelType@modelType == PARAM_EMAX ||
        .Object@modelType@modelType == PARAM_LINEAR) {
      errStatments <-
        gsub(pattern = "EObs",
             replacement = "EObs(C)",
             x = errStatments)
    }

    .Object@statements <- c(statements, errStatments)
    return(.Object)
  }
)


generateEffectsVariables <- function(.Object) {
  CovariateEffNames <- getCovariateEffNames(.Object)
  params <-
    vector(mode = "list", length = length(CovariateEffNames))

  if (length(CovariateEffNames) == 0) {
    .Object@effectsParams <- params
    return(.Object)
  }

  for (i in seq_along(CovariateEffNames)) {
    fixedEffName <- CovariateEffNames[[i]]
    hasRandomEffect <- FALSE
    style <- STP_PRODUCT
    initialValue <- "0"
    lowerBound <- ""
    upperBound <- ""
    units <- ""
    isFrozen <- FALSE
    isSequential <- FALSE

    params[[i]] <- NlmeStructuralParameter(
      name = CovariateEffNames[[i]],
      fixedEffName = CovariateEffNames[[i]],
      hasRandomEffect = hasRandomEffect,
      style = style,
      initialValue = as.character(initialValue),
      lowerBound = as.character(lowerBound),
      upperBound = as.character(upperBound),
      units = as.character(units),
      isFrozen = isFrozen,
      isSequential = isSequential
    )
  }

  oldEffects <- .Object@effectsParams
  for (i in seq_along(params)) {
    for (j in seq_along(oldEffects)) {
      if (params[[i]]@name == oldEffects[[j]]@name) {
        params[[i]] <- oldEffects[[j]]
      }
    }
  }

  .Object@effectsParams <- params

  .Object
}

#' Lists covariate effect names in the model
#'
#' This function lists the names of covariate effects in a provided
#' pharmacokinetic/pharmacodynamic (PK/PD) model.
#'
#' @param .Object   PK/PD model
#'
#' @examples
#' \donttest{
#' model <- pkmodel(
#'   numCompartments = 2,
#'   data = pkData,
#'   ID = "Subject",
#'   Time = "Act_Time",
#'   A1 = "Amount",
#'   CObs = "Conc",
#'   workingDir = tempdir()
#' )
#' model <- addCovariate(model,
#'   covariate = "Gender",
#'   type = "Categorical",
#'   effect = c("V2", "Cl2"),
#'   levels = c(0, 1),
#'   labels = c("Female", "Male")
#' )
#' listCovariateEffectNames(model)
#' }
#' @return A vector of character strings containing the names
#' of the covariate effects in the model.
#' @name listCovariateEffectNames
#' @rdname listCovariateEffectNames
#' @export
setGeneric(
  name = "listCovariateEffectNames",
  def = function(.Object) {
    standardGeneric("listCovariateEffectNames")
  }
)

#' @rdname listCovariateEffectNames
#' @aliases listCovariateEffectNames,NlmePmlModel-method
setMethod(
  "listCovariateEffectNames",
  "NlmePmlModel",
  definition = function(.Object) {
    if (!.Object@isTextual) {
      CovariateEffectNames <-
        sapply(.Object@effectsParams, function(x) {
          x@name
        })
    } else {
      fixefStrings <- .get_fixefStrings(.Object)
      CovariateEffectNames <-
        sapply(fixefStrings, function(fixef) {
          if (fixef[2] >= 0) {
            fixef[1]
          } else {
            NA
          }
        })
      CovariateEffectNames <-
        c(na.omit(CovariateEffectNames))
    }

    CovariateEffectNames
  }
)

generateFixedEffStatment <- function(.Object) {
  statements <- .Object@statements
  structuralParams <- .Object@structuralParams
  effectsParams <- .Object@effectsParams
  frozenList <- c()
  if (length(structuralParams) > 0) {
    for (i in 1:length(structuralParams)) {
      stp <- structuralParams[[i]]
      name <- stp@name
      fixedEffName <- stp@fixedEffName
      randomEffName <- stp@randomEffName
      initialValue <- stp@initialValue
      lowerBound <- stp@lowerBound
      upperBound <- stp@upperBound
      isFrozen <- stp@isFrozen
      isSequential <- stp@isSequential
      if (fixedEffName != "") {
        if (isFrozen || isSequential) {
          freezeStatement <- "(freeze)"
          frozenList <- c(frozenList, randomEffName)
        } else {
          freezeStatement <- ""
        }
        statement <- paste0(
          "    fixef( ",
          fixedEffName,
          freezeStatement,
          " = c(",
          lowerBound,
          ",",
          initialValue,
          ",",
          upperBound,
          "))"
        )
      }
      statements <- c(statements, statement)
    }
  }
  directions <- getCovariateEffDirection(.Object)
  if (length(effectsParams) > 0) {
    for (i in 1:length(effectsParams)) {
      stp <- effectsParams[[i]]
      name <- stp@name
      fixedEffName <- stp@fixedEffName
      initialValue <- stp@initialValue
      lowerBound <- stp@lowerBound
      upperBound <- stp@upperBound
      isFrozen <- stp@isFrozen
      isSequential <- stp@isSequential

      if (fixedEffName != "") {
        if (isFrozen || isSequential) {
          statement <-
            paste0(
              "    fixef( ",
              fixedEffName,
              "(freeze) = c(",
              lowerBound,
              ",",
              initialValue,
              ",",
              upperBound,
              "))"
            )
        } else {
          statement <-
            paste0(
              "    fixef( ",
              fixedEffName,
              "(enable=c(",
              directions[i],
              ")) = c(",
              lowerBound,
              ",",
              initialValue,
              ",",
              upperBound,
              "))"
            )
        }
      }
      statements <- c(statements, statement)
    }
  }

  .Object@statements <- statements

  .Object
}

#'
generateSecondaryStatement <- function(.Object) {
  secondaryParameters <- .Object@secondaryParameters
  for (secondaryParameter in secondaryParameters) {
    .Object@statements <- c(
      .Object@statements,
      paste0(
        "    secondary(",
        secondaryParameter@name,
        "=",
        secondaryParameter@definition,
        ")"
      )
    )
  }

  .Object
}

generateRanEffStatment <- function(.Object) {
  statements <- .Object@statements
  structuralParams <- .Object@structuralParams

  randomEffectsStatements <- .Object@randomEffectsStatements
  covariateStatement <- ""

  if (!.Object@randomValuesInitialized) {
    .Object <- initializeRandomEffectsBlock(.Object)
  }

  if (length(randomEffectsStatements) > 0) {
    statements <- c(statements, randomEffectsStatements)
  } else if (length(structuralParams) > 0) {
    first <- TRUE
    firstCovariate <- TRUE
    effectStatement <- ""
    effectInitialValues <- ""
    for (i in seq_along(structuralParams)) {
      stp <- structuralParams[[i]]
      name <- stp@name
      randomEffName <- stp@randomEffName
      ranEffInitValue <- stp@ranEffInitValue
      hasRandomEffect <- stp@hasRandomEffect
      isSequential <- stp@isSequential
      # if ( randomEffName != "" && hasRandomEffect &&(isFrozen == FALSE) && ( isSequential == FALSE ) ){
      if (randomEffName != "" &&
          hasRandomEffect && !isSequential) {
        if (first) {
          effectStatement <- "    ranef(diag("
          effectInitialValues <- " c("
          first <- FALSE
        } else {
          effectStatement <- paste0(effectStatement, ",")
          effectInitialValues <-
            paste0(effectInitialValues, ",")
        }

        effectStatement <-
          paste0(effectStatement, randomEffName)
        effectInitialValues <-
          paste0(effectInitialValues, ranEffInitValue)
        #                    effectInitialValues=paste0(effectInitialValues,"1")
      }
      # if ( randomEffName != "" && hasRandomEffect &&(isFrozen == FALSE) && ( isSequential == TRUE ) ){
      if (randomEffName != "" &&
          hasRandomEffect && isSequential) {
        if (firstCovariate) {
          covariateStatement <- randomEffName
          firstCovariate <- FALSE
        } else {
          covariateStatement <- paste0(covariateStatement, ",", randomEffName)
        }
      }
    }

    if (covariateStatement != "") {
      covariateStatement <-
        paste0("    covariate(", covariateStatement, ")")
      statements <- c(statements, covariateStatement)
    }

    if (effectStatement != "") {
      # if ( effectStatement != "" && hasRandomEffect) {
      statement <-
        paste0(effectStatement, ") = ", effectInitialValues, "))")
      statements <- c(statements, statement)
    }
  } # closes control flow from above problematic TRUE statement

  # Generate occasion covariate effect's random effect
  if (length(.Object@randomOccasionalEffectsStatements) > 0) {
    statements <-
      c(statements, .Object@randomOccasionalEffectsStatements)
  } else {
    covariates <- .Object@covariateList
    for (CovariateInstance in covariates) {
      if (CovariateInstance@type != Occasion)
        next

      RanefNames <- c()
      items <- CovariateInstance@covarItems
      effects <- CovariateInstance@covarEffList
      isEnabled <-
        effects == 1 # Added to generate ran eff statement for only effects that are enabled
      effects <- effects[isEnabled]
      if (length(effects) == 0)
        next

      values <- CovariateInstance@catEffInitValues[isEnabled]
      AffectedStParmNames <- names(effects)

      for (indx in seq_along(AffectedStParmNames)) {
        RanefNames <- c(
          RanefNames,
          paste0(
            "n",
            AffectedStParmNames[[indx]],
            "x",
            CovariateInstance@name,
            items[[1]]@value
          )
        )
      }

      if (length(RanefNames) == length(values)) {
        statement <- paste0(
          "    ranef(diag(",
          paste(as.character(RanefNames), collapse = ","),
          ") = c(",
          paste(as.character(values), collapse = ","),
          ")"
        )
      } else {
        statement <- paste0(
          "    ranef(block(",
          paste(as.character(RanefNames), collapse = ","),
          ") = c(",
          paste(as.character(values), collapse = ","),
          ")"
        )
      }

      if (length(items) > 1) {
        for (i in 2:length(items)) {
          RanefNames <- c()
          for (indx in seq_along(AffectedStParmNames)) {
            RanefNames <- c(
              RanefNames,
              paste0(
                "n",
                AffectedStParmNames[[indx]],
                "x",
                CovariateInstance@name,
                items[[i]]@value
              )
            )
          }

          statement <- paste0(statement,
                              ", same(",
                              paste(as.character(RanefNames), collapse = ","),
                              ")")
        }
      }

      statement <- paste0(statement, ")")
      statements <- c(statements, statement)
    }
  }

  .Object@statements <- statements
  return(.Object)
}


#' Generates PML statements based on the current model
#'
#' Generates PML statements based on the current model
#'
#' @param .Object PK/PD model
#' @keywords internal
#' @noRd
setGeneric(
  name = "generatePMLModel",
  def = function(.Object) {
    standardGeneric("generatePMLModel")
  }
)

setMethod(
  "generatePMLModel",
  "NlmePmlModel",
  definition = function(.Object) {
    modelStatements(.Object) <- list()
    modelType <- .Object@modelType@modelType
    paramType <- .Object@pkModelAttrs@parameterization@paramType
    frozen <- FALSE
    .Object@dosePoints <- list()
    pdInput <- "C"
    if (paramType == Macro || paramType == Macro1) {
      if (modelType == PARAM_PK_INDIRECT ||
          modelType == PARAM_PK_EMAX ||
          modelType == PARAM_PK_LINEAR) {
        pdInput <- "C1"
      }
    }

    if (.Object@hasEffectsCompartment) {
      pdInput <- "Ce"
    }
    if (modelType == PARAM_PK ||
        modelType == PARAM_PK_EMAX ||
        modelType == PARAM_PK_INDIRECT ||
        modelType == PARAM_PK_LINEAR) {
      .Object <- generatePkModel(.Object)
    }
    if (modelType == PARAM_EMAX ||
        modelType == PARAM_PK_EMAX) {
      .Object <- generateEmaxModel(.Object, pdInput)
      frozen <- .Object@emaxModelAttrs@frozen
    }
    if (modelType == PARAM_LINEAR ||
        modelType == PARAM_PK_LINEAR) {
      .Object <- generateLinearModel(.Object, pdInput)
      frozen <- .Object@isLinearFrozen
    }
    if (modelType == PkIndirect) {
      .Object <- generateIndirectModel(.Object, pdInput)
      frozen <- .Object@indirectModelAttrs@frozen
    }
    if (.Object@hasEffectsCompartment) {
      if (paramType == Micro || paramType == Clearance) {
        scInput <- "C"
      } else {
        scInput <- "C1"
      }
      .Object <- generateEffectsModel(.Object, scInput, frozen)
    }

    # note that it does not generate occasion effects:
    .Object <- generateEffectsVariables(.Object)

    .Object <- generateErrorStatment(.Object)
    .Object <- generateStparmStatement(.Object)
    .Object <- generateCovariateStatement(.Object)
    .Object <- generateStparmExtraCode(.Object)
    .Object <- generateFixedEffStatment(.Object)
    .Object <- generateRanEffStatment(.Object)
    .Object <- generateSecondaryStatement(.Object)
    statements <- .Object@statements
    if (length(statements) == 0) {
      statements <- c(statements, "test(){")
    }
    statements <- c(statements, "}")
    .Object@statements <- statements

    .Object
  }
)

generateIndirectModel <- function(.Object, scInput) {
  statements <- .Object@statements
  if (length(statements) == 0) {
    statements <- c(statements, "test(){")
  }

  structuralParams <- attr(.Object, "structuralParams")
  dosePoints <- attr(.Object, "dosePoints")
  outputParams <- attr(.Object, "outputParams")
  diffEquations <- attr(.Object, "diffEquations")
  attrs <- attr(.Object, "indirectModelAttrs")

  type <- attrs@type
  hasEffectsCompartment <- attrs@hasEffectsCompartment
  isBuildup <- attrs@isBuildup
  isExponent <- attrs@isExponent
  frozen <- attrs@frozen


  sC <- scInput
  sMax <- ""
  s50 <- ""

  if (type == LIMITED_STIM) {
    sMax <- "Emax"
    s50 <- "EC50"
    if (isExponent) {
      s50 <- paste0(s50, " ^gam ")
      sC <- paste0(sC, " ^gam ")
    }
    sFrac <- paste0(sC, " / (", sC, " + ", s50, ")")
    statement <- paste0(sMax, " * ", sFrac)
    statement <- paste0("(1 + ", statement, ")")
  }
  if (type == INFINITE_STIM) {
    sMax <- ""
    s50 <- "EC50"
    sFrac <- paste0("(", sC, " / ", s50, ")")
    if (isExponent) {
      sFrac <- paste0(sFrac, " ^ gam")
    }
    statement <- sFrac
    statement <- paste0("(1 + ", statement, ")")
  }
  if (type == LIMITED_INHIB) {
    sMax <- "Imax"
    s50 <- "IC50"
    if (isExponent) {
      s50 <- paste0(s50, " ^ gam")
      sC <- paste0(sC, " ^ gam")
    }
    sFrac <- paste0(sC, " / (", sC, " + ", s50, ")")
    statement <- paste0(sMax, " * ", sFrac)
    statement <- paste0("(1 - ", statement, ")")
  }
  if (type == INVERSE_INHIB) {
    sMax <- "Imax"
    s50 <- "IC50"
    if (isExponent) {
      s50 <- paste0(s50, " ^ gam")
      sC <- paste0(sC, " ^ gam")
    }
    sFrac <- paste0(sC, " / (", sC, " + ", s50, ")")
    statement <- paste0(sMax, " * ", sFrac)
    statement <- paste0("1 / (1 + ", statement, ")")
  }
  if (type == LINEAR_STIM) {
    sS <- "s"
    if (isExponent) {
      sC <- paste0(sC, " ^ gam")
    }
    statement <- paste0("(1 +  ", sS, " * ", sC, ")")
  }
  if (type == LOG_LINEAR_STIM) {
    sS <- "s"
    if (isExponent) {
      sC <- paste0(sC, " ^ gam")
    }
    statement <- paste0("(1 + log(1 + ", sS, " * ", sC, "))")
  }
  if (isBuildup == TRUE) {
    sIn <- paste0("Kin * ", statement)
    sOut <- "Kout"
  } else {
    sIn <- "Kin"
    sOut <- paste0("Kout * ", statement)
  }
  deriv <- paste0("    deriv(E = ", sIn, " - ", sOut, " * E)")
  diffEquations <- c(diffEquations, deriv)
  statements <- c(statements, deriv)
  statement <- "    sequence{ E= Kin / Kout}"
  statements <- c(statements, statement)

  .Object@statements <- statements
  .Object@diffEquations <- diffEquations
  .Object@outputParams <- outputParams
  .Object
}


# adds covariate instance to the model
# for this covariate, the function will
# concatenate the existed covEffList with all stparms associated with covariates
associateCovarsWithParams <- function(model, CovClassInstance) {
  if (length(CovClassInstance) == 0) {
    return(model)
  }

  covEffList <- CovClassInstance@covarEffList
  covariateType <- CovClassInstance@type
  StParmNames <- c()
  for (StParm in model@structuralParams) {
    # not clear why if the structural parameter is frozen, it should not be associated with a covariate
    if (StParm@hasCovariateEffect == TRUE &&
        StParm@isFrozen == FALSE) {
      StParmNames <- c(StParmNames, StParm@name)
      if (covariateType == COVAR_CONTINUOUS) {
        covEffList <- c(covEffList, name = COVAR_EFF_PLUS_ONE)
      } else {
        covEffList <- c(covEffList, name = COVAR_EFF_YES)
      }
    }
  }

  names(covEffList) <- StParmNames
  CovClassInstance@covarEffList <- covEffList

  model@covariateList <- c(model@covariateList, CovClassInstance)

  model
}

addCovariates <- function(model, NewCovariateInstance, effects) {
  # add covariates to the model (covariateList)
  # update covEffList for each covariate
  model <- associateCovarsWithParams(model, NewCovariateInstance)
  if (length(effects) == 0) {
    # model <- generatePMLModel(model)
    return(model)
  }

  # effects is a vector of covariates
  # names(effects) is a vector of affected structural parameters
  for (AffectedStParmName in names(effects)) {
    CovName <- effects[AffectedStParmName]
    if (NewCovariateInstance@name != CovName)
      next

    if (NewCovariateInstance@type == Continuous &&
        !NewCovariateInstance@isPositive) {
      covariateEffect(model, CovName, AffectedStParmName) <-
        COVAR_EFF_PLUS_ONE
    } else {
      covariateEffect(model, CovName, AffectedStParmName) <- COVAR_EFF_YES
    }
  }

  covariateList <- model@covariateList
  for (CovariateInstanceIndex in seq_along(covariateList)) {
    CovariateInstance <- covariateList[[CovariateInstanceIndex]]

    if (CovariateInstance@type != Occasion)
      next

    num <- length(CovariateInstance@covarEffList)
    if (length(CovariateInstance@catEffInitValues) == 0) {
      CovariateInstance@catEffInitValues <- as.list(rep(1, num))
    }

    covariateList[[CovariateInstanceIndex]] <- CovariateInstance
  }

  model@covariateList <- covariateList

  #generatePMLModel(model)
  model
}


.get_fixefStrings <- function(model, initialcf) {
  if (missing(initialcf)) {
    initialcf <- createModelInfo(model, ForceRun = TRUE)
  }

  fixefLine <-
    initialcf[grepl("^\\(fixedeffects ", initialcf)]
  fixefStrings <-
    unlist(strsplit(fixefLine, split = "(\\(fixedeffects\\W*\\()|(\\)\\W*\\()|(\\)\\))"))
  fixefStrings <- fixefStrings[fixefStrings != ""]
  splittedFixefsStrings <-
    strsplit(fixefStrings, split = " ")
  splittedFixefsStrings
}

#' Display/Set initial estimates for fixed effects
#'
#' @param .Object    PK/PD model
#' @return Named numeric vector of fixed effects estimates
#' @examples
#' model <- pkmodel(
#'   numCompartments = 2,
#'   data = pkData,
#'   ID = "Subject",
#'   Time = "Act_Time",
#'   A1 = "Amount",
#'   CObs = "Conc",
#'   modelName = "TwCpt_IVBolus_FOCE_ELS",
#'   workingDir = tempdir()
#'   )
#'
#' # View initial/current fixed effect values
#'  initFixedEffects(model)
#'
#' # May also use as a 'replacement function' to set the values
#' initFixedEffects(model) <- c(tvV = 15, tvCl = 5, tvV2 = 40, tvCl2 = 15)
#'
#' @seealso \code{\link{fixedEffect}}
#' @export
#' @rdname initFixedEffects
setGeneric(
  name = "initFixedEffects",
  def = function(.Object) {
    standardGeneric("initFixedEffects")
  }
)

#' @export
#' @rdname initFixedEffects
setMethod(
  "initFixedEffects",
  "NlmePmlModel",
  definition = function(.Object) {
    if (.Object@isTextual) {
      estimates <- getThetas(.Object)
    } else {
      structuralParams <- .Object@structuralParams
      effectsParams <- .Object@effectsParams
      estimates <- c()
      names <- c()
      if (length(structuralParams) > 0) {
        for (i in 1:length(structuralParams)) {
          stp <- structuralParams[[i]]
          fixedEffName <- stp@fixedEffName
          initialValue <- stp@initialValue
          if (fixedEffName != "") {
            estimates <- c(estimates, initialValue)
            names <- c(names, fixedEffName)
          }
        }
      }
      if (length(effectsParams) > 0) {
        for (i in 1:length(effectsParams)) {
          stp <- effectsParams[[i]]
          fixedEffName <- stp@fixedEffName
          initialValue <- stp@initialValue
          if (fixedEffName != "") {
            estimates <- c(estimates, initialValue)
            names <- c(names, fixedEffName)
          }
        }
      }
      names(estimates) <- names
    }
    return(estimates)
  }
)


#' @export
#' @param value Named numeric vector
#' @rdname initFixedEffects
setGeneric(
  name = "initFixedEffects<-",
  def = function(.Object, value) {
    standardGeneric("initFixedEffects<-")
  }
)

#' @export
#' @rdname initFixedEffects
setMethod(
  "initFixedEffects<-",
  "NlmePmlModel",
  definition = function(.Object, value) {
    if (.Object@isTextual) {
      statements <- .update_PMLwithThetas(.Object, value)
      .Object@statements <- list(statements)
    } else {
      sps <- .Object@structuralParams
      if (length(sps) > 0) {
        for (i in 1:length(sps)) {
          sp <- sps[[i]]
          name <- sp@name
          fixedEffName <- sp@fixedEffName
          if (!is.na(value[fixedEffName])) {
            sp@initialValue <- as.character(value[fixedEffName])
          }
          extraCode <- sp@extraCode
          if (length(extraCode) != 0) {
            pos <- grep("fixef\\(", extraCode)
            if (length(pos) != 0) {
              for (indx in 1:length(pos)) {
                ret <- updateFixedEffectStr(extraCode[[pos[[indx]]]],
                                            value,
                                            isTextual = FALSE)
                extraCode[[indx]] <- ret
              }
            }
            sp@extraCode <- extraCode
          }
          sps[[i]] <- sp
        }

        .Object@structuralParams <- sps
      }

      effectsParams <- .Object@effectsParams
      if (length(effectsParams) > 0) {
        for (i in 1:length(effectsParams)) {
          sp <- effectsParams[[i]]
          name <- sp@name
          fixedEffName <- sp@fixedEffName
          if (!is.na(value[fixedEffName])) {
            sp@initialValue <- as.character(value[fixedEffName])
          }
          effectsParams[[i]] <- sp
        }

        .Object@effectsParams <- effectsParams
      }

      .Object <- generatePML(.Object)
    }

    return(.Object)
  }
)

.get_columnNameOldmap <- function(oldDosepoint, OldMapping) {
  if (is.null(OldMapping[[oldDosepoint]]@variableType$Infusion)) {
    Infusion <- NA
  } else {
    Infusion <- OldMapping[[oldDosepoint]]@variableType$Infusion
  }

  OldMapping[[oldDosepoint]]@columnName
}

#' Generates PML statements based on the current model
#'
#' Generates PML statements based on the current model
#'
#' @param .Object PK/PD Model
#' @keywords internal
setGeneric(
  name = "generatePML",
  def = function(.Object) {
    standardGeneric("generatePML")
  }
)

setMethod(
  "generatePML",
  "NlmePmlModel",
  definition = function(.Object) {
    if (.Object@isTextual == FALSE) {
      .Object <- generatePMLModel(.Object)

      if (.Object@randomValuesInitialized == FALSE) {
        .Object <- initializeRandomEffectsBlock(.Object)
      }
    }
    return(.Object)
  }
)

#' Adds reset instructions to the model
#'
#' @param .Object PK/PD model
#' @param low Lower value of reset range
#' @param hi Upper value of reset range
#' @param Reset Name of reset column in input data set for column mapping.
#' The default is NULL.
#'
#' @return Depends on the specific methods
#'
#' @name addReset
#' @rdname addReset
#' @export
setGeneric(
  name = "addReset",
  def = function(.Object, low, hi, Reset = NULL) {
    standardGeneric("addReset")
  }
)

#' @describeIn addReset Method for the 'NlmePmlModel' class
#'
#' This method adds reset instructions to the NlmePmlModel object.
#' It updates the reset information, checks column mappings if input data is not null,
#' and adds a reset definition to user-defined extra definitions.
#'
#' @param .Object An 'NlmePmlModel' object to which you want to add reset instructions.
#' @param low Lower value of reset range.
#' @param hi Upper value of reset range.
#' @param Reset Name of reset column in input data set for column mapping.
#' The default is NULL.
#'
#' @return Returns the 'NlmePmlModel' object with updated reset information and definitions.
#'
#' @export
setMethod(
  "addReset",
  "NlmePmlModel",
  definition = function(.Object,
                        low,
                        hi,
                        Reset = NULL) {
    mdata <- .Object@inputData
    .Object@hasResetInfo <- TRUE
    .Object@resetInfo <- ResetColumnInfo(low, hi)
    # if(.Object@isTextual){
    if (!is.null(mdata)) {
      .check_column_mappings(Reset, data = mdata)
    } else {
      warning("argument `Reset` is NULL, 'Reset' column must be mapped in `modelColumnMapping()`")
    }
    existing_def <- .Object@userDefinedExtraDefs
    userDefinedExtraDefinitions(.Object) <-
      paste0("reset(\"", Reset, "\", c(", low, ",", hi, "))")
    current_def <- .Object@userDefinedExtraDefs
    defs <- c(existing_def, current_def)
    .Object@userDefinedExtraDefs <- unique(defs)
    return(.Object)
  }
)



###
### this function determines if the given real symmetric matrix is positive definite
###
### parameters
### x = a square numeric matrix object
### tol = tolerance level for zero
###
.is.positive.definite <- function(x, tol = 1e-8) {
  if (!.is.square.matrix(x)) {
    stop("argument x is not a square matrix")
  }
  if (!.is.symmetric.matrix(x)) {
    stop("argument x is not a symmetric matrix")
  }
  if (!is.numeric(x)) {
    stop("argument x is not a numeric matrix")
  }
  eigenvalues <- eigen(x, only.values = TRUE)$values
  n <- nrow(x)
  for (i in 1:n) {
    if (abs(eigenvalues[i]) < tol) {
      eigenvalues[i] <- 0
    }
  }
  if (any(eigenvalues <= 0)) {
    return(FALSE)
  }
  return(TRUE)
}

###
### determines if the given matrix is a square matrix
###
### arguments
### x = a matrix object
###
.is.square.matrix <- function(x) {
  if (!is.matrix(x)) {
    stop("argument x is not a matrix")
  }
  return(nrow(x) == ncol(x))
}

###
### this function determines if the matrix is symmetric
###
### argument
### x = a numeric matrix object
###
.is.symmetric.matrix <- function(x) {
  if (!is.matrix(x)) {
    stop("argument x is not a matrix")
  }
  if (!is.numeric(x)) {
    stop("argument x is not a numeric matrix")
  }
  if (!.is.square.matrix(x)) {
    stop("argument x is not a square numeric matrix")
  }
  return(sum(x == t(x)) == (nrow(x) ^ 2))
}

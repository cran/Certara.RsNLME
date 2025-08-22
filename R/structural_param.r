STP_PRODUCT <- 1
LogNormal <- 1
STP_SUM_ETA <- 2
Normal <- 2
STP_SUM_EXP <- 3
Combination <- 3
STP_EXP_SUM <- 4
Log <- 4
STP_ILOGIT <- 5
Logit <- 5

Block <- 1
Diagonal <- 2


StructParamTypeNames <-
  c("LogNormal", "Normal", "Combination", "Log", "Logit")

#' Class represents style of structural parameter
#'
#' Class represents style of structural parameter
#'
#' @slot  style    Parameter style: 1=LogNormal, 2=Normal,
#'                                  3=Combination, 4=Log,
#'                                  5=Logit
#'
#' @name NlmeStrucParamStyle
#' @rdname NlmeStrucParamStyle
#'
#' @keywords internal
NlmeStrucParamStyle <- setClass("NlmeStrucParamStyle",
                                slots = c(style = "numeric"))


setMethod("initialize", "NlmeStrucParamStyle",
          function(.Object,
                   style = STP_PRODUCT) {
            pStyle <- NlmeStrucParamStyle(style)
            .Object@style <- pStyle
          })


#' Class represents an NLME structural parameter
#'
#' Class represents an NLME structural parameter
#'
#' @param name             Name of the structural parameter
#' @param fixedEffName     Name to use for fixed effects
#' @param randomEffName    Name to use for random effects
#' @param hasRandomEffect  Does the parameter have a random effect?
#' @param style            Parameter style:
#'                           LogNormal, Normal, Combination, Log, Logit
#' @param initialValue     Initial value for the parameter
#' @param lowerBound       Lower limit for the parameter value
#' @param upperBound       Upper limit for the parameter value
#' @param units            Unit of measurement for the parameter
#' @param isFrozen         Is the parameter frozen?
#' @param isSequential     Estimate the parameter sequentially
#' @param ranEffInitValue  Initial value for the random effect
#' @param code             For Custom style, PML code to override the definition
#' @param extraCode        Extra lines of code that relates to this parameter
#'
#' @examples
#' \donttest{
#' NlmeStructuralParameter(STP_SUM_ETA, "EC50")
#' NlmeStructuralParameter(STP_SUM_EXP, "Imax")
#' }
#'
#' @noRd
#' @keywords internal
NlmeStructuralParameter <- setClass(
  "NlmeStructuralParameter",
  slots = c(
    name = "character",
    fixedEffName = "character",
    randomEffName = "character",
    hasRandomEffect = "logical",
    hasCovariateEffect = "logical",
    style = "numeric",
    initialValue = "character",
    lowerBound = "character",
    upperBound = "character",
    units = "character",
    isFrozen = "logical",
    isSequential = "logical",
    ranEffInitValue = "character",
    code = "ANY",
    extraCode = "list"
  )
)


setMethod("initialize", "NlmeStructuralParameter",
          function(.Object,
                   name = "",
                   fixedEffName = "",
                   randomEffName = "",
                   hasRandomEffect = TRUE,
                   hasCovariateEffect = FALSE,
                   style = STP_PRODUCT,
                   initialValue = "1",
                   lowerBound = "",
                   upperBound = "",
                   units = "",
                   isFrozen = FALSE,
                   isSequential = FALSE,
                   ranEffInitValue = "1",
                   code = list(),
                   extraCode = list()) {
            if (fixedEffName == "") {
              fixedEffName <- paste0("tv", name)
            }
            if (randomEffName == "") {
              randomEffName <- paste0("n", name)
            }
            if (hasRandomEffect == FALSE) {
              randomEffName <- ""
            }
            .Object@name <- name
            .Object@fixedEffName <- fixedEffName
            .Object@randomEffName <- randomEffName
            .Object@hasRandomEffect <- hasRandomEffect
            .Object@hasCovariateEffect <- hasCovariateEffect
            .Object@style <- style
            .Object@initialValue <- initialValue
            .Object@lowerBound <- lowerBound
            .Object@upperBound <- upperBound
            .Object@units <- units
            .Object@isFrozen <- isFrozen
            .Object@ranEffInitValue <- ranEffInitValue
            .Object@isSequential <- isSequential
            .Object@code <- code
            .Object@extraCode <- extraCode
            .Object
          })

#'
#' Prints structural parameter information
#'
#' Prints structural parameter information
#'
#' @param x    Structural parameter
#' @inheritParams ellipsis::dots_used
#'
#' @keywords internal
#' @noRd
print.NlmeStructuralParameter <- function(x, ...) {
  cat(paste("Name           : ", x@name), "\n")
  cat(paste("Frozen         :", x@isFrozen), "\n")
  cat(paste("isSequential   :", x@isSequential), "\n")
  cat(paste("hasRandomEffect:", x@hasRandomEffect), "\n")
  cat(paste("Type           :", StructParamTypeNames[x@style]), "\n")
  cat(paste("Initial Value  :", x@initialValue), "\n")
  cat(paste("Lower Bound    :", x@lowerBound), "\n")
  cat(paste("Upper Bound    :", x@upperBound), "\n")
  cat(paste("Units          :", x@units), "\n")
  cat(paste("code           :", paste(x@code)), "\n")
  cat(paste("extraCode      :", paste(x@extraCode)), "\n")
}

setMethod(
  f = "show",
  signature = "NlmeStructuralParameter",
  definition = function(object) {
    print(object)
  }
)

#' Set structural parameter in model object
#'
#' Use to specify the relationship of the structural parameter with corresponding fixed effect,
#' random effect, and covariate.
#'
#' @param .Object          Model object
#' @param paramName        Name of the structural parameter
#' @param fixedEffName     Name of the corresponding fixed effect
#' @param randomEffName    Name of the corresponding random effect;
#' only applicable to population models.
#' @param style            Use to specify the relationship of
#' the structural parameter with its corresponding fixed effect,
#' random effect, and covariate, if exists.
#' \itemize{
#' \item \code{"LogNormal" } (Default): The structural parameter
#' is defined as \code{Product * exp(Eta)}
#' \item \code{"LogNormal1"}: The structural parameter
#' is defined as \code{Sum * exp(Eta)}
#' \item \code{"LogNormal2"}: The structural parameter
#' is defined as \code{exp(Sum + Eta)}
#' \item \code{"LogitNormal"}: The structural parameter
#' is defined as \code{ilogit(Sum + Eta)}
#' \item \code{"Normal"}: The structural parameter
#' is defined as \code{Sum + Eta)}
#' }
#' 	\code{Product} denotes the product of the corresponding
#' 	fixed effect and covariate effect terms (if exists),
#' 	\code{Eta} represents the corresponding random effect,
#' 	and \code{Sum} denotes the sum of its corresponding fixed effect
#' 	and covariate effect terms (if exists).
#'
#' @param hasRandomEffect  Set to \code{FALSE} to remove
#' the corresponding random effect from the model.
#' Only applicable to population models.
#' If \code{NULL} the system will automatically set
#'  \code{hasRandomEffect = TRUE} for population models,
#'  and \code{hasRandomEffect = FALSE} for individual models.
#'
#' @return Modified \code{NlmePmlModel} object
#' @examples
#'
#' model <- pkindirectmodel(
#'   indirectType = "LimitedInhibition",
#'   isBuildup = FALSE,
#'   data = pkpdData,
#'   ID = "ID",
#'   Time = "Time",
#'   A1 = "Dose",
#'   CObs = "CObs",
#'   EObs = "EObs"
#' )
#'
#' # Change style of structural parameter "Imax" to "LogitNormal"
#' # and rename fixed effect to "tvlogitImax"
#' model <- structuralParameter(model,
#'   paramName = "Imax",
#'   style = "LogitNormal", fixedEffName = "tvlogitImax"
#' )
#'
#' # Remove random effect for structural parameter "IC50"
#' model <- structuralParameter(model,
#'   paramName = "IC50",
#'   hasRandomEffect = FALSE
#' )
#'
#' @export
structuralParameter <- function(.Object,
                                paramName,
                                fixedEffName = NULL,
                                randomEffName = NULL,
                                style = "LogNormal",
                                hasRandomEffect = NULL) {
  if (!inherits(.Object, "NlmePmlModel")) {
    stop("Object must be of class 'NlmePmlmodel'")
  }

  if (.Object@isTextual) {
    stop("`structuralParameter()` cannot be used for edited or textual models")
  }


  # Check if pk/pd/indirect/linear are frozen in structural model and set isFrozen to TRUE
  pkFrozen <- .Object@pkModelAttrs@isPkFrozen
  if (pkFrozen &&
      paramName %in% c(
        "V",
        "Cl",
        "V2",
        "Cl2",
        "V3",
        "Cl3",
        "Ka",
        # clearance
        "Ke",
        "K12",
        "K21",
        "K13",
        "K31",
        # micro
        "A",
        "Alpha",
        "B",
        "Beta",
        "C",
        "Gamma",
        # macro/#macro1
        "Km",
        "Vmax",
        # elimination comp
        "Fe",
        # Fraction excreted
        "MeanDelayTime",
        "ShapeParamMinusOne",
        "ShapeParam",
        # Distributed delay
        "Tlag"
      )) {
    # Tlag

    if (!is.null(hasRandomEffect) && hasRandomEffect == TRUE) {
      warning(
        "PK is frozen in structural model, argument `hasRandomEffect = TRUE` not applicable"
      )
    }
    hasRandomEffect <- FALSE
  }

  pdFrozen <- .Object@emaxModelAttrs@frozen
  if (pdFrozen &&
      paramName %in% c("EC50", "Emax", "Ke0", "IC50", "Gam", "E0", "Imax")) {
    if (!is.null(hasRandomEffect) && hasRandomEffect == TRUE) {
      warning(
        "PD is frozen in structural model, argument `hasRandomEffect = TRUE` not applicable"
      )
    }
    hasRandomEffect <- FALSE
  }

  indirectFrozen <- .Object@indirectModelAttrs@frozen
  if (indirectFrozen &&
      paramName %in% c("Kin", "Kout", "EC50", "Emax", "Ke0", "gam", "s")) {
    if (!is.null(hasRandomEffect) && hasRandomEffect == TRUE) {
      warning(
        "Indirect is frozen in structural model, argument `hasRandomEffect = TRUE` not applicable"
      )
    }
    hasRandomEffect <- FALSE
  }


  linearFrozen <- .Object@isLinearFrozen
  if (linearFrozen &&
      paramName %in% c("EAlpha", "EBeta", "EGam", "Ke0")) {
    if (!is.null(hasRandomEffect) && hasRandomEffect == FALSE) {
      warning(
        "Linear is frozen in structural model, argument `hasRandomEffect = TRUE` not applicable"
      )
    }
    hasRandomEffect <- FALSE
  }

  if (.Object@isPopulation == FALSE) {
    if (is.null(hasRandomEffect)) {
      hasRandomEffect <- FALSE
    }
    if (hasRandomEffect == TRUE) {
      stop(
        " `isPopulation = FALSE`; argument `hasRandomEffect = TRUE` not applicable for individual models"
      )
    }
    if (!is.null(randomEffName)) {
      stop(
        " `isPopulation = FALSE`; argument `randomEffName` not applicable for individual models"
      )
    }
  } else {
    if (is.null(hasRandomEffect)) {
      hasRandomEffect <- TRUE
    }
  }


  if (style == "LogNormal") {
    style <- LogNormal
  } else if (style == "Normal") {
    style <- Normal
  } else if (style == "LogNormal1") {
    style <- Combination
  } else if (style == "LogNormal2") {
    style <- Log
  } else if (style == "LogitNormal") {
    style <- Logit
  } else {
    stop(
      "argument `style` must be one of 'Normal', 'LogNormal', 'LogNormal1',  'LogNormal2', 'LogitNormal'"
    )
  }

  `%notin%` <- Negate(`%in%`)

  # structuralParams <- .Object@structuralParams
  stParmPos <-
    which(structuralParameterNames(.Object, omitEmpties = FALSE) == paramName)
  if (length(stParmPos) == 0) {
    stop(paste0(
      "'",
      paramName,
      "' not found in existing structural parameters"
    ))
  }

  oldRanEff <-
    .Object@structuralParams[[stParmPos]]@randomEffName
  if (oldRanEff == "") {
    oldRanEff <- paste0("n", paramName)
  }

  noRandom <-
    !hasRandomEffect &&
    (length(.Object@randomValues) == 0 ||
      (length(unlist(.Object@randomValues@effectNames)) == 1 &&
         .Object@randomValues@effectNames[[1]] == oldRanEff))

  .Object@randomValuesInitialized <- TRUE
  if (noRandom) {
    structuralParam(.Object, paramName) <-
      c(
        style = style,
        fixedEffName = fixedEffName,
        randomEffName = randomEffName,
        hasRandomEffect = hasRandomEffect
      )

    methods::slot(.Object, "randomValues", check = FALSE) <- NULL
    methods::slot(.Object, "randomBlocks", check = FALSE) <- NULL

  } else {
    structuralParam(.Object, paramName) <-
      c(
        style = style,
        fixedEffName = fixedEffName,
        randomEffName = randomEffName,
        hasRandomEffect = hasRandomEffect
      )

    # dealing with NlmeRandomEffectValues class (randomValues)
    # and list of NlmeRandomEffectBlock class (randomBlocks)

    # we expect consistency between row names of randomValues@values
    # and randomValues@effectNames
    if (length(.Object@randomValues) == 0) {
      randomEffectNames <- character(0)
    } else {
      randomEffectNames <- unlist(.Object@randomValues@effectNames)
    }

    if (is.null(randomEffName)) {
      randomEffName <- oldRanEff
    } else if (randomEffName != oldRanEff &&
               randomEffName %in% randomEffectNames ) {
      stop(paste("Cannot use", randomEffName, "random effect name\n",
                 "since it is bound to other structural parameter."))
    }

    if (!hasRandomEffect) {
      # removing existing random
      if (length(c(randomEffName, oldRanEff)) > 0 &&
          length(.Object@randomValues@effectNames) != 0) {
        # removing
        randomValuesDF <- as.data.frame(.Object@randomValues@values)
        randomValuesDF <-
          randomValuesDF[rownames(randomValuesDF) %notin% c(randomEffName, oldRanEff),
                         colnames(randomValuesDF) %notin% c(randomEffName, oldRanEff), drop = FALSE]
        .Object@randomValues@values <- as.matrix.data.frame(randomValuesDF)
        .Object@randomValues@effectNames <-
          list(randomEffectNames[randomEffectNames %notin% c(randomEffName, oldRanEff)])

        for (blockNumber in seq_along(.Object@randomBlocks)) {
          effectNames <- .Object@randomBlocks[[blockNumber]]@effectNames
          effectNames <-
            effectNames[effectNames %notin% c(randomEffName, oldRanEff)]
          if (length(effectNames) == 0) {
            .Object@randomBlocks[[blockNumber]] <- NULL
            break
          } else {
            .Object@randomBlocks[[blockNumber]]@effectNames <- effectNames
          }
        }
      }
    } else {
      # random effect shouldn't be removed or should be added
      if (oldRanEff %notin% randomEffectNames) {
        # need to add a new one
        RanefInitValue <- 1
        if (length(.Object@randomValues) > 0) {
          randomValuesDF <- as.data.frame(.Object@randomValues@values)
          randomValuesDF <- cbind.data.frame(randomValuesDF, rep(0, nrow(randomValuesDF)))
          randomValuesDF <- rbind.data.frame(randomValuesDF, c(rep(0, nrow(randomValuesDF)), RanefInitValue))
          colnames(randomValuesDF) <- c(randomEffectNames, randomEffName)
          rownames(randomValuesDF) <- c(randomEffectNames, randomEffName)
          .Object@randomValues@values <- as.matrix.data.frame(randomValuesDF)
          .Object@randomValues@effectNames <-
            list(c(randomEffectNames, randomEffName))


          randomBlock <- NlmeRandomEffectBlock(Diagonal, list(randomEffName), FALSE)
          .Object@randomBlocks <- c(.Object@randomBlocks, randomBlock)
        } else {
          randomValuesDF <- data.frame(RanefInitValue)
          colnames(randomValuesDF) <- randomEffName
          rownames(randomValuesDF) <- randomEffName
          .Object@randomValues <-
            NlmeRandomEffectValues(effectNames = list(randomEffName),
                                   values = as.matrix(randomValuesDF))

          .Object@randomBlocks <- list(NlmeRandomEffectBlock(Diagonal, list(randomEffName), FALSE))
        }

      } else {
        # the random effect is already there
        # need to modify the name if it is different
        if (oldRanEff != randomEffName) {
          newRandomEffectNames <- randomEffectNames
          newRandomEffectNames[newRandomEffectNames == oldRanEff] <- randomEffName
          colnames(.Object@randomValues@values) <- newRandomEffectNames
          rownames(.Object@randomValues@values) <- newRandomEffectNames
          .Object@randomValues@effectNames <- list(newRandomEffectNames)
          for (blockNumber in seq_along(.Object@randomBlocks)) {
            effectNames <- .Object@randomBlocks[[blockNumber]]@effectNames
            if (oldRanEff %notin% effectNames) next
            effectNames[effectNames == oldRanEff] <-
              randomEffName
            .Object@randomBlocks[[blockNumber]]@effectNames <-
              effectNames
          }
        }
      }
    }
  }

  if (length(.Object@randomBlocks) != 0) {
    .Object@randomEffectsStatements <-
      as.list(randomBlockStatement(.Object))
  } else {
    .Object@randomEffectsStatements <- list()
  }

  if (length(.Object@covariateList) != 0) {
    .Object@randomOccasionalEffectsStatements <-
      as.list(randomOccasionalBlockStatement(.Object))
  } else {
    .Object@randomOccasionalEffectsStatements <- list()
  }

  .Object <- generatePML(.Object)

  return(.Object)
}

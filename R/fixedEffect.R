#' Specifies the initial values, lower bounds, upper bounds, and units for fixed effects in a model
#'
#' Specifies the initial values, lower bounds, upper bounds, and units for fixed effects in a model
#'
#' @param .Object       Model object in which to define fixed effects values
#' @param effect        Character or character vector specifying names of fixed effects
#' @param value         Numeric or numeric vector specifying the initial values of fixed effects. If supplying vector, must be in the same order/length as corresponding \code{effect}.
#' @param lowerBound    Numeric or numeric vector specifying the lower limit values of fixed effects. If supplying vector, must be in the same order as \code{effect}.
#' @param upperBound    Numeric or numeric vector specifying the upper limit values of fixed effects. If supplying vector, must be in the same order as \code{effect}.
#' @param isFrozen      Logical or logical vector. Set to \code{TRUE} to freeze the fixed effect to the specified initial value. If supplying vector, must be in the same order as \code{effect}.
#' @param unit          Character or character vector specifying units of measurement for the fixed effects. If supplying a vector, must be in the same order as \code{effect}.
#'
#' @return Modified \code{NlmePmlModel} object
#' @examples
#' model <- pkmodel(
#'   numCompartments = 2,
#'   data = pkData,
#'   ID = "Subject",
#'   Time = "Act_Time",
#'   A1 = "Amount",
#'   CObs = "Conc",
#'   modelName = "TwCpt_IVBolus_FOCE_ELS"
#'   )
#'
#'  # View initial/current fixed effect values
#'  initFixedEffects(model)
#'
#' model <- model |>
#' fixedEffect(
#'   effect = c("tvV", "tvCl", "tvV2", "tvCl2"),
#'   value = c(15, 5, 40, 15)
#'   )
#'
#' @export
fixedEffect <- function(.Object,
                        effect,
                        value = NULL,
                        lowerBound = NULL,
                        upperBound = NULL,
                        isFrozen = NULL,
                        unit = NULL) {
  if (!inherits(.Object, "NlmePmlModel")) {
    stop("Object must be of class 'NlmePmlmodel'")
  }

  if (.Object@isTextual) {
    stop("`fixedEffect()` cannot be used for edited or textual models")
  }

  if (!is.null(value) && length(value) != length(effect)) {
    stop("The length of 'value' must be the same as the length of 'effect'.")
  }

  if (!is.null(lowerBound) && length(lowerBound) != length(effect)) {
    stop("The length of 'lowerBound' must be the same as the length of 'effect'.")
  }

  if (!is.null(upperBound) && length(upperBound) != length(effect)) {
    stop("The length of 'upperBound' must be the same as the length of 'effect'.")
  }

  if (!is.null(isFrozen) && length(isFrozen) != length(effect)) {
    stop("The length of 'isFrozen' must be the same as the length of 'effect'.")
  }

  if (!is.null(unit) && length(unit) != length(effect)) {
    stop("The length of 'unit' must be the same as the length of 'effect'.")
  }

  # Check if pk/pd/indirect/linear are frozen in structural model and set isFrozen to TRUE
  pkFrozen <- .Object@pkModelAttrs@isPkFrozen
  if (pkFrozen &&
      any(
        effect %in% c(
          "tvV",
          "tvCl",
          "tvV2",
          "tvCl2",
          "tvV3",
          "tvCl3",
          "tvKa",
          # clearance
          "tvKe",
          "tvK12",
          "tvK21",
          "tvK13",
          "tvK31",
          # micro
          "tvA",
          "tvAlpha",
          "tvB",
          "tvBeta",
          "tvC",
          "tvGamma",
          # macro/#macro1
          "tvKm",
          "tvVmax",
          # elimination comp
          "tvFe",
          # Has effects compartment and Fraction Excreted
          "tvMeanDelayTime",
          "tvShapeParamMinusOne",
          "tvShapeParam",
          # Distributed delay
          "tvTlag"
        )
      )) {
    # Has Tlag
    if (!is.null(isFrozen) && any(isFrozen == FALSE)) {
      warning("PK is frozen in structural model, argument `isFrozen = FALSE` not applicable")
    }

    isFrozen <- rep(TRUE, length(effect))
  }

  pdFrozen <- .Object@emaxModelAttrs@frozen
  if (pdFrozen &&
      any(effect %in% c(
        "tvEC50",
        "tvEmax",
        "tvKe0",
        "tvIC50",
        "tvGam",
        "tvE0",
        "tvImax"
      ))) {
    if (!is.null(isFrozen) && any(isFrozen == FALSE)) {
      warning("PD is frozen in structural model, argument `isFrozen = FALSE` not applicable")
    }

    isFrozen <- rep(TRUE, length(effect))
  }

  indirectFrozen <- .Object@indirectModelAttrs@frozen
  if (indirectFrozen &&
      any(effect %in% c("tvKin", "tvKout", "tvEC50", "tvEmax", "tvKe0", "tvgam", "tvs"))) {
    if (!is.null(isFrozen) && any(isFrozen == FALSE)) {
      warning(
        "Indirect is frozen in structural model, argument `isFrozen = FALSE` not applicable"
      )
    }

    isFrozen <- rep(TRUE, length(effect))
  }

  linearFrozen <- .Object@isLinearFrozen
  if (linearFrozen &&
      any(effect %in% c("tvEAlpha", "tvEBeta", "tvEGam", "tvKe0"))) {
    if (!is.null(isFrozen) && any(isFrozen == FALSE)) {
      warning("Linear is frozen in structural model, argument `isFrozen = FALSE` not applicable")
    }

    isFrozen <- rep(TRUE, length(effect))
  }

  effectsParams <- .Object@effectsParams
  sps <- .Object@structuralParams

  fnames <- lapply(sps, function(x) {
    x@fixedEffName
  })
  enames <- lapply(effectsParams, function(x) {
    x@fixedEffName
  })

  fenames <- c(fnames, enames)

  `%notin%` <- Negate(`%in%`)

  if (any(effect %notin% fenames)) {
    stop("one or more values in `effect` argument not found in existing fixed effects")
  }

  if (!is.null(value)) {
    names(value) <- effect
  }

  CurrentFixefValues <-
    as.numeric(sapply(.Object@structuralParams, function(x) {
      x@initialValue
    }))
  names(CurrentFixefValues) <- unlist(fnames)

  if (!is.null(lowerBound)) {
    assertthat::assert_that(length(lowerBound) == length(effect), msg = "length of lowerBound is not equal to length of effect")
    if (!is.null(value)) {
      assertthat::assert_that(length(lowerBound) == length(value), msg = "length of lowerBound is not equal to length of values")
    }

    names(lowerBound) <- effect
    for (l in seq_along(lowerBound)) {
      if (!is.null(value)) {
        fixefValueToCheck <- value[l]
      } else {
        fixefValueToCheck <-
          CurrentFixefValues[names(CurrentFixefValues) == names(lowerBound[l])]
      }

      msg <-
        paste0(
          "The fixed effect value should be more than value supplied to `lowerBound`: ",
          effect[l],
          " = ",
          fixefValueToCheck,
          ", `lowerBound` = ",
          lowerBound[l]
        )
      assertthat::assert_that(lowerBound[l] < fixefValueToCheck, msg = msg)
    }
  }

  if (!is.null(upperBound)) {
    assertthat::assert_that(length(upperBound) == length(effect),
                            msg = "length of upperBound is not equal to length of effect"
    )
    if (!is.null(value)) {
      assertthat::assert_that(length(upperBound) == length(value),
                              msg = "length of upperBound is not equal to length of fixef values provided"
      )
    }

    names(upperBound) <- effect
    for (u in seq_along(upperBound)) {
      if (!is.null(value)) {
        fixefValueToCheck <- value[u]
      } else {
        fixefValueToCheck <-
          CurrentFixefValues[names(CurrentFixefValues) == names(upperBound[u])]
      }

      msg <-
        paste0(
          "The fixed effect value should be less than value supplied to `upperBound`: ",
          effect[u],
          " = ",
          fixefValueToCheck,
          ", `upperBound` = ",
          upperBound[u]
        )

      assertthat::assert_that(upperBound[u] > fixefValueToCheck, msg = msg)
    }
  }

  if (!is.null(unit)) {
    assertthat::assert_that(length(unit) == length(effect),
                            msg = "length of `unit` is not equal to length of `effect`"
    )
    names(unit) <- effect
  }

  if (!is.null(isFrozen)) {
    assertthat::assert_that(length(isFrozen) == length(effect),
                            msg = "length of `isFrozen` is not equal to length of `effect`"
    )
    names(isFrozen) <- effect
  }

  if (length(sps) > 0) {
    for (i in 1:length(sps)) {
      sp <- sps[[i]]
      name <- sp@name
      fixedEffName <- sp@fixedEffName
      if (!is.null(value) && !is.na(value[fixedEffName])) {
        sp@initialValue <- as.character(value[fixedEffName])
      }

      if (!is.null(lowerBound) &&
          !is.na(lowerBound[fixedEffName])) {
        sp@lowerBound <- as.character(lowerBound[fixedEffName])
      }
      if (!is.null(upperBound) &&
          !is.na(upperBound[fixedEffName])) {
        sp@upperBound <- as.character(upperBound[fixedEffName])
      }
      if (!is.null(unit) && !is.na(unit[fixedEffName])) {
        sp@units <- unit[fixedEffName]
      }
      if (!is.null(isFrozen) && !is.na(isFrozen[fixedEffName])) {
        sp@isFrozen <- isFrozen[fixedEffName]
      }

      extraCode <- sp@extraCode
      if (length(extraCode) != 0) {
        pos <- grep("fixef\\(", extraCode)
        if (length(pos) != 0) {
          for (indx in 1:length(pos)) {
            ret <- updateFixedEffectStr(extraCode[[pos[[indx]]]],
                                        value,
                                        isTextual = FALSE
            )
            extraCode[[indx]] <- ret
          }
        }
        sp@extraCode <- extraCode
      }
      sps[[i]] <- sp
    }

    .Object@structuralParams <- sps
  }

  if (length(effectsParams) > 0) {
    for (i in 1:length(effectsParams)) {
      sp <- effectsParams[[i]]
      name <- sp@name
      fixedEffName <- sp@fixedEffName
      if (!is.null(value) && !is.na(value[fixedEffName])) {
        sp@initialValue <- as.character(value[fixedEffName])
      }

      if (!is.null(lowerBound) &&
          !is.na(lowerBound[fixedEffName])) {
        sp@lowerBound <- as.character(lowerBound[fixedEffName])
      }
      if (!is.null(upperBound) &&
          !is.na(upperBound[fixedEffName])) {
        sp@upperBound <- as.character(upperBound[fixedEffName])
      }
      if (!is.null(unit) && !is.na(unit[fixedEffName])) {
        sp@units <- unit[fixedEffName]
      }
      if (!is.null(isFrozen) && !is.na(isFrozen[fixedEffName])) {
        names(isFrozen) <- effect
        sp@isFrozen <- isFrozen[fixedEffName]
      }

      effectsParams[[i]] <- sp
    }

    .Object@effectsParams <- effectsParams
  }

  .Object <- generatePML(.Object)
  return(.Object)
}

updateFixedEffectStr <- function(line, values, isTextual = FALSE) {
  newLine <- line
  if (length(grep("enable", line)) == 1) {
    isCovEff <- TRUE
  } else {
    isCovEff <- FALSE
  }

  if (isCovEff && isTextual) {
    tokens <- unlist(strsplit(line, split = "\\("))
    fixEffName <- trimws(unlist(strsplit(tokens[[2]], split = "="))[[1]], "both")
    value <- as.numeric(values[fixEffName])
    if (!is.na(values[fixEffName])) {
      value <- as.numeric(values[fixEffName])
      newValue <- gsub(
        "\\,.*\\,",
        paste0("\\,", value, "\\,"),
        trimws(tokens[[5]], "left")
      )
      tokens[[5]] <- newValue
      newLine <- paste(tokens, collapse = "(")
    }
  } else {
    tokens <- unlist(strsplit(line, split = "\\("))
    fixEffName <- trimws(unlist(strsplit(tokens[[2]], split = "="))[[1]], "both")

    if (!is.na(values[fixEffName])) {
      value <- as.numeric(values[fixEffName])
      # newValue = sub("[^,]+[^,]+",value,trimws(tokens[[3]],"left"))
      fix_values <- unlist(strsplit(tokens[[3]], split = ","))
      fix_values[[2]] <- value
      tokens[[3]] <- paste0(fix_values, collapse = ",")
      newLine <- paste(tokens, collapse = "(")
    }
  }
  newLine
}

#' Sets or updates the covariance matrix of random effects
#'
#' Use to set or update the covariance matrix of random effects in a model object.
#'
#' @param .Object    Model object
#' @param effect    One or more names of available random effects.
#' @param value     Initial values for the diagonal elements of the covariance matrix of
#' random effects (if \code{isDiagonal = TRUE}, or initial values for the lower triangular
#' elements (including diagonal elements) of the
#' covariance matrix (if \code{isDiagonal = FALSE}) in a row-wise order.
#' @param isDiagonal Set to \code{TRUE} to if the covariance matrix of the specified random effects is a diagonal matrix.
#' or \code{FALSE} if not.
#' @param isFrozen   Set to \code{TRUE} to freeze the covariance matrix of random effects.
#' @param ... Additional arguments
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
#' model <- model |>
#'   randomEffect(effect = c("nV", "nCl", "nCl2"), value = rep(0.1, 3))
#' @export
#'
randomEffect <-
  function(.Object,
           effect,
           value = NULL,
           isDiagonal = TRUE,
           isFrozen = FALSE,
           ...) {
    if (!inherits(.Object, "NlmePmlModel")) {
      stop("Object must be of class 'NlmePmlmodel'")
    }

    if (.Object@isTextual) {
      stop("`randomEffect()` cannot be used for edited or textual models")
    }

    arg_list <- as.list(substitute(list(...)))

    effects <- eval(arg_list$effects)
    values <- eval(arg_list$values)

    if (!is.null(effects)) {
      warning("`effects` argument is deprecated, please use `effect`.")
      if (missing(effect)) {
        effect <- effects
      }
    }

    if (!is.null(values)) {
      warning("`values` argument is deprecated, please use `value`")
      if (is.null(value)) {
        value <- values
      }
    }

    mod_effects <-
      getRandomEffectNames(.Object) # apply this to model@randomValues to update names with mod_effects
    structuralParams <- .Object@structuralParams

    mod_ranEffects <-
      vector(mode = "character", length = length(structuralParams))

    for (i in seq_along(structuralParams)) {
      mod_ranEffects[[i]] <- structuralParams[[i]]@randomEffName
    }

    if (any(duplicated(mod_effects))) {
      warning(
        "Duplicated random effects detected in the structural parameters. ",
        "Please check and fix before continue.",
        call. = FALSE
      )
    }

    rmEffects <- setdiff(mod_ranEffects, mod_effects)
    # If any random effects were renamed, update @randomValues
    if (length(rownames(.Object@randomValues@values)) == length(mod_effects)) {
      curRowNames <- rownames(.Object@randomValues@values)
      if (!all(curRowNames %in% mod_effects)) {
        # renaming one by one
        for (StParmRanefName in mod_effects) {
          if (StParmRanefName %in% curRowNames)
            next
          RanefColToBeRenamed <-
            curRowNames[curRowNames %notin% StParmRanefName][1]
          curRowNames[curRowNames %notin% StParmRanefName][1] <-
            StParmRanefName
          message(
            RanefColToBeRenamed,
            " random effect is renamed to ",
            StParmRanefName,
            "in model@randomValues"
          )
        }
      }

      rownames(.Object@randomValues@values) <- curRowNames
      colnames(.Object@randomValues@values) <- curRowNames
      .Object@randomValues@effectNames <- as.list(curRowNames)
    } else {
      .Object@randomValues@effectNames <- as.list(mod_effects)
    }

    `%notin%` <- Negate(`%in%`)
    if (any(effect %notin% mod_effects)) {
      stop(
        "one or more effects specified in `effect` argument does not exist in model random effects"
      )
    }

    block_old <- .Object@randomBlocks

    for (h in seq_along(block_old)) {
      for (i in seq_along(block_old[[h]]@effectNames)) {
        if (block_old[[h]]@effectNames[[i]] %in% effect) {
          block_old[[h]]@effectNames[[i]] <- ""
        }
        if (block_old[[h]]@effectNames[[i]] %notin% mod_effects) {
          block_old[[h]]@effectNames[[i]] <- ""
        }
      }
    }

    for (i in seq_along(block_old)) {
      block_old[[i]]@effectNames[block_old[[i]]@effectNames == ""] <-
        NULL
    }


    block_ex <- list()
    # Remove blocks with no effectNames
    for (e in seq_along(block_old)) {
      if (length(block_old[[e]]@effectNames) != 0) {
        block_ex[[e]] <- block_old[[e]]
      }
    }

    if (length(block_ex) != 0) {
      block_ex[sapply(block_ex, is.null)] <- NULL
    }


    for (i in seq_along(block_ex)) {
      for (j in seq_along(block_ex[[i]]@effectNames)) {
        if (block_ex[[i]]@effectNames[[j]] %in% rmEffects) {
          block_ex[[i]]@effectNames[[j]] <- ""
        }
      }
    }
    for (i in seq_along(block_ex)) {
      block_ex[[i]]@effectNames[block_ex[[i]]@effectNames == ""] <- NULL
    }


    for (i in seq_along(block_ex)) {
      if (length(block_ex[[i]]@effectNames) == 0) {
        block_ex[[i]] <- NULL
      }
    }


    if (isDiagonal) {
      block <- NlmeRandomEffectBlock(type = Diagonal,
                                     effectNames = as.list(effect),
                                     frozen = isFrozen)
    } else {
      block <- NlmeRandomEffectBlock(type = Block,
                                     effectNames = as.list(effect),
                                     frozen = isFrozen)
    }

    if (length(block_ex) == 0) {
      .Object@randomBlocks <- list(block)
    } else {
      blocks <- c(block_ex, block)
      .Object@randomBlocks <- as.list(blocks)
    }


    if (!is.null(value)) {
      if (any(value < 0)) {
        stop("supplied `values` must be positive")
      }
      if (isDiagonal) {
        assertthat::assert_that(length(value) == length(effect),
                                msg = "incorrect number of `values` specified for corresponding `effects` in diagonal matrix")
        randomMatrixDF <-
          as.data.frame.matrix(.Object@randomValues@values)
        for (i in seq_along(effect)) {
          if (effect[i] %notin% colnames(randomMatrixDF))
            next
          randomMatrixDF[effect[i], ] <- 0
          randomMatrixDF[, effect[i]] <- 0
          randomMatrixDF[effect[i], effect[i]] <- value[i]
        }

        .Object@randomValues@values <-
          as.matrix.data.frame(randomMatrixDF)
      } else {
        indx <- 1
        for (i in 1:length(effect)) {
          for (j in 1:i) {
            tryCatch({
              .Object@randomValues <- updateValue2(.Object@randomValues,
                                                   effect[[i]],
                                                   effect[[j]],
                                                   value[[indx]])
              indx <- indx + 1
            },
            error = function(e) {
              stop(
                "isDiagonal = FALSE; incorrect number of `values` specified for corresponding `effects` in block covariance matrix "
              )
            })
          }
        }
        d <- length(effect)
        covMatrix <-
          matrix(NA, d, d) # dimension: length(effects) x length(effects)
        covMatrix[upper.tri(covMatrix, diag = TRUE)] <-
          value # specify the lower triangular part (including diagonal elements)
        # of the covariance matrix using the values specified by the user
        covMatrix[lower.tri(covMatrix)] <-
          t(covMatrix)[lower.tri(covMatrix)] # specify the upper triangular part of the covariance matrix
        # using the informative provided for the lower triangular part
        if (!.is.positive.definite(covMatrix)) {
          stop("the block covariance matrix specified is not positive definite")
        } # determine whether the specified covariance matrix is positive-definite or not.
      }
    }
    .Object@randomEffectsStatements <-
      as.list(randomBlockStatement(.Object))

    .Object <- generatePML(.Object)


    return(.Object)
  }

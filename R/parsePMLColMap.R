.extract_ResponsesObservations <-
  function(statement_string, ResponsesObservations) {
    regexprString <-
      paste0("(?<=",
             ResponsesObservations,
             " )[ a-zA-Z\\d_\\(\\)]+(?=\\)\\|)")
    observationsString <- regmatches(statement_string,
                                     regexpr(regexprString,
                                             statement_string,
                                             perl = TRUE))

    observationswithBQL <-
      unlist(strsplit(observationsString, "(\\) \\()|(\\()|(\\))", perl = TRUE))
    observationswithBQL <-
      observationswithBQL[observationswithBQL != ""]
    observations <-
      unlist(lapply(observationswithBQL,
                    function(x) {
                      observation <- strsplit(x, " ")[[1]][1]
                      observation
                    }))

    observations
  }

#' Embed column definition info into the model
#'
#' Add/update column definition information for the model object
#'
#' @inheritParams colMapping
#' @inheritParams createModelInfo
#'
#' @return modified `NLMEPmlModel` object with column mapping
#' definitions
#'
#' @details
#' Intended to be used by other packages
#'
#'
#' @md
#' @export
parsePMLColMap <- function(.Object, ForceRun = TRUE) {
  initialcf <- ""

  tryCatch({
    initialcf <- createModelInfo(.Object, ForceRun = ForceRun)
  }, error = function(e) {
    warning(e)
  })

  if (length(initialcf) < 2)
    return(.Object)

  statement_string <- paste0(initialcf, collapse = '|')

  #### remove old unmapped objects
  OldMapping <- .Object@columnMapping@mapping
  .Object@columnMapping@mapping <- list()
  #### mapping time ####
  if (grepl("(time)", statement_string, fixed = TRUE)) {
    .Object@isTimeBased <- TRUE
    if ("time" %in% names(OldMapping)) {
      mTimeCol <- NlmeColumnMap(
        variableName = "time",
        columnName = OldMapping[["time"]]@columnName,
        variableType = list(type = "time")
      )
    } else {
      mTimeCol <- NlmeColumnMap(
        variableName = "time",
        columnName = "?",
        variableType = list(type = "time")
      )
    }
  } else {
    .Object@isTimeBased <- FALSE
    mTimeCol <- NULL
  }


  # Dosepoints
  allDosepoints <-
    unlist(regmatches(
      statement_string,
      gregexpr(
        "(?<=dosepoints1 )[ a-zA-Z\\d_]+(?=\\)\\|)",
        statement_string,
        perl = TRUE
      )
    ))
  dosepoints <- c()
  if (length(allDosepoints) > 0) {
    dosepoints <-
      unlist(strsplit(allDosepoints, split = " ", fixed = TRUE))
  }

  allDosepoints2 <-
    unlist(regmatches(
      statement_string,
      gregexpr(
        "(?<=dosepoints2 )[ a-zA-Z\\d_]+(?=\\)\\|)",
        statement_string,
        perl = TRUE
      )
    ))
  dosepoints2 <- c()
  if (length(allDosepoints2) > 0) {
    dosepoints2 <-
      strsplit(allDosepoints2, split = " ", fixed = TRUE)[[1]]
  }

  mDoseCol <- c()
  if (length(dosepoints) > 0) {
    mDoseCol <-
      map_dosepoints(.Object, OldMapping, dosepoints, dosepoints2, dosepointN =
                       1)
  }

  if (length(dosepoints2) > 0) {
    mDoseCol <- c(
      mDoseCol,
      map_dosepoints(.Object, OldMapping, dosepoints, dosepoints2, dosepointN =
                       2)
    )
  }

  .Object@dosePoints <-
    lapply(mDoseCol[unlist(lapply(mDoseCol, function(x)
      x@variableType$type == "dosepoint"))], function(x)
        x@variableName)

  # Covariates
  allCovariates <-
    unlist(regmatches(
      statement_string,
      gregexpr(
        "(?<=covariates )[ a-zA-Z\\d_()]+(?=\\)\\|)",
        statement_string,
        perl = TRUE
      )
    ))

  # stripdose should be handled as a covariate
  StripDoses <- unlist(regmatches(
    statement_string,
    gregexpr(
      "(?<=stripdoses )[ a-zA-Z\\d_()]+(?=\\)\\|)",
      statement_string,
      perl = TRUE
    )
  ))

  allCovariates <- c(allCovariates, StripDoses)

  if (length(allCovariates) > 0) {
    covariates <-
      unlist(strsplit(allCovariates, split = " ", fixed = TRUE))
    listCov_mCovCol <-
      map_covariates(covariates, OldMapping, .Object@covariateList)
    .Object@covariateList <- as.list(listCov_mCovCol$cov)
    mCovCol <- listCov_mCovCol$mCovCol
  } else {
    .Object@covariateList <- list()
    mCovCol <- c()
  }


  # OBSERVATIONS

  mObsCol = c()

  observations <-
    .extract_ResponsesObservations(statement_string, "observations")
  responses <-
    .extract_ResponsesObservations(statement_string, "responses")
  AllObservations <- c(observations, responses)

  for (Observation in unique(AllObservations)) {
    if (Observation %in% names(OldMapping)) {
      columnName <- OldMapping[[Observation]]@columnName
    } else {
      columnName <- "?"
    }

    mObsCol <- c(
      mObsCol,
      NlmeColumnMap(
        variableName = Observation,
        columnName = columnName,
        variableType = list(type = "observation")
      )
    )
  }

  #BQL
  mBqlCol = c()
  object_statements <-
    gsub("[\r\n ]|\\(\\)",
         "" ,
         paste0(.Object@statements, collapse = ''))
  bqlObservations <-
    regmatches(
      object_statements,
      gregexpr(
        "(?<=observe\\()[a-zA-Z\\d_]+(?=[=|,|\\(].*bql(?!=))",
        object_statements,
        perl = TRUE
      )
    )[[1]]

  bqlObservationsNames = c()
  if (length(bqlObservations) > 0) {
    bqlObservationsNames <- paste0(bqlObservations, "BQL")
    for (bqlObservation in bqlObservationsNames) {
      if (bqlObservation %in% names(OldMapping)) {
        columnName <- OldMapping[[bqlObservation]]@columnName
      } else {
        columnName <- "?"
      }

      mBqlCol <- c(
        mBqlCol,
        NlmeColumnMap(
          variableName = bqlObservation,
          columnName = columnName,
          variableType = list(type = "bql")
        )
      )
    }
  }

  if (length(observations) > 0) {
    residuals <- c()
    oldObservationsNames <-
      sapply(.Object@errorModel@effectsList, function(x)
        x@observeName)
    for (observation in observations) {
      bqlFlag <- observation %in% bqlObservations
      if (observation %in% oldObservationsNames) {
        obsIndex <- which(oldObservationsNames %in% observation)
        .Object@errorModel@effectsList[[obsIndex]]@isBQL <-
          bqlFlag
        residuals <- c(residuals,
                       .Object@errorModel@effectsList[[obsIndex]])
      } else {
        # using fake errorType and effectName
        residuals <- c(
          residuals,
          NlmeResidualEffect(
            errorType = ERR_MULTIPLICATIVE,
            effectName = substring(observation, 1, 1),
            observeName = observation,
            frozen = FALSE,
            isBQL = bqlFlag
          )
        )

      }

    }
    .Object@errorModel <- NlmeErrorModel(residuals)
  }

  #ID mapping
  if (!.Object@isPopulation) {
    mIDCol <- NULL
  } else {
    if ("id" %in% names(OldMapping)) {
      columnName <- OldMapping$id@columnName
    } else {
      columnName <- "?"
    }

    mIDCol <-  NlmeColumnMap(
      variableName = "id",
      columnName = columnName,
      variableType = list(type = "id")
    )
  }

  #Mapping with names
  mapping <-
    c(mObsCol, mCovCol, mDoseCol, mBqlCol, mTimeCol, mIDCol)

  if (length(OldMapping$SteadyState) != 0) {
    mapping <- c(mapping, OldMapping$SteadyState)
  }

  if (length(OldMapping$ADDL) != 0) {
    mapping <- c(mapping, OldMapping$ADDL)
  }

  names(mapping) <- sapply(mapping, function(x) {
    x@variableName
  })
  .Object@columnMapping@mapping <- mapping

  # need to update stparms
  st_params <- regmatches(
    statement_string,
    gregexpr("(?<=sparms )[a-zA-Z\\d_ ]+(?=\\))", statement_string,  perl = TRUE)
  )[[1]]

  if (length(st_params) != 0) {
    oldStParmNames <- structuralParameterNames(.Object)
    newStParmNames <- unlist(strsplit(st_params, " "))

    for (newStParmName in newStParmNames) {
      if (newStParmName %in% oldStParmNames)
        next

      .Object@structuralParams <-
        c(
          .Object@structuralParams,
          NlmeStructuralParameter(
            name = newStParmName,
            hasRandomEffect = .Object@isPopulation
          )
        )
    }

  } else {
    .Object@structuralParams <- list()
  }

  # need to update secondaries
  secondaries <- regmatches(
    statement_string,
    gregexpr(
      "(?<=secondary )[a-zA-Z\\d_\\(\\) ]+(?=\\))",
      statement_string,
      perl = TRUE
    )
  )[[1]]

  if (length(secondaries) != 0) {
    # we do not have full secondary parameter definition, so just put the name
    oldSecNames <- secondaryParameterNames(.Object)
    newSecNames <- unlist(strsplit(secondaries,  "\\(\\)\\W*"))

    for (newSecName in newSecNames) {
      if (newSecName %in% oldSecNames) next

      .Object@secondaryParameters <-
        c(
          .Object@secondaryParameters,
          SecondaryParameter(
            name = newStParmName,
            definition = ""
          )
        )
    }

  } else {
    .Object@secondaryParameters <- list()
  }

  return(.Object)
}

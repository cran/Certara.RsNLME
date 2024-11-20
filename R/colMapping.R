#' Add column mappings
#'
#' Piping compatible function for \code{modelColumnMapping} used to add column mappings from input data to model object
#'
#' @param .Object    Model (\code{NlmePmlModel}) object
#' @param mappings   Named character vector specifying valid column names in the input data.
#' Character vector names must be valid model variable names contained
#' in \code{modelVariableNames(model)}.
#' @param ... optional pairs ModelTerm = ColumnName or ModelTerm = "ColumnName".
#' Has higher precedence than \code{mappings} if some ModelTerm is mapped twice in
#' \code{mappings} and in \code{...}. For multiple mapping, i.e. id mapping,
#' a vector should be provided with the names of columns. See example below.
#'
#' @return modified \code{NlmePmlModel} object
#'
#' @examples
#' pkData$id2 <- pkData$Subject
#' model <- pkmodel(columnMap = FALSE,
#'                  data = pkData)
#'
#' modelvar <- unlist(modelVariableNames(model))
#'
#' colnames <- c("Subject", "Act_Time", "Amount", "Conc")
#' names(colnames) <- modelvar
#' # will map subject directly
#' colnames <- colnames[-c(1)]
#'
#' model <- colMapping(model, colnames, id = c(Subject, id2))
#' # also possible:
#' model <- colMapping(model, colnames, id = c("Subject", "id2"))
#' # not recommended since only not quoted names are identified
#' # if both types are provided:
#' model <- colMapping(model, colnames, id = c("Subject", id2))
#'
#' @seealso \code{\link{dataMapping}} \code{\link{modelVariableNames}}
#' @export colMapping
colMapping <- function(.Object, mappings = NULL, ...) {
  stopifnot(inherits(.Object, "NlmePmlModel"))
  stopifnot(inherits(.Object@inputData, "data.frame"))
  if (...length() > 0) {
    dotsPrepared <- .transform_EllipsisToVector(...)
  }

  if (is.null(mappings)) {
    mappings <- dotsPrepared
  } else {
    stopifnot(inherits(mappings, "character"))
    mappingNames <- names(mappings)
    if (any(is.null(mappingNames) |
            is.na(mappingNames) | mappingNames == "")) {
      stop(
        "All arguments provided in mappings should be properly named.\n",
        "Currently the names of mappings are: ",
        paste0("\"", mappingNames, "\"", collapse = ", ")
      )
    }

    multipleIDs <- grepl("id[1-5]", mappingNames)
    if (sum(multipleIDs) > 1 && !"id" %in% mappingNames) {
      # multiple ids found
      id <- paste(mappings[multipleIDs], collapse = ",")
      multipleIDsNames <- mappingNames[multipleIDs]
      mappings <- c(mappings, id = id)
      mappings <- mappings[!names(mappings) %in% multipleIDsNames]
    }

    if (...length() > 0) {
      # merging dots with mappings arg
      namesIntersect <-
        intersect(names(mappings), names(dotsPrepared))
      if (length(namesIntersect) > 0) {
        warning(
          "Some column names are provided twice in mapping vector and in ellipsis: ",
          paste(namesIntersect, collapse = ", "),
          "\nThose column mappings in mapping argument will be ignored."
        )
        mappings <- mappings[!names(mappings) %in% namesIntersect]
      }

      mappings <- c(mappings, dotsPrepared)
    }
  }


  if (any(duplicated(mappings))) {
    warning("Duplicated column names are presented in the mappings:",
            mappings[duplicated(mappings)])
  } else if (any(duplicated(names(mappings)))) {
    warning("Duplicated model terms are presented in the mappings:",
            mappings[duplicated(mappings)])
  }

  notInMapping <-
    names(mappings)[!names(mappings) %in% names(.Object@columnMapping@mapping)]
  if (length(notInMapping) > 0) {
    if (!.Object@isTextual) {
      # the model could be sequential
      SequentialParamsNames <-
        names(formals(pkindirectmodel_MappingParameters))
      SequentialParamsNames <-
        SequentialParamsNames[which(SequentialParamsNames == "nV"):length(SequentialParamsNames)]

      SequentialMappingNames <- intersect(names(mappings), SequentialParamsNames)
      notInMapping <- setdiff(notInMapping, SequentialParamsNames)

      if (length(SequentialMappingNames) > 0) {
        # sequential parameters detected
        SequentialMapping <- mappings[names(mappings) %in% SequentialParamsNames]
        mappings <- mappings[!names(mappings) %in% SequentialParamsNames]
        initRandParamsMapping(model) <- .Object@inputData
        modelRandParamsMapping(.Object) <- SequentialMapping
      }
    }

    if (length(notInMapping) > 0) {
      stop(
        "One or more names specified for mappings do not exist in the model variable names:",
        paste(notInMapping, collapse = " ")
      )
    }
  }

  .Object <- `modelColumnMapping<-`(.Object, mappings)
  .Object
}

.transform_EllipsisToVector <- function(...) {
  if (...length() == 0) {
    stop("No mapping is provided, but columnMap == TRUE")
  }

    dotsNames <- names(match.call(expand.dots = FALSE)$...)
    if (any(is.null(dotsNames) |
            is.na(dotsNames) | dotsNames == "")) {
      stop(
        "All arguments provided in dots should be properly named.\n",
        "Currently the names of additional arguments given are: ",
        paste0("\"", dotsNames, "\"", collapse = ", ")
      )
    }

    dots <- eval(substitute(alist(...), env = environment()))
    dotsPrepared <- sapply(dotsNames, function(ModelTerm, dots) {
      dotsVars <- all.vars(dots[[ModelTerm]])
      if (length(dotsVars) >= 1) {
        # variables are found in dots
        # if there are additional character values, we are ignoring it
        # not sure how to handle this c("ID1", ID2)
        columnToMap <- paste0(dotsVars, collapse = ",")
        if (requireNamespace("rlang", quietly = TRUE)) {
          tryCatch(
            expr = {
              # try to evaluoate the quosure first
              if (rlang::is_quosure(eval(parse(text = dots[[ModelTerm]])))) {
                columnToMap <-
                  rlang::eval_tidy(eval(parse(text = dots[[ModelTerm]])))
              }
            },
            error = function(e) {
              # if evaluation fails, try to extract the name
              try(columnToMap <<-
                    rlang::as_name(eval(parse(text = dots[[ModelTerm]]))),
                  silent = TRUE)
            }
          )
        }

      } else {
        # assuming text values
        if (!inherits(dots[[ModelTerm]], "character")) {
          tryCatch({
            columnToMap <-
              paste0(eval(parse(text = deparse(dots[[ModelTerm]]))),
                     collapse = ",")
          },
          error = function(cond, ModelTerm) {
            stop("Cannot evaluate the column names provided for ",
                 ModelTerm)
          })
        } else {
          columnToMap <- paste0(dots[[ModelTerm]], collapse = ",")
        }

      }
      columnToMap
    },
    dots)

    dotsPrepared
}

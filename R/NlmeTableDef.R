setClassUnion("numeric.character", c("numeric", "character"))

#' Class initializer for NlmeSimTableDef
#'
#' Creates NlmeSimTableDef class object used to specify parameters for VPC/Simulation runs
#'
#' @param  name Name of the generated simulation file.
#' @param  timesList  Numeric; Time values for simulation.
#' Applicable for time-based models only. Ignored when \code{"keepSource=TRUE"}
#' @param  covrSet Character; Vector of covariate names. Simulation point is added
#' when the covariate value is set. See \code{\link{covariateNames}}
#' @param  whenDose Character; Vector of dosing compartment names.
#' Simulation point is added when the dose value is set.
#' @param  whenObs Character; String of observed variables names.
#' Simulation point is added when the observation value is set.
#' @param  variablesList Character; List of variables from the model for simulation.
#' @param  keepSource Logical; Set to \code{TRUE} to keep the number of rows appearing
#' in the table the same as the number of rows in the input dataset.
#' @param  timeAfterDose Set to \code{TRUE} to output time after dose.
#'
#' @export NlmeSimTableDef
#' @keywords internal
NlmeSimTableDef <-
  setClass(
    "NlmeSimTableDef",
    slots = c(
      name = "character",
      timesList = "numeric.character",
      covrSet = "character",
      whenDose = "character",
      whenObs = "character",
      variablesList = "character",
      keepSource = "logical",
      timeAfterDose = "logical"
    )
  )


setMethod("initialize", "NlmeSimTableDef",
          function(.Object,
                   name = "",
                   timesList = numeric(0),
                   covrSet = "",
                   whenDose = "",
                   whenObs = "",
                   variablesList = "",
                   keepSource = FALSE,
                   timeAfterDose = FALSE) {
            .Object@name <- name
            .Object@timesList <- .check_table(timesList, keepSource)
            .Object@covrSet <- .splitString(covrSet)
            .Object@whenDose <- .splitString(whenDose)
            .Object@whenObs <- .splitString(whenObs)
            .Object@variablesList <- .splitString(variablesList)
            .Object@keepSource <- keepSource
            .Object@timeAfterDose <- timeAfterDose

            .Object
          })

#' @export
#' @keywords internal
print.NlmeSimTableDef <- function(x, ...) {
  Names <- .get_NamesNlmeSimTableDefPrint(x)

  NamesTriggersFlags <-
    lapply(slotNames(x), function(snames, x) {
      slot(x, snames)
    },
    x)

  if (length(Names) != length(NamesTriggersFlags)) {
    stop("Not enough names to describe all slots.")
  }

  .print_TableDefProperties(Names, NamesTriggersFlags)

  invisible(x)
}

setMethod(
  "show",
  "NlmeSimTableDef",
  definition = function(object) {
    print(object)
  }
)

#' Class initializer for NlmeTableDef
#'
#' Creates NlmeTableDef class object used to specify parameters
#' for fitting runs
#' @param  name Name of the generated simulation file.
#' @param  timesList  Numeric; Time values for simulation.
#' Applicable for time-based models only. Ignored when \code{"keepSource=TRUE"}
#' @param  covrSet Character; Vector of covariate names. Simulation point is added
#' when the covariate value is set. See \code{\link{covariateNames}}
#' @param  whenDose Character; Vector of dosing compartment names.
#' Simulation point is added when the dose value is set.
#' @param  whenObs Character; String of observed variables names.
#' Simulation point is added when the observation value is set.
#' @param  variablesList Character; List of variables from the model for simulation.
#' @param  keepSource Logical; Set to \code{TRUE} to keep the number of rows appearing
#' in the table the same as the number of rows in the input dataset.
#' @param  timeAfterDose Set to \code{TRUE} to output time after dose.
#' @param  IRES Logical; Set to \code{TRUE} to output individual residuals.
#' Valid only if whenObs is specified.
#' @param  Weight Logical; Set to \code{TRUE} to output the weight of current observation.
#' Valid only if whenObs is specified.
#' @param  IWRES Logical; Set to \code{TRUE} to output individual weighted residuals.
#' Valid only if whenObs is specified.
#' @param  mode Character; The mode of output. Options are \code{"all"} (default),
#' \code{"unique"}, \code{"first"}.
#' Only applicable to non time-based models for the case where only \code{covrSet}
#' is defined or the case where only \code{covrSet} and \code{variablesList} are defined.
#'
#' Option \code{"all"} (default): it outputs all the rows invoked by specified covariates.
#' Option \code{"unique"}: if the values in a row are the same as the ones
#' in the previous row for the current subject, then the row is omitted; otherwise, it is printed out.
#' Option \code{"first"}: it outputs only the first row for each subject.
#'
#' @export NlmeTableDef
#' @keywords internal
NlmeTableDef <-
  setClass(
    "NlmeTableDef",
    slots = c(
      IRES = "logical",
      Weight = "logical",
      IWRES = "logical",
      mode = "character"
    ),
    contains = "NlmeSimTableDef"
  )

setMethod("initialize", "NlmeTableDef",
          function(.Object,
                   name = "",
                   timesList = numeric(0),
                   covrSet = "",
                   whenDose = "",
                   whenObs = "",
                   variablesList = "",
                   keepSource = FALSE,
                   timeAfterDose = FALSE,
                   IRES = FALSE,
                   Weight = FALSE,
                   IWRES = FALSE,
                   mode = "all") {
            .Object@name <- name
            .Object@timesList <- .check_table(timesList, keepSource)
            .Object@timesList <- timesList
            .Object@covrSet <- .splitString(covrSet)
            .Object@whenDose <- .splitString(whenDose)
            .Object@whenObs <- .splitString(whenObs)
            .Object@variablesList <- .splitString(variablesList)
            .Object@keepSource <- keepSource
            .Object@timeAfterDose <- timeAfterDose
            .Object@IRES <- IRES
            .Object@Weight <- Weight
            .Object@IWRES <- IWRES

            if (any(IRES, Weight, IWRES) && whenObs == "") {
              stop(
                "Cannot use special variables IRES, Weight, IWRES ",
                "\nwithout Observable variables specified in 'When observe'"
              )
            }

            if (!missing(mode) && mode != "all" && keepSource) {
              warning("mode is ignored when source keeping is selected", call. = FALSE)
            } else if (!mode %in% c("all", "first", "unique")) {
              stop("current mode: ", mode, " is not supported for tables")
            }

            .Object@mode <- mode

            .Object
          })

#' @export
#' @keywords internal
print.NlmeTableDef <- function(x, ...) {
  Names <- .get_NamesNlmeSimTableDefPrint(x)

  NamesTriggersFlags <-
    lapply(slotNames(x), function(snames, x) {
      slot(x, snames)
    },
    x)
  # due to inheritance need to change the order
  NamesTriggersFlags <- NamesTriggersFlags[c(5:12, 1:4)]

  if (length(Names) != length(NamesTriggersFlags)) {
    stop("Not enough names to describe all slots.")
  }

  .print_TableDefProperties(Names, NamesTriggersFlags)

  invisible(x)
}

setMethod(
  "show",
  "NlmeTableDef",
  definition = function(object) {
    print(object)
  }
)

.splitString <- function(String) {
  if (any(grepl(",", String, fixed = TRUE))) {
    String <- unlist(strsplit(String, "\\W*,\\W*"))
  }

  String <- trimws(String)

  if (length(String) > 1) {
    String <- String[String != "" & !is.null(String)]
  }

  if (length(String) == 0) {
    String <- ""
  }

  String
}

.check_table <- function(timesList, keepSource) {
  if (!missing(timesList) && is.character(timesList)) {
    tryCatch(
      eval(parse(text = paste0(
        "c(", timesList, ")"
      ))),
      error = function(e) {
        stop("current time series for a table:\n",
             timesList,
             "\nis not valid",
             call. = FALSE)
      }
    )
  }

  if (!missing(timesList) && length(timesList) > 0 && keepSource) {
    warning(
      "In keep source mode the specified time points are ignored!",
      call. = FALSE,
      immediate. = TRUE
    )
  }

  timesList
}

.print_TableDefProperties <- function(Names, NamesTriggersFlags) {
  lapply(seq_along(1:length(NamesTriggersFlags)),
         function(x, Names, NamesTriggersFlags) {
           slotValue <- NamesTriggersFlags[[x]]
           if (!all(slotValue == "")) {
             if (all(is.numeric(slotValue))) {
               if (length(slotValue) <= 10) {

               } else if (length(unique(diff(slotValue))) == 1) {
                 slotValue <-
                   paste0("seq(",
                          min(slotValue),
                          ", ",
                          max(slotValue),
                          ", ",
                          unique(diff(slotValue)),
                          ")")
               } else {
                 slotValue <-
                   paste0(c(head(slotValue, 3), "...", tail(slotValue, 3)), collapse = " ")
               }
             }

             cat(sprintf("%-60s:%s\n", Names[x], paste(slotValue, collapse = ", ")))
           }
         },
         Names, NamesTriggersFlags)
}

.get_NamesNlmeSimTableDefPrint <- function(.Object) {
  Names <- c(
    "Name of the output table file",
    "Time values for output",
    "Names of covariates triggering simulation point",
    "Names of dosing compartments triggering simulation point",
    "Names of observed variables triggering simulation point",
    "List of variables for output",
    "Keep the number of rows the same as in the input dataset",
    "Output time after dose"
  )

  if (inherits(.Object, "NlmeTableDef")) {
    Names <- c(Names,
               "Output individual residuals",
               "Output the weight of current observation",
               "Output individual weighted residuals",
               "The mode of output for non time-based models")
  }

  Names
}

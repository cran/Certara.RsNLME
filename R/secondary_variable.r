#' Class represents an NLME secondary parameter
#'
#' Class represents an NLME secondary parameter
#'
#' @param name          Name of the secondary parameter
#' @param definition    Definition of secondary variable
#' @param unit          Optional units
#'
#' @examples
#' \donttest{
#'      param = SecondaryParameter("Spc_Param","log(2)/tvKe")
#'
#'      param = SecondaryParameter("Tmax",
#'                                 "CalcTMax(tvA,tvAlpha,tvB,tvBeta,C,Gamma)")
#'                                 }
#'
#' @keywords internal
SecondaryParameter= setClass("SecondaryParameter",
    slots = c(
        name="character",
        definition="character",
        unit="character")
    )


setMethod("initialize","SecondaryParameter",
    function(.Object,
              name,
              definition,
              unit=""){

        .Object@name=name
        .Object@definition=definition
        .Object@unit=unit
        .Object
    })

#' Prints secondary parameter information
#'
#' Prints secondary parameter information
#'
#' @param x    Secondary parameter
#' @inheritParams ellipsis::dots_used
#'
#' @noRd
#' @keywords internal
print.SecondaryParameter <-function(x, ...)
{
    cat(paste("Name           : ", x@name), "\n")
    cat(paste("Definition     :", x@definition), "\n")
    cat(paste("Unit           :", x@unit), "\n")
}


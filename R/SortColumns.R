#' Class initializer for SortColumns
#'
#' Class represents an NLME sort columns object
#'
#' @param ...   Names of input data columns
#' (up to 5 for individual models and no limit for population models)
#' used to sort the input data and model outputs. Can be supplied
#' as either a single string or  a vector of strings.
#' @export SortColumns
#' @examples
#' # The following two setups are equivalent
#' sortColumnSetUp <- SortColumns("Country,City")
#' sortColumnSetUp <- SortColumns(c("Country","City"))
#'
#' # The following two setups are equivalent
#' sortColumnSetUp <- SortColumns("Sort1 Sort2", "Sort3")
#' sortColumnSetUp <- SortColumns(c("Sort1, Sort2 "), "Sort3 ")
#' @keywords internal
SortColumns <-
  setClass("SortColumns",
           slots = c(numSortColumns = "numeric",
                     sortColumnList = "character"))

setMethod("initialize", "SortColumns",
          function(.Object, ...) {
            ColumnNamesUncleaned <-
              unique(as.character(unlist(strsplit(
                paste(...), split = "\\W+"
              ))))
            ColumnNames <-
              ColumnNamesUncleaned[!is.na(ColumnNamesUncleaned) |
                                     !is.null(ColumnNamesUncleaned) |
                                     ColumnNamesUncleaned == ""]
            .Object@sortColumnList <- ColumnNames
            .Object@numSortColumns <- length(ColumnNames)
            .Object
          })

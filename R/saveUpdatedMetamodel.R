
#' workhorse for change_ThetasMmdlin RsNLME.ModelBuilder
#'
#' @param mmdl_withComments the metamodel text to be substituted
#' @param mmdl_model the old model from mmdl_withComments
#' @param resmodel the model to be pasted
#' @param metamodelFile the name of initial file with metamodel
#' to be overwritten
#'
#' @return text of new metamodel
#' @export
#' @keywords internal
saveUpdatedMetamodel <- function(mmdl_withComments,
                                 mmdl_model,
                                 resmodel,
                                 metamodelFile,
                                 updateModel = TRUE,
                                 updatedBasedOn = "",
                                 updatedEstArgsBlock = NULL,
                                 updatedTablesBlock = NULL) {
  blocks <- c("Author:", "Description:", "Based on:",
              "DATA1", "MAP1", "DATA ", "MAP ",
              "COLDEF", "MODEL",
              "ESTARGS", "TABLES", "RSNLMEDATA")
  # splitting to the blocks
  split <- paste0("\\s*", blocks, collapse = "|")

  mmdl_blocks <-
    unlist(strsplit(mmdl_withComments,
                    split = paste0("(?i)##(?=", split, ")"),
                    perl = TRUE))
  mmdl_blocks <- mmdl_blocks[mmdl_blocks != ""]


  blockNameParsed <- c()
  for(blockIndex in seq_along(mmdl_blocks)) {

    blockFlag <- sapply(blocks,
                        function(x, mmdl_block) {
                          return(grepl(paste0("^\\s*", x), mmdl_block, ignore.case = TRUE))
                        },
                        mmdl_blocks[blockIndex])
    if(any(blockFlag)) {
      blockNameParsed[blockIndex] <- blocks[blockFlag]
    } else {
      blockNameParsed[blockIndex] <- ""
    }

  }

  if (!"Based on:" %in% blockNameParsed && updatedBasedOn != "") {
    mmdl_blocks <- c(" Based on:", mmdl_blocks)
    blockNameParsed <- c("Based on:", blockNameParsed)
  }
  if (!"ESTARGS" %in% blockNameParsed && !is.null(updatedEstArgsBlock)) {
    mmdl_blocks <- c(mmdl_blocks, " ESTARGS")
    blockNameParsed <- c(blockNameParsed, "ESTARGS")
  }
  if (!"TABLES" %in% blockNameParsed && !is.null(updatedTablesBlock)) {
    mmdl_blocks <- c(mmdl_blocks, " TABLES")
    blockNameParsed <- c(blockNameParsed, "TABLES")
  }

  newMMDL <- character(0)
  OSCR <- ifelse(.Platform$OS.type == "windows", "\r\n", "\n")
  for(blockIndex in seq_along(mmdl_blocks)) {
    if (blockNameParsed[blockIndex] == "MODEL" && updateModel) {
      statementsWOEnds <- sapply(unlist(resmodel@statements), function(x) {
        if(!grepl("\\n$", x)) {
          x <- paste0(x, OSCR, collapse = "")
        }
        x
      }, USE.NAMES = FALSE)
      statements <- paste0(statementsWOEnds, collapse = "")

      if(!identical(statements, mmdl_model$model@statements)) {
        # there are changes made
        newMMDL <- paste0(newMMDL, "## MODEL ", statements)
      } else {
        # keep it as is
        newMMDL <- paste0(newMMDL, mmdl_blocks[[blockIndex]])
      }
    } else if (blockNameParsed[blockIndex] == "Based on:" && updatedBasedOn != "") {

      newMMDL <- paste0(newMMDL, "## Based on: ", updatedBasedOn)

    } else if (blockNameParsed[blockIndex] == "ESTARGS" && !is.null(updatedEstArgsBlock)) {

      newMMDL <- paste0(newMMDL, "## ESTARGS", updatedEstArgsBlock)

    } else if (blockNameParsed[blockIndex] == "TABLES" && !is.null(updatedTablesBlock)) {

      newMMDL <- paste0(newMMDL, "## TABLES", updatedTablesBlock)

    } else {
      # keep it as is
      newMMDL <- paste0(newMMDL, "##", mmdl_blocks[[blockIndex]])
    }

    if(!grepl("\\n$", newMMDL)) {
      newMMDL <- paste0(newMMDL, OSCR)
    }
  }

  writeChar(newMMDL, con = metamodelFile, eos = NULL)

  newMMDL
}

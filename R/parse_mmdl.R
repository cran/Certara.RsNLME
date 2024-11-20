.extract_mmdlblock <- function(mmdl_blocks,
                               key_name,
                               mmdlfile,
                               mustexist = FALSE,
                               onerow = TRUE,
                               removeComments = TRUE,
                               multipleBlocks = FALSE) {
  mmdlBlockIndx <-
    grepl(paste0("^(", key_name, ")"), toupper(mmdl_blocks))
  if (sum(mmdlBlockIndx) == 0) {
    if (mustexist) {
      stop(paste(key_name, "key not found in the file", mmdlfile),
           call. = FALSE)
    } else {
      mmdlBlock <- ""
    }
  } else {
    if (sum(mmdlBlockIndx) > 1 && !multipleBlocks) {
      stop(paste("More than one", key_name, "key found in the file", mmdlfile),
           call. = FALSE)
    }

    mmdlBlock <- substring(mmdl_blocks[mmdlBlockIndx],
                           first = nchar(key_name) + 1)

    if (removeComments) {
      # do not collapse multiple blocks
      if (multipleBlocks) {
        collapse = NULL
      } else {
        collapse = ""
      }

      mmdlBlock <-
        .remove_comments(
          mmdlBlock,
          removeWhites = FALSE,
          type = "All",
          collapse = collapse
        )
    }

    if (onerow) {
      mmdlBlock <- trimws(gsub("\r?\n|\r", " ", mmdlBlock))
    }

  }
  mmdlBlock
}

.prepare_datafilepath <- function(DATA_block, directory) {
  # to normalizePath we should have one type of slashes
  DATA_block <- gsub("\\", "/", DATA_block, fixed = TRUE)
  DATA_file <-
    normalizePath(file.path(directory, DATA_block),
                  winslash = "/",
                  mustWork = FALSE)

  if (!file.exists(DATA_file)) {
    # the DATA1 file is not a simple file name nor relative path
    # the other option is full path provided
    if (file.exists(DATA_block)) {
      DATA_file <-
        normalizePath(DATA_block,
                      winslash = "/",
                      mustWork = TRUE)
    } else {
      stop(
        paste(
          "Cannot find the file specified as DATA1:\n",
          DATA_block,
          "\n in the directory \n",
          directory
        )
      )
    }
  }
  DATA_file
}

.find_covLabelsPairs <-
  function(covariateLabelsInfo,
           col_terms,
           model_terms) {
    if (length(covariateLabelsInfo[[1]]) == 0) {
      return(list())
    }

    label_terms <- vector(mode = "list", length = length(col_terms))
    names(label_terms) <- col_terms
    for (covariateLabel in covariateLabelsInfo[[1]]) {
      if (length(covariateLabel) == 0)
        next()
      # name of covariate to be bound
      colName <-
        unlist(regmatches(
          covariateLabel,
          gregexpr("^[[:word:]]+", covariateLabel, perl = TRUE)
        ))
      covName <- model_terms[which(colName == col_terms)]
      # extract the labels from brackets
      labelsInfo <-
        unlist(regmatches(
          covariateLabel,
          gregexpr("(?<=\\()[^()]+(?=\\))", covariateLabel, perl = TRUE)
        ))
      # retain white and = as separators only
      labelsInfoWhitesep <-
        gsub('(\\s*[^a-zA-Z0-9_\\.\\=\\"\\-]+\\s*)|(\\s+)',
             " ",
             trimws(labelsInfo))

      # separate by space but keeping spaces in quotes
      labelsInfoSplittedByW <-
        unlist(regmatches(
          labelsInfoWhitesep,
          gregexpr(
            "[^\\s\"]+=\\d+|\"([^\"]+)\"=\\d+",
            labelsInfoWhitesep,
            perl = TRUE
          )
        ))
      labelsInfoSplittedByEq <-
        strsplit(labelsInfoSplittedByW, split = "=")

      labels <- sapply(labelsInfoSplittedByEq,
                       function(x, covariateLabel) {
                         if (length(x) != 2) {
                           stop("Cannot parse the current covariate label: ",
                                covariateLabel)
                         }
                         x[1]
                       }, covariateLabel)
      labels <- gsub("[\'\"]", "", labels)
      values <- sapply(labelsInfoSplittedByEq, function(x) {
        x[2]
      })
      if (any(!.is_numeric(values))) {
        stop(
          "cannot use current covariate label since there are non-numeric values assigned: ",
          covariateLabel
        )
      }
      values <- as.numeric(values)
      if (any(is.na(values))) {
        stop(
          "Cannot convert to numeric values assigned to labels in the current statement:",
          covariateLabel
        )
      }
      names(values) <- labels
      label_terms[[covName]] <- values
    }

    label_terms
  }

.find_col_terms <- function(model_col_term) {
  if (length(model_col_term) > 2) {
    stop(paste(
      "Please check the mapping row, cannot resolve terms",
      paste0(model_col_term, collapse = "=")
    ))
  } else if (length(model_col_term) == 2) {
    return(model_col_term[2])
  } else {
    return(model_col_term)
  }
}

.find_map_pairs <- function(MAP_string) {
  MAP_ws <- gsub("(\\s*=\\s*)", "=", trimws(MAP_string))
  # extract covariate labels info if any
  covariateLabelsInfo <-
    regmatches(MAP_ws,
               gregexpr("[[:word:]]+\\s*\\([^()]+\\)", MAP_ws, perl = TRUE))
  # remove covariate lables info
  MAP_ws_woLabels <- gsub("\\([^()]+\\)", "", MAP_ws)
  MAP_ws_whitesep <-
    gsub('(\\s*[^a-zA-Z0-9_\\.\\=\\"\\-]+\\s*)|(\\s+)',
         " ",
         trimws(MAP_ws_woLabels))
  MAPvec <- unlist(strsplit(MAP_ws_whitesep, split = " "))
  model_cols_terms <- strsplit(MAPvec, split = "=")
  model_terms <- unlist(lapply(model_cols_terms, function(x) {
    x[1]
  }))
  col_terms <- unlist(lapply(model_cols_terms,
                             .find_col_terms))
  covLabels_terms <-
    .find_covLabelsPairs(covariateLabelsInfo, col_terms, model_terms)
  return(
    list(
      model_terms = model_terms,
      col_terms = col_terms,
      covLabels_terms = covLabels_terms
    )
  )
}

.get_datafromfile <- function(DATA1_file, MAP1_list) {
  NOHEADER_index <-
    grepl("_NOHEADER", MAP1_list$model_terms, fixed = TRUE)
  # if there's a flag _NOHEADER, MAP should have the column names
  # no header in data is assumed and the column names are used from MAP row
  if (sum(NOHEADER_index) > 0) {
    NOHEADERinDATA <- TRUE
    MAP1_list <- lapply(MAP1_list,
                        function(x, NOHEADER_index) {
                          x <- x[!NOHEADER_index]
                        },
                        NOHEADER_index)
  } else {
    NOHEADERinDATA <- FALSE
  }

  MAPFROMMODEL_index <-
    grepl("_MAPFROMMODEL", MAP1_list$model_terms, fixed = TRUE)
  # if there's a flag _MAPFROMMODEL, _NOHEADER is not acceptable
  # we are using automatic matching between terms and column names
  if (sum(MAPFROMMODEL_index) > 0) {
    MAPFROMMODEL <- TRUE
    MAP1_list <- lapply(MAP1_list,
                        function(x, MAPFROMMODEL_index) {
                          x <- x[!MAPFROMMODEL_index]
                        },
                        MAPFROMMODEL_index)
  } else {
    MAPFROMMODEL <- FALSE
  }


  if (requireNamespace("data.table", quietly = TRUE)) {
    DATA1 <- data.table::fread(
      file = DATA1_file,
      header = !NOHEADERinDATA,
      data.table = FALSE
    )

  } else {
    # figure out the end of comments line
    commentsEnded <- 0L

    while (grepl(
      "^#",
      scan(
        DATA1_file,
        what = "character",
        sep = '\n',
        quiet = TRUE,
        skip = commentsEnded,
        nlines = 1
      )
    )) {
      commentsEnded <- commentsEnded + 1L
    }

    firstDataLine <- scan(
      DATA1_file,
      what = "character",
      sep = '\n',
      quiet = TRUE,
      skip = commentsEnded,
      nlines = 1
    )
    if (isTRUE(grep(",", firstDataLine, fixed = TRUE))) {
      sep = ","
    } else {
      sep = " "
    }

    DATA1 <- utils::read.table(
      DATA1_file,
      comment.char = '#',
      sep = sep,
      header = !NOHEADERinDATA
    )
  }
  if (NOHEADERinDATA) {
    if (ncol(DATA1) > length(MAP1_list$col_terms)) {
      warning(
        "The number of columns in DATA1 is ",
        ncol(DATA1),
        "\nMAP1 has ",
        length(MAP1_list$col_terms),
        " terms.",
        "\nlast ",
        ncol(DATA1) - length(MAP1_list$col_terms),
        " columns",
        "will be ignored"
      )
      DATA1 <- DATA1[, 1:length(MAP1_list$col_terms)]
    } else if (ncol(DATA1) < length(MAP1_list$col_terms)) {
      stop(
        "The number of columns in DATA1 is ",
        ncol(DATA1),
        "\nMAP1 has ",
        length(MAP1_list$col_terms),
        " terms."
      )
    }

    colnames(DATA1) <-
      MAP1_list$col_terms
  }

  # we should know during the mapping is the automatic mapping allowed or not
  MAP1_list$MAPFROMMODEL <- MAPFROMMODEL
  list(DATA1 = DATA1, MAP1_list = MAP1_list)
}

#' Parse Metamodel file
#'
#' Parses the metamodel file to separate mmdl blocks.
#'
#' @param mmdlfile File with metamodel description.
#' @param directory An optional directory where data file is looked for; if not
#'   given, it is assumed to be in the current working directory
#'
#' @details See
#' \href{https://certara.github.io/R-RsNLME/articles/metamodels_overview.html#metamodel-overview}{Metamodel
#' documentation}#'
#'
#' @return A list returned with the following elements: model_name, DATA1,
#' MAP1_list, DATA2, MAP2_list, COLDEF_block, MODEL_block, ESTARGS_block,
#' TABLES_BLOCK, DATARSNLME_BLOCK
#'
#' @keywords internal
#' @noRd
parse_metamodel <- function(mmdlfile, directory) {
  if (missing(mmdlfile)) {
    stop("`mmdlfile` argument should be specified.")
  }
  if (!missing(directory)) {
    if (dirname(mmdlfile) != ".") {
      warning("`mmdlfile argument is given with a path:", mmdlfile,
              "\n`directory` argument will be ignored.")
      directory <- normalizePath(dirname(mmdlfile))
    } else {
      mmdlfile <- file.path(directory, mmdlfile)
    }
  } else {
    directory <- normalizePath(dirname(mmdlfile))
  }

  if (!file.exists(mmdlfile)) {
    stop(paste("File ", mmdlfile, "not found."))
  }

  mmdl_withComments <-
    rawToChar(readBin(mmdlfile, "raw", n = 30000))
  # remove /**/ comments since they could be multiline
  mmdl <-
    .remove_comments(mmdl_withComments,
                     type = "Asterisk",
                     removeWhites = FALSE)
  blocks <- c(
    "Author:",
    "Description:",
    "Based on:",
    "DATA1",
    "MAP1",
    "DATA",
    "MAP",
    "COLDEF",
    "MODEL",
    "DOSING CYCLE",
    "ESTARGS",
    "SIMARGS",
    "TABLES",
    "RSNLMEDATA"
  )
  blocks <- paste0(blocks, "\\s+")
  # splitting to the blocks
  splitCaseInsensitive <-
    paste0("\\s*", blocks[1:3], collapse = "|")
  splitCaseSensitive <-
    paste0("\\s*", blocks[4:length(blocks)], collapse = "|")
  # case insensitive
  mmdl_blocks <-
    trimws(unlist(strsplit(
      mmdl,
      split = paste0("(?i)##(?=", splitCaseInsensitive, ")"),
      perl = TRUE
    )))
  # case sensitive
  mmdl_blocks <-
    trimws(unlist(strsplit(
      mmdl_blocks,
      split = paste0("##(?=", splitCaseSensitive, ")"),
      perl = TRUE
    )))

  # parsing first data file path
  DATA1_block <-
    .extract_mmdlblock(
      mmdl_blocks,
      "DATA1",
      mmdlfile,
      mustexist = FALSE,
      onerow = TRUE,
      removeComments = TRUE
    )

  if (DATA1_block == "") {
    # DATA1 does not exist; the DATA should exist but MAP could, but may be
    # set in COLDEF
    DATA1_block <-
      .extract_mmdlblock(
        mmdl_blocks,
        "DATA ",
        mmdlfile,
        mustexist = TRUE,
        onerow = TRUE,
        removeComments = TRUE
      )
    MAP1_block <-
      .extract_mmdlblock(
        mmdl_blocks,
        "MAP ",
        mmdlfile,
        mustexist = FALSE,
        onerow = TRUE,
        removeComments = TRUE
      )

    DATA2 <- NA
    MAP2_list <- NA
  } else {
    # ignore DATA if DATA1 is given and look for DATA2
    MAP1_block <-
      .extract_mmdlblock(
        mmdl_blocks,
        "MAP1",
        mmdlfile,
        mustexist = FALSE,
        onerow = TRUE,
        removeComments = TRUE
      )

    # for now we are parsing data2 but do nothing since it is not implemented
    # in the model class
    DATA2_block <-
      .extract_mmdlblock(
        mmdl_blocks,
        "DATA2",
        mmdlfile,
        mustexist = FALSE,
        onerow = TRUE,
        removeComments = TRUE
      )

    if (DATA2_block == "") {
      DATA2 <- NA
      MAP2_list <- NA
    } else {
      DATA2_file <- .prepare_datafilepath(DATA2_block, directory)
      DATA2 <- read.csv(DATA2_file)

      MAP2_block <-
        .extract_mmdlblock(
          mmdl_blocks,
          "MAP2",
          mmdlfile,
          mustexist = FALSE,
          onerow = TRUE,
          removeComments = TRUE
        )

      MAP2_list <- .find_map_pairs(MAP2_block)
    }
  }

  DATA1_file <- .prepare_datafilepath(DATA1_block, directory)
  MAP1_list <- .find_map_pairs(MAP1_block)

  DATA1_MAP1_list <-
    .get_datafromfile(DATA1_file, MAP1_list)

  DATA1 <- DATA1_MAP1_list$DATA1
  MAP1_list <- DATA1_MAP1_list$MAP1_list

  DOSINGCYCLE_block <-
    .extract_mmdlblock(
      mmdl_blocks,
      "DOSING CYCLE",
      mmdlfile,
      mustexist = FALSE,
      onerow = FALSE,
      removeComments = TRUE
    )

  COLDEF_block <-
    .extract_mmdlblock(
      mmdl_blocks,
      "COLDEF",
      mmdlfile,
      mustexist = FALSE,
      onerow = FALSE,
      removeComments = TRUE
    )

  MODEL_block <-
    .extract_mmdlblock(
      mmdl_blocks,
      "MODEL",
      mmdlfile,
      mustexist = TRUE,
      onerow = FALSE,
      removeComments = FALSE
    )


  ESTARGS_block <-
    .extract_mmdlblock(
      mmdl_blocks,
      "ESTARGS",
      mmdlfile,
      mustexist = FALSE,
      onerow = TRUE,
      removeComments = TRUE,
      multipleBlocks = TRUE
    )

  SIMARGS_block <-
    .extract_mmdlblock(
      mmdl_blocks,
      "SIMARGS",
      mmdlfile,
      mustexist = FALSE,
      onerow = TRUE,
      removeComments = TRUE,
      multipleBlocks = TRUE
    )

  TABLES_BLOCK <-
    .extract_mmdlblock(
      mmdl_blocks,
      "TABLES",
      mmdlfile,
      mustexist = FALSE,
      onerow = FALSE,
      removeComments = TRUE
    )

  DATARSNLME_BLOCK <-
    .extract_mmdlblock(
      mmdl_blocks,
      "RSNLMEDATA",
      mmdlfile,
      mustexist = FALSE,
      onerow = FALSE,
      removeComments = FALSE
    )

  model_name <- tools::file_path_sans_ext(basename(mmdlfile))

  list(
    model_name = model_name,
    DATA1 = DATA1,
    MAP1_list = MAP1_list,
    DATA2 = DATA2,
    MAP2_list = MAP2_list,
    DOSINGCYCLE_block = DOSINGCYCLE_block,
    COLDEF_block = COLDEF_block,
    MODEL_block = MODEL_block,
    ESTARGS_block = ESTARGS_block,
    SIMARGS_block = SIMARGS_block,
    TABLES_BLOCK = TABLES_BLOCK,
    DATARSNLME_BLOCK = DATARSNLME_BLOCK
  )
}

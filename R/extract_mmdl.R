#' extract files used for powershell script
#'
#' Use for extraction of the NLME files from mmdl file.
#'
#' @param mmdlfile The metamodel file path; relative paths are acceptable.
#' @param directoryToExtract The directory where the files should be stored
#' If `missing`, current working directory is used.
#' @param dataFileName the name of the data file
#' If `missing`, the default file name 'data1.txt' is used to prepare
#' `nlmeargsOutput` file
#' @param mdlOutput the name of the file to output PML code
#' If `missing`, the default file name 'test.mdl' is used.
#' @param cols1Output the name of the file to output columns defintion
#' If `missing`, the default file name 'cols1.txt' is used.
#' @param nlmeargsOutput the name of the file to output engine parameters
#' If `missing`, the default file name 'nlmeargs.txt' is used.
#'
#' @return
#' The results of [create_model_from_metamodel()] run are returned.
#'
#' @details
#' `mdlOutput`, `dataFileName`, `cols1Output`, `nlmeargsOutput` files are
#' extracted into the folders, the names of the folders are built as
#' `{Number of estimation/simulation block in metamodel}`-
#' `{'est' for estimation block/'sim' for simulation block}`. All estimation blocks are going first
#' irrespective of the simulation blocks presence, all simulation blocks
#' are going next.
#'
#' @examples
#' \dontrun{
#' # path to metamodel should be specified, all other arguments set to default
#' extract_mmdl(system.file("extdata/mmdlNoTime", "test.mmdl",
#'                               package = "Certara.RsNLME"),
#'              directoryToExtract = tempdir())
#' }
#'
#' @seealso [create_model_from_metamodel()]
#'
#' @md
#' @export
#' @keywords internal
extract_mmdl <-
  function(mmdlfile,
           directoryToExtract,
           dataFileName = "data1.txt",
           mdlOutput = "test.mdl",
           cols1Output = "cols1.txt",
           nlmeargsOutput = "nlmeargs.txt") {
    if (!file.exists(mmdlfile)) {
      stop("mmdl file is not found:", mmdlfile)
    }

    mmdlfileText <- readLines(mmdlfile)
    if (any(grepl("{data_dir}", mmdlfileText, fixed = TRUE))) {
      mmdlfileText <-
        gsub("{data_dir}", dirname(mmdlfile), mmdlfileText, fixed = TRUE)
      writeLines(mmdlfileText, mmdlfile)
    }

    if (missing(directoryToExtract)) {
      directoryToExtract <- normalizePath(".", winslash = "/", mustWork = FALSE)
    } else {
      directoryToExtract <- .prepare_wd(directoryToExtract)
    }

    fitmodel_args <-
      create_model_from_metamodel(mmdlfile = mmdlfile,
                                  directoryToRun = directoryToExtract)

    fitmodel_args$model@dataset@dataFile <- dataFileName
    fitmodel_args$model@dataset@colDefFile <- cols1Output
    fitmodel_args$model@dataset@engineParamsFile <- nlmeargsOutput


    if (!is.list(fitmodel_args$params)) {
      fitmodel_args$params <- list(fitmodel_args$params)
    }

    for (ParamsIndex in seq_along(fitmodel_args$params)) {
      params <- fitmodel_args$params[[ParamsIndex]]
      if (inherits(params, "NlmeEngineExtraParams")) {
        workingDir <-
          file.path(directoryToExtract, paste0(ParamsIndex, "-est"))
      } else if (inherits(params, "simParamsBlock")) {
        workingDir <-
          file.path(directoryToExtract, paste0(ParamsIndex, "-sim"))
      } else {
        warning(
          "Cannot write engine parameters file.\nPlease check the template file."
        )
        next
      }

      if (!dir.exists(workingDir) &&
          !dir.create(workingDir, showWarnings = FALSE)) {
        stop("Cannot create ", workingDir)
      }

      # test.mdl
      writeModelStatements(model = fitmodel_args$model,
                           filename = mdlOutput,
                           workingDir = workingDir)

      # cols1.txt
      writeColumnMapping(model = fitmodel_args$model,
                         filename = cols1Output,
                         workingDir = workingDir)

      # data1.txt
      writeInputData(
        model = fitmodel_args$model,
        datafileName = dataFileName,
        workingDir = workingDir
      )

      if (inherits(params, "NlmeEngineExtraParams")) {
        GenerateParamsfile(
          argsFilename = file.path(workingDir, nlmeargsOutput),
          dataset = fitmodel_args$model@dataset,
          params = params
        )
      } else if (inherits(params, "simParamsBlock")) {
        outputLine <-
          paste(output(params),
                "-d1",
                basename(cols1Output),
                basename(dataFileName))
        writeLines(outputLine, con = file.path(workingDir, nlmeargsOutput))
      }
    }


    return(fitmodel_args)
  }

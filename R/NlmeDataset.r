
#' NLME dataset object
#'
#' Class represents an NLME dataset object
#'
#' @param dataFile            Subject datafile
#' @param colDefFile          Datafile to model mapping file
#' @param modelFile           PML model file
#' @param estimatesDefFile    Initial estimates mapping file
#' @param estimatesDataFile   Initial estimates values
#' @param doseDefFile         Dose column definition file
#' @param doseDataFile        Dose datafile
#' @param ranEffectDefFile    Random effects column definition file
#' @param ranEffectDataFile   Random effects data file
#' @param predoutFilename     Name of predcheck output file
#' @param phoenixSourceDir    Directory containing phoenix generated files
#' @param workingDir          Directory containing datafiles(default cwd)
#' @export NlmeDataset
#' @examples
#' dataset <- NlmeDataset(workingDir)
#' @keywords internal
NlmeDataset <- setClass("NlmeDataset", representation(
  dataFile = "character",
  colDefFile = "character",
  modelFile = "character",
  estimatesDefFile = "character",
  estimatesDataFile = "character",
  doseDefFile = "character",
  doseDataFile = "character",
  ranEffectDefFile = "character",
  ranEffectDataFile = "character",
  predoutFilename = "character",
  simoutFilename = "character",
  phoenixSourceDir = "character",
  model = "ANY",
  engineParamsFile = "character"
))

setMethod("initialize", "NlmeDataset",
  function(.Object,
           workingDir,
           dataFile = "data1.txt",
           colDefFile = "cols1.txt",
           modelFile = "test.mdl",
           estimatesDefFile = "", # cols3.txt
           estimatesDataFile = "",
           doseDefFile = "", # cols2.txt
           doseDataFile = "",
           ranEffectDefFile = "", # cols4.txt
           ranEffectDataFile = "",
           predoutFilename = "predout.csv",
           simoutFilename = "simout.csv",
           phoenixSourceDir = "",
           model = NULL,
           engineParamsFile = "nlmeargs.txt") {


    .Object@phoenixSourceDir <- phoenixSourceDir

    if (dir.exists(phoenixSourceDir)) {
      if (doseDefFile == "" && doseDataFile == "") {
        if (file.exists(file.path(phoenixSourceDir, "cols2.txt")) &&
          file.exists(file.path(phoenixSourceDir, "data2.txt"))) {
          .Object@doseDefFile <- "cols2.txt"
          .Object@doseDataFile <- "data2.txt"
        }
      }
      if (estimatesDefFile == "" && estimatesDataFile == "") {
        if (file.exists(file.path(phoenixSourceDir, "cols3.txt")) &&
          file.exists(file.path(phoenixSourceDir, "data3.txt"))) {
          .Object@estimatesDefFile <- "cols3.txt"
          .Object@estimatesDataFile <- "data3.txt"
        }
      }
      if (ranEffectDefFile == "" && ranEffectDataFile == "") {
        if (file.exists(file.path(phoenixSourceDir, "cols4.txt")) &&
          file.exists(file.path(phoenixSourceDir, "data4.txt"))) {
          .Object@ranEffectDefFile <- "cols4.txt"
          .Object@ranEffectDataFile <- "data4.txt"
        }
      }
    }

    .Object@dataFile <- dataFile
    .Object@colDefFile <- colDefFile
    .Object@engineParamsFile <- engineParamsFile
    .Object@modelFile <- modelFile
    .Object@predoutFilename <- predoutFilename
    .Object@simoutFilename <- simoutFilename
    .Object@doseDefFile <- doseDefFile
    .Object@doseDataFile <- doseDataFile
    .Object@estimatesDefFile <- estimatesDefFile
    .Object@estimatesDataFile <- estimatesDataFile
    .Object@ranEffectDefFile <- ranEffectDefFile
    .Object@ranEffectDataFile <- ranEffectDataFile

    .Object
  }
)


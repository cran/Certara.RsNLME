test_that("Mapping for built-in models works ", {
  workingDir <- tempdir(TRUE)

  pkData$ID2 <- pkData$Subject
  model <- pkmodel(
    parameterization = "Clearance",
    numCompartments = 3,
    data = pkData,
    ID = c(Subject, ID2),
    Time = "Act_Time",
    A1 = Amount,
    CObs = "Conc",
    workingDir = workingDir,
    modelName = "pkmodel"
  )

  testthat::local_edition(3)
  # exclude working directory
  PrintModelOutput <- capture.output(print(model))[-c(5)]
  # in non-interactive mode the table is not returned
  testthat::expect_snapshot_value(as.list(PrintModelOutput), style = "json")

  model <-
    linearmodel(
      type = "Linear",
      data = pkpdData,
      ID = "ID",
      C = "CObs",
      EObs = EObs,
      workingDir = workingDir,
      modelName = "linearmodel"
    )

  # exclude working directory
  PrintModelOutput <- capture.output(print(model))[-c(5)]
  # in non-interactive mode the table is not returned
  testthat::expect_snapshot_value(as.list(PrintModelOutput), style = "json")

  model <- emaxmodel(
    checkBaseline = TRUE,
    checkFractional = TRUE,
    checkInhibitory = TRUE,
    data = pkpdData,
    ID = "ID",
    C = "CObs",
    EObs = "EObs",
    workingDir = workingDir,
    modelName = "emaxmodel"
  )

  # exclude working directory
  PrintModelOutput <- capture.output(print(model))[-c(5)]
  # in non-interactive mode the table is not returned
  testthat::expect_snapshot_value(as.list(PrintModelOutput), style = "json")

  pkpdData$nV <- 1
  pkpdData$nCl <- 1
  testthat::expect_warning(
    model <- pklinearmodel(
      parameterization = "Clearance",
      linearType = "Constant",
      data = pkpdData,
      ID = "ID",
      Time = "Time",
      A1 = "Dose",
      CObs = "CObs",
      EObs = "EObs",
      nV = nV,
      nCl = nCl,
      workingDir = workingDir,
      modelName = "pklinearmodel",
      isSequential = TRUE
    )
  )

  pkpdData$nKe <- 1
  model <- pklinearmodel(
    parameterization = "Micro",
    linearType = "Constant",
    data = pkpdData,
    ID = "ID",
    Time = "Time",
    A1 = "Dose",
    EObs = "EObs",
    nV = nV,
    nKe = nKe,
    workingDir = workingDir,
    modelName = "pklinearmodel",
    isSequential = TRUE
  )

  # exclude working directory
  PrintModelOutput <- capture.output(print(model))[-c(5)]
  # in non-interactive mode the table is not returned
  testthat::expect_snapshot_value(as.list(PrintModelOutput), style = "json")

  pkData$nMeanDelayTime <- 1
  pkData$nShapeParamMinusOne <- 1
  pkData$nKe <- 1
  pkData$nK12 <- 1
  pkData$nK21 <- 1
  pkData$nK13 <- 1
  pkData$nK31 <- 1
  pkData$nV <- 1
  model <-
    pkemaxmodel(
      isPopulation = TRUE,
      parameterization = "Micro",
      absorption = "Gamma",
      numCompartments = 3,
      isClosedForm = FALSE,
      isTlag = FALSE,
      hasEliminationComp = TRUE,
      isFractionExcreted = FALSE,
      isSaturating = FALSE,
      isSequential = TRUE,
      infusionAllowed = FALSE,
      isDuration = FALSE,
      isPkFrozen = FALSE,
      hasEffectsCompartment = FALSE,
      checkBaseline = TRUE,
      checkFractional = TRUE,
      checkInhibitory = TRUE,
      checkSigmoid = TRUE,
      isEmaxFrozen = FALSE,
      id = "Subject",
      time = "Nom_Time",
      A0Obs = "Gender",
      EObs = "Age",
      # CObs = "Conc",
      A1 = "Amount",
      nMeanDelayTime = nMeanDelayTime,
      nShapeParamMinusOne = nShapeParamMinusOne,
      nV = nV,
      nKe = nKe,
      nK12 = nK12,
      nK21 = nK21,
      nK13 = nK13,
      nK31 = nK31,
      data = pkData,
      modelName = "Sequential"
    )

  # exclude working directory
  PrintModelOutput <- capture.output(print(model))[-c(5)]
  # in non-interactive mode the table is not returned
  testthat::expect_snapshot_value(as.list(PrintModelOutput), style = "json")
})

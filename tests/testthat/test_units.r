test_that("class running a simple estimation to check units", {
  testthat::skip_on_cran()
  testthat::skip_if(Sys.getenv("INSTALLDIR") == "",
                    message = "cannot start the test, INSTALLDIR variable is not specified.")


  NLME_ROOT_DIRECTORY <- file.path(tempdir(TRUE), "UnitsTest")
  dir.create(NLME_ROOT_DIRECTORY)
  Sys.setenv("NLME_ROOT_DIRECTORY" = NLME_ROOT_DIRECTORY)

  # Define the model
  model <- pkmodel(
    isPopulation = TRUE,
    parameterization = "Clearance",
    absorption = "Intravenous",
    numCompartments = 2,
    isClosedForm = TRUE,
    isTlag = FALSE,
    hasEliminationComp = FALSE,
    isFractionExcreted = FALSE,
    isSaturating = FALSE,
    infusionAllowed = FALSE,
    isDuration = FALSE,
    columnMap = FALSE,
    modelName = "PKPDmodel",
    workingDir = NLME_ROOT_DIRECTORY
  )

  model <-
    residualError(
      model,
      predName = "C",
      errorType = "Multiplicative",
      SD = 0.1,
      isFrozen = FALSE,
      isBQL = FALSE
    )
  model <-
    structuralParameter(
      model,
      paramName = "Cl",
      fixedEffName = "tvCl",
      randomEffName = "nCl",
      style = "LogNormal",
      hasRandomEffect = TRUE
    )
  model <-
    structuralParameter(
      model,
      paramName = "V",
      fixedEffName = "tvV",
      randomEffName = "nV",
      style = "LogNormal",
      hasRandomEffect = FALSE
    )
  model <-
    structuralParameter(
      model,
      paramName = "V2",
      fixedEffName = "tvV2",
      randomEffName = "nV2",
      style = "LogNormal",
      hasRandomEffect = FALSE
    )
  model <-
    structuralParameter(
      model,
      paramName = "Cl2",
      fixedEffName = "tvCl2",
      randomEffName = "nCl2",
      style = "LogNormal",
      hasRandomEffect = TRUE
    )
  model <-
    fixedEffect(
      model,
      effect = "tvCl2",
      value = 14,
      lowerBound = NULL,
      upperBound = NULL,
      isFrozen = FALSE,
      unit = NULL
    )
  model <-
    fixedEffect(
      model,
      effect = "tvV2",
      value = 41,
      lowerBound = NULL,
      upperBound = NULL,
      isFrozen = TRUE,
      unit = NULL
    )
  model <-
    fixedEffect(
      model,
      effect = "tvV",
      value = 16,
      lowerBound = NULL,
      upperBound = NULL,
      isFrozen = FALSE,
      unit = "L"
    )
  model <-
    fixedEffect(
      model,
      effect = "tvCl",
      value = 7,
      lowerBound = NULL,
      upperBound = NULL,
      isFrozen = FALSE,
      unit = "L/h"
    )
  model <- dataMapping(model, pkData)
  model <-
    colMapping(model,
               c(
                 id = "Subject",
                 time = "Act_Time",
                 CObs = "Conc",
                 A1 = "Amount"
               ))

  # Define the engine parameters
  params <- Certara.RsNLME::engineParams(model,
                                         numIterations = 3,
                                         stdErr = "None")

  LocalHost <-
    NlmeParallelHost(
      sharedDirectory = Sys.getenv("NLME_ROOT_DIRECTORY"),
      installationDirectory = Sys.getenv("INSTALLDIR"),
      parallelMethod = NlmeParallelMethod("none"),
      hostName = "none",
      numCores = 1
    )

  res <-
    fitmodel(
      model,
      hostPlatform = LocalHost,
      params = params,
      filesToReturn = "theta.csv"
    )

  testthat::local_edition(3)
  testthat::expect_snapshot_file(
    file.path(model@modelInfo@workingDir, "theta.csv"),
    compare = compare_file_text
  )
})

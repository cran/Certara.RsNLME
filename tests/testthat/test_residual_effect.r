# Test residual effects initialization

test_that("Test residual effects initialization I", {
  location <- system.file("extdata/ResidualEffects",
                          "set_residual_effect_1.txt",
                          package = "Certara.RsNLME")
  referenceModel <- gsub("\\s+", "", readLines(location))

  model <- pkmodel(
    numCompartments = 1,
    isPopulation = TRUE,
    absorption = "Intravenous",
    parameterization = "Clearance",
    modelName = "PK01Model",
    isTlag = FALSE,
    hasEliminationComp = FALSE,
    isClosedForm = TRUE,
    columnMap = FALSE
  )

  model <-
    residualError(
      model,
      predName = "C",
      errorType = "Multiplicative",
      SD = 0.094356,
      isFrozen = TRUE
    )

  newModel <- gsub("\\s+", "", as.character(model@statements))
  expect_that(referenceModel, equals(newModel))
})


test_that("Test residual effects initialization II ", {
  location <- system.file("extdata/ResidualEffects",
                          "set_residual_effect_2.txt",
                          package = "Certara.RsNLME")
  referenceModel <- gsub("\\s+", "", readLines(location))

  model <- pkmodel(
    numCompartments = 1,
    isPopulation = TRUE,
    absorption = "Intravenous",
    parameterization = "Clearance",
    modelName = "PK01Model",
    isTlag = FALSE,
    hasEliminationComp = FALSE,
    isClosedForm = TRUE,
    columnMap = FALSE
  )

  model <-
    residualError(
      model,
      predName = "C",
      errorType = "Power",
      SD = 0.16,
      exponent = 3
    )

  newModel <- gsub("\\s+", "", as.character(model@statements))

  testthat::expect_that(referenceModel, testthat::equals(newModel))
})


test_that("Test residual effects initialization III ", {
  location <- system.file("extdata/ResidualEffects",
                          "set_residual_effect_3.txt",
                          package = "Certara.RsNLME")
  referenceModel <- gsub("\\s+", "", readLines(location))

  model <- pkmodel(
    numCompartments = 1,
    isPopulation = TRUE,
    absorption = "Intravenous",
    parameterization = "Clearance",
    modelName = "PK01Model",
    isTlag = FALSE,
    hasEliminationComp = FALSE,
    isClosedForm = TRUE,
    columnMap = FALSE
  )

  model <-
    residualError(
      model,
      predName = "C",
      errorType = "MixRatio",
      SD = 0.16,
      isFrozen = TRUE
    )

  newModel <- gsub("\\s+", "", as.character(model@statements))

  testthat::expect_that(referenceModel, testthat::equals(newModel))
})

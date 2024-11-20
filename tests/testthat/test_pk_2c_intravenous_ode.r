# 2Comp Intra PML generation

test_that("PML generation", {
  location <- system.file("extdata/PkModel", "pk_2c_intravenous_ode.txt", package =
                           "Certara.RsNLME")
  referenceModel <- gsub("\\s+", "", readLines(location))

  model <- pkmodel(
    isPopulation = TRUE,
    numCompartments = 2,
    absorption = "Intravenous",
    modelName = "PK",
    isClosedForm = FALSE,
    columnMap = FALSE
  )

  model <- residualError(model, errorType = "Additive", SD = 1)

  newModel <- gsub("\\s+", "", as.character(model@statements))

  expect_that(referenceModel, equals(newModel))
})

# 1 Comp Extravascular PML generation

test_that("PML generation", {
  location <-
    system.file("extdata/PkModel", "pk_1c_extravascular_ode.txt",
                package =
                  "Certara.RsNLME")
  referenceModel <- gsub("\\s+", "", readLines(location))

  model <- pkmodel(
    isPopulation = TRUE,
    numCompartments = 1,
    absorption = "Extravascular",
    modelName = "Categorical",
    isClosedForm = FALSE,
    columnMap = FALSE
  )

  model <- model %>%
    residualError(errorType = "Additive", SD = 1)

  newModel <- gsub("\\s+", "", as.character(model@statements))

  expect_that(referenceModel, equals(newModel))
})

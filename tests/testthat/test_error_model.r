# Error Model Parameter Validation

test_that("Test error model", {
  resEff1 <- NlmeResidualEffect(ERR_ADDITIVE, "C")
  resEff2 <- NlmeResidualEffect(ERR_MULTIPLICATIVE, "E")
  effectsList <- c(resEff1, resEff2)
  errorModel <- NlmeErrorModel(effectsList)

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

  newModel <- addToErrorModel(model, effectsList)
  newModelEffectsList <- newModel@errorModel@effectsList

  startIdx <-
    length(newModelEffectsList) - length(errorModel@effectsList) + 1
  endIdx <- length(newModelEffectsList)
  expect_that(errorModel@effectsList, equals(c(newModelEffectsList[(startIdx:endIdx)])))
})

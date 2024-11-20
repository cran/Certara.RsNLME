# Occasion Covariate PML generation

test_that("covariate effect is removed from model", {
  model <- pkmodel(numCompartments = 2,
                   columnMap = FALSE)

  model <-
    addCovariate(
      model,
      covariate = "BW",
      effect = c("V", "Cl"),
      center = "Value",
      centerValue = 70
    )

  model <- addCovariate(model, covariate = "Age", effect = "V")

  model <-
    removeCovariate(model, covariate = "BW", paramName = "V")

  model <-
    addCovariate(
      model,
      covariate = "Sex",
      effect = "Cl",
      type = "Occasion",
      levels = c(1, 2),
      labels = c("one", "two"),
      direction = "Backward"
    )

  statements <- paste(model@statements, collapse = "\n")

  testthat::local_edition(3)
  testthat::expect_snapshot_value(statements)
})

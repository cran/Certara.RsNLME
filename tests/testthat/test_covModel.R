# Test covariateModel function

test_that("dfs for fixefs associated with categorical covs are correct", {
  testthat::skip_on_cran()
  testthat::skip_if(Sys.getenv("INSTALLDIR") == "",
                    message = "cannot start the test, INSTALLDIR variable is not specified.")

  ModelName <- "Cov5Ls_Cov3Ls_Cov2Ls"
  mmdlfile <-
    system.file(
      paste0("extdata/CovModel/", ModelName, ".csv"),
      package = "Certara.RsNLME",
      mustWork = TRUE
    )

  directoryToRun <- file.path(tempdir(TRUE), "CovModel")
  dt_InputDataSet <- data.table::fread(mmdlfile)

  ## Define the model
  model <- pkemaxmodel(
    absorption = "FirstOrder",
    data = dt_InputDataSet,
    ID = "ID",
    Time = "time",
    Aa = "Dose",
    CObs = "CObs",
    EObs = "EObs",
    modelName = ModelName
  )

  model <-
    residualError(model,
                  predName = "E",
                  errorType = "Multiplicative",
                  SD = 0.1)
  model <-
    structuralParameter(model, paramName = "EC50", hasRandomEffect = FALSE)
  model <-
    fixedEffect(
      model,
      effect = c("tvKa", "tvV", "tvCl", "tvEmax", "tvEC50"),
      value = c(0.5, 5, 1.5, 1, 100)
    )
  model <-
    randomEffect(model,
                 effect = c("nKa", "nV", "nCl", "nEmax"),
                 value = rep(0.1, 4))
  model <-
    addCovariate(
      model,
      covariate = "BW",
      effect = c("Ka", "V", "Cl"),
      center = "Value",
      centerValue = 70
    )
  model <-
    addCovariate(
      model,
      covariate = "Study",
      effect = c("Ka"),
      type = "Categorical",
      levels = c(1, 2, 3)
    )
  model <-
    addCovariate(
      model,
      covariate = "Sex",
      effect = c("Ka", "V", "Cl"),
      type = "Categorical",
      levels = c(0, 1)
    )
  model <-
    addCovariate(
      model,
      covariate = "Race",
      effect = c("V", "Cl", "Emax"),
      type = "Categorical",
      levels = c(0, 1, 2, 3, 4)
    )

  CovModelBuiltin <- covariateModel(model)
  testthat::local_edition(3)
  testthat::expect_snapshot(CovModelBuiltin)
})

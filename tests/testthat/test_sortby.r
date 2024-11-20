test_that("class running an estimation with sort columns ", {
  testthat::skip_on_cran()
  testthat::skip_if(Sys.getenv("INSTALLDIR") == "",
                    message = "cannot start the test, INSTALLDIR variable is not specified.")

  NLME_ROOT_DIRECTORY <- file.path(tempdir(TRUE), "SortfitTest")
  dir.create(NLME_ROOT_DIRECTORY, showWarnings = FALSE)
  Sys.setenv("NLME_ROOT_DIRECTORY" = NLME_ROOT_DIRECTORY)

  model <-
    pkmodel(
      numCompartments = 2,
      data = pkData,
      ID = "Subject",
      Time = "Act_Time",
      A1 = "Amount",
      CObs = "Conc",
      workingDir = NLME_ROOT_DIRECTORY
    )

  model <-
    addCovariate(
      model,
      covariate = "BodyWeight",
      direction = "Backward",
      center = "Mean",
      effect = c("V", "Cl")
    )

  # specify scenarios
  CovariateEffectNames <- listCovariateEffectNames(model)

  combinations <-
    combn(c("", CovariateEffectNames),
          length(CovariateEffectNames),
          simplify = FALSE)

  scenarioNames <-
    lapply(combinations,
           function(x) {
             paste(x, collapse = " ")
           })

  scenarios <-
    lapply(scenarioNames,
           function(x, CovariateEffectNames) {
             CovariateCombinations <- unlist(strsplit(x, " ", fixed = TRUE))
             scenarioIndex <-
               paste(
                 which(CovariateEffectNames %in% CovariateCombinations,
                       arr.ind = TRUE),
                 collapse = ", "
               )
             NlmeScenario(trimws(x), scenarioIndex)
           },
           CovariateEffectNames)

  # multicore
  multicoreHost <-
    NlmeParallelHost(
      installationDirectory = Sys.getenv("INSTALLDIR"),
      parallelMethod = NlmeParallelMethod("multicore"),
      hostName = "multicore",
      numCores = 4
    )

  res <-
    sortfit(
      model,
      hostPlatform = multicoreHost,
      params = engineParams(model, numIterations = 5),
      sortColumns = SortColumns("Gender"),
      scenarios = scenarios,
      filesToReturn = c("omega.csv", "theta.csv")
    )

  testthat::local_edition(3)
  testthat::expect_snapshot_file(
    path = file.path(model@modelInfo@workingDir, "omega.csv"),
    compare = compare_file_text
  )

  res <-
    sortfit(
      model,
      hostPlatform = multicoreHost,
      params = engineParams(model, method = "Naive-Pooled"),
      sortColumns = SortColumns("Gender"),
      scenarios = scenarios,
      filesToReturn = c("theta.csv", "ConvergenceData.csv", "thetaCovariance.csv", "Covariance.csv", "residuals.csv")
    )

  testthat::expect_snapshot_file(
    path = file.path(model@modelInfo@workingDir, "theta.csv"),
    compare = compare_file_text
  )

})


test_that("class running a sorted individual estimation", {
  testthat::skip_on_cran()
  testthat::skip_if(Sys.getenv("INSTALLDIR") == "",
                    message = "cannot start the test, INSTALLDIR variable is not specified.")

  NLME_ROOT_DIRECTORY <-
    file.path(tempdir(TRUE), "IndSortfitTest")
  dir.create(NLME_ROOT_DIRECTORY)
  Sys.setenv("NLME_ROOT_DIRECTORY" = NLME_ROOT_DIRECTORY)

  # Define the model
  model <- pkmodel(
    isPopulation = FALSE,
    numCompartments = 2,
    Time = "Act_Time",
    CObs = "Conc",
    A1 = "Amount",
    data = pkData,
    modelName = "IndModel",
    workingDir = NLME_ROOT_DIRECTORY
  )

  model <- addSecondary(model, "ClRatio", "Cl/Cl2")

  res <-
    sortfit(model,
            sortColumns = SortColumns(c("Gender", "Subject")))

  testthat::local_edition(3)
  testthat::expect_snapshot_file(
    path = file.path(model@modelInfo@workingDir, "residuals.csv"),
    compare = compare_file_text,
    variant = paste0("2sort_Ind_", .Platform$OS.type)
  )
})

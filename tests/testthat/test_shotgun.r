test_that("class running a shotgun covariate search ", {
  testthat::skip_on_cran()
  testthat::skip_if(Sys.getenv("INSTALLDIR") == "",
                    message = "cannot start the test, INSTALLDIR variable is not specified.")

  NLME_ROOT_DIRECTORY <-
    normalizePath(file.path(tempdir(TRUE), "ShotgunTest"),
                  winslash = "/",
                  mustWork = FALSE)
  dir.create(NLME_ROOT_DIRECTORY)
  Sys.setenv("NLME_ROOT_DIRECTORY" = NLME_ROOT_DIRECTORY)

  model <- pkmodel(
    numCompartments = 2,
    data = pkData,
    ID = "Subject",
    Time = "Act_Time",
    A1 = "Amount",
    CObs = "Conc",
    workingDir = tempdir(TRUE)
  )

  # Add Gender covariate of type categorical
  model <- addCovariate(
    model,
    covariate = "Gender",
    type = "Categorical",
    effect = c("V2", "Cl2"),
    levels = c(0, 1),
    labels = c("Female", "Male")
  )

  # Add Bodyweight covariate of type continuous
  model <- addCovariate(
    model,
    covariate = "BodyWeight",
    type = "Continuous",
    direction = "Backward",
    center = "Mean",
    effect = c("V", "Cl")
  )

  # multicore
  multicoreHost <-
    NlmeParallelHost(
      sharedDirectory = Sys.getenv("NLME_ROOT_DIRECTORY"),
      installationDirectory = Sys.getenv("INSTALLDIR"),
      parallelMethod = NlmeParallelMethod("multicore"),
      hostName = "multicore",
      numCores = 4
    )

  cp <- covariateModel(model)
  params <- Certara.RsNLME::engineParams(model,
                                         numIterations = 2,
                                         stdErr = "None")

  OverallDF <- Certara.RsNLME::shotgunSearch(
    model = model,
    hostPlatform = multicoreHost,
    covariateModel = cp,
    params = params
  )

  testthat::local_edition(3)
  testthat::expect_snapshot_value(OverallDF,
                                  style = "json2")
})

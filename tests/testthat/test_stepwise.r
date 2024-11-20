test_that("class validation test", {
  expect_is(StepwiseParams(0.01, 0.001, "-2LL"),
            "StepwiseParams")
})

test_that("test setting values", {
  params <- StepwiseParams(0.01, 0.001, "-2LL")
  testthat::expect_equal(params@method, "-2LL")
  testthat::expect_equal(params@addPValue, 0.01)
  testthat::expect_equal(params@removePValue, 0.001)
})

test_that("class running a stepwise covariate search ", {
  testthat::skip_on_cran()
  testthat::skip_if(Sys.getenv("INSTALLDIR") == "",
                    message = "cannot start the test, INSTALLDIR variable is not specified.")

  NLME_ROOT_DIRECTORY <-
    normalizePath(file.path(tempdir(TRUE), "StepwiseTest"),
                  winslash = "/",
                  mustWork = FALSE)
  dir.create(NLME_ROOT_DIRECTORY)
  Sys.setenv("NLME_ROOT_DIRECTORY" = NLME_ROOT_DIRECTORY)

  # Define the model
  model <- pkmodel(
    numCompartments = 2,
    data = pkData,
    ID = "Subject",
    Time = "Act_Time",
    A1 = "Amount",
    CObs = "Conc",
    workingDir = normalizePath(tempdir(), winslash = "/")
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
      hostName = "multicore1",
      numCores = 4
    )

  # Define the engine parameters
  params <- Certara.RsNLME::engineParams(model,
                                         stdErr = "None")

  # Define covariate model
  cp <- covariateModel(model)

  # Define the stepwise parameters
  sp <- StepwiseParams(0.01, 0.001, "-2LL")

  # Perform stepwise search
  OverallDF <-  stepwiseSearch(
    model = model,
    hostPlatform = multicoreHost,
    params = params,
    covariateModel = cp,
    stepwiseParams = sp,
    runInBackground = FALSE,
    hostName = "multicore"
  )

  testthat::local_edition(3)
  testthat::expect_snapshot_value(OverallDF,
                                  style = "json2")
})

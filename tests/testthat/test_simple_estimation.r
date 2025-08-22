test_that("class running a simple population estimation", {
  testthat::skip_on_cran()
  testthat::skip_if(Sys.getenv("INSTALLDIR") == "",
                    message = "cannot start the test, INSTALLDIR variable is not specified.")

  NLME_ROOT_DIRECTORY <-
    file.path(tempdir(TRUE), "Simplefit Test")
  dir.create(NLME_ROOT_DIRECTORY)
  Sys.setenv("NLME_ROOT_DIRECTORY" = NLME_ROOT_DIRECTORY)

  # Define the model
  model <- pkmodel(
    numCompartments = 2,
    ID = "Subject",
    Time = "Act_Time",
    CObs = "Conc",
    A1 = "Amount",
    data = pkData,
    modelName = "PkModel" ,
    workingDir = NLME_ROOT_DIRECTORY
  )

  LocalHost <-
    NlmeParallelHost(
      sharedDirectory = Sys.getenv("NLME_ROOT_DIRECTORY"),
      installationDirectory = Sys.getenv("INSTALLDIR"),
      parallelMethod = NlmeParallelMethod("none"),
      hostName = "none",
      numCores = 1
    )

  res <- fitmodel(
    model,
    hostPlatform = LocalHost,
    stdErr = "None",
    method = "QRPEM",
    numIterMAPNP = 1,
    numIterations = 3
  )

  testthat::local_edition(3)
  testthat::expect_snapshot_file(
    path = file.path(model@modelInfo@workingDir, "theta.csv"),
    compare = compare_file_text
  )

  testthat::expect_snapshot_file(
    path = file.path(model@modelInfo@workingDir, "omega.csv"),
    compare = compare_file_text
  )

  # the emax model with 0 iterations
  model <- emaxmodel(
    checkBaseline = TRUE,
    data = pkpdData,
    ID = "ID",
    C = "CObs",
    EObs = "EObs",
    modelName = "Emax_FOCE-ELS",
    workingDir = NLME_ROOT_DIRECTORY
  )

  Table2 <- NlmeTableDef(
    name = "CovrSet_C.csv",
    covrSet = c("C"),
    whenObs = "EObs",
    variablesList = c("E")
  )

  job <-
    fitmodel(
      model,
      simpleTables = Table2,
      stdErr = "None",
      numIterations = 0,
      numCores = 1,
      filesToReturn = "CovrSet_C.csv"
    )

  testthat::expect_snapshot(
    job$CovrSet_C
  )
})

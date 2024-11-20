test_that("class running a bootstrap ", {
  testthat::skip_on_cran()
  testthat::skip_if(Sys.getenv("INSTALLDIR") == "",
                    message = "cannot start the test, INSTALLDIR variable is not specified.")

  bootModelDataLocation <-
    system.file("extdata/Bootstrap/data1.csv",
                package = "Certara.RsNLME",
                mustWork = TRUE)
  Data <- read.csv(bootModelDataLocation)

  NLME_ROOT_DIRECTORY <- file.path(tempdir(), "BootstrapTest")
  dir.create(NLME_ROOT_DIRECTORY)

  bootModel <- pkmodel(
    absorption = "FirstOrder",
    numCompartments = 1,
    data = Data,
    ID = "xid",
    Time = "time",
    Aa = "dose",
    CObs = "yobs",
    modelName = "OneCpt_Absorp",
    workingDir = NLME_ROOT_DIRECTORY
  )

  Sys.setenv("NLME_ROOT_DIRECTORY" = NLME_ROOT_DIRECTORY)
  # using default host
  # initial estimates are not computed since initialEstimates by default is FALSE
  bootstrapResults <-
    bootstrap(
      bootModel,
      params = engineParams(bootModel, method = "FOCE-LB"),
      numReplicates = 5,
      randomNumSeed = 1234
    )

  testthat::local_edition(3)
  testthat::expect_snapshot_file(
    file.path(bootModel@modelInfo@workingDir, "BootOverall.csv"),
    compare = compare_file_text
  )

})

test_that("class running a profile pertubation", {
  testthat::skip_on_cran()
  testthat::skip_if(Sys.getenv("INSTALLDIR") == "",
                    message = "cannot start the test, INSTALLDIR variable is not specified.")

  workingDir <- tempdir(TRUE)
  NLME_ROOT_DIRECTORY <- file.path(workingDir, "ProfileTest")
  dir.create(NLME_ROOT_DIRECTORY)
  Sys.setenv("NLME_ROOT_DIRECTORY" = NLME_ROOT_DIRECTORY)
  host <-
    NlmeParallelHost(
      sharedDirectory = Sys.getenv("NLME_ROOT_DIRECTORY"),
      installationDirectory = Sys.getenv("INSTALLDIR"),
      parallelMethod = NlmeParallelMethod("Multicore"),
      hostName = "Multicore",
      numCores = 4
    )

  dataset <- NlmeDataset()
  DataPackagePath <- system.file("extdata/Profile/",
                                 package = "Certara.RsNLME")
  filesToTransfer <-
    file.path(DataPackagePath,
              c(dataset@dataFile,
                dataset@colDefFile,
                dataset@modelFile))

  file.copy(from = filesToTransfer,
            to = workingDir,
            overwrite = TRUE)

  params <- NlmeEngineExtraParams(method = 5,
                                  numIterations = 1000)

  profileV <- ProfileVar("tvV", 9.95482, "-2,-1,0,1,2")
  profiles <- ProfileParameters("USE_DELTA", c(profileV))

  sortColumns <- SortColumns("")

  scenarios <- list()

  job <-
    Certara.RsNLME::RunProfilePertubation(
      hostPlatform = host,
      dataset = dataset,
      params = params,
      profiles = profiles,
      sortColumns = sortColumns,
      scenarios = scenarios,
      workingDir = workingDir,
      runInBackground = FALSE
    )

  testthat::local_edition(3)
  testthat::expect_snapshot_file(
    path = file.path(workingDir, "Profile.csv"),
    compare = compare_file_text
  )
})

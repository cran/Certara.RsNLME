# Test checkHostParams functions

test_that("test for installdir if presented", {
  testthat::skip_on_cran()
  testthat::skip_if(Sys.getenv("INSTALLDIR") == "",
                    message = "cannot start the test, INSTALLDIR variable is not specified.")

  multicoreHost <-
    NlmeParallelHost(
      sharedDirectory = tempdir(TRUE),
      installationDirectory = Sys.getenv("INSTALLDIR"),
      parallelMethod = NlmeParallelMethod("multicore"),
      hostName = "multicore",
      numCores = 4
    )

  testthat::local_edition(3)
  testthat::expect_snapshot_value(checkHostParams(multicoreHost))


})

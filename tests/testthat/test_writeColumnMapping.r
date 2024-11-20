# Test that columns are properly mapped from mmdl
testthat::skip_on_cran()
testthat::skip_if(Sys.getenv("INSTALLDIR") == "",
                  message = "cannot start the test, INSTALLDIR variable is not specified.")

mmdlfilesLocation <- system.file("extdata/writeColumnMapping/",
                                 package = "Certara.RsNLME")

testthat::local_edition(3)
mmdlfiles <-
  list.files(mmdlfilesLocation,
             pattern = "*.mmdl",
             full.names = TRUE)
for (mmdlfile in mmdlfiles) {
  test_that(paste0("model from ", basename(mmdlfile), " created properly:"), {
    ModelArgs <-
      create_model_from_metamodel(mmdlfile = mmdlfile,
                                  directoryToRun = tempdir(TRUE))
    ModelArgs$model@modelInfo@workingDir <- ""
    ModelArgs$model@statements <- list()
    ModelArgs$model@inputData <- data.frame()
    testthat::expect_snapshot_output(print(ModelArgs))
  })
}

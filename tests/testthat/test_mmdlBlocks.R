# Test metamodel coldefs

test_that("correct cols1.txt is built with DOSING CYCLE block", {
  testthat::skip_on_cran()
  testthat::skip_if(Sys.getenv("INSTALLDIR") == "",
                    message = "cannot start the test, INSTALLDIR variable is not specified.")

  mmdlfile <-
    system.file("extdata/mmdlBlocks/addl_ss.mmdl",
                package = "Certara.RsNLME",
                mustWork = TRUE)

  directoryToRun <- file.path(tempdir(TRUE), "mmdlBlocks")
  NLMEModel <-
    create_model_from_metamodel(mmdlfile = mmdlfile,
                                directoryToRun = directoryToRun)

  testthat::local_edition(3)
  testthat::expect_snapshot_value(
    Certara.RsNLME:::writeColumnMapping(
      model = NLMEModel$model,
      filename = "addl_ss_cols1.txt",
      workingDir = tempdir()
    ),
    style = "json2"
  )

})


test_that("correct cols1.txt is built with categorical covariate lables in MAP block",
          {
            testthat::skip_on_cran()
            testthat::skip_if(Sys.getenv("INSTALLDIR") == "",
                              message = "cannot start the test, INSTALLDIR variable is not specified.")

            mmdlfile <-
              system.file("extdata/mmdlBlocks/genderGroup.mmdl",
                          package = "Certara.RsNLME",
                          mustWork = TRUE)

            directoryToRun <- file.path(tempdir(TRUE), "mmdlBlocks")
            NLMEModel <-
              create_model_from_metamodel(mmdlfile = mmdlfile,
                                          directoryToRun = directoryToRun)

            testthat::local_edition(3)
            testthat::expect_snapshot_value(
              Certara.RsNLME:::writeColumnMapping(
                model = NLMEModel$model,
                filename = "covLabels_cols1.txt",
                workingDir = tempdir()
              ),
              style = "json2"
            )

          })

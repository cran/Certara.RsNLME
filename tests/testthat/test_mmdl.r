# Test metamodel runs

test_that("correct run for not time based metamodel with coldef only;
          also checks posthoc",
          {
            testthat::skip_on_cran()
            testthat::skip_if(Sys.getenv("INSTALLDIR") == "",
                              message = "cannot start the test, INSTALLDIR variable is not specified.")

            mmdlfile <- system.file("extdata/mmdlNoTime/test.mmdl",
                                    package = "Certara.RsNLME",
                                    mustWork = TRUE)

            directoryToRun <- file.path(tempdir(TRUE), "MmdlNoTimeTest")
            # using default host
            mmdlResults <- Certara.RsNLME::run_metamodel(mmdlfile = mmdlfile,
                                                         directoryToRun = directoryToRun)

            testthat::local_edition(3)
            testthat::expect_snapshot_file(
              path = file.path(directoryToRun, "theta.csv"),
              compare = compare_file_text
            )
          })

test_that("comparing runs with infusion specified in coldef vs map blocks;
          also checks posthoc",
          {
            testthat::skip_on_cran()
            testthat::skip_if(Sys.getenv("INSTALLDIR") == "",
                              message = "cannot start the test, INSTALLDIR variable is not specified.")

            OneCpt_IVInfusion <-
              system.file(
                "extdata/mmdlInfusionTest/OneCpt_IVInfusion.mmdl",
                package = "Certara.RsNLME",
                mustWork = TRUE
              )

            OneCpt_IVInfusion_doseStatement <-
              system.file(
                "extdata/mmdlInfusionTest/OneCpt_IVInfusion_doseStatement.mmdl",
                package = "Certara.RsNLME",
                mustWork = TRUE
              )

            directoryToRun <- file.path(tempdir(TRUE), "MmdlInfusionTest")
            # using default host
            OneCpt_IVInfusionResults <- Certara.RsNLME::run_metamodel(mmdlfile = OneCpt_IVInfusion,
                                                                      directoryToRun = directoryToRun)

            OneCpt_IVInfusionResults$nlme7engine.log <- NULL
            OneCpt_IVInfusionResults$dmp.txt <- NULL

            OneCpt_IVInfusion_doseStatementResults <-
              Certara.RsNLME::run_metamodel(mmdlfile = OneCpt_IVInfusion_doseStatement,
                                            directoryToRun = directoryToRun)
            OneCpt_IVInfusion_doseStatementResults$nlme7engine.log <- NULL
            OneCpt_IVInfusion_doseStatementResults$dmp.txt <- NULL

            testthat::local_edition(3)
            testthat::expect_equal(OneCpt_IVInfusionResults,
                                   OneCpt_IVInfusion_doseStatementResults)
          })

# Test metamodel extraction

test_that("correct test.mdl nlmeargs.txt cols1.txt extracted",
          {
            testthat::skip_on_cran()
            testthat::skip_if(Sys.getenv("INSTALLDIR") == "",
                              message = "cannot start the test, INSTALLDIR variable is not specified.")

            mmdlfile <- system.file("extdata/mmdlNoTime/test.mmdl",
                                    package = "Certara.RsNLME",
                                    mustWork = TRUE)

            directoryToExtract <-
              file.path(tempdir(TRUE), "MmdlExtraction")
            Certara.RsNLME::extract_mmdl(mmdlfile, directoryToExtract)

            testthat::local_edition(3)
            testthat::expect_snapshot_file(path = file.path(directoryToExtract, "1-est", "test.mdl"),
                                           compare = compare_file_text)

            testthat::expect_snapshot_file(path = file.path(directoryToExtract, "1-est", "cols1.txt"),
                                           compare = compare_file_text)

            testthat::expect_snapshot_file(path = file.path(directoryToExtract, "1-est", "nlmeargs.txt"),
                                           compare = compare_file_text)
          })

test_that("correct multiple nlmeargs.txt extracted",
          {
            testthat::skip_on_cran()
            testthat::skip_if(Sys.getenv("INSTALLDIR") == "",
                              message = "cannot start the test, INSTALLDIR variable is not specified.")

            mmdlfile <-
              system.file(
                "extdata/mmdlBlocks/multipleEstSim.mmdl",
                package = "Certara.RsNLME",
                mustWork = TRUE
              )

            directoryToExtract <-
              file.path(tempdir(TRUE), "MultMmdlExtraction")
            nlmeargsOutput <- "multArgs.txt"
            Certara.RsNLME::extract_mmdl(mmdlfile, directoryToExtract, nlmeargsOutput = nlmeargsOutput)

            testthat::local_edition(3)

            nlmeargsFullPaths <-
              file.path(directoryToExtract, c("1-est", "2-est", "3-sim", "4-sim"), nlmeargsOutput)
            for (nlmeargsIndex in seq_along(nlmeargsFullPaths)) {
              testthat::expect_snapshot_file(
                name = paste0(nlmeargsOutput, nlmeargsIndex),
                path = file.path(nlmeargsFullPaths[nlmeargsIndex]),
                compare = compare_file_text
              )

            }
          })

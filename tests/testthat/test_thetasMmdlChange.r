# Test changes in metamodel thetas

test_that("change thetas in mmdl for not time based metamodel with coldef only",
          {
            testthat::skip_on_cran()
            testthat::skip_if(Sys.getenv("INSTALLDIR") == "",
                              message = "cannot start the test, INSTALLDIR variable is not specified.")
            mmdlfileSystem <-
              system.file("extdata/thetasMmdlChange/test.mmdl",
                          package = "Certara.RsNLME",
                          mustWork = TRUE)

            dataFileSystem <-
              system.file("extdata/thetasMmdlChange/data1.txt",
                          package = "Certara.RsNLME",
                          mustWork = TRUE)

            mmdlfile <- file.path(tempdir(TRUE),
                                  "thetasMmdlChange.mmdl")
            file.copy(mmdlfileSystem, mmdlfile, overwrite = TRUE)
            file.copy(dataFileSystem, dirname(mmdlfile), overwrite = TRUE)

            mmdl_withComments <-
              rawToChar(readBin(mmdlfile, "raw", n = 30000))

            mmdl_model <-
              create_model_from_metamodel(mmdlfile = mmdlfile,
                                          directoryToRun = tempdir(TRUE))

            resmodelRDS <-
              system.file(
                "extdata/thetasMmdlChange/resmodel.RDS",
                package = "Certara.RsNLME",
                mustWork = TRUE
              )

            resmodel <- readRDS(resmodelRDS)

            # load_all causes error in VPC test
            substitutedMmdl <-
              saveUpdatedMetamodel(mmdl_withComments = mmdl_withComments,
                                   mmdl_model = mmdl_model,
                                   resmodel = resmodel,
                                   metamodelFile = mmdlfile)

            # expect_snapshot_file is the only way for the system dependent checks
            testthat::local_edition(3)
            testthat::expect_snapshot_file(mmdlfile,
                                           compare = compare_file_text)

          })

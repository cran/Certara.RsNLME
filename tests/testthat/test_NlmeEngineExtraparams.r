test_that("class validation test", {
  testthat::expect_is(NlmeEngineExtraParams(), "NlmeEngineExtraParams")
})


test_that("test default values", {
  params <- NlmeEngineExtraParams()
  testthat::expect_equal(params@isPopulation, TRUE)
  testthat::expect_equal(params@method, 5)
  testthat::expect_equal(params@odeToUse, 6)
  testthat::expect_equal(params@isQRPEMStyleMethod, 0)
  testthat::expect_equal(params@numIterations, 1000)
  testthat::expect_equal(params@xfocehess, 1)
  testthat::expect_equal(params@nmxstep, 5000)
  testthat::expect_equal(params@sort, "  -sort ")
  testthat::expect_equal(params@sand, "")
  testthat::expect_equal(params@csv, "  -csv ")
  testthat::expect_equal(params@fisher, "")
})

test_that("test print nlme engine default values",  {
  params <- NlmeEngineExtraParams()
  lines <- capture.output(print(params))
  testthat::expect_equal(lines[4], "Is population               :  TRUE")
  testthat::expect_equal(lines[6], "Engine used                 :  FOCE-ELS")
  testthat::expect_equal(lines[7], "Maximum number of iterations:  1000")
  testthat::expect_equal(lines[8], "ODE solver                  :  Matrix Exponent")
  testthat::expect_equal(lines[15], "Use synthetic gradients     :  FALSE")
  testthat::expect_equal(lines[18], "Linearization step size     :  0.002")
  testthat::expect_equal(lines[19], "ODE relative tolerance      :  1e-06")
  testthat::expect_equal(lines[20], "ODE absolute tolerance      :  1e-06")
  testthat::expect_equal(lines[21], "ODE max steps               :  5000")
  testthat::expect_equal(lines[25], "Standard Errors Method      :  Hessian")
  testthat::expect_equal(lines[26], "Finite Difference Method    :  Central Difference")
  testthat::expect_equal(lines[27], "Step size                   :  0.01")
})

test_that("engine params wrapper prints expected values for population model",
          {
            testthat::skip_on_cran()
            testthat::skip_if(Sys.getenv("INSTALLDIR") == "",
                              message = "cannot start the test, INSTALLDIR variable is not specified.")
            model <- pkmodel(
              numCompartments = 1,
              data = pkData,
              ID = "Subject",
              Time = "Act_Time",
              A1 = "Amount",
              CObs = "Conc",
              modelName = "OneCpt_IVBolus_FOCE-ELS"
            )


            params <-
              engineParams(model, ODE = "AutoDetect", numIterations = 10000)
            lines <- capture.output(print(params))
            testthat::expect_equal(lines[4], "Is population               :  TRUE")
            testthat::expect_equal(lines[6], "Engine used                 :  FOCE-ELS")
            testthat::expect_equal(lines[7], "Maximum number of iterations:  10000")
            testthat::expect_equal(lines[8], "ODE solver                  :  Auto-detect")
            testthat::expect_equal(lines[15], "Use synthetic gradients     :  FALSE")
            testthat::expect_equal(lines[18], "Linearization step size     :  0.002")
            testthat::expect_equal(lines[19], "ODE relative tolerance      :  1e-06")
            testthat::expect_equal(lines[20], "ODE absolute tolerance      :  1e-06")
            testthat::expect_equal(lines[21], "ODE max steps               :  50000")
            testthat::expect_equal(lines[25], "Standard Errors Method      :  Sandwich")
            testthat::expect_equal(lines[26], "Finite Difference Method    :  Central Difference")
            testthat::expect_equal(lines[27], "Step size                   :  0.01")
          })


test_that("engine params wrapper prints default values for individual model",
          {
            testthat::skip_on_cran()
            testthat::skip_if(Sys.getenv("INSTALLDIR") == "",
                              message = "cannot start the test, INSTALLDIR variable is not specified.")
            model <- pkmodel(
              numCompartments = 1,
              isPopulation = FALSE,
              data = pkData,
              Time = "Act_Time",
              A1 = "Amount",
              CObs = "Conc",
              modelName = "OneCpt_IVBolus_FOCE-ELS_Individual"
            )

            params <- engineParams(model)
            lines <- capture.output(print(params))
            testthat::expect_equal(lines[4], "Is population               :  FALSE")
            testthat::expect_equal(lines[5], "Sort input data             :  TRUE")
            testthat::expect_equal(lines[6], "Engine used                 :  NAIVE-POOLED")
            testthat::expect_equal(lines[8], "ODE solver                  :  Matrix Exponent")
            testthat::expect_equal(lines[12], "Step size for partial deriv :  1e-05")
            testthat::expect_equal(lines[14], "ODE relative tolerance      :  1e-06")
            testthat::expect_equal(lines[15], "ODE absolute tolerance      :  1e-06")
            testthat::expect_equal(lines[16], "ODE max steps               :  50000")
            testthat::expect_equal(lines[20], "Standard Errors Method      :  Hessian")
            testthat::expect_equal(lines[21], "Finite Difference Method    :  Central Difference")
          })

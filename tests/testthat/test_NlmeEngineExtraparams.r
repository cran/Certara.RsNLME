# --- Model Setup ---
pkData1 <- pkData
pkData1$Reset <- 0

pop_model <-
  pkmodel(
    parameterization = "Clearance",
    numCompartments = 1,
    data = pkData1,
    ID = "Subject",
    Time = "Act_Time",
    A1 = "Amount",
    CObs = "Conc",
    workingDir = tempdir()
  )

ind_model <-
  pkmodel(
    isPopulation = FALSE,
    parameterization = "Clearance",
    numCompartments = 1,
    data = pkData1,
    Time = "Act_Time",
    A1 = "Amount",
    CObs = "Conc",
    workingDir = tempdir()
  )

pop_model_reset <-
  addReset(pop_model, low = 4, hi = 4, Reset = "Reset")

# --- Test Suite ---

test_that("NlmeEngineExtraParams: Class validation and construction", {
  expect_s4_class(NlmeEngineExtraParams(), "NlmeEngineExtraParams")
  # Test direct slot setting during construction
  params <- NlmeEngineExtraParams(method = 1, numIterations = 50)
  expect_equal(params@method, 1)
  expect_equal(params@numIterations, 50)
})

test_that("NlmeEngineExtraParams: Default slot values are correct", {
  params <- NlmeEngineExtraParams()
  # Core
  expect_true(params@isPopulation)
  expect_equal(params@method, 5) # FOCE-ELS/Laplacian
  expect_equal(params@numIterations, 1000)
  expect_equal(params@odeToUse, 6) # Matrix Exponent
  expect_equal(params@sort, " -sort ")
  expect_equal(params@csv, " -csv ")
  # Tolerances
  expect_equal(params@rtol, 1e-06)
  expect_equal(params@atol, 1e-06)
  expect_equal(params@nmxstep, 5000) # Note: prototype is 5000, wrapper default is 50000
  # ELS/Laplacian related (defaults assume FOCE-ELS)
  expect_equal(params@xfocehess, 1) # FOCE-ELS default
  expect_equal(params@anagrad, 0)
  expect_equal(params@xnorderagq, 1)
  expect_equal(params@xlandig, 7)
  expect_equal(params@xblndig, 13)
  expect_equal(params@xbltol, 0.002)
  expect_equal(params@gradTolOuter, 2e-4)
  expect_equal(params@stepTolOuter, 1e-4)
  expect_equal(params@gradTolInner, 1.71e-5)
  expect_equal(params@stepTolInner, 7.07e-8)
  expect_equal(params@refDeltaLagl, 1e-3)
  # SE related
  expect_equal(params@xstderr, 1) # Central diff
  expect_equal(params@sand, "")
  expect_equal(params@fisher, "")
  expect_equal(params@autodetect, "")
  expect_equal(params@xlatol, 0.01) # Default for population
  # QRPEM related (defaults)
  expect_equal(params@xisample, 300)
  expect_equal(params@xmapassist, 0)
  expect_equal(params@ximpsampdof, 0) # Normal
  expect_equal(params@xmcpem, 0) # Quasi-Random
  expect_equal(params@xsirsamp, 10)
  expect_equal(params@xburnin, 0)
  expect_equal(params@xnonomegaburn, 0)
  expect_equal(params@xaccratio, 0.1)
  expect_equal(params@xscramble, 1) # Owen
  expect_equal(params@emTolType, 0)
  expect_equal(params@emConvLen, 10)
  expect_equal(params@emConvCritVal, 5)
  # Others
  expect_equal(params@xnp, 0)
  expect_equal(params@xmapnp, 0)
  expect_equal(params@isPCWRES, 0)
  expect_equal(params@xpcwresnrep, 1) # Note: prototype=1, wrapper default=0
  expect_equal(params@pardern, 20)
  expect_equal(params@parderd, 1e-05)
  expect_equal(params@logtran, 1)
})

test_that("NlmeEngineExtraParams: Print method default output correct", {
  params <- NlmeEngineExtraParams()
  output <- capture.output(print(params))

  # General section
  expect_true(any(grepl("Is population\\s+:\\s+TRUE", output)))
  expect_true(any(grepl("Sort input data\\s+:\\s+TRUE", output)))
  expect_true(any(grepl("Engine used\\s+:\\s+FOCE-ELS", output)))
  expect_true(any(grepl("Max iterations\\s+:\\s+1000", output)))
  expect_true(any(grepl(
    "ODE solver\\s+:\\s+Matrix Exponent", output
  ))) # Based on prototype odeToUse=6

  # Advanced Options (Defaults for FOCE-ELS)
  expect_true(any(grepl("Integration points\\s+:\\s+1", output)))
  expect_true(any(grepl("fastOptimization\\s+:\\s+FALSE", output)))
  expect_true(any(grepl(
    "Outer Grad Tolerance\\s+:\\s+2e-04", output
  )))
  expect_true(any(grepl(
    "Outer Step Tolerance\\s+:\\s+1e-04", output
  )))
  expect_true(any(grepl(
    "Inner Grad Tolerance\\s+:\\s+1.71e-05", output
  )))
  expect_true(any(grepl(
    "Inner Step Tolerance\\s+:\\s+7.07e-08", output
  )))
  expect_true(any(grepl(
    "LL Delta Tolerance\\s+:\\s+0.001", output
  )))
  expect_true(any(grepl("Non-param iter\\s+:\\s+0", output)))
  expect_true(any(grepl("MAPNP iter\\s+:\\s+0", output)))
  expect_true(any(grepl("LAGL nDigit\\s+:\\s+7", output)))
  expect_true(any(grepl("BLUP nDigit\\s+:\\s+13", output)))
  expect_true(any(grepl(
    "Linearization step size\\s+:\\s+0\\.002", output
  )))

  # ODE Options
  expect_true(any(grepl(
    "ODE relative tolerance\\s+:\\s+1e-06", output
  )))
  expect_true(any(grepl(
    "ODE absolute tolerance\\s+:\\s+1e-06", output
  )))
  expect_true(any(grepl("ODE max steps\\s+:\\s+5000", output))) # From prototype

  # Standard Errors (Defaults)
  expect_true(any(grepl(
    "Standard Errors Method\\s+:\\s+Hessian", output
  ))) # Based on prototype xstderr=1, sand/fisher/auto empty
  expect_true(any(
    grepl(
      "Finite Difference Method\\s+:\\s+Central Difference",
      output
    )
  )) # Based on xstderr=1
  expect_true(any(grepl("Step size\\s+:\\s+0\\.01", output))) # Based on prototype xlatol

  # Should NOT see QRPEM options by default
  expect_false(any(grepl("QRPEM Options", output)))
  # Should NOT see Individual options by default
  expect_false(any(grepl("Partial deriv step size", output)))
})

# --- engineParams Wrapper Tests ---

test_that("engineParams: Wrapper basic population model (pkmodel)", {
  skip_on_cran()
  params <-
    engineParams(
      pop_model,
      ODE = "AutoDetect",
      numIterations = 10000,
      maxStepsODE = 60000
    )
  output <- capture.output(print(params))

  # Check object slots directly
  expect_true(params@isPopulation)
  expect_equal(params@method, 5)       # Default FOCE-ELS
  expect_equal(params@xfocehess, 1)    # Default FOCE-ELS
  expect_equal(params@odeToUse, 5)     # AutoDetect
  expect_equal(params@numIterations, 10000)
  expect_equal(params@nmxstep, 60000)
  expect_equal(params@sand, "  -sand ") # Default SE for FOCE-ELS is Sandwich
  expect_equal(params@xstderr, 1)      # Default is central diff

  # Check print output
  expect_true(any(grepl("Is population\\s+:\\s+TRUE", output)))
  expect_true(any(grepl("Engine used\\s+:\\s+FOCE-ELS", output))) # Default
  expect_true(any(grepl("Max iterations\\s+:\\s+10000", output)))
  expect_true(any(grepl("ODE max steps\\s+:\\s+60000", output)))
  expect_true(any(grepl(
    "Standard Errors Method\\s+:\\s+Sandwich", output
  ))) # Default for FOCE-ELS
  expect_true(any(
    grepl(
      "Finite Difference Method\\s+:\\s+Central Difference",
      output
    )
  ))
})

test_that("engineParams: Wrapper basic individual model (pkmodel)", {
  skip_on_cran()

  params <-
    engineParams(ind_model, rtolODE = 1e-7, stepSizePartialDeriv = 5e-6)
  output <- capture.output(print(params))

  # Check object slots directly
  expect_false(params@isPopulation)
  expect_equal(params@method, 6) # Naive-Pooled
  expect_equal(params@odeToUse, 6) # Default Matrix Exponent
  expect_equal(params@numIterations, 1000) # Default
  expect_equal(params@rtol, 1e-7)
  expect_equal(params@parderd, 5e-6)
  expect_equal(params@xstderr, 1) # Default SE for Ind is Hessian (Central)
  expect_equal(params@sand, "")
  expect_equal(params@fisher, "")
  expect_equal(params@autodetect, "")
  expect_equal(params@xlatol, 0.001) # Default SE step for Individual

  # Check print output
  expect_true(any(grepl("Is population\\s+:\\s+FALSE", output)))
  expect_true(any(grepl(
    "Engine used\\s+:\\s+NAIVE-POOLED", output
  )))
  expect_true(any(grepl(
    "ODE solver\\s+:\\s+Matrix Exponent", output
  )))
  expect_true(any(grepl(
    "ODE relative tolerance\\s+:\\s+1e-07", output
  )))
  expect_true(any(grepl(
    "Partial deriv step size\\s+:\\s+5e-06", output
  )))
  expect_true(any(grepl(
    "Standard Errors Method\\s+:\\s+Hessian", output
  ))) # Default for Ind
  expect_true(any(
    grepl(
      "Finite Difference Method\\s+:\\s+Central Difference",
      output
    )
  ))
  # Step size for SE not printed for individual models in the current print method
  expect_false(any(grepl("^Step size\\s+:", output)))
})


# --- engineParams Validation Error Tests ---

test_that("engineParams: Validation errors (stop)", {
  skip_on_cran()
  expect_error(engineParams(pop_model, numIterations = -1), regexp = "`numIterations` must be a non-negative integer")
  expect_error(engineParams(pop_model, numIterations = 10.5), regexp = "`numIterations` must be a non-negative integer")
  expect_error(engineParams(pop_model, numIterations = 10001), regexp = "`numIterations` must be <= 10000")
  expect_error(engineParams(pop_model, stdErr = "Invalid"), regexp = "`stdErr` must be one of")
  expect_error(engineParams(ind_model, stdErr = "Sandwich"), regexp = "For individual models, `stdErr` must be 'Hessian' or 'None'")
  expect_error(engineParams(pop_model, method = "QRPEM", stdErr = "Hessian"),
               regexp = "For population models with `QRPEM`, `stdErr` must be 'Fisher-Score' or 'None'")
  expect_error(engineParams(pop_model, method = "IT2S-EM", stdErr = "Sandwich"),
               regexp = "For `IT2S-EM`, `stdErr` must be 'None'")
  expect_error(engineParams(pop_model, stepSizeStdErr = -1), regexp = "`stepSizeStdErr` must be a positive number")
  expect_error(engineParams(pop_model, numIterNonParametric = -1),
               regexp = "`numIterNonParametric` must be a non-negative integer")
  expect_error(engineParams(pop_model, numRepPCWRES = -1), regexp = "`numRepPCWRES` must be an integer between")
  expect_error(engineParams(pop_model, stepSizeLinearize = -0.1),
               regexp = "`stepSizeLinearize` must be positive")
  expect_error(engineParams(pop_model, numDigitLaplacian = 0), regexp = "`numDigitLaplacian` must be a natural number")
  expect_error(engineParams(pop_model, numDigitBlup = 1.5), regexp = "`numDigitBlup` must be a natural number")
  expect_error(engineParams(pop_model, method = "QRPEM", mapAssist = -1),
               regexp = "`mapAssist` must be a non-negative integer")
  expect_error(engineParams(pop_model, method = "QRPEM", iSample = 0),
               regexp = "`iSample` must be a natural number")
  expect_error(engineParams(pop_model, method = "QRPEM", iAcceptRatio = 0),
               regexp = "`iAcceptRatio` must be a positive number")
  expect_error(engineParams(pop_model, method = "QRPEM", impDist = "Invalid"),
               regexp = "`impDist` must be one of")
  expect_error(engineParams(
    pop_model,
    method = "QRPEM",
    impDist = "T",
    tDOF = 2
  ),
  regexp = "`tDOF` must be a natural number between 3 and 30")
  expect_error(engineParams(pop_model, method = "QRPEM", numSampleSIR = 0),
               regexp = "`numSampleSIR` must be a natural number")
  expect_error(engineParams(pop_model, method = "QRPEM", numBurnIn = -1),
               regexp = "`numBurnIn` must be a non-negative integer")
  expect_error(engineParams(pop_model, method = "QRPEM", scramble = "Invalid"),
               regexp = "`scramble` must be one of")
  expect_error(engineParams(pop_model, method = "QRPEM", emTolType = 4),
               regexp = "`emTolType` must be 0, 1, 2, or 3")
  expect_error(engineParams(ind_model, stepSizePartialDeriv = 0),
               regexp = "`stepSizePartialDeriv` must be a positive number")
  expect_error(engineParams(ind_model, numTimeStepPartialDeriv = 0),
               regexp = "`numTimeStepPartialDeriv` must be a positive integer")
  expect_error(engineParams(pop_model_reset, sort = TRUE), regexp = "Model has reset info; set `sort = FALSE`")
  expect_error(engineParams(pop_model, ODE = "Invalid"), regexp = "`ODE` must be one of")

})

# --- engineParams Validation Warning Tests ---

test_that("engineParams: Validation warnings (warn)", {
  skip_on_cran()
  # Warnings for ignored pop args in individual model
  expect_warning(engineParams(ind_model, numIntegratePtsAGQ = 5),
                 regexp = "only applicable for population models.*numIntegratePtsAGQ")
  expect_warning(engineParams(ind_model, mapAssist = 5), regexp = "only applicable for population models.*mapAssist") # Should be caught by general pop arg check
  expect_warning(engineParams(ind_model, gradTolOuter = 0.01), regexp = "only applicable for population models.*gradTolOuter") # Should be caught by general pop arg check

  # Warnings for ignored method-specific args in population model
  expect_warning(engineParams(pop_model, method = "FO", gradTolOuter = 0.01),
                 regexp = "tolerance arguments.*only applicable.*FOCE-ELS/LAPLACIAN.*gradTolOuter")
  expect_warning(engineParams(pop_model, method = "FO", mapAssist = 5),
                 regexp = "arguments are only applicable when `method` is 'QRPEM'.*mapAssist")
  expect_warning(engineParams(pop_model, method = "FO", emTolType = 1),
                 regexp = "QRPEM convergence arguments.*only applicable.*method.* is 'QRPEM'.*emTolType")
  expect_warning(engineParams(pop_model, method = "FOCE-ELS", mapAssist = 5),
                 regexp = "arguments are only applicable when `method` is 'QRPEM'.*mapAssist")
  expect_warning(engineParams(pop_model, method = "QRPEM", gradTolOuter = 0.01),
                 regexp = "tolerance arguments.*only applicable.*FOCE-ELS/LAPLACIAN.*gradTolOuter")

  # Warning for fastOptimization with wrong method
  expect_warning(engineParams(pop_model, method = "QRPEM", fastOptimization = TRUE),
                 regexp = "fastOptimization is supported by FOCE-ELS/LAPLACIAN methods only")

  # Warning for QRPEM convergence args ignored when emTolType=0
  expect_warning(engineParams(
    pop_model,
    method = "QRPEM",
    emTolType = 0,
    emConvLen = 20
  ),
  regexp = "emConvLen and emConvCritVal are ignored when emTolType is 0")
  expect_warning(
    engineParams(
      pop_model,
      method = "QRPEM",
      emTolType = 0,
      emConvCritVal = 3
    ),
    regexp = "emConvLen and emConvCritVal are ignored when emTolType is 0"
  )
  expect_warning(
    engineParams(
      pop_model,
      method = "QRPEM",
      emTolType = 0,
      emConvLen = 20,
      emConvCritVal = 3
    ),
    regexp = "emConvLen and emConvCritVal are ignored when emTolType is 0"
  )
  # No warning if emTolType != 0
  expect_warning(engineParams(
    pop_model,
    method = "QRPEM",
    emTolType = 1,
    emConvLen = 20
  ),
  regexp = NA) # NA means no warning expected


})

# --- engineParams Specific Settings Tests ---

test_that("engineParams: Method settings and interactions", {
  skip_on_cran()
  # Laplacian
  params_lap <- engineParams(pop_model, method = "Laplacian")
  expect_equal(params_lap@method, 5)
  expect_equal(params_lap@xfocehess, 0) # Key difference from FOCE-ELS
  expect_match(capture.output(print(params_lap)),
               "Engine used\\s+:\\s+LAPLACIAN",
               all = FALSE)

  # QRPEM
  params_qr <- engineParams(pop_model, method = "QRPEM")
  expect_equal(params_qr@method, 1)
  expect_equal(params_qr@xfocehess, 1) # Should revert to default 1 if not Laplacian
  expect_equal(params_qr@sand, "") # Default SE for QRPEM is Fisher
  expect_equal(params_qr@fisher, "  -fscore ")
  expect_match(capture.output(print(params_qr)), "Engine used\\s+:\\s+QRPEM", all = FALSE)
  expect_match(capture.output(print(params_qr)),
               "Standard Errors Method\\s+:\\s+Fisher Score",
               all = FALSE)
  expect_match(capture.output(print(params_qr)), "QRPEM Options", all = FALSE) # Check section appears
})

test_that("engineParams: Standard Error settings", {
  skip_on_cran()
  # SE None
  params_none <- engineParams(pop_model, stdErr = "None")
  expect_equal(params_none@xstderr, 0)
  expect_match(capture.output(print(params_none)),
               "Standard Errors Method\\s+:\\s+None",
               all = FALSE)
  expect_false(any(grepl(
    "Finite Difference Method", capture.output(print(params_none))
  )))

  # SE Hessian + Forward Diff
  params_hess_fwd <-
    engineParams(pop_model,
                 stdErr = "Hessian",
                 isCentralDiffStdErr = FALSE)
  expect_equal(params_hess_fwd@xstderr, 2) # Forward diff
  expect_equal(params_hess_fwd@sand, "")
  expect_equal(params_hess_fwd@fisher, "")
  expect_equal(params_hess_fwd@autodetect, "")
  expect_match(capture.output(print(params_hess_fwd)),
               "Standard Errors Method\\s+:\\s+Hessian",
               all = FALSE)
  expect_match(
    capture.output(print(params_hess_fwd)),
    "Finite Difference Method\\s+:\\s+Forward Difference",
    all = FALSE
  )

  # SE Sandwich (default for FOCE-ELS)
  params_sand <-
    engineParams(pop_model, method = "FOCE-ELS", stdErr = "Sandwich")
  expect_equal(params_sand@sand, "  -sand ")
  expect_equal(params_sand@xstderr, 1) # Default diff method is central
  expect_match(capture.output(print(params_sand)),
               "Standard Errors Method\\s+:\\s+Sandwich",
               all = FALSE)

  # SE Fisher (default for QRPEM)
  params_fish <-
    engineParams(pop_model, method = "QRPEM", stdErr = "Fisher-Score")
  expect_equal(params_fish@fisher, "  -fscore ")
  expect_match(capture.output(print(params_fish)),
               "Standard Errors Method\\s+:\\s+Fisher Score",
               all = FALSE)

  # SE Auto-Detect
  params_auto <-
    engineParams(pop_model, stdErr = "Auto-Detect")
  expect_equal(params_auto@autodetect, "  -AutoSE ")
  expect_match(capture.output(print(params_auto)),
               "Standard Errors Method\\s+:\\s+Auto-Detect",
               all = FALSE)

  # SE Step Size
  params_step <- engineParams(pop_model, stepSizeStdErr = 0.05)
  expect_equal(params_step@xlatol, 0.05)
  expect_match(capture.output(print(params_step)), "Step size\\s+:\\s+0\\.05", all =
                 FALSE)

  params_step_ind <-
    engineParams(ind_model, stepSizeStdErr = 0.005)
  expect_equal(params_step_ind@xlatol, 0.005)
  # Should not be printed for individual
  expect_false(any(grepl(
    "^Step size\\s+:", capture.output(print(params_step_ind))
  )))
})


test_that("engineParams: QRPEM specific settings", {
  skip_on_cran()
  params_qr <- engineParams(
    pop_model,
    method = "QRPEM",
    iSample = 500,
    mapAssist = 10,
    impDist = "T",
    tDOF = 5,
    numSampleSIR = 20,
    numBurnIn = 50,
    freezeOmega = TRUE,
    MCPEM = TRUE,
    runAllIterations = TRUE,
    scramble = "None",
    emTolType = 1,
    emConvLen = 8,
    emConvCritVal = 3.5
  )

  expect_equal(params_qr@method, 1)
  expect_equal(params_qr@xisample, 500)
  expect_equal(params_qr@xmapassist, 10)
  expect_equal(params_qr@ximpsampdof, 5) # T with 5 DOF
  expect_equal(params_qr@xsirsamp, 20)
  expect_equal(params_qr@xburnin, 50)
  expect_equal(params_qr@xnonomegaburn, 1) # Freeze TRUE
  expect_equal(params_qr@xmcpem, 1) # MCPEM TRUE
  expect_equal(params_qr@xpemrunall, 1) # RunAll TRUE
  expect_equal(params_qr@xscramble, 0) # None
  expect_equal(params_qr@emTolType, 1)
  expect_equal(params_qr@emConvLen, 8)
  expect_equal(params_qr@emConvCritVal, 3.5)

  output <- capture.output(print(params_qr))
  expect_match(output, "MAP assistance enabled\\s+:\\s+TRUE", all = FALSE)
  expect_match(output, "Period to MAP assistance\\s+:\\s+10", all = FALSE)
  expect_match(output, "Sample points\\s+:\\s+500", all = FALSE)
  expect_match(output,
               "Importance sampling distr\\s+:\\s+Multivariate T",
               all = FALSE)
  expect_match(output, "Degrees of freedom\\s+:\\s+5", all = FALSE)
  expect_match(output, "SIR samples\\s+:\\s+20", all = FALSE)
  expect_match(output, "Burn-in iter\\s+:\\s+50", all = FALSE)
  expect_match(output, "Freeze omega during burn-in\\s+:\\s+TRUE", all = FALSE)
  expect_match(output, "Monte-Carlo sampling\\s+:\\s+TRUE", all = FALSE)
  expect_match(output, "Run all iterations\\s+:\\s+TRUE", all = FALSE)
  expect_match(output, "QR Scrambling method\\s+:\\s+None", all = FALSE)
  expect_match(output,
               "Conv\\. check type\\s+:\\s+LL & Params with rollout",
               all = FALSE) # emTolType = 1
  expect_match(output, "Conv\\. iterations\\s+:\\s+8", all = FALSE)
  expect_match(output, "Conv\\. crit\\. value\\s+:\\s+3\\.5", all = FALSE)
})

test_that("engineParams: ELS/Laplacian specific settings", {
  skip_on_cran()
  params_els <- engineParams(
    pop_model,
    method = "FOCE-ELS",
    numIntegratePtsAGQ = 3,
    fastOptimization = TRUE,
    numDigitLaplacian = 8,
    numDigitBlup = 12,
    gradTolOuter = 1e-5,
    stepTolOuter = 1e-5,
    gradTolInner = 1e-6,
    stepTolInner = 1e-8,
    refDeltaLagl = 1e-4
  )

  expect_equal(params_els@method, 5)
  expect_equal(params_els@xfocehess, 1)
  expect_equal(params_els@xnorderagq, 3)
  expect_equal(params_els@anagrad, 1) # ADPO TRUE
  expect_equal(params_els@xlandig, 8)
  expect_equal(params_els@xblndig, 12)
  expect_equal(params_els@gradTolOuter, 1e-5)
  expect_equal(params_els@stepTolOuter, 1e-5)
  expect_equal(params_els@gradTolInner, 1e-6)
  expect_equal(params_els@stepTolInner, 1e-8)
  expect_equal(params_els@refDeltaLagl, 1e-4)

  output <- capture.output(print(params_els))
  expect_match(output, "Integration points\\s+:\\s+3", all = FALSE)
  expect_match(output, "fastOptimization\\s+:\\s+TRUE", all = FALSE)
  expect_match(output, "LAGL nDigit\\s+:\\s+8", all = FALSE)
  expect_match(output, "BLUP nDigit\\s+:\\s+12", all = FALSE)
  expect_match(output, "Outer Grad Tolerance\\s+:\\s+1e-05", all = FALSE)
  expect_match(output, "Outer Step Tolerance\\s+:\\s+1e-05", all = FALSE)
  expect_match(output, "Inner Grad Tolerance\\s+:\\s+1e-06", all = FALSE)
  expect_match(output, "Inner Step Tolerance\\s+:\\s+1e-08", all = FALSE)
  expect_match(output, "LL Delta Tolerance\\s+:\\s+1e-04", all = FALSE)
})

test_that("engineParams: Sorting logic", {
  skip_on_cran()
  # Default sort=TRUE for model without reset
  params_sort_def <- engineParams(pop_model, logTransform = FALSE)
  expect_equal(params_sort_def@sort, " -sort ")
  expect_match(capture.output(print(params_sort_def)), "Sort input data\\s+:\\s+TRUE", all = FALSE)

  # Explicit sort=FALSE
  params_sort_f <- engineParams(pop_model, sort = FALSE)
  expect_equal(params_sort_f@sort, "")
  expect_match(capture.output(print(params_sort_f)),
               "Sort input data\\s+:\\s+FALSE",
               all = FALSE)

  # Default sort=FALSE for model with reset
  params_sort_reset <- engineParams(pop_model_reset)
  expect_equal(params_sort_reset@sort, "")
  expect_match(capture.output(print(params_sort_reset)),
               "Sort input data\\s+:\\s+FALSE",
               all = FALSE)
})

# Re-run existing tests 5 & 6 which test print output via direct S4 creation
# Test 5: Verify printed output for FOCE-ELS/Laplacian convergence tolerances (Direct S4)
test_that(
  "NlmeEngineExtraParams: Print method outputs FOCE-ELS/Laplacian convergence tolerances (Direct S4)",
  {
    params <- NlmeEngineExtraParams(
      method = 5,
      # Assume FOCE-ELS via xfocehess=1 default
      gradTolOuter = 3e-4,
      stepTolOuter = 2e-4,
      gradTolInner = 2e-5,
      stepTolInner = 1e-7,
      refDeltaLagl = 2e-3
    )
    output <- capture.output(print(params))
    expect_true(any(grepl(
      "Outer Grad Tolerance\\s+:\\s+3e-04", output
    )))
    expect_true(any(grepl(
      "Outer Step Tolerance\\s+:\\s+2e-04", output
    )))
    expect_true(any(grepl(
      "Inner Grad Tolerance\\s+:\\s+2e-05", output
    )))
    expect_true(any(grepl(
      "Inner Step Tolerance\\s+:\\s+1e-07", output
    )))
    expect_true(any(grepl(
      "LL Delta Tolerance\\s+:\\s+0.002", output
    ))) # Check formatting
  }
)

# Test 6: Verify printed output for QRPEM convergence options (Direct S4)
test_that("NlmeEngineExtraParams: Print method outputs QRPEM convergence options (Direct S4)",
          {
            params <- NlmeEngineExtraParams(
              method = 1,
              emTolType = 2,
              emConvLen = 15,
              emConvCritVal = 6
            )
            output <- capture.output(print(params))
            expect_true(any(grepl("QRPEM Options", output))) # Ensure section header is present
            expect_true(any(grepl(
              "Conv\\. check type\\s+:\\s+LL with rollout", output
            )))
            expect_true(any(grepl(
              "Conv\\. iterations\\s+:\\s+15", output
            )))
            expect_true(any(grepl(
              "Conv\\. crit\\. value\\s+:\\s+6", output
            )))
          })

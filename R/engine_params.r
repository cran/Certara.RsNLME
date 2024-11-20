.load_engineParams <- function(model, params, ellipsisArgs) {
  engineParamsNames <-
    intersect(names(as.list(args(engineParams))), names(ellipsisArgs))
  if (missing(params)) {
    if (length(engineParamsNames) == 0) {
      params <- Certara.RsNLME::engineParams(model)
    } else {
      engineParamsArgs <- ellipsisArgs[engineParamsNames]
      engineParamsArgs$model <- model
      params <-
        do.call(eval(parse(text = "Certara.RsNLME::engineParams")), engineParamsArgs)
    }
  } else {
    if (!inherits(params, "NlmeEngineExtraParams")) {
      "The argument given is not NlmeEngineExtraParams class. Please check the syntax."
    }

    if (length(engineParamsNames) > 0) {
      engineParamsArgs <- ellipsisArgs[engineParamsNames]
      engineParamsArgs$model <- model
      params <-
        do.call(eval(parse(text = "Certara.RsNLME::engineParams")), engineParamsArgs)
      warning(
        "Engine arguments are supplied through both params argument and additional argument.",
        "\nThe ones defined through params argument will be ignored.",
        call. = FALSE,
        immediate. = TRUE
      )
    }
  }

  params
}

#' NlmeEngineExtraParams : Defines all extra engine parameters
#'
#' @slot isPopulation is this a population model
#' @slot method what engine to use
#' QRPEM = 1|IT2S-EM = 2|FOCE-LB = 3|FO = 4|
#'        LAPLACIAN = 5|NAIVE-POOLED = 6
#' The only difference between FOCE-ELS and Laplacian is the setup for the other parameter "PARAMS_FOCEHESE".
#' If FOCE-ELS is chosen, PARAMS_FOCEHESE = 1. If Laplacian is chosen, PARAMS_FOCEHESE = 0. PARAMS_FOCEHESE arg
#' should not be provided, and is controlled by the method.
#' @slot numIterations maximum number of iterations
#' @slot odeToUse What ODE to use
#'        common : ODE_STIFF=2|ODE_NON_STIFF=3|ODE_AUTO_DETECT=5|MATRIX_EXP=6
#'        others : ODE_LSODE=1|ODE_LSODE_ANA_JAC=2|ODE_RK=3|ODE_LSODA=4|
#'                 ODE_LSODA_ANA_JAC=5|MATRIX_EXP=6
#' @slot anagrad n  0, or 1 to allow analytic gradients (default 0)
#' @slot xnp  n  number of nonparametric generations (or 0)
#' @slot xrestart n     0, or 1 to restart
#' @slot xnorderagq n   number of AGQ points per axis (or 0)
#' @slot xfocehess n    0 for numerical hessian, 1 for foce
#' @slot xstderr n    0=none, 1=central, 2=forward
#' @slot xlameth n    LAGL Method 1, 2, or 3
#' @slot xlandig n      LAGL nDigit
#' @slot xlatol n       LAGL tolerance
#' @slot xblndig n      BLUP Method 1, 2, or 3
#' @slot xblndig n      BLUP nDigit
#' @slot xbltol n       BLUP tolerance
#' @slot xpcwresnrep n  number of reps for PCWRES simulation
#' @slot xisample n     number of samples for QRPEM
#' @slot xmapassist n   0 (default) or >0, to enable map assist and specify periodicity
#' @slot xmapnp n       map naive-pool control
#' @slot ximpsampdof n  importance sampling control (-3 to 30)
#'        NORMAL=0
#'        DBL_EXP=1
#'        DIRECT=2
#'        MIXTURE_2=-2
#'        MIXTURE_3=-3
#' @slot xmcpem n       0 for QRPEM, 1 for MCPEM
#' @slot xsirsamp n     number of SIR samples
#' @slot xburnin n      number of burn-in samples
#' @slot xnonomegaburn n   0 (default) or 1, to use non-omega burn-in
#' @slot xnoburn n   0 (default) or 1, to suppress burn-in
#' @slot xstartfromsavedposteriors n   0 (default) or 1, to start from saved posteriors
#' @slot xaccratio n    acceptance ratio
#' @slot xscramble n    0 for None, 1 for Owen, 2 for Faure-Tezuka
#' @slot pardern n      partial derivative # steps
#' @slot parderd n      partial derivative delta
#' @slot logtran n      0, or 1(default) to enable log-translate of data
#'
#' @export NlmeEngineExtraParams
#'
#' @examples
#' param <- NlmeEngineExtraParams(
#'   method = 3,
#'   numIterations = 1000
#' )
#' param <- NlmeEngineExtraParams(
#'   method = 1,
#'   numIterations = 300
#' )
#' param <- NlmeEngineExtraParams(
#'   method = 1,
#'   numIterations = 300,
#'   isPopulation = TRUE,
#'   odeToUse = 2
#' )
#' @keywords internal
NlmeEngineExtraParams <- setClass(
  "NlmeEngineExtraParams",
  slots = c(
    isPopulation = "logical",
    isPCWRES = "numeric",
    isQRPEMStyleMethod = "numeric",
    method = "numeric",
    numIterations = "numeric",
    odeToUse = "numeric",
    scenarios = "character",
    xnp = "numeric",
    anagrad = "numeric",
    logtran = "numeric",
    xrestart = "numeric",
    xnorderagq = "numeric",
    xfocehess = "numeric",
    xmapnp = "numeric",
    xpcwresnrep = "numeric",
    xstderr = "numeric",
    xlameth = "numeric",
    xlandig = "numeric",
    xlatol = "numeric",
    xblmeth = "numeric",
    xblndig = "numeric",
    xbltol = "numeric",
    parderd = "numeric",
    pardern = "numeric",
    rtol = "numeric",
    atol = "numeric",
    xisample = "numeric",
    xmapassist = "numeric",
    ximpsampdof = "numeric",
    xmcpem = "numeric",
    xpemrunall = "numeric",
    xsirsamp = "numeric",
    xburnin = "numeric",
    xnonomegaburn = "numeric",
    xnoburn = "numeric",
    nmxstep = "numeric",
    sort = "character",
    csv = "character",
    sand = "character",
    fisher = "character",
    autodetect = "character",
    xstartfromsavedposteriors = "numeric",
    xaccratio = "numeric",
    xscramble = "numeric"
  ),
  prototype = list(autodetect = "")
) -> NlmeEngineExtraParams


setMethod("initialize", "NlmeEngineExtraParams",
          function(.Object,
                   isPopulation = TRUE,
                   isPCWRES = 0,
                   method = 5,
                   numIterations = 1000,
                   odeToUse = 6,
                   scenarios = "",
                   xnp = 0,
                   anagrad = 0,
                   logtran = 1,
                   xrestart = 0,
                   xnorderagq = 1,
                   xfocehess = 1,
                   xmapnp = 0,
                   xpcwresnrep = 1,
                   xstderr = 1,
                   xlameth = 1,
                   xlandig = 7,
                   xlatol = 0.01,
                   xblmeth = 1,
                   xblndig = 13,
                   xbltol = 0.002,
                   parderd = 1E-05,
                   pardern = 20,
                   rtol = 1E-06,
                   atol = 1E-06,
                   xisample = 300,
                   xmapassist = 0,
                   ximpsampdof = 0,
                   xmcpem = 0,
                   xpemrunall = 0,
                   xsirsamp = 10,
                   xburnin = 0,
                   xnonomegaburn = 0,
                   xnoburn = 0,
                   nmxstep = 5000,
                   sort = " -sort ",
                   csv = " -csv ",
                   sand = "",
                   fisher = "",
                   autodetect = "",
                   xstartfromsavedposteriors = 0,
                   xaccratio = 0.1,
                   xscramble = 1) {
            if (method == 1 || method == 2) {
              isQRPEMStyleMethod <- 1
            } else {
              isQRPEMStyleMethod <- 0
            }
            if (sort != "") {
              sort <- "  -sort "
            }
            if (csv != "") {
              csv <- "  -csv "
            }
            if (sand != "") {
              sand <- "  -sand "
            }
            if (fisher != "") {
              fisher <- "  -fscore "
            }
            if (autodetect != "") {
              autodetect <- "  -AutoSE "
            }
            .Object@method <- method
            .Object@isPopulation <- isPopulation
            .Object@isQRPEMStyleMethod <- isQRPEMStyleMethod
            .Object@isPCWRES <- isPCWRES
            .Object@numIterations <- numIterations
            .Object@odeToUse <- odeToUse
            .Object@scenarios <- scenarios
            .Object@xnp <- xnp
            .Object@anagrad <- anagrad
            .Object@logtran <- logtran
            .Object@xrestart <- xrestart
            .Object@xnorderagq <- xnorderagq
            .Object@xfocehess <- xfocehess
            .Object@xmapnp <- xmapnp
            .Object@xpcwresnrep <- xpcwresnrep
            .Object@xstderr <- xstderr
            .Object@xlameth <- xlameth
            .Object@xlandig <- xlandig
            .Object@xlatol <- xlatol
            .Object@xblmeth <- xblmeth
            .Object@xblndig <- xblndig
            .Object@xbltol <- xbltol
            .Object@parderd <- parderd
            .Object@pardern <- pardern
            .Object@rtol <- rtol
            .Object@atol <- atol
            .Object@xisample <- xisample
            .Object@xmapassist <- xmapassist
            .Object@ximpsampdof <- ximpsampdof
            .Object@xmcpem <- xmcpem
            .Object@xpemrunall <- xpemrunall
            .Object@xsirsamp <- xsirsamp
            .Object@xburnin <- xburnin
            .Object@xnonomegaburn <- xnonomegaburn
            .Object@xnoburn <- xnoburn
            .Object@xstartfromsavedposteriors <- xstartfromsavedposteriors
            .Object@xaccratio <- xaccratio
            .Object@xscramble <- xscramble
            .Object@nmxstep <- nmxstep
            .Object@sort <- sort
            .Object@csv <- csv
            .Object@sand <- sand
            .Object@fisher <- fisher
            .Object@autodetect <- autodetect
            .Object
          })

#' Print generic for class NlmeEngineExtraParams
#'
#' Print generic for class NlmeEngineExtraParams
#'
#' @param x NlmeEngineExtraParams class instance
#' @inheritParams ellipsis::dots_used
#'
#' @examples
#' print(NlmeEngineExtraParams())
#' @export
#' @keywords internal
#' @return \code{NULL}
print.NlmeEngineExtraParams <- function(x, ...) {
  cat("\n Engine Parameters \n ------------------------------------------- \n")
  cat(paste("Is population               : ", x@isPopulation), fill = TRUE)
  sort <- !(is.null(x@sort) || x@sort == "")
  cat(paste("Sort input data             : ", sort), fill = TRUE)

  if (x@method == 1) {
    method <- "QRPEM"
  } else if (x@method == 2) {
    method <- "IT2S-EM"
  } else if (x@method == 3) {
    method <- "FOCE-LB"
  } else if (x@method == 4) {
    method <- "FO"
  } else if (x@method == 6) {
    method <- "NAIVE-POOLED"
  } else if (x@method == 5 && x@xfocehess == 0) {
    method <- "LAPLACIAN"
  } else if (x@method == 5 && x@xfocehess == 1) {
    method <- "FOCE-ELS"
  }

  solver <-
    switch(
      x@odeToUse,
      "ODE_LSODA_ANA_JAC",
      "Stiff",
      "Non-stiff DVERK",
      "ODE_LSODA",
      "Auto-detect",
      "Matrix Exponent",
      "Non-stiff DOPRI5",
      "MatExp Higham"
    )

  cat(paste("Engine used                 : ", method), fill = TRUE)
  cat(paste("Maximum number of iterations: ", x@numIterations),
      fill = TRUE)
  cat(paste("ODE solver                  : ", solver), fill = TRUE)

  cat("\n Advanced Options \n ------------------------------------------- \n")
  if (method %in% c("FOCE-ELS", "LAPLACIAN")) {
    cat(paste("Number of integration points: ", x@xnorderagq), fill = TRUE)
  }
  if (method != "NAIVE-POOLED") {
    cat(paste("Number of non-param iter    : ", x@xnp), fill = TRUE)
    cat(paste("Number of MAPNP iter        : ", x@xmapnp), fill = TRUE)
    if (x@isPCWRES) {
      cat(paste("Number of replicates PCWRES : ", x@xpcwresnrep),
          fill = TRUE)
    }
    cat(paste("Use synthetic gradients     : ", as.logical(x@anagrad)), fill = TRUE)

    cat(paste("LAGL nDigit                 : ", x@xlandig), fill = TRUE)
    cat(paste("BLUP nDigit                 : ", x@xblndig), fill = TRUE)
    cat(paste("Linearization step size     : ", x@xbltol), fill = TRUE)
  }
  if (!x@isPopulation) {
    cat(paste("Step size for partial deriv : ", x@parderd), fill = TRUE)
    cat(paste("Number of steps output deriv: ", x@pardern), fill = TRUE)
  }
  cat(paste("ODE relative tolerance      : ", x@rtol), fill = TRUE)
  cat(paste("ODE absolute tolerance      : ", x@atol), fill = TRUE)
  cat(paste("ODE max steps               : ", x@nmxstep), fill = TRUE)

  if (method == "QRPEM") {
    if (x@ximpsampdof == -3) {
      impDist <- "Mixture-3"
    } else if (x@ximpsampdof == -2) {
      impDist <- "Mixture-2"
    } else if (x@ximpsampdof == 0) {
      impDist <- "Multivariate Normal"
    } else if (x@ximpsampdof == 1) {
      impDist <- "Multivariate Laplace"
    } else if (x@ximpsampdof == 2) {
      impDist <- "Direct Sampling"
    } else  {
      impDist <- "Multivariate T"
    }

    if (x@xscramble == 0) {
      scramble <- "None"
    } else if (x@xscramble == 1) {
      scramble <- "Owen"
    } else if (x@xscramble == 2) {
      scramble <- "Tezuka-Faur"
    }

    cat("\n QRPEM Options \n ------------------------------------------- \n")
    cat(paste("MAP assistance enabled      : ", x@xmapassist > 0), fill = TRUE)
    if (x@xmapassist > 0) {
      cat(paste("Period to MAP assistance    : ", x@xmapassist), fill = TRUE)
    }
    cat(paste("Number of sample points     : ", x@xisample), fill = TRUE)
    cat(paste("Acceptance ratio            : ", x@xaccratio), fill = TRUE)

    cat(paste("Importance sampling distr   : ", impDist), fill = TRUE)
    if (impDist == "Multivariate T") {
      cat(paste("Degrees of freedom          : ", x@ximpsampdof), fill = TRUE)
    }
    cat(paste("QR Scrambling method        : ", scramble), fill = TRUE)
    cat(paste("Number of SIR samples       : ", x@xsirsamp), fill = TRUE)
    cat(paste("Number of burn-in iter      : ", x@xburnin), fill = TRUE)
    cat(paste(
      "Freeze omega during burn-in : ",
      as.logical(x@xnonomegaburn)
    ), fill = TRUE)
    cat(paste("Monte-Carlo sampling        : ", as.logical(x@xmcpem)), fill = TRUE)
    cat(paste("Run all iterations          : ", as.logical(x@xpemrunall)), fill = TRUE)
  }

  cat("\n Standard Errors \n ------------------------------------------- \n")


  if (x@sand == "  -sand ")  {
    stdErr <- "Sandwich"
  } else if (x@fisher == "  -fscore ") {
    stdErr <- "Fisher Score"
  } else if (x@autodetect == "  -AutoSE ") {
    stdErr <- "Auto-Detect"
  } else if (x@xstderr == 0) {
    stdErr <- "None"
  } else {
    stdErr <- "Hessian"
  }

  cat(paste("Standard Errors Method      : ", stdErr), fill = TRUE)
  if (stdErr != "None") {
    if (x@xstderr == 2) {
      cat(paste("Finite Difference Method    : ", "Forward Difference"),
          fill = TRUE)
    } else {
      cat(paste("Finite Difference Method    : ", "Central Difference"),
          fill = TRUE)
    }
    if (x@isPopulation) {
      cat(paste("Step size                   : ", x@xlatol), fill = TRUE)
    }
  }

  cat("\n ------------------------------------------- ")
}

setMethod("show", "NlmeEngineExtraParams",
          definition = function(object){
            print(object)
          })

#' Specify engine parameters for model execution
#'
#' Use to define extra engine parameters for model execution.
#'
#' @param model  Model object
#' @param sort Logical; Specifying whether or not to sort the input data by subject and time values.
#' \itemize{
#' \item If \code{model@@hasResetInfo = TRUE}, then \code{sort} must be set to \code{FALSE} (default);
#' \item Otherwise, the default value for \code{sort} is \code{TRUE}.
#' }
#' @param ODE Character; Specifying the solver used to numerically solve
#'   Ordinary Differential Equations (ODEs). Options are `"MatrixExponent"`,
#'   `"Higham"`, `"DVERK"`, `"DOPRI5"`, `"AutoDetect"`, `"Stiff"`. See Details
#'   section.
#' @param rtolODE Numeric; Specifying relative tolerance for the numerical ODE solver.
#' @param atolODE Numeric; Specifying absolute tolerance for the numerical ODE solver.
#' @param maxStepsODE Numeric; Specifying maximum number of allowable steps or function evaluations for the ODE solver.
#' @param numIterations  Numeric; Specifying maximum number of iterations for estimation.
#' @param method Character; Specifying engine method for estimation. For population models,
#' options are \code{"QRPEM"}, \code{"IT2S-EM"}, \code{"FOCE-LB"}, \code{"FO"},
#' \code{"FOCE-ELS"}, \code{"Laplacian"}, \code{"Naive-Pooled"}.
#' While, for individual models, \code{"Naive-Pooled"} is the only option.\cr
#'
#' Note: For population models, if \code{model} involves any discontinuous observed variable (e.g., count data) or BQL data,
#' the default method is \code{"Laplacian"}; otherwise, the default method is \code{"FOCE-ELS"}.
#' @param stdErr Character; Specifying method for standard error computations.  \cr
#' \itemize{
#' \item For individual models, options are \code{"Hessian"} (default) and \code{"None"};
#' \item For population models with \code{method = "QRPEM"}, options are \code{"Fisher-Score"} (default) and \code{"None"};
#' \item For population models with \code{method = "IT2s-EM"}, the only option is \code{"None"};
#' \item For population models with \code{method} set to either \code{"FOCE-LB"}, \code{"FO"},
#' \code{"FOCE-ELS"}, \code{"Laplacian"}, or \code{"Naive-Pooled"}, options are \code{"Sandwich"} (default),
#' \code{"Hessian"}, \code{"Fisher-Score"}, \code{"Auto-Detect"}, and \code{"None"}.
#' }
#' Here \code{"None"} means that standard error calculations are not performed.
#' @param isCentralDiffStdErr Logical; Default \code{TRUE} uses central difference for \code{stdErr} calculations.
#' Set to \code{FALSE} for forward difference method.
#' @param stepSizeStdErr Numeric; Specifying the step size used for \code{stdErr} calculations.
#' If not specified, 0.01 is used for population models and 0.001 for individual models.
#' @param numIntegratePtsAGQ Numeric; Specifying the number of integration points
#' for adaptive Gaussian quadrature (AGQ) algorithm. Only applicable to population models with
#'  \code{method} set to either \code{"FOCE-ELS"} or \code{"Laplacian"}.
#' @param numIterNonParametric Numeric;  Specifying the number of iterations to perform non-parametric estimation.
#' Only applicable to population models when \code{method} is not set to \code{Naive-Pooled}.
#' @param allowSyntheticGradient Logical, Set to \code{TRUE} to use synthetic gradient during the estimation process.
#' Only applicable to population models when \code{method} is not set to \code{Naive-Pooled}.
#' @param numIterMAPNP Numeric; Specifying the number of iterations
#' to perform Maximum A Posterior (MAP) initial Naive Pooling (NP) run before estimation.
#' Only applicable to population models when \code{method} is not set to \code{Naive-Pooled}.
#' @param numRepPCWRES Numeric; Specifying the number of replicates to generate the PCWRES
#' after the simple estimation. Only applicable to population models when \code{method} is not set to \code{Naive-Pooled}.
#' @param stepSizeLinearize Numeric; Specifying the step size used for numerical differentiation
#' when linearizing the model function during the estimation process.
#' @param numDigitLaplacian Numeric; Specifying the number of significant decimal digits for the Laplacian algorithm
#' to use to reach convergence. Only applicable to population models.
#' @param numDigitBlup Numeric; Specifying the number of significant decimal digits for the individual estimation
#' to use to reach convergence. Only applicable to population models.
#' @param mapAssist Numeric; Specifying the period used to perform MAP assistance (\code{mapAssist = 0} means that
#'  MAP assistance is not performed). Only applicable to population models with \code{method = "QRPEM"}.
#' @param iSample Numeric; Specifying the number of samples. Only applicable to population models with \code{method = "QRPEM"}.
#' @param iAcceptRatio Numeric; Specifying the acceptance ratio. Only applicable to population models with \code{method = "QRPEM"}.
#' @param impDist Character; Specifying the distribution used for important sampling, and options are
#' \code{"Normal"} (default), \code{"DoubleExponential"}, \code{"Direct"}, \code{"T"}, \code{"Mixture-2"}, \code{Mixture-3}.
#' Only applicable to population models with \code{method = "QRPEM"}.
#' @param tDOF Numeric; Specifing the degree of freedom (allowed value is between 3 and 30) for T distribution.
#' Only applicable to population models with \code{method = "QRPEM"} and \code{impDist = "T"}.
#' @param numSampleSIR Numeric; Specifying the number of samples per subject used
#' in the Sampling Importance Re-Sampling (SIR) algorithm to determine the number of SIR samples
#' taken from the empirical discrete distribution that approximates the target conditional distribution.
#' Only applicable to population models with \code{method = "QRPEM"}.
#' @param numBurnIn Numeric; Specifying the number of burn-in iterations to perform at startup
#' to adjust certain internal parameters. Only applicable to population models with \code{method = "QRPEM"}.
#' @param freezeOmega Logical; Set to \code{TRUE} to freeze Omega but not Theta for the number of iterations
#'  specified in the \code{numBurnIn}. Only applicable to population models with \code{method = "QRPEM"}.
#' @param MCPEM Logical; Set to \code{TRUE} to use Monte-Carlo sampling instead of Quasi-Random.
#' Only applicable to population models with \code{method = "QRPEM"}.
#' @param runAllIterations Logical; Set to \code{TRUE} to execute all requested iterations specified in \code{numIterations}.
#' Only applicable to population models with \code{method = "QRPEM"}.
#' @param scramble Character; Specifying the quasi-random scrambling method to use,
#' and options are \code{"Owen"}, \code{"Tezuka-Faur"}, or \code{"None"}.
#' Only applicable to population models with \code{method = "QRPEM"}.
#' @param stepSizePartialDeriv Numeric; Specifying the step size used to numerically calculate the partial derivatives
#' of observed variables with respect to parameters. Only applicable to individual models.
#' @param numTimeStepPartialDeriv Numeric; Specifying the number of time steps used to output the partial derivatives
#' of observed variables with respect to parameters. Only applicable to individual models.
#'
#' @details
#' Both `"DVERK"` and `"DOPRI5"` are non-stiff solvers. `"Higham"` is a matrix
#' exponent based ODE solver which could be useful when overscaling issue should
#' be avoided, i.e. the ratio between observed values and doses is too high or
#' too low. `"AutoDetect"` represents LSODA solver implemenation, which solves
#' the initial value problem for stiff or nonstiff systems of first order
#' ordinary differential equations. `"Stiff"` is a LSODE (Livermore solver). It
#' is best suited for stiff problems.
#'
#' @return List of engine parameters to be used during fitting or simulation
#' @export
engineParams <- function(model,
                         sort = NULL,
                         ODE = "MatrixExponent",
                         rtolODE = 1e-6,
                         atolODE = 1e-6,
                         maxStepsODE = 50000,
                         numIterations = 1000,
                         method = NULL,
                         stdErr = NULL,
                         isCentralDiffStdErr = TRUE,
                         stepSizeStdErr = NULL,
                         numIntegratePtsAGQ = 1,
                         numIterNonParametric = 0,
                         allowSyntheticGradient = FALSE,
                         numIterMAPNP = 0,
                         numRepPCWRES = 0,
                         stepSizeLinearize = 0.002,
                         numDigitLaplacian = 7,
                         numDigitBlup = 13,
                         mapAssist = 0,
                         iSample = 300,
                         iAcceptRatio = 0.1,
                         impDist = "Normal",
                         tDOF = 4,
                         numSampleSIR = 10,
                         numBurnIn = 0,
                         freezeOmega = FALSE,
                         MCPEM = FALSE,
                         runAllIterations = FALSE,
                         scramble = "Owen",
                         stepSizePartialDeriv = 1e-5,
                         numTimeStepPartialDeriv = 20)
{
  ep <- NlmeEngineExtraParams()

  if (model@isPopulation) {
    ep@isPopulation <- TRUE
  } else {
    ep@isPopulation <- FALSE
  }

  if (numIterations < 0 || numIterations %% 1 != 0) {
    stop("value supplied to argument `numIterations` must be non-negative integer")
  } else {
    ep@numIterations <- numIterations
  }


  `%notin%` <- Negate(`%in%`)
  is.natural <- function(x) {
    x > 0 && identical(round(x), x)
  }

  #Check if PML contains discont statements and assign flag
  modelDiagnostic <- createModelInfo(model = model)
  if (any(modelDiagnostic == "(allowgaussianfit 1)")) {
    hasDiscont <- FALSE
  } else {
    hasDiscont <- TRUE
  }

  # Check method ---------------------------------------------------------
  #If missing method assign default based on individual/population model
  if (is.null(method)) {
    if (model@isPopulation == FALSE) {
      method <- "Naive-Pooled"
    } else {
      if (hasDiscont) {
        method <- "Laplacian"
      } else {
        method <- "FOCE-ELS"
      }
    }
  } else {
    #If method specified, provide condition checks
    if (model@isPopulation == FALSE && method != "Naive-Pooled") {
      stop("model@isPopulation = FALSE; must specify method = 'Naive-Pooled'")
    }
    if (model@isPopulation == TRUE &&
        hasDiscont &&
        method %notin% c("Laplacian", "QRPEM", "IT2S-EM", "Naive-Pooled")) {
      stop(
        "model has discontinuous observed variables and/or BQL data;",
        " available options for `method` are 'Laplacian', 'QRPEM', 'IT2S-EM', or 'Naive-Pooled'"
      )
    }
  }

  # Check stdErr------------------------------------------------------
  if (is.null(stdErr)) {
    #Assign standard error defaults if is.null
    if (model@isPopulation == FALSE) {
      stdErr <- "Hessian"
    } else {
      if (method == "QRPEM") {
        stdErr <- "Fisher-Score"
      } else if (method %in% c("FOCE-ELS", "Laplacian", "FOCE-LB", "FO", "Naive-Pooled")) {
        stdErr <- "Sandwich"
      } else {
        stdErr <- "None"
      }
    }
  } else {
    #Check for incompatible stdErr if specified
    if (model@isPopulation == FALSE) {
      if (stdErr %notin% c("Hessian", "None")) {
        stop(
          "model@isPopulation = FALSE; available options for argument `stdErr` are 'Hessian' and 'None'"
        )
      }
    } else {
      if (method == "QRPEM" && stdErr %notin% c("Fisher-Score", "None")) {
        stop(
          "model@isPopulation = TRUE and method = 'QRPEM'; available options for argument `stdErr` are 'Fisher-Score' or 'None'"
        )
      }
      if (method == "IT2S-EM" && stdErr != "None") {
        stop(
          "model@isPopulation = TRUE and method = 'IT2S-EM'; must set argument `stdErr` = 'None'"
        )
      }
    }
  }

  #Assign xfocehess according to method, note cannot use m here as both require /m 5 for both Laplacian and FOCE-ELS
  if (!is.null(method)) {
    if (method == "Laplacian") {
      ep@xfocehess = 0
    } else {
      ep@xfocehess = 1
    }
  }

  m  <-
    .assignEngineMethod(method, model) #See helper function below engineParams()
  ep@method <- m

  if (m == 1 || m == 2) {
    ep@isQRPEMStyleMethod <- 1
  } else {
    ep@isQRPEMStyleMethod <- 0
  }

  if (model@isPopulation) {
    if (method %in% c("FOCE-ELS", "Laplacian")) {
      ep@xnorderagq <- numIntegratePtsAGQ
    } else {
      ep@xnorderagq <- 0
    }
  }

  if (model@isPopulation) {
    if (numIterNonParametric < 0 || numIterNonParametric %% 1 != 0) {
      stop(
        "value supplied to argument `numIterNonParametric` must be a non-negative integer"
      )
    } else {
      ep@xnp <- numIterNonParametric
    }
  }

  if (model@isPopulation) {
    if (numIterMAPNP < 0 || numIterMAPNP %% 1 != 0) {
      stop("value supplied to argument `numIterMAPNP` must be a non-negative integer")
    } else {
      ep@xmapnp <- numIterMAPNP
    }
  }

  if (model@isPopulation) {
    if (numRepPCWRES < 0  ||
        numRepPCWRES > 1001 || numRepPCWRES %% 1 != 0) {
      stop(
        "value supplied to argument ` numRepPCWRES` must be a non-negative integer between 0 and 1001"
      )
    } else {
      if (numRepPCWRES > 0) {
        ep@isPCWRES <- 1
        ep@xpcwresnrep <- numRepPCWRES
      } else {
        ep@isPCWRES <- 0
      }
    }
  }

  if (model@isPopulation) {
    if (stepSizeLinearize < 0) {
      stop("value supplied to argument `stepSizeLinearize` must be positive")
    } else {
      ep@xbltol <- stepSizeLinearize
    }
  }

  if (model@isPopulation) {
    if (!is.natural(numDigitLaplacian)) {
      stop("value supplied to argument `stepSizeLinearize` must be natural number")
    } else {
      ep@xlandig  <- numDigitLaplacian
    }
  }

  if (model@isPopulation) {
    if (!is.natural(numDigitBlup)) {
      stop("value supplied to argument `stepSizeLinearize` must be natural number")
    } else {
      ep@xblndig  <- numDigitBlup
    }
  }

  if (model@isPopulation && method == "QRPEM") {
    if (mapAssist < 0 || mapAssist %% 1 != 0) {
      stop("value supplied to argument `mapAssist` must be positive")
    } else {
      ep@xmapassist <- mapAssist
    }
  }

  if (model@isPopulation && method == "QRPEM") {
    if (!is.natural(iSample)) {
      stop("value supplied to argument `iSample` must be a natural number")
    } else {
      ep@xisample <- iSample
    }
  }

  if (model@isPopulation && method == "QRPEM") {
    if (iAcceptRatio <=  0) {
      stop("value supplied to argument `iAcceptRatio` must be positive")
    } else {
      ep@xaccratio <- iAcceptRatio
    }
  }

  if (method == "Naive-Pooled") {
    if (numIterNonParametric != 0) {
      stop("argument `numIterNonParametric` not applicable for `method` = 'Naive-Pooled'")
    }
    if (allowSyntheticGradient) {
      stop(
        "argument `allowSyntheticGradient` not applicable for `method` = 'Naive-Pooled'"
      )
    }
    if (numIterMAPNP != 0) {
      stop("argument `numIterMAPNP` not applicable for `method` = 'Naive-Pooled'")
    }
    if (numRepPCWRES != 0) {
      stop("argument `numRepPCWRES` not applicable for `method` = 'Naive-Pooled'")
    }
  }

  if (model@isPopulation && method == "QRPEM") {
    if (tDOF < 3 || tDOF > 30) {
      stop("value supplied to argument `tDOF` must be between 3 and 30")
    }
    if (!is.natural(tDOF)) {
      stop("value supplied to argument `tDOF` must be a natural number")
    }
    if (impDist == "Normal") {
      ep@ximpsampdof <- 0
    } else if (impDist == "DoubleExponential") {
      ep@ximpsampdof <- 1
    } else if (impDist == "Direct") {
      ep@ximpsampdof <- 2
    } else if (impDist == "T") {
      ep@ximpsampdof <- tDOF
    } else if (impDist == "Mixture-2") {
      ep@ximpsampdof <- -2
    } else if (impDist == "Mixture-3") {
      ep@ximpsampdof <- -3
    } else {
      stop(
        "available options for argument `impDist` are 'Normal', 'DoubleExponential', 'Direct', 'T', 'Mixture-2', or 'Mixture-3'"
      )
    }
  }

  if (model@isPopulation && method == "QRPEM") {
    if (!is.natural(numSampleSIR)) {
      stop("value supplied to argument `numSampleSIR` must be a natural number")
    } else {
      ep@xsirsamp <- numSampleSIR
    }
  }

  if (model@isPopulation && method == "QRPEM") {
    if (numBurnIn < 0) {
      stop("value supplied to argument `numBurnIn` must be positive")
    } else {
      ep@xburnin <- numBurnIn
    }
  }

  if (model@isPopulation && method == "QRPEM") {
    if (freezeOmega) {
      ep@xnonomegaburn <- 1
    } else {
      ep@xnonomegaburn <- 0
    }
  }

  if (model@isPopulation && method == "QRPEM") {
    if (MCPEM) {
      ep@xmcpem <- 1
    } else {
      ep@xmcpem <- 0
    }
  }

  if (model@isPopulation && method == "QRPEM") {
    if (runAllIterations) {
      ep@xpemrunall <- 1
    } else {
      ep@xpemrunall <- 0
    }
  }

  if (model@isPopulation && method == "QRPEM") {
    if (scramble == "None") {
      ep@xscramble <- 0
    } else if (scramble == "Owen") {
      ep@xscramble <- 1
    } else if (scramble == "Tezuka-Faur") {
      ep@xscramble <- 2
    } else {
      stop("argument `scramble` must be one of 'Owen', 'Tezuka-Faur', or 'None'")
    }
  }

  if (method != "QRPEM" || model@isPopulation == FALSE) {
    if (mapAssist != 0) {
      stop(
        "argument `mapAssist` only applicable for model@isPopulation = TRUE and method = 'QRPEM'"
      )
    }
    if (iSample != 300) {
      stop(
        "argument `iSample` only applicable for model@isPopulation = TRUE and method = 'QRPEM'"
      )
    }
    if (iAcceptRatio != 0.1) {
      stop(
        "argument `iSample` only applicable for model@isPopulation = TRUE and method = 'QRPEM'"
      )
    }
    if (impDist != "Normal") {
      stop(
        "argument `impDist` only applicable for model@isPopulation = TRUE and method = 'QRPEM'"
      )
    }
    if (tDOF != 4) {
      stop(
        "argument `tDOF` only applicable for model@isPopulation = TRUE and method = 'QRPEM'"
      )
    }
    if (numSampleSIR != 10) {
      stop(
        "argument `numSampleSIR` only applicable for model@isPopulation = TRUE and method = 'QRPEM'"
      )
    }
    if (numBurnIn != 0) {
      stop(
        "argument `numBurnIn` only applicable for model@isPopulation = TRUE and method = 'QRPEM'"
      )
    }
    if (freezeOmega == TRUE) {
      stop(
        "argument `freezeOmega` only applicable for model@isPopulation = TRUE and method = 'QRPEM'"
      )
    }
    if (MCPEM == TRUE) {
      stop(
        "argument `MCPEM` only applicable for model@isPopulation = TRUE and method = 'QRPEM'"
      )
    }
    if (runAllIterations == TRUE) {
      stop(
        "argument `runAllIterations` only applicable for model@isPopulation = TRUE and method = 'QRPEM'"
      )
    }
    if (scramble != "Owen") {
      stop(
        "argument `scramble` only applicable for model@isPopulation = TRUE and method = 'QRPEM'"
      )
    }
  }

  if (method %notin% c("FOCE-ELS", "Laplacian") &
      numIntegratePtsAGQ != 1) {
    stop(
      "argument `numIntegratePtsAGQ` only applicable for `method` 'FOCE-ELS' and 'Laplacian'"
    )
  }


  if (model@isPopulation == FALSE) {
    if (numIntegratePtsAGQ != 1) {
      stop("argument `numIntegratePtsAGQ` only applicable for model@isPopulation = TRUE")
    }
    if (numIterNonParametric != 0) {
      stop("argument `numIterNonParametric` only applicable for model@isPopulation = TRUE")
    }
    if (numIterMAPNP != 0) {
      stop("argument `numIterMAPNP` only applicable for model@isPopulation = TRUE")
    }
    if (numRepPCWRES != 0) {
      stop("argument `numRepPCWRES` only applicable for model@isPopulation = TRUE")
    }
    if (allowSyntheticGradient == TRUE) {
      stop(
        "argument `allowSyntheticGradient` only applicable for model@isPopulation = TRUE"
      )
    }
    if (stepSizeLinearize != 0.002) {
      stop("argument `stepSizeLinearize` only applicable for model@isPopulation = TRUE")
    }
    if (numDigitLaplacian != 7) {
      stop("argument `numDigitLaplacian` only applicable for model@isPopulation = TRUE")
    }
    if (numDigitBlup != 13) {
      stop("argument `numDigitBlup` only applicable for model@isPopulation = TRUE")
    }
  }



  o <-
    .assignODEMethod(ODE) #See helper function below engineParams()

  ep@odeToUse <- o

  ep@rtol <- rtolODE
  ep@atol <- atolODE

  ep@nmxstep <- maxStepsODE

  if (!is.null(stepSizeStdErr)) {
    if (!is.numeric(stepSizeStdErr) || stepSizeStdErr <= 0) {
      stop("value supplied for argument `stepSizeStdErr` must be positive number")
    }

    ep@xlatol <- stepSizeStdErr
  } else {
    if (model@isPopulation) {
      ep@xlatol <- 0.01
    } else {
      ep@xlatol <- 0.001
    }
  }

  if (isCentralDiffStdErr == FALSE) {
    ep@xstderr <- 2
  }

  if (is.null(stdErr)) {
    stdErr <- "None"
  }

  if (model@isPopulation && method == "IT2S-EM") {
    stdErr <- "None"
  }

  if (stdErr == "Sandwich") {
    ep@sand <- "  -sand "
  } else if (stdErr == "Fisher-Score") {
    ep@fisher <- "  -fscore "
  } else if (stdErr == "Auto-Detect") {
    ep@autodetect <- "  -AutoSE "
  } else if (stdErr == "Hessian") {
    ep@sand <- ""
    ep@fisher <- ""
    ep@autodetect <- ""
  } else if (stdErr == "None") {
    ep@xstderr <- 0
  } else {
    stop(
      "argument `stderr` must be one of 'Hessian', 'Sandwich', 'Fisher-Score', 'Auto-Detect', or 'None'"
    )
  }

  # do not add sort for the models with reset
  pattern <- "(?<=^|\\n)\\s*reset\\s*\\([^\\n]+\\)"
  colDefs <-
    paste0(unlist(model@userDefinedExtraDefs), collapse = "\n")
  ResetInfoColDef <- grepl(pattern, colDefs, perl = TRUE)

  if (!is.null(sort)) {
    if (sort) {
      if (model@hasResetInfo | ResetInfoColDef) {
        stop("model has reset info; set argument `sort` = FALSE")
      } else {
        ep@sort <- " -sort "
      }
    } else {
      ep@sort <- ""
    }
  } else {
    if (model@hasResetInfo | ResetInfoColDef) {
      ep@sort <- ""
    } else {
      ep@sort <- " -sort "
    }
  }


  if (ep@isPopulation) {
    if (stepSizePartialDeriv != 1e-5) {
      stop("argument `stepSizePartialDeriv` only applicable to model@isPopulation = FALSE")
    }
    if (numTimeStepPartialDeriv != 20) {
      stop(
        "argument `numTimeStepPartialDeriv` only applicable to model@isPopulation = FALSE"
      )
    }
  } else {
    ep@parderd <- stepSizePartialDeriv
    ep@pardern <- numTimeStepPartialDeriv
  }


  if (allowSyntheticGradient) {
    ep@anagrad <- 1
  } else {
    ep@anagrad <- 0
  }

  ep@csv <- " -csv "


  return(ep)
}

#---------------------------------------------------------------------------------------------------------------------------------------------
# Helper functions for object assignments in engineParams()
#---------------------------------------------------------------------------------------------------------------------------------------------


.assignEngineMethod <- function(method, model) {
  if (is.null(method)) {
    if (model@isPopulation == FALSE) {
      method <-
        "Naive-Pooled" #Only method available for individual models
    } else {
      method <- "QRPEM"
    }
  }

  methodMap <- list(
    "QRPEM" = 1,
    "IT2S-EM" = 2,
    "FOCE-LB" = 3,
    "FO" = 4,
    "FOCE-ELS" = 5,
    "LAPLACIAN" = 5,
    "NAIVE-POOLED" = 6
  )
  method <- toupper(method)
  if (length(methodMap[[method]]) != 1) {
    param_methods <- names(methodMap)
    stop(
      "argument `method` must be one of \"",
      paste0(param_methods, collapse = "\", \""),
      "\""
    )
  }

  methodMap[[method]]
}

.assignODEMethod <- function(ODE) {
  ODEMap <- list(
    "LSODE" = 1,
    "STIFF" = 2,
    "LSODE_ANA_JAC" = 2,
    "DVERK" = 3,
    "RK" = 3,
    "LSODA" = 4,
    "AUTODETECT" = 5,
    "LSODA_ANA_JAC" = 5,
    "MATRIXEXPONENT" = 6,
    "MATRIX_EXP" = 6,
    "DOPRI5" = 7,
    "HIGHAM" = 8
  )
  ODE <- toupper(ODE)
  if (length(ODEMap[[ODE]]) != 1) {
    ODE_methods <- names(ODEMap)
    stop("argument `ODE` must be one of \"",
         paste0(ODE_methods, collapse = "\", \""),
         "\"")
  }

  ODEMap[[ODE]]
}

.load_engineParams <- function(model, params, ellipsisArgs) {
  # Get formal argument names of the engineParams function itself
  engineParamsFunArgs <-
    names(formals(Certara.RsNLME::engineParams))
  # Find which ellipsis arguments match the engineParams function arguments
  engineParamsNames <-
    intersect(engineParamsFunArgs, names(ellipsisArgs))

  if (missing(params)) {
    if (length(engineParamsNames) == 0) {
      # No params object, no relevant ellipsis args: use defaults
      params <- Certara.RsNLME::engineParams(model)
    } else {
      # No params object, but relevant ellipsis args: call engineParams with them
      engineParamsArgs <- ellipsisArgs[engineParamsNames]
      engineParamsArgs$model <- model
      params <-
        do.call(Certara.RsNLME::engineParams, engineParamsArgs)
    }
  } else {
    # params object IS provided
    if (!inherits(params, "NlmeEngineExtraParams")) {
      # STOP execution if the provided object is wrong type
      stop("The argument 'params' must be an object of class 'NlmeEngineExtraParams'.")
    }

    if (length(engineParamsNames) > 0) {
      # Both params object AND relevant ellipsis args provided. Prioritize ellipsis args.
      warning(
        "Engine arguments were supplied via both the 'params' object and individual arguments (...). ",
        "Arguments from the 'params' object will be ignored in favor of the individual arguments.",
        call. = FALSE,
        immediate. = TRUE
      )
      engineParamsArgs <- ellipsisArgs[engineParamsNames]
      engineParamsArgs$model <- model
      # Use direct call instead of eval(parse(...))
      params <-
        do.call(Certara.RsNLME::engineParams, engineParamsArgs)
    }
  }

  params
}

#' NlmeEngineExtraParams : Defines all engine parameters for NLME models
#' Wrapped up by \code{engineParams} function.
#'
#' @slot isPopulation logical; TRUE if the model is a population model, FALSE
#'   for an individual model.
#' @slot sort character; String to pass sorting options to the NLME engine.
#'   Typically " -sort " to enable sorting or "" to disable it.
#' @slot csv character; String to control CSV input options. Typically " -csv "
#'   to strict the input data to comma-separated-values. Use "" to disable it
#'   (NLME will try to guess the input format).
#' @slot method numeric; Integer code specifying the estimation method.
#' \itemize{
#'   \item 1: QRPEM
#'   \item 2: IT2S-EM
#'   \item 3: FOCE-LB
#'   \item 4: FO
#'   \item 5: FOCE-ELS/LAPLACIAN (see below for choice between FOCE-ELS and LAPLACIAN)
#'   \item 6: NAIVE-POOLED
#' }
#'   The choice between FOCE-ELS and LAPLACIAN (when \code{method} is 5) depends
#'   on the \code{xfocehess} slot: \code{xfocehess = 1} selects FOCE-ELS, and
#'   \code{xfocehess = 0} selects LAPLACIAN.
#' @slot numIterations numeric; The maximum number of iterations allowed for the
#'   estimation algorithm.  Values must be non-negative integers.
#' @slot odeToUse numeric; Integer code specifying the ODE solver to be used.
#'   Possible values are:
#' \itemize{
#'   \item 1: LSODE with numerical Jacobian
#'   \item 2: LSODE with analytical Jacobian
#'   \item 3: Runge-Kutta
#'   \item 4: LSODA with numerical Jacobian
#'   \item 5: LSODA with analytical Jacobian
#'   \item 6: Matrix Exponent
#'   \item 7: DOPRI5
#' }
#' @slot rtol numeric; Specifies the relative tolerance for the ODE solver.
#' @slot atol numeric; Specifies the absolute tolerance for the ODE solver.
#' @slot nmxstep numeric; Specifies the maximum number of steps allowed for the
#'   ODE solver.
#' @slot anagrad numeric; Flag controlling the differentiation method used
#'   during the optimization of random effects (etas). 0 uses a finite
#'   difference approach, and 1 uses automatic differentiation where possible.
#' @slot xnp numeric; Controls the use of non-parametric (NP) optimization.
#' \itemize{
#'   \item 0: No NP optimization.
#'   \item 1: NONMEM-style NP optimization using posthoc estimates as support points.
#'   \item >1: Evolutionary NP algorithm with \code{xnp} generations.
#' }
#' @slot xnorderagq numeric; Specifies the number of quadrature points per
#'   dimension for Adaptive Gaussian Quadrature (AGQ). Only applicable when
#'   \code{method} is \code{FOCE-ELS} or \code{LAPLACIAN}.
#' \itemize{
#'   \item 1: Standard FOCE-ELS/LAPLACIAN computation (no AGQ).
#'   \item >1: AGQ is performed. The total number of quadrature points used is \code{(number of ETAs)^xnorderagq}.
#' }
#' @slot xfocehess numeric; Determines the method for calculating the Hessian
#'   matrix when using FOCE methods.
#' \itemize{
#'   \item 0: Use numerical second derivatives.
#'   \item 1: Use the FOCE approximation.
#' }
#'   Applicable only when \code{method} is \code{FOCE-ELS} or \code{LAPLACIAN}.
#' @slot xstderr numeric; Specifies the method for standard error estimation.
#' \itemize{
#'   \item 0: No standard error estimation.
#'   \item 1: Central difference method.
#'   \item 2: Forward difference method.
#' }
#' @slot sand character; String to request sandwich standard error calculation.
#'   Typically " -sand " or "".
#' @slot fisher character; String to request Fisher score standard error
#'   calculation. Typically " -fscore " or "".
#' @slot autodetect character; String to request auto-detection of standard
#'   error method. Typically " -AutoSE " or "".
#' @slot xlandig numeric; Specifies the optimization accuracy (NDIGIT) for the
#'   outer loop (thetas and sigmas) when using \code{FOCE-ELS} or
#'   \code{LAPLACIAN} methods.
#' @slot xlatol numeric; Specifies the relative step size used for numerical
#'   computation of the Hessian matrix (second derivatives) during standard
#'   error calculation.
#' @slot xblndig numeric; Specifies the optimization accuracy (NDIGIT) for the
#'   inner loop (optimization of etas).  Also applies to the single optimization
#'   loop in the \code{NAIVE-POOLED} method.
#' @slot xbltol numeric; Specifies the relative step size for numerical
#'   differentiation during model linearization.
#' @slot gradTolOuter Numeric maximum gradient tolerance in the outer
#'   (Theta/Omega/Sigma) optimization loop. Applicable to \code{FOCE-ELS} and
#'   \code{LAPLACIAN} methods.
#' @slot stepTolOuter Numeric maximum step tolerance in the outer
#'   (Theta/Omega/Sigma) optimization loop. Applicable to \code{FOCE-ELS} and
#'   \code{LAPLACIAN} methods.
#' @slot gradTolInner Numeric maximum gradient tolerance in the inner (Eta)
#'   optimization loop. Applicable to \code{FOCE-ELS} and
#'   \code{LAPLACIAN} methods.
#' @slot stepTolInner Numeric maximum step tolerance in the inner (Eta)
#'   optimization loop. Applicable to \code{FOCE-ELS} and
#'   \code{LAPLACIAN} methods.
#' @slot refDeltaLagl Numeric LL Delta tolerance value used during
#'   Theta/Omega/Sigma optimization. Applicable to \code{FOCE-ELS} and
#'   \code{LAPLACIAN} methods.
#' @slot isPCWRES numeric; Flag indicating if Population Conditional Weighted
#'   Residuals (PCWRES) should be computed.  A value of 1 indicates computation,
#'   while 0 indicates no computation. Only applicable to population models.
#' @slot xpcwresnrep numeric; Stores the number of simulation replicates used
#'   for PCWRES computation.  Applicable only when \code{isPCWRES} is 1.
#' @slot xisample numeric; Specifies the number of sample points used in the
#'   QRPEM algorithm. Only applicable when \code{method} is \code{QRPEM}.
#' @slot xmapassist numeric; Controls the use of MAP assistance in the QRPEM
#'   algorithm.
#' \itemize{
#'   \item 0: No MAP assistance.
#'   \item >0: The inner ETAs optimization loop is used in the QRPEM outer
#'    optimization loop with a periodicity equal to the value of \code{xmapassist}.
#' }
#'   Only applicable when \code{method} is \code{QRPEM}.
#' @slot xmapnp numeric; Specifies the number of iterations for a preliminary
#'   Naive-Pooled optimization run before the main estimation. Applicable when
#'   the method is not \code{NAIVE-POOLED}.
#' @slot ximpsampdof numeric; Controls the importance sampling distribution used
#'   in the QRPEM algorithm. Only applicable when \code{method} is \code{QRPEM}.
#' \itemize{
#'  \item 0: Multivariate Normal distribution.
#'  \item 1: Multivariate Double Exponential (Laplace) distribution.
#'  \item 2: Direct sampling from the prior.
#'  \item 3-30: Multivariate T distribution with degrees of freedom equal to the value of \code{ximpsampdof}.
#'  \item -2: Mixture-2 distribution.
#'  \item -3: Mixture-3 distribution.
#' }
#' @slot xmcpem numeric; Controls the sampling method used in the QRPEM
#'   algorithm.
#' \itemize{
#'   \item 0: Quasi-Random sampling.
#'   \item 1: Monte-Carlo sampling.
#' }
#'   Only applicable when \code{method} is \code{QRPEM}.
#' @slot xpemrunall numeric; Set to \code{1} to execute all requested
#'   iterations specified in \code{numIterations}. Only applicable to population
#'   models with \code{method = "QRPEM"}.
#' @slot xsirsamp numeric; Specifies the number of samples per eta per subject
#'   used in the Sampling Importance Resampling (SIR) algorithm within QRPEM.
#'   Only applicable when \code{method} is \code{QRPEM}.
#' @slot xburnin numeric; Specifies the number of burn-in iterations in the
#'   QRPEM algorithm.  During burn-in, omegas can be frozen (see
#'   \code{xnonomegaburn}). Only applicable when \code{method} is \code{QRPEM}.
#' @slot xnonomegaburn numeric; Controls whether omegas are frozen during the
#'   burn-in phase of the QRPEM algorithm.
#' \itemize{
#'   \item 0: burn-in with frozen omegas is off.
#'   \item 1: burn-in with frozen omegas is on.
#' }
#'   Only applicable when \code{method} is \code{QRPEM}. See also
#'   \code{xburnin}.
#' @slot xaccratio numeric; Specifies the acceptance ratio used in the QRPEM
#'   algorithm for scaling the covariance matrix. Only applicable when
#'   \code{method} is \code{QRPEM}. Only applicable to population models with
#'   \code{method = "QRPEM"}.
#' @slot xscramble numeric; Specifies the scrambling method for quasi-random
#'   number generation in the QRPEM algorithm.
#' \itemize{
#'   \item 0: No scrambling.
#'   \item 1: Owen-type scrambling.
#'   \item 2: Faure-Tezuka scrambling.
#' }
#'   Only applicable when \code{method} is \code{QRPEM}.
#' @slot emTolType
#'   Numeric specifying QRPEM convergence check type:
#'   \describe{
#'     \item{0}{Default (no rollout, LL & Thetas)}
#'     \item{1}{LL & Params with rollout}
#'     \item{2}{LL with rollout}
#'     \item{3}{Params with rollout}
#'   }
#'   Only applicable when \code{method} is \code{QRPEM}.
#' @slot emConvLen Numeric specifying the number of iterations to check for
#'   convergence. Only applicable when \code{method} is \code{QRPEM}.
#' @slot emConvCritVal Numeric specifying the convergence critical value. Only
#'   applicable when \code{method} is \code{QRPEM}.
#' @slot pardern numeric; Specifies the number of time steps used for outputting
#'   partial derivatives of observed variables with respect to parameters. Only
#'   applicable to individual models.
#' @slot parderd numeric; Specifies the step size for numerical calculation of
#'   partial derivatives of observed variables with respect to parameters. Only
#'   applicable to individual models.
#' @slot logtran numeric; Engine flag controlling log-transformation behavior
#'   for single LogAdditive error model.
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
#' @seealso [engineParams()]
#' @keywords internal
#' @exportClass NlmeEngineExtraParams
setClass(
  "NlmeEngineExtraParams",
  slots = c(
    isPopulation   = "logical",
    sort           = "character",
    csv            = "character",
    method         = "numeric",
    numIterations  = "numeric",
    odeToUse       = "numeric",
    rtol           = "numeric",
    atol           = "numeric",
    nmxstep        = "numeric",
    anagrad        = "numeric",
    xnp            = "numeric",
    xnorderagq     = "numeric",
    xfocehess      = "numeric",
    xstderr        = "numeric",
    sand           = "character",
    fisher         = "character",
    autodetect     = "character",
    xlandig        = "numeric",
    xlatol         = "numeric",
    xblndig        = "numeric",
    xbltol         = "numeric",
    gradTolOuter   = "numeric",
    stepTolOuter   = "numeric",
    gradTolInner   = "numeric",
    stepTolInner   = "numeric",
    refDeltaLagl   = "numeric",
    isPCWRES       = "numeric",
    xpcwresnrep    = "numeric",
    xisample       = "numeric",
    xmapassist     = "numeric",
    xmapnp         = "numeric",
    ximpsampdof    = "numeric",
    xmcpem         = "numeric",
    xpemrunall     = "numeric",
    xsirsamp       = "numeric",
    xburnin        = "numeric",
    xnonomegaburn  = "numeric",
    xaccratio      = "numeric",
    xscramble      = "numeric",
    emTolType      = "numeric",
    emConvLen      = "numeric",
    emConvCritVal  = "numeric",
    pardern        = "numeric",
    parderd        = "numeric",
    logtran        = "numeric"
  ),
  prototype = list(
    isPopulation   = TRUE,
    sort           = " -sort ",
    csv            = " -csv ",
    method         = 5,
    numIterations  = 1000,
    odeToUse       = 6,
    rtol           = 1e-06,
    atol           = 1e-06,
    nmxstep        = 5000,
    anagrad        = 0,
    xnp            = 0,
    xnorderagq     = 1,
    xfocehess      = 1,
    xstderr        = 1,
    sand           = "",
    fisher         = "",
    autodetect     = "",
    xlandig        = 7,
    xlatol         = 0.01,
    xblndig        = 13,
    xbltol         = 0.002,
    gradTolOuter   = 2e-4,
    stepTolOuter   = 1e-4,
    gradTolInner   = 1.71e-5,
    stepTolInner   = 7.07e-8,
    refDeltaLagl   = 1e-3,
    isPCWRES       = 0,
    xpcwresnrep    = 1,
    xisample       = 300,
    xmapassist     = 0,
    xmapnp         = 0,
    ximpsampdof    = 0,
    xmcpem         = 0,
    xpemrunall     = 0,
    xsirsamp       = 10,
    xburnin        = 0,
    xnonomegaburn  = 0,
    xaccratio      = 0.1,
    xscramble      = 1,
    emTolType      = 0,
    emConvLen      = 10,
    emConvCritVal  = 5,
    pardern        = 20,
    parderd        = 1e-05,
    logtran        = 1
  )
)


#' Create a new NlmeEngineExtraParams object
#'
#' This function creates a new instance of the NlmeEngineExtraParams class
#'
#' @param ... Named arguments to override the default values.
#' @return An object of class NlmeEngineExtraParams.
#' @keywords internal
#' @export
NlmeEngineExtraParams <- function(...) {
  new("NlmeEngineExtraParams", ...)
}


setMethod("initialize", "NlmeEngineExtraParams",
          function(.Object, ...) {
            args <- list(...)
            for (name in names(args)) {
              if (!(name %in% methods::slotNames(.Object))) {
                warning(sprintf("Unknown slot: %s", name))
              }
            }
            .Object <- callNextMethod(.Object, ...)
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
  # Helper function to format a single line with a fixed-width label
  fmt_line <- function(label, value) {
    sprintf("%-30s: %s", label, value)
  }

  # Determine method string using switch and if/else where needed
  method <- switch(
    as.character(x@method),
    "1" = "QRPEM",
    "2" = "IT2S-EM",
    "3" = "FOCE-LB",
    "4" = "FO",
    "6" = "NAIVE-POOLED",
    "5" = if (x@xfocehess == 0)
      "LAPLACIAN"
    else
      "FOCE-ELS",
    "Unknown"
  )

  # Determine solver from the odeToUse slot
  solver <- switch(
    as.character(x@odeToUse),
    "1" = "LSODE with numerical Jacobian",
    "2" = "LSODE with analytical Jacobian",
    "3" = "Non-stiff DVERK",
    "4" = "LSODA with numerical Jacobian",
    "5" = "LSODA with analytical Jacobian",
    "6" = "Matrix Exponent",
    "7" = "Non-stiff DOPRI5",
    "Unknown"
  )

  # Build output lines
  output <- c(
    "\n Engine Parameters \n -------------------------------------------",
    fmt_line("Is population", x@isPopulation),
    fmt_line("Sort input data",!is.null(x@sort) && x@sort != ""),
    fmt_line("Engine used", method),
    fmt_line("Max iterations", x@numIterations),
    fmt_line("ODE solver", solver)
  )

  output <- c(
    output,
    fmt_line("Allow Log-transform", as.logical(x@logtran))
  )

  output <- c(output,
              "\n Advanced Options \n -------------------------------------------")

  if (method %in% c("FOCE-ELS", "LAPLACIAN")) {
    output <- c(output, fmt_line("Integration points", x@xnorderagq))
    output <-
      c(output, fmt_line("fastOptimization", as.logical(x@anagrad)))
    output <- c(
      output,
      fmt_line("Outer Grad Tolerance", x@gradTolOuter),
      fmt_line("Outer Step Tolerance", x@stepTolOuter),
      fmt_line("Inner Grad Tolerance", x@gradTolInner),
      fmt_line("Inner Step Tolerance", x@stepTolInner),
      fmt_line("LL Delta Tolerance", x@refDeltaLagl)
    )
  }

  if (method != "NAIVE-POOLED") {
    output <- c(output,
                fmt_line("Non-param iter", x@xnp),
                fmt_line("MAPNP iter", x@xmapnp))
    if (x@isPCWRES)
      # x@isPCWRES is numeric (0 or 1)
      output <-
        c(output, fmt_line("PCWRES replicates", x@xpcwresnrep))
    output <- c(
      output,
      fmt_line("LAGL nDigit", x@xlandig),
      fmt_line("BLUP nDigit", x@xblndig),
      fmt_line("Linearization step size", x@xbltol)
    )
  }

  if (!x@isPopulation) {
    output <- c(
      output,
      fmt_line("Partial deriv step size", x@parderd),
      fmt_line("Steps output deriv", x@pardern)
    )
  }

  output <- c(
    output,
    fmt_line("ODE relative tolerance", x@rtol),
    fmt_line("ODE absolute tolerance", x@atol),
    fmt_line("ODE max steps", x@nmxstep)
  )

  if (method == "QRPEM") {
    # Determine importance sampling distribution
    impDist <- switch(
      as.character(x@ximpsampdof),
      "-3" = "Mixture-3",
      "-2" = "Mixture-2",
      "0" = "Multivariate Normal",
      "1" = "Multivariate Laplace",
      "2" = "Direct Sampling",
      "Multivariate T" # Default for values 3-30
    )
    # Determine scrambling method
    scramble <- switch(
      as.character(x@xscramble),
      "0" = "None",
      "1" = "Owen",
      "2" = "Faure-Tezuka",
      "Unknown"
    )

    output <- c(
      output,
      "\n QRPEM Options \n -------------------------------------------",
      fmt_line("MAP assistance enabled", x@xmapassist > 0)
    )
    if (x@xmapassist > 0)
      output <-
      c(output, fmt_line("Period to MAP assistance", x@xmapassist))

    output <- c(
      output,
      fmt_line("Sample points", x@xisample),
      fmt_line("Acceptance ratio", x@xaccratio),
      fmt_line("Importance sampling distr", impDist)
    )

    if (impDist == "Multivariate T")
      output <-
      c(output, fmt_line("Degrees of freedom", x@ximpsampdof))

    output <- c(
      output,
      fmt_line("QR Scrambling method", scramble),
      fmt_line("SIR samples", x@xsirsamp),
      fmt_line("Burn-in iter", x@xburnin),
      fmt_line("Freeze omega during burn-in", as.logical(x@xnonomegaburn)),
      fmt_line("Monte-Carlo sampling", as.logical(x@xmcpem)),
      fmt_line("Run all iterations", as.logical(x@xpemrunall))
    )

    # Print convergence criteria options
    convTypeStr <- switch(
      as.character(x@emTolType),
      "0" = "Default (no rollout, LL & Thetas)",
      "1" = "LL & Params with rollout",
      "2" = "LL with rollout",
      "3" = "Params with rollout",
      "Unknown"
    )

    output <- c(
      output,
      fmt_line("Conv. check type", convTypeStr),
      fmt_line("Conv. iterations", x@emConvLen),
      fmt_line("Conv. crit. value", x@emConvCritVal)
    )
  }

  # Standard Errors Section
  output <- c(output,
              "\n Standard Errors \n -------------------------------------------")

  stdErr <- if (!is.null(x@sand) && x@sand == "  -sand ") {
    "Sandwich"
  } else if (!is.null(x@fisher) && x@fisher == "  -fscore ") {
    "Fisher Score"
  } else if (!is.null(x@autodetect) &&
             x@autodetect == "  -AutoSE ") {
    "Auto-Detect"
  } else if (x@xstderr == 0) {
    "None"
  } else {
    "Hessian"
  }

  output <- c(output, fmt_line("Standard Errors Method", stdErr))

  if (stdErr != "None") {
    diffMethod <-
      if (x@xstderr == 2)
        "Forward Difference"
    else
      # Includes xstderr == 1 (Central) and potentially other values if not 0 or 2
      "Central Difference"
    output <-
      c(output, fmt_line("Finite Difference Method", diffMethod))
    if (x@isPopulation)
      output <- c(output, fmt_line("Step size", x@xlatol))
  }

  output <-
    c(output, "\n -------------------------------------------")

  cat(paste(output, collapse = "\n"), "\n")
  invisible(x)
}

setMethod(
  "show",
  "NlmeEngineExtraParams",
  definition = function(object) {
    print(object)
  }
)

#' Main function to specify engine parameters
#'
#' Use to define engine parameters for model execution.
#'
#' @param model Model object.  The type of model (population or individual) is
#'   determined by the model@isPopulation slot.  If model@isPopulation is TRUE,
#'   the model is treated as a population model; otherwise, it's treated as an
#'   individual model.
#' @param sort Logical; Specifies whether to sort the input data by subject and
#'   time. If \code{TRUE}, data are sorted. If \code{FALSE}, data are not
#'   sorted. Defaults to \code{FALSE} if the model contains reset information
#'   (\code{model@hasResetInfo = TRUE}); otherwise, defaults to \code{TRUE}.
#' @param ODE Character; Specifies the ODE solver to be used. Options are:
#'   \code{"MatrixExponent"}, \code{"DVERK"}, \code{"DOPRI5"},
#'   \code{"AutoDetect"}, \code{"Stiff"}, \code{"LSODE"}. See Details section
#'   for a description of each solver.
#' @param rtolODE Numeric; Specifying relative tolerance for the numerical ODE
#'   solver.
#' @param atolODE Numeric; Specifying absolute tolerance for the numerical ODE
#'   solver.
#' @param maxStepsODE Numeric; Specifies the maximum number of steps allowed for
#'   the ODE solver.
#' @param numIterations Integer; Specifies the maximum number of iterations for
#'   the estimation algorithm. Must be a non-negative integer, with a maximum
#'   value of 10000.
#' @param method Character; Specifies the estimation method. For population
#'   models, options are: \code{"QRPEM"}, \code{"IT2S-EM"}, \code{"FOCE-LB"},
#'   \code{"FO"}, \code{"FOCE-ELS"}, \code{"Laplacian"}, and
#'   \code{"Naive-Pooled"}. For individual models, only \code{"Naive-Pooled"} is
#'   available. The default for population models depends on model
#'   characteristics:
#'   \itemize{
#'     \item If the model includes discontinuous observed variables (e.g., count data),
#'           Below Quantifiable Limit (BQL) data, or has no unfrozen sigmas, the default is \code{"Laplacian"}.
#'     \item Otherwise, the default is \code{"FOCE-ELS"}.
#'   }
#' @param stdErr Character; Specifies the method for standard error
#'   computations. Options vary depending on the model type and estimation
#'   method:
#'   \itemize{
#'     \item Individual models: \code{"Hessian"} (default) or \code{"None"}.
#'     \item Population models with \code{method = "QRPEM"}: \code{"Fisher-Score"} (default) or \code{"None"}.
#'     \item Population models with \code{method = "IT2S-EM"}: \code{"None"} only.
#'     \item Population models with \code{method} in \code{c("FOCE-LB", "FO", "FOCE-ELS", "Laplacian", "Naive-Pooled")}:
#'           \code{"Sandwich"} (default), \code{"Hessian"}, \code{"Fisher-Score"}, \code{"Auto-Detect"}, or \code{"None"}.
#'   }
#'   \code{"None"} means that standard error calculations are not performed.
#' @param isCentralDiffStdErr Logical; If \code{TRUE} (default), uses central
#'   difference for standard error calculations when applicable. If
#'   \code{FALSE}, uses forward difference.
#' @param stepSizeStdErr Numeric; Specifies the relative step size used for the
#'   numerical computation of the Hessian matrix during standard error
#'   calculations. If not specified, a default value is used (0.001 for
#'   \code{"Naive-Pooled"} method, and 0.01 otherwise).
#' @param logTransform Logical or NULL; Controls log-transformation behavior,
#'   particularly for models with a LogAdditive residual error (e.g.,
#'   C*exp(epsilon)). The internal engine parameter 'logtran' is set based on
#'   this argument and specific model characteristics as detailed below.
#'   \itemize{
#'     \item \code{NULL} (default) or \code{TRUE}: When the model has exactly
#'       one residual error model and it is LogAdditive, this setting enables
#'       Log-Transform Both Sides (LTBS). In LTBS, predictions and observations
#'       are log-transformed, and the model is fit in the log-domain. This
#'       results in the internal `logtran` engine parameter being set to 1.
#'
#'     \item \code{FALSE}: When the model has exactly one residual error model
#'       and it is LogAdditive, this setting results in the LogAdditive error
#'       being treated as a proportional/multiplicative error during fitting
#'       (by neglecting third and higher-order terms in the Taylor expansion
#'       of exp(epsilon)). This sets the internal `logtran` engine parameter
#'       to 0. For simulation, the error is treated as exp(epsilon).
#'   }
#'
#'   For other model configurations, the `logtran` parameter is determined as follows:
#'   \itemize{
#'     \item If there are multiple residual error models or no residual error
#'       models, `logtran` is set to 0, irrespective of the `logTransform`
#'       value. (In the case of multiple errors, any LogAdditive errors present
#'       are treated as proportional).
#'     \item If there is a single residual error model that is not LogAdditive:
#'       \itemize{
#'         \item For built-in models: `logtran` is set to 0.
#'         \item For textual models: `logtran` reflects the `logTransform`
#'           setting (it becomes 1 if `logTransform` is `NULL` or `TRUE`, and
#'           0 if `logTransform` is `FALSE`). A warning is issued if
#'           `logTransform` is `NULL` or `TRUE` in this scenario, highlighting
#'           that LTBS is typically for LogAdditive errors and that error type
#'           identification can be challenging in textual models.
#'       }
#'   }
#' @param numIntegratePtsAGQ Integer; Specifies the number of quadrature points
#'   per dimension to use for Adaptive Gaussian Quadrature (AGQ). Only
#'   applicable to population models when \code{method} is \code{"FOCE-ELS"} or
#'   \code{"Laplacian"}.
#' \itemize{
#'   \item 1: Standard FOCE-ELS/LAPLACIAN computation (no AGQ).
#'   \item >1: AGQ is performed.  The total number of quadrature points used is \code{(number of ETAs)^numIntegratePtsAGQ}.
#' }
#' @param numIterNonParametric Integer; Controls non-parametric (NP)
#'   optimization.
#'   \itemize{
#'     \item 0: Disables NP optimization.
#'     \item 1: Enables NONMEM-style NP optimization using posthoc estimates as support points.
#'     \item >1: Enables an evolutionary NP algorithm, using \code{numIterNonParametric} as the number of generations.
#'   }
#'   Only applicable to population models when \code{method} is not
#'   \code{"Naive-Pooled"}.
#' @param allowSyntheticGradient Deprecated.
#' @param fastOptimization Logical; Controls the differentiation method used
#'   during the optimization of random effects (etas). If \code{TRUE}, automatic
#'   differentiation is used where possible. If \code{FALSE}, a finite
#'   difference approach is used. Only applicable to population models when
#'   \code{method} is \code{"FOCE-ELS"} or \code{"Laplacian"}.
#' @param numIterMAPNP Integer; Specifies the number of iterations for a
#'   preliminary Naive-Pooled (NP) optimization run before the main estimation.
#'   Applicable when the \code{method} is not \code{"NAIVE-POOLED"}.
#' @param numRepPCWRES Integer; Specifies the number of replicates to generate
#'   for Population Conditional Weighted Residuals (PCWRES) calculations.
#'   Setting this value to 0 disables PCWRES computation.  Only applicable to
#'   population models when method is not set to \code{"Naive-Pooled"}.
#' @param stepSizeLinearize Numeric; Specifies the relative step size for
#'   numerical differentiation during model linearization.
#' @param numDigitLaplacian Numeric; Specifies the optimization accuracy
#'   (NDIGIT) for the outer loop (thetas and sigmas) when using
#'   \code{"FOCE-ELS"} or \code{"Laplacian"} methods. Only applicable to
#'   population models.
#' @param numDigitBlup Numeric; Specifies the optimization accuracy (NDIGIT) for
#'   the inner loop (optimization of etas). Also applies to the single
#'   optimization loop in the \code{"NAIVE-POOLED"} method.
#' @param gradTolOuter Numeric; maximum gradient tolerance for the outer loop
#'   (Theta/Omega/Sigma optimization) of "FOCE-ELS" or "Laplacian" method.
#'   This tolerance controls how close the gradient must be to zero before the
#'   outer optimization is considered converged.
#' @param stepTolOuter Numeric; maximum step tolerance for the outer loop
#'   (Theta/Omega/Sigma optimization) of "FOCE-ELS" or "Laplacian" method.
#'   This measures the relative change in the solution vector between iterations.
#' @param gradTolInner Numeric; maximum gradient tolerance for the inner loop
#'   (Eta optimization) of "FOCE-ELS" or "Laplacian" method. A smaller value
#'   forces the algorithm to iterate until a very small gradient is achieved.
#' @param stepTolInner Numeric; maximum step tolerance for the inner loop (Eta
#'   optimization) of "FOCE-ELS" or "Laplacian" method. This determines when
#'   the algorithm will terminate based on minimal changes in the solution vector.
#' @param refDeltaLagl Numeric; tolerance for the change in the log-likelihood
#'   (LL) value during outer loop optimization of "FOCE-ELS" or "Laplacian" method.
#'   This parameter is used to check convergence by comparing the absolute change
#'   in LL between major iterations. If the change in LL is less than refDeltaLagl
#'   and the optimization driver returns a specific termination code, the algorithm
#'   considers the solution sufficiently converged. This tolerance helps to
#'   avoid unnecessary iterations when improvements in LL become marginal.
#' @param mapAssist Numeric; Controls the use of MAP assistance in the QRPEM
#'   algorithm.
#' \itemize{
#'   \item 0: No MAP assistance.
#'   \item >0: The inner ETAs optimization loop is used in the QRPEM outer
#'    optimization loop with a periodicity equal to the value of \code{mapAssist}.
#' }
#'   Only applicable to population models with \code{method = "QRPEM"}.
#' @param iSample Numeric; Specifies the number of sample points used in the
#'   QRPEM algorithm. Only applicable to population models with \code{method =
#'   "QRPEM"}.
#' @param iAcceptRatio Numeric; Specifies the acceptance ratio used in the QRPEM
#'   algorithm for scaling the covariance matrix. Only applicable to population
#'   models with \code{method = "QRPEM"}.
#' @param impDist Character; Specifies the importance sampling distribution used
#'   in the QRPEM algorithm. Options are: \code{"Normal"},
#'   \code{"DoubleExponential"}, \code{"Direct"}, \code{"T"},
#'   \code{"Mixture-2"}, \code{"Mixture-3"}. Only applicable to population
#'   models with \code{method = "QRPEM"}.  See Details for further information.
#' @param tDOF Numeric; Specifies the degrees of freedom for the multivariate T
#'   distribution used in importance sampling. Only applicable when \code{method
#'   = "QRPEM"} and \code{impDist = "T"}.  Must be between 3 and 30.
#' @param numSampleSIR Numeric; Specifies the number of samples per eta per
#'   subject used in the Sampling Importance Resampling (SIR) algorithm within
#'   QRPEM. Only applicable to population models with \code{method = "QRPEM"}.
#' @param numBurnIn Numeric; Specifies the number of burn-in iterations in the
#'   QRPEM algorithm. During burn-in, omegas can be frozen (see
#'   \code{freezeOmega} parameter). Only applicable to population models with
#'   \code{method = "QRPEM"}.
#' @param freezeOmega Logical; Set to \code{TRUE} to freeze Omega but not Theta
#'   for the number of iterations specified in the \code{numBurnIn}. Only
#'   applicable to population models with \code{method = "QRPEM"}.
#' @param MCPEM Logical; Controls the sampling method used in the QRPEM
#'   algorithm.
#' \itemize{
#'   \item \code{FALSE}: Quasi-Random sampling.
#'   \item \code{TRUE}: Monte-Carlo sampling.
#' }
#'   Only applicable to population models with \code{method = "QRPEM"}.
#' @param runAllIterations Logical; Set to \code{TRUE} to execute all requested
#'   iterations specified in \code{numIterations}. Only applicable to population
#'   models with \code{method = "QRPEM"}.
#' @param scramble Character; Specifies the scrambling method for quasi-random
#'   number generation in the QRPEM algorithm. Options are: \code{"None"},
#'   \code{"Owen"}, \code{"Faure-Tezuka"}. Only applicable to population models
#'   with \code{method = "QRPEM"}.
#' @param emTolType Numeric; QRPEM convergence check type. Options:
#'   \itemize{
#'     \item 0: Default (no rollout, LL & Theta and Sigma).
#'     \item 1: LL & All Population Params (Theta, Omega, and Sigma) with rollout.
#'     \item 2: LL with rollout.
#'     \item 3: All Population Params with rollout.
#'   }
#'   Only applicable to population models with method = "QRPEM".
#' @param emConvLen Numeric; number of iterations over which convergence is
#'   checked in the QRPEM method. Only applicable to population models with
#'   method = "QRPEM" and emTolType being nonzero.
#' @param emConvCritVal Numeric; critical value used in the QRPEM convergence
#'   check. It specifies the threshold improvement required to continue iterating.
#'   Only applicable to population models with method = "QRPEM" and emTolType
#'   being nonzero.
#' @param stepSizePartialDeriv Numeric; Specifying the step size used to
#'   numerically calculate the partial derivatives of observed variables with
#'   respect to parameters. Only applicable to individual models.
#' @param numTimeStepPartialDeriv Numeric; Specifying the number of time steps
#'   used to output the partial derivatives of observed variables with respect
#'   to parameters. Only applicable to individual models.
#'
#' @details Both \code{"DVERK"} and \code{"DOPRI5"} are non-stiff solvers.
#' \code{"AutoDetect"} represents LSODA solver implemenation, which solves the
#' initial value problem for stiff or nonstiff systems of first order ordinary
#' differential equations. \code{"Stiff"} is a LSODE (Livermore solver). It is
#' best suited for stiff problems. \code{"MatrixExponent"} is a matrix
#' exponential solver.
#'
#' For the QRPEM method, the \code{impDist} parameter controls the importance
#' sampling distribution.  The \code{ximpsampdof} slot in the internal
#' \code{NlmeEngineExtraParams} object is set based on \code{impDist} as
#' follows:
#' \itemize{
#'  \item \code{"Normal"}: \code{ximpsampdof} = 0
#'  \item \code{"DoubleExponential"}: \code{ximpsampdof} = 1
#'  \item \code{"Direct"}: \code{ximpsampdof} = 2
#'  \item \code{"T"}: \code{ximpsampdof} is set to the value of \code{tDOF}.
#'  \item \code{"Mixture-2"}: \code{ximpsampdof} = -2
#'  \item \code{"Mixture-3"}: \code{ximpsampdof} = -3
#' }
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
                         logTransform = NULL,
                         numIntegratePtsAGQ = 1,
                         numIterNonParametric = 0,
                         allowSyntheticGradient = FALSE,
                         fastOptimization = FALSE,
                         numIterMAPNP = 0,
                         numRepPCWRES = 0,
                         stepSizeLinearize = 0.002,
                         numDigitLaplacian = 7,
                         numDigitBlup = 13,
                         gradTolOuter = 2e-4,
                         stepTolOuter = 1e-4,
                         gradTolInner = 1.71e-5,
                         stepTolInner = 7.07e-8,
                         refDeltaLagl = 1e-3,
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
                         emTolType = 0,
                         emConvLen = 10,
                         emConvCritVal = 5.0,
                         stepSizePartialDeriv = 1e-5,
                         numTimeStepPartialDeriv = 20) {
  # --- Helper functions ---
  `%notin%` <- Negate(`%in%`)
  is.natural <- function(x) {
    is.numeric(x) && x > 0 && (round(x) == x)
  }

  # --- Initialize the engine parameters object ---
  ep <- NlmeEngineExtraParams()
  ep@isPopulation <- model@isPopulation

  # --- Validate and assign general parameters ---
  if (numIterations < 0 || numIterations %% 1 != 0) {
    stop("`numIterations` must be a non-negative integer")
  }
  if (numIterations > 10000) {
    stop("`numIterations` must be <= 10000")
  }
  ep@numIterations <- numIterations

  # Determine discontinuity flag based on model diagnostics
  modelDiagnostic <- createModelInfo(model = model)
  hasDiscont <- !any(modelDiagnostic == "(allowgaussianfit 1)")

  # --- Determine and validate method ---
  if (is.null(method)) {
    method <- if (!model@isPopulation) {
      "Naive-Pooled"
    } else {
      if (hasDiscont) {
        "Laplacian"
      } else {
        "FOCE-ELS"
      }
    }
  } else {
    if (!model@isPopulation && toupper(method) != "NAIVE-POOLED") {
      stop("For individual models, `method` must be 'Naive-Pooled'")
    }
    if (model@isPopulation && hasDiscont &&
        toupper(method) %notin% c("LAPLACIAN", "QRPEM", "IT2S-EM", "NAIVE-POOLED")) {
      stop(
        "For models with discontinuous variables, available `method` options are 'Laplacian', 'QRPEM', 'IT2S-EM', or 'Naive-Pooled'"
      )
    }
  }

  # --- Validate and assign standard error method ---
  if (is.null(stdErr)) {
    stdErr <- if (!model@isPopulation) {
      "Hessian"
    } else {
      if (toupper(method) == "QRPEM") {
        "Fisher-Score"
      } else if (toupper(method) %in% c("FOCE-ELS", "LAPLACIAN", "FOCE-LB", "FO", "NAIVE-POOLED")) {
        "Sandwich"
      } else {
        "None"
      }
    }
  } else {
    if (!model@isPopulation &&
        toupper(stdErr) %notin% c("HESSIAN", "NONE")) {
      # Ensure case-insensitivity for comparison
      stop("For individual models, `stdErr` must be 'Hessian' or 'None'")
    }
    if (model@isPopulation) {
      if (toupper(method) == "QRPEM" &&
          toupper(stdErr) %notin% c("FISHER-SCORE", "NONE")) {
        stop("For population models with `QRPEM`, `stdErr` must be 'Fisher-Score' or 'None'")
      }
      if (toupper(method) == "IT2S-EM" &&
          toupper(stdErr) != "NONE") {
        stop("For `IT2S-EM`, `stdErr` must be 'None'")
      }
    }
  }

  # Set xfocehess flag based on method
  ep@xfocehess <- if (toupper(method) == "LAPLACIAN") {
    0
  } else {
    1 # Default for FOCE-ELS and others
  }

  # Convert method to internal numeric code using helper
  m <- .assignEngineMethod(method, model)
  ep@method <- m


  # --- ODE solver settings ---
  o <- .assignODEMethod(ODE)
  ep@odeToUse <- o
  ep@rtol <- rtolODE
  ep@atol <- atolODE
  ep@nmxstep <- maxStepsODE

  # --- ADPO settings ---
  if (toupper(method) %in% c("FOCE-ELS", "LAPLACIAN")) {
    ep@anagrad <- if (fastOptimization) {
      1
    } else {
      0
    }
  } else if (fastOptimization) {
    warning(
      "fastOptimization is supported by FOCE-ELS/LAPLACIAN methods only. Ignoring fastOptimization setting."
    )
  }

  if (allowSyntheticGradient)
    warning("'allowSyntheticGradient' option is deprecated.")


  # FOCE-ELS/Laplacian specific tolerance arguments
  ELS_Tols_Grads <-
    c("gradTolOuter",
      "stepTolOuter",
      "gradTolInner",
      "stepTolInner",
      "refDeltaLagl")

  # QRPEM-only arguments (basic)
  QRPEM_Only_Args_Names <-
    c(
      "mapAssist",
      "iSample",
      "iAcceptRatio",
      "impDist",
      "tDOF",
      "numSampleSIR",
      "numBurnIn",
      "freezeOmega",
      "MCPEM",
      "runAllIterations",
      "scramble"
    )

  # QRPEM convergence arguments
  QRPEM_Conv_Args <- c("emTolType", "emConvLen", "emConvCritVal")

  # General population-only arguments (excluding method-specific ones already listed)
  Population_Only_General_Args <-
    c(
      "numIntegratePtsAGQ",
      "numIterNonParametric",
      "numIterMAPNP",
      "numRepPCWRES",
      "stepSizeLinearize",
      "numDigitLaplacian",
      "fastOptimization"
    )

  # Combine all arguments ONLY applicable to population models for warning purposes
  All_Population_Only_Args <- c(
    Population_Only_General_Args,
    ELS_Tols_Grads,
    QRPEM_Only_Args_Names,
    QRPEM_Conv_Args
  )

  # --- Check which specific args were provided by the user ---
  call_args_names <-
    names(as.list(match.call())[-1]) # Exclude function name itself
  els_tols_provided <-
    sapply(ELS_Tols_Grads, function(arg_name)
      arg_name %in% call_args_names)
  qrpem_conv_args_provided <-
    sapply(QRPEM_Conv_Args, function(arg_name)
      arg_name %in% call_args_names)
  all_pop_only_provided <-
    sapply(All_Population_Only_Args, function(arg_name)
      arg_name %in% call_args_names)


  # --- Population-specific parameter validations ---
  if (model@isPopulation) {
    # Integration points for AGQ (for FOCE-ELS/Laplacian only)
    ep@xnorderagq <-
      if (toupper(method) %in% c("FOCE-ELS", "LAPLACIAN")) {
        if (!is.natural(numIntegratePtsAGQ))
          stop("`numIntegratePtsAGQ` must be a natural number.")
        numIntegratePtsAGQ
      } else {
        if ("numIntegratePtsAGQ" %in% call_args_names) {
          warning(
            "`numIntegratePtsAGQ` is only applicable for FOCE-ELS or Laplacian methods and will be ignored."
          )
        }
        1
      }


    if (numIterNonParametric < 0 || numIterNonParametric %% 1 != 0)
      stop("`numIterNonParametric` must be a non-negative integer")
    ep@xnp <- numIterNonParametric

    if (numIterMAPNP < 0 || numIterMAPNP %% 1 != 0)
      stop("`numIterMAPNP` must be a non-negative integer")
    ep@xmapnp <- numIterMAPNP

    if (numRepPCWRES < 0 ||
        numRepPCWRES > 10000 ||
        numRepPCWRES %% 1 != 0)
      stop("`numRepPCWRES` must be an integer between 0 and 10000")
    if (numRepPCWRES > 0) {
      ep@isPCWRES <- 1
      ep@xpcwresnrep <- numRepPCWRES
    } else {
      ep@isPCWRES <- 0
    }

    if (stepSizeLinearize < 0)
      stop("`stepSizeLinearize` must be positive")
    ep@xbltol <- stepSizeLinearize

    if (!is.natural(numDigitLaplacian))
      stop("`numDigitLaplacian` must be a natural number")
    ep@xlandig <- numDigitLaplacian


    # Handle FOCE-ELS/Laplacian Tolerances
    ProvidedTols <-
      names(els_tols_provided)[els_tols_provided]

    if (toupper(method) %in% c("FOCE-ELS", "LAPLACIAN")) {
      for (TolName in ProvidedTols) {
        Tol <-
          get(TolName, envir = environment()) # Get from function arguments
        if (!is.numeric(Tol) || Tol < 0) {
          warning(
            paste0(
              "The value of ",
              TolName,
              " must be a non-negative number. Supplied value will be ignored, using default."
            )
          )

          next
        }

        methods::slot(ep, TolName) <-
          Tol # Assign to slot, e.g. ep@gradTolOuter
      }
    } else {
      # Method is NOT FOCE-ELS or LAPLACIAN: Check if any specific tol args were provided
      if (length(ProvidedTols) > 0) {
        warning(
          "The following tolerance arguments were provided but are only applicable ",
          "for FOCE-ELS/LAPLACIAN methods and will be ignored: ",
          paste(ProvidedTols, collapse = ", "),
          call. = FALSE
        )
      }
    }

    # QRPEM-specific validations
    if (toupper(method) == "QRPEM") {
      if (mapAssist < 0 || mapAssist %% 1 != 0)
        stop("`mapAssist` must be a non-negative integer")
      ep@xmapassist <- mapAssist

      if (!is.natural(iSample))
        stop("`iSample` must be a natural number")
      ep@xisample <- iSample

      if (!is.numeric(iAcceptRatio) ||
          iAcceptRatio <= 0)
        stop("`iAcceptRatio` must be a positive number")
      ep@xaccratio <- iAcceptRatio

      if (toupper(impDist) == "T" &&
          (tDOF < 3 || tDOF > 30 || !is.natural(tDOF)))
        stop("If `impDist` is 'T', `tDOF` must be a natural number between 3 and 30")

      ep@ximpsampdof <- switch(
        toupper(impDist),
        "NORMAL" = 0,
        "DOUBLEEXPONENTIAL" = 1,
        "DOUBLE EXPONENTIAL" = 1,
        "DIRECT" = 2,
        "T" = tDOF,
        "MIXTURE-2" = -2,
        "MIXTURE-3" = -3,
        stop(
          "`impDist` must be one of 'Normal', 'DoubleExponential', 'Direct', 'T', 'Mixture-2', or 'Mixture-3'"
        )
      )

      if (!is.natural(numSampleSIR))
        stop("`numSampleSIR` must be a natural number")
      ep@xsirsamp <- numSampleSIR

      if (numBurnIn < 0 || numBurnIn %% 1 != 0)
        # Ensure integer
        stop("`numBurnIn` must be a non-negative integer")
      ep@xburnin <- numBurnIn

      ep@xnonomegaburn <- if (freezeOmega) {
        1
      } else {
        0
      }

      ep@xmcpem <- if (MCPEM) {
        1
      } else {
        0
      }

      ep@xpemrunall <-
        if (runAllIterations) {
          1
        } else {
          0
        }

      ep@xscramble <- switch(
        tolower(scramble),
        "none" = 0,
        "owen" = 1,
        "faure-tezuka" = 2,
        stop(
          "`scramble` must be one of 'Owen', 'Faure-Tezuka', or 'None'"
        )
      )

      # --- Convergence criteria for QRPEM ---
      if (!emTolType %in% 0:3) {
        stop("`emTolType` must be 0, 1, 2, or 3.")
      }
      ep@emTolType <- emTolType
      if (ep@emTolType == 0 &&
          (any(c("emConvLen", "emConvCritVal") %in% call_args_names))) {
        # Check if explicitly passed
        warning(
          "emConvLen and emConvCritVal are ignored when emTolType is 0 (default convergence check).",
          call. = FALSE
        )
      }


      if (!is.natural(emConvLen)) {
        stop("`emConvLen` must be a positive integer.")
      }
      ep@emConvLen <- emConvLen

      if (!is.numeric(emConvCritVal) || emConvCritVal <= 0) {
        stop("`emConvCritVal` must be a positive number.")
      }
      ep@emConvCritVal <- emConvCritVal

    } else {
      # Method is NOT QRPEM
      provided_qrpem_args <-
        sapply(QRPEM_Only_Args_Names, function(arg_name)
          arg_name %in% call_args_names)
      provided_but_ignored_qrpem <-
        names(provided_qrpem_args)[provided_qrpem_args]

      if (length(provided_but_ignored_qrpem) > 0) {
        warning(
          "The following arguments are only applicable when `method` is 'QRPEM' ",
          "and will be ignored: ",
          paste(provided_but_ignored_qrpem, collapse = ", "),
          call. = FALSE
        )
      }
      provided_but_ignored_conv <-
        names(qrpem_conv_args_provided)[qrpem_conv_args_provided]
      if (length(provided_but_ignored_conv) > 0) {
        warning(
          "The following QRPEM convergence arguments were provided but are only applicable ",
          "when `method` is 'QRPEM' and will be ignored: ",
          paste(provided_but_ignored_conv, collapse = ", "),
          call. = FALSE
        )
      }
    }
  } else {
    # --- Individual (non-population) model validations ---
    provided_pop_args_for_indiv <-
      names(all_pop_only_provided)[all_pop_only_provided]


    if (length(provided_pop_args_for_indiv) > 0) {
      warning(
        "The following arguments are only applicable for population models ",
        "and will be ignored for this individual model: ",
        paste(sort(
          unique(provided_pop_args_for_indiv)
        ), collapse = ", "),
        # Sort for consistency
        call. = FALSE
      )
    }

    if (!is.numeric(stepSizePartialDeriv) ||
        stepSizePartialDeriv <= 0) {
      stop("`stepSizePartialDeriv` must be a positive number.")
    }
    ep@parderd <- stepSizePartialDeriv

    if (!is.numeric(numTimeStepPartialDeriv) ||
        numTimeStepPartialDeriv <= 0 ||
        numTimeStepPartialDeriv %% 1 != 0) {
      stop("`numTimeStepPartialDeriv` must be a positive integer.")
    }
    ep@pardern <- numTimeStepPartialDeriv
  }

  # This applies to both population and individual models (Naive-Pooled uses it)
  if (!is.natural(numDigitBlup))
    stop("`numDigitBlup` must be a natural number")
  ep@xblndig <- numDigitBlup


  # --- Standard error and step size settings ---
  if (!is.null(stepSizeStdErr)) {
    if (!is.numeric(stepSizeStdErr) || stepSizeStdErr <= 0)
      stop("`stepSizeStdErr` must be a positive number")
    ep@xlatol <- stepSizeStdErr
  } else {
    ep@xlatol <- if (model@isPopulation) {
      0.01
    } else {
      0.001
    }
  }

  if (isCentralDiffStdErr == FALSE) {
    ep@xstderr <- 2
  }


  if (model@isPopulation && toupper(method) == "IT2S-EM") {
    if (toupper(stdErr) != "NONE" &&
        "stdErr" %in% call_args_names) {
      # Check if user explicitly set it
      warning(
        "For IT2S-EM method, `stdErr` is forced to 'None'. User input '",
        stdErr,
        "' ignored.",
        call. = FALSE
      )
    }
    stdErr <- "None" # Enforce
  }


  # Reset string-based SE flags before setting
  ep@sand <- ""
  ep@fisher <- ""
  ep@autodetect <- ""

  if (toupper(stdErr) == "SANDWICH") {
    ep@sand <- "  -sand "
  } else if (toupper(stdErr) == "FISHER-SCORE") {
    ep@fisher <- "  -fscore "
  } else if (toupper(stdErr) == "AUTO-DETECT") {
    ep@autodetect <- "  -AutoSE "
  } else if (toupper(stdErr) == "HESSIAN") {
    # No specific string flag, ep@xstderr (1 or 2) controls Hessian
  } else if (toupper(stdErr) == "NONE") {
    ep@xstderr <- 0
  } else {
    stop(
      "`stdErr` must be one of 'Hessian', 'Sandwich', 'Fisher-Score', 'Auto-Detect', or 'None'"
    )
  }
  # If Hessian was chosen and xstderr was 0 (from None), set it back to 1 (Central)
  if (toupper(stdErr) == "HESSIAN" && ep@xstderr == 0) {
    ep@xstderr <-
      1 # Default to Central Difference for Hessian if SEs are not 'None'
  }


  # --- Handle data sorting based on reset info ---
  # Assuming model@hasResetInfo and model@userDefinedExtraDefs are accessible
  pattern <- "(?<=^|\\n)\\s*reset\\s*\\([^\\n]+\\)"
  colDefs <- if (!is.null(model@userDefinedExtraDefs)) {
    paste0(unlist(model@userDefinedExtraDefs), collapse = "\n")
  } else {
    ""
  }
  ResetInfoColDef <- grepl(pattern, colDefs, perl = TRUE)

  effective_has_reset_info <- model@hasResetInfo || ResetInfoColDef

  if (!is.null(sort)) {
    if (!is.logical(sort))
      stop("`sort` must be TRUE or FALSE")
    if (sort) {
      if (effective_has_reset_info) {
        stop("Model has reset info; set `sort = FALSE`")
      } else {
        ep@sort <- " -sort "
      }
    } else {
      ep@sort <- ""
    }
  } else {
    # sort is NULL (default)
    ep@sort <-
      if (effective_has_reset_info) {
        "" # Do not sort if reset info present
      } else {
        " -sort " # Default to sort if no reset info
      }
  }

  if (!is.null(logTransform) && !is.logical(logTransform)) {
    warning(
      "`logTransform` must be NULL, TRUE, or FALSE. Invalid input treated as NULL (default behavior)."
    )
    logTransform <- NULL
  }

  if (is.null(logTransform)) {
    # use the default value
    logTransformIsNull <- TRUE
    logTransform <- TRUE
  } else {
    logTransformIsNull <- FALSE
  }

  # the difference in the warnings between built-in and textual models
  # related to the fact that we cannot with 100% confidence recognize
  # logadditive residual model in textual mode
  # So we warn the user in such case, but the flag is not ignored
  # But for built-in we can ignore it even on printing level
  actual_hasLogAdditiveError <- FALSE
  if (model@isTextual) {
    residualErrors <-
      modelDiagnostic[grepl("^\\(error [A-Za-z]+ [\\d\\.Ee]+\\)$",
                            modelDiagnostic,
                            perl = TRUE)]
    residualErrors <- strsplit(residualErrors, " ")
    residualErrors <- sapply(residualErrors, function(x)
      x[2])

    modelWOComments <- .remove_comments(
      model@statements,
      type = "All",
      removeWhites = TRUE,
      collapse = ""
    )

    for (residualError in residualErrors) {
      # trying to catch it, but it is not 100% reliable
      if (grepl(paste0("exp(", residualError, ")"),
                modelWOComments,
                fixed = TRUE)) {
        actual_hasLogAdditiveError <- TRUE
      }
    }

    ep@logtran <- as.numeric(logTransform)
    if (length(residualErrors) > 1 && logTransform) {
      conditionalWarning(
        c(
          "Log-transformation is not allowed for models with multiple residual errors;",
          " will set `logTransform` to FALSE."
        ), !logTransformIsNull
      )

      ep@logtran <- 0
    } else if (length(residualErrors) == 1) {
      if (!actual_hasLogAdditiveError && logTransform) {
        conditionalWarning(
          c(
            "Note that LTBS is applied to the model with LogAdditive residual error only.",
            " It seems like there is no LogAdditive residual error in the current model;",
            " despite this, Will keep `logTransform = TRUE` flag as it is set."
          ), !logTransformIsNull
        )
      }
    } else if (length(residualErrors) == 0) {
      conditionalWarning(
        "logTransform flag is ignored for models without residual errors",
        !logTransformIsNull
      )
      ep@logtran <- 0
    }
  } else {
    # builtin
    residualErrors <- c()
    for (redidualErrorInstance in model@errorModel@effectsList) {
      residualErrors <-
        c(residualErrors, redidualErrorInstance@epsilonName)
      # LogAdditive = 2
      if (redidualErrorInstance@errorType == 2) {
        actual_hasLogAdditiveError <- TRUE
      }
    }

    ep@logtran <- as.numeric(logTransform)
    if (length(residualErrors) > 1 && logTransform) {
      conditionalWarning(
        c(
          "Log-transformation is not allowed for models with multiple residual errors;",
          " will set `logTransform` to FALSE."
        ),!logTransformIsNull
      )
      ep@logtran <- 0
    } else if (length(residualErrors) == 1 &&
               !actual_hasLogAdditiveError && logTransform) {
      conditionalWarning(
        c(
          "There is no LogAdditive residual error in the current model;",
          " will set `logTransform` to FALSE."
        ),
        !logTransformIsNull
      )

      ep@logtran <- 0
    }
  }

  ep
}

conditionalWarning <- function(WarningText, Condition = TRUE) {
  if (Condition) {
    warning(WarningText, call. = FALSE)
  }
}

# --- Helper Functions ---

.assignEngineMethod <- function(method, model) {
  if (is.null(method)) {
    method <- if (!model@isPopulation)
      "Naive-Pooled"
    else
      "QRPEM"
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
    stop("`method` must be one of \"",
         paste(names(methodMap), collapse = "\", \""),
         "\"")
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
    "DOPRI5" = 7
  )
  ODE <- toupper(ODE)
  if (length(ODEMap[[ODE]]) != 1) {
    stop("`ODE` must be one of \"",
         paste(names(ODEMap), collapse = "\", \""),
         "\"")
  }
  ODEMap[[ODE]]
}

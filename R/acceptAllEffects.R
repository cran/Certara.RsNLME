#' Accepts all estimates for fixed effects, Sigma, and random effects
#'
#' Updates PML statements in model object with estimates returned from model
#' execution. Use \code{\link{copyModel}} and set argument
#' \code{acceptAllEffects = TRUE} to create new model object with final
#' estimates from base model run.
#'
#' @param model Model object
#'
#' @examples
#' \dontrun{
#' # Define the model
#' model <- pkmodel(numComp = 1,
#'                  absorption = "Intravenous",
#'                  ID = "Subject",
#'                  Time = "Act_Time",
#'                  CObs = "Conc",
#'                  A1 = "Amount",
#'                  data = pkData,
#'                  modelName = "PkModel",
#'                  workingDir = tempdir())
#'
#' # Fit model
#' res <- fitmodel(model = model,
#'                 hostPlatform = hostParams(sharedDirectory = tempdir()))
#' model <- acceptAllEffects(model)
#' }
#' @return \code{NlmePmlModel} object
#' @seealso \code{\link{copyModel}}
#' @keywords internal
#' @export
acceptAllEffects <- function(model) {
  resultsFile <- file.path(model@modelInfo@workingDir, "dmp.txt")
  if (!file.exists(resultsFile)) {
    stop(
      "File with results",
      resultsFile,
      "not found.",
      "\nCannot update the model parameters with new estimates"
    )
  }

  if (model@isTextual == TRUE) {
    model_file <- file.path(tempdir(TRUE), "test.mdl")
    cat(paste(unlist(model@statements), collapse = "\n"), file = model_file)
    updatedModel <-
      Certara.NLME8::UpdateMDLfrom_dmptxt(
        dmpfile = resultsFile,
        SharedWorkingDir = model@modelInfo@workingDir,
        model_file = model_file,
        compile = FALSE
      )

    model@statements <-
      as.list(readLines(updatedModel, warn = FALSE))
  } else {
    source(resultsFile, local = TRUE)
    fixedEffects <- dmp.txt$coefficients$fixed
    omegas <- dmp.txt$omega
    sigmas <- dmp.txt$sigma
    initFixedEffects(model) <- fixedEffects
    if (length(model@randomValues) > 0) {
      model@randomValues@values <- omegas
    }
    model@randomEffectsStatements <-
      as.list(randomBlockStatement(model)) # Added to generate updated random effects
    model@randomOccasionalEffectsStatements <-
      as.list(randomOccasionalBlockStatement(model))

    if (length(model@errorModel@effectsList) > 0) {
      for (i in 1:length(model@errorModel@effectsList)) {
        epsilonName <- model@errorModel@effectsList[[i]]@epsilonName
        if (!is.na(fixedEffects[epsilonName])) {
          model@errorModel@effectsList[[i]]@SD <- fixedEffects[epsilonName]
        }
      }

      model <- generatePML(model)
    }
  }
  model
}

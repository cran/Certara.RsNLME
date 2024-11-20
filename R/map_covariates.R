COVAR_CONTINUOUS = 1
COVAR_CATEGORY = 2
COVAR_OCCASION = 3
map_covariates <- function(covariates, OldMapping, covariateList) {
  cov <- c()
  mCovCol <- c()
  for (i in seq_along(covariates)) {
    if (grepl(")", covariates[i], fixed = TRUE)) {
      covar <- gsub("\\(|\\)", "", covariates[i])
      covType <- COVAR_CATEGORY
    } else {
      # there's no way to differentiate continuous and occasion covariates
      # we do that later if there's info from previous mapping
      covar <- covariates[i]
      covType <- COVAR_CONTINUOUS
    }

    if (covar %in% names(OldMapping) &&
        OldMapping[[covar]]@variableType$type == "covariate" &&
        OldMapping[[covar]]@variableType$covType != covType &&
        OldMapping[[covar]]@variableType$covType == COVAR_OCCASION &&
        covType == COVAR_CONTINUOUS) {
      covType <- OldMapping[[covar]]@variableType$covType
    }

    if (covar %in% names(OldMapping)) {
      columnName <- OldMapping[[covar]]@columnName
    } else {
      columnName <- "?"
    }

    mCovCol <- c(
      mCovCol,
      NlmeColumnMap(
        variableName = covar,
        columnName = columnName,
        variableType = list(type = "covariate",
                            covType = covType)
      )
    )

    # Check if there is covariate with that name already in covariateList
    cov_list <- sapply(covariateList, function(x) {
      x@name == covar
    })
    # If there is a covariateList and if covariate is found
    if (length(cov_list) > 0 && any(cov_list)) {
      old_cov <- covariateList[which(cov_list)]
      # If covariate found but type has changed, create new
      if (old_cov[[1]]@type != covType) {
        new_cov <- NlmeCovariateParameter(name = covar, type = covType)
      } else {
        new_cov <- old_cov
      }
    } else {
      new_cov <- NlmeCovariateParameter(name = covar, type = covType)
    }

    cov <- c(cov, new_cov)
  }

  # returning covariates list and mapping structure of covariates
  list(cov = cov, mCovCol = mCovCol)
}

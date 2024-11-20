.get_omega_DFs <- function(omegaDF) {
  LabelPos <- which("Label" == colnames(omegaDF), arr.ind = TRUE)
  if (length(LabelPos) == 0) {
    return(NA)
  }
  NumOmegas <- ncol(omegaDF) - LabelPos

  # remove NA columns and omega label row
  omegaDF <-
    omegaDF[omegaDF$Label != "Omega",
            which(unlist(lapply(omegaDF, function(x)
              !all(is.na(x))))), with = FALSE]
  if (!is.null(omegaDF$Scenario) &&
      all(omegaDF$Scenario == "WorkFlow")) {
    # we don't need scenario column if it is specified by NLME8
    omegaDF$Scenario <- NULL
  }
  Eta_Shrinkage <- omegaDF[omegaDF$Label == "Shrinkage",]
  omega_woshrinkage <-
    omegaDF[!omegaDF$Label %in% c("Shrinkage", "Correlation"),]

  chunk <- NumOmegas
  n <- nrow(omega_woshrinkage)
  r  <- rep(1:ceiling(n / chunk), each = chunk)[1:n]
  omega_woshrinkage_splitted <- split(omega_woshrinkage, r)

  omega_Correlation <- data.frame()
  omega <- data.frame()

  for (dfN in names(omega_woshrinkage_splitted)) {
    if (as.integer(dfN) %% 2) {
      omega <- rbind(omega, omega_woshrinkage_splitted[[dfN]])
    } else {
      omega_Correlation <-
        rbind(omega_Correlation, omega_woshrinkage_splitted[[dfN]])
    }
  }

  rownames(omega) <- NULL
  rownames(omega_Correlation) <- NULL
  rownames(Eta_Shrinkage) <- NULL

  list(
    omega = omega,
    omega_Correlation = omega_Correlation,
    Eta_Shrinkage = Eta_Shrinkage
  )
}

# this function just removes Scenario column if not used and gives rownames if possible (without sort)
.get_omegaSE_DF <- function(omegaSEDF) {
  LabelPos <- which("Label" == colnames(omegaSEDF), arr.ind = TRUE)
  if (length(LabelPos) == 0) {
    return(NA)
  }

  if (!is.null(omegaSEDF$Scenario) &&
      all(omegaSEDF$Scenario == "WorkFlow")) {
    # we don't need scenario column if it is specified by NLME8
    omegaSEDF$Scenario <- NULL
  }

  `#` <- NULL # R CMD CHECK
  if ("#" %in% colnames(omegaSEDF)) {
    omegaSEDF <- omegaSEDF[, `#` := NULL]
  }

  rownames(omegaSEDF) <- NULL

  omegaSEDF
}

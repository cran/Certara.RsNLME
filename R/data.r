#' Pharmacokinetic dataset containing 16 subjects with single bolus dose
#'
#' Pharmacokinetic dataset containing 16 subjects with single bolus dose.
#'
#' @format A data frame with 112 rows and 8 variables:
#' \describe{
#'   \item{Subject}{Subject ID}
#'   \item{Nom_Time}{Nominal Time}
#'   \item{Act_Time}{Actual Time}
#'   \item{Amount}{Amount of dose}
#'   \item{Conc}{Observations of drug concentration in blood}
#'   \item{Age}{Age}
#'   \item{BodyWeight}{Body weight}
#'   \item{Gender}{Gender ("male", "female")}
#' }
#' @source \href{https://www.certara.com/training/}{Certara University}
"pkData"

#' Pharmacokinetic/Pharmacodynamic dataset containing 200 subjects with single bolus dose
#'
#' Pharmacokinetic/Pharmacodynamic dataset containing 200 subjects with single bolus dose.
#'
#' @format A data frame with 2600 rows and 5 variables:
#' \describe{
#'   \item{ID}{Subject ID}
#'   \item{Time}{Nominal Time}
#'   \item{Dose}{Amount of dose}
#'   \item{CObs}{Observations of drug concentration in blood}
#'   \item{EObs}{Observations of drug effect}
#' }
#' @source The data is simulated using a PKPD model with PK described by a one-compartment model with IV bolus and PD described by an indirect response model with the loss inhibited.
"pkpdData"

#' Pharmacokinetic pediatric dataset containing 80 subjects with single bolus dose.
#'
#' Pharmacokinetic pediatric dataset containing 80 subjects with single bolus dose. Dataset includes covariates and observations Below Quantification Limit (BQL).
#'
#' @format A data frame with 880 rows and 8 variables:
#' \describe{
#'   \item{ID}{Subject ID}
#'   \item{Time}{Nominal Time}
#'   \item{Dose}{Amount of dose}
#'   \item{CObs}{Observations of drug concentration in blood}
#'   \item{LLOQ}{Lower Limit of Quantification}
#'   \item{CObsBQL}{Variable that indicates whether the observed drug concentration is below the limit of quantification}
#'   \item{BW}{Body weight}
#'   \item{PMA}{Postmenstrual age}
#' }
#' @source The data is simulated using a one-compartment model with IV bolus, where the central volume is allometric weight scaled, and the clearance is scaled by a combination of allometric weight scaling and a sigmoidal maturation function driven by PMA. Germovsek E., et al, Pharmacokinetic–Pharmacodynamic Modeling in Pediatric Drug Development, and the Importance of Standardized Scaling of Clearance, Clin Pharmacokinet (2019) 58:39–52.
"pkcovbqlData"

#' Pharmacokinetic dataset containing 100 subjects with single dose given by infusion
#'
#' Pharmacokinetic dataset containing 16 subjects with single dose given by infusion.
#'
#' @format A data frame with 800 rows and 6 variables:
#' \describe{
#'   \item{Subject}{Subject ID}
#'   \item{Time}{Time point}
#'   \item{Dose}{Amount of dose}
#'   \item{CObs}{Observations of drug concentration in blood}
#'   \item{Rate}{Rate of infusion}
#'   \item{Duration}{Duration of infusion}
#' }
#' @source The data is simulated using a PK model described by a one-compartment model with IV infusion
"OneCpt_IVInfusionData"

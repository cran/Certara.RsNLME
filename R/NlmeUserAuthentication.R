#' Class initializer for NlmeUserAuthentication
#'
#' Use for authentication records
#'
#' @slot userName             How the user is identified to the remote system
#' @slot privateKeyFile       path to private key file, see \code{\link[ssh:ssh_connect]{keyfile}}
#'                            for details
#' @slot userPassword         either a string or a callback function for password prompt, see
#'                            \code{\link[ssh:ssh_connect]{passwd}} for details
#'
#'
#' @export NlmeUserAuthentication
#' @keywords internal
NlmeUserAuthentication <-
  setClass(
    "NlmeUserAuthentication",
    slots = c(
      userName = "character",
      privateKeyFile = "ANY",
      userPassword = "ANY"
    )
  )

setMethod("initialize", "NlmeUserAuthentication",
  function(.Object,
           userName = "",
           privateKeyFile = NULL,
           userPassword = NULL) {
    .Object@userName <- userName
    .Object@privateKeyFile <- privateKeyFile
    .Object@userPassword <- userPassword
    .Object
  }
)

#'
#' @export
#' @keywords internal
print.NlmeUserAuthentication <- function(x, ...) {
  cat("\n NLME User Authentication \n ------------------------------------------- \n")

  cat(paste("Username            : ", x@userName), fill = TRUE)
  cat(paste("Private key file    : ", x@privateKeyFile), fill = TRUE)
  cat(paste("Password            : ", x@userPassword), fill = TRUE)
}


setMethod(
  "show",
  "NlmeUserAuthentication",
  definition = function(object) {
    print(object)
  }
)

#'Class initializer for NlmeParallelMethod
#'
#'Use to define NlmeParallelMethod as for `parallelMethod` argument in
#'[NlmeParallelHost()].
#'
#'@slot method Options are:
#'  `None|Multicore|LOCAL_MPI|SGE|SGE_MPI|TORQUE|TORQUE_MPI|LSF|LSF_MPI|SLURM|SLURM_MPI`.
#'
#'@md
#'@export NlmeParallelMethod
#'@keywords internal
NlmeParallelMethod <-
  setClass("NlmeParallelMethod",
           slots = c(method = "character"))

setMethod("initialize", "NlmeParallelMethod",
          function(.Object,
                   method = "None") {
            if (!toupper(method) %in% .get_supportedMethods()) {
              if (toupper(method) == "MPI") {
                # same as LOCAL_MPI
                method <- "LOCAL_MPI"
              } else {
                warning(paste(method, "Is not supported! using NONE"))
                method <- "NONE"
              }

            }
            .Object@method <- method
            .Object
          })


.get_supportedMethods <- function(platform = "linux") {
  if (tolower(platform) != "windows") {
    supportedMethods <-
      c(
        "NONE",
        "MULTICORE",
        "LOCAL_MPI",
        "TORQUE",
        "TORQUE_MPI",
        "SGE",
        "SGE_MPI",
        "LSF",
        "LSF_MPI",
        "SLURM",
        "SLURM_MPI"
      )
  } else {
    supportedMethods <-
      c(
        "NONE",
        "MULTICORE",
        "LOCAL_MPI"
      )
  }
}

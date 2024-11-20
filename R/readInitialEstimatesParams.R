readInitialEstimatesParams <- function(fileName) {
  inputFile <- file(fileName, "rb")

  numThetas <- readBin(inputFile, integer(), n = 1)

  for (n in 1:numThetas) {
    name <- readBin(inputFile, character(), n = 1)
    value <- readBin(inputFile, numeric(), n = 1)
  }
  numVars <- readBin(inputFile, integer(), n = 1)

  if (numVars > 0) {
    for (n in 1:numVars) {
      name <- readBin(inputFile, character(), n = 1)
    }
  }

  numSteps <- readBin(inputFile, integer(), n = 1)

  steps <- readBin(inputFile, numeric(), n = numSteps)

  end <- readBin(inputFile, integer(), n = 1)
}

parseControlFile <- function(localDir, controlFile) {
  if (dirname(controlFile) == ".") {
    lines <- readLines(file.path(localDir, controlFile))
  } else {
    lines <- readLines(controlFile)
  }
  modelFile <- trimws(lines[1])
  filesToCopy <- trimws(lines[2])
  colFile <- unlist(strsplit(filesToCopy, " "))[2]
  dataFile <- unlist(strsplit(filesToCopy, " "))[3]
  controlLine <- trimws(lines[5])
  argsFile <- unlist(strsplit(controlLine, ","))[2]
  tokens <- unlist(strsplit(argsFile, ":"))
  if (length(tokens) > 2) {
    argsFile <- paste0(tokens[1], ":", tokens[2])
  } else {
    argsFile <- tokens[1]
  }

  outputFile <- unlist(strsplit(controlLine, ","))[4]
  if (dirname(argsFile) == ".") {
    lines <- readLines(file.path(localDir, argsFile))
  } else {
    lines <- readLines(argsFile)
  }

  for (l in lines) {
    pos <- unlist(strsplit(l, split = "/m "))
    if (length(pos) > 1) {
      method <- unlist(strsplit(pos[2], split = " "))[1]
    }
    pos <- unlist(strsplit(l, split = "/n "))
    if (length(pos) > 1) {
      iterations <- unlist(strsplit(pos[2], split = " "))[1]
    }
  }

  return(c(
    modelFile, dataFile, colFile, argsFile, outputFile, filesToCopy,
    method, iterations
  ))
}


get_extraArgsFile <- function(argsFileLines) {
  extraArgsFile <- unlist(strsplit(argsFileLines[5], ","))[2]
  tokens <- unlist(strsplit(extraArgsFile, ":"))
  if (length(tokens) > 2) {
    extraArgsFile <- paste0(tokens[1], ":", tokens[2])
  } else {
    extraArgsFile <- tokens[1]
  }

  extraArgsFile
}

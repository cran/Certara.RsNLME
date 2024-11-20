# Test table class and coldef written

test_that("test for NlmeTableDef and addTablesToColumnMapping", {
  workingDir <- file.path(tempdir(TRUE), "TableTest")
  dir.create(workingDir, recursive = TRUE)
  model <- pkmodel(
    absorption = "Extravascular",
    data = pkData, ID = "Subject", Time = "Act_Time", Aa = "Amount", CObs = "Conc",
    modelName = "TableTest",
    workingDir = workingDir
  )

  testthat::local_edition(3)
  testthat::expect_snapshot_error(tableParams(
    name = "Table1.csv",
    timesList = seq(0, 24, 2),
    variablesList = "C",
    IRES = TRUE,
    IWRES = TRUE,
    Weight = TRUE
  ))


  testthat::expect_snapshot(tableParams(
    name = "Table1.csv",
    whenObs = c("CObs"),
    variablesList = "C",
    IRES = TRUE,
    IWRES = TRUE,
    Weight = TRUE,
    keepSource = TRUE
  ))

  Table1 <- tableParams(
    name = "Table1.csv",
    timesList = seq(0, 24, 2),
    whenObs = c("CObs"),
    variablesList = "C",
    IRES = TRUE,
    IWRES = TRUE,
    Weight = TRUE
  )

  tablecoldef <- "colstest.txt"
  if (file.exists(file.path(workingDir, tablecoldef))) {
    file.remove(file.path(workingDir, tablecoldef))
  }

  testthat::expect_snapshot(addTablesToColumnMapping(
    model = model,
    Tables = Table1,
    filename = tablecoldef
  ))

  testthat::expect_snapshot_file(file.path(workingDir, tablecoldef),
                                 compare = compare_file_text)

  TableSim <- NlmeSimTableDef(
    name = "simTable1.csv",
    whenObs = c("CObs"),
    whenDose = "Aa",
    variablesList = "C",
    keepSource = TRUE
  )

  simtablecoldef <- "simcolstest.txt"

  if (file.exists(file.path(workingDir, simtablecoldef))) {
    file.remove(file.path(workingDir, simtablecoldef))
  }

  testthat::expect_snapshot(addTablesToColumnMapping(
    model = model,
    Tables = TableSim,
    filename = simtablecoldef,
    forSim = TRUE
  ))

  testthat::expect_snapshot_file(file.path(workingDir, simtablecoldef),
                                 compare = compare_file_text)
})

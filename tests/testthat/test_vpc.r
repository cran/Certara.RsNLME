test_that("class running a VPC simulation  ", {
  testthat::skip_on_cran()
  testthat::skip_if(Sys.getenv("INSTALLDIR") == "",
                    message = "cannot start the test, INSTALLDIR variable is not specified.")

  testthat::skip()

  workingDir = tempdir(TRUE)
  NLME_ROOT_DIRECTORY <- file.path(workingDir, "VPCTest")
  dir.create(NLME_ROOT_DIRECTORY)
  Sys.setenv("NLME_ROOT_DIRECTORY" = NLME_ROOT_DIRECTORY)

  # adding one more categorical covariate
  pkData$AgeGroup <- ifelse(pkData$Age > mean(pkData$Age),
                            1,
                            2)

  # Define the model
  model <- pkmodel(
    numCompartments = 2,
    ID = "Subject",
    Time = "Act_Time",
    CObs = "Conc",
    A1 = "Amount",
    data = pkData,
    modelName = "PkModel" ,
    workingDir = workingDir
  )

  # Add Gender covariate of type categorical
  model <- addCovariate(
    model,
    covariate = "Gender",
    type = "Categorical",
    effect = c("V2", "Cl2"),
    levels = c(0, 1),
    labels = c("Female", "Male")
  )

  # Add Gender covariate of type categorical
  model <- addCovariate(
    model,
    covariate = "AgeGroup",
    type = "Categorical",
    effect = c("V"),
    levels = c(1, 2)
  )


  observe <-
    NlmeObservationVar(
      name = "CObs",
      quantilesValues = c(5, 50, 95),
      stratifyColumns = c("Gender")
    )

  SimTable <- NlmeSimTableDef(name = "SimTable.csv",
                              timesList = seq(0,100,10),
                              variablesList = c("C","CObs"),
                              covrSet = c("Gender", "AgeGroup"))

  vpc <-
    NlmeVpcParams(
      observationVars = list(observe),
      stratifyColumns = c("Gender", "AgeGroup")
    )

  res <-
    vpcmodel(
      model = model,
      vpcParams = vpc,
      numReplicates = 2,
      seed = 1234,
      simulationTables = SimTable
    )

  testthat::local_edition(3)
  # in non-interactive mode the table is not returned
  testthat::expect_snapshot(res$predout)
})

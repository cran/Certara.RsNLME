test_that("parsePmlColMap works properly", {
  testthat::skip_on_cran()
  testthat::skip_if(Sys.getenv("INSTALLDIR") == "",
                    message = "cannot start the test, INSTALLDIR variable is not specified.")


  data <- pkData
  data$SS <- ifelse(data$Nom_Time == 0, 1, 0)
  data$II <- ifelse(data$SS == 1, 12, NA)
  model <- pkmodel(
    numCompartments = 2,
    data = data,
    id = Subject,
    time = Act_Time,
    A1 = Amount,
    CObs = Conc
  )

  model <-
    addDoseCycle(
      model,
      type = "SteadyState",
      name = "A1",
      amount = "Amount",
      II = "II",
      colName = "SS"
    )

  model2 <- parsePMLColMap(model)

  testthat::expect_equal(capture.output(print(model)),
                         capture.output(print(model2)))

  urinedat <- data.frame(ID = NA,
                         Time = NA,
                         Dose = NA,
                         Conc = NA,
                         UrineAmtCum = NA)

  urineBuiltin <-
    pkmodel(
      hasEliminationComp = TRUE,
      isClosedForm = FALSE,
      isFractionExcreted = TRUE,
      data = urinedat,
      id = ID,
      time = Time,
      A1 = Dose,
      CObs = Conc,
      A0Obs = UrineAmtCum
    )

  parsePMLColMapRes <- parsePMLColMap(urineBuiltin)
  testthat::expect_identical(parsePMLColMapRes@columnMapping@mapping$CObs@columnName, "Conc")
  testthat::expect_identical(parsePMLColMapRes@columnMapping@mapping$A0Obs@columnName, "UrineAmtCum")

  # A1Strip in textual mode
  pkData$strip <- pkData$Amount

  model <- pkmodel(
    parameterization = "Macro",
    numCompartments = 2,
    data = pkData,
    ID = "Subject",
    Time = "Act_Time",
    A1 = "Amount",
    C1Obs = "Conc",
    A1Strip = "strip"
  )

  model <- addCovariate(model, covariate = "BodyWeight", effect = "Beta", center = "Mean")
  model <- parsePMLColMap(model)
  testthat::expect_identical(model@columnMapping@mapping$A1Strip@columnName, "strip")
})

# Fixed Effect Initialization

test_that("Test fixed effects initialization", {
  location <-
    system.file("extdata/FixedEffects", "set_fixed_effect_1.txt", package = "Certara.RsNLME")
  referenceModel <- gsub("\\s+", "", readLines(location))

  model <- pkmodel(
    numCompartments = 1,
    absorption = "Intravenous",
    modelName = "InitialModel",
    columnMap = FALSE
  )

  model <- model %>%
    residualError(errorType = "Additive", SD = 1) %>%
    fixedEffect(effect = c("tvV", "tvCl"), value = c(16, 41))

  newModel <- gsub("\\s+", "", as.character(model@statements))

  expect_that(referenceModel, equals(newModel))
})


test_that("Test fixed effect is frozen if previously added covariate effect",
          {
            statements <-  c(
              "test(){",
              "    cfMicro(A1,Cl/V)",
              "    dosepoint(A1)",
              "    C = A1 / V",
              "    error(CEps=0.1)" ,
              "    observe(CObs=C * ( 1 + CEps))",
              "    stparm(V = tvV * ((BW/mean(BW))^dVdBW)   * exp(nV))",
              "    stparm(Cl = tvCl * exp(nCl))",
              "    fcovariate(BW)",
              "    fixef( tvV(freeze) = c(,3,))",
              "    fixef( tvCl = c(,1,))",
              "    fixef( dVdBW(enable=c(0)) = c(1,2,3))",
              "    ranef(diag(nV,nCl) =  c(1,1))",
              "}"
            )


            model <- pkmodel(columnMap = FALSE, modelName = "PKPDmodel")

            model <-
              addCovariate(
                model,
                covariate = "BW",
                effect = "V",
                type = "Continuous",
                direction = "Forward",
                center = "Mean"
              )

            model <-
              fixedEffect(
                model,
                effect = "dVdBW",
                value = 2,
                lowerBound = 1,
                upperBound = 3,
                isFrozen = FALSE
              )

            model <-
              fixedEffect(model,
                          effect = "tvV",
                          value = 3,
                          isFrozen = TRUE)

            expect_that(unlist(model@statements), equals(statements))
          })

test_that("Test fixed effects stay frozen if PK and Emax are frozen in structural model",
          {
            statements <- c(
              "test(){",
              "    cfMicro(A1,Cl/V)",
              "    dosepoint(A1)",
              "    C = A1 / V",
              "    E = Emax * C / (EC50 + C)",
              "    stparm(V = tvV)",
              "    stparm(Cl = tvCl)",
              "    stparm(EC50 = tvEC50)",
              "    stparm(Emax = tvEmax)",
              "    fixef( tvV(freeze) = c(,3,))",
              "    fixef( tvCl(freeze) = c(,1,))",
              "    fixef( tvEC50(freeze) = c(,4,))",
              "    fixef( tvEmax(freeze) = c(,1,))",
              "}"
            )


            model <-
              pkemaxmodel(
                columnMap = FALSE,
                modelName = "PKPDmodel",
                isPkFrozen = TRUE,
                isEmaxFrozen = TRUE
              )

            model <-
              fixedEffect(model,
                          effect = c("tvV", "tvEC50"),
                          value = c(3, 4))

            expect_that(unlist(model@statements), equals(statements))
          })

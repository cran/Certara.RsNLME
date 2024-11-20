# Test random effects initialization

test_that("Test random effects initialization", {
  location <- system.file("extdata/RandomEffects",
                         "test_rand_effect_1.txt",
                         package = "Certara.RsNLME")
  referenceModel <- gsub("\\s+", "", readLines(location))

  model <- pkmodel(
    isPopulation = TRUE,
    numCompartments = 2,
    absorption = "Intravenous",
    modelName = "PK",
    isClosedForm = FALSE,
    columnMap = FALSE
  )

  model <- residualError(model, errorType = "Additive", SD = 1)
  #initRandomEffects(model)=c(Diagonal,FALSE,"nV,nCl,nV2,nCl2","0.2,0.09,0.1,0.01")
  model <-
    randomEffect(
      model,
      effect = c("nV", "nCl", "nV2", "nCl2"),
      value = c(0.2, 0.09, 0.1, 0.01)
    )

  newModel <- gsub("\\s+", "", as.character(model@statements))

  expect_that(referenceModel, equals(newModel))
})

test_that("Test multiple block effects initialization", {
  model <- pkmodel(
    isPopulation = TRUE,
    numCompartments = 2,
    absorption = "Intravenous",
    modelName = "PK",
    isClosedForm = FALSE,
    columnMap = FALSE
  )

  #initRandomEffects(model)=c(Diagonal,FALSE,"nV,nCl","0.2,0.09",
  #                           Block,TRUE,"nV2,nCl2","0.1,0,0.01")
  model <- model %>%
    randomEffect(effect = c("nV", "nCl"), value = c(0.2, 0.09)) %>%
    randomEffect(
      effect = c("nV2", "nCl2"),
      value = c(0.1, 0, 0.01),
      isDiagonal = FALSE,
      isFrozen = TRUE
    )
  pos <- grep("ranef", model@statements)
  randomEffectLine <- gsub("\\s+", "", model@statements[[pos[[1]]]])

  expect_that(
    randomEffectLine,
    equals(
      "ranef(diag(nV,nCl)=c(0.2,0.09),block(nV2,nCl2)(freeze)=c(0.1,0,0.01))"
    )
  )
})


test_that("Test specify initialization with missing variable ", {
  model <- pkmodel(
    isPopulation = TRUE,
    numCompartments = 2,
    absorption = "Intravenous",
    modelName = "PK",
    isClosedForm = FALSE,
    columnMap = FALSE
  )

  model <- model %>%
    randomEffect(effect = c("nV", "nCl"), value = c(0.2, 0.09)) %>%
    randomEffect(effect = c("nV2", "nCl2"), isFrozen = TRUE)

  #initRandomEffects(model)=c(Diagonal,FALSE,"nV,nCl","0.2,0.09")
  pos <- grep("ranef", model@statements)
  randomEffectLine <- gsub("\\s+", "", model@statements[[pos[[1]]]])

  expect_that(
    randomEffectLine,
    equals(
      "ranef(diag(nV,nCl)=c(0.2,0.09),diag(nV2,nCl2)(freeze)=c(1,1))"
    )
  )
})




test_that(
  "Updated random effect names using structuralParameter() appear in random effects block",
  {
    model <-
      pkindirectmodel(
        numCompartments = 2,
        indirectType = "LimitedInhibition",
        columnMap = FALSE
      )
    model <-
      structuralParameter(
        model,
        paramName = "Imax",
        style = "LogitNormal",
        fixedEffName = "tvlogitImax",
        randomEffName = "nlogitImax"
      )
    model <-
      randomEffect(model,
                   effect = c("nlogitImax", "nIC50"),
                   value = c(0.1, 0.01))

    pos <- grep("ranef", model@statements)
    randomEffectLine <- gsub("\\s+", "", model@statements[[pos[[1]]]])

    expect_that(
      randomEffectLine,
      equals(
        "ranef(diag(nV,nCl,nV2,nCl2,nKin,nKout)=c(1,1,1,1,1,1),diag(nlogitImax,nIC50)=c(0.1,0.01))"
      )
    )
  }
)

test_that("random effect statement correctly generate after changing hasRandomEffect argument",
          {
            model <-
              pkmodel(
                numCompartments = 2,
                data = pkData,
                ID = "Subject",
                Time = "Act_Time",
                A1 = "Amount",
                CObs = "Conc"
              )
            model <-
              structuralParameter(
                model,
                paramName = "V2",
                randomEffName = "n_V2",
                hasRandomEffect = FALSE
              )

            pos <- grep("ranef", model@statements)
            randomEffectLine <- gsub("\\s+", "", model@statements[[pos[[1]]]])

            expect_that(randomEffectLine,
                        equals("ranef(diag(nV,nCl,nCl2)=c(1,1,1))"))

            model <-
              randomEffect(
                model,
                effect = c("nV", "nCl"),
                value = c(0.1, 0.01, 0.1),
                isDiagonal = FALSE
              )

            pos <- grep("ranef", model@statements)
            randomEffectLine <- gsub("\\s+", "", model@statements[[pos[[1]]]])

            expect_that(
              randomEffectLine,
              equals("ranef(diag(nCl2)=c(1),block(nV,nCl)=c(0.1,0.01,0.1))")
            )
            #Add random effect back
            model <-
              structuralParameter(model, paramName = "V2", hasRandomEffect = TRUE)

            pos <- grep("ranef", model@statements)
            randomEffectLine <- gsub("\\s+", "", model@statements[[pos[[1]]]])

            expect_that(
              randomEffectLine,
              equals(
                "ranef(diag(nCl2)=c(1),block(nV,nCl)=c(0.1,0.01,0.1),diag(n_V2)=c(1))"
              )
            )
          })

test_that("random effect statement correctly generates adding random effect for CMultStdev",
          {
            model <-
              pkindirectmodel(
                isPopulation = TRUE,
                parameterization = "Clearance",
                absorption = "Intravenous",
                numCompartments = 1,
                isClosedForm = TRUE,
                isTlag = FALSE,
                hasEliminationComp = FALSE,
                isFractionExcreted = FALSE,
                isSaturating = FALSE,
                infusionAllowed = FALSE,
                isDuration = FALSE,
                isPkFrozen = FALSE,
                hasEffectsCompartment = FALSE,
                indirectType = "LimitedStimulation",
                isBuildup = TRUE,
                isExponent = FALSE,
                indirectFrozen = FALSE,
                columnMap = FALSE,
                modelName = "PKPDmodel"
              )
            model <-
              residualError(
                model,
                predName = "E",
                errorType = "Multiplicative",
                SD = 0.1,
                isFrozen = FALSE,
                isBQL = FALSE
              )
            model <-
              residualError(
                model,
                predName = "C",
                errorType = "AdditiveMultiplicative",
                SD = 1,
                isFrozen = FALSE,
                isBQL = FALSE
              )
            model <-
              structuralParameter(
                model,
                paramName = "V",
                fixedEffName = "tvV",
                randomEffName = "nV",
                style = "LogNormal",
                hasRandomEffect = TRUE
              )
            model <-
              structuralParameter(
                model,
                paramName = "Cl",
                fixedEffName = "tvCl",
                randomEffName = "nCl",
                style = "LogNormal",
                hasRandomEffect = TRUE
              )
            model <-
              structuralParameter(
                model,
                paramName = "Kin",
                fixedEffName = "tvKin",
                randomEffName = "nKin",
                style = "LogNormal",
                hasRandomEffect = TRUE
              )
            model <-
              structuralParameter(
                model,
                paramName = "Emax",
                fixedEffName = "tvEmax",
                randomEffName = "nEmax",
                style = "LogNormal",
                hasRandomEffect = TRUE
              )
            model <-
              structuralParameter(
                model,
                paramName = "EC50",
                fixedEffName = "tvEC50",
                randomEffName = "nEC50",
                style = "LogNormal",
                hasRandomEffect = TRUE
              )
            model <-
              structuralParameter(
                model,
                paramName = "CMultStdev",
                fixedEffName = "tvCMultStdev",
                randomEffName = "nCMultStdev",
                style = "LogNormal",
                hasRandomEffect = TRUE
              )
            model <-
              structuralParameter(
                model,
                paramName = "Kout",
                fixedEffName = "tvKout",
                randomEffName = "nKout",
                style = "LogNormal",
                hasRandomEffect = TRUE
              )
            model <-
              randomEffect(
                model,
                effect = "nCMultStdev",
                value = 2,
                isDiagonal = TRUE,
                isFrozen = FALSE
              )

            pos <- grep("ranef", model@statements)
            randomEffectLine <- gsub("\\s+", "", model@statements[[pos[[1]]]])

            expect_that(
              randomEffectLine,
              equals(
                "ranef(diag(nV,nCl,nKin,nKout,nEmax,nEC50)=c(1,1,1,1,1,1),diag(nCMultStdev)=c(2))"
              )
            )
          })

test_that("Test remove all random effects and add a block again ", {
  model <- pkmodel(parameterization = "Clearance",
                   numCompartments = 1,
                   absorption	= "FirstOrder",
                   columnMap = FALSE)

  ## remove all etas
  for (this_parm in structuralParameterNames(model)) {
    model <-
      structuralParameter(model, paramName = this_parm, # only those to be removed
                          hasRandomEffect = FALSE)
  }

  # add back V and CL
  model <-
    structuralParameter(model, paramName = "V", hasRandomEffect = TRUE)
  model <-
    structuralParameter(model, paramName = "Cl", hasRandomEffect = TRUE)

  model <-
    randomEffect(
      model,
      effect = c("nCl", "nV"),
      isDiagonal = FALSE,
      value = c(0.3, 0.1, 0.3)
    )

  expect_that(
    unlist(model@randomBlocks[[1]]@effectNames),
    equals(
      c("nCl", "nV")
    )
  )
})

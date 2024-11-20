# Test models with distributed delay correctly generated

test_that("delayInfCpt statement correctly generated", {
  model <-
    pkmodel(
      absorption = "Gamma",
      hasEliminationComp = TRUE,
      columnMap = FALSE
    )

  pos <- grep("delayInfCpt", model@statements)
  delayInfCptLine <- gsub("\\s+", "", model@statements[[pos[[1]]]])

  testthat::expect_that(
    delayInfCptLine,
    testthat::equals(
      "delayInfCpt(A1,MeanDelayTime,ShapeParamMinusOne,out=-Cl*C,dist=Gamma)"
    )
  )

  model <-
    pkindirectmodel(
      absorption = "Weibull",
      hasEliminationComp = TRUE,
      columnMap = FALSE
    )

  pos <- grep("delayInfCpt", model@statements)
  delayInfCptLine <- gsub("\\s+", "", model@statements[[pos[[1]]]])

  testthat::expect_that(
    delayInfCptLine,
    testthat::equals(
      "delayInfCpt(A1,MeanDelayTime,ShapeParamMinusOne,out=-Cl*C,dist=Weibull)"
    )
  )

  model <-
    pkemaxmodel(
      absorption = "InverseGaussian",
      hasEliminationComp = TRUE,
      columnMap = FALSE
    )

  pos <- grep("delayInfCpt", model@statements)
  delayInfCptLine <- gsub("\\s+", "", model@statements[[pos[[1]]]])

  testthat::expect_that(
    delayInfCptLine,
    testthat::equals(
      "delayInfCpt(A1,MeanDelayTime,ShapeParam,out=-Cl*C,dist=InverseGaussian)"
    )
  )

})

test_that("model parameters are set corrrectly", {
  model <-
    pkmodel(
      absorption = "Gamma",
      hasEliminationComp = TRUE,
      columnMap = FALSE
    )

  testthat::expect_that(model@pkModelAttrs@parameterization@paramType,
                        testthat::equals(2))
  testthat::expect_that(model@pkModelAttrs@infusionAllowed,
                        testthat::equals(FALSE))
  testthat::expect_that(model@pkModelAttrs@isDuration, testthat::equals(FALSE))
  testthat::expect_that(model@pkModelAttrs@isSequential, testthat::equals(FALSE))
  testthat::expect_that(model@pkModelAttrs@isPkFrozen, testthat::equals(FALSE))
  testthat::expect_that(model@pkModelAttrs@isTlag, testthat::equals(FALSE))
  testthat::expect_that(model@pkModelAttrs@isClosedForm, testthat::equals(FALSE))
  testthat::expect_that(model@pkModelAttrs@hasEliminationComp,
                        testthat::equals(TRUE))
  testthat::expect_that(model@pkModelAttrs@isFractionExcreted,
                        testthat::equals(FALSE))
  testthat::expect_that(model@pkModelAttrs@isSaturating, testthat::equals(FALSE))

})

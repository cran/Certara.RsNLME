test_that("class running an estimation with sort columns ", {
  jobTempl <- system.file("extdata/NlmeJobStatus/jobTempl.RDS",
                          package = "Certara.RsNLME",
                          mustWork = TRUE)
  job <- readRDS(jobTempl)

  job@localDir <- system.file("extdata/NlmeJobStatus/",
                              package = "Certara.RsNLME")

  testthat::local_edition(3)
  expect_snapshot(NlmeJobStatus(job))

})

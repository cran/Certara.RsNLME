

test_that("Parallel host class validation test", {
  testthat::expect_is(NlmeParallelHost(), "NlmeParallelHost")
})

test_that("test parallel host default values",  {
  host <- NlmeParallelHost()
  testthat::expect_equal(host@machineName, Sys.info()[["nodename"]])
  testthat::expect_equal(host@hostType, Sys.info()[["sysname"]])
  testthat::expect_equal(host@parallelMethod@method, "None")
  testthat::expect_equal(host@numCores, 1)
})

test_that("Parallel method class validation test", {
  testthat::expect_is(NlmeParallelMethod(), "NlmeParallelMethod")
})

test_that("invalid parallel method", {
  testthat::expect_warning(NlmeParallelMethod("junk"))
})

test_that("test parallel method default value", {
  testthat::expect_equal(NlmeParallelMethod()@method, "None")
})

test_that("test print parallel host default values",  {
  host <- NlmeParallelHost(
    sharedDirectory = "shared",
    installationDirectory = "INSTALLDIR"
  )

  lines <- capture.output(print(host))
  #lines <- capture.output(print(host))
  testthat::expect_equal(lines[7], paste0("Address of the host         :  ", Sys.info()[["nodename"]]))
  testthat::expect_equal(lines[8], paste0("Host System                 :  ", Sys.info()[["sysname"]]))
  testthat::expect_equal(lines[9], "Number of cores             :  1")
  testthat::expect_equal(lines[13], "Parallel method             :  None")

})

test_that("hostParams function works with all arguments", {
  testthat::local_edition(3)
  testthat::expect_snapshot_output(print(hostParams(sharedDirectory = "sharedDirectoryPath",
                                                    installationDirectory = "installationDirectoryPath",
                                                    hostName = "Test",
                                                    hostType = "UBUNTU",
                                                    numCores = 2,
                                                    parallelMethod = "LSF",
                                                    userName = "test",
                                                    privateKeyFile = "privateKeyFilePath",
                                                    userPassword = "Password",
                                                    scriptPath = "scriptPath",
                                                    rLocation = "rLocationPath",
                                                    isLocal = FALSE)))
})

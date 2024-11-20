test_that("categorical observation mapping works  ", {
  testthat::skip_on_cran()
  testthat::skip_if(Sys.getenv("INSTALLDIR") == "",
                    message = "cannot start the test, INSTALLDIR variable is not specified.")

  workingDir = tempdir(TRUE)

  model <- textualmodel(modelName = "CategoricalModel_VPC", data = pkData, workingDir = workingDir)
  mdl <- "test(){
	cfMicro(A1, Cl / V, first = (Aa = Ka))
	dosepoint(Aa)
	C = A1 / V
	error(CEps = 0.121901345020298)
	observe(CObs = C * (1 + CEps))
	stparm(Ka = exp(tvlogKa + nlogKa))
	stparm(V = exp(tvlogV + nlogV))
	stparm(Cl = exp(tvlogCl + nlogCl))
	fixef(tvlogKa = c(,-0.356814599929669,))
	fixef(tvlogV = c(,1.63708816103322,))
	fixef(tvlogCl = c(,-0.224349311854445,))
	ranef(diag(nlogV, nlogCl, nlogKa) = c(0.086595005, 0.10519789, 0.11496373))
	E = Emax * C / (EC50 + C)
	error(EEps = 0.179826134019237)
	observe(EObs = E * (1 + EEps))
	multi(CategoricalObs, ilogit, -E, -(E + CatParam))
	stparm(EC50 = exp(tvlogEC50 + nlogEC50))
	stparm(Emax = exp(tvlogEmax + nlogEmax))
	stparm(CatParam = exp(tvlogCatParam + nlogCatParam))
	fixef(tvlogEC50 = c(,2.29009944179079,))
	fixef(tvlogEmax = c(,-2.3003019591701,))
	fixef(tvlogCatParam = c(,0.997620495151059,))
	ranef(diag(nlogEC50, nlogEmax, nlogCatParam) = c(0.066115814, 0.10268428, 1.1051435))}"

  model@statements <- as.list(mdl)
  model <- Certara.RsNLME:::parsePMLColMap(model)

  model <-
    colMapping(model, c(id = "Subject",
                        time = "Nom_Time",
                        Aa = "Amount",
                        CObs = "Conc",
                        EObs = "BodyWeight",
                        CategoricalObs = "Gender"))

  testthat::local_edition(3)
  # exclude working directory
  PrintModelOutput <- capture.output(print(model))[-c(5)]

  # in non-interactive mode the table is not returned
  testthat::expect_snapshot_value(as.list(PrintModelOutput), style = "json")
})

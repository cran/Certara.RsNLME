generateScenarioArg <- function(scenario) {
  argFlag <- " /xe "
  submodels <- c()
  for (i in 1:50) {
    submodels <- c(submodels, FALSE)
  }
  covariatesList <- attr(scenario, "covariatesList")
  if (length(covariatesList) == 0 || covariatesList == "") {
    argFlag <- " /xe _ "
  } else {
    covarsUsed <- unlist(strsplit(covariatesList, split = ","))
    for (i in covarsUsed) {
      argFlag <- paste0(argFlag, "_", (as.integer(i) - 1))
    }
    argFlag <- paste0(argFlag, "_ ")
  }
  return(argFlag)
}


GenerateParamsfile <-
  function(argsFilename,
           dataset,
           params,
           bootStratify = "",
           vpcOption = NULL,
           simOption = NULL,
           scenarios = c()) {
    appendFlag <- FALSE

    numTodo <- length(scenarios)
    done <- FALSE
    current <- 1
    while (!done) {
      if (is.null(params) && dataset@phoenixSourceDir != "") {
        lines <- readLines(file.path(dataset@phoenixSourceDir,
                                     dataset@engineParamsFile))

        for (l in lines) {
          cat(
            sprintf(" %s ", l),
            file = argsFilename,
            sep = "\n",
            append = appendFlag
          )

          appendFlag <- TRUE
        }
      } else {
        if (numTodo != 0) {
          arg <- generateScenarioArg(scenarios[[current]])
          cat(
            sprintf(" %s ", arg),
            file = argsFilename,
            sep = "\n",
            append = appendFlag
          )

          appendFlag <- TRUE
        } else {
          cat(
            sprintf(" -e -1 "),
            file = argsFilename,
            sep = "\n",
            append = appendFlag
          )

          appendFlag <- TRUE
        }

        if (!is.null(vpcOption)) {
          # /predn
          cat(
            sprintf(" -predn %d  ", vpcOption@numReplicates),
            file = argsFilename,
            sep = "\n",
            append = appendFlag
          )

          appendFlag <- TRUE
          cat(
            sprintf(" -predout %s  ", dataset@predoutFilename),
            file = argsFilename,
            sep = "\n",
            append = appendFlag
          )

          cat(
            sprintf(" -pcseed %d  ", vpcOption@seed),
            file = argsFilename,
            sep = "\n",
            append = appendFlag
          )


          predCorrection <- vpcOption@predCorrection

          if (predCorrection %in% c("proportional", "additive")) {
            if (vpcOption@predVarCorr) {
              predValue <- " -predvpc"
            } else {
              predValue <- " -predpc"
            }

            cat(predValue,
                file = argsFilename,
                sep = "\n",
                append = appendFlag)

            if (predCorrection == "additive") {
              cat(
                sprintf(" -predpcadd "),
                file = argsFilename,
                sep = "\n",
                append = appendFlag
              )

            }
          }

          # outputPRED is an independent option
          if (vpcOption@outputPRED) {
            cat(
              sprintf(" -pcpredoutput "),
              file = argsFilename,
              sep = "\n",
              append = appendFlag
            )
          }

          obsVars <- vpcOption@observationVars

          if (length(obsVars) == 0) {
            stop("Cannot proceed with VPC since no observables were found.")
          }

          predNames <- ""
          predxValues <- ""
          binningValues <- ""
          ycatValues <- ""
          BQLasLLOQValues <- ""
          pstratValues <-
            paste(vpcOption@stratifyColumns, collapse = " ")

          quantilesValues <- ""
          quantilesSecondaryValues <- ""

          for (obs in obsVars) {
            sep = ";"

            # -predname
            predNames <- paste0(predNames, obs@name, sep)

            # /predx
            xaxis <- obs@xaxis
            predxValues <- paste0(predxValues, xaxis, sep)

            # /predbin
            binningMethod <- obs@binningMethod
            binningOption <- obs@binningOption

            if (binningMethod == "none") {
              binningValues <- paste0(binningValues, "prednobin", sep)
            } else if (binningMethod == "kmeans") {
              binningValues <- paste0(binningValues, "predkmeans", sep)
            } else if (binningMethod == "centers") {
              binningValues <- paste0(binningValues,
                                      "predcenters ",
                                      paste(binningOption, collapse = ","),
                                      sep)
            } else if (binningMethod == "boundaries") {
              binningValues <- paste0(
                binningValues,
                "predboundaries ",
                paste(binningOption, collapse = ","),
                sep
              )
            }

            # /ycat
            if (length(obs@ygroup) == 0) {
              ycatValues <- paste0(ycatValues, " ", sep)
            } else {
              ycatValues <- paste0(ycatValues, obs@ygroup, sep)
            }

            # /bql
            BQLasLLOQValues <- paste0(BQLasLLOQValues,
                                      as.numeric(obs@BQLasLLOQ), sep)

            # /pstrat
            # there are some overriden stratas
            if (length(obs@stratifyColumns) > 0 &&
                all(obs@stratifyColumns != "")) {
              pstratValues <- paste0(
                pstratValues,
                " ",
                obs@name,
                ":[ ",
                paste(obs@stratifyColumns, collapse = " "),
                "]"
              )
            }

            # /pcpi
            if (length(obs@quantilesValues) != 0) {
              qv <- obs@quantilesValues
            } else {
              qv = "-"
            }

            quantilesValues <- paste0(quantilesValues,
                                      paste0(qv, collapse = ","), sep)

            # /pcpe
            if (length(obs@quantilesSecondaryValues) != 0) {
              sv <- obs@quantilesSecondaryValues
            } else {
              sv = "-"
            }

            quantilesSecondaryValues <-
              paste0(quantilesSecondaryValues,
                     paste0(sv, collapse = ","),
                     sep)

          }

          cat(
            paste(" -predname", predNames),
            file = argsFilename,
            sep = "\n",
            append = appendFlag
          )

          if (predxValues != "") {
            cat(
              paste(" -predx", predxValues),
              file = argsFilename,
              sep = "\n",
              append = appendFlag
            )
          }

          if (binningValues != "") {
            cat(
              sprintf(" -predbin \"%s\" ", binningValues),
              file = argsFilename,
              sep = "\n",
              append = appendFlag
            )
          }

          if (ycatValues != "") {
            cat(
              sprintf(" -ycat \"%s\" ", ycatValues),
              file = argsFilename,
              sep = "\n",
              append = appendFlag
            )
          }

          if (BQLasLLOQValues != "") {
            cat(
              sprintf(" -bql %s ", BQLasLLOQValues),
              file = argsFilename,
              sep = "\n",
              append = appendFlag
            )
          }

          if (quantilesValues != "") {
            cat(
              sprintf(" -pcpi \"%s\"  ", quantilesValues),
              file = argsFilename,
              sep = "\n",
              append = appendFlag
            )
          }

          if (quantilesSecondaryValues != "") {
            cat(
              sprintf(" -pcpe \"%s\"  ", quantilesSecondaryValues),
              file = argsFilename,
              sep = "\n",
              append = appendFlag
            )
          }

          if (pstratValues != "\"\"") {
            cat(
              sprintf(" -pstrat [ %s]", pstratValues),
              file = argsFilename,
              sep = "\n",
              append = TRUE
            )
          }
        } else  if (!is.null(simOption)) {
          if (params@isPopulation) {
            cat(
              sprintf(" -predn %d  ", simOption@numReplicates),
              file = argsFilename,
              sep = "\n",
              append = appendFlag
            )

            appendFlag <- TRUE
            cat(
              sprintf(" -predout %s  ", dataset@predoutFilename),
              file = argsFilename,
              sep = "\n",
              append = appendFlag
            )

            cat(
              sprintf(" -pcseed %d  ", simOption@seed),
              file = argsFilename,
              sep = "\n",
              append = appendFlag
            )
          } else {
            if (simOption@yVariables == "") {
              warning("No Y variables are given for individual simulation output",
                      call. = FALSE)
            }

            cat(
              sprintf(" -simn %d  ", simOption@numPoints),
              file = argsFilename,
              sep = "\n",
              append = appendFlag
            )

            cat(
              sprintf(" -simmax %d  ", simOption@maxXRange),
              file = argsFilename,
              sep = "\n",
              append = appendFlag
            )

            cat(
              sprintf(" -simvary \"%s\"  ", simOption@yVariables),
              file = argsFilename,
              sep = "\n",
              append = appendFlag
            )

            cat(
              sprintf(" -simout %s", dataset@simoutFilename),
              file = argsFilename,
              sep = "\n",
              append = appendFlag
            )

            if (simOption@simAtObs == TRUE) {
              cat(
                sprintf(" -simobs "),
                file = argsFilename,
                sep = "\n",
                append = appendFlag
              )

            }
          }
        }

        # /o due to counting the number of models in NLME8
        cat(
          sprintf(
            "-m %d -n %d /o %d %s",
            params@method,
            params@numIterations,
            params@odeToUse,
            params@scenarios
          ),
          file = argsFilename,
          sep = "\n",
          append = appendFlag
        )

        appendFlag <- TRUE

        if (bootStratify != "") {
          tokens <- unlist(strsplit(bootStratify, ","))
          for (i in 1:length(tokens)) {
            cat(
              sprintf("-bstrat%0d \"%s\"", i, tokens[[i]]),
              file = argsFilename,
              sep = "\n",
              append = TRUE
            )
          }
        }

        cat(
          sprintf(
            "-xnp %d -anagrad %d -logtran %d -xrestart %d -xnorderagq %d -xfocehess %d  -xstderr %d -rtol %f -atol %f ",
            params@xnp,
            params@anagrad,
            params@logtran,
            params@xrestart,
            params@xnorderagq,
            params@xfocehess,
            params@xstderr,
            params@rtol,
            params@atol
          ),
          file = argsFilename,
          sep = "\n",
          append = TRUE
        )

        if (params@isPopulation == FALSE) {
          cat(
            sprintf(
              "-pardern %d -parderd %f",
              params@pardern,
              params@parderd
            ),
            file = argsFilename,
            sep = "\n",
            append = TRUE
          )
        }

        cat(
          sprintf(" -xlameth %d  ", params@xlameth),
          file = argsFilename,
          sep = "\n",
          append = TRUE
        )

        cat(
          sprintf(" -xlandig %d ", params@xlandig),
          file = argsFilename,
          sep = "\n",
          append = TRUE
        )

        cat(
          sprintf(" -xlatol %f ", params@xlatol),
          file = argsFilename,
          sep = "\n",
          append = TRUE
        )

        cat(
          sprintf(" -xblmeth %d ", params@xblmeth),
          file = argsFilename,
          sep = "\n",
          append = TRUE
        )

        cat(
          sprintf(" -xblndig %d ", params@xblndig),
          file = argsFilename,
          sep = "\n",
          append = TRUE
        )

        cat(
          sprintf(" -xbltol %f ", params@xbltol),
          file = argsFilename,
          sep = "\n",
          append = TRUE
        )

        if (params@method != 6) {
          cat(
            sprintf(" -xmapnp %d ", params@xmapnp),
            file = argsFilename,
            sep = "\n",
            append = TRUE
          )
        }

        if (params@isPCWRES == 1) {
          cat(
            sprintf(" -xpcwresnrep %d",
                    params@xpcwresnrep),
            file = argsFilename,
            sep = "\n",
            append = TRUE
          )
        }

        if (params@isQRPEMStyleMethod == 1) {
          cat(
            sprintf(
              "-xisample %d -xmapassist %d -ximpsampdof %d -xmcpem %d -xpemrunall %d -xsirsamp %d -xburnin %d -xnonomegaburn %d -xstartfromsavedposteriors %d -xaccratio %f -xscramble %d",
              params@xisample,
              params@xmapassist,
              params@ximpsampdof,
              params@xmcpem,
              params@xpemrunall,
              params@xsirsamp,
              params@xburnin,
              params@xnonomegaburn,
              params@xstartfromsavedposteriors,
              params@xaccratio,
              params@xscramble
            ),
            file = argsFilename,
            sep = "\n",
            append = TRUE
          )
        }


        cat(
          sprintf(
            "-nmxstep %d %s %s %s %s %s",
            params@nmxstep,
            params@sort,
            params@csv,
            params@sand,
            params@fisher,
            params@autodetect
          ),
          file = argsFilename,
          sep = "\n",
          append = TRUE
        )

      }

      cat(
        paste("-d1", dataset@colDefFile, dataset@dataFile),
        file = argsFilename,
        sep = "\n",
        append = TRUE
      )

      doseDef <- dataset@doseDefFile
      if (doseDef != "") {
        cat(
          paste("-d2", doseDef, dataset@doseDataFile),
          file = argsFilename,
          sep = "\n",
          append = TRUE
        )

      }

      estDef <- dataset@estimatesDefFile
      if (estDef != "") {
        cat(
          paste("-d3", estDef, dataset@estimatesDataFile),
          file = argsFilename,
          sep = "\n",
          append = TRUE
        )

      }

      ranEffDef <- dataset@ranEffectDefFile
      if (ranEffDef != "") {
        cat(
          paste("-d4", ranEffDef, dataset@ranEffectDataFile),
          file = argsFilename,
          sep = "\n",
          append = TRUE
        )

      }

      if (numTodo == 0) {
        done <- TRUE
      } else {
        current <- current + 1
        if (current > numTodo) {
          done <- TRUE
        }
      }
    }
  }

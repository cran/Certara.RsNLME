map_dosepoints <-
  function(.Object,
           OldMapping,
           dosepoints,
           dosepoints2,
           dosepointN) {
    mDoseCol <- c()
    currentDosepoints <-
      if (dosepointN == 1) {
        dosepoints
      } else {
        dosepoints2
      }
    for (dosepoint in unique(currentDosepoints)) {
      columnName <- "?"
      Infusion <- NA
      # find out if dosepoint was presented before as a simple dosepoint
      # or dosepoint1 and dosepoint2
      if (dosepoint %in% .Object@dosePoints) {
        oldDosepoint <- dosepoint

        # special case: a simple dose was mapped then dosepoint2 is added
        if (dosepointN == 2 && dosepoint %in% dosepoints) {
          # already done above:
          # columnName <- "?"
          # Infusion <- NA
        } else {
          if (!is.null(OldMapping[[oldDosepoint]]@variableType$Infusion)) {
            Infusion <- OldMapping[[oldDosepoint]]@variableType$Infusion
          }

          columnName <- OldMapping[[oldDosepoint]]@columnName
        }
      } else if (dosepointN == 1 &&
                 (paste0(dosepoint, "_1") %in% .Object@dosePoints) &&
                 is.logical(OldMapping[[paste0(dosepoint, "_1")]]@variableType$DosepointDouble) &&
                 OldMapping[[paste0(dosepoint, "_1")]]@variableType$DosepointDouble) {
        oldDosepoint <- paste0(dosepoint, "_1")
        if (!is.null(OldMapping[[oldDosepoint]]@variableType$Infusion)) {
          Infusion <- OldMapping[[oldDosepoint]]@variableType$Infusion
        }

        columnName <- OldMapping[[oldDosepoint]]@columnName
      } else if (dosepointN == 2 &&
                 (paste0(dosepoint, "_2") %in% .Object@dosePoints) &&
                 is.logical(OldMapping[[paste0(dosepoint, "_2")]]@variableType$DosepointDouble) &&
                 OldMapping[[paste0(dosepoint, "_2")]]@variableType$DosepointDouble) {
        oldDosepoint <- paste0(dosepoint, "_2")
        if (!is.null(OldMapping[[oldDosepoint]]@variableType$Infusion)) {
          Infusion <- OldMapping[[oldDosepoint]]@variableType$Infusion
        }

        columnName <- OldMapping[[oldDosepoint]]@columnName
      }

      if (dosepointN == 1 &&
          dosepoint %in% dosepoints2) {
        variableName <- paste0(dosepoint, "_1")
        DosepointDouble <- TRUE
      } else if (dosepointN == 2 &&
                 dosepoint %in% dosepoints) {
        variableName <- paste0(dosepoint, "_2")
        DosepointDouble <- TRUE
      } else {
        variableName <- dosepoint
        DosepointDouble <- FALSE
      }

      # will need to keep properties
      mDoseCol <-
        c(
          mDoseCol,
          NlmeColumnMap(
            variableName = variableName,
            columnName = columnName,
            variableType = list(
              type = "dosepoint",
              Infusion = Infusion,
              DosepointN = dosepointN,
              DosepointDouble = DosepointDouble
            )
          )
        )

      # if there's infusion specified, need to keep it
      if (!is.na(Infusion)) {
        InfVariableName <- paste0(variableName, "_", Infusion)
        if (InfVariableName %in% names(OldMapping)) {
          InfcolumnName <- OldMapping[[InfVariableName]]@columnName
        } else if (paste0(dosepoint, "_", Infusion) %in% names(OldMapping)) {
          InfcolumnName <-
            OldMapping[[paste0(dosepoint, "_", Infusion)]]@columnName
        } else {
          InfcolumnName <- "?"
        }

        mDoseCol <-
          c(
            mDoseCol,
            NlmeColumnMap(
              variableName = InfVariableName,
              columnName = InfcolumnName,
              variableType = list(
                type = "dosepointInf",
                Infusion = Infusion,
                DosepointN = dosepointN,
                DosepointDouble = DosepointDouble
              )
            )
          )
      }
    }
    mDoseCol
  }

# Test extra dosing information correctly generates in cols1.txt

test_that("extra dose lines for steady state case with infusion are correct for numeric",
          {
            dt_SimInputData_SS = data.frame(
              ID = 1,
              Time = 0,
              Dose = 10,
              SS = 1,
              II = 12,
              rate = 5,
              duration = 2,
              SS2 = 1,
              CObs = "."
            )

            model <- pkmodel(
              absorption = "Extravascular",
              data = dt_SimInputData_SS,
              ID = "ID",
              Time = "Time",
              Aa = "Dose",
              CObs = "CObs"
            )

            model <-
              addDoseCycle(
                model,
                type = "SteadyState",
                name = "Aa",
                administration =  "Infusion",
                amount = 10,
                II = 12,
                rate = 5,
                colName = "SS"
              )

            doseLines <- extraDoseLines(model)

            testthat::expect_that(doseLines, testthat::equals("ss(\"SS\", 10 5 inf(Aa) 12 dt)"))
          })

test_that("extra dose lines for steady state case with infusion are correct for column map",
          {
            dt_SimInputData_SS = data.frame(
              ID = 1,
              Time = 0,
              Dose = 10,
              SS = 1,
              II = 12,
              rate = 5,
              duration = 2,
              SS2 = 1,
              CObs = "."
            )

            model <- pkmodel(
              absorption = "Extravascular",
              data = dt_SimInputData_SS,
              ID = "ID",
              Time = "Time",
              Aa = "Dose",
              CObs = "CObs"
            )

            model <-
              addDoseCycle(
                model,
                type = "SteadyState",
                name = "Aa",
                administration =  "Bolus",
                amount = "Dose",
                II = "II",
                colName = "SS"
              )

            doseLines <- extraDoseLines(model)

            testthat::expect_that(doseLines,
                                  testthat::equals("ss(\"SS\", \"Dose\" bolus(Aa) \"II\" dt )"))
          })


test_that("extra dose lines for ADDL case with infusion are correct for numeric",
          {
            dt_SimInputData_ADDL = data.frame(
              ID = 1,
              Time = 0,
              Dose = 10,
              ADDL = 3,
              II = 12,
              rate = 5,
              duration = 2,
              ADDL2 = 2,
              CObs = "."
            )

            model <- pkmodel(
              absorption = "Extravascular",
              data = dt_SimInputData_ADDL,
              ID = "ID",
              Time = "Time",
              Aa = "Dose",
              CObs = "CObs"
            )

            model <-
              addDoseCycle(
                model,
                type = "ADDL",
                name = "Aa",
                administration = "Infusion",
                amount = 10,
                II = 12,
                duration = 2,
                colName = "ADDL"
              )

            doseLines <- extraDoseLines(model)

            testthat::expect_that(doseLines,
                                  testthat::equals("addl(\"ADDL\", 12 dt 10 dup / 2 inf(Aa) )"))
          })

test_that("extra dose lines for ADDL case with infusion are correct for column map",
          {
            dt_SimInputData_ADDL = data.frame(
              ID = 1,
              Time = 0,
              Dose = 10,
              ADDL = 3,
              II = 12,
              rate = 5,
              duration = 2,
              ADDL2 = 2,
              CObs = "."
            )

            model <- pkmodel(
              absorption = "Extravascular",
              data = dt_SimInputData_ADDL,
              ID = "ID",
              Time = "Time",
              Aa = "Dose",
              CObs = "CObs"
            )

            model <-
              addDoseCycle(
                model,
                type = "ADDL",
                name = "Aa",
                administration = "Infusion",
                amount = "Dose",
                II = "II",
                duration = "duration",
                colName = "ADDL"
              )

            doseLines <- extraDoseLines(model)

            testthat::expect_that(
              doseLines,
              testthat::equals("addl(\"ADDL\", \"II\" dt \"Dose\" dup / \"duration\" inf(Aa) )")
            )
          })

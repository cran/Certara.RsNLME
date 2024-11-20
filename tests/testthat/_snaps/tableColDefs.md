# test for NlmeTableDef and addTablesToColumnMapping

    Cannot use special variables IRES, Weight, IWRES 
    without Observable variables specified in 'When observe'

---

    Code
      tableParams(name = "Table1.csv", whenObs = c("CObs"), variablesList = "C",
      IRES = TRUE, IWRES = TRUE, Weight = TRUE, keepSource = TRUE)
    Output
      Name of the output table file                               :Table1.csv
      Names of observed variables triggering simulation point     :CObs
      List of variables for output                                :C
      Keep the number of rows the same as in the input dataset    :TRUE
      Output time after dose                                      :FALSE
      Output individual residuals                                 :TRUE
      Output the weight of current observation                    :TRUE
      Output individual weighted residuals                        :TRUE
      The mode of output for non time-based models                :all

---

    Code
      addTablesToColumnMapping(model = model, Tables = Table1, filename = tablecoldef)
    Output
      Name of the output table file                               :Table1.csv
      Time values for output                                      :seq(0, 24, 2)
      Names of observed variables triggering simulation point     :CObs
      List of variables for output                                :C
      Keep the number of rows the same as in the input dataset    :FALSE
      Output time after dose                                      :FALSE
      Output individual residuals                                 :TRUE
      Output the weight of current observation                    :TRUE
      Output individual weighted residuals                        :TRUE
      The mode of output for non time-based models                :all

---

    Code
      addTablesToColumnMapping(model = model, Tables = TableSim, filename = simtablecoldef,
        forSim = TRUE)
    Output
      Name of the output table file                               :simTable1.csv
      Names of dosing compartments triggering simulation point    :Aa
      Names of observed variables triggering simulation point     :CObs
      List of variables for output                                :C
      Keep the number of rows the same as in the input dataset    :TRUE
      Output time after dose                                      :FALSE


testthat::local_edition(3)

# --- Setup Code ---
# Create the dummy dataset
numSub <- 10
dt_dummyData <- data.frame(
  ID = rep(seq(1, numSub, 1), each = 6),
  Time = rep(c(0, 12, 24, 36, 48, 60), numSub),
  Sex = rep(rep(c(0, 1), numSub / 2), each = 6),
  BW = rep(seq(70, 88, 2), each = 6),
  OCC1 = rep(c(0, 0, 0, 1, 1, 1), numSub),
  OCC2 = rep(c(1, 2, 1, 2, 1, 2), numSub),
  OCC3 = rep(c(1, 2, 3, 1, 2, 3), numSub)
)

# Helper function to get model statements string
get_model_statements <- function(model) {
  paste(model@statements, collapse = "\n")
}

# Helper function to extract diagonal ranef values (simple version)
extract_diag_ranef_values <- function(statements, ranef_prefix) {
  ranef_lines <-
    statements[grep(paste0("ranef.*diag.*", ranef_prefix), statements)]
  if (length(ranef_lines) == 0)
    return(NULL)
  val_str <-
    sub(paste0(".*", ranef_prefix, "[^=]+= c\\("), "", ranef_lines[1])
  val_str <- sub("\\).*", "", val_str)
  val_str <- gsub("\\s", "", val_str)
  val_str_parts <- strsplit(val_str, ",same")[[1]]
  numeric_vals <- tryCatch(
    as.numeric(strsplit(val_str_parts[1], ",")[[1]]),
    warning = function(w) {
      numeric()
    }
  )
  return(numeric_vals)
}
# --- End Setup ---

test_that("removeCovariate updates block ranef values correctly for single parameter removal",
          {
            model <-
              pkmodel(numCompartments = 3,
                      data = dt_dummyData,
                      columnMap = FALSE)
            # Add a 4x4 block occasion covariate effect
            model <- addCovariate(
              model,
              covariate = "OCC1",
              type = "Occasion",
              levels = c(1, 2),
              # Use levels 1,2
              effect = c("V", "Cl", "V2", "Cl2"),
              isDiagonal = FALSE,
              values = c(0.1,                            # V
                         0.021, 0.2,                     # Cl
                         0.031, 0.032, 0.3,              # V2
                         0.041, 0.042, 0.043, 0.4)       # Cl2
            )

            # Remove V2 (parameter 3) - Expect V, Cl, Cl2 block
            model_remove_v2 <-
              removeCovariate(model, covariate = "OCC1", paramName = "V2")
            statements_remove_v2 <- get_model_statements(model_remove_v2)
            expect_true(grepl(
              "block\\(nVxOCC11,nClxOCC11,nCl2xOCC11\\)",
              statements_remove_v2
            ),
            info = "Ranef params should be V, Cl, Cl2 after removing V2")
            expect_false(grepl("nV2xOCC1", statements_remove_v2))
            # Expected values: V(1,1)=0.1, Cl(2,1)=0.021, Cl(2,2)=0.2, Cl2(4,1)=0.041, Cl2(4,2)=0.042, Cl2(4,4)=0.4
            expect_snapshot_value(statements_remove_v2, cran = FALSE, variant = "block_remove_v2")

            # Remove Cl2 (parameter 4) from original model - Expect V, Cl, V2 block
            model_remove_cl2 <-
              removeCovariate(model, covariate = "OCC1", paramName = "Cl2")
            statements_remove_cl2 <- get_model_statements(model_remove_cl2)
            expect_true(grepl(
              "block\\(nVxOCC11,nClxOCC11,nV2xOCC11\\)",
              statements_remove_cl2
            ),
            info = "Ranef params should be V, Cl, V2 after removing Cl2")
            expect_false(grepl("nCl2xOCC1", statements_remove_cl2))
            # Expected values: V(1,1)=0.1, Cl(2,1)=0.021, Cl(2,2)=0.2, V2(3,1)=0.031, V2(3,2)=0.032, V2(3,3)=0.3
            expect_snapshot_value(statements_remove_cl2, cran = FALSE, variant = "block_remove_cl2")
          })


test_that("removeCovariate updates diagonal ranef values correctly for multi-param removal",
          {
            model <-
              pkmodel(numCompartments = 3,
                      data = dt_dummyData,
                      columnMap = FALSE)
            params_affected <- c("V", "Cl", "V2", "Cl2", "V3", "Cl3")
            initial_values <- c(0.11, 0.12, 0.13, 0.14, 0.15, 0.16)
            model <- addCovariate(
              model,
              covariate = "OCC1",
              type = "Occasion",
              levels = c(0, 1),
              effect = params_affected,
              isDiagonal = TRUE,
              values = initial_values
            )

            # Remove c("V2", "V3") (params 3 and 5) -> Keep V, Cl, Cl2, Cl3 (0.11, 0.12, 0.14, 0.16)
            model_removed_v2_v3 <-
              removeCovariate(model,
                              covariate = "OCC1",
                              paramName = c("V2", "V3"))
            statements_removed_v2_v3 <-
              get_model_statements(model_removed_v2_v3)
            expect_true(
              grepl(
                "diag\\(nVxOCC10,nClxOCC10,nCl2xOCC10,nCl3xOCC10\\)",
                statements_removed_v2_v3
              )
            )
            expect_false(grepl("nV2xOCC1|nV3xOCC1", statements_removed_v2_v3))
            vals_v2_v3 <-
              extract_diag_ranef_values(strsplit(statements_removed_v2_v3, "\n")[[1]], "nVxOCC10")
            expect_equal(vals_v2_v3, c(0.11, 0.12, 0.14, 0.16))
            expect_snapshot_value(statements_removed_v2_v3,
                                  cran = FALSE,
                                  variant = "diag_remove_v2_v3")

            # Remove c("V3", "Cl3") (params 5 and 6) -> Keep V, Cl, V2, Cl2 (0.11, 0.12, 0.13, 0.14)
            model_removed_v3_cl3 <-
              removeCovariate(model,
                              covariate = "OCC1",
                              paramName = c("V3", "Cl3"))
            statements_removed_v3_cl3 <-
              get_model_statements(model_removed_v3_cl3)
            expect_true(
              grepl(
                "diag\\(nVxOCC10,nClxOCC10,nV2xOCC10,nCl2xOCC10\\)",
                statements_removed_v3_cl3
              )
            )
            expect_false(grepl("nV3xOCC1|nCl3xOCC1", statements_removed_v3_cl3))
            vals_v3_cl3 <-
              extract_diag_ranef_values(strsplit(statements_removed_v3_cl3, "\n")[[1]], "nVxOCC10")
            expect_equal(vals_v3_cl3, c(0.11, 0.12, 0.13, 0.14))
            expect_snapshot_value(statements_removed_v3_cl3,
                                  cran = FALSE,
                                  variant = "diag_remove_v3_cl3")

            # Remove c("V2", "Cl2", "V3", "Cl3") (params 3, 4, 5, 6) -> Keep V, Cl (0.11, 0.12)
            model_removed_4 <-
              removeCovariate(model,
                              covariate = "OCC1",
                              paramName = c("V2", "Cl2", "V3", "Cl3"))
            statements_removed_4 <- get_model_statements(model_removed_4)
            expect_true(grepl("diag\\(nVxOCC10,nClxOCC10\\)", statements_removed_4))
            expect_false(grepl(
              "nV2xOCC1|nCl2xOCC1|nV3xOCC1|nCl3xOCC1",
              statements_removed_4
            ))
            vals_4 <-
              extract_diag_ranef_values(strsplit(statements_removed_4, "\n")[[1]], "nVxOCC10")
            expect_equal(vals_4, c(0.11, 0.12))
            expect_snapshot_value(statements_removed_4, cran = FALSE, variant = "diag_remove_4")
          })


test_that(
  "removeCovariate updates block ranef structure/values correctly for multi-param removal",
  {
    model <-
      pkmodel(numCompartments = 3,
              data = dt_dummyData,
              columnMap = FALSE)
    params_affected <- c("V", "Cl", "V2", "Cl2", "V3", "Cl3")
    initial_values <- c(
      0.1,
      0.021,
      0.2,
      0.031,
      0.032,
      0.3,
      0.041,
      0.042,
      0.043,
      0.4,
      0.051,
      0.052,
      0.053,
      0.054,
      0.5,
      0.061,
      0.062,
      0.063,
      0.064,
      0.065,
      0.6
    )
    model <- addCovariate(
      model,
      covariate = "OCC1",
      type = "Occasion",
      levels = c(1, 2),
      # Use levels 1,2
      effect = params_affected,
      isDiagonal = FALSE,
      values = initial_values
    )

    # Remove c("V2", "V3") (params 3 and 5) -> Keep V, Cl, Cl2, Cl3 (Indices 1, 2, 4, 6)
    model_removed_v2_v3 <-
      removeCovariate(model,
                      covariate = "OCC1",
                      paramName = c("V2", "V3"))
    statements_removed_v2_v3 <-
      get_model_statements(model_removed_v2_v3)
    expect_true(
      grepl(
        "block\\(nVxOCC11,nClxOCC11,nCl2xOCC11,nCl3xOCC11\\)",
        statements_removed_v2_v3
      )
    )
    expect_false(grepl("nV2xOCC1|nV3xOCC1", statements_removed_v2_v3))
    # Expected values: 0.1, 0.021, 0.2, 0.041, 0.042, 0.4, 0.061, 0.062, 0.064, 0.6
    expect_snapshot_value(statements_removed_v2_v3,
                          cran = FALSE,
                          variant = "block_remove_v2_v3")

    # Remove c("V2", "Cl2", "V3", "Cl3") (params 3, 4, 5, 6) -> Keep V, Cl (Indices 1, 2)
    model_removed_4 <-
      removeCovariate(model,
                      covariate = "OCC1",
                      paramName = c("V2", "Cl2", "V3", "Cl3"))
    statements_removed_4 <- get_model_statements(model_removed_4)
    expect_true(grepl("block\\(nVxOCC11,nClxOCC11\\)", statements_removed_4))
    expect_false(grepl(
      "nV2xOCC1|nCl2xOCC1|nV3xOCC1|nCl3xOCC1",
      statements_removed_4
    ))
    # Expected values: 0.1, 0.021, 0.2
    expect_snapshot_value(statements_removed_4, cran = FALSE, variant = "block_remove_4")
  }
)


# --- Tests Covering Other Logic/Syntax Issues ---

test_that("addCovariate option='No' removes diagonal occasion ranef statement",
          {
            model <-
              pkmodel(numCompartments = 3,
                      data = dt_dummyData,
                      columnMap = FALSE)
            model <- addCovariate(
              model,
              covariate = "OCC1",
              type = "Occasion",
              levels = c(0, 1),
              effect = c("V", "Cl"),
              isDiagonal = TRUE,
              values = c(0.1, 0.2)
            )
            model_after_remove <-
              addCovariate(
                model,
                covariate = "OCC1",
                effect = c("V", "Cl"),
                option = "No"
              )
            statements_after <- get_model_statements(model_after_remove)

            expect_false(grepl("nVxOCC1|nClxOCC1", statements_after))
            expect_false(grepl("ranef.*diag.*nVxOCC10", statements_after))
            expect_snapshot_value(statements_after, cran = FALSE, variant = "option_no_diag")
          })

test_that("addCovariate option='No' removes block occasion ranef statement",
          {
            model <-
              pkmodel(numCompartments = 3,
                      data = dt_dummyData,
                      columnMap = FALSE)
            model <- addCovariate(
              model,
              covariate = "OCC1",
              type = "Occasion",
              levels = c(0, 1),
              effect = c("V", "Cl"),
              isDiagonal = FALSE,
              values = c(0.1, 0.021, 0.2)
            )
            model_after_remove <-
              addCovariate(
                model,
                covariate = "OCC1",
                effect = c("V", "Cl"),
                option = "No"
              )
            statements_after <- get_model_statements(model_after_remove)

            expect_false(grepl("nVxOCC1|nClxOCC1", statements_after))
            expect_false(grepl("ranef.*block.*nVxOCC10", statements_after))
            expect_snapshot_value(statements_after, cran = FALSE, variant = "option_no_block")
          })

test_that("No extra '+' when adding OccCov to no-ranef param after another OccCov exists",
          {
            model <-
              pkmodel(numCompartments = 3,
                      data = dt_dummyData,
                      columnMap = FALSE)
            model <-
              structuralParameter(
                model,
                paramName = "V",
                hasRandomEffect = FALSE,
                style = "Normal"
              )
            model <-
              structuralParameter(
                model,
                paramName = "V2",
                hasRandomEffect = FALSE,
                style = "LogNormal1"
              )
            model <-
              structuralParameter(
                model,
                paramName = "V3",
                hasRandomEffect = FALSE,
                style = "LogNormal2"
              )
            # Add OCC1 to a param WITH ranef first
            model_with_occ1 <-
              addCovariate(
                model,
                covariate = "OCC1",
                type = "Occasion",
                levels = c(0, 1),
                effect = "Cl",
                values = 0.1
              )
            # Add OCC2 to params WITHOUT ranef
            model_with_occ2 <-
              addCovariate(
                model_with_occ1,
                covariate = "OCC2",
                type = "Occasion",
                levels = c(1, 2),
                effect = c("V", "V2", "V3"),
                isDiagonal = TRUE,
                values = c(0.21, 0.22, 0.23)
              )
            statements <- get_model_statements(model_with_occ2)

            # Check for absence of double plus or plus at start of exp()
            expect_false(grepl("stparm\\(V = tvV \\+ \\+", statements))
            expect_false(grepl("stparm\\(V2 = tvV2 \\* exp\\(\\+", statements))
            expect_false(grepl("stparm\\(V3 = exp\\(tvV3 \\+ \\+", statements))
            # Check that the OCC2 effect *is* present correctly
            expect_true(grepl("stparm\\(V = tvV \\+ nVxOCC2", statements))
            expect_true(grepl("stparm\\(V2 = \\(tvV2\\) \\* exp\\(nV2xOCC2", statements))
            expect_true(grepl("stparm\\(V3 = exp\\(tvV3 \\+ nV3xOCC2", statements))
            expect_snapshot_value(statements, cran = FALSE, variant = "no_extra_plus_on_no_ranef")
          })

test_that("No orphaned '+' when removing one OccCov from a no-ranef param with multiple OccCovs",
          {
            # Normal style param V
            model_norm <-
              pkmodel(numCompartments = 3,
                      data = dt_dummyData,
                      columnMap = FALSE)
            model_norm <-
              structuralParameter(
                model_norm,
                paramName = "V",
                hasRandomEffect = FALSE,
                style = "Normal"
              )
            model_norm <-
              addCovariate(
                model_norm,
                covariate = "OCC1",
                type = "Occasion",
                levels = c(0, 1),
                effect = "V",
                values = 0.11
              )
            model_norm_occ12 <-
              addCovariate(
                model_norm,
                covariate = "OCC2",
                type = "Occasion",
                levels = c(1, 2),
                effect = "V",
                values = 0.21
              )

            # Remove OCC1 -> Should leave 'tvV + nVxOCC2...'
            model_remove_occ1_norm <-
              removeCovariate(model_norm_occ12,
                              covariate = "OCC1",
                              paramName = "V")
            statements_remove_occ1_norm <-
              get_model_statements(model_remove_occ1_norm)
            expect_false(grepl("stparm\\(V = tvV \\+ \\+", statements_remove_occ1_norm)) # No double plus
            expect_true(grepl("stparm\\(V = tvV \\+ nVxOCC2", statements_remove_occ1_norm)) # OCC2 remains
            expect_snapshot_value(statements_remove_occ1_norm,
                                  cran = FALSE,
                                  variant = "no_orphan_plus_remove_occ1")

            # Remove OCC2 -> Should leave 'tvV + nVxOCC1...' (no trailing plus)
            model_remove_occ2_norm <-
              removeCovariate(model_norm_occ12,
                              covariate = "OCC2",
                              paramName = "V")
            statements_remove_occ2_norm <-
              get_model_statements(model_remove_occ2_norm)
            stparm_v_line <-
              trimws(statements_remove_occ2_norm[grep("stparm\\(V =", statements_remove_occ2_norm)])
            expect_false(grepl("\\) \\+$", stparm_v_line)) # No trailing plus
            expect_true(grepl("nVxOCC11\\*\\(OCC1==1\\)\\)", stparm_v_line)) # Ends correctly
            expect_snapshot_value(statements_remove_occ2_norm,
                                  cran = FALSE,
                                  variant = "no_orphan_plus_remove_occ2")

            # Remove OCC1 from V & V3 (no-ranef), V2 keeps OCC1+OCC2
            model_ex42 <-
              pkmodel(numCompartments = 3,
                      data = dt_dummyData,
                      columnMap = FALSE)
            model_ex42 <-
              structuralParameter(
                model_ex42,
                paramName = "V",
                hasRandomEffect = FALSE,
                style = "Normal"
              )
            model_ex42 <-
              structuralParameter(
                model_ex42,
                paramName = "V2",
                hasRandomEffect = FALSE,
                style = "LogNormal2"
              )
            model_ex42 <-
              structuralParameter(
                model_ex42,
                paramName = "V3",
                hasRandomEffect = FALSE,
                style = "Normal"
              )
            model_ex42 <-
              addCovariate(
                model_ex42,
                covariate = "OCC1",
                type = "Occasion",
                levels = c(0, 1),
                effect = c("V", "V2", "V3"),
                isDiagonal = TRUE,
                values = c(0.11, 0.12, 0.13)
              )
            model_ex42 <-
              addCovariate(
                model_ex42,
                covariate = "OCC2",
                type = "Occasion",
                levels = c(1, 2),
                effect = c("V", "V2", "V3"),
                isDiagonal = TRUE,
                values = c(0.21, 0.22, 0.23)
              )
            model_ex42_removed <-
              removeCovariate(model_ex42,
                              covariate = "OCC1",
                              paramName = c("V", "V3"))
            statements_ex42_removed <-
              get_model_statements(model_ex42_removed)
            expect_false(grepl("stparm\\(V = tvV \\+ \\+", statements_ex42_removed)) # Check V syntax
            expect_false(grepl("stparm\\(V3 = tvV3 \\+ \\+", statements_ex42_removed)) # Check V3 syntax
            expect_true(
              grepl(
                "stparm\\(V2 = exp\\(tvV2 \\+ nV2xOCC10.*\\+ nV2xOCC21",
                statements_ex42_removed
              )
            ) # V2 unchanged
            expect_snapshot_value(statements_ex42_removed,
                                  cran = FALSE,
                                  variant = "no_orphan_plus_multi_param")
          })

test_that("removeCovariate works correctly for multiple consecutive no-ranef params",
          {
            model <-
              pkmodel(numCompartments = 3,
                      data = dt_dummyData,
                      columnMap = FALSE)
            model <-
              structuralParameter(
                model,
                paramName = "V",
                hasRandomEffect = FALSE,
                style = "Normal"
              )
            model <-
              structuralParameter(
                model,
                paramName = "V2",
                hasRandomEffect = FALSE,
                style = "LogNormal1"
              )
            model <-
              structuralParameter(
                model,
                paramName = "V3",
                hasRandomEffect = FALSE,
                style = "LogNormal2"
              )
            model_with_occ1 <-
              addCovariate(
                model,
                covariate = "OCC1",
                type = "Occasion",
                levels = c(0, 1),
                effect = c("V", "V2", "V3", "Cl"),
                isDiagonal = TRUE,
                values = c(0.11, 0.12, 0.13, 0.1)
              )

            # Remove from V and V2 (consecutive no-ranef) -> Both should be removed
            model_removed_v_v2 <-
              removeCovariate(model_with_occ1,
                              covariate = "OCC1",
                              paramName = c("V", "V2"))
            statements_removed_v_v2 <-
              get_model_statements(model_removed_v_v2)
            expect_false(grepl("nVxOCC1", statements_removed_v_v2), info = "V clean")
            expect_false(grepl("nV2xOCC1", statements_removed_v_v2), info = "V2 clean") # Specific check for Issue 5
            expect_true(grepl("nV3xOCC1", statements_removed_v_v2), info = "V3 remains")
            expect_snapshot_value(statements_removed_v_v2,
                                  cran = FALSE,
                                  variant = "remove_consecutive_no_ranef_v_v2")

            # Remove from V2 and V3 (consecutive no-ranef) -> Both should be removed
            model_removed_v2_v3 <-
              removeCovariate(model_with_occ1,
                              covariate = "OCC1",
                              paramName = c("V2", "V3"))
            statements_removed_v2_v3 <-
              get_model_statements(model_removed_v2_v3)
            expect_true(grepl("nVxOCC1", statements_removed_v2_v3), info = "V remains")
            expect_false(grepl("nV2xOCC1", statements_removed_v2_v3), info = "V2 clean")
            expect_false(grepl("nV3xOCC1", statements_removed_v2_v3), info = "V3 clean") # Specific check for Issue 5
            expect_snapshot_value(statements_removed_v2_v3,
                                  cran = FALSE,
                                  variant = "remove_consecutive_no_ranef_v2_v3")
          })

test_that("Adding multiple different Occasion Covariates works correctly",
          {
            model <-
              pkmodel(numCompartments = 3,
                      data = dt_dummyData,
                      columnMap = FALSE)
            model <-
              structuralParameter(
                model,
                paramName = "V",
                style = "LogNormal2",
                hasRandomEffect = FALSE
              )
            model <-
              structuralParameter(
                model,
                paramName = "V2",
                style = "LogNormal1",
                hasRandomEffect = FALSE
              )
            model <-
              structuralParameter(
                model,
                paramName = "V3",
                style = "Normal",
                hasRandomEffect = FALSE
              )

            # Add OCC1
            model <-
              addCovariate(
                model,
                covariate = "OCC1",
                type = "Occasion",
                levels = c(0, 1),
                effect = c("V", "V2", "V3"),
                isDiagonal = TRUE,
                values = c(0.11, 0.12, 0.13)
              )
            # Add OCC2
            model_occ2 <-
              addCovariate(
                model,
                covariate = "OCC2",
                type = "Occasion",
                levels = c(1, 2),
                effect = c("V", "V2", "V3"),
                isDiagonal = TRUE,
                values = c(0.21, 0.22, 0.23)
              )
            statements_after_occ2 <- get_model_statements(model_occ2)
            # Check both OCC1 and OCC2 effects are present
            expect_true(
              grepl("nVxOCC1", statements_after_occ2) &&
                grepl("nVxOCC2", statements_after_occ2)
            )
            expect_true(
              grepl("nV2xOCC1", statements_after_occ2) &&
                grepl("nV2xOCC2", statements_after_occ2)
            )
            expect_true(
              grepl("nV3xOCC1", statements_after_occ2) &&
                grepl("nV3xOCC2", statements_after_occ2)
            )
            expect_snapshot_value(statements_after_occ2, cran = FALSE, variant = "add_multi_occ_no_ranef_occ2")

            # Add OCC3 (block)
            model_occ3 <-
              addCovariate(
                model_occ2,
                covariate = "OCC3",
                type = "Occasion",
                levels = c(1, 2, 3),
                effect = c("V", "V2", "V3"),
                isDiagonal = FALSE,
                values = c(0.31, 0.021, 0.32, 0.031, 0.032, 0.33)
              )
            statements_after_occ3 <- get_model_statements(model_occ3)
            # Check OCC1, OCC2 (diag ranefs) and OCC3 (block ranef) effects are present
            expect_true(grepl("ranef.*diag.*nVxOCC1", statements_after_occ3))
            expect_true(grepl("ranef.*diag.*nVxOCC2", statements_after_occ3))
            expect_true(grepl("ranef.*block.*nVxOCC3", statements_after_occ3))
            expect_snapshot_value(statements_after_occ3, cran = FALSE, variant = "add_multi_occ_no_ranef_occ3")
          })

# Consider keeping this simple diagonal removal test as a basic check
test_that("removeCovariate works for single param from diagonal occasion ranef",
          {
            model <-
              pkmodel(numCompartments = 3,
                      data = dt_dummyData,
                      columnMap = FALSE)
            model <- addCovariate(
              model,
              covariate = "OCC1",
              type = "Occasion",
              levels = c(0, 1),
              effect = c("V", "Cl", "V2", "Cl2"),
              isDiagonal = TRUE,
              values = c(0.1, 0.2, 0.3, 0.4)
            )
            # Remove occ1 from V2
            model_removed <-
              removeCovariate(model, covariate = "OCC1", paramName = "V2")
            statements <- get_model_statements(model_removed)
            # Expect V, Cl, Cl2; Values 0.1, 0.2, 0.4
            expect_true(grepl("diag\\(nVxOCC10,nClxOCC10,nCl2xOCC10\\)", statements))
            expect_false(grepl("nV2xOCC1", statements))
            vals <-
              extract_diag_ranef_values(strsplit(statements, "\n")[[1]], "nVxOCC10")
            expect_equal(vals, c(0.1, 0.2, 0.4))
            expect_snapshot_value(statements, cran = FALSE, variant = "diag_remove_single_v2")
          })

#' @srrstats {G5.2} Tests exercise successful calls and error conditions for
#' utility functions including `parse_formula_vars()`, `lmerge()`, and
#' `BatchControl()`.
#' @srrstats {G5.3} Tests verify expected types, values, and list structure
#' for utility function outputs.
#' @srrstats {G5.6} Results of `modelCriterion()` with a function argument
#' match the base-R AIC/BIC exactly, confirming correctness.
#' @srrstats {G2.6} `parse_formula_vars()` correctly extracts variables by
#' name and group from complex formula expressions.
test_that("model Criterion functions work correctly", {

  mylm<- lm(mpg ~ wt + hp, data = mtcars)

   expect_identical(modelCriterion(mylm,AIC), stats::AIC(mylm))
   expect_identical(modelCriterion(mylm,BIC), stats::BIC(mylm))

  #' Not strictly necessary to test formulas, but good to know `parse_formula_vars` runs
  formula <- y ~ x + z + asymmetric(z) + Lasymmetric(x1+x2) + Sasymmetric(x3+x4+x5) + deterministic(d1) + trend
  vars <- parse_formula_vars(formula)

  expect_type(vars, "list")
  expect_true("asymmetric" %in% names(vars$inside))
  expect_true("Sasymmetric" %in% names(vars$inside))
  expect_true("x4" %in% vars$inside$Sasymmetric)
  expect_false("x2" %in% vars$inside$Sasymmetric)
  expect_true(vars$intercept)
  expect_false(vars$dot)
})

#' @srrstats {G5.4a} Tests simple deterministic cases to verify formula
#' parsing behaviour with the dot operator and no-intercept syntax.
test_that("parse_formula_vars handles dot and intercept removal", {
  vars_dot <- parse_formula_vars(y ~ .)
  expect_true(vars_dot$dot)

  vars_noint <- parse_formula_vars(y ~ x - 1)
  expect_false(vars_noint$intercept)

  vars_noint2 <- parse_formula_vars(y ~ x + 0)
  expect_false(vars_noint2$intercept)
})

#' @srrstats {G5.4a} Confirms that a character string formula is coerced
#' and parsed correctly.
test_that("parse_formula_vars handles character input", {
  vars <- parse_formula_vars("y ~ x + z")
  expect_type(vars, "list")
  expect_true("x" %in% vars$outside)
})

#' @srrstats {G5.8c} Tests degenerate/unusual formula syntax (variable
#' subtraction) to ensure robust parsing.
test_that("parse_formula_vars handles subtraction of a variable", {
  vars <- parse_formula_vars(y ~ x + z - w)
  expect_true("w" %in% vars$outside)
})


#' @srrstats {G5.4a} Verifies correct priority logic: keys in the first list
#' win over the same keys in the second list.
test_that("lmerge prioritises first list", {
  a <- list(x = 1, y = 2)
  b <- list(x = 99, z = 3)
  result <- lmerge(a, b)
  expect_identical(result$x, 1)   #' a wins
  expect_identical(result$z, 3)   #' only in b
  expect_identical(result$y, 2)   #' only in a
})

#' @srrstats {G5.4a} Tests three-way merge with priority to the leftmost list.
test_that("lmerge works with more than two lists", {
  a <- list(x = 1)
  b <- list(y = 2)
  cc <- list(z = 3, x = 100)
  result <- lmerge(a, b, cc)
  expect_identical(result$x, 1)   #' a wins over cc
  expect_identical(result$z, 3)
})

#' @srrstats {G5.4a} Verifies lmerge behaviour with named atomic vectors
#' in addition to lists.
test_that("lmerge handles named vectors", {
  a <- c(k = 1)
  b <- c(k = 99, m = 5)
  result <- lmerge(a, b)
  expect_identical(result[["k"]], 1)
  expect_identical(result[["m"]], 5)
})


#' @srrstats {G5.4a} Tests the single-batch case where all lag combinations
#' are processed in one run.
test_that("BatchControl returns full range for '1/1'", {
  kardl_reset()
  spec <- kardl(imf_example_data, CPI ~ ER + PPI,
                mode = "grid_custom", maxlag = 2)
  #' Reconstruct a minimal spec-like list for BatchControl
  fake_spec <- list(
    argsInfo = list(batch = "1/1"),
    extractedInfo = list(lagRowsNumber = 100)
  )
  res <- BatchControl(fake_spec)
  expect_identical(res$startRow, 1)
  expect_identical(res$endRow, 100)
})

#' @srrstats {G5.4a} Tests that the second of four equal batches returns the
#' correct start and end row indices.
test_that("BatchControl splits correctly for '2/4'", {
  fake_spec <- list(
    argsInfo = list(batch = "2/4"),
    extractedInfo = list(lagRowsNumber = 100)
  )
  res <- BatchControl(fake_spec)
  expect_identical(res$startRow, 26)
  expect_identical(res$endRow, 50)
})

#' @srrstats {G5.2a} Checks that an out-of-range batch number triggers a
#' warning with an informative message.
test_that("BatchControl warns and defaults to batch 1 when batch number out of range", {
  fake_spec <- list(
    argsInfo = list(batch = "10/4"),
    extractedInfo = list(lagRowsNumber = 100)
  )
  expect_warning(BatchControl(fake_spec), "between 1 and")
})

#' @srrstats {G5.2a} Checks that a malformed batch string produces an
#' informative error before any processing occurs.
#' @srrstats {G5.8a} Confirms that invalid inputs are rejected early.
test_that("BatchControl errors on malformed batch string", {
  fake_spec <- list(
    argsInfo = list(batch = "bad_format"),
    extractedInfo = list(lagRowsNumber = 100)
  )
  expect_error(BatchControl(fake_spec), "Invalid batch format")
})

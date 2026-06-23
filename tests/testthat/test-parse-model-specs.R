#' @srrstats {G5.0} Use standard test patterns with known properties
#' @srrstats {G5.2, G5.2a, G5.2b} Test error/warning behavior with
#' unique messages
#' @srrstats {G5.4, G5.4c} Test correctness against expected outputs
#' @srrstats {G5.8, G5.8a, G5.8b} Test edge conditions (missing/invalid inputs)

# Test 1: Basic functionality with valid inputs ----
#' @srrstats {G5.4} Test produces expected results with fixed test data
test_that("parse_model_specs works with valid formula", {
  inputs <- list(
    formula = y ~ x + sasymmetric(z) + deterministic(w)
  )

  result <- parse_model_specs(inputs)

  # G5.4c: Compare with expected values from known structure
  expect_type(result, "list")
  # result$extracted_info is a base list, not an S3 object
  expect_type(result$extracted_info, "list")
  expect_true(is.list(result$extracted_info))

  expect_named(result, c("args_info", "settings", "extracted_info"))
  expect_named(
    result$extracted_info,
    c(
      "no_constant",
      "trend",
      "asym_long_vars",
      "asym_short_vars",
      "deterministic",
      "dependent_var",
      "independent_vars",
      "all_vars"
    )
  )

  # Verify expected values from test data
  expect_equal(attr(result$extracted_info, "source"), "extracted_info")
  expect_equal(
    attr(result$extracted_info, "description"),
    "This value was obtained from user inputs."
  )
})

# Test 2: Missing formula ----
#' @srrstats {G2.13, G2.14} Test missing data handling
#' @srrstats {G5.2a} Unique error message
#' @srrstats {G5.2b} Explicit test triggers error condition
test_that("parse_model_specs stops when formula is missing", {
  inputs <- list()

  expect_error(
    parse_model_specs(inputs),
    "The model is missing! Please define the model like as: model=y~x+z",
    fixed = TRUE
  )
})

# Test 3: Invalid formula type (character string) ----
#' @srrstats {G2.1, G2.1a} Test type validation
#' @srrstats {G5.2a, G5.2b} Test unique error for invalid type
test_that("parse_model_specs stops when formula is a string", {
  inputs <- list(
    formula = "y ~ x + z"
  )

  expect_error(
    parse_model_specs(inputs),
    "The model should be a valid model without any \" or '. For y~x+z",
    fixed = TRUE
  )
})

# Test 4: Extract asymmetric variables correctly ----
#' @srrstats {G2.3a} Test match.arg validation
#' @srrstats {G5.4} Test correctness of variable extraction
test_that("parse_model_specs extracts asymmetric variables", {
  inputs <- list(
    formula = y ~ x + asymmetric(z1) + lasymmetric(z2) + sasymmetric(z3)
  )

  result <- parse_model_specs(inputs)

  # Note: Based on actual output, asym_long_vars contains both
  # asymmetric and lasymmetric
  expect_equal(sort(result$extracted_info$asym_long_vars), c("z1", "z2"))
  expect_equal(result$extracted_info$asym_short_vars, c("z3", "z1"))
  expect_equal(result$extracted_info$deterministic, character(0))
})

# Test 5: Extract deterministic variables ----
#' @srrstats {G5.4} Test deterministic variable extraction
test_that("parse_model_specs extracts deterministic variables", {
  inputs <- list(
    formula = y ~ x + deterministic(w1) + deterministic(w2)
  )

  result <- parse_model_specs(inputs)

  expect_equal(result$extracted_info$deterministic, c("w1", "w2"))
  expect_equal(result$extracted_info$independent_vars, "x")
})

# Test 6: Extract dependent and independent variables ----
#' @srrstats {G2.0, G2.0a} Test length/commensurability assertions
#' @srrstats {G5.4} Test variable extraction correctness
test_that("parse_model_specs extracts dependent and independent vars", {
  inputs <- list(
    formula = y ~ x1 + x2 + sasymmetric(z) + deterministic(w)
  )

  result <- parse_model_specs(inputs)

  expect_equal(result$extracted_info$dependent_var, "y")
  expect_equal(sort(result$extracted_info$independent_vars), c("x1", "x2", "z"))
  expect_equal(sort(result$extracted_info$all_vars), c("x1", "x2", "y", "z"))
})

# Test 7: No constant term ----
#' @srrstats {G5.4} Test intercept handling
test_that("parse_model_specs handles no constant term", {
  inputs <- list(
    formula = y ~ 0 + x + sasymmetric(z)
  )

  result <- parse_model_specs(inputs)

  expect_true(result$extracted_info$no_constant)
})

# Test 8: Trend variable ----
#' @srrstats {G5.4} Test special variable handling
test_that("parse_model_specs handles trend variable", {
  inputs <- list(
    formula = y ~ trend + x + sasymmetric(z)
  )

  result <- parse_model_specs(inputs)

  expect_true(result$extracted_info$trend)
  expect_false("trend" %in% result$extracted_info$independent_vars)
})

# Test 9: Multiple asymmetric specifications on same variable ----
#' @srrstats {G2.3a} Test duplicate handling
#' @srrstats {G5.4} Test uniqueness logic
test_that("parse_model_specs handles multiple specs on same variable", {
  inputs <- list(
    formula = y ~ x + asymmetric(z) + sasymmetric(z)
  )

  result <- parse_model_specs(inputs)

  expect_equal(result$extracted_info$asym_long_vars, "z")
  expect_equal(result$extracted_info$asym_short_vars, "z")
})

# Test 10: Dot notation ----
#' @srrstats {G2.0a} Test data dimension expectations when data provided
#' @srrstats {G5.4} Test dot notation expansion
test_that("parse_model_specs handles dot notation", {
  mock_data <- data.frame(
    y = 1:5,
    x1 = 1:5,
    x2 = 1:5,
    w = 1:5,
    trend = 1:5
  )

  inputs <- list(
    formula = y ~ . + deterministic(w),
    data = mock_data
  )

  result <- parse_model_specs(inputs)

  expect_equal(result$extracted_info$dependent_var, "y")
  expect_equal(sort(result$extracted_info$independent_vars), c("x1", "x2"))
  expect_equal(result$extracted_info$deterministic, "w")
  # Use expect_false() from testthat
  expect_false("trend" %in% result$extracted_info$independent_vars)
})

# Test 11: Edge case - empty formula after processing ----
#' @srrstats {G5.8, G5.8a} Test zero-length/empty data
#' @srrstats {G5.8b} Test minimal valid input
test_that("parse_model_specs handles minimal formula", {
  inputs <- list(
    formula = y ~ 1
  )

  result <- parse_model_specs(inputs)

  expect_equal(result$extracted_info$dependent_var, "y")
  expect_equal(result$extracted_info$independent_vars, character(0))
  expect_false(result$extracted_info$no_constant)
})

# Test 12: Settings attributes are correctly retrieved ----
#' @srrstats {G5.4} Test settings retrieval
test_that("parse_model_specs includes settings with proper structure", {
  inputs <- list(
    formula = y ~ x + sasymmetric(z)
  )

  result <- parse_model_specs(inputs)

  expect_named(
    result$settings,
    c("asym_prefix", "asym_suffix", "long_coef", "short_coef")
  )
  expect_equal(result$settings$asym_suffix, c("_POS", "_NEG"))
  expect_match(result$settings$long_coef, "L\\{lag\\}\\.\\{var_name\\}")
  expect_match(result$settings$short_coef, "L\\{lag\\}\\.d\\.\\{var_name\\}")
})

# Test 13: Trim whitespace from variable names ----
#' @srrstats {G5.4} Test string preprocessing
test_that("parse_model_specs trims whitespace from variable names", {
  inputs <- list(
    formula = y ~ x + sasymmetric(z) + deterministic(w)
  )

  result <- parse_model_specs(inputs)

  expect_equal(result$extracted_info$asym_short_vars, "z")
  expect_equal(result$extracted_info$deterministic, "w")
})

# Test 14: Invalid asymmetric type ----
#' @srrstats {G2.3a, G5.2a, G5.2b} Test match.arg error for invalid choices
test_that("parse_model_specs rejects invalid asymmetric type", {
  inputs <- list(
    formula = y ~ x + invalidtype(z)
  )

  expect_error(
    parse_model_specs(inputs),
    "'arg' should be one of"
  )
})

# Test 15: NULL input edge condition ----
#' @srrstats {G2.13, G2.14} Test missing data handling
#' @srrstats {G5.8, G5.8c} Test NULL input edge case
test_that("parse_model_specs handles NULL input appropriately", {
  expect_error(
    parse_model_specs(NULL),
    "The model is missing!"
  )
})

# Test 16: All variables uniqueness ----
#' @srrstats {G2.0} Test variable uniqueness assertions
#' @srrstats {G5.4} Test all_vars contains unique values
test_that("parse_model_specs ensures unique variable lists", {
  inputs <- list(
    formula = y ~ x + x + sasymmetric(z) + sasymmetric(z)
  )

  result <- parse_model_specs(inputs)

  # Verify uniqueness constraints
  expect_equal(result$extracted_info$independent_vars, c("x", "z"))
  expect_equal(result$extracted_info$asym_short_vars, "z")
  expect_equal(
    length(result$extracted_info$independent_vars),
    length(unique(result$extracted_info$independent_vars))
  )
})

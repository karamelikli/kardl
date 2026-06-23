#' Additional comprehensive tests for extract methods
#'
#' @srrstats {G5.2} Tests exercise all extraction paths and error conditions
#' @srrstats {G5.3} Tests verify all component types and edge cases
#' @srrstats {G5.4a} Tests handle NULL and missing components

test_that("kardl_extract.kardl_symmetric handles all component combinations", {
  kardl_reset()

  model <- kardl(CPI ~ asym(ER + PPI), data = imf_example_data, maxlag = 1)
  sym_result <- symmetrytest(model)

  # Test extraction with component = "both"
  both_tests <- kardl_extract(
    sym_result,
    what = "long_wald_tests",
    component = "both"
  )
  expect_type(both_tests, "list")

  # Test extraction with component = "longrun"
  long_tests <- kardl_extract(
    sym_result,
    what = "long_wald_tests",
    component = "longrun"
  )
  expect_type(long_tests, "list")

  # Test extraction with component = "shortrun"
  short_tests <- kardl_extract(
    sym_result,
    what = "short_wald_tests",
    component = "shortrun"
  )
  expect_type(short_tests, "list")
})

test_that("kardl_extract handles H1 hypothesis extraction", {
  kardl_reset()

  model <- kardl(CPI ~ asym(ER + PPI), data = imf_example_data, maxlag = 1)
  sym_result <- symmetrytest(model)

  # Extract H1 hypotheses
  h1 <- kardl_extract(sym_result, what = "long_hypotheses", component = "H1")
  expect_type(h1, "list")
  expect_true("H1" %in% names(h1))
  expect_s3_class(h1, "kardl_hypotheses")
})

test_that("kardl_extract handles variable and component together", {
  kardl_reset()

  model <- kardl(CPI ~ asym(ER + PPI), data = imf_example_data, maxlag = 1)
  sym_result <- symmetrytest(model)

  # Extract specific variable with specific component
  er_h0 <- kardl_extract(
    sym_result,
    what = "short_hypotheses",
    variable = "ER",
    component = "H0"
  )
  expect_type(er_h0, "list")
  expect_true("H0" %in% names(er_h0))
})

test_that("kardl_extract handles span extraction", {
  kardl_reset()

  model <- kardl(CPI ~ ER, data = imf_example_data, maxlag = 1)

  # Test span extraction
  span <- kardl_extract(model, "span")
  expect_type(span, "double")
  expect_true(span > 0)
})

test_that("kardl_extract handles short_run_vars and long_run_vars", {
  kardl_reset()

  model <- kardl(CPI ~ ER + PPI, data = imf_example_data, maxlag = 2)

  # Test short run variables
  short_vars <- kardl_extract(model, "short_run_vars")
  expect_type(short_vars, "character")

  # Test long run variables
  long_vars <- kardl_extract(model, "long_run_vars")
  expect_type(long_vars, "character")
})

test_that("kardl_extract handles alternative hypothesis extraction", {
  kardl_reset()

  model <- kardl(CPI ~ ER + PPI, data = imf_example_data, maxlag = 1)
  test_result <- pssf(model, case = 3, sig = "auto")

  # Test alternative hypothesis extraction
  expect_type(kardl_extract(test_result, "alternative"), "character")
})

test_that("kardl_extract handles data.name extraction", {
  kardl_reset()

  model <- kardl(CPI ~ ER + PPI, data = imf_example_data, maxlag = 1)
  test_result <- pssf(model, case = 3, sig = "auto")

  # Test data.name extraction
  data_name <- kardl_extract(test_result, "data.name")
  expect_type(data_name, "character")
})

test_that("kardl_extract handles sample.size extraction", {
  kardl_reset()

  model <- kardl(CPI ~ ER + PPI, data = imf_example_data, maxlag = 1)
  test_result <- pssf(model, case = 3, sig = "auto")

  # Test sample.size extraction if available
  if (!is.null(test_result$sample.size)) {
    sample_size <- kardl_extract(test_result, "sample.size")
    expect_type(sample_size, "double")
  }
})

test_that("kardl_extract handles hypotheses extraction from test objects", {
  kardl_reset()

  model <- kardl(CPI ~ ER + PPI, data = imf_example_data, maxlag = 1)
  test_result <- pssf(model, case = 3, sig = "auto")

  # Test hypotheses extraction
  hyp <- kardl_extract(test_result, "hypotheses")
  expect_s3_class(hyp, "kardl_hypotheses")
  expect_true("H0" %in% names(hyp))
  expect_true("H1" %in% names(hyp))
})

test_that("kardl_extract handles sig extraction", {
  kardl_reset()

  model <- kardl(CPI ~ ER + PPI, data = imf_example_data, maxlag = 1)
  test_result <- pssf(model, case = 3, sig = "0.05")

  # Test sig extraction
  sig <- kardl_extract(test_result, "sig")
  expect_equal(sig, "0.05")
})

test_that("kardl_extract handles notes extraction when present", {
  kardl_reset()

  model <- kardl(CPI ~ ER + PPI, data = imf_example_data, maxlag = 1)
  test_result <- psst(model, case = 3, sig = "auto")
  test_summ <- summary(test_result)

  # Test notes extraction
  notes <- kardl_extract(test_summ, "notes")
  # Notes may be NULL or character
  expect_true(is.null(notes) || is.character(notes))
})

test_that("kardl_extract handles vars extraction from mplier", {
  kardl_reset()

  model <- kardl(CPI ~ ER + PPI, data = imf_example_data, maxlag = 1)
  mpl <- mplier(model, horizon = 10)

  # Test vars extraction through the "vars" option
  # This might not be in the match.arg list, so it may error
  # Let's check if it errors as expected
  expect_error(kardl_extract(mpl, "vars"))
})

test_that("kardl_extract handles multiple variable extraction correctly", {
  kardl_reset()

  model <- kardl(CPI ~ asym(ER + PPI), data = imf_example_data, maxlag = 1)
  sym_result <- symmetrytest(model)

  # Extract multiple variables from hypotheses
  multi_hyp <- kardl_extract(
    sym_result,
    what = "long_hypotheses",
    variable = c("ER", "PPI")
  )
  expect_s3_class(multi_hyp, "kardl_hypotheses")
  expect_equal(length(multi_hyp$H0), 2)
})

test_that("kardl_extract preserves class structure", {
  kardl_reset()

  model <- kardl(CPI ~ asym(ER), data = imf_example_data, maxlag = 1)
  sym_result <- symmetrytest(model)

  # Extract hypotheses and verify class is preserved
  hyp <- kardl_extract(sym_result, what = "long_hypotheses")
  expect_s3_class(hyp, "kardl_hypotheses")

  # Extract with variable and verify class is preserved
  hyp_var <- kardl_extract(sym_result,
    what = "long_hypotheses",
    variable = "ER"
  )
  expect_s3_class(hyp_var, "kardl_hypotheses")
})

test_that("kardl_extract handles empty variable lists", {
  kardl_reset()

  # Model without asymmetry
  model <- kardl(CPI ~ ER + PPI, data = imf_example_data, maxlag = 1)

  # Extract asymmetric variables (should be empty)
  asym_long <- kardl_extract(model, "asym_long_vars")
  expect_true(length(asym_long) == 0 || is.null(asym_long))

  asym_short <- kardl_extract(model, "asym_short_vars")
  expect_true(length(asym_short) == 0 || is.null(asym_short))
})

test_that("kardl_extract handles type extraction from test objects", {
  kardl_reset()

  model <- kardl(CPI ~ ER + PPI, data = imf_example_data, maxlag = 1)
  test_result <- pssf(model, case = 3, sig = "auto")

  # Test type extraction
  type <- kardl_extract(test_result, "type")
  expect_type(type, "character")
})

test_that("kardl_extract handles case extraction from test_summary", {
  kardl_reset()

  model <- kardl(CPI ~ ER + PPI, data = imf_example_data, maxlag = 1)
  test_result <- pssf(model, case = 3, sig = "auto")
  test_summ <- summary(test_result)

  # Test case extraction
  case <- kardl_extract(test_summ, "case")
  expect_type(case, "character")
  expect_true(case %in% c("I", "II", "III", "IV", "V"))
})

test_that("kardl_extract handles variables extraction from test_summary", {
  kardl_reset()

  model <- kardl(CPI ~ ER + PPI, data = imf_example_data, maxlag = 1)
  test_result <- pssf(model, case = 3, sig = "auto")
  test_summ <- summary(test_result)

  # Test variables extraction
  vars <- kardl_extract(test_summ, "variables")
  expect_type(vars, "character")
  expect_true(length(vars) > 0)
})

test_that("kardl_extract handles k extraction from test_summary", {
  kardl_reset()

  model <- kardl(CPI ~ ER + PPI, data = imf_example_data, maxlag = 1)
  test_result <- pssf(model, case = 3, sig = "auto")
  test_summ <- summary(test_result)

  # Test k extraction
  k <- kardl_extract(test_summ, "k")
  expect_type(k, "double")
  expect_true(k > 0)
})

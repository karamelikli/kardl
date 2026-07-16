#' Test coverage for kardl_extract() methods
#'
#' @srrstats {G5.2} Tests exercise successful calls and error conditions for
#' the extraction interface across all supported object classes.
#' @srrstats {G5.3} Tests verify expected types and values for extracted
#' components.
#' @srrstats {G5.4a} Tests simple deterministic cases to verify extraction
#' behavior with minimal objects.

test_that("kardl_extract works for kardl_lm objects", {
  kardl_reset()

  # Create a simple kardl model
  model <- kardl(DriversKilled ~ PetrolPrice + drivers,
    data = Seatbelts,
    maxlag = 2
  )

  # Test extraction of basic components
  expect_type(kardl_extract(model, "dependent_var"), "character")
  expect_equal(kardl_extract(model, "dependent_var"), "DriversKilled")

  expect_type(kardl_extract(model, "independent_vars"), "character")
  expect_true(
    all(
      c("PetrolPrice", "drivers") %in% kardl_extract(model, "independent_vars")
    )
  )

  expect_type(kardl_extract(model, "all_vars"), "character")
  expect_true("DriversKilled" %in% kardl_extract(model, "all_vars"))

  # Test extraction of model info
  expect_type(kardl_extract(model, "opt_lag"), "double")
  expect_type(kardl_extract(model, "n"), "integer")
  expect_type(kardl_extract(model, "k"), "integer")

  # Test extraction of time info
  expect_type(kardl_extract(model, "start_time"), "double")
  expect_type(kardl_extract(model, "end_time"), "double")

  # Test extraction of data
  expect_s3_class(kardl_extract(model, "data"), "ts")
})

test_that("kardl_extract works for kardl_mplier objects", {
  kardl_reset()

  model <- kardl(DriversKilled ~ PetrolPrice, data = Seatbelts, maxlag = 1)
  mpl <- mplier(model, horizon = 10)

  # Test extraction of multipliers
  mult <- kardl_extract(mpl, "multipliers")
  expect_type(mult, "double")
  expect_true(is.matrix(mult))

  # Test extraction of horizon
  expect_equal(kardl_extract(mpl, "horizon"), 10)

  # Test extraction of omega
  omega <- kardl_extract(mpl, "omega")
  expect_type(omega, "double")

  # Test extraction of lambda
  lambda <- kardl_extract(mpl, "lambda")
  expect_type(lambda, "double")
})

test_that("kardl_extract works for kardl_boot objects", {
  kardl_reset()

  model <- kardl(DriversKilled ~ PetrolPrice, data = Seatbelts, maxlag = 1)
  boot_result <- bootstrap(model, horizon = 5, replications = 10, level = 95)

  # Test extraction of multipliers
  mult <- kardl_extract(boot_result, "multipliers")
  expect_type(mult, "list")

  # Test extraction of confidence level
  expect_equal(kardl_extract(boot_result, "level"), 95)

  # Test extraction of replications
  expect_equal(kardl_extract(boot_result, "replications"), 10)

  # Test extraction of horizon
  expect_equal(kardl_extract(boot_result, "horizon"), 5)
})

test_that("kardl_extract works for kardl_test objects", {
  kardl_reset()

  model <- kardl(DriversKilled ~ PetrolPrice + drivers,
    data = Seatbelts,
    maxlag = 1
  )
  test_result <- pssf(model, case = 3, sig = "auto")

  # Test extraction of test components
  expect_type(kardl_extract(test_result, "statistic"), "double")
  expect_type(kardl_extract(test_result, "case"), "double")
  expect_type(kardl_extract(test_result, "method"), "character")
  expect_type(kardl_extract(test_result, "var_names"), "character")

  # Test extraction of sample size info
  expect_type(kardl_extract(test_result, "k"), "double")
  expect_type(kardl_extract(test_result, "n"), "double")
})

test_that("kardl_extract works for kardl_test_summary objects", {
  kardl_reset()

  model <- kardl(DriversKilled ~ PetrolPrice + drivers,
    data = Seatbelts,
    maxlag = 1
  )
  test_result <- pssf(model, case = 3, sig = "auto")
  test_summary <- summary(test_result)

  # Test extraction of summary components
  expect_type(kardl_extract(test_summary, "statistic"), "double")
  expect_type(kardl_extract(test_summary, "case"), "character")
  expect_type(kardl_extract(test_summary, "decision"), "character")
  expect_type(kardl_extract(test_summary, "numeric_decision"), "double")
  expect_type(kardl_extract(test_summary, "significance_level"), "character")

  # Test extraction of critical values
  cr_vals <- kardl_extract(test_summary, "critical_values")
  expect_s3_class(cr_vals, "data.frame")
  expect_true("L" %in% names(cr_vals))
  expect_true("U" %in% names(cr_vals))
})

test_that("kardl_extract works for kardl_symmetric objects", {
  kardl_reset()

  model <- kardl(DriversKilled ~ asym(PetrolPrice + drivers),
    data = Seatbelts,
    maxlag = 1
  )
  sym_result <- symmetrytest(model)

  # Test extraction of summary tables
  expect_s3_class(
    kardl_extract(sym_result, "long_wald_summary"),
    "data.frame"
  )
  expect_s3_class(
    kardl_extract(sym_result, "short_wald_summary"),
    "data.frame"
  )

  # Test extraction of variables
  vars <- kardl_extract(sym_result, "vars")
  expect_type(vars, "character")

  # Test extraction of type
  expect_type(kardl_extract(sym_result, "type"), "character")

  # Test extraction of Wald tests
  long_tests <- kardl_extract(sym_result, "long_wald_tests")
  expect_type(long_tests, "list")

  # Test extraction for specific variable
  er_test <- kardl_extract(sym_result, "long_wald_tests",
    variable = "PetrolPrice"
  )
  expect_s3_class(er_test, "htest")

  # expect error for non-existent variable
  expect_error(
    kardl_extract(sym_result, "short_hypotheses", variable = "NONEXISTENT")
  )

  # Test extraction of hypotheses
  long_hyp <- kardl_extract(sym_result, "long_hypotheses")
  expect_s3_class(long_hyp, "kardl_hypotheses")

  # Test extraction of hypotheses for specific variable
  er_hyp <- kardl_extract(sym_result, "long_hypotheses",
    variable = "PetrolPrice"
  )
  expect_s3_class(er_hyp, "kardl_hypotheses")

  # Test extraction of specific hypothesis component
  h0 <- kardl_extract(sym_result, "long_hypotheses", component = "H0")
  expect_type(h0, "list")
  expect_true("H0" %in% names(h0))
})

test_that("kardl_extract errors appropriately for unsupported classes", {
  # Test default method error
  obj <- list(a = 1, b = 2)
  class(obj) <- "unsupported_class"

  expect_error(
    kardl_extract(obj, "something"),
    "No kardl_extract\\(\\) method for objects of class"
  )
})

test_that("kardl_extract errors for invalid 'what' arguments", {
  kardl_reset()

  model <- kardl(DriversKilled ~ PetrolPrice, data = Seatbelts, maxlag = 1)

  # Invalid what for kardl_lm
  expect_error(
    kardl_extract(model, "invalid_component")
  )

  mpl <- mplier(model, horizon = 5)

  # Invalid what for kardl_mplier
  expect_error(
    kardl_extract(mpl, "invalid_component")
  )
})

test_that("kardl_extract handles variable extraction correctly", {
  kardl_reset()

  model <- kardl(DriversKilled ~ asym(PetrolPrice + drivers),
    data = Seatbelts,
    maxlag = 1
  )
  sym_result <- symmetrytest(model)

  # Test extraction with multiple variables
  multi_vars <- kardl_extract(
    sym_result,
    "long_wald_tests",
    variable = c("PetrolPrice", "drivers")
  )
  expect_type(multi_vars, "list")
  expect_equal(length(multi_vars), 2)
  expect_true(all(c("PetrolPrice", "drivers") %in% names(multi_vars)))

  # Test error for non-existent variable
  expect_error(
    kardl_extract(sym_result, "long_wald_tests", variable = "NONEXISTENT"),
    "Variable\\(s\\) not found"
  )
})

test_that("kardl_extract handles asymmetric variable extraction", {
  kardl_reset()

  model <- kardl(
    DriversKilled ~ lasymmetric(PetrolPrice) + sasymmetric(drivers),
    data = Seatbelts,
    maxlag = 1
  )

  # Test extraction of asymmetric variable lists
  asym_long <- kardl_extract(model, "asym_long_vars")
  asym_short <- kardl_extract(model, "asym_short_vars")
  all_asym <- kardl_extract(model, "all_asym_vars")

  expect_true("PetrolPrice" %in% asym_long)
  expect_true("drivers" %in% asym_short)
  expect_true(all(c("PetrolPrice", "drivers") %in% all_asym))

  # Test extraction of excluded variables
  indep_al_excluded <- kardl_extract(model, "indep_al_excluded")
  indep_as_excluded <- kardl_extract(model, "indep_as_excluded")

  expect_type(indep_al_excluded, "character")
  expect_type(indep_as_excluded, "character")
})

test_that("kardl_extract retrieves model specification correctly", {
  kardl_reset()

  model <- kardl(
    DriversKilled ~ PetrolPrice + drivers + deterministic(law) + trend - 1,
    data = Seatbelts,
    maxlag = 2
  )

  # Test no constant flag
  expect_true(kardl_extract(model, "no_constant"))

  # Test trend specification
  expect_true(kardl_extract(model, "trend"))

  # Test deterministic variables
  det_vars <- kardl_extract(model, "deterministic")
  expect_true("law" %in% det_vars)

  # Test model type
  model_type <- kardl_extract(model, "model_type")
  expect_type(model_type, "character")
})

test_that("kardl_extract handles lag information correctly", {
  kardl_reset()

  model <- kardl(DriversKilled ~ PetrolPrice + drivers,
    data = Seatbelts,
    maxlag = 3
  )

  # Test lag criteria extraction
  lag_criteria <- kardl_extract(model, "lag_criteria")
  expect_type(lag_criteria, "double")

  # Test all lag combinations (may be NULL in quick mode)
  all_lags <- kardl_extract(model, "all_cr_lags")
  # In quick mode, all_cr_lags may be NULL
  if (!is.null(all_lags)) {
    expect_type(all_lags, "list")
  }

  # Test shortrun length
  shortrun_len <- kardl_extract(model, "shortrun_length")
  expect_type(shortrun_len, "double")

  # Test lag rows number
  lag_rows <- kardl_extract(model, "lag_rows_number")
  expect_type(lag_rows, "double")
})

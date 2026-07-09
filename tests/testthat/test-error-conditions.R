#' Tests for error conditions and edge cases in kardl functions
#'
#' @srrstats {G5.2} Tests for error handling and validation
#' @srrstats {G5.8} Tests verify expected errors occur
#' @srrstats {G5.8b} Tests validate error messages

test_that("kardl_extract errors with invalid what for kardl_lm", {
  kardl_reset()

  model <- kardl(DriversKilled ~ PetrolPrice, data = Seatbelts, maxlag = 1)

  # Invalid what value
  expect_error(
    kardl_extract(model, "invalid_component")
  )
})

test_that("kardl_extract errors with invalid what for kardl_mplier", {
  kardl_reset()

  model <- kardl(DriversKilled ~ PetrolPrice, data = Seatbelts, maxlag = 1)
  mpl <- mplier(model, horizon = 10)

  # Invalid what value should error
  expect_error(
    kardl_extract(mpl, "invalid_thing")
  )
})

test_that("kardl_extract errors with invalid what for kardl_test", {
  kardl_reset()

  model <- kardl(DriversKilled ~ PetrolPrice + drivers, data = Seatbelts, maxlag = 1)
  test_result <- pssf(model, case = 3, sig = "auto")

  # Invalid what value should error
  expect_error(
    kardl_extract(test_result, "invalid_stat")
  )
})

test_that("summary.kardl_test errors with wrong test function", {
  kardl_reset()

  model <- kardl(DriversKilled ~ PetrolPrice + drivers, data = Seatbelts, maxlag = 1)
  test_result <- pssf(model, case = 3, sig = "auto")

  # Modify test.func to invalid value
  test_result$test.func <- "invalid_test"

  expect_error(
    summary(test_result),
    "only applicable to test objects"
  )
})

test_that("mplier errors with invalid horizon", {
  kardl_reset()

  model <- kardl(DriversKilled ~ PetrolPrice, data = Seatbelts, maxlag = 1)

  # Negative horizon
  expect_error(
    mplier(model, horizon = -1)
  )
})

test_that("kardl_get returns current options", {
  kardl_reset()

  opts <- kardl_get()
  expect_type(opts, "list")
  expect_true(length(opts) > 0)
})

test_that("model_criterion handles custom function", {
  kardl_reset()

  model <- kardl(DriversKilled ~ PetrolPrice, data = Seatbelts, maxlag = 1)

  # Custom criterion function
  custom_crit <- function(x) AIC(x) + BIC(x)

  result <- model_criterion(model, custom_crit)
  expect_type(result, "double")
})

test_that("bounds tests handle 0.025 significance level", {
  kardl_reset()

  model <- kardl(DriversKilled ~ PetrolPrice + drivers, data = Seatbelts, maxlag = 1)

  # Test with 0.025 significance level
  pssf_025 <- pssf(model, case = 3, sig = "0.025")
  expect_s3_class(pssf_025, "htest")

  psst_025 <- psst(model, case = 3, sig = "0.025")
  expect_s3_class(psst_025, "htest")
})

test_that("plot functions error with invalid inputs", {
  kardl_reset()

  model <- kardl(DriversKilled ~ PetrolPrice + drivers, data = Seatbelts, maxlag = 1)
  mpl <- mplier(model, horizon = 10)

  # Invalid variable name
  expect_error(
    plot(mpl, variables = "NOTAVAR")
  )
})

test_that("kardl handles formula with I() transformation", {
  kardl_reset()

  # Formula with I() transformation
  expect_silent({
    model <- kardl(I(log(DriversKilled)) ~ PetrolPrice, data = Seatbelts, maxlag = 1)
  })
  expect_s3_class(model, "kardl_lm")
})

test_that("kardl works with different criterion functions", {
  kardl_reset()

  # Test with BIC
  model_bic <- kardl(
    DriversKilled ~ PetrolPrice + drivers,
    data = Seatbelts,
    maxlag = 2,
    criterion = "BIC"
  )
  expect_s3_class(model_bic, "kardl_lm")

  # Test with HQ
  model_hq <- kardl(
    DriversKilled ~ PetrolPrice + drivers,
    data = Seatbelts,
    maxlag = 2,
    criterion = "HQ"
  )
  expect_s3_class(model_hq, "kardl_lm")
})

test_that("case mapping in tests is correct", {
  kardl_reset()

  model <- kardl(DriversKilled ~ PetrolPrice + drivers, data = Seatbelts, maxlag = 1)

  # Test that case 2 maps correctly (should map to 3 in some contexts)
  pssf_c2 <- pssf(model, case = 2, sig = "auto")
  expect_true(is.numeric(pssf_c2$case))

  # Test that case 4 maps correctly (should map to 5 in some contexts)
  pssf_c4 <- pssf(model, case = 4, sig = "auto")
  expect_true(is.numeric(pssf_c4$case))
})

test_that("kardl works with Lasymmetric specification", {
  kardl_reset()

  model <- kardl(
    DriversKilled ~ Lasymmetric(PetrolPrice),
    data = Seatbelts,
    maxlag = 1
  )
  expect_s3_class(model, "kardl_lm")

  # Check asymmetric variables are detected
  asym_vars <- kardl_extract(model, "asym_long_vars")
  expect_true(length(asym_vars) > 0)
})

test_that("kardl works with Sasymmetric specification", {
  kardl_reset()

  model <- kardl(
    DriversKilled ~ Sasymmetric(PetrolPrice),
    data = Seatbelts,
    maxlag = 1
  )
  expect_s3_class(model, "kardl_lm")

  # Check asymmetric variables are detected
  asym_vars <- kardl_extract(model, "asym_short_vars")
  expect_true(length(asym_vars) > 0)
})

test_that("kardl_longrun works correctly", {
  kardl_reset()

  model <- kardl(DriversKilled ~ PetrolPrice + drivers, data = Seatbelts, maxlag = 1)
  expect_warning(
    lr <- kardl_longrun(model)
  )

  expect_s3_class(lr, "kardl_longrun")
  expect_s3_class(lr, "lm")
})

test_that("bootstrap seed produces reproducible results", {
  kardl_reset()

  model <- kardl(DriversKilled ~ PetrolPrice, data = Seatbelts, maxlag = 1)

  # Run with same seed
  set.seed(123)
  boot1 <- bootstrap(model, horizon = 5, replications = 10)

  set.seed(123)
  boot2 <- bootstrap(model, horizon = 5, replications = 10)

  # Results should be identical
  expect_equal(
    kardl_extract(boot1, "multipliers"),
    kardl_extract(boot2, "multipliers")
  )
})

test_that("summary.kardl_symmetric handles different levels", {
  kardl_reset()

  model <- kardl(DriversKilled ~ asym(PetrolPrice), data = Seatbelts, maxlag = 1)
  sym_result <- symmetrytest(model)

  # Test with different levels
  summ_10 <- summary(sym_result, level = 0.10)
  summ_05 <- summary(sym_result, level = 0.05)
  summ_01 <- summary(sym_result, level = 0.01)

  expect_type(summ_10, "list")
  expect_type(summ_05, "list")
  expect_type(summ_01, "list")
})

test_that("kardl works with trend specification", {
  kardl_reset()

  model <- kardl(
    DriversKilled ~ PetrolPrice + trend,
    data = Seatbelts,
    maxlag = 1
  )
  expect_s3_class(model, "kardl_lm")

  # Check trend is detected
  trend_val <- kardl_extract(model, "trend")
  expect_true(!is.null(trend_val))
})

test_that("kardl works without intercept", {
  kardl_reset()

  model <- kardl(
    DriversKilled ~ PetrolPrice - 1,
    data = Seatbelts,
    maxlag = 1
  )
  expect_s3_class(model, "kardl_lm")

  # Check no_constant is TRUE
  no_const <- kardl_extract(model, "no_constant")
  expect_true(no_const)
})

test_that("mplier variables parameter works", {
  kardl_reset()

  model <- kardl(DriversKilled ~ PetrolPrice + drivers, data = Seatbelts, maxlag = 1)

  # Test with specific variable selection
  mpl_er <- mplier(model, horizon = 10, variables = "PetrolPrice")
  expect_s3_class(mpl_er, "kardl_mplier")

  # Check multipliers exist
  mults <- kardl_extract(mpl_er, "multipliers")
  expect_type(mults, "double")
})

test_that("bootstrap variables parameter works", {
  kardl_reset()

  model <- kardl(DriversKilled ~ PetrolPrice + drivers, data = Seatbelts, maxlag = 1)

  # Test with specific variable selection
  boot_er <- bootstrap(model, horizon = 5, replications = 10, variables = "PetrolPrice")
  expect_s3_class(boot_er, "kardl_boot")

  # Check that bootstrap completed
  level <- kardl_extract(boot_er, "level")
  expect_type(level, "double")
})

test_that("bootstrap confidence levels work", {
  kardl_reset()

  model <- kardl(DriversKilled ~ PetrolPrice, data = Seatbelts, maxlag = 1)

  # Test with 90% confidence
  boot_90 <- bootstrap(model, horizon = 5, replications = 10, level = 0.90)
  expect_equal(kardl_extract(boot_90, "level"), 0.90)

  # Test with 99% confidence
  boot_99 <- bootstrap(model, horizon = 5, replications = 10, level = 0.99)
  expect_equal(kardl_extract(boot_99, "level"), 0.99)
})

test_that("narayan handles all observation numbers", {
  kardl_reset()

  model <- kardl(DriversKilled ~ PetrolPrice + drivers, data = Seatbelts, maxlag = 1)

  # Narayan test should work
  narayan_test <- narayan(model, case = 2, sig = "auto")
  expect_s3_class(narayan_test, "htest")

  summ <- summary(narayan_test)
  expect_s3_class(summ, "kardl_test_summary")
})

test_that("pssf and psst produce consistent output", {
  kardl_reset()

  model <- kardl(DriversKilled ~ PetrolPrice + drivers, data = Seatbelts, maxlag = 1)

  pssf_result <- pssf(model, case = 3, sig = "auto")
  psst_result <- psst(model, case = 3, sig = "auto")

  # Both should have similar structure
  expect_true("statistic" %in% names(pssf_result))
  expect_true("statistic" %in% names(psst_result))

  expect_true("hypotheses" %in% names(pssf_result))
  expect_true("hypotheses" %in% names(psst_result))
})

test_that("kardl handles asymmetric models", {
  kardl_reset()

  model <- kardl(
    DriversKilled ~ PetrolPrice + asym(drivers),
    data = Seatbelts,
    maxlag = 1
  )
  expect_s3_class(model, "kardl_lm")

  # Check asymmetric variables
  asym_long <- kardl_extract(model, "asym_long_vars")
  expect_true(length(asym_long) > 0)
})

#' Comprehensive tests for additional coverage
#'
#' @srrstats {G5.2} Tests for uncovered code paths
#' @srrstats {G5.4a} Tests for edge cases and boundary conditions

test_that("kardl with fixed lags (mode parameter)", {
  kardl_reset()

  # Test with fixed lags for each variable
  model <- kardl(
    DriversKilled ~ PetrolPrice + drivers,
    data = Seatbelts,
    mode = c(2, 1, 1), # Dependent, PetrolPrice, drivers
    maxlag = 3
  )
  expect_s3_class(model, "kardl_lm")
})

test_that("kardl with Sasymmetric and Lasymmetric combinations", {
  kardl_reset()

  # Model with both S and L asymmetric
  model <- kardl(
    DriversKilled ~ Lasymmetric(PetrolPrice) + Sasymmetric(drivers),
    data = Seatbelts,
    maxlag = 1
  )
  expect_s3_class(model, "kardl_lm")

  # Check both types are detected
  asym_long <- kardl_extract(model, "asym_long_vars")
  asym_short <- kardl_extract(model, "asym_short_vars")

  expect_true("PetrolPrice" %in% asym_long)
  expect_true("drivers" %in% asym_short)
})

test_that("symmetrytest with Lasymmetric only", {
  kardl_reset()

  model <- kardl(
    DriversKilled ~ Lasymmetric(PetrolPrice),
    data = Seatbelts,
    maxlag = 1
  )

  # Test should focus on long-run
  sym_test <- symmetrytest(model)
  expect_s3_class(sym_test, "kardl_symmetric")

  # Should have long-run tests
  long_tests <- kardl_extract(sym_test, "long_wald_tests")
  expect_type(long_tests, "list")
})

test_that("symmetrytest with Sasymmetric only", {
  kardl_reset()

  model <- kardl(
    DriversKilled ~ Sasymmetric(PetrolPrice),
    data = Seatbelts,
    maxlag = 1
  )

  # Test should focus on short-run
  sym_test <- symmetrytest(model)
  expect_s3_class(sym_test, "kardl_symmetric")

  # Should have short-run tests
  short_tests <- kardl_extract(sym_test, "short_wald_tests")
  expect_type(short_tests, "list")
})

test_that("kardl handles AICc criterion", {
  kardl_reset()

  model <- kardl(
    DriversKilled ~ PetrolPrice + drivers,
    data = Seatbelts,
    maxlag = 2,
    criterion = "AICc"
  )
  expect_s3_class(model, "kardl_lm")
})

test_that("bootstrap with progress=FALSE", {
  kardl_reset()

  model <- kardl(DriversKilled ~ PetrolPrice, data = Seatbelts, maxlag = 1)

  # Bootstrap without progress
  boot_result <- bootstrap(
    model,
    horizon = 5,
    replications = 10,
    progress = FALSE
  )
  expect_s3_class(boot_result, "kardl_boot")
})

test_that("mplier with specific variable subset", {
  kardl_reset()

  model <- kardl(DriversKilled ~ PetrolPrice + drivers, data = Seatbelts, maxlag = 1)

  # Test extracting multipliers for single variable
  mpl_single <- mplier(model, horizon = 10, variables = "drivers")
  expect_s3_class(mpl_single, "kardl_mplier")

  mults <- kardl_extract(mpl_single, "multipliers")
  expect_true(ncol(mults) >= 1)
})

test_that("plot.kardl_mplier with different parameters", {
  kardl_reset()

  model <- kardl(DriversKilled ~ PetrolPrice, data = Seatbelts, maxlag = 1)
  mpl <- mplier(model, horizon = 15)

  # Test plot with no specific variables (should plot all)
  plots1 <- plot(mpl)
  expect_type(plots1, "list")

  # Test plot with variables = NULL
  plots2 <- plot(mpl, variables = NULL)
  expect_type(plots2, "list")
})

test_that("kardl_extract handles case_txt from test_summary", {
  kardl_reset()

  model <- kardl(DriversKilled ~ PetrolPrice + drivers, data = Seatbelts, maxlag = 1)

  # Test case 1 - but it may map to a different case internally
  test_c1 <- pssf(model, case = 1, sig = "auto")
  summ_c1 <- summary(test_c1)
  case_txt1 <- kardl_extract(summ_c1, "case")
  expect_true(case_txt1 %in% c("I", "II", "III", "IV", "V"))

  # Test case 3
  test_c3 <- pssf(model, case = 3, sig = "auto")
  summ_c3 <- summary(test_c3)
  case_txt3 <- kardl_extract(summ_c3, "case")
  expect_true(case_txt3 %in% c("I", "II", "III", "IV", "V"))

  # Test case 5
  test_c5 <- pssf(model, case = 5, sig = "auto")
  summ_c5 <- summary(test_c5)
  case_txt5 <- kardl_extract(summ_c5, "case")
  expect_true(case_txt5 %in% c("I", "II", "III", "IV", "V"))
})

test_that("narayan decision with auto selects correct significance", {
  kardl_reset()

  model <- kardl(DriversKilled ~ PetrolPrice + drivers, data = Seatbelts, maxlag = 1)

  # Test narayan with auto
  narayan_auto <- narayan(model, case = 2, sig = "auto")
  summ <- summary(narayan_auto)

  # Check significance was selected
  expect_true(summ$significance_level %in% c("0.10", "0.05", "0.01"))
})

test_that("psst and pssf with all significance levels", {
  kardl_reset()

  model <- kardl(DriversKilled ~ PetrolPrice, data = Seatbelts, maxlag = 1)

  # Test all four significance levels for pssf
  for (sig in c("0.10", "0.05", "0.025", "0.01")) {
    pssf_test <- pssf(model, case = 3, sig = sig)
    summ <- summary(pssf_test)
    expect_equal(summ$significance_level, sig)
  }

  # Test all four significance levels for psst
  for (sig in c("0.10", "0.05", "0.025", "0.01")) {
    psst_test <- psst(model, case = 3, sig = sig)
    summ <- summary(psst_test)
    expect_equal(summ$significance_level, sig)
  }
})

test_that("kardl_extract handles all kardl_lm components", {
  kardl_reset()

  model <- kardl(DriversKilled ~ PetrolPrice + drivers + trend - 1,
    data = Seatbelts, maxlag = 1
  )

  # Test all extraction options
  expect_true(kardl_extract(model, "no_constant"))
  expect_type(kardl_extract(model, "dependent_var"), "character")
  expect_type(kardl_extract(model, "independent_vars"), "character")
  expect_type(kardl_extract(model, "all_vars"), "character")
  expect_type(kardl_extract(model, "short_run_vars"), "character")
  expect_type(kardl_extract(model, "long_run_vars"), "character")
  expect_type(kardl_extract(model, "shortrun_length"), "double")
  expect_type(kardl_extract(model, "lag_rows_number"), "double")
  expect_type(kardl_extract(model, "start_time"), "double")
  expect_type(kardl_extract(model, "end_time"), "double")
  expect_type(kardl_extract(model, "span"), "double")
  # k and n may be integer or double
  expect_true(is.numeric(kardl_extract(model, "k")))
  expect_true(is.numeric(kardl_extract(model, "n")))
})

test_that("kardl_extract handles all kardl_mplier components", {
  kardl_reset()

  model <- kardl(DriversKilled ~ PetrolPrice, data = Seatbelts, maxlag = 1)
  mpl <- mplier(model, horizon = 10)

  # Test all mplier extraction options
  expect_type(kardl_extract(mpl, "multipliers"), "double")
  expect_type(kardl_extract(mpl, "omega"), "double")
  expect_type(kardl_extract(mpl, "lambda"), "double")
  expect_equal(kardl_extract(mpl, "horizon"), 10)
})

test_that("kardl_extract handles all kardl_boot components", {
  kardl_reset()

  model <- kardl(DriversKilled ~ PetrolPrice, data = Seatbelts, maxlag = 1)
  boot_result <- bootstrap(model, horizon = 5, replications = 10, level = 0.95)

  # Test all boot extraction options
  expect_type(kardl_extract(boot_result, "multipliers"), "list")
  expect_equal(kardl_extract(boot_result, "level"), 0.95)
  expect_equal(kardl_extract(boot_result, "replications"), 10)
  expect_equal(kardl_extract(boot_result, "horizon"), 5)
})

test_that("kardl_extract handles all kardl_test components", {
  kardl_reset()

  model <- kardl(DriversKilled ~ PetrolPrice + drivers, data = Seatbelts, maxlag = 1)
  test_result <- pssf(model, case = 3, sig = "auto")

  # Test all test extraction options
  expect_type(kardl_extract(test_result, "type"), "character")
  expect_type(kardl_extract(test_result, "case"), "double")
  expect_type(kardl_extract(test_result, "statistic"), "double")
  expect_type(kardl_extract(test_result, "method"), "character")
  expect_type(kardl_extract(test_result, "alternative"), "character")
  expect_type(kardl_extract(test_result, "data.name"), "character")
  expect_s3_class(kardl_extract(test_result, "hypotheses"), "kardl_hypotheses")
  expect_type(kardl_extract(test_result, "var_names"), "character")
  expect_type(kardl_extract(test_result, "k"), "double")
  expect_type(kardl_extract(test_result, "sig"), "character")
})

test_that("kardl_extract handles all kardl_test_summary components", {
  kardl_reset()

  model <- kardl(DriversKilled ~ PetrolPrice + drivers, data = Seatbelts, maxlag = 1)
  test_result <- pssf(model, case = 3, sig = "auto")
  test_summ <- summary(test_result)

  # Test all test_summary extraction options
  expect_type(kardl_extract(test_summ, "statistic"), "double")
  expect_type(kardl_extract(test_summ, "case"), "character")
  expect_type(kardl_extract(test_summ, "variables"), "character")
  expect_type(kardl_extract(test_summ, "decision"), "character")
  expect_s3_class(kardl_extract(test_summ, "hypotheses"), "kardl_hypotheses")
  expect_type(kardl_extract(test_summ, "numeric_decision"), "double")
  expect_type(kardl_extract(test_summ, "significance_level"), "character")
  expect_s3_class(kardl_extract(test_summ, "critical_values"), "data.frame")
  expect_type(kardl_extract(test_summ, "k"), "double")
})

test_that("kardl_extract handles all kardl_symmetric components", {
  kardl_reset()

  model <- kardl(DriversKilled ~ asym(PetrolPrice + drivers), data = Seatbelts, maxlag = 1)
  sym_result <- symmetrytest(model)

  # Test all symmetric extraction options
  expect_type(kardl_extract(sym_result, "long_wald_summary"), "list")
  expect_s3_class(
    kardl_extract(
      sym_result,
      "long_hypotheses"
    ),
    "kardl_hypotheses"
  )
  expect_type(kardl_extract(sym_result, "short_wald_summary"), "list")
  expect_s3_class(
    kardl_extract(
      sym_result,
      "short_hypotheses"
    ),
    "kardl_hypotheses"
  )
  expect_type(kardl_extract(sym_result, "long_wald_tests"), "list")
  expect_type(kardl_extract(sym_result, "short_wald_tests"), "list")
  expect_type(kardl_extract(sym_result, "type"), "character")
  expect_type(kardl_extract(sym_result, "call"), "language")
})

test_that("print.kardl_test_summary produces output", {
  kardl_reset()

  model <- kardl(DriversKilled ~ PetrolPrice + drivers, data = Seatbelts, maxlag = 1)
  test_result <- pssf(model, case = 3, sig = "auto")
  test_summ <- summary(test_result)

  # Capture output
  output <- capture.output(print(test_summ))
  expect_true(length(output) > 0)
  expect_true(any(grepl("Decision", output) | grepl("Cointegration", output)))
})

test_that("summary.kardl_symmetric prints correctly", {
  kardl_reset()

  model <- kardl(DriversKilled ~ asym(PetrolPrice), data = Seatbelts, maxlag = 1)
  sym_result <- symmetrytest(model)
  summ <- summary(sym_result, level = 0.05)

  # Print should work
  output <- capture.output(print(sym_result))
  expect_true(length(output) > 0)
})

test_that("lmerge with complex nested structures", {
  a <- list(x = list(y = list(z = 1)), w = 2)
  b <- list(x = list(y = list(z = 999)), p = 3)

  result <- lmerge(a, b)

  # First list takes precedence
  expect_equal(result$x$y$z, 1)
  expect_equal(result$w, 2)
  expect_equal(result$p, 3)
})

test_that("model_criterion with different criteria strings", {
  kardl_reset()

  model <- kardl(DriversKilled ~ PetrolPrice, data = Seatbelts, maxlag = 1)

  # Test all criteria
  aic <- model_criterion(model, "AIC")
  bic <- model_criterion(model, "BIC")
  aicc <- model_criterion(model, "AICc")
  hq <- model_criterion(model, "HQ")

  expect_type(aic, "double")
  expect_type(bic, "double")
  expect_type(aicc, "double")
  expect_type(hq, "double")

  # Different criteria should give different values
  expect_true(aic != bic || aic != hq)
})

test_that("parse_formula_vars handles all formula variations", {
  # Test with 0 instead of -1
  formula1 <- y ~ x + 0
  vars1 <- parse_formula_vars(formula1)
  expect_false(vars1$intercept)

  # Test with multiple asymmetric types
  formula2 <- y ~ asymmetric(a) + Lasymmetric(b) + Sasymmetric(c)
  vars2 <- parse_formula_vars(formula2)
  expect_true("asymmetric" %in% names(vars2$inside))
  expect_true(
    "lasymmetric" %in% names(vars2$inside) ||
      "Lasymmetric" %in% names(vars2$inside)
  )
  expect_true(
    "sasymmetric" %in% names(vars2$inside) ||
      "Sasymmetric" %in% names(vars2$inside)
  )
})

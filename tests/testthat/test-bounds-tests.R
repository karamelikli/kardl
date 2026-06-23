#' Additional tests for bounds testing and decision functions
#'
#' @srrstats {G5.2} Tests exercise decision functions with various input
#' conditions and significance levels.
#' @srrstats {G5.3} Tests verify expected decision outputs and numeric codes.
#' @srrstats {G5.4a} Tests simple deterministic cases for decision rules.

test_that("pssf, psst, and narayan tests work with different cases", {
  kardl_reset()

  model <- kardl(CPI ~ ER + PPI, data = imf_example_data, maxlag = 1)

  # Test pssf with different cases
  for (case_num in c(1, 2, 3, 4, 5)) {
    pssf_result <- pssf(model, case = case_num, sig = "auto")
    expect_s3_class(pssf_result, "htest")
    expect_s3_class(pssf_result, "kardl_test")
    expect_true("statistic" %in% names(pssf_result))
  }

  # Test psst with different cases
  for (case_num in c(1, 3, 5)) {
    psst_result <- psst(model, case = case_num, sig = "auto")
    expect_s3_class(psst_result, "htest")
    expect_s3_class(psst_result, "kardl_test")
  }

  # Test narayan with different cases
  for (case_num in c(2, 3, 4, 5)) {
    narayan_result <- narayan(model, case = case_num, sig = "auto")
    expect_s3_class(narayan_result, "htest")
    expect_s3_class(narayan_result, "kardl_test")
  }
})

test_that("bounds tests work with specific significance levels", {
  kardl_reset()

  model <- kardl(CPI ~ ER + PPI, data = imf_example_data, maxlag = 1)

  # Test pssf with specific significance levels
  for (sig in c("0.10", "0.05", "0.025", "0.01")) {
    pssf_result <- pssf(model, case = 3, sig = sig)
    expect_s3_class(pssf_result, "htest")
    expect_equal(pssf_result$sig, sig)
  }

  # Test psst with specific significance levels
  for (sig in c("0.10", "0.05", "0.025", "0.01")) {
    psst_result <- psst(model, case = 3, sig = sig)
    expect_s3_class(psst_result, "htest")
    expect_equal(psst_result$sig, sig)
  }

  # Test narayan with specific significance levels
  for (sig in c("0.10", "0.05", "0.01")) {
    narayan_result <- narayan(model, case = 2, sig = sig)
    expect_s3_class(narayan_result, "htest")
    expect_equal(narayan_result$sig, sig)
  }
})

test_that("test summaries produce correct decision structures", {
  kardl_reset()

  model <- kardl(CPI ~ ER + PPI, data = imf_example_data, maxlag = 1)

  # Test pssf decision structure
  pssf_result <- pssf(model, case = 3, sig = "auto")
  pssf_summ <- summary(pssf_result)

  expect_true("numeric_decision" %in% names(pssf_summ))
  expect_true(pssf_summ$numeric_decision %in% c(-1, 0, 1))
  expect_type(pssf_summ$decision, "character")
  expect_type(pssf_summ$significance_level, "character")

  # Test psst decision structure
  psst_result <- psst(model, case = 3, sig = "auto")
  psst_summ <- summary(psst_result)

  expect_true("numeric_decision" %in% names(psst_summ))
  expect_true(psst_summ$numeric_decision %in% c(-1, 0, 1))

  # Test narayan decision structure
  narayan_result <- narayan(model, case = 2, sig = "auto")
  narayan_summ <- summary(narayan_result)

  expect_true("numeric_decision" %in% names(narayan_summ))
  expect_true(narayan_summ$numeric_decision %in% c(-1, 0, 1))
})

test_that("hypotheses are generated correctly", {
  kardl_reset()

  model <- kardl(CPI ~ ER + PPI, data = imf_example_data, maxlag = 1)
  pssf_result <- pssf(model, case = 3, sig = "auto")

  # Check hypotheses structure
  expect_s3_class(pssf_result$hypotheses, "kardl_hypotheses")
  expect_true("H0" %in% names(pssf_result$hypotheses))
  expect_true("H1" %in% names(pssf_result$hypotheses))

  # Check hypotheses contain variable names
  expect_true(
    grepl("ER", pssf_result$hypotheses$H0) ||
      grepl("PPI", pssf_result$hypotheses$H0)
  )
})

test_that("critical values are retrieved correctly", {
  kardl_reset()

  model <- kardl(CPI ~ ER + PPI, data = imf_example_data, maxlag = 1)

  # Test pssf critical values
  pssf_result <- pssf(model, case = 3, sig = "auto")
  pssf_summ <- summary(pssf_result)
  cr_vals <- pssf_summ$cr_vals

  expect_s3_class(cr_vals, "data.frame")
  expect_true("L" %in% names(cr_vals))
  expect_true("U" %in% names(cr_vals))
  expect_equal(nrow(cr_vals), 4) # Four significance levels

  # Test psst critical values
  psst_result <- psst(model, case = 3, sig = "auto")
  psst_summ <- summary(psst_result)
  cr_vals_t <- psst_summ$cr_vals

  expect_s3_class(cr_vals_t, "data.frame")
  expect_true("L" %in% names(cr_vals_t))
  expect_true("U" %in% names(cr_vals_t))

  # Test narayan critical values
  narayan_result <- narayan(model, case = 2, sig = "auto")
  narayan_summ <- summary(narayan_result)
  cr_vals_n <- narayan_summ$cr_vals

  expect_s3_class(cr_vals_n, "data.frame")
  expect_true("L" %in% names(cr_vals_n))
  expect_true("U" %in% names(cr_vals_n))
})

test_that("bounds tests handle edge cases", {
  kardl_reset()

  # Test with minimal model
  model_min <- kardl(CPI ~ ER, data = imf_example_data, maxlag = 1)

  pssf_min <- pssf(model_min, case = 3, sig = "auto")
  expect_s3_class(pssf_min, "htest")

  psst_min <- psst(model_min, case = 3, sig = "auto")
  expect_s3_class(psst_min, "htest")

  # Test with more variables
  model_max <- kardl(CPI ~ ER + PPI, data = imf_example_data, maxlag = 2)

  pssf_max <- pssf(model_max, case = 3, sig = "auto")
  expect_s3_class(pssf_max, "htest")
})

test_that("test case labels are correct", {
  kardl_reset()

  model <- kardl(CPI ~ ER + PPI, data = imf_example_data, maxlag = 1)

  # Test that case labels exist and are Roman numerals
  for (case_num in c(1, 2, 3, 4, 5)) {
    pssf_result <- pssf(model, case = case_num, sig = "auto")
    pssf_summ <- summary(pssf_result)

    # Check that case_txt is a Roman numeral
    expect_true(pssf_summ$case_txt %in% c("I", "II", "III", "IV", "V"))
  }
})

test_that("bounds tests preserve model information", {
  kardl_reset()

  model <- kardl(CPI ~ ER + PPI, data = imf_example_data, maxlag = 1)
  pssf_result <- pssf(model, case = 3, sig = "auto")

  # Check that test preserves variable names
  expect_true(length(pssf_result$var_names) > 0)
  expect_type(pssf_result$var_names, "character")

  # Check that test preserves sample size info
  expect_type(pssf_result$n, "double")
  expect_type(pssf_result$k, "double")
  expect_true(pssf_result$n > 0)
  expect_true(pssf_result$k > 0)
})

test_that("decision text is informative", {
  kardl_reset()

  model <- kardl(CPI ~ ER + PPI, data = imf_example_data, maxlag = 1)

  # Test pssf decision text
  pssf_result <- pssf(model, case = 3, sig = "auto")
  pssf_summ <- summary(pssf_result)

  expect_true(nchar(pssf_summ$decision) > 0)
  expect_true(
    grepl("Reject|Fail|Inconclusive", pssf_summ$decision)
  )

  # Test psst decision text
  psst_result <- psst(model, case = 3, sig = "auto")
  psst_summ <- summary(psst_result)

  expect_true(nchar(psst_summ$decision) > 0)
  expect_true(
    grepl("Reject|Fail|Inconclusive", psst_summ$decision)
  )
})

test_that("auto significance selection works correctly", {
  kardl_reset()

  model <- kardl(CPI ~ ER + PPI, data = imf_example_data, maxlag = 1)

  # Test with auto significance
  pssf_auto <- pssf(model, case = 3, sig = "auto")
  pssf_summ <- summary(pssf_auto)

  # Should have selected a specific significance level
  expect_true(
    pssf_summ$significance_level %in% c("0.10", "0.05", "0.025", "0.01")
  )

  # Test with manual significance
  pssf_manual <- pssf(model, case = 3, sig = "0.05")
  pssf_summ_manual <- summary(pssf_manual)

  expect_equal(pssf_summ_manual$significance_level, "0.05")
})

test_that("bounds tests work with asymmetric models", {
  kardl_reset()

  model_asym <- kardl(
    CPI ~ lasymmetric(ER) + PPI,
    data = imf_example_data,
    maxlag = 1
  )

  # All bounds tests should work with asymmetric models
  pssf_asym <- pssf(model_asym, case = 3, sig = "auto")
  expect_s3_class(pssf_asym, "htest")

  psst_asym <- psst(model_asym, case = 3, sig = "auto")
  expect_s3_class(psst_asym, "htest")

  narayan_asym <- narayan(model_asym, case = 2, sig = "auto")
  expect_s3_class(narayan_asym, "htest")
})

test_that("test methods are consistent", {
  kardl_reset()

  model <- kardl(CPI ~ ER + PPI, data = imf_example_data, maxlag = 1)

  # Test that method descriptions are present
  pssf_result <- pssf(model, case = 3, sig = "auto")
  expect_type(pssf_result$method, "character")
  expect_true(nchar(pssf_result$method) > 0)

  psst_result <- psst(model, case = 3, sig = "auto")
  expect_type(psst_result$method, "character")
  expect_true(nchar(psst_result$method) > 0)

  narayan_result <- narayan(model, case = 2, sig = "auto")
  expect_type(narayan_result$method, "character")
  expect_true(nchar(narayan_result$method) > 0)
})

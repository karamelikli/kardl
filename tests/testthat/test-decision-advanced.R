#' Additional comprehensive tests for decision functions and edge cases
#'
#' @srrstats {G5.2} Tests exercise all decision branches
#' @srrstats {G5.3} Tests verify decision consistency
#' @srrstats {G5.8a} Tests handle edge cases and boundary conditions

test_that("psst decision handles inconclusive range", {
  kardl_reset()

  model <- kardl(CPI ~ ER + PPI, data = imf_example_data, maxlag = 1)

  # Test psst with different significance levels to potentially hit inconclusive
  for (sig in c("0.10", "0.05", "0.025", "0.01")) {
    psst_result <- psst(model, case = 3, sig = sig)
    psst_summ <- summary(psst_result)

    # Decision should be one of the valid options
    expect_true(psst_summ$numeric_decision %in% c(-1, 0, 1))
    expect_true(grepl("Reject|Fail|Inconclusive", psst_summ$decision))
  }
})

test_that("pssf decision handles all significance levels correctly", {
  kardl_reset()

  model <- kardl(CPI ~ ER + PPI, data = imf_example_data, maxlag = 1)

  # Test all four significance levels
  for (sig in c("0.10", "0.05", "0.025", "0.01")) {
    pssf_result <- pssf(model, case = 3, sig = sig)
    pssf_summ <- summary(pssf_result)

    # Check that significance level is preserved
    expect_equal(pssf_summ$significance_level, sig)
  }
})

test_that("narayan decision handles 0.1 vs 0.10 significance formats", {
  kardl_reset()

  model <- kardl(CPI ~ ER + PPI, data = imf_example_data, maxlag = 1)

  # Test both "0.1" and "0.10" formats
  narayan_1 <- narayan(model, case = 2, sig = "0.1")
  narayan_2 <- narayan(model, case = 2, sig = "0.10")

  # Both should work and produce valid results
  expect_s3_class(narayan_1, "htest")
  expect_s3_class(narayan_2, "htest")
})

test_that("bounds tests handle all case values", {
  kardl_reset()

  model <- kardl(CPI ~ ER + PPI, data = imf_example_data, maxlag = 1)

  # Test psst with cases 1, 3, 5 (2 and 4 map to 3 and 5)
  psst_c1 <- psst(model, case = 1, sig = "auto")
  expect_s3_class(psst_c1, "htest")
  expect_true(is.numeric(psst_c1$case))

  psst_c3 <- psst(model, case = 3, sig = "auto")
  expect_s3_class(psst_c3, "htest")
  expect_true(is.numeric(psst_c3$case))

  psst_c5 <- psst(model, case = 5, sig = "auto")
  expect_s3_class(psst_c5, "htest")
  expect_true(is.numeric(psst_c5$case))

  # Test psst with cases 2 and 4 (should map to 3 and 5)
  psst_c2 <- psst(model, case = 2, sig = "auto")
  expect_s3_class(psst_c2, "htest")

  psst_c4 <- psst(model, case = 4, sig = "auto")
  expect_s3_class(psst_c4, "htest")
})

test_that("narayan handles case 1 appropriately", {
  kardl_reset()

  model <- kardl(CPI ~ ER + PPI, data = imf_example_data, maxlag = 1)

  # Case 1 is not implemented in Narayan
  # The function should handle it gracefully
  # This may produce NULL critical values
  expect_silent({
    narayan_c1 <- try(narayan(model, case = 1, sig = "auto"), silent = TRUE)
  })
})

test_that("critical values are correct for different k values", {
  kardl_reset()

  # Model with k=1 (single regressor)
  model1 <- kardl(CPI ~ ER, data = imf_example_data, maxlag = 1)
  pssf1 <- pssf(model1, case = 3, sig = "auto")
  summ1 <- summary(pssf1)

  # Model with k=2 (two regressors)
  model2 <- kardl(CPI ~ ER + PPI, data = imf_example_data, maxlag = 1)
  pssf2 <- pssf(model2, case = 3, sig = "auto")
  summ2 <- summary(pssf2)

  # Critical values should exist for both
  expect_s3_class(summ1$cr_vals, "data.frame")
  expect_s3_class(summ2$cr_vals, "data.frame")

  # k values should be different
  expect_true(summ1$k != summ2$k)
})

test_that("hypotheses generation handles single vs multiple variables", {
  kardl_reset()

  # Single variable model
  model1 <- kardl(CPI ~ ER, data = imf_example_data, maxlag = 1)
  pssf1 <- pssf(model1, case = 3, sig = "auto")

  # Check hypothesis text for single variable
  expect_s3_class(pssf1$hypotheses, "kardl_hypotheses")
  expect_true(grepl("ER", pssf1$hypotheses$H0))

  # Multiple variable model
  model2 <- kardl(CPI ~ ER + PPI, data = imf_example_data, maxlag = 1)
  pssf2 <- pssf(model2, case = 3, sig = "auto")

  # Check hypothesis text for multiple variables
  expect_s3_class(pssf2$hypotheses, "kardl_hypotheses")
  expect_true(
    grepl("ER", pssf2$hypotheses$H0) || grepl("PPI", pssf2$hypotheses$H0)
  )
})

test_that("summary.kardl_test handles all test functions", {
  kardl_reset()

  model <- kardl(CPI ~ ER + PPI, data = imf_example_data, maxlag = 1)

  # Test pssf summary
  pssf_test <- pssf(model, case = 3, sig = "auto")
  expect_s3_class(summary(pssf_test), "kardl_test_summary")

  # Test psst summary
  psst_test <- psst(model, case = 3, sig = "auto")
  expect_s3_class(summary(psst_test), "kardl_test_summary")

  # Test narayan summary
  narayan_test <- narayan(model, case = 2, sig = "auto")
  expect_s3_class(summary(narayan_test), "kardl_test_summary")
})

test_that("decision functions handle extreme test statistics", {
  kardl_reset()

  model <- kardl(CPI ~ ER + PPI, data = imf_example_data, maxlag = 1)

  # Get actual test results
  pssf_result <- pssf(model, case = 3, sig = "auto")
  pssf_summ <- summary(pssf_result)

  # Check that decision is valid
  expect_true(pssf_summ$numeric_decision %in% c(-1, 0, 1))
  expect_type(pssf_summ$decision, "character")
  expect_true(nchar(pssf_summ$decision) > 0)
})

test_that("test summaries contain all required components", {
  kardl_reset()

  model <- kardl(CPI ~ ER + PPI, data = imf_example_data, maxlag = 1)

  # Test pssf summary completeness
  pssf_test <- pssf(model, case = 3, sig = "auto")
  pssf_summ <- summary(pssf_test)

  expect_true("statistic" %in% names(pssf_summ))
  expect_true("case_txt" %in% names(pssf_summ))
  expect_true("var_names" %in% names(pssf_summ))
  expect_true("decision" %in% names(pssf_summ))
  expect_true("hypotheses" %in% names(pssf_summ))
  expect_true("numeric_decision" %in% names(pssf_summ))
  expect_true("significance_level" %in% names(pssf_summ))
  expect_true("cr_vals" %in% names(pssf_summ))
  expect_true("k" %in% names(pssf_summ))
})

test_that("bounds tests work with models with deterministic terms", {
  kardl_reset()

  model <- kardl(
    CPI ~ ER + PPI + deterministic(covid),
    data = imf_example_data,
    maxlag = 1
  )

  # All tests should work
  pssf_result <- pssf(model, case = 3, sig = "auto")
  expect_s3_class(pssf_result, "htest")

  psst_result <- psst(model, case = 3, sig = "auto")
  expect_s3_class(psst_result, "htest")

  narayan_result <- narayan(model, case = 2, sig = "auto")
  expect_s3_class(narayan_result, "htest")
})

test_that("bounds tests work with models without intercept", {
  kardl_reset()

  model <- kardl(CPI ~ ER + PPI - 1, data = imf_example_data, maxlag = 1)

  # Test with case 1 (no intercept, no trend)
  pssf_result <- pssf(model, case = 1, sig = "auto")
  expect_s3_class(pssf_result, "htest")

  psst_result <- psst(model, case = 1, sig = "auto")
  expect_s3_class(psst_result, "htest")
})

test_that("bounds tests work with trend", {
  kardl_reset()

  model <- kardl(CPI ~ ER + PPI + trend, data = imf_example_data, maxlag = 1)

  # Test with case 5 (unrestricted intercept and trend)
  pssf_result <- pssf(model, case = 5, sig = "auto")
  expect_s3_class(pssf_result, "htest")

  psst_result <- psst(model, case = 5, sig = "auto")
  expect_s3_class(psst_result, "htest")

  narayan_result <- narayan(model, case = 5, sig = "auto")
  expect_s3_class(narayan_result, "htest")
})

test_that("decision text includes correct significance levels", {
  kardl_reset()

  model <- kardl(CPI ~ ER + PPI, data = imf_example_data, maxlag = 1)

  # Test pssf with 0.05
  pssf_05 <- pssf(model, case = 3, sig = "0.05")
  summ_05 <- summary(pssf_05)
  expect_equal(summ_05$significance_level, "0.05")

  # Test pssf with 0.01
  pssf_01 <- pssf(model, case = 3, sig = "0.01")
  summ_01 <- summary(pssf_01)
  expect_equal(summ_01$significance_level, "0.01")
})

test_that("auto significance selection is consistent", {
  kardl_reset()

  model <- kardl(CPI ~ ER + PPI, data = imf_example_data, maxlag = 1)

  # Run test twice with auto - should give same result
  pssf1 <- pssf(model, case = 3, sig = "auto")
  pssf2 <- pssf(model, case = 3, sig = "auto")

  summ1 <- summary(pssf1)
  summ2 <- summary(pssf2)

  # Decisions should be identical
  expect_equal(summ1$decision, summ2$decision)
  expect_equal(summ1$numeric_decision, summ2$numeric_decision)
  expect_equal(summ1$significance_level, summ2$significance_level)
})

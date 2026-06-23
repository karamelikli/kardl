#' Tests for print and summary methods
#'
#' @srrstats {G5.2} Tests exercise print methods for all major kardl object
#' classes to ensure proper output formatting.
#' @srrstats {G5.3} Tests verify that print methods return their input
#' invisibly as expected.

test_that("print.kardl_mplier works correctly", {
  kardl_reset()

  model <- kardl(CPI ~ ER, data = imf_example_data, maxlag = 1)
  mpl <- mplier(model, horizon = 10)

  # Test that print returns invisibly
  expect_invisible(print(mpl))

  # Test that output contains expected text
  output <- capture.output(print(mpl))
  expect_true(any(grepl("kardl Dynamic Multiplier Object", output)))
  expect_true(any(grepl("Horizon", output)))
})

test_that("summary.kardl_mplier works correctly", {
  kardl_reset()

  model <- kardl(CPI ~ ER, data = imf_example_data, maxlag = 1)
  mpl <- mplier(model, horizon = 10)

  # Test summary method
  summ <- summary(mpl)
  expect_s3_class(summ, "summary.kardl_mplier")
  expect_true("horizon" %in% names(summ))
  expect_equal(summ$horizon, 10)

  # Test print of summary
  expect_invisible(print(summ))
  output <- capture.output(print(summ))
  expect_true(any(grepl("Summary of Dynamic Multipliers", output)))
})

test_that("print.kardl_boot works correctly", {
  kardl_reset()

  model <- kardl(CPI ~ ER, data = imf_example_data, maxlag = 1)
  boot_result <- bootstrap(model, horizon = 5, replications = 10, level = 95)

  # Test that print returns invisibly
  expect_invisible(print(boot_result))

  # Test that output contains expected text
  output <- capture.output(print(boot_result))
  expect_true(any(grepl("kardl Bootstrap Results", output)))
  expect_true(any(grepl("Confidence level", output)))
  expect_true(any(grepl("95", output)))
})

test_that("summary.kardl_boot works correctly", {
  kardl_reset()

  model <- kardl(CPI ~ ER, data = imf_example_data, maxlag = 1)
  boot_result <- bootstrap(model, horizon = 5, replications = 10, level = 95)

  # Test summary method
  summ <- summary(boot_result)
  expect_s3_class(summ, "summary.kardl_boot")
  expect_true("horizon" %in% names(summ))

  # Test print of summary
  expect_invisible(print(summ))
})

test_that("plot.kardl_mplier works correctly", {
  kardl_reset()

  model <- kardl(CPI ~ ER + PPI, data = imf_example_data, maxlag = 1)
  mpl <- mplier(model, horizon = 10)

  # Test plot method with single variable
  expect_silent({
    plots <- plot(mpl, variables = "ER")
  })
  expect_type(plots, "list")

  # Test plot method with "all" variables
  expect_warning(
    plot(mpl, variables = "all"),
    "Multiple variables selected"
  )
})

test_that("print.kardl_hypotheses works correctly", {
  kardl_reset()

  model <- kardl(CPI ~ ER + PPI, data = imf_example_data, maxlag = 1)
  test_result <- pssf(model, case = 3, sig = "auto")

  # Test that hypotheses print correctly
  expect_invisible(print(test_result$hypotheses))

  output <- capture.output(print(test_result$hypotheses))
  expect_true(any(grepl("Hypotheses", output)))
  expect_true(any(grepl("H0", output)))
  expect_true(any(grepl("H1", output)))
})

test_that("print.summary_kardl_longrun works correctly", {
  kardl_reset()

  model <- kardl(CPI ~ ER + PPI, data = imf_example_data, maxlag = 1)
  expect_warning(lr <- kardl_longrun(model))
  summ <- summary(lr)

  # Test that print returns invisibly
  expect_invisible(print(summ))

  # Test that output contains expected text
  output <- capture.output(print(summ))
  expect_true(any(grepl("Call", output)))
  expect_true(any(grepl("Estimation type", output)))
  expect_true(any(grepl("Coefficients", output)))
})

test_that("print.kardl_symmetric works correctly", {
  kardl_reset()

  model <- kardl(CPI ~ asym(ER + PPI), data = imf_example_data, maxlag = 1)
  sym_result <- symmetrytest(model)

  # Test that print returns invisibly
  expect_invisible(print(sym_result))

  # Test that output contains expected text
  output <- capture.output(print(sym_result))
  expect_true(any(grepl("KARDL Symmetry Test Results", output)))
})

test_that("summary.kardl_symmetric works correctly", {
  kardl_reset()

  model <- kardl(CPI ~ asym(ER + PPI), data = imf_example_data, maxlag = 1)
  sym_result <- symmetrytest(model)

  # Test summary at default significance level
  summ <- summary(sym_result)
  expect_type(summ, "list")
  expect_true("decision" %in% names(summ))

  # Test summary at custom significance level
  summ_01 <- summary(sym_result, level = 0.01)
  expect_type(summ_01, "list")
})

test_that("plot method handles invalid variable names", {
  kardl_reset()

  model <- kardl(CPI ~ ER + PPI, data = imf_example_data, maxlag = 1)
  mpl <- mplier(model, horizon = 10)

  # Test error for non-existent variable
  expect_error(
    plot(mpl, variables = "NONEXISTENT"),
    "is not exits among independent variables"
  )
})

test_that("print methods handle empty or minimal objects", {
  kardl_reset()

  # Create minimal model
  model <- kardl(CPI ~ ER, data = imf_example_data, maxlag = 1)
  mpl <- mplier(model, horizon = 3)

  # Test that minimal object still prints
  expect_invisible(print(mpl))
  output <- capture.output(print(mpl))
  expect_true(length(output) > 0)
})

test_that("summary.kardl_test works correctly", {
  kardl_reset()

  model <- kardl(CPI ~ ER + PPI, data = imf_example_data, maxlag = 1)

  # Test pssf summary
  pssf_test <- pssf(model, case = 3, sig = "auto")
  pssf_summ <- summary(pssf_test)
  expect_s3_class(pssf_summ, "kardl_test_summary")
  expect_true("decision" %in% names(pssf_summ))
  expect_true("statistic" %in% names(pssf_summ))
  expect_true("cr_vals" %in% names(pssf_summ))

  # Test psst summary
  psst_test <- psst(model, case = 3, sig = "auto")
  psst_summ <- summary(psst_test)
  expect_s3_class(psst_summ, "kardl_test_summary")

  # Test narayan summary
  narayan_test <- narayan(model, case = 2, sig = "auto")
  narayan_summ <- summary(narayan_test)
  expect_s3_class(narayan_summ, "kardl_test_summary")
})

test_that("summary.kardl_test errors for non-test objects", {
  # Create non-test object
  obj <- list(a = 1, b = 2)
  class(obj) <- "kardl_test"

  expect_error(
    summary(obj),
    "Input object must be of class 'kardl_test' and 'htest'"
  )
})

test_that("summary.kardl_test errors for unsupported test functions", {
  kardl_reset()

  model <- kardl(CPI ~ ER + PPI, data = imf_example_data, maxlag = 1)
  test_result <- pssf(model, case = 3, sig = "auto")

  # Manually change test.func to unsupported value
  test_result$test.func <- "unsupported_test"

  expect_error(
    summary(test_result),
    "only applicable to test objects from"
  )
})

test_that("print methods preserve object structure", {
  kardl_reset()

  model <- kardl(CPI ~ ER, data = imf_example_data, maxlag = 1)
  mpl <- mplier(model, horizon = 5)

  # Capture returned object
  returned <- capture.output(returned_obj <- print(mpl))

  # Check that returned object is identical to input
  expect_identical(returned_obj, mpl)
})

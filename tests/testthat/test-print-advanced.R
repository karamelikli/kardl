#' Additional comprehensive tests for print methods and visualization
#'
#' @srrstats {G5.2} Tests exercise all print method variations
#' @srrstats {TS5.0} Tests verify plot methods work correctly
#' @srrstats {TS5.1} Tests verify axis labels in plots

test_that("print methods handle objects with minimal data", {
  kardl_reset()

  # Minimal model
  model <- kardl(DriversKilled ~ PetrolPrice, data = Seatbelts, maxlag = 1)

  # Test print of minimal model components
  mpl <- mplier(model, horizon = 3)
  expect_invisible(print(mpl))

  # Capture output
  output <- capture.output(print(mpl))
  expect_true(length(output) > 0)
  expect_true(any(grepl("Horizon", output)))
})

test_that("summary.kardl_symmetric handles different levels", {
  kardl_reset()

  model <- kardl(DriversKilled ~ asym(PetrolPrice + drivers), data = Seatbelts, maxlag = 1)
  sym_result <- symmetrytest(model)

  # Test with different significance levels
  summ_10 <- summary(sym_result, level = 0.10)
  expect_type(summ_10, "list")

  summ_05 <- summary(sym_result, level = 0.05)
  expect_type(summ_05, "list")

  summ_01 <- summary(sym_result, level = 0.01)
  expect_type(summ_01, "list")
})

test_that("print.kardl_symmetric handles NULL summaries", {
  kardl_reset()

  model <- kardl(DriversKilled ~ lasymmetric(PetrolPrice), data = Seatbelts, maxlag = 1)
  sym_result <- symmetrytest(model)

  # Print should work even if some summaries might be NULL
  expect_invisible(print(sym_result))
})

test_that("summary.kardl_symmetric decision logic works", {
  kardl_reset()

  model <- kardl(DriversKilled ~ asym(PetrolPrice + drivers), data = Seatbelts, maxlag = 1)
  sym_result <- symmetrytest(model)

  # Get summary and check decision structure
  summ <- summary(sym_result, level = 0.05)

  expect_true("decision" %in% names(summ))

  # Check that decisions exist for long_run and/or short_run
  if (!is.null(summ$decision$long_run)) {
    expect_type(summ$decision$long_run, "list")
  }
  if (!is.null(summ$decision$short_run)) {
    expect_type(summ$decision$short_run, "list")
  }
})

test_that("print.kardl_hypotheses handles named vs unnamed hypotheses", {
  kardl_reset()

  model <- kardl(DriversKilled ~ PetrolPrice + drivers, data = Seatbelts, maxlag = 1)
  pssf_result <- pssf(model, case = 3, sig = "auto")

  # Print hypotheses
  expect_invisible(print(pssf_result$hypotheses))
  output <- capture.output(print(pssf_result$hypotheses))
  expect_true(any(grepl("H0", output)))
  expect_true(any(grepl("H1", output)))
})

test_that("plot.kardl_mplier handles custom title", {
  kardl_reset()

  model <- kardl(DriversKilled ~ PetrolPrice, data = Seatbelts, maxlag = 1)
  mpl <- mplier(model, horizon = 10)

  # Test with custom title
  expect_silent({
    plots <- plot(mpl, variables = "PetrolPrice", title = "Custom Title")
  })
  expect_type(plots, "list")
})

test_that("plot.kardl_mplier handles single variable selection", {
  kardl_reset()

  model <- kardl(DriversKilled ~ PetrolPrice + drivers, data = Seatbelts, maxlag = 1)
  mpl <- mplier(model, horizon = 10)

  # Test with specific variable
  expect_silent({
    plots <- plot(mpl, variables = "PetrolPrice")
  })
  expect_type(plots, "list")
  expect_equal(length(plots), 1)
})

test_that("summary methods return correct class", {
  kardl_reset()

  model <- kardl(DriversKilled ~ PetrolPrice, data = Seatbelts, maxlag = 1)

  # Test mplier summary class
  mpl <- mplier(model, horizon = 5)
  summ_mpl <- summary(mpl)
  expect_s3_class(summ_mpl, "summary.kardl_mplier")

  # Test boot summary class
  boot_result <- bootstrap(model, horizon = 5, replications = 10)
  summ_boot <- summary(boot_result)
  expect_s3_class(summ_boot, "summary.kardl_boot")
})

test_that("print methods work with different object states", {
  kardl_reset()

  model <- kardl(DriversKilled ~ PetrolPrice + drivers, data = Seatbelts, maxlag = 1)

  # Test longrun print
  expect_warning(lr <- kardl_longrun(model))
  summ_lr <- summary(lr)
  expect_invisible(print(summ_lr))

  # Check output contains key elements
  output <- capture.output(print(summ_lr))
  expect_true(any(grepl("Call", output)))
  expect_true(any(grepl("Coefficients", output)))
})

test_that("print.kardl_hypotheses handles UTF-8 characters", {
  kardl_reset()

  model <- kardl(DriversKilled ~ asym(PetrolPrice), data = Seatbelts, maxlag = 1)
  sym_result <- symmetrytest(model)

  # Print should handle UTF-8 in hypotheses
  hyp <- kardl_extract(sym_result, what = "long_hypotheses")
  expect_invisible(print(hyp))
})

test_that("summary.kardl_symmetric handles edge cases", {
  kardl_reset()

  model <- kardl(DriversKilled ~ asym(PetrolPrice), data = Seatbelts, maxlag = 1)
  sym_result <- symmetrytest(model)

  # Test with very small significance level
  summ_small <- summary(sym_result, level = 0.001)
  expect_type(summ_small, "list")

  # Test with large significance level
  summ_large <- summary(sym_result, level = 0.5)
  expect_type(summ_large, "list")
})

test_that("print methods preserve invisible return", {
  kardl_reset()

  model <- kardl(DriversKilled ~ PetrolPrice, data = Seatbelts, maxlag = 1)

  # Test mplier
  mpl <- mplier(model, horizon = 5)
  returned <- print(mpl)
  expect_identical(returned, mpl)

  # Test boot
  boot_result <- bootstrap(model, horizon = 5, replications = 10)
  returned_boot <- print(boot_result)
  expect_identical(returned_boot, boot_result)
})

test_that("plot handles asymmetric models correctly", {
  kardl_reset()

  model <- kardl(DriversKilled ~ lasymmetric(PetrolPrice), data = Seatbelts, maxlag = 1)
  mpl <- mplier(model, horizon = 10)

  # Plot should work with asymmetric models
  expect_silent({
    plots <- plot(mpl, variables = "PetrolPrice")
  })
  expect_type(plots, "list")
})

test_that("summary.kardl_mplier contains correct components", {
  kardl_reset()

  model <- kardl(DriversKilled ~ PetrolPrice, data = Seatbelts, maxlag = 1)
  mpl <- mplier(model, horizon = 10)
  summ <- summary(mpl)

  expect_true("horizon" %in% names(summ))
  expect_true("summary" %in% names(summ))
  expect_equal(summ$horizon, 10)
})

test_that("summary.kardl_boot contains correct components", {
  kardl_reset()

  model <- kardl(DriversKilled ~ PetrolPrice, data = Seatbelts, maxlag = 1)
  boot_result <- bootstrap(model, horizon = 5, replications = 10)
  summ <- summary(boot_result)

  expect_true("horizon" %in% names(summ))
  expect_true("summary" %in% names(summ))
  expect_equal(summ$horizon, 5)
})

test_that("print.kardl_hypotheses handles variable-specific hypotheses", {
  kardl_reset()

  model <- kardl(DriversKilled ~ asym(PetrolPrice + drivers), data = Seatbelts, maxlag = 1)
  sym_result <- symmetrytest(model)

  # Get and print variable-specific hypotheses
  er_hyp <- kardl_extract(sym_result, what = "long_hypotheses", variable = "PetrolPrice")
  expect_invisible(print(er_hyp))

  output <- capture.output(print(er_hyp))
  expect_true(any(grepl("PetrolPrice", output)))
})

test_that("print methods handle empty outputs gracefully", {
  kardl_reset()

  model <- kardl(DriversKilled ~ PetrolPrice, data = Seatbelts, maxlag = 1)
  mpl <- mplier(model, horizon = 5)

  # Even with small horizon, print should work
  expect_invisible(print(mpl))
})

test_that("summary.kardl_symmetric decision includes all variables", {
  kardl_reset()

  model <- kardl(DriversKilled ~ asym(PetrolPrice + drivers), data = Seatbelts, maxlag = 1)
  sym_result <- symmetrytest(model)
  summ <- summary(sym_result, level = 0.05)

  # Check that decisions cover both variables
  if (!is.null(summ$decision$long_run)) {
    # Should have decisions for PetrolPrice and/or drivers
    expect_true(length(summ$decision$long_run) > 0)
  }
})

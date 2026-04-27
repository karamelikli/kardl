#' @srrstats {G5.2} Tests exercise successful calls and error conditions for
#' print, summary, and plot S3 methods across all kardl object classes.
#' @srrstats {G5.3} Tests verify expected output strings, class labels, and
#' component values for summary and print methods.


local_models <- local({
  kardl_reset()
  linear <- kardl(imf_example_data, CPI ~ ER + PPI + trend,
                  mode = c(1, 1, 1))
  asym <- kardl(imf_example_data, CPI ~ ER + PPI + asymmetric(ER) + trend,
                mode = c(1, 1, 1, 1))
  list(linear = linear, asym = asym)
})


#' @srrstats {G5.3} Checks that `print.kardl_lm()` outputs the lag summary
#' line for both linear and asymmetric model objects.
test_that("print.kardl_lm produces expected output", {
  expect_output(print(local_models$linear), "Optimal lags")
  expect_output(print(local_models$asym),   "Optimal lags")
})


  #' @srrstats {G5.3} Checks that `kardl_longrun()` returns a `kardl_longrun`
  #' object with a populated summary table containing estimation type and
  #' coefficient labels.
test_that("summary_kardl_longrun works (class is kardl_longrun)", {
  lr <- kardl_longrun(local_models$linear)
  # The actual class assigned in longrun.R is "kardl_longrun"
  expect_s3_class(lr, "kardl_longrun")

  # summary dispatches correctly via summary.kardl_longrun
  sm <- summary(lr)
  expect_s3_class(sm, "summary_kardl_longrun")
  expect_false(is.null(sm$coefficients))

  expect_output(print(sm), "Estimation type")
  expect_output(print(sm), "Coefficients")
})

#' @srrstats {G5.3} Checks that print and summary for `kardl_symmetric`
#' produce the correct class and output strings for the F-test variant.
test_that("print and summary of kardl_symmetric (type F) work", {
  st <- symmetrytest(local_models$asym)

  # print.kardl_symmetric
  expect_output(print(st))

  # summary.kardl_symmetric + print.summary.kardl_symmetric
  sm <- summary(st)
  expect_s3_class(sm, "summary.kardl_symmetric")
  expect_output(print(sm), "Long-run symmetry tests")
})

#' @srrstats {G5.7} Tests that the `level` argument is accepted and does not
#' alter the returned class.
test_that("summary.kardl_symmetric with a custom level works", {
  st <- symmetrytest(local_models$asym)
  sm_strict <- summary(st, level = 0.01)
  expect_s3_class(sm_strict, "summary.kardl_symmetric")
})

#' @srrstats {G5.7} Tests chi-squared variant of the symmetry test summary.
test_that("print and summary of kardl_symmetric (type Chisq) work", {
  st_chi <- symmetrytest(local_models$asym, type = "Chisq")
  expect_output(print(st_chi))

  sm_chi <- summary(st_chi)
  expect_s3_class(sm_chi, "summary.kardl_symmetric")
  expect_output(print(sm_chi), "Long-run symmetry tests")
})

#' @srrstats {G5.7} Confirms that short-run-only symmetry test results
#' print correctly with the expected heading.
test_that("print.kardl_symmetric handles short-run-only results", {
  kardl_reset()
  sasym_model <- kardl(imf_example_data, CPI ~ PPI + sasymmetric(ER),
                       mode = c(1, 1, 1, 1))
  st_sr <- symmetrytest(sasym_model, component = "shortrun")
  expect_output(print(st_sr))

  sm_sr <- summary(st_sr)
  expect_output(print(sm_sr), "Short-run symmetry tests")
})

#' @srrstats {G5.3} Checks that `summary()` on a `pssf()` result returns
#' a `summary_htest` object containing critical values.
test_that("summary.kardl_test dispatches correctly for pssf", {
  pf <- pssf(local_models$linear)
  sm <- summary(pf)
  expect_s3_class(sm, "summary_htest")
  expect_output(print(sm), "Critical Values")
})

#' @srrstats {G5.3} Checks that `summary()` on a `psst()` result returns
#' a `summary_htest` object containing critical values.
test_that("summary.kardl_test dispatches correctly for psst", {
  pt <- psst(local_models$linear)
  sm <- summary(pt)
  expect_s3_class(sm, "summary_htest")
  expect_output(print(sm), "Critical Values")
})

#' @srrstats {G5.3} Checks that `summary()` on a `narayan()` result returns
#' a `summary_htest` object containing critical values.
test_that("summary.kardl_test dispatches correctly for narayan", {
  nr <- narayan(local_models$linear)
  sm <- summary(nr)
  expect_s3_class(sm, "summary_htest")
  expect_output(print(sm), "Critical Values")
})

#' @srrstats {G5.3} Checks print output headings and summary class for
#' `kardl_mplier` objects.
#' @srrstats {TS4.3} Confirms the horizon value is echoed in the print output.
test_that("print and summary for kardl_mplier work", {
  mp <- mplier(local_models$asym, horizon = 10)

  expect_output(print(mp), "kardl Dynamic Multiplier Object")
  expect_output(print(mp), "Horizon: 10")

  sm <- summary(mp)
  expect_s3_class(sm, "summary.kardl_mplier")
  expect_output(print(sm), "Summary of Dynamic Multipliers")
  expect_output(print(sm), "Horizon: 10")
})

#' @srrstats {G5.3} Checks print output and summary class for `kardl_boot`.
#' @srrstats {G5.5} Bootstrap replications use the random residual resampling
#' method; this test confirms that results are returned correctly.
test_that("print and summary for kardl_boot work", {
  kardl_reset()
  asym_only <- kardl(imf_example_data, CPI ~ asymmetric(ER),
                     mode = c(1, 1, 1))
  bt <- bootstrap(asym_only, horizon = 10, replications = 5, level = 90)

  expect_s3_class(bt, "kardl_boot")
  expect_output(print(bt), "kardl Bootstrap Results")
  expect_output(print(bt), "Confidence level: 90")

  sm <- summary(bt)
  expect_s3_class(sm, "summary.kardl_boot")
  expect_output(print(sm), "Summary of Dynamic Multipliers")
})

#' @srrstats {TS5.0} Confirms that `plot.kardl_mplier()` runs without error
#' for both default (all variables) and single-variable calls.
#' @srrstats {TS5.8} Verifies that variable-specific plots can be requested.
test_that("plot.kardl_mplier runs without error", {
  mp <- mplier(local_models$asym, horizon = 5)
  expect_no_error(plot(mp))
  expect_no_error(plot(mp, variables = "ER"))
})

#' @srrstats {G5.2a} Checks that requesting an unknown variable in `plot()`
#' produces a warning rather than a silent failure.
test_that("plot.kardl_mplier warns for unknown variable", {
  mp <- mplier(local_models$asym, horizon = 5)
  expect_warning(plot(mp, variables = "NOTAVAR"), "not exits")
})

#' @srrstats {TS5.5} Confirms that plot methods operate on computed model
#' outputs without error.
test_that("plot on kardl_longrun runs without error", {
  # Note: class is "kardl_longrun", so plot.lm is dispatched (not plot.kardl_long_run)
  lr <- kardl_longrun(local_models$linear)
  expect_no_error(plot(lr, which = 1))
})

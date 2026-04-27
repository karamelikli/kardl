
#' @srrstats {TS4.3} Verifies that the horizon index is stored in the result.
#' @srrstats {G5.2} The test suite exercises representative successful calls
#' and selected error conditions for post-estimation tests, multipliers, and
#' bootstrap confidence intervals.
#' @srrstats {G5.3} Tests verify expected classes, dimensions, and component
#' values for all post-estimation objects.
#' @srrstats {G5.4} Correctness tests compare selected outputs against
#' expected structural properties of ARDL and NARDL models.
test_that("Post-estimation tests run correctly", {
  kardl_reset()
  formula <- CPI ~ ER + PPI + trend

  model <- kardl(data = imf_example_data, formula = formula, mode = "quick", maxlag = 2)

  #' Test kardl_longrun
  lr <- kardl_longrun(model)
  expect_s3_class(lr, c("kardl_long_run","lm"))
  expect_gt(length(lr$coef), 0)

  #' Test symmetrytest
  symm_model <- kardl(data = imf_example_data, formula = CPI ~ ER + PPI + asymmetric(ER) + trend, mode = "quick", maxlag = 2)
  st <- symmetrytest(symm_model)
  expect_s3_class(st, c("kardl_symmetric","list"))
  expect_s3_class(st$Lwald, c("anova","data.frame"))
  expect_identical(st$Lwald$Df, 1L)

  #' Test pssf
  pf <- pssf(model, case = 3, signif_level = "auto")
  expect_s3_class(pf,c("kardl_test", "htest" ))
  expect_identical(pf$type, "cointegration")
  expect_false(is.null(pf$statistic))

  #' Test psst
  pt <- psst(model, case = 3, signif_level = "auto")
  expect_s3_class(pt,c("kardl_test", "htest" ))
  expect_identical(pt$type, "cointegration")
  expect_false(is.null(pt$statistic))

  #' Test narayan
  nr <- narayan(model, case = 3, signif_level = "auto")
  expect_s3_class(nr,c("kardl_test", "htest" ))
  expect_identical(nr$type, "cointegration")
  expect_false(is.null(nr$statistic))

  #' Test mplier
  mp <- mplier(symm_model,horizon = 38)
  expect_s3_class(mp, "kardl_mplier" )
  expect_identical(mp$horizon, 38)
  expect_false(is.null(mp$mpsi))

  #' Test ecm
  ec <- ecm(data = imf_example_data, formula = formula, mode = "quick", case = 3, signif_level = "auto")
  expect_s3_class(ec, c("kardl_lm", "lm"))
})

#' @srrstats {G5.7} Tests symmetry test when restricted to the long-run
#' component, confirming Swald is NULL and Lwald is populated.
test_that("symmetrytest with component = 'longrun' only works", {
  kardl_reset()
  model <- kardl(imf_example_data, CPI ~ ER + PPI + asymmetric(ER) + trend,
                 mode = c(1, 1, 1, 1))
  st <- symmetrytest(model, component = "longrun")
  expect_s3_class(st, "kardl_symmetric")
  expect_false(is.null(st$Lwald))
  expect_null(st$Swald)
})
#' @srrstats {G5.7} Tests symmetry test when restricted to the short-run
#' component, confirming Lwald is NULL and Swald is populated.
test_that("symmetrytest with component = 'shortrun' only works", {

  kardl_reset()
  model <- kardl(imf_example_data, CPI ~ PPI + sasymmetric(ER),
                 mode = c(1, 1, 1, 1))
  st <- symmetrytest(model, component = "shortrun")
  expect_s3_class(st, "kardl_symmetric")
  expect_null(st$Lwald)
  expect_false(is.null(st$Swald))
})
#' @srrstats {G5.7} Tests that chi-squared test type produces a Pr(>Chisq)
#' column in the Lwald table.
test_that("symmetrytest with type = 'Chisq' works", {

  kardl_reset()
  model <- kardl(imf_example_data, CPI ~ ER + PPI + asymmetric(ER) + trend,
                 mode = c(1, 1, 1, 1))
  st <- symmetrytest(model, type = "Chisq")
  expect_s3_class(st, "kardl_symmetric")
  expect_identical(st$type, "Chisq")
  expect_true("Pr(>Chisq)" %in% colnames(st$Lwald))
})
#' @srrstats {G5.7} Confirms that when `vars` is restricted to a single
#' variable, only that variable appears in the Lwald row names.
test_that("symmetrytest with specific vars argument works", {

  kardl_reset()
  model <- kardl(imf_example_data, CPI ~ ER + PPI + asymmetric(ER) + trend,
                 mode = c(1, 1, 1, 1))
  st <- symmetrytest(model, vars = "ER")
  expect_s3_class(st, "kardl_symmetric")
  expect_identical(rownames(st$Lwald), "ER")
})
#' @srrstats {G5.2b} Checks that a variable name not in the model produces
#' an informative error.
#' @srrstats {G5.8} Verifies edge-condition handling for invalid variable selection.
test_that("symmetrytest errors on bad vars", {

  kardl_reset()
  model <- kardl(imf_example_data, CPI ~ ER + PPI + asymmetric(ER) + trend,
                 mode = c(1, 1, 1, 1))
  expect_error(symmetrytest(model, vars = "NOTAVAR"))
})
#' @srrstats {G5.2b} Confirms that passing an ECM object to `symmetrytest()`
#' triggers an informative error.
test_that("symmetrytest errors on ECM model", {

  kardl_reset()
  ec <- ecm(imf_example_data, CPI ~ ER + PPI + trend, mode = c(1, 1, 1))
  expect_error(symmetrytest(ec), "ECM")
})

#' @srrstats {G5.2b} Checks that a plain `lm` object is rejected with an error.
test_that("symmetrytest errors on non-kardl_lm input", {
  expect_error(symmetrytest(lm(mpg ~ wt, data = mtcars)))
})

#' @srrstats {G5.7} Tests `pssf()` with an explicitly specified significance
#' level, confirming both test and summary objects are returned correctly.
test_that("pssf with specific signif_level works", {
  kardl_reset()
  model <- kardl(imf_example_data, CPI ~ ER + PPI + trend, mode = c(1, 1, 1))
  pf <- pssf(model, signif_level = "0.05")
  expect_s3_class(pf, "kardl_test")
  sm <- summary(pf)
  expect_s3_class(sm, "summary_htest")
})

#' @srrstats {G5.7} Tests that a numeric significance level is accepted and
#' coerced to a character value internally.
test_that("pssf with numeric signif_level works", {
  kardl_reset()
  model <- kardl(imf_example_data, CPI ~ ER + PPI + trend, mode = c(1, 1, 1))
  pf <- pssf(model, signif_level = 0.05)
  expect_s3_class(pf, "kardl_test")
})

#' @srrstats {G5.7} Tests `pssf()` under the unrestricted-intercept-and-trend
#' case specification.
test_that("pssf with case = 5 (trend model) works", {
  kardl_reset()
  model <- kardl(imf_example_data, CPI ~ ER + PPI + trend, mode = c(1, 1, 1))
  pf <- pssf(model, case = 5)
  expect_s3_class(pf, "kardl_test")
})

#' @srrstats {G5.2b} Checks that passing an ECM object to `pssf()` produces
#' an informative error.
test_that("pssf errors on ECM model", {
  kardl_reset()
  ec <- ecm(imf_example_data, CPI ~ ER + PPI + trend, mode = c(1, 1, 1))
  expect_error(pssf(ec))
})

#' @srrstats {G5.8} Checks that an out-of-range case value is rejected.
test_that("pssf errors on invalid case", {
  kardl_reset()
  model <- kardl(imf_example_data, CPI ~ ER + PPI + trend, mode = c(1, 1, 1))
  expect_error(pssf(model, case = 99))
})

#' @srrstats {G5.8} Checks that an unsupported significance level string
#' is rejected before the test is performed.
test_that("pssf errors on invalid signif_level", {
  kardl_reset()
  model <- kardl(imf_example_data, CPI ~ ER + PPI + trend, mode = c(1, 1, 1))
  expect_error(pssf(model, signif_level = "0.03"))
})

#' @srrstats {G5.7} Tests the PSS t-test on an ECM object where the
#' error-correction residual is the test variable.
test_that("psst with ECM model works (uses EcmRes as test var)", {
  kardl_reset()
  ec <- ecm(imf_example_data, CPI ~ ER + PPI + trend, mode = c(1, 1, 1))
  pt <- psst(ec)
  expect_s3_class(pt, "kardl_test")
  sm <- summary(pt)
  expect_s3_class(sm, "summary_htest")
})

#' @srrstats {G5.4} Checks that the case auto-adjustment logic correctly
#' detects a trend term and elevates the case to 5.
test_that("psst with trend model auto-adjusts to case 5", {
  kardl_reset()
  model <- kardl(imf_example_data, CPI ~ ER + PPI + trend, mode = c(1, 1, 1))
  pt <- psst(model, case = 3)
  expect_s3_class(pt, "kardl_test")
  expect_identical(pt$case, 5)  #' auto-adjusted because model has trend
})

#' @srrstats {G5.8} Verifies that an invalid case value is rejected.
test_that("psst errors on invalid case", {
  kardl_reset()
  model <- kardl(imf_example_data, CPI ~ ER + PPI + trend, mode = c(1, 1, 1))
  expect_error(psst(model, case = 99))
})

#' @srrstats {G5.7} Tests the Narayan small-sample bounds test under case 5.
test_that("narayan with case = 5 works", {
  kardl_reset()
  model <- kardl(imf_example_data, CPI ~ ER + PPI + trend, mode = c(1, 1, 1))
  nr <- narayan(model, case = 5)
  expect_s3_class(nr, "kardl_test")
})

#' @srrstats {G5.7} Tests `narayan()` with an explicit 1% significance level
#' and confirms that `summary()` dispatches correctly.
test_that("narayan with specific signif_level works", {
  kardl_reset()
  model <- kardl(imf_example_data, CPI ~ ER + PPI, mode = c(1, 1, 1))
  nr <- narayan(model, signif_level = "0.01")
  expect_s3_class(nr, "kardl_test")
  sm <- summary(nr)
  expect_s3_class(sm, "summary_htest")
})

#' @srrstats {G5.2b} Checks that passing an ECM object to `narayan()` is rejected.
test_that("narayan errors on ECM model", {
  kardl_reset()
  ec <- ecm(imf_example_data, CPI ~ ER + PPI + trend, mode = c(1, 1, 1))
  expect_error(narayan(ec))
})

#' @srrstats {G5.8} Verifies that case = 1 (unsupported by Narayan) is rejected.
test_that("narayan errors on invalid case", {
  kardl_reset()
  model <- kardl(imf_example_data, CPI ~ ER + PPI, mode = c(1, 1, 1))
  expect_error(narayan(model, case = 1))
})

#' @srrstats {G5.7} Tests that the `minProb` threshold filters coefficients
#' and returns a `kardl_mplier` object with the correct number of rows.
#' @srrstats {TS4.3} Verifies the horizon index dimension of the returned mpsi matrix.
test_that("mplier with minProb > 0 works", {
  kardl_reset()
  model <- kardl(imf_example_data, CPI ~ ER + PPI + asymmetric(ER) + trend,
                 mode = c(1, 1, 1, 1))
  mp <- mplier(model, horizon = 10, minProb = 0.1)
  expect_s3_class(mp, "kardl_mplier")
  expect_identical(nrow(mp$mpsi), 11L)
})

#' @srrstats {G5.2b} Checks that a plain `lm` object is rejected by `mplier()`.
test_that("mplier errors on non-kardl_lm input", {
  expect_error(mplier(lm(mpg ~ wt, data = mtcars)))
})

#' @srrstats {G5.7} Tests `mplier()` on a long-run-asymmetric (SA) model.
test_that("mplier with lasymmetric model works", {
  kardl_reset()
  model <- kardl(imf_example_data, CPI ~ lasymmetric(ER), mode = c(1, 1))
  mp <- mplier(model, horizon = 10)
  expect_s3_class(mp, "kardl_mplier")
})

#' @srrstats {G5.7} Tests `mplier()` on a short-run-asymmetric (AS) model.
test_that("mplier with sasymmetric model works", {
  kardl_reset()
  model <- kardl(imf_example_data, CPI ~ PPI + sasymmetric(ER),
                 mode = c(1, 1, 1, 1))
  mp <- mplier(model, horizon = 10)
  expect_s3_class(mp, "kardl_mplier")
})



#' @srrstats {G5.5} The bootstrap resampling uses a random seed approach
#' (set externally before test runs under `testthat`).
#' @srrstats {G5.3} Checks that the returned `kardl_boot` object has the
#' correct class, level, horizon, and a non-NULL `mpsi` component.
test_that("bootstrap with asymmetric model works", {
  kardl_reset()
  model <- kardl(imf_example_data, CPI ~ asymmetric(ER),
                 mode = c(1, 1, 1))
  bt <- bootstrap(model, horizon = 10, replications = 5, level = 95)
  expect_s3_class(bt, "kardl_boot")
  expect_identical(bt$level, 95)
  expect_identical(bt$horizon, 10)
  expect_false(is.null(bt$mpsi))
})

#' @srrstats {G5.2b} Confirms that a plain `lm` object is rejected by `bootstrap()`.
test_that("bootstrap errors on non-kardl_lm input", {
  expect_error(bootstrap(lm(mpg ~ wt, data = mtcars)))
})

#' @srrstats {G5.7} Tests that `bootstrap()` runs on a linear (non-asymmetric)
#' model without attempting to compute confidence interval columns.
test_that("bootstrap with linear model (no CI columns) still works", {
  kardl_reset()
  model <- kardl(imf_example_data, CPI ~ ER + PPI, mode = c(1, 1, 1))
  bt <- bootstrap(model, horizon = 5, replications = 3)
  expect_s3_class(bt, "kardl_boot")
})

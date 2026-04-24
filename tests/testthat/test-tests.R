test_that("Post-estimation tests run correctly", {
  kardl_reset()
  formula <- CPI ~ ER + PPI + trend

  model <- kardl(data = imf_example_data, formula = formula, mode = "quick", maxlag = 2)

  # Test kardl_longrun
  lr <- kardl_longrun(model)
  expect_s3_class(lr, c("kardl_long_run","lm"))
  expect_gt(length(lr$coef), 0)

  # Test symmetrytest
  # Need an asymmetric model for meaningful symmetry test, but function should run anyway
  symm_model <- kardl(data = imf_example_data, formula = CPI ~ ER + PPI + asymmetric(ER) + trend, mode = "quick", maxlag = 2)
  st <- symmetrytest(symm_model)
  expect_s3_class(st, c("kardl_symmetric","list"))
  expect_s3_class(st$Lwald, c("anova","data.frame"))
  expect_identical(st$Lwald$Df, 1L)

  # Test pssf
  pf <- pssf(model, case = 3, signif_level = "auto")
  expect_s3_class(pf,c("kardl_test", "htest" ))
  expect_identical(pf$type, "cointegration")
  expect_false(is.null(pf$statistic))

  # Test psst
  pt <- psst(model, case = 3, signif_level = "auto")
  expect_s3_class(pt,c("kardl_test", "htest" ))
  expect_identical(pt$type, "cointegration")
  expect_false(is.null(pt$statistic))

  # Test narayan
  nr <- narayan(model, case = 3, signif_level = "auto")
  expect_s3_class(nr,c("kardl_test", "htest" ))
  expect_identical(nr$type, "cointegration")
  expect_false(is.null(nr$statistic))

  # Test mplier
  mp <- mplier(symm_model,horizon = 38)
  expect_s3_class(mp, "kardl_mplier" )
  expect_identical(mp$horizon, 38)
  expect_false(is.null(mp$mpsi))

  # Test ecm
  ec <- ecm(data = imf_example_data, formula = formula, mode = "quick", case = 3, signif_level = "auto")
  expect_s3_class(ec, c("kardl_lm", "lm"))
})

# ── symmetrytest variations ───────────────────────────────────────────────────
test_that("symmetrytest with component = 'longrun' only works", {
  kardl_reset()
  model <- kardl(imf_example_data, CPI ~ ER + PPI + asymmetric(ER) + trend,
                 mode = c(1, 1, 1, 1))
  st <- symmetrytest(model, component = "longrun")
  expect_s3_class(st, "kardl_symmetric")
  expect_false(is.null(st$Lwald))
  expect_null(st$Swald)
})

test_that("symmetrytest with component = 'shortrun' only works", {
  kardl_reset()
  model <- kardl(imf_example_data, CPI ~ PPI + sasymmetric(ER),
                 mode = c(1, 1, 1, 1))
  st <- symmetrytest(model, component = "shortrun")
  expect_s3_class(st, "kardl_symmetric")
  expect_null(st$Lwald)
  expect_false(is.null(st$Swald))
})

test_that("symmetrytest with type = 'Chisq' works", {
  kardl_reset()
  model <- kardl(imf_example_data, CPI ~ ER + PPI + asymmetric(ER) + trend,
                 mode = c(1, 1, 1, 1))
  st <- symmetrytest(model, type = "Chisq")
  expect_s3_class(st, "kardl_symmetric")
  expect_identical(st$type, "Chisq")
  expect_true("Pr(>Chisq)" %in% colnames(st$Lwald))
})

test_that("symmetrytest with specific vars argument works", {
  kardl_reset()
  model <- kardl(imf_example_data, CPI ~ ER + PPI + asymmetric(ER) + trend,
                 mode = c(1, 1, 1, 1))
  st <- symmetrytest(model, vars = "ER")
  expect_s3_class(st, "kardl_symmetric")
  expect_identical(rownames(st$Lwald), "ER")
})

test_that("symmetrytest errors on bad vars", {
  kardl_reset()
  model <- kardl(imf_example_data, CPI ~ ER + PPI + asymmetric(ER) + trend,
                 mode = c(1, 1, 1, 1))
  expect_error(symmetrytest(model, vars = "NOTAVAR"))
})

test_that("symmetrytest errors on ECM model", {
  kardl_reset()
  ec <- ecm(imf_example_data, CPI ~ ER + PPI + trend, mode = c(1, 1, 1))
  expect_error(symmetrytest(ec), "ECM")
})

test_that("symmetrytest errors on non-kardl_lm input", {
  expect_error(symmetrytest(lm(mpg ~ wt, data = mtcars)))
})

# ── pssf variations ──────────────────────────────────────────────────────────
test_that("pssf with specific signif_level works", {
  kardl_reset()
  model <- kardl(imf_example_data, CPI ~ ER + PPI + trend, mode = c(1, 1, 1))
  pf <- pssf(model, signif_level = "0.05")
  expect_s3_class(pf, "kardl_test")
  sm <- summary(pf)
  expect_s3_class(sm, "summary_htest")
})

test_that("pssf with numeric signif_level works", {
  kardl_reset()
  model <- kardl(imf_example_data, CPI ~ ER + PPI + trend, mode = c(1, 1, 1))
  pf <- pssf(model, signif_level = 0.05)
  expect_s3_class(pf, "kardl_test")
})

test_that("pssf with case = 5 (trend model) works", {
  kardl_reset()
  model <- kardl(imf_example_data, CPI ~ ER + PPI + trend, mode = c(1, 1, 1))
  pf <- pssf(model, case = 5)
  expect_s3_class(pf, "kardl_test")
})

test_that("pssf errors on ECM model", {
  kardl_reset()
  ec <- ecm(imf_example_data, CPI ~ ER + PPI + trend, mode = c(1, 1, 1))
  expect_error(pssf(ec))
})

test_that("pssf errors on invalid case", {
  kardl_reset()
  model <- kardl(imf_example_data, CPI ~ ER + PPI + trend, mode = c(1, 1, 1))
  expect_error(pssf(model, case = 99))
})

test_that("pssf errors on invalid signif_level", {
  kardl_reset()
  model <- kardl(imf_example_data, CPI ~ ER + PPI + trend, mode = c(1, 1, 1))
  expect_error(pssf(model, signif_level = "0.03"))
})

# ── psst variations ──────────────────────────────────────────────────────────
test_that("psst with ECM model works (uses EcmRes as test var)", {
  kardl_reset()
  ec <- ecm(imf_example_data, CPI ~ ER + PPI + trend, mode = c(1, 1, 1))
  pt <- psst(ec)
  expect_s3_class(pt, "kardl_test")
  sm <- summary(pt)
  expect_s3_class(sm, "summary_htest")
})

test_that("psst with trend model auto-adjusts to case 5", {
  kardl_reset()
  model <- kardl(imf_example_data, CPI ~ ER + PPI + trend, mode = c(1, 1, 1))
  pt <- psst(model, case = 3)
  expect_s3_class(pt, "kardl_test")
  expect_identical(pt$case, 5)  # auto-adjusted because model has trend
})

test_that("psst errors on invalid case", {
  kardl_reset()
  model <- kardl(imf_example_data, CPI ~ ER + PPI + trend, mode = c(1, 1, 1))
  expect_error(psst(model, case = 99))
})

# ── narayan variations ────────────────────────────────────────────────────────
test_that("narayan with case = 5 works", {
  kardl_reset()
  model <- kardl(imf_example_data, CPI ~ ER + PPI + trend, mode = c(1, 1, 1))
  nr <- narayan(model, case = 5)
  expect_s3_class(nr, "kardl_test")
})

test_that("narayan with specific signif_level works", {
  kardl_reset()
  model <- kardl(imf_example_data, CPI ~ ER + PPI, mode = c(1, 1, 1))
  nr <- narayan(model, signif_level = "0.01")
  expect_s3_class(nr, "kardl_test")
  sm <- summary(nr)
  expect_s3_class(sm, "summary_htest")
})

test_that("narayan errors on ECM model", {
  kardl_reset()
  ec <- ecm(imf_example_data, CPI ~ ER + PPI + trend, mode = c(1, 1, 1))
  expect_error(narayan(ec))
})

test_that("narayan errors on invalid case", {
  kardl_reset()
  model <- kardl(imf_example_data, CPI ~ ER + PPI, mode = c(1, 1, 1))
  expect_error(narayan(model, case = 1))
})

# ── mplier variations ─────────────────────────────────────────────────────────
test_that("mplier with minProb > 0 works", {
  kardl_reset()
  model <- kardl(imf_example_data, CPI ~ ER + PPI + asymmetric(ER) + trend,
                 mode = c(1, 1, 1, 1))
  mp <- mplier(model, horizon = 10, minProb = 0.1)
  expect_s3_class(mp, "kardl_mplier")
  expect_identical(nrow(mp$mpsi), 11L)
})

test_that("mplier errors on non-kardl_lm input", {
  expect_error(mplier(lm(mpg ~ wt, data = mtcars)))
})

test_that("mplier with lasymmetric model works", {
  kardl_reset()
  model <- kardl(imf_example_data, CPI ~ lasymmetric(ER), mode = c(1, 1))
  mp <- mplier(model, horizon = 10)
  expect_s3_class(mp, "kardl_mplier")
})

test_that("mplier with sasymmetric model works", {
  kardl_reset()
  model <- kardl(imf_example_data, CPI ~ PPI + sasymmetric(ER),
                 mode = c(1, 1, 1, 1))
  mp <- mplier(model, horizon = 10)
  expect_s3_class(mp, "kardl_mplier")
})

# ── bootstrap ─────────────────────────────────────────────────────────────────
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

test_that("bootstrap errors on non-kardl_lm input", {
  expect_error(bootstrap(lm(mpg ~ wt, data = mtcars)))
})

test_that("bootstrap with linear model (no CI columns) still works", {
  kardl_reset()
  model <- kardl(imf_example_data, CPI ~ ER + PPI, mode = c(1, 1, 1))
  bt <- bootstrap(model, horizon = 5, replications = 3)
  expect_s3_class(bt, "kardl_boot")
})

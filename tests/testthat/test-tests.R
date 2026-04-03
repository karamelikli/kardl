test_that("Post-estimation tests run correctly", {
  kardl_reset()
  formula <- CPI ~ ER + PPI + trend

  model <- kardl(data = imf_example_data, formula = formula, mode = "quick", maxlag = 2)

  # Test kardl_longrun
  lr <- kardl_longrun(model)
  expect_s3_class(lr, c("kardl_long_run","lm"))
  expect_true(length(lr$coef) > 0)

  # Test symmetrytest
  # Need an asymmetric model for meaningful symmetry test, but function should run anyway
  symm_model <- kardl(data = imf_example_data, formula = CPI ~ ER + PPI + asymmetric(ER) + trend, mode = "quick", maxlag = 2)
  st <- symmetrytest(symm_model)
  expect_s3_class(st, c("kardl_symmetric","list"))
  expect_s3_class(st$Lwald, c("anova","data.frame"))
  expect_equal(st$Lwald$Df, 1)

  # Test pssf
  pf <- pssf(model, case = 3, signif_level = "auto")
  expect_s3_class(pf,c("kardl_test", "htest" ))
  expect_equal(pf$type, "cointegration")
  expect_true(!is.null(pf$statistic))

  # Test psst
  pt <- psst(model, case = 3, signif_level = "auto")
  expect_s3_class(pt,c("kardl_test", "htest" ))
  expect_equal(pt$type, "cointegration")
  expect_true(!is.null(pt$statistic))

  # Test narayan
  nr <- narayan(model, case = 3, signif_level = "auto")
  expect_s3_class(nr,c("kardl_test", "htest" ))
  expect_equal(nr$type, "cointegration")
  expect_true(!is.null(nr$statistic))

  # Test mplier
  mp <- mplier(symm_model,horizon = 38)
  expect_s3_class(mp, "kardl_mplier" )
  expect_equal(mp$horizon, 38)
  expect_true(!is.null(mp$mpsi))

  # Test ecm
  ec <- ecm(data = imf_example_data, formula = formula, mode = "quick", case = 3, signif_level = "auto")
  expect_s3_class(ec, c("kardl_lm", "lm"))
})

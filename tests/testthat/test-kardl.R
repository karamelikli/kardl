test_that("kardl model estimates correctly in quick mode", {
  kardl_reset()
  formula <- CPI ~ ER + PPI + asymmetric(ER) + trend

  model <- kardl(data = imf_example_data, formula = formula, mode = "quick", maxlag = 2)

  expect_s3_class(model, "kardl_lm")
  expect_true(!is.null(model))
  expect_s3_class(model, "lm")
  expect_equal(model$estInfo$type, "kardlmodel")
})

test_that("kardl model estimates correctly with custom lags", {
  kardl_reset()
  formula <- CPI ~ ER + PPI + asymmetric(ER) + deterministic(covid) + trend

  # Providing a vector of custom lags
  model <- kardl(data = imf_example_data, formula = formula, mode = c(2, 1, 1,  0))

  expect_s3_class(model, "kardl_lm")
  expect_true(!is.null(model$lagInfo$OptLag))
})

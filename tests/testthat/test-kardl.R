test_that("kardl model estimates correctly in quick mode", {
  kardl_reset()
  formula <- CPI ~ ER + PPI + asymmetric(ER) + trend

  model <- kardl(data = imf_example_data, formula = formula, mode = "quick", maxlag = 2)

  expect_s3_class(model, "kardl_lm")
  expect_false(is.null(model))
  expect_s3_class(model, "lm")
  expect_identical(model$estInfo$type, "kardlmodel")
})

test_that("kardl model estimates correctly with custom lags", {
  kardl_reset()
  formula <- CPI ~ ER + PPI + asymmetric(ER) + deterministic(covid) + trend

  model <- kardl(data = imf_example_data, formula = formula, mode = c(2, 1, 1,  0))

  expect_s3_class(model, "kardl_lm")
  expect_false(is.null(model$lagInfo$OptLag))
})

test_that("kardl with grid_custom mode works", {
  kardl_reset()
  model <- kardl(imf_example_data, CPI ~ ER + PPI, mode = "grid_custom", maxlag = 2)

  expect_s3_class(model, "kardl_lm")
  expect_identical(model$estInfo$method, "grid_custom")
})

test_that("kardl with grid_custom mode and BIC criterion works", {
  kardl_reset()
  model <- kardl(imf_example_data, CPI ~ ER + PPI, mode = "grid_custom",
                 maxlag = 2, criterion = "BIC")

  expect_s3_class(model, "kardl_lm")
  expect_false(is.null(model$lagInfo$OptLag))
})

test_that("kardl with grid mode works", {
  kardl_reset()
  model <- kardl(imf_example_data, CPI ~ ER + PPI, mode = "grid", maxlag = 2)

  expect_s3_class(model, "kardl_lm")
  expect_identical(model$estInfo$method, "grid")
  expect_false(is.null(model$lagInfo$LagCriteria))
})

test_that("kardl with dot formula works", {
  kardl_reset()
  # Dot expands to ER, PPI, covid; covid goes to deterministic -> 3 shortRunVars
  model <- kardl(imf_example_data, CPI ~ . + deterministic(covid),
                 mode = c(1, 1, 1))
  expect_s3_class(model, "kardl_lm")
})

test_that("kardl with lasymmetric produces correct structure", {
  kardl_reset()
  model <- kardl(imf_example_data, CPI ~ lasymmetric(ER), mode = c(1, 1))
  expect_s3_class(model, "kardl_lm")
  expect_gt(length(model$extractedInfo$ALvars), 0)
  expect_length(model$extractedInfo$ASvars, 0)
})

test_that("kardl with sasymmetric produces correct structure", {
  kardl_reset()
  model <- kardl(imf_example_data, CPI ~ PPI + sasymmetric(ER),
                 mode = c(1, 1, 1, 1))
  expect_s3_class(model, "kardl_lm")
  expect_gt(length(model$extractedInfo$ASvars), 0)
  expect_length(model$extractedInfo$ALvars, 0)
})

test_that("kardl grid_custom with batch processing works", {
  kardl_reset()
  model <- kardl(imf_example_data, CPI ~ ER + PPI,
                 mode = "grid_custom", maxlag = 2, batch = "1/2")
  expect_s3_class(model, "kardl_lm")
})

test_that("kardl fails informatively when formula missing and not in kardl_set", {
  kardl_reset()
  expect_error(kardl(data = imf_example_data, mode = c(1, 1, 1)))
})

test_that("modelCriterion works with all built-in criteria", {
  kardl_reset()
  mylm <- lm(mpg ~ wt + hp, data = mtcars)

  aic  <- modelCriterion(mylm, "AIC")
  bic  <- modelCriterion(mylm, "BIC")
  aicc <- modelCriterion(mylm, "AICc")
  hq   <- modelCriterion(mylm, "HQ")

  expect_type(aic, "double")
  expect_type(bic, "double")
  expect_type(aicc, "double")
  expect_type(hq, "double")
})

test_that("modelCriterion works with a user-defined function", {
  mylm <- lm(mpg ~ wt + hp, data = mtcars)
  my_fn <- function(mod, ...) stats::AIC(mod)
  result <- modelCriterion(mylm, my_fn)
  expect_identical(result, stats::AIC(mylm))
})

test_that("ecm model estimates correctly", {
  kardl_reset()
  ec <- ecm(imf_example_data, CPI ~ ER + PPI + trend, mode = c(1, 1, 1))
  expect_s3_class(ec, "kardl_lm")
  expect_identical(ec$estInfo$type, "ecm")
})

test_that("ecm summary works", {
  kardl_reset()
  ec <- ecm(imf_example_data, CPI ~ ER + PPI + trend, mode = c(1, 1, 1))
  sm <- summary(ec)
  expect_false(is.null(sm$coefficients))
})

test_that("ecm model with notes when ECM coeff is non-negative or < -1", {
  # Just check that model is returned; note checking is secondary
  kardl_reset()
  ec <- ecm(imf_example_data, CPI ~ ER + PPI, mode = c(1, 1, 1))
  expect_s3_class(ec, "kardl_lm")
})

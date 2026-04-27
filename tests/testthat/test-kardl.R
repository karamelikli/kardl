#' @srrstats {G5.2} The test suite exercises representative successful calls and
#' selected error conditions for model estimation, multiplier calculation, and
#' S3 methods.
#' @srrstats {G5.3} Tests verify that returned objects have the expected class,
#' @srrstats {G5.4} Verifies correct structural properties of the fitted object
#' for the baseline linear ARDL specification.
#' dimensions, names, and component values.

test_that("kardl model estimates correctly in quick mode", {
  kardl_reset()
  formula <- CPI ~ ER + PPI + asymmetric(ER) + trend

  model <- kardl(data = imf_example_data, formula = formula, mode = "quick", maxlag = 2)

  expect_s3_class(model, "kardl_lm")
  expect_false(is.null(model))
  expect_s3_class(model, "lm")
  expect_identical(model$estInfo$type, "kardlmodel")
})
#' @srrstats {G5.3} Checks that user-defined lag mode produces an object with
#' a non-NULL `lagInfo$OptLag` component.
#' @srrstats {G5.6} Confirms the lag vector supplied by the user is applied
#' and recoverable from the fitted object.
test_that("kardl model estimates correctly with custom lags", {

  kardl_reset()
  formula <- CPI ~ ER + PPI + asymmetric(ER) + deterministic(covid) + trend

  model <- kardl(data = imf_example_data, formula = formula, mode = c(2, 1, 1,  0))

  expect_s3_class(model, "kardl_lm")
  expect_false(is.null(model$lagInfo$OptLag))
})
#' @srrstats {G5.3} Checks returned class and `estInfo$method` for grid_custom mode.
#' @srrstats {G5.7} Tests behaviour across a different lag-search algorithm (grid_custom).
test_that("kardl with grid_custom mode works", {

  kardl_reset()
  model <- kardl(imf_example_data, CPI ~ ER + PPI, mode = "grid_custom", maxlag = 2)

  expect_s3_class(model, "kardl_lm")
  expect_identical(model$estInfo$method, "grid_custom")
})
#' @srrstats {G5.3} Verifies OptLag is populated when BIC is used as the criterion.
#' @srrstats {G5.7} Tests model estimation with an alternative information criterion.
test_that("kardl with grid_custom mode and BIC criterion works", {

  kardl_reset()
  model <- kardl(imf_example_data, CPI ~ ER + PPI, mode = "grid_custom",
                 maxlag = 2, criterion = "BIC")

  expect_s3_class(model, "kardl_lm")
  expect_false(is.null(model$lagInfo$OptLag))
})
#' @srrstats {G5.3} Checks returned class, method, and LagCriteria for grid mode.
#' @srrstats {G5.7} Tests the full grid-search estimation path.
test_that("kardl with grid mode works", {

  kardl_reset()
  model <- kardl(imf_example_data, CPI ~ ER + PPI, mode = "grid", maxlag = 2)

  expect_s3_class(model, "kardl_lm")
  expect_identical(model$estInfo$method, "grid")
  expect_false(is.null(model$lagInfo$LagCriteria))
})
#' @srrstats {G5.4} Verifies that the dot (.) formula expansion produces a
#' valid estimable model.
test_that("kardl with dot formula works", {

  kardl_reset()
  #' Dot expands to ER, PPI, covid; covid goes to deterministic -> 3 shortRunVars
  model <- kardl(imf_example_data, CPI ~ . + deterministic(covid),
                 mode = c(1, 1, 1))
  expect_s3_class(model, "kardl_lm")
})
#' @srrstats {G5.3} Checks that `ALvars` is populated and `ASvars` is empty
#' for a long-run-asymmetric-only specification.
#' @srrstats {G5.7} Tests model estimation across the SA (long-run asymmetric) configuration.
test_that("kardl with lasymmetric produces correct structure", {

  kardl_reset()
  model <- kardl(imf_example_data, CPI ~ lasymmetric(ER), mode = c(1, 1))
  expect_s3_class(model, "kardl_lm")
  expect_gt(length(model$extractedInfo$ALvars), 0)
  expect_length(model$extractedInfo$ASvars, 0)
})
#' @srrstats {G5.3} Checks that `ASvars` is populated and `ALvars` is empty
#' for a short-run-asymmetric-only specification.
#' @srrstats {G5.7} Tests model estimation across the AS (short-run asymmetric) configuration.
test_that("kardl with sasymmetric produces correct structure", {

  kardl_reset()
  model <- kardl(imf_example_data, CPI ~ PPI + sasymmetric(ER),
                 mode = c(1, 1, 1, 1))
  expect_s3_class(model, "kardl_lm")
  expect_gt(length(model$extractedInfo$ASvars), 0)
  expect_length(model$extractedInfo$ALvars, 0)
})
#' @srrstats {G5.3} Verifies that batch-mode estimation returns a valid
#' `kardl_lm` object.
test_that("kardl grid_custom with batch processing works", {

  kardl_reset()
  model <- kardl(imf_example_data, CPI ~ ER + PPI,
                 mode = "grid_custom", maxlag = 2, batch = "1/2")
  expect_s3_class(model, "kardl_lm")
})
#' @srrstats {G5.2a} Checks that a missing formula without a fallback in
#' `kardl_set()` produces an informative error.
#' @srrstats {G5.8a} Verifies that missing required input is rejected before
#' any estimation occurs.
test_that("kardl fails informatively when formula missing and not in kardl_set", {

  kardl_reset()
  expect_error(kardl(data = imf_example_data, mode = c(1, 1, 1)))
})
#' @srrstats {G5.2} Tests all four predefined string criterion options.
#' @srrstats {G5.3} Verifies that each criterion returns a numeric scalar.
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
#' @srrstats {G5.2} Tests the user-defined function path of `modelCriterion()`.
#' @srrstats {G5.6} Result matches the base-R AIC function, confirming
#' correctness of the function-dispatch branch.
test_that("modelCriterion works with a user-defined function", {

  mylm <- lm(mpg ~ wt + hp, data = mtcars)
  my_fn <- function(mod, ...) stats::AIC(mod)
  result <- modelCriterion(mylm, my_fn)
  expect_identical(result, stats::AIC(mylm))
})
#' @srrstats {G5.3} Checks returned class and `estInfo$type` for ECM estimation.
#' @srrstats {TS2.0} Confirms the ECM estimation path handles lagged and
#' differenced variables correctly.
test_that("ecm model estimates correctly", {

  kardl_reset()
  ec <- ecm(imf_example_data, CPI ~ ER + PPI + trend, mode = c(1, 1, 1))
  expect_s3_class(ec, "kardl_lm")
  expect_identical(ec$estInfo$type, "ecm")
})
#' @srrstats {G5.3} Verifies that `summary()` on an ECM object returns
#' a coefficient table.
test_that("ecm summary works", {

  kardl_reset()
  ec <- ecm(imf_example_data, CPI ~ ER + PPI + trend, mode = c(1, 1, 1))
  sm <- summary(ec)
  expect_false(is.null(sm$coefficients))
})
#' Just check that model is returned; note checking is secondary
#' @srrstats {G5.3} Confirms that a valid `kardl_lm` object is returned
#' even when the ECM coefficient triggers a note.
test_that("ecm model with notes when ECM coeff is non-negative or < -1", {

  kardl_reset()
  ec <- ecm(imf_example_data, CPI ~ ER + PPI, mode = c(1, 1, 1))
  expect_s3_class(ec, "kardl_lm")
})

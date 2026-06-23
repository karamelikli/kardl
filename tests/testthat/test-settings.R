#' @srrstats {G5.2} Tests exercise successful calls and error conditions for
#' the kardl package settings system (`kardl_set()`, `kardl_get()`,
#' `kardl_reset()`).
#' @srrstats {G5.3} Checks that default settings match documented values
#' (maxlag = 4L, criterion = "AIC", different_asym_lag = TRUE).
#' @srrstats {G5.4a} Uses deterministic known defaults to verify the settings
#' round-trip through set -> get -> reset.
test_that("Settings can be set, retrieved, and reset", {
  kardl_reset()
  default_settings <- kardl_get()

  expect_identical(default_settings$maxlag, 4L)
  expect_identical(default_settings$criterion, "AIC")
  expect_true(default_settings$different_asym_lag)

  kardl_set(maxlag = 5, criterion = "BIC", different_asym_lag = FALSE)
  new_settings <- kardl_get()

  expect_identical(new_settings$maxlag, 5)
  expect_identical(new_settings$criterion, "BIC")
  expect_false(new_settings$different_asym_lag)

  kardl_reset()
  reset_settings <- kardl_get()

  expect_identical(reset_settings$maxlag, 4L)
  expect_identical(reset_settings$criterion, "AIC")
  expect_true(reset_settings$different_asym_lag)
})

#'
#'
#' @srrstats {G5.2} Tests that the .onLoad and .onAttach functions properly set
#' default options and that the reset function works as intended, including the
#' exclude argument functionality.
#'

test_that("zzz.R functions work correctly", {
  # Test .onLoad / options
  old_opts <- options()
  kardl_reset()
  expect_equal(kardl_get("criterion"), "AIC")
  expect_equal(kardl_get("different_asym_lag"), TRUE)

  # Test with exclude
  kardl_set(criterion = "BIC")
  kardl_reset(exclude = "criterion")
  expect_equal(kardl_get("criterion"), "BIC") # preserved
  expect_equal(kardl_get("maxlag"), 4L)

  # Test .onAttach (quietly)
  # Restore
  options(old_opts)
})

#' @srrstats {G5.2} Tests that kardl_get returns correct values for existing
#' options and produces an informative error when requesting an invalid option
#' name.
#'

test_that("kardl_get returns correct values and errors", {
  kardl_reset()
  expect_equal(kardl_get("criterion"), "AIC")
  expect_equal(kardl_get("different_asym_lag"), TRUE)

  # Test error for invalid name
  expect_error(
    kardl_get("invalid_option"),
    "Option(s) not found: invalid_option",
    fixed = TRUE
  )
})

#' @srrstats {G5.2} Tests that kardl_set successfully updates valid options and

test_that("kardl_set invalidates option names", {
  expect_error(
    kardl_set(invalid_option = 123),
    "Invalid option(s): invalid_option",
    fixed = TRUE
  )
})

test_that("kardl_reset with exclude retains specified options", {
  kardl_set(criterion = "BIC", different_asym_lag = FALSE)
  kardl_reset(exclude = c("criterion", "different_asym_lag"))
  expect_equal(kardl_get("criterion"), "BIC")
  expect_equal(kardl_get("different_asym_lag"), FALSE)
  expect_error(
    kardl_reset("invalid_option"),
    "Invalid setting(s) in `exclude`: invalid_option",
    fixed = TRUE
  )
  expect_error(
    kardl_reset(exclude = 1L),
    "`exclude` must be a character vector of setting names.",
    fixed = TRUE
  )
})

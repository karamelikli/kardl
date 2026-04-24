test_that("Settings can be set, retrieved, and reset", {
  kardl_reset()
  default_settings <- kardl_get()

  expect_identical(default_settings$maxlag, 4L)
  expect_identical(default_settings$criterion, "AIC")
  expect_true(default_settings$differentAsymLag)

  kardl_set(maxlag = 5, criterion = "BIC", differentAsymLag = FALSE)
  new_settings <- kardl_get()

  expect_identical(new_settings$maxlag, 5)
  expect_identical(new_settings$criterion, "BIC")
  expect_false(new_settings$differentAsymLag)

  kardl_reset()
  reset_settings <- kardl_get()

  expect_identical(reset_settings$maxlag, 4L)
  expect_identical(reset_settings$criterion, "AIC")
  expect_true(reset_settings$differentAsymLag)
})

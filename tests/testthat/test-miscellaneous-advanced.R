#' Comprehensive tests for miscellaneous utility functions
#'
#' @srrstats {G5.2} Tests exercise utility functions with various inputs
#' @srrstats {G5.3} Tests verify utility function outputs
#' @srrstats {G5.4a} Tests handle edge cases

test_that("replace_lag_var handles single variable", {
  string <- "L{lag}.{var_name}"
  result <- replace_lag_var(string, "PetrolPrice", 2)

  expect_equal(result, "L2.PetrolPrice")
})

test_that("replace_lag_var handles multiple variables", {
  string <- "L{lag}.{var_name}"
  result <- replace_lag_var(string, c("PetrolPrice", "drivers"), 2)

  expect_equal(length(result), 2)
  expect_equal(as.character(result["PetrolPrice"]), "L2.PetrolPrice")
  expect_equal(as.character(result["drivers"]), "L2.drivers")
})

test_that("replace_lag_var handles numeric lag values", {
  string <- "Lag{lag}_{var_name}"
  result <- replace_lag_var(string, "DriversKilled", 5)

  expect_equal(result, "Lag5_CPI")
})

test_that("replace_lag_var handles character lag values", {
  string <- "L{lag}.{var_name}"
  result <- replace_lag_var(string, "PetrolPrice", "3")

  expect_equal(result, "L3.PetrolPrice")
})

test_that("progress_bar handles different percentages", {
  # Test at 0%
  expect_invisible(progress_bar(0, 100, verbose = TRUE, use_message = TRUE))

  # Test at 50%
  expect_invisible(progress_bar(50, 100, verbose = TRUE, use_message = TRUE))

  # Test at 100%
  expect_invisible(progress_bar(100, 100, verbose = TRUE, use_message = TRUE))
})

test_that("progress_bar handles verbose = FALSE", {
  # Should not produce output when verbose = FALSE
  expect_invisible(progress_bar(50, 100, verbose = FALSE))
})

test_that("progress_bar handles additional strings", {
  expect_invisible(
    progress_bar(25, 100,
      additional_strings = "Processing...",
      verbose = TRUE, use_message = TRUE
    )
  )
})

test_that("progress_bar handles different total values", {
  # Test with small total
  expect_invisible(progress_bar(5, 10, verbose = TRUE, use_message = TRUE))

  # Test with large total
  expect_invisible(progress_bar(500, 1000, verbose = TRUE, use_message = TRUE))
})

test_that("progress_bar animates correctly", {
  # Test different current values to trigger animation
  expect_invisible(progress_bar(1, 100, verbose = TRUE, use_message = TRUE))
  expect_invisible(progress_bar(2, 100, verbose = TRUE, use_message = TRUE))
  expect_invisible(progress_bar(3, 100, verbose = TRUE, use_message = TRUE))
})

test_that("batch_control handles valid batch formats", {
  spec <- list(
    args_info = list(batch = "1/1"),
    extracted_info = list(lag_rows_number = 100)
  )
  result <- batch_control(spec)

  expect_equal(result$start_row, 1)
  expect_equal(result$end_row, 100)
  expect_equal(result$batch_size, 100)
})

test_that("batch_control handles multiple batches", {
  spec <- list(
    args_info = list(batch = "1/3"),
    extracted_info = list(lag_rows_number = 90)
  )
  result <- batch_control(spec)

  expect_equal(result$start_row, 1)
  expect_equal(result$end_row, 30)
  expect_equal(result$batch_size, 30)
})

test_that("batch_control handles last batch", {
  spec <- list(
    args_info = list(batch = "3/3"),
    extracted_info = list(lag_rows_number = 90)
  )
  result <- batch_control(spec)

  expect_equal(result$start_row, 61)
  expect_equal(result$end_row, 90)
})

test_that("batch_control handles uneven division", {
  spec <- list(
    args_info = list(batch = "2/3"),
    extracted_info = list(lag_rows_number = 100)
  )
  result <- batch_control(spec)

  expect_true(result$start_row > 0)
  expect_true(result$end_row <= 100)
})

test_that("batch_control returns zero range for out-of-range batch", {
  spec <- list(
    args_info = list(batch = "5/3"),
    extracted_info = list(lag_rows_number = 90)
  )

  expect_warning(
    result <- batch_control(spec),
    "between 1 and"
  )
})

test_that("parse_formula_vars handles complex formulas", {
  formula <- y ~ x + z + asymmetric(a + b) + deterministic(d)
  vars <- parse_formula_vars(formula)

  expect_type(vars, "list")
  expect_true("response" %in% names(vars))
  expect_true("outside" %in% names(vars))
  expect_true("inside" %in% names(vars))
})

test_that("parse_formula_vars handles trend", {
  formula <- y ~ x + trend
  vars <- parse_formula_vars(formula)

  expect_true(
    "trend" %in% vars$outside ||
      ("trend" %in% unlist(vars$inside))
  )
})

test_that("parse_formula_vars handles intercept variations", {
  # With intercept
  formula1 <- y ~ x
  vars1 <- parse_formula_vars(formula1)
  expect_true(vars1$intercept)

  # Without intercept (-1)
  formula2 <- y ~ x - 1
  vars2 <- parse_formula_vars(formula2)
  expect_false(vars2$intercept)

  # Without intercept (+0)
  formula3 <- y ~ x + 0
  vars3 <- parse_formula_vars(formula3)
  expect_false(vars3$intercept)
})

test_that("parse_formula_vars handles dot operator", {
  formula <- y ~ .
  vars <- parse_formula_vars(formula)

  expect_true(vars$dot)
})

test_that("parse_formula_vars handles multiple asymmetric specs", {
  formula <- y ~ Lasymmetric(a) + Sasymmetric(b) + asymmetric(c)
  vars <- parse_formula_vars(formula)

  expect_true("Lasymmetric" %in% names(vars$inside))
  expect_true("Sasymmetric" %in% names(vars$inside))
  expect_true("asymmetric" %in% names(vars$inside))
})

test_that("lmerge handles nested lists", {
  a <- list(x = list(y = 1))
  b <- list(x = list(y = 2), z = 3)

  result <- lmerge(a, b)

  expect_equal(result$x$y, 1) # First list takes precedence
  expect_equal(result$z, 3)
})

test_that("lmerge handles NULL values", {
  a <- list(x = NULL, y = 2)
  b <- list(x = 1, z = 3)

  result <- lmerge(a, b)

  expect_null(result$x) # NULL from first list is preserved
  expect_equal(result$y, 2)
  expect_equal(result$z, 3)
})

test_that("lmerge handles empty lists", {
  a <- list()
  b <- list(x = 1, y = 2)

  result <- lmerge(a, b)

  expect_equal(result$x, 1)
  expect_equal(result$y, 2)
})

test_that("lmerge handles many lists", {
  a <- list(x = 1)
  b <- list(y = 2)
  c <- list(z = 3)
  d <- list(w = 4)

  result <- lmerge(a, b, c, d)

  expect_equal(result$x, 1)
  expect_equal(result$y, 2)
  expect_equal(result$z, 3)
  expect_equal(result$w, 4)
})

test_that("model_criterion works with different criteria", {
  kardl_reset()

  model <- kardl(DriversKilled ~ PetrolPrice + drivers, data = Seatbelts, maxlag = 1)

  # Test with string criteria
  aic_val <- model_criterion(model, "AIC")
  expect_type(aic_val, "double")

  bic_val <- model_criterion(model, "BIC")
  expect_type(bic_val, "double")
})

test_that("parse_formula_vars handles character formula input", {
  vars <- parse_formula_vars("y ~ x + z")

  expect_type(vars, "list")
  expect_true("x" %in% vars$outside)
  expect_true("z" %in% vars$outside)
})

test_that("parse_formula_vars handles subtraction", {
  formula <- y ~ x + z - w
  vars <- parse_formula_vars(formula)

  expect_true("w" %in% vars$outside)
})

test_that("parse_formula_vars handles whitespace", {
  formula <- y ~ x + z
  vars <- parse_formula_vars(formula)

  expect_true("x" %in% vars$outside)
  expect_true("z" %in% vars$outside)
})

test_that("lmerge handles duplicate names in additional args", {
  a <- list(x = 1)
  b <- list(y = 2)
  c <- list(x = 99, z = 3)

  result <- lmerge(a, b, c)

  # First x should win
  expect_equal(result$x, 1)
  expect_equal(result$y, 2)
  expect_equal(result$z, 3)
})

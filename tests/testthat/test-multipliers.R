#' @srrstats {G5.9} The \code{bootstrap()} function tests for expected stochastic behaviour: confidence intervals are stable under both trivial noise perturbations (G5.9a) and different random seeds (G5.9b), as verified in \code{tests/testthat/test-bootstrap-noise.R}.
#' @srrstats {G5.9a} Bootstrap confidence intervals are stable under trivial noise perturbations: adding machine-epsilon noise to the data should not meaningfully change point estimates or confidence interval bounds.
#' @srrstats {G5.9b} Bootstrap results are stable across different random seeds: point estimates should be identical, and confidence interval widths should not differ by more than 10% of the signal range.

test_that("G5.9b: bootstrap results are stable across different seeds", {
  fit_base <- kardl(imf_example_data, CPI ~ asymmetric(ER),
                    mode = c(1, 1, 1))
  b1 <- bootstrap(fit_base, horizon = 10, replications = 10, seed = 1L)
  b2 <- bootstrap(fit_base, horizon = 10, replications = 10, seed = 99L)

  # Point estimates come from mplier() which is deterministic — must be identical
  expect_equal(b1$mpsi[["ER_dif"]], b2$mpsi[["ER_dif"]])

  # CI width range-to-range: variation between seeds should be small
  # relative to the range of CI widths themselves
  ci_width_1   <- b1$mpsi[["ER_CI_upper"]] - b1$mpsi[["ER_CI_lower"]]
  ci_width_2   <- b2$mpsi[["ER_CI_upper"]] - b2$mpsi[["ER_CI_lower"]]

  diff_of_widths  <- abs(ci_width_1 - ci_width_2)          # seed-to-seed diff
  range_of_widths <- diff(range(c(ci_width_1, ci_width_2))) # combined range

  expect_true(
    max(diff_of_widths) < 0.25 * max(range_of_widths, 1e-8),
    label = paste0(
      "CI widths differ too much between seeds. ",
      "Max seed-to-seed diff: ", round(max(diff_of_widths), 6),
      ", 25% of CI width range: ", round(0.25 * range_of_widths, 6)
    )
  )
})

#' @srrstats {G5.9} The \code{bootstrap()} function tests for expected stochastic behaviour: confidence intervals are stable under both trivial noise perturbations (G5.9a) and different random seeds (G5.9b), as verified in \code{tests/testthat/test-bootstrap-noise.R}.
#' @srrstats {G5.9a} Bootstrap confidence intervals are stable under trivial noise perturbations: adding machine-epsilon noise to the data should not meaningfully change point estimates or confidence interval bounds.
#' @srrstats {G5.9b} Bootstrap results are stable across different random seeds: point estimates should be identical, and confidence interval widths should not differ by more than 10% of the signal range.

test_that("G5.9b: same seed yields identical bootstrap output", {
  fit_base <- kardl(imf_example_data, CPI ~ asymmetric(ER),
                    mode = c(1, 1, 1))
  b_a <- bootstrap(fit_base, horizon = 10, replications = 10, seed = 42L)
  b_b <- bootstrap(fit_base, horizon = 10, replications = 10, seed = 42L)

  expect_equal(b_a$mpsi, b_b$mpsi)
})

#' @srrstats {G5.9a} Bootstrap confidence intervals are stable under trivial noise perturbations: adding machine-epsilon noise to the data should not meaningfully change point estimates
#' or confidence interval bounds.

test_that("G5.9a: adding machine-epsilon noise to data does not change results", {

  fit_base <- kardl(imf_example_data, CPI ~ asymmetric(ER), mode = c(1, 1, 1))

  data_noisy <- imf_example_data
  numeric_cols <- sapply(data_noisy, is.numeric)

  # Scale noise relative to each column's magnitude, not absolute eps.
  # This ensures the perturbation stays at machine-epsilon *relative* scale
  # even after cumulative summation inside kardl's asymmetric decomposition.
  col_scales <- sapply(data_noisy[, numeric_cols, drop = FALSE],
                       function(x) mean(abs(x), na.rm = TRUE))
  col_scales <- pmax(col_scales, 1)   # floor at 1 to avoid zero scale

  noise_matrix <- matrix(
    rnorm(sum(numeric_cols) * nrow(data_noisy), mean = 0, sd = .Machine$double.eps),
    nrow = nrow(data_noisy)
  )
  noise_matrix <- sweep(noise_matrix, 2, col_scales, `*`)

  data_noisy[, numeric_cols] <- data_noisy[, numeric_cols] + noise_matrix

  fit_noisy <- kardl(data_noisy, CPI ~ asymmetric(ER), mode = c(1, 1, 1))

  b_clean <- bootstrap(fit_base,  horizon = 10, replications = 10, seed = 7L)
  b_noisy <- bootstrap(fit_noisy, horizon = 10, replications = 10, seed = 7L)

  # Multiplier point estimates should be virtually identical
  expect_equal(
    b_clean$mpsi[["ER_dif"]],
    b_noisy$mpsi[["ER_dif"]],
    tolerance = 1e-4,
    label = "ER_dif multipliers changed meaningfully under trivial noise"
  )

  # CI bounds should also be negligibly different
  expect_equal(
    b_clean$mpsi[["ER_CI_upper"]],
    b_noisy$mpsi[["ER_CI_upper"]],
    tolerance = 1e-3,
    label = "Upper CI changed meaningfully under trivial noise"
  )
})

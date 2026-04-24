
# Helper: build a small, fast set of models once per test file
local_models <- local({
  kardl_reset()
  linear <- kardl(imf_example_data, CPI ~ ER + PPI + trend,
                  mode = c(1, 1, 1))
  asym <- kardl(imf_example_data, CPI ~ ER + PPI + asymmetric(ER) + trend,
                mode = c(1, 1, 1, 1))
  list(linear = linear, asym = asym)
})

# ── print.kardl_lm ────────────────────────────────────────────────────────────
test_that("print.kardl_lm produces expected output", {
  expect_output(print(local_models$linear), "Optimal lags")
  expect_output(print(local_models$asym),   "Optimal lags")
})

# ── kardl_longrun & its print / summary ───────────────────────────────────────
test_that("summary_kardl_longrun works (class is kardl_longrun)", {
  lr <- kardl_longrun(local_models$linear)
  # The actual class assigned in longrun.R is "kardl_longrun"
  expect_s3_class(lr, "kardl_longrun")

  # summary dispatches correctly via summary.kardl_longrun
  sm <- summary(lr)
  expect_s3_class(sm, "summary_kardl_longrun")
  expect_false(is.null(sm$coefficients))

  expect_output(print(sm), "Estimation type")
  expect_output(print(sm), "Coefficients")
})

# ── summary / print for kardl_symmetric (F type) ─────────────────────────────
test_that("print and summary of kardl_symmetric (type F) work", {
  st <- symmetrytest(local_models$asym)

  # print.kardl_symmetric
  expect_output(print(st))

  # summary.kardl_symmetric + print.summary.kardl_symmetric
  sm <- summary(st)
  expect_s3_class(sm, "summary.kardl_symmetric")
  expect_output(print(sm), "Long-run symmetry tests")
})

test_that("summary.kardl_symmetric with a custom level works", {
  st <- symmetrytest(local_models$asym)
  sm_strict <- summary(st, level = 0.01)
  expect_s3_class(sm_strict, "summary.kardl_symmetric")
})

# ── summary / print for kardl_symmetric (Chisq type) ─────────────────────────
test_that("print and summary of kardl_symmetric (type Chisq) work", {
  st_chi <- symmetrytest(local_models$asym, type = "Chisq")
  expect_output(print(st_chi))

  sm_chi <- summary(st_chi)
  expect_s3_class(sm_chi, "summary.kardl_symmetric")
  expect_output(print(sm_chi), "Long-run symmetry tests")
})

# ── kardl_symmetric with short-run component only ────────────────────────────
test_that("print.kardl_symmetric handles short-run-only results", {
  kardl_reset()
  sasym_model <- kardl(imf_example_data, CPI ~ PPI + sasymmetric(ER),
                       mode = c(1, 1, 1, 1))
  st_sr <- symmetrytest(sasym_model, component = "shortrun")
  expect_output(print(st_sr))

  sm_sr <- summary(st_sr)
  expect_output(print(sm_sr), "Short-run symmetry tests")
})

# ── summary / print for kardl_test (pssf, psst, narayan) ─────────────────────
test_that("summary.kardl_test dispatches correctly for pssf", {
  pf <- pssf(local_models$linear)
  sm <- summary(pf)
  expect_s3_class(sm, "summary_htest")
  expect_output(print(sm), "Critical Values")
})

test_that("summary.kardl_test dispatches correctly for psst", {
  pt <- psst(local_models$linear)
  sm <- summary(pt)
  expect_s3_class(sm, "summary_htest")
  expect_output(print(sm), "Critical Values")
})

test_that("summary.kardl_test dispatches correctly for narayan", {
  nr <- narayan(local_models$linear)
  sm <- summary(nr)
  expect_s3_class(sm, "summary_htest")
  expect_output(print(sm), "Critical Values")
})

# ── print / summary for kardl_mplier ─────────────────────────────────────────
test_that("print and summary for kardl_mplier work", {
  mp <- mplier(local_models$asym, horizon = 10)

  expect_output(print(mp), "kardl Dynamic Multiplier Object")
  expect_output(print(mp), "Horizon: 10")

  sm <- summary(mp)
  expect_s3_class(sm, "summary.kardl_mplier")
  expect_output(print(sm), "Summary of Dynamic Multipliers")
  expect_output(print(sm), "Horizon: 10")
})

# ── print / summary for kardl_boot ───────────────────────────────────────────
test_that("print and summary for kardl_boot work", {
  kardl_reset()
  asym_only <- kardl(imf_example_data, CPI ~ asymmetric(ER),
                     mode = c(1, 1, 1))
  bt <- bootstrap(asym_only, horizon = 10, replications = 5, level = 90)

  expect_s3_class(bt, "kardl_boot")
  expect_output(print(bt), "kardl Bootstrap Results")
  expect_output(print(bt), "Confidence level: 90")

  sm <- summary(bt)
  expect_s3_class(sm, "summary.kardl_boot")
  expect_output(print(sm), "Summary of Dynamic Multipliers")
})

# ── plot methods (non-interactive: should not error) ──────────────────────────
test_that("plot.kardl_mplier runs without error", {
  mp <- mplier(local_models$asym, horizon = 5)
  expect_no_error(plot(mp))
  expect_no_error(plot(mp, variables = "ER"))
})

test_that("plot.kardl_mplier warns for unknown variable", {
  mp <- mplier(local_models$asym, horizon = 5)
  expect_warning(plot(mp, variables = "NOTAVAR"), "not exits")
})

test_that("plot on kardl_longrun runs without error", {
  # Note: class is "kardl_longrun", so plot.lm is dispatched (not plot.kardl_long_run)
  lr <- kardl_longrun(local_models$linear)
  expect_no_error(plot(lr, which = 1))
})

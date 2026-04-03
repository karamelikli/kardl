test_that("model Criterion functions work correctly", {
  mylm<- lm(mpg ~ wt + hp, data = mtcars)

   expect_equal(modelCriterion(mylm,AIC), stats::AIC(mylm))
   expect_equal(modelCriterion(mylm,BIC), stats::BIC(mylm))

  # Not strictly necessary to test formulas, but good to know `parse_formula_vars` runs
  formula <- y ~ x + z + asymmetric(z) + Lasymmetric(x1+x2) + Sasymmetric(x3+x4+x5) + deterministic(d1) + trend
  vars <- parse_formula_vars(formula)

  expect_type(vars, "list")
  expect_true("asymmetric" %in% names(vars$inside))
  expect_true("Sasymmetric" %in% names(vars$inside))
  expect_true("x4" %in% vars$inside$Sasymmetric)
  expect_false("x2" %in% vars$inside$Sasymmetric)
  expect_true(vars$intercept)
  expect_false(vars$dot)
})

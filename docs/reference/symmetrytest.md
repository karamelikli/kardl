# Symmetry Test for Nonlinear kardl Models

The symmetry test is a statistical procedure used to assess the presence
of symmetry in the relationship between variables in a model. It is
particularly useful in econometric analysis, where it helps to identify
whether the effects of changes in one variable on another are symmetric
or asymmetric. The test involves estimating a model that includes both
positive and negative components of the variables and then performing a
Wald test to determine if the coefficients of these components are
significantly different from each other. If the test indicates
significant differences, it suggests that the relationship is
asymmetric, meaning that the impact of increases and decreases in the
variables differs. This test returns results for both long-run and
short-run variables in a KARDL model. Where applicable, it provides the
Wald test statistics, p-values, degrees of freedom, sum of squares, and
mean squares for each variable tested. If the null hypothesis of
symmetry is rejected, it indicates that the effects of positive and
negative changes in the variable are significantly different, suggesting
an asymmetric relationship.

The non-linear model with one asymmetric variables is specified as
follows: \$\$ \Delta{y\_{t}} = \psi + \eta\_{0}y\_{t - 1} +
\eta^{+}\_{1} x^{+}\_{t - 1} + \eta^{-}\_{1} x^{-}\_{t - 1} + \sum\_{j =
1}^{p}{\gamma\_{j}\Delta y\_{t - j}} + \sum\_{j =
0}^{q}{\beta^{+}\_{j}\Delta x^{+}\_{t - j}} + \sum\_{j =
0}^{m}{\beta^{-}\_{j}\Delta x^{-}\_{t - j}} + e\_{t} \$\$

This function performs the symmetry test both for long-run and short-run
variables in a kardl model. It uses the
[`nlWaldtest`](https://rdrr.io/pkg/nlWaldTest/man/nlWaldtest.html)
function from the nlWaldTest package for long-run variables and the
[`linearHypothesis`](https://rdrr.io/pkg/car/man/linearHypothesis.html)
function from the car package for short-run variables. The hypotheses
for the long-run variables are: \$\$ H\_{0}:
-\frac{\eta^{+}\_{1}}{\eta\_{0}} = -\frac{\eta^{-}\_{1}}{\eta\_{0}} \$\$
\$\$ H\_{1}: -\frac{\eta^{+}\_{1}}{\eta\_{0}} \neq
-\frac{\eta^{-}\_{1}}{\eta\_{0}} \$\$

The hypotheses for the short-run variables are: \$\$ H\_{0}: \sum\_{j =
0}^{q}{\beta^{+}\_{j}} = \sum\_{j = 0}^{m}{\beta^{-}\_{j}} \$\$ \$\$
H\_{1}: \sum\_{j = 0}^{q}{\beta^{+}\_{j}} \neq \sum\_{j =
0}^{m}{\beta^{-}\_{j}} \$\$

## Usage

``` r
symmetrytest(
  kardl_model,
  selected_vars = NULL,
  component = "both",
  type = "F",
  ...
)
```

## Arguments

- kardl_model:

  An object of class `kardl_lm` produced by
  [`kardl`](https://karamelikli.github.io/kardl/reference/kardl.md) or
  an object of class `kardl_longrun` produced by
  [`kardl_longrun`](https://karamelikli.github.io/kardl/reference/kardl_longrun.md).

- selected_vars:

  A character vector specifying the names of the variables to be
  included in the symmetry test. If NULL (the default), all eligible
  variables will be tested. The variable names should match those used
  in the KARDL model, and they can be either long-run or short-run
  variables. If any specified variable is not found in the model, an
  error will be raised.

- component:

  A character string specifying which component(s) of the model to test
  for symmetry. The options are "both" (default), "shortrun", or
  "longrun". If "both" is selected, the function will perform symmetry
  tests for both long-run and short-run variables. If "shortrun" is
  selected, only short-run variables will be tested, and if "longrun" is
  selected, only long-run variables will be tested. Invalid values will
  result in an error.

- type:

  A character string specifying the type of test statistic to be used in
  the Wald tests. The options are "F" (default) or "Chisq". If "F" is
  selected, the function will perform an F-test, which is appropriate
  for smaller sample sizes. If "Chisq" is selected, the function will
  perform a chi-squared test, which is more suitable for larger sample
  sizes. Invalid values will result in an error.

- ...:

  Additional arguments to be passed to the underlying test functions,
  such as
  [`nlWaldtest`](https://rdrr.io/pkg/nlWaldTest/man/nlWaldtest.html) for
  long-run tests or
  [`linearHypothesis`](https://rdrr.io/pkg/car/man/linearHypothesis.html)
  for short-run tests. These arguments can include options for
  controlling the behavior of the tests, such as specifying the type of
  test statistic or adjusting for multiple comparisons.

## Value

A list with class "kardl" containing the following components:

- `long_wald_summary:` A data frame containing the Wald test results for
  the long-run variables, including F-statistic, p-value, degrees of
  freedom, and residual degrees of freedom.

- `long_hypotheses:` A list containing the null and alternative
  hypotheses for the long-run variables.

- `short_wald_summary:` A data frame containing the Wald test results
  for the short-run variables, including F-statistic, p-value, degrees
  of freedom, residual degrees of freedom, and sum of squares.

- `short_hypotheses:` A list containing the null and alternative
  hypotheses for the short-run variables.

## Details

This function performs symmetry tests on non-linear KARDL models to
assess whether the effects of positive and negative changes in
independent variables are statistically different.

This function evaluates whether the inclusion of a particular variable
in the model follows a linear relationship or exhibits a non-linear
pattern. By analyzing the behavior of the variable, the function helps
to identify if the relationship between the variable and the outcome of
interest adheres to a straight-line assumption or if it deviates,
indicating a non-linear interaction. This distinction is important in
model specification, as it ensures that the variable is appropriately
represented, which can enhance the model's accuracy and predictive
performance.

## References

Shin, Y., Yu, B., & Greenwood-Nimmo, M. (2014). Modelling asymmetric
cointegration and dynamic multipliers in a nonlinear ARDL framework.
Festschrift in honor of Peter Schmidt: Econometric methods and
applications, 281-314.

## See also

[`kardl`](https://karamelikli.github.io/kardl/reference/kardl.md),
[`pssf`](https://karamelikli.github.io/kardl/reference/pssf.md),
[`psst`](https://karamelikli.github.io/kardl/reference/psst.md),
[`ecm`](https://karamelikli.github.io/kardl/reference/ecm.md),
[`narayan`](https://karamelikli.github.io/kardl/reference/narayan.md)

## Examples

``` r
kardl_model <- kardl(
  DriversKilled ~ Lasym(drivers + PetrolPrice) + Sas(PetrolPrice) +
    deterministic(law) + trend,
  Seatbelts
)
ast <- symmetrytest(kardl_model)
ast
#> kardl Symmetry Test 
#> 
#> Long-run:
#>             Df Sum of Sq Mean Sq F value Pr(>F)
#> PetrolPrice  1    14.688  14.688  0.1154 0.7345
#> drivers      1   215.576 215.576  1.6940 0.1948
#> 
#> Short-run:
#>             Df Sum of Sq Mean Sq F value Pr(>F)
#> PetrolPrice  1    161.38  161.38  1.2681 0.2616
#> 
# Detailed results of the test:
summary(ast)
#> Symmetry Test Summary
#> 
#> Long-run:
#>             Df Sum of Sq Mean Sq F value Pr(>F)
#> PetrolPrice  1    14.688  14.688  0.1154 0.7345
#> drivers      1   215.576 215.576  1.6940 0.1948
#> 
#> Hypotheses:
#> 
#>   PetrolPrice
#>    H0: - Coef(L1.PetrolPrice_POS)/Coef(L1.DriversKilled) = - Coef(L1.PetrolPrice_NEG)/Coef(L1.DriversKilled)
#>    H1: At least one coefficient differs from zero.
#>    Decision: Fail to Reject H0 at 5% level. Indicating long-run symmetry for variable PetrolPrice.
#> 
#>   drivers
#>    H0: - Coef(L1.drivers_POS)/Coef(L1.DriversKilled) = - Coef(L1.drivers_NEG)/Coef(L1.DriversKilled)
#>    H1: At least one coefficient differs from zero.
#>    Decision: Fail to Reject H0 at 5% level. Indicating long-run symmetry for variable drivers.
#> 
#> 
#> Short-run:
#>             Df Sum of Sq Mean Sq F value Pr(>F)
#> PetrolPrice  1    161.38  161.38  1.2681 0.2616
#> 
#> Hypotheses:
#> 
#>   PetrolPrice
#>    H0: Coef(L0.d.PetrolPrice_POS) = Coef(L0.d.PetrolPrice_NEG)
#>    H1: Coef(L0.d.PetrolPrice_POS) ≠ Coef(L0.d.PetrolPrice_NEG)
#>    Decision: Fail to Reject H0 at 5% level. Indicating short-run symmetry for variable PetrolPrice.
#> 
# The null hypothesis of the test is that the model is symmetric, while the
# alternative hypothesis is that the model is asymmetric. The test statistic
# and p-value are provided in the output. If the p-value is less than a
# chosen significance level (e.g., 0.05), we reject the null hypothesis and
# conclude that there is evidence of asymmetry in the model.

# The default significance level is 0.05, but you can specify a different
# level using the 'level' argument in the summary function. For example, to
# use a significance level of 0.01, you can use the following code:
summary(ast, level = 0.01)
#> Symmetry Test Summary
#> 
#> Long-run:
#>             Df Sum of Sq Mean Sq F value Pr(>F)
#> PetrolPrice  1    14.688  14.688  0.1154 0.7345
#> drivers      1   215.576 215.576  1.6940 0.1948
#> 
#> Hypotheses:
#> 
#>   PetrolPrice
#>    H0: - Coef(L1.PetrolPrice_POS)/Coef(L1.DriversKilled) = - Coef(L1.PetrolPrice_NEG)/Coef(L1.DriversKilled)
#>    H1: At least one coefficient differs from zero.
#>    Decision: Fail to Reject H0 at 1% level. Indicating long-run symmetry for variable PetrolPrice.
#> 
#>   drivers
#>    H0: - Coef(L1.drivers_POS)/Coef(L1.DriversKilled) = - Coef(L1.drivers_NEG)/Coef(L1.DriversKilled)
#>    H1: At least one coefficient differs from zero.
#>    Decision: Fail to Reject H0 at 1% level. Indicating long-run symmetry for variable drivers.
#> 
#> 
#> Short-run:
#>             Df Sum of Sq Mean Sq F value Pr(>F)
#> PetrolPrice  1    161.38  161.38  1.2681 0.2616
#> 
#> Hypotheses:
#> 
#>   PetrolPrice
#>    H0: Coef(L0.d.PetrolPrice_POS) = Coef(L0.d.PetrolPrice_NEG)
#>    H1: Coef(L0.d.PetrolPrice_POS) ≠ Coef(L0.d.PetrolPrice_NEG)
#>    Decision: Fail to Reject H0 at 1% level. Indicating short-run symmetry for variable PetrolPrice.
#> 

# To get symmetry test results in long-run, you can use the following code:
kardl_extract(ast, what = "long_wald_summary")
#> Long-run:
#>             Df Sum of Sq Mean Sq F value Pr(>F)
#> PetrolPrice  1    14.688  14.688  0.1154 0.7345
#> drivers      1   215.576 215.576  1.6940 0.1948

# To get symmetry test results in short-run, you can use the following code:
kardl_extract(ast, what = "short_wald_summary")
#> Short-run:
#>             Df Sum of Sq Mean Sq F value Pr(>F)
#> PetrolPrice  1    161.38  161.38  1.2681 0.2616

# To get the null and alternative hypotheses of the test in long-run,
# you can use the following code:
kardl_extract(ast, what = "long_hypotheses")
#> Hypotheses:
#> 
#> 
#> Variable: PetrolPrice 
#> H0: - Coef(L1.PetrolPrice_POS)/Coef(L1.DriversKilled) = - Coef(L1.PetrolPrice_NEG)/Coef(L1.DriversKilled) 
#> H1: At least one coefficient differs from zero. 
#> 
#> 
#> Variable: drivers 
#> H0: - Coef(L1.drivers_POS)/Coef(L1.DriversKilled) = - Coef(L1.drivers_NEG)/Coef(L1.DriversKilled) 
#> H1: At least one coefficient differs from zero. 
#> 

# To get the null and alternative hypotheses of the test in short-run,
# you can use the following code:
kardl_extract(ast, what = "short_hypotheses")
#> Hypotheses:
#> 
#> 
#> Variable: PetrolPrice 
#> H0: Coef(L0.d.PetrolPrice_POS) = Coef(L0.d.PetrolPrice_NEG) 
#> H1: Coef(L0.d.PetrolPrice_POS) ≠ Coef(L0.d.PetrolPrice_NEG) 
#> 

# Alternatively, you can also use the symmetrytest function with the
# component argument to specify whether you want to test for long-run or
# short-run symmetry. For example, to test for long-run symmetry, you can use
# the following code:
symmetrytest(kardl_model, component = "longrun")
#> kardl Symmetry Test 
#> 
#> Long-run:
#>             Df Sum of Sq Mean Sq F value Pr(>F)
#> PetrolPrice  1    14.688  14.688  0.1154 0.7345
#> drivers      1   215.576 215.576  1.6940 0.1948
#> 

# To test for short-run symmetry, you can use the following code:
symmetrytest(kardl_model, component = "shortrun")
#> kardl Symmetry Test 
#> 
#> Short-run:
#>             Df Sum of Sq Mean Sq F value Pr(>F)
#> PetrolPrice  1    161.38  161.38  1.2681 0.2616
#> 

# If you want to test for symmetry with respect to a specific variable,
# you can use the selected_vars argument in the symmetrytest function. For
# example, to test for symmetry with respect to the drivers variable, you can
# use the following code:
symmetrytest(kardl_model, selected_vars = "drivers")
#> kardl Symmetry Test 
#> 
#> Long-run:
#>         Df Sum of Sq Mean Sq F value Pr(>F)
#> drivers  1    215.58  215.58   1.694 0.1948
#> 

# To test for symmetry with respect to multiple variables, you can provide
# a vector of variable names to the selected_vars argument. For example, to
# test for symmetry with respect to both drivers and PetrolPrice, you can use
# the following code:
symmetrytest(kardl_model, selected_vars = c("drivers", "PetrolPrice"))
#> kardl Symmetry Test 
#> 
#> Long-run:
#>             Df Sum of Sq Mean Sq F value Pr(>F)
#> drivers      1   215.576 215.576  1.6940 0.1948
#> PetrolPrice  1    14.688  14.688  0.1154 0.7345
#> 
#> Short-run:
#>             Df Sum of Sq Mean Sq F value Pr(>F)
#> PetrolPrice  1    161.38  161.38  1.2681 0.2616
#> 

# Finally, you can also specify the type of test statistic to be used in the
# symmetry test. By default, the function uses the Wald test F statistic,
# but you can also choose to use the chi-squared test statistic.
# For example, to use the chi-squared test statistic, you can use the
# following code:
symmetrytest(kardl_model, type = "Chisq")
#> kardl Symmetry Test 
#> 
#> Long-run:
#>             Df  Chisq Pr(>Chisq)
#> PetrolPrice  1 0.1154     0.7341
#> drivers      1 1.6940     0.1931
#> 
#> Short-run:
#>             Df Sum of Sq  Chisq Pr(>Chisq)
#> PetrolPrice  1    161.38 1.2681     0.2601
#> 
```

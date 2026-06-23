# Narayan Bounds Test

This function performs the Narayan test, which is designed to assess
cointegration using critical values specifically tailored for small
sample sizes. Unlike traditional cointegration tests that may rely on
asymptotic distributions, the Narayan test adjusts for the limitations
of small samples, providing more accurate results in such contexts. This
makes the test particularly useful for studies with fewer observations,
as it accounts for sample size constraints when determining the presence
of a long-term equilibrium relationship between variables.

## Usage

``` r
narayan(kardl_model, case = "auto", signif_level = "auto", ...)
```

## Arguments

- kardl_model:

  A fitted KARDL model object of class 'kardl_lm' created using the
  [`kardl`](https://karamelikli.github.io/kardl/reference/kardl.md)
  function.

- case:

  Numeric or character. Specifies the case of the test to be used in the
  function. Acceptable values are 1, 2, 3, 4, 5, and "auto". If "auto"
  is chosen, the function determines the case automatically based on the
  model's characteristics. Invalid values will result in an error.

  - `1`: No intercept and no trend. This case is not supported by the
    Narayan test.

  - `2`: Restricted intercept and no trend.

  - `3`: Unrestricted intercept and no trend.

  - `4`: Unrestricted intercept and restricted trend.

  - `5`: Unrestricted intercept and unrestricted trend.

- signif_level:

  Character or numeric. Specifies the significance level to be used in
  the function. Acceptable values are "auto", "0.10", "0.1", "0.05",
  "0.025", and "0.01". If a numeric value is provided, it will be
  converted to a character string.

  When `"auto"` is selected, the function determines the significance
  level sequentially, starting from the most stringent level (`"0.01"`)
  and proceeding to `"0.025"`, `"0.05"`, and `"0.10"` until a suitable
  level is identified. Invalid values will result in an error.

- ...:

  Additional arguments (currently not used).

## Value

A list with class "htest" containing the following components:

- `statistic`: The calculated F-statistic for the test.

- `case_txt`: A character string describing the case used for the test,
  based on the specified case parameter.

- `alternative`: A character string describing the alternative
  hypothesis of the test.

- `sample.size`: The number of observations used in the test.

- `var_names`: A character vector containing the names of the dependent
  variable and independent variables used in the test.

- `k`: The number of independent variables (excluding the dependent
  variable) included in the test.

- `sig`: The significance level used for the test, either specified by
  the user or determined automatically.

- `notes`: A character vector containing any notes or warnings related
  to the test, such as the suitability of the test for small sample
  sizes or any adjustments made to the case based on the model's
  characteristics.

## Hypothesis testing

The null hypothesis (H0) of the F Bound test is that there is no
cointegration among the variables in the model. In other words, it tests
whether the long-term relationship between the variables is
statistically significant. If the calculated F-statistic exceeds the
upper critical value, we reject the null hypothesis and conclude that
there is cointegration. Conversely, if the F-statistic falls below the
lower critical value, we fail to reject the null hypothesis, indicating
no evidence of cointegration. If the F-statistic lies between the two
critical values, the result is inconclusive.

\$\$ \Delta {y}\_t = \psi + \varphi t + \eta \_0 {y}\_{t-1} +
\sum\_{i=1}^{k} { \eta \_i {x}\_{i,t-1} } + \sum\_{j=1}^{p} {
\gamma\_{j} \Delta {y}\_{t-j} }+ \sum\_{i=1}^{k} {\sum\_{j=0}^{q_i} {
\beta\_{ij} \Delta {x}\_{i,t-j} } }+ e_t \$\$

- Cases 1, 3, 5::

  \$\$\mathbf{H\_{0}:} \eta_0 = \eta_1 = \dots = \eta_k = 0\$\$
  \$\$\mathbf{H\_{1}:} \eta\_{0} \neq \eta\_{1} \neq \dots \neq
  \eta\_{k} \neq 0\$\$

- Case 2::

  \$\$\mathbf{H\_{0}:} \eta_0 = \eta_1 = \dots = \eta_k = \psi = 0\$\$
  \$\$\mathbf{H\_{1}:} \eta\_{0} \neq \eta\_{1} \neq \dots \neq
  \eta\_{k} \neq \psi \neq 0\$\$

- Case 4::

  \$\$\mathbf{H\_{0}:} \eta_0 = \eta_1 = \dots = \eta_k = \varphi =
  0\$\$ \$\$\mathbf{H\_{1}:} \eta\_{0} \neq \eta\_{1} \neq \dots \neq
  \eta\_{k} \neq \varphi \neq 0\$\$

## References

Narayan, P. K. (2005). The saving and investment nexus for China:
evidence from cointegration tests. Applied economics, 37(17), 1979-1990.

## See also

[`pssf`](https://karamelikli.github.io/kardl/reference/pssf.md)
[`psst`](https://karamelikli.github.io/kardl/reference/psst.md)
[`ecm`](https://karamelikli.github.io/kardl/reference/ecm.md)

## Examples

``` r
kardl_model<-kardl(
                   CPI~ER+PPI+asym(ER)+deterministic(covid)+trend,
                   imf_example_data,
                   mode=c(1,2,3,0))
my_test<-narayan(kardl_model, case=3, signif_level="auto")
# Getting the results of the test.
my_test
#> 
#>  Narayan F Test for Cointegration
#> 
#> data:  model
#> F = 10.204
#> alternative hypothesis: Cointegrating relationship exists
#> 
# Getting details of the test.
my_summary<-summary(my_test)
my_summary
#> 
#> ========================================
#> KARDL Cointegration Test Results
#> ========================================
#> 
#>  Decision: Reject H0 → Cointegration (at 1% level)
#> 
#>  Test Statistic:
#>   F: 10.2035571
#> 
#>  Critical Values (Lower & Upper Bounds):
#>           L     U
#>   10% 3.588 4.605
#>   5%  4.203 5.320
#>   1%  5.620 6.908
#> 
#> 
#>  Comparison:
#>   At the 1% significance level, F (10.2035571) exceeds the upper bound (6.908).
#>   This indicates that the variables tend to move together over  time.
#>   Conclusion: There is strong evidence of a long-run relationship  (cointegration).
#> 
#>  Hypotheses:
#> H0: Coef(L1.CPI) = Coef(L1.ER_POS) = Coef(L1.ER_NEG) = Coef(L1.PPI) = 0 
#> H1: Not all of Coef(L1.CPI), Coef(L1.ER_POS), Coef(L1.ER_NEG), Coef(L1.PPI) are zero. 
#> 
#>  Model Details:
#>   Number of regressors (k): 3
#>   Case: V 
#> 
#> 
#>  Note:The number of observations exceeds the maximum limit for the critical valuestable. Using the critical values for 80 observations.
#> ========================================

# Getting the critical values of the test.
kardl_extract(my_summary, what = "critical_values")
#>          L     U
#> 0.10 3.588 4.605
#> 0.05 4.203 5.320
#> 0.01 5.620 6.908




# Using magrittr :

library(magrittr)
imf_example_data %>%
  kardl(CPI ~ ER + PPI + asym(ER) + deterministic(covid) + trend,
    mode = c(1, 2, 3, 0), data = .
  ) %>%
  narayan()
#> 
#>  Narayan F Test for Cointegration
#> 
#> data:  model
#> F = 10.204
#> alternative hypothesis: Cointegrating relationship exists
#> 

# Getting details of the test results using magrittr:
imf_example_data %>%
  kardl(CPI ~ ER + PPI + asym(ER) + deterministic(covid) + trend,
    mode = c(1, 2, 3, 0), data = .
  ) %>%
  narayan() %>%
  summary()
#> 
#> ========================================
#> KARDL Cointegration Test Results
#> ========================================
#> 
#>  Decision: Reject H0 → Cointegration (at 1% level)
#> 
#>  Test Statistic:
#>   F: 10.2035571
#> 
#>  Critical Values (Lower & Upper Bounds):
#>           L     U
#>   10% 3.588 4.605
#>   5%  4.203 5.320
#>   1%  5.620 6.908
#> 
#> 
#>  Comparison:
#>   At the 1% significance level, F (10.2035571) exceeds the upper bound (6.908).
#>   This indicates that the variables tend to move together over  time.
#>   Conclusion: There is strong evidence of a long-run relationship  (cointegration).
#> 
#>  Hypotheses:
#> H0: Coef(L1.CPI) = Coef(L1.ER_POS) = Coef(L1.ER_NEG) = Coef(L1.PPI) = 0 
#> H1: Not all of Coef(L1.CPI), Coef(L1.ER_POS), Coef(L1.ER_NEG), Coef(L1.PPI) are zero. 
#> 
#>  Model Details:
#>   Number of regressors (k): 3
#>   Case: V 
#> 
#> 
#>  Note:The number of observations exceeds the maximum limit for the critical valuestable. Using the critical values for 80 observations.
#> ========================================
```

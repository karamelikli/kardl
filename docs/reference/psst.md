# Pesaran, Shin, and Smith t Bounds Test

This function performs the Pesaran t Bound test

## Usage

``` r
psst(kardl_model, ...)
```

## Arguments

- kardl_model:

  A fitted KARDL model object of class 'kardl_lm' created using the
  [`kardl`](https://karamelikli.github.io/kardl/reference/kardl.md)
  function.

- ...:

  Additional arguments (currently not used).

## Value

The function returns an object of class "htest" containing the following
components:

- statistic:

  The calculated t-statistic for the test.

- method:

  A description of the test performed.

- data.name:

  The name of the data used in the test.

- k:

  The number of independent variables in the model.

- notes:

  Any notes or warnings related to the test results, such as sample size
  considerations or adjustments made to the case based on model
  characteristics.

- sig:

  The significance level used for the test, either specified by the user
  or determined automatically.

- alternative:

  The alternative hypothesis being tested.

- case:

  The case used for the test, either specified by the user or determined
  automatically based on the model's characteristics.

## Details

This function performs the Pesaran, Shin, and Smith (PSS) t Bound test,
which is used to detect the existence of a long-term relationship
(cointegration) between variables in an autoregressive distributed lag
(ARDL) model. The t Bound test specifically focuses on the significance
of the coefficient of the lagged dependent variable, helping to assess
whether the variable reverts to its long-term equilibrium after
short-term deviations. The test provides critical values for both upper
and lower bounds. If the t-statistic falls within the appropriate range,
it confirms the presence of cointegration. This test is particularly
useful when working with datasets containing both stationary and
non-stationary variables.

## Hypothesis testing

The PSS t Bound test evaluates the null hypothesis that the long-run
coefficients of the model are equal to zero against the alternative
hypothesis that at least one of them is non-zero. The test is conducted
under different cases, depending on the model specification.

\$\$ \Delta {y}\_t = \psi + \varphi t + \eta \_0 {y}\_{t-1} +
\sum\_{i=1}^{k} { \eta \_i {x}\_{i,t-1} } + \sum\_{j=1}^{p} {
\gamma\_{j} \Delta {y}\_{t-j} }+ \sum\_{i=1}^{k} {\sum\_{j=0}^{q_i} {
\beta\_{ij} \Delta {x}\_{i,t-j} } }+ e_t \$\$

\$\$\mathbf{H\_{0}:} \eta_0 = 0\$\$ \$\$\mathbf{H\_{1}:} \eta\_{0} \neq
0\$\$

## References

Pesaran, M. H., Shin, Y. and Smith, R. (2001), "Bounds Testing
Approaches to the Analysis of Level Relationship", Journal of Applied
Econometrics, 16(3), 289-326.

## See also

[`pssf`](https://karamelikli.github.io/kardl/reference/pssf.md)
[`ecm`](https://karamelikli.github.io/kardl/reference/ecm.md)
[`narayan`](https://karamelikli.github.io/kardl/reference/narayan.md)

## Examples

``` r
kardl_model<-kardl(
                   CPI~ER+PPI+asym(ER)+deterministic(covid)+trend,
                   imf_example_data,
                   mode=c(1,2,3,0))
my_test<-psst(kardl_model)
# Getting the results of the test.
my_test
#> 
#>  Pesaran-Shin-Smith (PSS) Bounds t-test for cointegration
#> 
#> data:  model
#> t = -3.872
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
#>  Decision: Reject H0 → Weak evidence of cointegration (at 10% level)
#> 
#>  Test Statistic:
#>   t: -3.8719990
#> 
#>  Critical Values (Lower & Upper Bounds):
#>            L     U
#>   10%  -3.13 -3.84
#>   5%   -3.41 -4.16
#>   2.5% -3.65 -4.42
#>   1%   -3.96 -4.73
#> 
#> 
#>  Comparison:
#>   At the 10% significance level, t (3.8719990) exceeds the upper bound (3.84).
#>   This indicates that the variables tend to move together over  time.
#>   Conclusion: There is strong evidence of a long-run relationship  (cointegration).
#> 
#>  Hypotheses:
#> H0: Coef(L1.CPI) = 0 
#> H1: Coef(L1.CPI) ≠ 0 
#> 
#>  Model Details:
#>   Number of regressors (k): 3
#>   Case: V 
#> 
#> ========================================

# Getting the critical values of the test.
kardl_extract(my_summary, what = "critical_values")
#>           L     U
#> 0.10  -3.13 -3.84
#> 0.05  -3.41 -4.16
#> 0.025 -3.65 -4.42
#> 0.01  -3.96 -4.73




# Using magrittr :

library(magrittr)
imf_example_data %>%
  kardl(CPI ~ ER + PPI + asym(ER) + deterministic(covid) + trend,
    mode = c(1, 2, 3, 0), data = .
  ) %>%
  psst()
#> 
#>  Pesaran-Shin-Smith (PSS) Bounds t-test for cointegration
#> 
#> data:  model
#> t = -3.872
#> alternative hypothesis: Cointegrating relationship exists
#> 

# Getting details of the test results using magrittr:
imf_example_data %>%
  kardl(CPI ~ ER + PPI + asym(ER) + deterministic(covid) + trend,
    mode = c(1, 2, 3, 0), data = .
  ) %>%
  psst() %>%
  summary()
#> 
#> ========================================
#> KARDL Cointegration Test Results
#> ========================================
#> 
#>  Decision: Reject H0 → Weak evidence of cointegration (at 10% level)
#> 
#>  Test Statistic:
#>   t: -3.8719990
#> 
#>  Critical Values (Lower & Upper Bounds):
#>            L     U
#>   10%  -3.13 -3.84
#>   5%   -3.41 -4.16
#>   2.5% -3.65 -4.42
#>   1%   -3.96 -4.73
#> 
#> 
#>  Comparison:
#>   At the 10% significance level, t (3.8719990) exceeds the upper bound (3.84).
#>   This indicates that the variables tend to move together over  time.
#>   Conclusion: There is strong evidence of a long-run relationship  (cointegration).
#> 
#>  Hypotheses:
#> H0: Coef(L1.CPI) = 0 
#> H1: Coef(L1.CPI) ≠ 0 
#> 
#>  Model Details:
#>   Number of regressors (k): 3
#>   Case: V 
#> 
#> ========================================
```

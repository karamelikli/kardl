# PSS t Bound Test

This function performs the Pesaran t Bound test

## Usage

``` r
psst(kmodel, case = 3, signif_level = "auto")
```

## Arguments

- kmodel:

  A fitted KARDL model object of class 'kardl_lm' created using the
  [`kardl`](kardl.md) function.

- case:

  Numeric or character. Specifies the case of the test to be used in the
  function. Acceptable values are 1, 2, 3, 4, 5, and "auto". If "auto"
  is chosen, the function determines the case automatically based on the
  model's characteristics. Invalid values will result in an error.

  - `1`: No intercept and no trend

  - `2`: Restricted intercept and no trend

  - `3`: Unrestricted intercept and no trend

  - `4`: Unrestricted intercept and restricted trend

  - `5`: Unrestricted intercept and unrestricted trend

- signif_level:

  Character or numeric. Specifies the significance level to be used in
  the function. Acceptable values are "auto", "0.10", "0.1", "0.05",
  "0.025", and "0.01". If a numeric value is provided, it will be
  converted to a character string. If "auto" is chosen, the function
  determines the significance level automatically. Invalid values will
  result in an error.

## Value

The function returns an object of class "htest" containing the following
components:

- statistic: The calculated t-statistic for the test.

- method: A description of the test performed.

- data.name: The name of the data used in the test.

- k: The number of independent variables in the model.

- notes: Any notes or warnings related to the test results, such as
  sample size considerations or adjustments made to the case based on
  model characteristics.

- sig: The significance level used for the test, either specified by the
  user or determined automatically.

- alternative: The alternative hypothesis being tested.

- case: The case used for the test, either specified by the user or
  determined automatically based on the model's characteristics.

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

[`pssf`](pssf.md) [`ecm`](ecm.md) [`narayan`](narayan.md)

## Examples

``` r
kardl_model<-kardl(imf_example_data,
                   CPI~ER+PPI+asym(ER)+deterministic(covid)+trend,
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
#> Pesaran-Shin-Smith (PSS) Bounds t-test for cointegration 
#> t  =  -3.871999 
#> k =  3 
#> 
#> Hypotheses:
#> H0: Coef(LK1_CPI) = 0 
#> H1: Coef(LK1_CPI)≠ 0 
#> 
#> Test Decision:  Reject H0 → Cointegration (at 1% level) 
#> 
#> Critical Values (Case  V ):
#>           L     U
#> 0.10  -3.13 -3.84
#> 0.05  -3.41 -4.16
#> 0.025 -3.65 -4.42
#> 0.01  -3.96 -4.73
#> 
#> Notes:
#>    • Trend detected in the model. Case automatically adjusted to 5 (unrestricted intercept and trend).
#> 

# Getting the critical values of the test.
my_summary$crit_vals
#>           L     U
#> 0.10  -3.13 -3.84
#> 0.05  -3.41 -4.16
#> 0.025 -3.65 -4.42
#> 0.01  -3.96 -4.73




# Using magrittr :

library(magrittr)
imf_example_data %>% kardl(CPI~ER+PPI+asym(ER)+deterministic(covid)+trend,
                           mode=c(1,2,3,0)) %>% psst()
#> 
#>  Pesaran-Shin-Smith (PSS) Bounds t-test for cointegration
#> 
#> data:  model
#> t = -3.872
#> alternative hypothesis: Cointegrating relationship exists
#> 

# Getting details of the test results using magrittr:
imf_example_data %>% kardl(CPI~ER+PPI+asym(ER)+deterministic(covid)+trend,
                           mode=c(1,2,3,0)) %>% psst() %>% summary()
#> Pesaran-Shin-Smith (PSS) Bounds t-test for cointegration 
#> t  =  -3.871999 
#> k =  3 
#> 
#> Hypotheses:
#> H0: Coef(LK1_CPI) = 0 
#> H1: Coef(LK1_CPI)≠ 0 
#> 
#> Test Decision:  Reject H0 → Cointegration (at 1% level) 
#> 
#> Critical Values (Case  V ):
#>           L     U
#> 0.10  -3.13 -3.84
#> 0.05  -3.41 -4.16
#> 0.025 -3.65 -4.42
#> 0.01  -3.96 -4.73
#> 
#> Notes:
#>    • Trend detected in the model. Case automatically adjusted to 5 (unrestricted intercept and trend).
#> 

```

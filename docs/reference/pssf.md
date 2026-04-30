# Pesaran, Shin, and Smith Bounds F-Test

This function performs the Pesaran, Shin, and Smith (PSS) F Bound test
to assess the presence of a long-term relationship (cointegration)
between variables in the context of an autoregressive distributed lag
(ARDL) model. The PSS F Bound test examines the joint significance of
lagged levels of the variables in the model. It provides critical values
for both the upper and lower bounds, which help determine whether the
variables are cointegrated. If the calculated F-statistic falls outside
these bounds, it indicates the existence of a long-term equilibrium
relationship. This test is particularly useful when the underlying data
includes a mix of stationary and non-stationary variables.

## Usage

``` r
pssf(kmodel, case = 3, signif_level = "auto")
```

## Arguments

- kmodel:

  A fitted KARDL model object of class 'kardl_lm' created using the
  [`kardl`](https://karamelikli.github.io/kardl/reference/kardl.md)
  function.

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
  converted to a character string.

  When `"auto"` is selected, the function determines the significance
  level sequentially, starting from the most stringent level (`"0.01"`)
  and proceeding to `"0.025"`, `"0.05"`, and `"0.10"` until a suitable
  level is identified. Invalid values will result in an error.

## Value

A list with class "htest" containing the following components:

- `statistic`: The calculated F-statistic for the test.

- `caseTxt`: A character string describing the case used for the test,
  based on the specified case parameter.

- `alternative`: A character string describing the alternative
  hypothesis of the test.

- `sample.size`: The number of observations used in the test.

- `varnames`: A character vector containing the names of the dependent
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

Pesaran, M. H., Shin, Y. and Smith, R. (2001), "Bounds Testing
Approaches to the Analysis of Level Relationship", Journal of Applied
Econometrics, 16(3), 289-326.

## See also

[`psst`](https://karamelikli.github.io/kardl/reference/psst.md)
[`ecm`](https://karamelikli.github.io/kardl/reference/ecm.md)
[`narayan`](https://karamelikli.github.io/kardl/reference/narayan.md)

## Examples

``` r
kardl_model<-kardl(imf_example_data,
                   CPI~ER+PPI+asym(ER)+deterministic(covid)+trend,
                   mode=c(1,2,3,0))
my_pssF<-pssf(kardl_model)
# Getting the results of the test.
my_pssF
#> 
#>  Pesaran-Shin-Smith (PSS) Bounds F-test for cointegration
#> 
#> data:  model
#> F = 10.204
#> alternative hypothesis: Cointegrating relationship exists
#> 
# Getting details of the test.
my_summary<-summary(my_pssF)
my_summary
#> Pesaran-Shin-Smith (PSS) Bounds F-test for cointegration 
#> F  =  10.20356 
#> k =  3 
#> 
#> Hypotheses:
#> H0: Coef(L1.CPI) = Coef(L1.ER_POS) = Coef(L1.ER_NEG) = Coef(L1.PPI) = 0 
#> H1: Coef(L1.CPI) ≠ Coef(L1.ER_POS) ≠ Coef(L1.ER_NEG) ≠ Coef(L1.PPI)≠ 0 
#> 
#> Test Decision:  Reject H0 → Cointegration (at 1% level) 
#> 
#> Critical Values (Case  V ):
#>          L    U
#> 0.10  3.47 4.45
#> 0.05  4.01 5.07
#> 0.025 4.52 5.62
#> 0.01  5.17 6.36
#> 
#> Notes:
#>    • Trend detected in the model. Case automatically adjusted to 5 (unrestricted intercept and trend).
#> 

# Getting the critical values of the test.
my_summary$crit_vals
#>          L    U
#> 0.10  3.47 4.45
#> 0.05  4.01 5.07
#> 0.025 4.52 5.62
#> 0.01  5.17 6.36


# Using magrittr :

library(magrittr)
   imf_example_data %>%
   kardl(CPI~ER+PPI+asym(ER)+deterministic(covid)+trend,
                           mode=c(1,2,3,0)) %>% pssf()
#> 
#>  Pesaran-Shin-Smith (PSS) Bounds F-test for cointegration
#> 
#> data:  model
#> F = 10.204
#> alternative hypothesis: Cointegrating relationship exists
#> 

   # Getting details of the test results using magrittr:
   imf_example_data %>%
   kardl(CPI~ER+PPI+asym(ER)+deterministic(covid)+trend,
                           mode=c(1,2,3,0)) %>%
   pssf() %>% summary()
#> Pesaran-Shin-Smith (PSS) Bounds F-test for cointegration 
#> F  =  10.20356 
#> k =  3 
#> 
#> Hypotheses:
#> H0: Coef(L1.CPI) = Coef(L1.ER_POS) = Coef(L1.ER_NEG) = Coef(L1.PPI) = 0 
#> H1: Coef(L1.CPI) ≠ Coef(L1.ER_POS) ≠ Coef(L1.ER_NEG) ≠ Coef(L1.PPI)≠ 0 
#> 
#> Test Decision:  Reject H0 → Cointegration (at 1% level) 
#> 
#> Critical Values (Case  V ):
#>          L    U
#> 0.10  3.47 4.45
#> 0.05  4.01 5.07
#> 0.025 4.52 5.62
#> 0.01  5.17 6.36
#> 
#> Notes:
#>    • Trend detected in the model. Case automatically adjusted to 5 (unrestricted intercept and trend).
#> 

```

# Compute Long-Run Multipliers from a kardl Model

This function calculates the long-run parameters of a KARDL model
estimated using the `kardl` function. The long-run parameters are
calculated by dividing the negative of the coefficients of the
independent variables by the coefficient of the dependent variable. If
an intercept is included in the model, it is also standardized by
dividing it by the negative of the long-run parameter of the dependent
variable.

## Usage

``` r
kardl_longrun(model)
```

## Arguments

- model:

  An object of class `kardl` estimated using the `kardl` function.

## Value

An object of class `kardl_long_run`, which is a list containing:

- `coefficients`: A named vector of long-run multipliers.

- `residuals`: A vector of residuals from the long-run model.

- `effects`: A vector of effects from the long-run model.

- `rank`: The rank of the long-run model.

- `fitted.values`: A vector of fitted values from the long-run model.

- `assign`: A vector indicating the assignment of coefficients to terms
  in the long-run model.

- `qr`: The QR decomposition of the design matrix of the long-run model.

- `df.residual`: The degrees of freedom of the residuals of the long-run
  model.

- `xlevels`: A list of factor levels used in the long-run model.

- `call`: The matched call used to create the long-run model.

- `terms`: The terms object of the long-run model.

- `model`: The data frame used in the long-run model.

## Details

The function also calculates the standard errors of the long-run
multipliers using the delta method, which accounts for the covariance
between the coefficients. The fitted values and residuals of the
long-run model are calculated based on the original data and the
long-run multipliers.

The function returns an object of class `kardl_long_run`, which contains
the long-run multipliers, their standard errors, t-statistics, p-values,
fitted values, residuals, and other relevant information for further
analysis and diagnostics.

Note that the fitted values and residuals from the long-run model are
not centered (i.e., they do not have a mean of zero) by design, which
means that diagnostic plots and residual-based tests may not be valid
for this model. The primary focus of this function is on the estimation
of the long-run multipliers and their associated statistics.

The long-run multipliers are calculated using the formula: \$\$LRM_i =
-\frac{\eta_i}{\eta_0}\$\$.

t-values and p-values are calculated using the standard errors obtained
from the delta method, which accounts for the covariance between the
coefficients. Delta method formula for standard errors of long-run
multipliers: \$\$SE(LR_i) = \sqrt{(A^2) \cdot Var(\eta_i) + 2 \cdot A
\cdot B \cdot Cov(\eta_i, \eta_0) + (B^2) \cdot Var(\eta_0)}\$\$ where
\$\$A = \frac{\partial LRM_i}{\partial \eta_i} = -\frac{1}{\eta_0}\$\$
and \$\$B = \frac{\partial LRM_i}{\partial \eta_0} =
\frac{\eta_i}{\eta_0^2}\$\$ . Hence, \\\eta_i\\ is the coefficient of
the independent variable and \\\eta_0\\ is the coefficient of the
dependent variable in the original KARDL model.

## See also

[`kardl`](https://karamelikli.github.io/kardl/reference/kardl.md),
[`pssf`](https://karamelikli.github.io/kardl/reference/pssf.md),
[`psst`](https://karamelikli.github.io/kardl/reference/psst.md)

## Examples

``` r
kardl_model<-kardl(imf_example_data,
                   CPI~ER+PPI+asym(ER)+deterministic(covid)+trend,
                   mode=c(1,2,3,0))
long<-kardl_longrun(kardl_model)

# Calculate the long-run multipliers
long
#> 
#> Call:
#> kardl_longrun(model = kardl_model)
#> 
#> Coefficients:
#> L1.ER_POS  L1.ER_NEG     L1.PPI  
#>    0.8249     2.0830     2.6528  
#> 
# Details of the long-run multipliers
summary(long)
#> 
#> Call:
#> kardl_longrun(model = kardl_model)
#> 
#> Estimation type:
#> Long-run multipliers 
#> 
#> Coefficients:
#>           Estimate Std. Error t value  Pr(>|t|)    
#> L1.ER_POS  0.82494    0.18150  4.5453 7.064e-06 ***
#> L1.ER_NEG  2.08297    0.48083  4.3320 1.825e-05 ***
#> L1.PPI     2.65278    0.84724  3.1311  0.001855 ** 
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Note:
#> Coefficients, standard errors, t-statistics and p-values are reliably estimated.
#> Fitted values and residuals are NOT centered (E(u) ≠ 0 by design) → diagnostic plots and residual-based tests are invalid. 


# Using magrittr

library(magrittr)
     imf_example_data %>%
     kardl(CPI~ER+PPI+asym(ER)+deterministic(covid)+trend, mode=c(1,2,3,0)) %>%
     kardl_longrun() %>% summary()
#> 
#> Call:
#> kardl_longrun(model = .)
#> 
#> Estimation type:
#> Long-run multipliers 
#> 
#> Coefficients:
#>           Estimate Std. Error t value  Pr(>|t|)    
#> L1.ER_POS  0.82494    0.18150  4.5453 7.064e-06 ***
#> L1.ER_NEG  2.08297    0.48083  4.3320 1.825e-05 ***
#> L1.PPI     2.65278    0.84724  3.1311  0.001855 ** 
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Note:
#> Coefficients, standard errors, t-statistics and p-values are reliably estimated.
#> Fitted values and residuals are NOT centered (E(u) ≠ 0 by design) → diagnostic plots and residual-based tests are invalid. 
```

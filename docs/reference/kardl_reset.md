# Function to Reset KARDL Package Options to Default Values

This function resets all options in the kardl package to their default
values.

## Usage

``` r
kardl_reset(. = FALSE)
```

## Arguments

- .:

  If provided and not \`FALSE\`, the function will return this value
  after resetting the settings. If not provided or set to \`FALSE\`, it
  will return the current settings.

## Value

If resetting options, returns the provided value (if any) or invisibly
returns the current settings as a list.

If resetting options, returns the provided value (if any) or invisibly
returns the current settings as a list.

## See also

[`kardl_set`](kardl_set.md), [`kardl_get`](kardl_get.md)

## Examples

``` r
# Set some options
kardl_set(criterion = "BIC", differentAsymLag = TRUE)

# Reset to default options
kardl_get("criterion")  # Check current settings
#> [1] "BIC"
kardl_reset()
kardl_get("criterion")  # Check settings after reset
#> [1] "AIC"

library(magrittr)
 MyFormula<-CPI~ER+PPI+asym(ER)+deterministic(covid)+trend
imf_example_data %>%
  kardl_set(LongCoef= "K1{lag}w1{varName}",differentAsymLag= FALSE ) %>%  kardl(MyFormula ) %>%
    kardl_reset()
#> Optimal lags for each variable ( AIC ):
#> CPI: 2, ER_POS: 1, ER_NEG: 0, PPI: 3 
#> 
#> Call:
#> L0.d.CPI ~ K11w1CPI + K11w1ER_POS + K11w1ER_NEG + K11w1PPI + 
#>     L1.d.CPI + L2.d.CPI + L0.d.ER_POS + L1.d.ER_POS + L0.d.ER_NEG + 
#>     L0.d.PPI + L1.d.PPI + L2.d.PPI + L3.d.PPI + covid + trend
#> 
#> Coefficients:
#> (Intercept)     K11w1CPI  K11w1ER_POS  K11w1ER_NEG     K11w1PPI     L1.d.CPI  
#>  -2.779e-01   -1.747e-02    1.555e-02    3.522e-02    6.054e-02    3.886e-01  
#>    L2.d.CPI  L0.d.ER_POS  L1.d.ER_POS  L0.d.ER_NEG     L0.d.PPI     L1.d.PPI  
#>  -8.630e-02    1.062e-01    8.176e-02    1.848e-02    2.226e-02   -1.532e-02  
#>    L2.d.PPI     L3.d.PPI        covid        trend  
#>  -3.887e-02   -3.276e-02    7.944e-03   -5.187e-05  
#> 
kardl_get()
#> $batch
#> [1] "1/1"
#> 
#> $AsymSuffix
#> [1] "_POS" "_NEG"
#> 
#> $maxlag
#> [1] 4
#> 
#> $formula
#> NULL
#> 
#> $ShortCoef
#> [1] "L{lag}.d.{varName}"
#> 
#> $differentAsymLag
#> [1] FALSE
#> 
#> $criterion
#> [1] "AIC"
#> 
#> $data
#> NULL
#> 
#> $mode
#> [1] "quick"
#> 
#> $LongCoef
#> [1] "K1{lag}w1{varName}"
#> 
#> $AsymPrefix
#> NULL
#> 

imf_example_data %>%
  kardl_reset() %>%
    kardl_set(LongCoef= "K2{lag}w2{varName}",differentAsymLag=FALSE ) %>%  kardl(MyFormula)
#> Optimal lags for each variable ( AIC ):
#> CPI: 2, ER_POS: 1, ER_NEG: 0, PPI: 3 
#> 
#> Call:
#> L0.d.CPI ~ L1.CPI + L1.ER_POS + L1.ER_NEG + L1.PPI + L1.d.CPI + 
#>     L2.d.CPI + L0.d.ER_POS + L1.d.ER_POS + L0.d.ER_NEG + L0.d.PPI + 
#>     L1.d.PPI + L2.d.PPI + L3.d.PPI + covid + trend
#> 
#> Coefficients:
#> (Intercept)       L1.CPI    L1.ER_POS    L1.ER_NEG       L1.PPI     L1.d.CPI  
#>  -2.779e-01   -1.747e-02    1.555e-02    3.522e-02    6.054e-02    3.886e-01  
#>    L2.d.CPI  L0.d.ER_POS  L1.d.ER_POS  L0.d.ER_NEG     L0.d.PPI     L1.d.PPI  
#>  -8.630e-02    1.062e-01    8.176e-02    1.848e-02    2.226e-02   -1.532e-02  
#>    L2.d.PPI     L3.d.PPI        covid        trend  
#>  -3.887e-02   -3.276e-02    7.944e-03   -5.187e-05  
#> 

kardl_get(c("LongCoef","differentAsymLag","ShortCoef","batch"))
#> $LongCoef
#> [1] "L{lag}.{varName}"
#> 
#> $differentAsymLag
#> [1] TRUE
#> 
#> $ShortCoef
#> [1] "L{lag}.d.{varName}"
#> 
#> $batch
#> [1] "1/1"
#> 
```

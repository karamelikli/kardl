# Function to Set KARDL Package Options

This function allows users to set options for the kardl package. Users
can specify named arguments to set options or call the function without
arguments to retrieve all current settings.

## Usage

``` r
kardl_set(. = FALSE, ...)
```

## Arguments

- .:

  If provided and not \`FALSE\`, the function will return this value
  after setting the options. If not provided or set to \`FALSE\`, it
  will return the current settings.

- ...:

  Named arguments corresponding to the options to be set. Valid option
  names include those defined in the kardl package settings.

## Value

If no arguments are provided, returns all options as a list. If named
arguments are provided, sets those options and returns the updated list.

If no arguments are provided, returns all options as a list. If named
arguments are provided, sets those options and returns the updated list.

## See also

[`kardl_get`](kardl_get.md), [`kardl_reset`](kardl_reset.md)

## Examples

``` r
# Set options
kardl_set(maxlag = 5, mode = "grid")
# Get all options
kardl_get()
#> $batch
#> [1] "1/1"
#> 
#> $AsymSuffix
#> [1] "_POS" "_NEG"
#> 
#> $maxlag
#> [1] 5
#> 
#> $formula
#> NULL
#> 
#> $ShortCoef
#> [1] "L{lag}.d.{varName}"
#> 
#> $differentAsymLag
#> [1] TRUE
#> 
#> $criterion
#> [1] "AIC"
#> 
#> $data
#> NULL
#> 
#> $mode
#> [1] "grid"
#> 
#> $LongCoef
#> [1] "L{lag}.{varName}"
#> 
#> $AsymPrefix
#> NULL
#> 
# Get specific options
kardl_get("maxlag", "mode")
#> $maxlag
#> [1] 5
#> 
#> $mode
#> [1] "grid"
#> 

# Note: In interactive use, avoid calling kardl_get() directly to prevent cluttering the console.

# \donttest{
kardl_get()
#> $batch
#> [1] "1/1"
#> 
#> $AsymSuffix
#> [1] "_POS" "_NEG"
#> 
#> $maxlag
#> [1] 5
#> 
#> $formula
#> NULL
#> 
#> $ShortCoef
#> [1] "L{lag}.d.{varName}"
#> 
#> $differentAsymLag
#> [1] TRUE
#> 
#> $criterion
#> [1] "AIC"
#> 
#> $data
#> NULL
#> 
#> $mode
#> [1] "grid"
#> 
#> $LongCoef
#> [1] "L{lag}.{varName}"
#> 
#> $AsymPrefix
#> NULL
#> 
# }

# Example with magrittr pipe
library(magrittr)
# Set custom coefficient naming conventions

MyFormula<-CPI~ER+PPI+asym(ER)+deterministic(covid)+trend
kardl_set(ShortCoef = "L___{lag}.d.{varName}", formula = MyFormula, data = imf_example_data)
imf_example_data %>%   kardl(MyFormula)
#> 
#> Optimal lags for each variable ( AIC ):
#> CPI: 2, ER_POS: 1, ER_NEG: 0, PPI: 3 
#> 
#> Call:
#> L___0.d.CPI ~ L1.CPI + L1.ER_POS + L1.ER_NEG + L1.PPI + L___1.d.CPI + 
#>     L___2.d.CPI + L___0.d.ER_POS + L___1.d.ER_POS + L___0.d.ER_NEG + 
#>     L___0.d.PPI + L___1.d.PPI + L___2.d.PPI + L___3.d.PPI + covid + 
#>     trend
#> 
#> Coefficients:
#>    (Intercept)          L1.CPI       L1.ER_POS       L1.ER_NEG          L1.PPI  
#>     -2.779e-01      -1.747e-02       1.555e-02       3.522e-02       6.054e-02  
#>    L___1.d.CPI     L___2.d.CPI  L___0.d.ER_POS  L___1.d.ER_POS  L___0.d.ER_NEG  
#>      3.886e-01      -8.630e-02       1.062e-01       8.176e-02       1.848e-02  
#>    L___0.d.PPI     L___1.d.PPI     L___2.d.PPI     L___3.d.PPI           covid  
#>      2.226e-02      -1.532e-02      -3.887e-02      -3.276e-02       7.944e-03  
#>          trend  
#>     -5.187e-05  
#> 

kardl_reset()
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
#> [1] TRUE
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
#> [1] "L{lag}.{varName}"
#> 
#> $AsymPrefix
#> NULL
#> 

imf_example_data %>%  kardl_set(LongCoef= "LK{lag}_{varName}",ShortCoef = "D{lag}.d.{varName}") %>%
kardl(MyFormula)
#> Optimal lags for each variable ( AIC ):
#> CPI: 2, ER_POS: 1, ER_NEG: 0, PPI: 3 
#> 
#> Call:
#> D0.d.CPI ~ LK1_CPI + LK1_ER_POS + LK1_ER_NEG + LK1_PPI + D1.d.CPI + 
#>     D2.d.CPI + D0.d.ER_POS + D1.d.ER_POS + D0.d.ER_NEG + D0.d.PPI + 
#>     D1.d.PPI + D2.d.PPI + D3.d.PPI + covid + trend
#> 
#> Coefficients:
#> (Intercept)      LK1_CPI   LK1_ER_POS   LK1_ER_NEG      LK1_PPI     D1.d.CPI  
#>  -2.779e-01   -1.747e-02    1.555e-02    3.522e-02    6.054e-02    3.886e-01  
#>    D2.d.CPI  D0.d.ER_POS  D1.d.ER_POS  D0.d.ER_NEG     D0.d.PPI     D1.d.PPI  
#>  -8.630e-02    1.062e-01    8.176e-02    1.848e-02    2.226e-02   -1.532e-02  
#>    D2.d.PPI     D3.d.PPI        covid        trend  
#>  -3.887e-02   -3.276e-02    7.944e-03   -5.187e-05  
#> 
kardl_get(c("LongCoef","ShortCoef"))
#> $LongCoef
#> [1] "LK{lag}_{varName}"
#> 
#> $ShortCoef
#> [1] "D{lag}.d.{varName}"
#> 
```

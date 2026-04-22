# Set kardl Package Options

This function allows users to set options for the kardl package. Users
can specify named arguments to set options or call the function without
arguments to retrieve all current settings.

## Usage

``` r
kardl_set(...)
```

## Arguments

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
#> $LongCoef
#> [1] "L{lag}.{varName}"
#> 
#> $mode
#> [1] "grid"
#> 
#> $AsymPrefix
#> character(0)
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
#> $LongCoef
#> [1] "L{lag}.{varName}"
#> 
#> $mode
#> [1] "grid"
#> 
#> $AsymPrefix
#> character(0)
#> 
# }

# Set custom coefficient naming conventions

MyFormula<-CPI~ER+PPI+asym(ER)+deterministic(covid)+trend
kardl_set(ShortCoef = "L___{lag}.d.{varName}", formula = MyFormula, data = imf_example_data)

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
#> $LongCoef
#> [1] "L{lag}.{varName}"
#> 
#> $mode
#> [1] "quick"
#> 
#> $AsymPrefix
#> character(0)
#> 

```

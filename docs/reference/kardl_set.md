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

All current settings as a list after applying any updates from the
provided named arguments invisibly.

If no arguments are provided, returns all options as a list. If named
arguments are provided, sets those options and returns the updated list.

## See also

[`kardl_get`](https://karamelikli.github.io/kardl/reference/kardl_get.md),
[`kardl_reset`](https://karamelikli.github.io/kardl/reference/kardl_reset.md)

## Examples

``` r
# Get default options
kardl_get("maxlag", "mode")
#> $maxlag
#> [1] 4
#> 
#> $mode
#> [1] "quick"
#> 
# Set options
kardl_set(maxlag = 5, mode = "grid")
# Get specific options
kardl_get("maxlag", "mode")
#> $maxlag
#> [1] 5
#> 
#> $mode
#> [1] "grid"
#> 

# To have the updated settings available in the global environment, assign the output to a variable:
MySettings<- kardl_set(LongCoef = "LongRun_{varName}", ShortCoef = "ShortRun_{varName}")
# Now MySettings contains the updated settings, and the kardl package
# will use these settings for subsequent operations.
MySettings$LongCoef
#> [1] "LongRun_{varName}"
MySettings$maxlag
#> [1] 5

# Reset to defaults after demonstrating custom settings
kardl_reset()
```

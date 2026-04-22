# Reset kardl Package Options to Default Values

This function resets all options in the kardl package to their default
values.

## Usage

``` r
kardl_reset(value = FALSE)
```

## Arguments

- value:

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
before <- kardl_get("criterion")
out <-kardl_reset()
after <- kardl_get("criterion")
cat("Before reset:", before, "\n")
#> Before reset: BIC 
cat("After reset:", after, "\n")
#> After reset: AIC 
```

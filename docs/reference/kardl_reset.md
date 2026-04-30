# Reset kardl Package Options to Default Values

This function resets kardl package options to their default values.

## Usage

``` r
kardl_reset(exclude = NULL)
```

## Arguments

- exclude:

  Character vector of setting names that should not be reset. These
  settings retain their current values. By default, all settings are
  reset.

## Value

A list of the settings after reset, returned invisibly.

## See also

[`kardl_set`](https://karamelikli.github.io/kardl/reference/kardl_set.md),
[`kardl_get`](https://karamelikli.github.io/kardl/reference/kardl_get.md)

## Examples

``` r
kardl_set(criterion = "BIC", differentAsymLag = FALSE)

# Reset all settings to defaults except "criterion"
kardl_reset(exclude = "criterion")

# Get the current settings to verify the reset
print(kardl_get("criterion"))
#> [1] "BIC"

# This will show "BIC" since it was excluded from the reset,
#while other settings will be reset to their defaults.
print(kardl_get("differentAsymLag"))
#> [1] TRUE
```

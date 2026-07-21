# Parse model specifications from user inputs

Extracts and organizes information from model formulas including
asymmetric terms, deterministic components, and variable relationships.

## Usage

``` r
parse_model_specs(inputs)
```

## Arguments

- inputs:

  A list containing at minimum a `formula` element, and optionally a
  `data` element when using dot notation.

## Value

A list containing three components:

- args_info:

  The original inputs list

- settings:

  Package settings retrieved via
  [`kardl_get()`](https://karamelikli.github.io/kardl/reference/kardl_get.md)

- extracted_info:

  Processed model information including variables, constant terms, and
  asymmetric specifications

## Details

For package developers: This function is a crucial part of the model
preparation workflow. It ensures that the model formula is correctly
specified and that all variables are properly identified and
categorized. The extracted information is then used in subsequent steps
of the estimation process, such as constructing lagged variables and
fitting the model.

The function includes error handling to provide informative messages if
the model formula is missing or incorrectly specified. It first
validates the formula type, parses it to extract variable roles, and
verifies existence in the provided data.

The function also handles the special case of the dot (.) in the
formula, which indicates that all variables in the data (except the
dependent variable and deterministic variables) should be included as
independent variables.

## Developer Note

**This function is not intended for general users of the kardl
package.** It is designed as a low-level utility for developers who are
building extensions or using `kardl` as a dependency in their own
software. General users should interact with the high-level modeling
functions provided by the package.

## Examples

``` r
# Example of using parse_model_specs in a development context
inputs <- list(
  formula = y ~ x + sasymmetric(z) + deterministic(w)
)
result <- parse_model_specs(inputs)
result
#> $args_info
#> $args_info$formula
#> y ~ x + sasymmetric(z) + deterministic(w)
#> <environment: 0x5c5f424fd650>
#> 
#> 
#> $settings
#> $settings$asym_prefix
#> NULL
#> 
#> $settings$asym_suffix
#> [1] "_POS" "_NEG"
#> 
#> $settings$long_coef
#> [1] "L{lag}.{var_name}"
#> 
#> $settings$short_coef
#> [1] "L{lag}.d.{var_name}"
#> 
#> 
#> $extracted_info
#> $extracted_info$no_constant
#> [1] FALSE
#> 
#> $extracted_info$trend
#> [1] FALSE
#> 
#> $extracted_info$asym_long_vars
#> character(0)
#> 
#> $extracted_info$asym_short_vars
#> [1] "z"
#> 
#> $extracted_info$deterministic
#> [1] "w"
#> 
#> $extracted_info$dependent_var
#> [1] "y"
#> 
#> $extracted_info$independent_vars
#> [1] "x" "z"
#> 
#> $extracted_info$all_vars
#> [1] "y" "x" "z"
#> 
#> attr(,"source")
#> [1] "extracted_info"
#> attr(,"description")
#> [1] "This value was obtained from user inputs."
#> 
```

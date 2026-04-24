# Parse Formula Variables

The `parseFormula()` function analyzes a given formula to identify and
extract variables that match specified patterns. It is particularly
useful for isolating variables enclosed within certain functions or
constructs in the formula, such as `asym()`,
[`det()`](https://rdrr.io/r/base/det.html), or any user-defined
patterns.

## Usage

``` r
parse_formula_vars(formula)
```

## Arguments

- formula:

  The initial formula for the model, typically specified using R's
  formula syntax (e.g., `y ~ x + f(x1 + x2)`).

## Value

A list containing:

- `response`: The response variable(s) extracted from the formula.

- `intercept`: A logical value indicating whether the formula includes
  an intercept (default is TRUE).

- `dot`: A logical value indicating whether the formula includes a dot
  (.) representing all other variables (default is FALSE).

- `outside`: A vector of variables that are outside any specified
  patterns. It includes the variables that are not detected within the
  specified patterns in the formula.

- `inside`: A list where each element corresponds to a detected pattern
  (e.g., function name) and contains the variables found inside that
  pattern. For example, if the formula includes `asym(x1 + x2)`, the
  `inside` list will have an element named "asym" containing the
  variables "x1" and "x2". This allows for easy identification of
  variables that are part of specific constructs in the formula.

## See also

[`formula`](https://rdrr.io/r/stats/formula.html) and
[`gregexpr`](https://rdrr.io/r/base/grep.html)

## Examples

``` r

# Parse formulas containing various collection types like ()
formula_ <- y ~ x +det(s -gg- d) + asymS(d2 -rr+ s)-mm(y1+y2+y3)+asym(k1+k2+k3)+trend-huseyin
# Extract variables
parse_formula_vars(formula_)
#> $response
#> [1] "y"
#> 
#> $intercept
#> [1] TRUE
#> 
#> $dot
#> [1] FALSE
#> 
#> $outside
#> [1] "x"       "trend"   "huseyin"
#> 
#> $inside
#> $inside$det
#> [1] "s"  "gg" "d" 
#> 
#> $inside$asymS
#> [1] "d2" "rr" "s" 
#> 
#> $inside$mm
#> [1] "y1" "y2" "y3"
#> 
#> $inside$asym
#> [1] "k1" "k2" "k3"
#> 
#> 

```

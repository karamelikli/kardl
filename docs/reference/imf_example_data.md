# IMF Example Data (Deprecated)

**\[deprecated\]**

This dataset is **deprecated** and is provided for testing purposes only
for versions of kardl before 2.0.5. It is retained for backward
compatibility.

## Usage

``` r
imf_example_data
```

## Format

A data frame with 470 rows and 4 variables:

- ER:

  Numeric. Exchange rate of Turkey.

- CPI:

  Numeric. CPI of Turkey.

- PPI:

  Numeric. PPI of Turkey.

- covid:

  Integer. COVID-19 dummy variable.

## Examples

``` r
data(imf_example_data)
head(imf_example_data)
#>                ER       CPI      PPI covid
#> 1985-01 -7.698074 -4.895304 3.514587     0
#> 1985-02 -7.634863 -4.862836 3.393818     0
#> 1985-03 -7.618904 -4.816122 3.492322     0
#> 1985-04 -7.576649 -4.807842 3.452126     0
#> 1985-05 -7.545791 -4.784120 3.548855     0
#> 1985-06 -7.531908 -4.791754 3.275258     0
```

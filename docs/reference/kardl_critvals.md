# Extract Critical Values for KARDL Bounds Tests

Internal function to retrieve the appropriate critical values for the
Pesaran-Shin-Smith F-test, Pesaran-Shin-Smith t-test, or Narayan bounds
test based on the test object. This function checks the class and test
type of the input object and then extracts the critical values from the
corresponding tables, adjusting for the number of regressors and
observations as needed.

## Usage

``` r
kardl_critvals(x, ...)
```

## Details

@param x An object of class 'kardl_test @param ... Additional arguments
(currently unused). @return A list of class 'kardl_critvals' containing
the critical values, test function type, number of regressors, and any
notes regarding adjustments made for the critical values.

@noRd

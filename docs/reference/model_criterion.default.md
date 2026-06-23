# Default method for model_criterion

This is the default method for the `model_criterion` function. It is
called when the class of `lm_model` does not have a specific method
defined. The function throws an error indicating that no method is
available for the class of the fitted model object and suggests
providing a fitted model object of class `lm`.

## Usage

``` r
# Default S3 method
model_criterion(lm_model, cr, ...)
```

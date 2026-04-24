# Model Selection Criteria

Computes a model selection criterion (AIC, BIC, AICc, or HQ) or applies
a user-defined function to evaluate a statistical model.

## Usage

``` r
modelCriterion(estModel, cr, ...)
```

## Arguments

- estModel:

  An object containing the fitted model. The object should include at
  least:

  - `estModel$model` - the actual fitted model object (e.g., from `lm`,
    `glm`).

  - `k` - the number of estimated parameters.

  - `n` - the sample size.

- cr:

  A character string specifying the criterion to compute. Options are
  `"AIC"`, `"BIC"`, `"AICc"`, and `"HQ"`. Alternatively, a user-defined
  function can be provided. See details below for more information on
  using custom criteria.

- ...:

  Additional arguments passed to the user-defined criterion function if
  `cr` is a function.

## Value

A numeric value representing the selected criterion, normalized by the
sample size if one of the predefined options is used.

## Details

This function returns model selection criteria used to compare the
quality of different models. All criteria are defined such that **lower
values indicate better models** (i.e., the goal is minimization).

If you wish to compare models using a maximization approach (e.g.,
log-likelihood), you can multiply the result by `-1`.

Note: The predefined string options (e.g., `"AIC"`) are **not** the same
as the built-in R functions [`AIC()`](https://rdrr.io/r/stats/AIC.html)
or [`BIC()`](https://rdrr.io/r/stats/AIC.html). In particular, the
values returned by this function are adjusted by dividing by the sample
size `n` (i.e., normalized AIC/BIC), which makes it more comparable
across datasets of different sizes.

The function returns:

- **"AIC"**: \\ \frac{2k - 2\ell}{n} \\ Akaike Information Criterion
  divided by `n`.

- **"BIC"**: \\ \frac{\log(n) \cdot k - 2\ell}{n} \\ Bayesian
  Information Criterion divided by `n`.

- **"AICc"**: \\ \frac{2k(k+1)}{n - k - 1} + \frac{2k - 2\ell}{n} \\
  Corrected Akaike Information Criterion divided by `n`.

- **"HQ"**: \\ \frac{2 \log(\log(n)) \cdot k - 2\ell}{n} \\ Hannan-Quinn
  Criterion divided by `n`.

where:

- \\k\\ is the number of parameters,

- \\n\\ is the sample size,

- \\\ell\\ is the log-likelihood of the model.

If `cr` is a function, it is called with the fitted model and any
additional arguments passed through `...`.

## See also

[`kardl`](kardl.md)

## Examples

``` r

# Example usage of modelCriterion function with a simple linear model
mylm<- lm(mpg ~ wt + hp, data = mtcars)
modelCriterion(mylm, AIC )
#> [1] 156.6523
modelCriterion(mylm, "AIC" )
#> [1] 4.832886

 # Example usage of modelCriterion function with a kardl model
 kardl_model <- kardl(imf_example_data,
                      CPI ~ ER + PPI + asym(ER) + deterministic(covid) + trend,
                      mode = c(1, 2, 3, 0))

 # Using AIC as the kardl package's built-in criterion function which is different
 # from the base R AIC function.
 modelCriterion(kardl_model, "AIC")
#> [1] -5.499093

 # Using the base R AIC function directly on the fitted model object
 modelCriterion(kardl_model, AIC)
#> [1] -2555.078
 # Using the base R AIC function outside of modelCriterion to compute AIC for the fitted model
 AIC(kardl_model)
#> [1] -2555.078

 # Using BIC as the criterion for the kardl model which is different from the base R BIC function.
 modelCriterion(kardl_model, "BIC")
#> [1] -5.356571

 # Using a custom criterion function that divides AIC by the sample size
 my_cr_fun <- function(mod, ...) { AIC(mod) / length(mod$model[[1]]) }
 modelCriterion(kardl_model, my_cr_fun)
#> [1] -5.494792
```

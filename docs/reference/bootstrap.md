# Bootstrap Confidence Intervals for Dynamic Multipliers

This function computes bootstrap confidence intervals (CI) for dynamic
multipliers of a specified variable in a model estimated using the
`kardl` package. The bootstrap method generates resampled datasets to
estimate the variability of the dynamic multipliers, providing upper and
lower bounds for the confidence interval.

## Usage

``` r
bootstrap(
  kardl_model,
  horizon = 80,
  replications = 100,
  level = 95,
  min_prob = 0,
  seed = NULL,
  ...
)
```

## Arguments

- kardl_model:

  The model produced by the
  [`kardl`](https://karamelikli.github.io/kardl/reference/kardl.md)
  function. This is the model object from which the dynamic multipliers
  are calculated.

- horizon:

  An integer specifying the horizon over which dynamic multipliers will
  be computed. The horizon defines the time frame for the analysis
  (e.g., 40 periods).

- replications:

  An integer indicating the number of bootstrap replications to perform.
  Higher values increase accuracy but also computational time. Default
  is `100`.

- level:

  A numeric value specifying the confidence level for the intervals
  (e.g., 95 for 95% confidence). Default is `90`.

- min_prob:

  A numeric value specifying the minimum p-value threshold for including
  coefficients in the bootstrap. Coefficients with p-values above this
  threshold will be set to zero in the bootstrap samples. Default is `0`
  (no threshold). This parameter allows users to control the inclusion
  of coefficients in the bootstrap process based on their statistical
  significance. Setting a threshold can help focus the analysis on more
  relevant variables, but it may also exclude potentially important
  effects if set too stringently.

- seed:

  An optional integer to set the random seed for reproducibility of the
  bootstrap results. If not provided, the bootstrap will use the current
  random state.

- ...:

  Additional arguments (currently not used).

## Value

A list containing the following elements:

- **mpsi**: A data frame containing the dynamic multiplier estimates
  along with their upper and lower confidence intervals for each
  variable and time horizon.

- **level**: The confidence level used for the intervals (e.g 95).

- **horizon**: The horizon over which the multipliers were computed
  (e.g., 40).

- **vars**: A list of variable information extracted from the model,
  including dependent variable, independent variables, asymmetric
  variables, and deterministic terms.

- **replications**: The number of bootstrap replications performed.

- **type**: A character string indicating the type of analysis, in this
  case "bootstrap".

## Details

The `mpsi` component of the output contains the dynamic multiplier
estimates along with their upper and lower confidence intervals. These
values are provided for each variable and at each time horizon.

## See also

[`mplier`](https://karamelikli.github.io/kardl/reference/mplier.md) for
calculating dynamic multipliers

## Examples

``` r

# Example usage of the bootstrap function

# Fit a model using kardl
kardl_model <- kardl(
  CPI ~ ER + PPI + asy(ER) + det(covid) + trend,
  imf_example_data,
  mode = c(1, 2, 3, 0)
)

# Perform bootstrap with specific variables for plotting
boot <- bootstrap(kardl_model,
  horizon = 40, level = 95, min_prob = 0,
  replications = 5, seed = 123L
)
# The boot object will include all plots for the specified variables
# Displaying the boot object provides an overview of its components
names(boot)
#> [1] "mpsi"         "level"        "horizon"      "vars"         "replications"
#> [6] "type"        

# Inspect the first few rows of the dynamic multiplier estimates
head(kardl_extract(boot, "multipliers"))
#>   h    ER_POS       ER_NEG     ER_dif    PPI_POS     PPI_NEG PPI_dif
#> 1 0 0.1079048 -0.005754563 0.10215020 0.01650164 -0.01650164       0
#> 2 1 0.2441508 -0.068170052 0.17598074 0.06089675 -0.06089675       0
#> 3 2 0.3105905 -0.148501449 0.16208908 0.11412494 -0.11412494       0
#> 4 3 0.3407525 -0.227554838 0.11319771 0.16957406 -0.16957406       0
#> 5 4 0.3581333 -0.281735367 0.07639793 0.22496277 -0.22496277       0
#> 6 5 0.3709116 -0.326659918 0.04425171 0.27951609 -0.27951609       0
#>   ER_CI_upper ER_CI_lower
#> 1   0.1278960  0.07502600
#> 2   0.1917234  0.10406928
#> 3   0.2191685  0.10435071
#> 4   0.1848818  0.10108447
#> 5   0.1605490  0.06046077
#> 6   0.1405415  0.01145040

summary(boot)
#> Summary of Dynamic Multipliers
#> Horizon: 40 
#> 
#>        h          ER_POS           ER_NEG              ER_dif        
#>  Min.   : 0   Min.   :0.1079   Min.   :-1.293834   Min.   :-0.67261  
#>  1st Qu.:10   1st Qu.:0.4210   1st Qu.:-1.091555   1st Qu.:-0.52255  
#>  Median :20   Median :0.5034   Median :-0.837426   Median :-0.33403  
#>  Mean   :20   Mean   :0.4833   Mean   :-0.784433   Mean   :-0.30116  
#>  3rd Qu.:30   3rd Qu.:0.5690   3rd Qu.:-0.518143   3rd Qu.:-0.09717  
#>  Max.   :40   Max.   :0.6212   Max.   :-0.005755   Max.   : 0.17598  
#>     PPI_POS          PPI_NEG           PPI_dif   ER_CI_upper      
#>  Min.   :0.0165   Min.   :-1.5849   Min.   :0   Min.   :-0.21868  
#>  1st Qu.:0.5353   1st Qu.:-1.3112   1st Qu.:0   1st Qu.:-0.15439  
#>  Median :0.9673   Median :-0.9673   Median :0   Median :-0.06495  
#>  Mean   :0.9061   Mean   :-0.9061   Mean   :0   Mean   :-0.04213  
#>  3rd Qu.:1.3112   3rd Qu.:-0.5353   3rd Qu.:0   3rd Qu.: 0.05949  
#>  Max.   :1.5849   Max.   :-0.0165   Max.   :0   Max.   : 0.21917  
#>   ER_CI_lower     
#>  Min.   :-1.0294  
#>  1st Qu.:-0.8309  
#>  Median :-0.5566  
#>  Mean   :-0.5069  
#>  3rd Qu.:-0.2051  
#>  Max.   : 0.1044  

# Retrieve plots generated during the bootstrap process
# Accessing all plots
plot(boot)
#> Warning: Multiple variables selected. Only the first one will be plotted.


# Accessing the plot for a specific variable by its name
plot(boot, variable = "PPI")

plot(boot, variable = "ER")


library(magrittr)

imf_example_data %>%
  kardl(CPI ~ PPI + asym(ER) + trend, maxlag = 2, data = .) %>%
  bootstrap(replications = 5) %>%
  plot(variable = "ER")
```

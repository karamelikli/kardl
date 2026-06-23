# Compute Dynamic Multipliers for kardl Models

Computes cumulative dynamic multipliers based on a model estimated using
the `kardl` framework. The function supports different configurations of
linearity and asymmetry in both short-run and long-run dynamics.

## Usage

``` r
mplier(kardl_model, ...)
```

## Arguments

- kardl_model:

  An object of class `kardl_lm` produced by the `kardl` function.

- ...:

  Additional arguments (currently not used).

- horizon:

  Integer. Number of periods ahead for which dynamic multipliers are
  computed.

- min_prob:

  Numeric. Minimum p-value threshold for including coefficients in the
  calculation. Coefficients with p-values above this threshold will be
  set to zero. Default is `0` (no threshold). This parameter allows
  users to control the inclusion of coefficients in the calculation
  based on their statistical significance. Setting a threshold can help
  focus the analysis on more relevant variables, but it may also exclude
  potentially important effects if set too stringently.

## Value

A list of class `kardl_mplier` containing:

- **mpsi**: Matrix of cumulative dynamic multipliers.

- **omega**: Vector of omega coefficients (persistence structure).

- **lambda**: Matrix of short-run dynamic coefficients.

- **horizon**: Forecast horizon used.

- **vars**: Extracted model variable structure.

## Details

The asymmetry structure is determined internally:

- Variables in `extracted_info$asym_short_vars` are treated as
  asymmetric in the short run.

- Variables in `extracted_info$asym_long_vars` are treated as asymmetric
  in the long run.

This allows four possible configurations:

- **LL**: Linear in both short-run and long-run

- **NN**: Asymmetric in both short-run and long-run

- **SA**: Short-run linear, long-run asymmetric

- **AS**: Short-run asymmetric, long-run linear

When a component is linear, the same coefficient path is used for both
positive and negative changes. When asymmetric, separate positive and
negative effects are computed.

The `mplier` function computes dynamic multipliers based on the
coefficients and lag structure of a model estimated using the `kardl`
package. The function extracts necessary information from the model,
such as coefficients, lag structure, and variable names, to compute the
dynamic multipliers. It calculates the short-run coefficients, Lambda
values, and omega values based on the model's parameters and lag
structure. The output includes a matrix of dynamic multipliers (mpsi),
which can be used for further analysis or visualization. The dynamic
multipliers provide insight into how changes in the independent
variables affect the dependent variable over time, allowing for a deeper
understanding of the relationships captured by the model. The function
also allows users to set a minimum p-value threshold for including
coefficients in the calculation, providing flexibility in focusing on
statistically significant effects.

The function constructs dynamic multipliers based on the recursive
relationship:

\$\$ \psi\_{h}^{+} = \sum\_{i=0}^{h} \frac{\partial y\_{t+i}}{\partial
x\_{t}^{+}}, \quad \psi\_{h}^{-} = \sum\_{i=0}^{h} \frac{\partial
y\_{t+i}}{\partial x\_{t}^{-}} \$\$

where \\\psi_h^{+}\\ and \\\psi_h^{-}\\ represent cumulative responses
to positive and negative shocks.

The recursion is defined as:

\$\$ \psi_h = \lambda_h + \sum\_{j=1}^{p} \omega_j \psi\_{h-j} \$\$

where \\\lambda_h\\ captures short-run effects and \\\omega_j\\ reflects
persistence through lagged dependent variables.

When asymmetry is present, positive and negative shocks are propagated
separately. Otherwise, the same dynamic path is used.

## See also

[`bootstrap`](https://karamelikli.github.io/kardl/reference/bootstrap.md)

## Examples

``` r

# This example demonstrates how to use the mplier function to calculate
# dynamic multipliers from a model estimated using the kardl package.
# The example includes fitting a model with the kardl function,
# calculating the multipliers, and visualizing the results using both
# base R plotting and ggplot2.

# Calculating dynamic multipliers for a linear model in short and long run
# (NN)

kardl_model <- kardl(CPI ~ ER, imf_example_data)
m <- mplier(kardl_model, 40)
head(m$mpsi)
#>      h    ER_POS     ER_NEG ER_dif
#> [1,] 0 0.1011301 -0.1011301      0
#> [2,] 1 0.2430853 -0.2430853      0
#> [3,] 2 0.3149552 -0.3149552      0
#> [4,] 3 0.3564520 -0.3564520      0
#> [5,] 4 0.3891548 -0.3891548      0
#> [6,] 5 0.4154632 -0.4154632      0
plot(m)


# Calculating dynamic multipliers for a model with
# Short-run linear, long-run asymmetric (SA)
kardl_model <- kardl(CPI ~ lasym(ER), imf_example_data)
m <- mplier(kardl_model, horizon = 40, min_prob = 0)
head(kardl_extract(m, "multipliers"))
#>      h    ER_POS     ER_NEG ER_dif
#> [1,] 0 0.1016347 -0.1016347      0
#> [2,] 1 0.2438260 -0.2438260      0
#> [3,] 2 0.3149033 -0.3149033      0
#> [4,] 3 0.3546481 -0.3546481      0
#> [5,] 4 0.3853433 -0.3853433      0
#> [6,] 5 0.4103829 -0.4103829      0
plot(m)


# Calculating dynamic multipliers for a model with
# Short-run asymmetric, long-run linear (AS)
kardl_model <- kardl(CPI ~ sasym(ER), imf_example_data)
m <- mplier(kardl_model, 40)
plot(m)


# Calculating dynamic multipliers for a model with
# asymmetric effects in both short and long run (NN)
kardl_model <- kardl(CPI ~ asym(ER) + PPI, imf_example_data)
m <- mplier(kardl_model, 40)
plot(m)
#> Warning: Multiple variables selected. Only the first one will be plotted.


# The multipliers matrix contains the cumulative dynamic multipliers for each
# variable and time horizon. The omega vector contains the persistence
# structure of the model, while the lambda matrix contains the short-run
# dynamic coefficients. You can inspect these components to understand the
# dynamics captured by the model.

head(kardl_extract(m, "multipliers"))
#>      h    PPI_POS     PPI_NEG PPI_dif    ER_POS      ER_NEG     ER_dif
#> [1,] 0 0.02132813 -0.02132813       0 0.1063165 -0.03176480 0.07455172
#> [2,] 1 0.07315202 -0.07315202       0 0.2435925 -0.08346931 0.16012314
#> [3,] 2 0.11052625 -0.11052625       0 0.3036652 -0.13943921 0.16422600
#> [4,] 3 0.14464258 -0.14464258       0 0.3292762 -0.19429402 0.13498218
#> [5,] 4 0.20964369 -0.20964369       0 0.3467835 -0.24716862 0.09961483
#> [6,] 5 0.28576074 -0.28576074       0 0.3634339 -0.29819112 0.06524275
head(kardl_extract(m, "omega"))
#> [1]  1.37561845 -0.47712514  0.07968277
head(kardl_extract(m, "lambda"))
#>           PPI_POS      PPI_NEG       ER_POS      ER_NEG
#> [1,]  0.021328127  0.021328127  0.106316519 0.031764799
#> [2,]  0.022484531  0.022484531 -0.008975032 0.008008269
#> [3,] -0.023739497 -0.023739497 -0.078040265 0.000000000
#> [4,]  0.005730656  0.005730656  0.000000000 0.000000000
#> [5,]  0.031772768  0.031772768  0.000000000 0.000000000
#> [6,]  0.000000000  0.000000000  0.000000000 0.000000000

# For plotting specific variables, you can specify them in the plot
# function. For example, to plot the multipliers for the variable "PPI":

plot(m, variable = "PPI", title = "Dynamic Multipliers for PPI")
```

# Estimate ARDL and NARDL Models with Automatic Lag Selection

This function estimates an Autoregressive Distributed Lag (ARDL) or
Nonlinear ARDL (NARDL) model based on the provided data and model
formula. It allows for flexible specification of variables, including
deterministic terms, asymmetric variables, and trend components. The
function also supports automatic lag selection using various information
criteria.

## Usage

``` r
kardl(
  formula = NULL,
  data = NULL,
  maxlag = NULL,
  mode = NULL,
  criterion = NULL,
  different_asym_lag = NULL,
  batch = NULL,
  ...
)
```

## Arguments

- formula:

  A formula specifying the long-run model equation. This formula defines
  the relationships between the dependent variable and explanatory
  variables, including options for deterministic terms, asymmetric
  variables, and a trend component. Example formula:
  `y ~ x + z + Asymmetric(z) + Lasymmetric(x2 + x3) + Sasymmetric(x3 + x4) + deterministic(dummy1 + dummy2) + trend`

  ***Details***

  The formula allows flexible specification of variables and their
  roles:

  - **Deterministic variables:** Deterministic regressors (e.g., dummy
    variables) can be included using `deterministic()`. Multiple
    deterministic variables may be supplied using `+`, for example
    `deterministic(dummy1 + dummy2)`. These variables are treated as
    fixed components and are not associated with short-run or long-run
    dynamics.

  - **Asymmetric variables:** Asymmetric decompositions can be specified
    for short-run and/or long-run dynamics:

    - **Sasymmetric**: Specifies short-run asymmetric variables. For
      example, `Sasymmetric(x1 + x2)` applies short-run asymmetric
      decomposition to `x1` and `x2`.

    - **Lasymmetric**: Specifies long-run asymmetric variables. For
      example, `Lasymmetric(x1 + x3)` applies long-run asymmetric
      decomposition to `x1` and `x3`.

    - **Asymmetric**: Specifies variables that enter both short-run and
      long-run asymmetric decompositions. For example,
      `Asymmetric(x1 + x3)` applies asymmetric decomposition in both
      dynamics.

  A **trend** term may be included to capture deterministic linear time
  trends by simply adding `trend` to the formula.

  The formula also supports the use of `.` to represent all available
  regressors in the supplied data (excluding the dependent variable),
  following standard R formula conventions.

  All of the operators `Deterministic()`, `Sasymmetric()`,
  `Lasymmetric()`, and `Asymmetric()` follow the same usage rules:

  - They can be freely combined within a single formula, for example:


          y ~ . +
            Asymmetric(z) +
            Lasymmetric(x2 + x3) +
            Sasymmetric(x3 + x4) +
            deterministic(dummy1 + dummy2) +
            trend
          

  - They must not be nested within one another. Valid usage:
    `y ~ x + deterministic(dummy) + Asymmetric(z)`. Invalid usage (to be
    avoided): `y ~ x + deterministic(Asymmetric(z))` or
    `y ~ x + Asymmetric(deterministic(dummy))`.

  - Where applicable, arguments are validated internally using
    [`match.arg()`](https://rdrr.io/r/base/match.arg.html).
    Consequently, abbreviated inputs are accepted provided they uniquely
    identify a valid option. For example, if `"asymmetric"` is an
    admissible value, specifying `"a"` is sufficient. For clarity and
    reproducibility, however, full argument names are recommended.

  These components may therefore be combined flexibly to construct a
  specification tailored to the empirical analysis.

- data:

  The data of analysis, which should be a data frame containing the
  variables referenced in the `formula`. The function will check that
  all variables specified in the formula are present in the data before
  proceeding with estimation. The data should be time-ordered, and the
  function will internally handle the construction of lagged and
  differenced variables as needed for ARDL and NARDL estimation.

- maxlag:

  An integer specifying the maximum number of lags to be considered for
  the model. The default value is `4`. This parameter sets an upper
  limit on the lag length during the model estimation process.

  ***details***

  The `maxlag` parameter is crucial for defining the maximum lag length
  that the model will evaluate when selecting the optimal lag structure
  based on the specified `criterion`. It controls the computational
  effort and helps prevent overfitting by restricting the search space
  for lag selection.

  - If the data has a short time horizon or is prone to overfitting,
    consider reducing `maxlag`.

  - If the data is expected to have long-term dependencies, increasing
    `maxlag` may be necessary to capture the relevant dynamics.

  Setting an appropriate value for `maxlag` depends on the nature of
  your dataset and the context of the analysis:

  - For small datasets or quick tests, use smaller values (e.g.,
    `maxlag = 2`).

  - For datasets with more observations or longer-term patterns, larger
    values (e.g., `maxlag = 8`) may be appropriate, though this
    increases computational time.

- mode:

  Specifies the mode of estimation and output control. This parameter
  determines how the function handles lag estimation and what kind of
  feedback or control is provided during the process. The available
  options are:

  - **"quick"** (default): Displays progress and messages in the console
    while the function estimates the optimal lag values. This mode is
    suitable for interactive use or for users who want to monitor the
    estimation process in real-time. It provides detailed feedback for
    debugging or observation but may use additional resources due to
    verbose output.

  - **"grid"** : Displays progress and messages in the console while the
    function estimates the optimal lag values. This mode is suitable for
    interactive use or for users who want to monitor the estimation
    process in real-time. It provides detailed feedback for debugging or
    observation but may use additional resources due to verbose output.

  - **"grid_custom"**: Suppresses most or all console output,
    prioritizing faster execution and reduced resource usage on PCs or
    servers. This mode is recommended for high-performance scenarios,
    batch processing, or when the estimation process does not require
    user monitoring. Suitable for large-scale or repeated runs where
    output is unnecessary.

  - **User-defined vector**: A numeric vector of lag values specified by
    the user, allowing full customization of the lag structure used in
    model estimation. When a user-defined vector is provided (e.g.,
    `c(1, 2, 4, 5)`), the function skips the lag optimization process
    and directly uses the specified lags.

    Users can define lag values directly as a numeric vector. For
    example: `mode = c(1, 2, 4, 5)` assigns lags of 1, 2, 4, and 5 to
    variables in the specified order. Alternatively, lag values can be
    assigned to variables by name for clarity and control. For example:
    If the long-run model defined as
    `DriversKilled~Asy(PetrolPrice)+ drivers`, then
    `mode = c(DriversKilled = 2, PetrolPrice_POS = 3, PetrolPrice_NEG = 1, drivers = 3)`
    assigns lags to variables explicitly. Ensure that the lags are
    correctly designated by verifying the result using
    `kardl_model$proper_lag` after estimation.

    ***Attention!*** A function-based criterion or user-defined function
    can be specified for model selection, but this is only supported for
    `mode = "grid_custom"` and `mode = "quick"`. The `mode = "grid"`
    option is restricted to predefined criteria (e.g., AIC or BIC). For
    more information on available criteria, see the
    [`model_criterion`](https://karamelikli.github.io/kardl/reference/model_criterion.md)
    function documentation.

    - When using a numeric vector, ensure the order of lag values
      matches the variables in your formula.

    - If using named vectors, double-check the variable names to avoid
      mismatches or unintended results.

    - This mode bypasses the automatic lag optimization and assumes the
      user-defined lags are correct.

- criterion:

  A string specifying the information criterion to be used for selecting
  the optimal lag structure. The available options are:

  - **"AIC"**: Akaike Information Criterion (default). This criterion
    balances model fit and complexity, favoring models that explain the
    data well with fewer parameters.

  - **"BIC"**: Bayesian Information Criterion. This criterion imposes a
    stronger penalty for model complexity than AIC, making it more
    conservative in selecting models with fewer parameters.

  - **"AICc"**: Corrected Akaike Information Criterion. This is an
    adjusted version of AIC that accounts for small sample sizes, making
    it more suitable when the number of observations is limited relative
    to the number of parameters.

  - **"HQ"**: Hannan-Quinn Information Criterion. This criterion
    provides a compromise between AIC and BIC, favoring models that
    balance fit and complexity without being overly conservative.

  - **AdjR2**: Adjusted R-squared. This criterion adjusts the R-squared
    value for the number of predictors in the model, favoring models
    that explain more variance with fewer predictors.

  The criterion can be specified as a string (e.g., `"AIC"`) or as a
  user-defined function that takes a fitted model object. Please visit
  the
  [`model_criterion`](https://karamelikli.github.io/kardl/reference/model_criterion.md)
  function documentation for more details on using custom criteria.

- different_asym_lag:

  A logical value indicating whether to allow different lag lengths for
  positive and negative decompositions.

- batch:

  A string specifying the batch processing configuration in the format
  "current_batch/total_batches". If a user utilize grid or grid_custom
  mode and want to split the lag search into multiple batches, this
  parameter can be used to define the current batch and the total number
  of batches. For example, "2/5" indicates that the current batch is the
  second out of a total of five batches. The default value is "1/1",
  meaning that the entire lag search is performed in a single batch.

- ...:

  Additional arguments that can be passed to the function. These
  arguments can be used to specify other settings or parameters that are
  not explicitly defined in the main arguments.

## Value

An object of class `kardl_lm` containing the estimated ARDL or NARDL
model. The object includes the following components:

- args_info:

  A list of input arguments used for the estimation. It includes the
  data, formula, maxlag, mode, criterion, different_asym_lag, and batch
  settings.

- extracted_info:

  A list containing extracted information from the input data and
  formula, such as variable names, deterministic terms, asymmetric
  variables, and the prepared dataset for estimation.

- time_info:

  A list containing timing information for the estimation process,
  including start time, end time, and total duration.

- lag_info:

  A list containing lag selection information, including the optimal lag
  configuration and criteria values for different lag combinations.

- est_info:

  A list containing estimation details, such as the type of model,
  estimation method, model formula, number of parameters (k), number of
  observations (n), start and end points of the fitted values, and total
  time span.

- model:

  The fitted linear model object of class `lm` representing the
  estimated ARDL or NARDL model.

## Details

The general formula for the long-run model is specified as follows: \$\$
\begin{aligned} \Delta {y}\_t = c + \eta \_0 {y}\_{t-1} +
\sum\_{j=1}^{p} { \gamma\_{j} \Delta {y}\_{t-j} } +\sum\_{i=1}^{m} {(
\eta ^{+}\_i {x}^{+}\_{i,t-1 } + \eta ^{-}\_i {x}^{-}\_{i,t-1 } ) } +
\sum\_{i=m+1}^{k} {\eta \_i {x}\_{i,t-1 } } + \newline \sum\_{i=1}^{m}
{\sum\_{j=0}^{q^+\_i} { \beta^+\_{ij} \Delta {x}^+\_{i,t-j} } } +
\sum\_{i=1}^{m} {\sum\_{j=0}^{q^-\_i} { \beta^-\_{ij} \Delta
{x}^-\_{i,t-j} } } + \sum\_{i=m+1}^{k} {\sum\_{j=0}^{q_i} { \beta\_{ij}
\Delta {x}\_{i,t-j} } } + e_t \end{aligned} \$\$

Where:

- \\y_t\\ is the dependent variable at time t.

- \\c\\ is the constant term.

- \\\eta_0\\ is the coefficient of the lagged dependent variable.

- \\\gamma_j\\ are the coefficients of the lagged differences of the
  dependent variable.

- \\\eta^+\_i\\ and \\\eta^-\_i\\ are the coefficients of the positive
  and negative decompositions of the independent variables,
  respectively.

- \\\eta_i\\ are the coefficients of the independent variables that do
  not have asymmetric decompositions.

- \\\beta^+\_{ij}\\ and \\\beta^-\_{ij}\\ are the coefficients of the
  lagged differences of the positive and negative decompositions of the
  independent variables, respectively.

- \\\beta\_{ij}\\ are the coefficients of the lagged differences of the
  independent variables that do not have asymmetric decompositions.

- \\e_t\\ is the error term at time t.

- \\p\\ is the maximum lag length for the dependent variable.

- \\q^+\_i\\ and \\q^-\_i\\ are the maximum lag lengths for the positive
  and negative decompositions of the independent variables,
  respectively.

- \\q_i\\ is the maximum lag length for the independent variables that
  do not have asymmetric decompositions.

- \\m\\ is the number of independent variables with asymmetric
  decompositions.

- \\k\\ is the total number of independent variables.

- \\\Delta\\ denotes the first difference operator.

- \\x^+\_{i,t}\\ and \\x^-\_{i,t}\\ represent the positive and negative
  decompositions of the independent variable \\x_i\\ at time t,
  respectively.

## Notation of reported coefficients

In the reported coefficients, the prefix `L` denotes lagged variables,
where the accompanying number indicates the lag order, and `.d.` denotes
first differences. Accordingly, `L1.drivers` represents the first lag of
the level of `drivers` (long-run component), while `L3.d.drivers`
denotes the third lag of the first-differenced `drivers` (short-run
component).

In addition, the suffixes `_POS` and `_NEG` indicate the positive and
negative partial sum components of a variable, respectively. This
notation is used by default and remains valid unless modified through
the
[`kardl_set()`](https://karamelikli.github.io/kardl/reference/kardl_set.md)
function.

## See also

[`ecm`](https://karamelikli.github.io/kardl/reference/ecm.md),
[`kardl_set`](https://karamelikli.github.io/kardl/reference/kardl_set.md),
[`kardl_get`](https://karamelikli.github.io/kardl/reference/kardl_get.md),
[`kardl_reset`](https://karamelikli.github.io/kardl/reference/kardl_reset.md),
[`model_criterion`](https://karamelikli.github.io/kardl/reference/model_criterion.md)

## Examples

``` r


# Example: Road safety analysis using UK Seatbelts data
# Analyzing the effect of seatbelt law on driver deaths

kardl_set(
  formula = DriversKilled ~ PetrolPrice + drivers + Asymmetr(PetrolPrice) +
    deterministic(law) + trend,
  data = Seatbelts,
  maxlag = 2
) # setting the default values of the kardl function


# using the grid_custom mode with batch processing

kardl_model_grid <- kardl(
  mode = "grid_custom",
  batch = "2/3",
  criterion = "BIC"
)
kardl_model_grid
#> Optimal lags for each variable ( BIC ):
#> 
#> DriversKilled: 1, asyP_PetrolPrice_PP: 1, asyN_PetrolPrice_NN: 0, drivers: 0 
#> 
#> 
#> Call:
#> lm(formula = my_formula, data = model_data)
#> 
#> Coefficients:
#>              (Intercept)          L1.DriversKilled    L1.asyP_PetrolPrice_PP  
#>               -9.189e+00                -1.124e+00                 3.881e+00  
#>   L1.asyN_PetrolPrice_NN                L1.drivers        L1.d.DriversKilled  
#>               -1.580e+01                 8.782e-02                 1.391e-01  
#> L0.d.asyP_PetrolPrice_PP  L1.d.asyP_PetrolPrice_PP  L0.d.asyN_PetrolPrice_NN  
#>               -2.913e+02                -9.587e+01                 1.200e+03  
#>             L0.d.drivers                       law                     trend  
#>                7.932e-02                 2.909e+00                 2.484e-03  
#> 

kardl_model2 <- kardl(mode = c(2, 1, 1, 3))

# Getting the results
kardl_model2
#> Optimal lags for each variable ( AIC ):
#> 
#> DriversKilled: 2, asyP_PetrolPrice_PP: 1, asyN_PetrolPrice_NN: 1, drivers: 3 
#> 
#> 
#> Call:
#> lm(formula = my_formula, data = model_data)
#> 
#> Coefficients:
#>              (Intercept)          L1.DriversKilled    L1.asyP_PetrolPrice_PP  
#>               -8.687e+00                -1.145e+00                 1.859e+00  
#>   L1.asyN_PetrolPrice_NN                L1.drivers        L1.d.DriversKilled  
#>               -4.498e+01                 8.940e-02                 1.752e-01  
#>       L2.d.DriversKilled  L0.d.asyP_PetrolPrice_PP  L1.d.asyP_PetrolPrice_PP  
#>                1.204e-01                -3.019e+02                -1.294e+02  
#> L0.d.asyN_PetrolPrice_NN  L1.d.asyN_PetrolPrice_NN              L0.d.drivers  
#>                8.779e+02                 1.254e+02                 7.815e-02  
#>             L1.d.drivers              L2.d.drivers              L3.d.drivers  
#>               -1.617e-03                -1.313e-02                -2.473e-05  
#>                      law                     trend  
#>                3.193e+00                -2.662e-02  
#> 

# Getting the summary of the results
summary(kardl_model2)
#> 
#> Call:
#> lm(formula = my_formula, data = model_data)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -27.109  -7.140  -0.834   7.803  34.126 
#> 
#> Coefficients:
#>                            Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)              -8.687e+00  1.136e+01  -0.764   0.4457    
#> L1.DriversKilled         -1.145e+00  1.352e-01  -8.470 1.09e-14 ***
#> L1.asyP_PetrolPrice_PP    1.859e+00  9.999e+01   0.019   0.9852    
#> L1.asyN_PetrolPrice_NN   -4.498e+01  1.793e+02  -0.251   0.8022    
#> L1.drivers                8.940e-02  1.340e-02   6.671 3.38e-10 ***
#> L1.d.DriversKilled        1.752e-01  1.092e-01   1.605   0.1103    
#> L2.d.DriversKilled        1.204e-01  7.687e-02   1.567   0.1190    
#> L0.d.asyP_PetrolPrice_PP -3.019e+02  3.368e+02  -0.896   0.3713    
#> L1.d.asyP_PetrolPrice_PP -1.294e+02  3.548e+02  -0.365   0.7158    
#> L0.d.asyN_PetrolPrice_NN  8.779e+02  9.494e+02   0.925   0.3564    
#> L1.d.asyN_PetrolPrice_NN  1.254e+02  8.730e+02   0.144   0.8860    
#> L0.d.drivers              7.815e-02  4.743e-03  16.478  < 2e-16 ***
#> L1.d.drivers             -1.617e-03  1.000e-02  -0.162   0.8717    
#> L2.d.drivers             -1.313e-02  7.416e-03  -1.770   0.0785 .  
#> L3.d.drivers             -2.473e-05  4.581e-03  -0.005   0.9957    
#> law                       3.193e+00  3.762e+00   0.849   0.3973    
#> trend                    -2.662e-02  1.577e-01  -0.169   0.8662    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 11.37 on 171 degrees of freedom
#>   (4 observations deleted due to missingness)
#> Multiple R-squared:  0.7563, Adjusted R-squared:  0.7335 
#> F-statistic: 33.16 on 16 and 171 DF,  p-value: < 2.2e-16
#> 

# using '.' in the formula means that all variables in the data will be used

fit_bic <- kardl(formula = DriversKilled ~ . + deterministic(law))
fit_bic
#> Optimal lags for each variable ( AIC ):
#> 
#> DriversKilled: 2, drivers: 2, front: 0, rear: 0, kms: 1, PetrolPrice: 0, VanKilled: 0 
#> 
#> 
#> Call:
#> lm(formula = my_formula, data = model_data)
#> 
#> Coefficients:
#>        (Intercept)    L1.DriversKilled          L1.drivers            L1.front  
#>         -2.953e+00          -1.119e+00           8.015e-02           5.054e-03  
#>            L1.rear              L1.kms      L1.PetrolPrice        L1.VanKilled  
#>          1.553e-03           4.528e-04          -6.138e+01           1.090e-01  
#> L1.d.DriversKilled  L2.d.DriversKilled        L0.d.drivers        L1.d.drivers  
#>          1.786e-01           1.324e-01           7.579e-02          -3.126e-03  
#>       L2.d.drivers          L0.d.front           L0.d.rear            L0.d.kms  
#>         -1.783e-02          -4.833e-03           3.029e-03          -1.175e-03  
#>           L1.d.kms    L0.d.PetrolPrice      L0.d.VanKilled                 law  
#>         -1.172e-03          -1.108e+02          -9.841e-02           3.699e+00  
#> 

# Setting max lag instead of default value [4]
kardl(DriversKilled ~ PetrolPrice + drivers + Lasymmetric(PetrolPrice),
  Seatbelts,
  maxlag = 3, mode = "grid_custom"
)
#> Optimal lags for each variable ( AIC ):
#> 
#> DriversKilled: 1, PetrolPrice: 0, drivers: 1 
#> 
#> 
#> Call:
#> lm(formula = my_formula, data = model_data)
#> 
#> Coefficients:
#>            (Intercept)        L1.DriversKilled  L1.asyP_PetrolPrice_PP  
#>               -0.93070                -0.99602               -60.09542  
#> L1.asyN_PetrolPrice_NN              L1.drivers      L1.d.DriversKilled  
#>              -82.44844                 0.07285                 0.02822  
#>       L0.d.PetrolPrice            L0.d.drivers            L1.d.drivers  
#>              -25.55978                 0.07852                 0.01329  
#> 

# Using another criterion for finding the best lag
kardl_set(criterion = "HQ") # setting the criterion to HQ
kardl(mode = "grid_custom")
#> Optimal lags for each variable ( HQ ):
#> 
#> DriversKilled: 1, asyP_PetrolPrice_PP: 0, asyN_PetrolPrice_NN: 0, drivers: 0 
#> 
#> 
#> Call:
#> lm(formula = my_formula, data = model_data)
#> 
#> Coefficients:
#>              (Intercept)          L1.DriversKilled    L1.asyP_PetrolPrice_PP  
#>               -9.988e+00                -1.122e+00                -5.514e+00  
#>   L1.asyN_PetrolPrice_NN                L1.drivers        L1.d.DriversKilled  
#>               -2.496e+01                 8.791e-02                 1.396e-01  
#> L0.d.asyP_PetrolPrice_PP  L0.d.asyN_PetrolPrice_NN              L0.d.drivers  
#>               -2.829e+02                 1.146e+03                 7.957e-02  
#>                      law                     trend  
#>                3.079e+00                 4.598e-03  
#> 

# using default values of lags
kardl(mode = c(1, 2, 3, 0))
#> Optimal lags for each variable ( HQ ):
#> 
#> DriversKilled: 1, asyP_PetrolPrice_PP: 2, asyN_PetrolPrice_NN: 3, drivers: 0 
#> 
#> 
#> Call:
#> lm(formula = my_formula, data = model_data)
#> 
#> Coefficients:
#>              (Intercept)          L1.DriversKilled    L1.asyP_PetrolPrice_PP  
#>                -10.32621                  -1.13684                 -17.04269  
#>   L1.asyN_PetrolPrice_NN                L1.drivers        L1.d.DriversKilled  
#>                -76.15138                   0.08935                   0.14099  
#> L0.d.asyP_PetrolPrice_PP  L1.d.asyP_PetrolPrice_PP  L2.d.asyP_PetrolPrice_PP  
#>               -304.28093                 -43.50931                 175.62124  
#> L0.d.asyN_PetrolPrice_NN  L1.d.asyN_PetrolPrice_NN  L2.d.asyN_PetrolPrice_NN  
#>               1061.52868                 284.12478                -642.56827  
#> L3.d.asyN_PetrolPrice_NN              L0.d.drivers                       law  
#>               -788.63175                   0.07966                   4.59374  
#>                    trend  
#>                 -0.04246  
#> 

# For using different lag values for negative and positive decompositions
# of non-linear variables setting the same lags for positive and negative
# decompositions.

same <- kardl(
  formula = DriversKilled ~ Asymmetric(PetrolPrice),
  maxlag = 2, mode = "grid_custom",
  different_asym_lag = FALSE
)
dif <- kardl(
  formula = DriversKilled ~ Sasymmetric(PetrolPrice),
  maxlag = 2, mode = "grid_custom",
  different_asym_lag = TRUE
)
kardl_extract(same, "opt_lag")
#>       DriversKilled asyP_PetrolPrice_PP asyN_PetrolPrice_NN 
#>                   1                   0                   0 
kardl_extract(dif, "opt_lag")
#>       DriversKilled asyP_PetrolPrice_PP asyN_PetrolPrice_NN 
#>                   1                   0                   0 

# Optional: use magrittr if available
library(magrittr)
kardl_model_pipe <- Seatbelts %>%
  kardl(mode = "grid_custom", data = .)
kardl_model_pipe
#> Optimal lags for each variable ( HQ ):
#> 
#> DriversKilled: 1, asyP_PetrolPrice_PP: 0, asyN_PetrolPrice_NN: 0, drivers: 0 
#> 
#> 
#> Call:
#> lm(formula = my_formula, data = model_data)
#> 
#> Coefficients:
#>              (Intercept)          L1.DriversKilled    L1.asyP_PetrolPrice_PP  
#>               -9.988e+00                -1.122e+00                -5.514e+00  
#>   L1.asyN_PetrolPrice_NN                L1.drivers        L1.d.DriversKilled  
#>               -2.496e+01                 8.791e-02                 1.396e-01  
#> L0.d.asyP_PetrolPrice_PP  L0.d.asyN_PetrolPrice_NN              L0.d.drivers  
#>               -2.829e+02                 1.146e+03                 7.957e-02  
#>                      law                     trend  
#>                3.079e+00                 4.598e-03  
#> 
```

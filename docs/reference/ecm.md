# Estimate a Restricted ECM Model

The `ecm` function estimates a restricted Error Correction Model (ECM)
based on the provided data and model specification. This function is
designed to test for cointegration using the PSS t Bound test, which
assesses the presence of a long-term equilibrium relationship between
the dependent variable and the independent variables in the model.

## Usage

``` r
ecm(
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

A list containing the results of the restricted ECM test, including:

- `ecm`: The estimated ECM model objects including:

  - `longrun_eq`: The estimated long-run model equation object.

  - `shortrun_eq`: The estimated short-run model equation.

  - `ecm_l`: The estimated long-run model object.

- **args_info**: A list of input arguments used for the estimation. It
  includes the data, formula, maxlag, mode, criterion,
  different_asym_lag, and batch settings.

- **extracted_info**: A list containing extracted information from the
  input data and formula, such as variable names, deterministic terms,
  asymmetric variables, and the prepared dataset for estimation.

- **time_info**: A list containing timing information for the estimation
  process, including start time, end time, and total duration.

- **lag_info**: A list containing lag selection information, including
  the optimal lag configuration and criteria values for different lag
  combinations.

- **est_info**: A list containing estimation details, such as the type
  of model, estimation method, model formula, number of parameters (k),
  number of observations (n), start and end points of the fitted values,
  and total time span.

- **model**: The fitted linear model object of class `lm` representing
  the estimated ARDL or NARDL model.

## Hypothesis testing

The null and alternative hypotheses for the restricted ECM test are as
follows:

\$\$\mathbf{H\_{0}:} \theta = 0\$\$ \$\$\mathbf{H\_{1}:} \theta \neq
0\$\$

The null hypothesis (\\H\_{0}\\) states that there is no cointegration
in the model, meaning that the long-run relationship between the
variables is not significant. The alternative hypothesis (\\H\_{1}\\)
suggests that there is cointegration, indicating a significant long-term
relationship between the variables.

The test statistic is calculated as the t-statistic of the coefficient
of the error correction term (\\\theta\\) in the ECM model. If the
absolute value of the t-statistic exceeds the critical value from the
PSS t Bound table, we reject the null hypothesis in favor of the
alternative hypothesis, indicating that cointegration is present.

The cases for the restricted ECM Bound test are defined as follows:

- `case 1`: No constant, no trend.

  This case is used when the model does not include a constant term or a
  trend term. It is suitable for models where the variables are
  stationary and do not exhibit any long-term trends.

  The model is specified as follows:

  \$\$ \begin{aligned} \Delta y_t = \sum\_{j=1}^{p} \gamma_j \Delta
  y\_{t-j} + \sum\_{i=1}^{k} \sum\_{j=0}^{q_i} \beta\_{ij} \Delta
  x\_{i,t-j} + \theta (y\_{t-1} - \sum\_{i=1}^{k} \alpha_i x\_{i,t-1}
  ) + e_t \end{aligned} \$\$

- `case 2`: Restricted constant, no trend.

  This case is used when the model includes a constant term but no trend
  term. It is suitable for models where the variables exhibit a
  long-term relationship but do not have a trend component.

  The model is specified as follows:

  \$\$ \begin{aligned} \Delta y_t &= \sum\_{j=1}^{p} \gamma_j \Delta
  y\_{t-j} + \sum\_{i=1}^{k} \sum\_{j=0}^{q_i} \beta\_{ij} \Delta
  x\_{i,t-j} + \theta (y\_{t-1} - \alpha_0 - \sum\_{i=1}^{k} \alpha_i
  x\_{i,t-1} ) + e_t \end{aligned} \$\$

- `case 3`: Unrestricted constant, no trend.

  This case is used when the model includes an unrestricted constant
  term but no trend term. It is suitable for models where the variables
  exhibit a long-term relationship with a constant but do not have a
  trend component.

  The model is specified as follows:

  \$\$ \begin{aligned} \Delta y_t &= \sum\_{j=1}^{p} \gamma_j \Delta
  y\_{t-j} + \sum\_{i=1}^{k} \sum\_{j=0}^{q_i} \beta\_{ij} \Delta
  x\_{i,t-j} + \theta (y\_{t-1} - \alpha_0 - \sum\_{i=1}^{k} \alpha_i
  x\_{i,t-1} ) + e_t \end{aligned} \$\$

- `case 4`: Unrestricted Constant, restricted trend.

  This case is used when the model includes an unrestricted constant
  term and a restricted trend term. It is suitable for models where the
  variables exhibit a long-term relationship with a constant and a trend
  component.

  The model is specified as follows:

  \$\$ \begin{aligned} \Delta y_t &= \phi + \sum\_{j=1}^{p} \gamma_j
  \Delta y\_{t-j} + \sum\_{i=1}^{k} \sum\_{j=0}^{q_i} \beta\_{ij} \Delta
  x\_{i,t-j} + \theta (y\_{t-1} - \pi (t-1) - \sum\_{i=1}^{k} \alpha_i
  x\_{i,t-1} ) + e_t \end{aligned} \$\$

- `case 5`: Unrestricted constant, unrestricted trend.

The Error Correction Model (ECM) is specified as follows:

\$\$ \begin{aligned} \Delta y_t &= \phi + \varphi t + \sum\_{j=1}^{p}
\gamma_j \Delta y\_{t-j} + \sum\_{i=1}^{k} \sum\_{j=0}^{q_i} \beta\_{ij}
\Delta x\_{i,t-j} + \theta (y\_{t-1} - \sum\_{i=1}^{k} \alpha_i
x\_{i,t-1} ) + e_t \end{aligned} \$\$

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

[`kardl`](https://karamelikli.github.io/kardl/reference/kardl.md)
[`pssf`](https://karamelikli.github.io/kardl/reference/pssf.md)
[`psst`](https://karamelikli.github.io/kardl/reference/psst.md) `ecm`
[`narayan`](https://karamelikli.github.io/kardl/reference/narayan.md)

## Examples

``` r

# Example: Road safety analysis using UK Seatbelts data
# Analyzing the effect of seatbelt law on driver deaths
kardl_set(
  formula = DriversKilled ~ PetrolPrice + drivers + asym(PetrolPrice) +
    deterministic(law) + trend,
  data = Seatbelts,
  maxlag = 3
)

# Using the grid mode with batch processing to decrease execution time
ecm_model_grid <- ecm(mode = "grid")
#> 
ecm_model_grid
#> Optimal lags for each variable ( AIC ):
#> 
#> DriversKilled: 1, PetrolPrice_POS: 0, PetrolPrice_NEG: 0, drivers: 1 
#> 
#> 
#> Call:
#> lm(formula = shortrun_eq, data = ecm_data)
#> 
#> Coefficients:
#>          (Intercept)                EcmRes    L1.d.DriversKilled  
#>            1.760e+00            -1.122e+00             1.317e-01  
#> L0.d.PetrolPrice_POS  L0.d.PetrolPrice_NEG          L0.d.drivers  
#>           -2.577e+02             1.038e+03             8.092e-02  
#>                  law                 trend  
#>            3.896e+00            -9.601e-03  
#> 
#> 
#> Notes:
#>    • The coefficient of the error correction term (EcmRes) is less than -1. This may suggest over-adjustment or instability in the long-run relationship. 
#> 
#> 

# Checking the cointegration test results using Pesaran t test
psst(ecm_model_grid)
#> 
#>  Pesaran-Shin-Smith (PSS) Bounds t-test for cointegration
#> 
#> data:  model
#> t = -12.828
#> alternative hypothesis: Cointegrating relationship exists
#> 

# Getting the details of psst result
summary(psst(ecm_model_grid))
#> KARDL Cointegration Test Summary
#> 
#> Pesaran-Shin-Smith (PSS) Bounds t-test for cointegration
#> 
#> t statistic = -12.8280123
#> 
#> Critical Values (Lower & Upper Bounds):
#>            L     U
#>   10%  -3.13 -3.84
#>   5%   -3.41 -4.16
#>   2.5% -3.65 -4.42
#>   1%   -3.96 -4.73
#> 
#> Decision:
#>   Reject H0 → Cointegration (at 1% level)
#> 
#> Comparison:
#>   At the 1% significance level, t (12.8280123) exceeds the upper bound (4.73).
#>   This indicates that the variables tend to move together over  time.
#>   Conclusion: There is strong evidence of a long-run relationship  (cointegration).
#> 
#> Hypotheses:
#>   H0: Coef(EcmRes) = 0
#>   H1: Coef(EcmRes) ≠ 0
#> 
#> Model Details:
#>   Number of regressors (k): 3
#>   Case: V 

# Using the grid_custom mode for faster execution without console output
ecm_model <- ecm(
  mode = "grid_custom",
  criterion = "HQ", batch = "2/3"
)
ecm_model
#> Optimal lags for each variable ( HQ ):
#> 
#> DriversKilled: 2, PetrolPrice_POS: 0, PetrolPrice_NEG: 0, drivers: 0 
#> 
#> 
#> Call:
#> lm(formula = shortrun_eq, data = ecm_data)
#> 
#> Coefficients:
#>          (Intercept)                EcmRes    L1.d.DriversKilled  
#>            1.690e+00            -1.137e+00             1.376e-01  
#>   L2.d.DriversKilled  L0.d.PetrolPrice_POS  L0.d.PetrolPrice_NEG  
#>            1.610e-02            -2.480e+02             1.045e+03  
#>         L0.d.drivers                   law                 trend  
#>            8.094e-02             3.983e+00            -9.322e-03  
#> 
#> 
#> Notes:
#>    • The coefficient of the error correction term (EcmRes) is less than -1. This may suggest over-adjustment or instability in the long-run relationship. 
#> 
#> 

# Estimating the model with user-defined lag values
ecm_model2 <- ecm(mode = c(2, 1, 1, 3))

# Getting the results
ecm_model2
#> Optimal lags for each variable ( AIC ):
#> 
#> DriversKilled: 2, PetrolPrice_POS: 1, PetrolPrice_NEG: 1, drivers: 3 
#> 
#> 
#> Call:
#> lm(formula = shortrun_eq, data = ecm_data)
#> 
#> Coefficients:
#>          (Intercept)                EcmRes    L1.d.DriversKilled  
#>            1.836e+00            -1.151e+00             1.796e-01  
#>   L2.d.DriversKilled  L0.d.PetrolPrice_POS  L1.d.PetrolPrice_POS  
#>            1.239e-01            -3.127e+02            -1.161e+02  
#> L0.d.PetrolPrice_NEG  L1.d.PetrolPrice_NEG          L0.d.drivers  
#>            8.225e+02             8.272e+01             7.906e-02  
#>         L1.d.drivers          L2.d.drivers          L3.d.drivers  
#>           -3.097e-03            -1.436e-02            -8.408e-04  
#>                  law                 trend  
#>            3.852e+00            -9.890e-03  
#> 
#> 
#> Notes:
#>    • The coefficient of the error correction term (EcmRes) is less than -1. This may suggest over-adjustment or instability in the long-run relationship. 
#> 
#> 

# Getting the summary of the results
summary(ecm_model2)
#> 
#> Call:
#> lm(formula = shortrun_eq, data = ecm_data)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -26.596  -6.808  -0.674   7.842  34.317 
#> 
#> Coefficients:
#>                        Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)           1.836e+00  1.950e+00   0.941   0.3478    
#> EcmRes               -1.151e+00  1.326e-01  -8.684  2.7e-15 ***
#> L1.d.DriversKilled    1.796e-01  1.076e-01   1.668   0.0970 .  
#> L2.d.DriversKilled    1.239e-01  7.579e-02   1.635   0.1038    
#> L0.d.PetrolPrice_POS -3.127e+02  3.193e+02  -0.979   0.3288    
#> L1.d.PetrolPrice_POS -1.161e+02  3.227e+02  -0.360   0.7194    
#> L0.d.PetrolPrice_NEG  8.225e+02  8.621e+02   0.954   0.3414    
#> L1.d.PetrolPrice_NEG  8.272e+01  8.521e+02   0.097   0.9228    
#> L0.d.drivers          7.906e-02  4.022e-03  19.656  < 2e-16 ***
#> L1.d.drivers         -3.097e-03  8.929e-03  -0.347   0.7291    
#> L2.d.drivers         -1.436e-02  6.567e-03  -2.187   0.0301 *  
#> L3.d.drivers         -8.408e-04  3.978e-03  -0.211   0.8329    
#> law                   3.852e+00  3.280e+00   1.174   0.2418    
#> trend                -9.890e-03  1.932e-02  -0.512   0.6094    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 11.28 on 174 degrees of freedom
#> Multiple R-squared:  0.7558, Adjusted R-squared:  0.7376 
#> F-statistic: 41.43 on 13 and 174 DF,  p-value: < 2.2e-16
#> 

# Alternative specification
summary(ecm(DriversKilled ~ drivers + asym(PetrolPrice) + trend, Seatbelts))
#> 
#> Call:
#> lm(formula = shortrun_eq, data = ecm_data)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -25.229  -7.337  -0.904   6.959  36.079 
#> 
#> Coefficients:
#>                        Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)           1.139e+00  1.798e+00   0.634    0.527    
#> EcmRes               -1.039e+00  1.034e-01 -10.050   <2e-16 ***
#> L1.d.DriversKilled    5.270e-02  7.429e-02   0.709    0.479    
#> L0.d.drivers          8.118e-02  3.877e-03  20.941   <2e-16 ***
#> L1.d.drivers          7.803e-03  6.292e-03   1.240    0.217    
#> L0.d.PetrolPrice_POS -2.959e+02  3.099e+02  -0.955    0.341    
#> L0.d.PetrolPrice_NEG  1.225e+03  7.909e+02   1.549    0.123    
#> trend                 3.711e-03  1.498e-02   0.248    0.805    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 11.27 on 182 degrees of freedom
#> Multiple R-squared:  0.7456, Adjusted R-squared:  0.7358 
#> F-statistic:  76.2 on 7 and 182 DF,  p-value: < 2.2e-16
#> 

# For increasing the performance of finding the most fitted lag vector
ecm(mode = "grid_custom")
#> Optimal lags for each variable ( AIC ):
#> 
#> DriversKilled: 1, PetrolPrice_POS: 0, PetrolPrice_NEG: 0, drivers: 1 
#> 
#> 
#> Call:
#> lm(formula = shortrun_eq, data = ecm_data)
#> 
#> Coefficients:
#>          (Intercept)                EcmRes    L1.d.DriversKilled  
#>            1.688e+00            -1.053e+00             5.880e-02  
#> L0.d.PetrolPrice_POS  L0.d.PetrolPrice_NEG          L0.d.drivers  
#>           -2.285e+02             9.629e+02             8.138e-02  
#>         L1.d.drivers                   law                 trend  
#>            7.649e-03             3.816e+00            -9.604e-03  
#> 
#> 
#> Notes:
#>    • The coefficient of the error correction term (EcmRes) is less than -1. This may suggest over-adjustment or instability in the long-run relationship. 
#> 
#> 

# Setting max lag instead of default value [4]
ecm(maxlag = 2, mode = "grid_custom")
#> Optimal lags for each variable ( AIC ):
#> 
#> DriversKilled: 1, PetrolPrice_POS: 0, PetrolPrice_NEG: 0, drivers: 1 
#> 
#> 
#> Call:
#> lm(formula = shortrun_eq, data = ecm_data)
#> 
#> Coefficients:
#>          (Intercept)                EcmRes    L1.d.DriversKilled  
#>            1.688e+00            -1.053e+00             5.880e-02  
#> L0.d.PetrolPrice_POS  L0.d.PetrolPrice_NEG          L0.d.drivers  
#>           -2.285e+02             9.629e+02             8.138e-02  
#>         L1.d.drivers                   law                 trend  
#>            7.649e-03             3.816e+00            -9.604e-03  
#> 
#> 
#> Notes:
#>    • The coefficient of the error correction term (EcmRes) is less than -1. This may suggest over-adjustment or instability in the long-run relationship. 
#> 
#> 

# Using another criterion for finding the best lag
ecm(criterion = "HQ", mode = "grid_custom")
#> Optimal lags for each variable ( HQ ):
#> 
#> DriversKilled: 1, PetrolPrice_POS: 0, PetrolPrice_NEG: 0, drivers: 0 
#> 
#> 
#> Call:
#> lm(formula = shortrun_eq, data = ecm_data)
#> 
#> Coefficients:
#>          (Intercept)                EcmRes    L1.d.DriversKilled  
#>            1.760e+00            -1.122e+00             1.317e-01  
#> L0.d.PetrolPrice_POS  L0.d.PetrolPrice_NEG          L0.d.drivers  
#>           -2.577e+02             1.038e+03             8.092e-02  
#>                  law                 trend  
#>            3.896e+00            -9.601e-03  
#> 
#> 
#> Notes:
#>    • The coefficient of the error correction term (EcmRes) is less than -1. This may suggest over-adjustment or instability in the long-run relationship. 
#> 
#> 

# For using different lag values for positive and negative decompositions
# Setting the same lags for positive and negative decompositions
kardl_set(different_asym_lag = FALSE)

diffAsymLags <- ecm(mode = "grid_custom")
kardl_extract(diffAsymLags, "opt_lag")
#>   DriversKilled PetrolPrice_POS PetrolPrice_NEG         drivers 
#>               1               0               0               1 

# Setting different lags for positive and negative decompositions
sameAsymLags <- ecm(different_asym_lag = TRUE, mode = "grid_custom")
kardl_extract(sameAsymLags, "opt_lag")
#>   DriversKilled PetrolPrice_POS PetrolPrice_NEG         drivers 
#>               1               0               0               1 

# Setting the prefixes and suffixes for nonlinear variables
kardl_reset()
kardl_set(
  asym_prefix = c("asyP_", "asyN_"),
  asym_suffix = c("_PP", "_NN")
)
customizedNames <- ecm(DriversKilled ~ PetrolPrice + drivers +
  asym(PetrolPrice), Seatbelts)
customizedNames
#> Optimal lags for each variable ( AIC ):
#> 
#> DriversKilled: 1, asyP_PetrolPrice_PP: 0, asyN_PetrolPrice_NN: 0, drivers: 1 
#> 
#> 
#> Call:
#> lm(formula = shortrun_eq, data = ecm_data)
#> 
#> Coefficients:
#>              (Intercept)                    EcmRes        L1.d.DriversKilled  
#>                1.483e+00                -1.039e+00                 5.249e-02  
#> L0.d.asyP_PetrolPrice_PP  L0.d.asyN_PetrolPrice_NN              L0.d.drivers  
#>               -2.899e+02                 1.210e+03                 8.119e-02  
#>             L1.d.drivers  
#>                7.827e-03  
#> 
#> 
#> Notes:
#>    • The coefficient of the error correction term (EcmRes) is less than -1. This may suggest over-adjustment or instability in the long-run relationship. 
#> 
#> 
```

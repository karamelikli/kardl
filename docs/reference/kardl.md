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
  data = NULL,
  formula = NULL,
  maxlag = NULL,
  mode = NULL,
  criterion = NULL,
  differentAsymLag = NULL,
  batch = NULL,
  ...
)
```

## Arguments

- data:

  The data of analysis

- formula:

  A formula specifying the long-run model equation. This formula defines
  the relationships between the dependent variable and explanatory
  variables, including options for deterministic terms, asymmetric
  variables, and a trend component.

  Example formula:
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

  ***examples***

  Using the default maximum lag (4)

  `kardl(data, MyFormula, maxlag = 4)`

  Reducing the maximum lag to 2 for faster computation

  `kardl(data, MyFormula, maxlag = 2)`

  Increasing the maximum lag to 8 for datasets with longer dependencies

  `kardl(data, MyFormula, maxlag = 8)`

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
    \`c(1, 2, 4, 5)\`), the function skips the lag optimization process
    and directly uses the specified lags.

    \- Users can define lag values directly as a numeric vector. For
    example: `mode = c(1, 2, 4, 5)` assigns lags of 1, 2, 4, and 5 to
    variables in the specified order. - Alternatively, lag values can be
    assigned to variables by name for clarity and control. For example:
    `mode = c(CPI = 2, ER_POS = 3, ER_NEG = 1, PPI = 3)` assigns lags to
    variables explicitly. - Ensure that the lags are correctly
    designated by verifying the result using `kardl_model$properLag`
    after estimation.

    ***Attention!*** -A function-based criterion or user-defined
    function can be specified for model selection, but this is only
    supported for `mode = "grid_custom"` and `mode = "quick"`. The
    `mode = "grid"` option is restricted to predefined criteria (e.g.,
    AIC or BIC). For more information on available criteria, see the
    [`modelCriterion`](modelCriterion.md) function documentation. - When
    using a numeric vector, ensure the order of lag values matches the
    variables in your formula. - If using named vectors, double-check
    the variable names to avoid mismatches or unintended results. - This
    mode bypasses the automatic lag optimization and assumes the
    user-defined lags are correct.

  The \`mode\` parameter provides flexibility for different use cases: -
  Use \`"grid"\` mode for debugging or interactive use where progress
  visibility is important. - Use \`"grid_custom"\` mode to minimize
  overhead in computationally intensive tasks. - Specify a user-defined
  vector to customize the lag structure based on prior knowledge or
  analysis.

  Selecting the appropriate mode can improve the efficiency and
  usability of the function depending on the user's requirements and the
  computational environment.

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

  The criterion can be specified as a string (e.g., `"AIC"`) or as a
  user-defined function that takes a fitted model object. Please visit
  the [`modelCriterion`](modelCriterion.md) function documentation for
  more details on using custom criteria.

- differentAsymLag:

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
  arguments can be used to

## Value

An object of class `kardl_lm` containing the estimated ARDL or NARDL
model. The object includes the following components:

- argsInfo:

  A list of input arguments used for the estimation. It includes the
  data, formula, maxlag, mode, criterion, differentAsymLag, and batch
  settings.

- extractedInfo:

  A list containing extracted information from the input data and
  formula, such as variable names, deterministic terms, asymmetric
  variables, and the prepared dataset for estimation.

- timeInfo:

  A list containing timing information for the estimation process,
  including start time, end time, and total duration.

- lagInfo:

  A list containing lag selection information, including the optimal lag
  configuration and criteria values for different lag combinations.

- estInfo:

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
\Delta {x}\_{i,t-j} } }+ e_t \end{aligned} \$\$

Where: - \\y_t\\ is the dependent variable at time t. - \\c\\ is the
constant term. - \\\eta_0\\ is the coefficient of the lagged dependent
variable. - \\\gamma_j\\ are the coefficients of the lagged differences
of the dependent variable. - \\\eta^+\_i\\ and \\\eta^-\_i\\ are the
coefficients of the positive and negative decompositions of the
independent variables, respectively. - \\\eta_i\\ are the coefficients
of the independent variables that do not have asymmetric
decompositions. - \\\beta^+\_{ij}\\ and \\\beta^-\_{ij}\\ are the
coefficients of the lagged differences of the positive and negative
decompositions of the independent variables, respectively. -
\\\beta\_{ij}\\ are the coefficients of the lagged differences of the
independent variables that do not have asymmetric decompositions. -
\\e_t\\ is the error term at time t. - \\p\\ is the maximum lag length
for the dependent variable. - \\q^+\_i\\ and \\q^-\_i\\ are the maximum
lag lengths for the positive and negative decompositions of the
independent variables, respectively. - \\q_i\\ is the maximum lag length
for the independent variables that do not have asymmetric
decompositions. - \\m\\ is the number of independent variables with
asymmetric decompositions. - \\k\\ is the total number of independent
variables. - \\\Delta\\ denotes the first difference operator. -
\\x^+\_{i,t}\\ and \\x^-\_{i,t}\\ represent the positive and negative
decompositions of the independent variable \\x_i\\ at time t,
respectively.

## See also

[`ecm`](ecm.md), [`kardl_set`](kardl_set.md),
[`kardl_get`](kardl_get.md), [`kardl_reset`](kardl_reset.md),
[`modelCriterion`](modelCriterion.md)

## Examples

``` r

# Sample article: THE DYNAMICS OF EXCHANGE RATE PASS-THROUGH TO DOMESTIC PRICES IN TURKEY

kardl_set(formula =CPI~ER+PPI+Asymmetr(ER)+deterministic(covid)+trend ,
          data=imf_example_data,
          maxlag=2
) # setting the default values of the kardl function


# using the grid_custom mode with batch processing

kardl_model_grid<-kardl( mode = "grid_custom",batch = "2/3")
kardl_model_grid
#> Optimal lags for each variable ( AIC ):
#> CPI: 1, asyP_ER_PP: 1, asyN_ER_NN: 0, PPI: 0 
#> 
#> Call:
#> L0.d.CPI ~ L1.CPI + L1.asyP_ER_PP + L1.asyN_ER_NN + L1.PPI + 
#>     L1.d.CPI + L0.d.asyP_ER_PP + L1.d.asyP_ER_PP + L0.d.asyN_ER_NN + 
#>     L0.d.PPI + covid + trend
#> 
#> Coefficients:
#>     (Intercept)           L1.CPI    L1.asyP_ER_PP    L1.asyN_ER_NN  
#>      -1.945e-01       -1.466e-02        1.080e-02        3.356e-02  
#>          L1.PPI         L1.d.CPI  L0.d.asyP_ER_PP  L1.d.asyP_ER_PP  
#>       3.945e-02        3.471e-01        1.073e-01        9.488e-02  
#> L0.d.asyN_ER_NN         L0.d.PPI            covid            trend  
#>       6.284e-03        1.649e-02        7.898e-03        9.699e-05  
#> 

kardl_model2<-kardl(mode = c( 2    ,  1    ,  1   ,   3 ))

# Getting the results
kardl_model2
#> Optimal lags for each variable ( AIC ):
#> CPI: 2, asyP_ER_PP: 1, asyN_ER_NN: 1, PPI: 3 
#> 
#> Call:
#> L0.d.CPI ~ L1.CPI + L1.asyP_ER_PP + L1.asyN_ER_NN + L1.PPI + 
#>     L1.d.CPI + L2.d.CPI + L0.d.asyP_ER_PP + L1.d.asyP_ER_PP + 
#>     L0.d.asyN_ER_NN + L1.d.asyN_ER_NN + L0.d.PPI + L1.d.PPI + 
#>     L2.d.PPI + L3.d.PPI + covid + trend
#> 
#> Coefficients:
#>     (Intercept)           L1.CPI    L1.asyP_ER_PP    L1.asyN_ER_NN  
#>      -2.760e-01       -1.745e-02        1.566e-02        3.488e-02  
#>          L1.PPI         L1.d.CPI         L2.d.CPI  L0.d.asyP_ER_PP  
#>       6.005e-02        3.893e-01       -8.462e-02        1.062e-01  
#> L1.d.asyP_ER_PP  L0.d.asyN_ER_NN  L1.d.asyN_ER_NN         L0.d.PPI  
#>       7.988e-02        1.612e-02        1.762e-02        2.207e-02  
#>        L1.d.PPI         L2.d.PPI         L3.d.PPI            covid  
#>      -1.508e-02       -3.872e-02       -3.269e-02        7.771e-03  
#>           trend  
#>      -5.595e-05  
#> 

# Getting the summary of the results
summary(kardl_model2)
#> 
#> Call:
#> L0.d.CPI ~ L1.CPI + L1.asyP_ER_PP + L1.asyN_ER_NN + L1.PPI + 
#>     L1.d.CPI + L2.d.CPI + L0.d.asyP_ER_PP + L1.d.asyP_ER_PP + 
#>     L0.d.asyN_ER_NN + L1.d.asyN_ER_NN + L0.d.PPI + L1.d.PPI + 
#>     L2.d.PPI + L3.d.PPI + covid + trend
#> 
#> Residuals:
#>       Min        1Q    Median        3Q       Max 
#> -0.055270 -0.007958 -0.001177  0.006413  0.102447 
#> 
#> Coefficients:
#>                   Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)     -2.760e-01  4.805e-02  -5.743 1.72e-08 ***
#> L1.CPI          -1.745e-02  3.777e-03  -4.620 5.01e-06 ***
#> L1.asyP_ER_PP    1.566e-02  4.894e-03   3.200 0.001472 ** 
#> L1.asyN_ER_NN    3.488e-02  6.323e-03   5.517 5.85e-08 ***
#> L1.PPI           6.005e-02  1.169e-02   5.135 4.21e-07 ***
#> L1.d.CPI         3.893e-01  4.407e-02   8.833  < 2e-16 ***
#> L2.d.CPI        -8.462e-02  4.221e-02  -2.005 0.045604 *  
#> L0.d.asyP_ER_PP  1.062e-01  1.817e-02   5.847 9.62e-09 ***
#> L1.d.asyP_ER_PP  7.988e-02  1.914e-02   4.174 3.60e-05 ***
#> L0.d.asyN_ER_NN  1.612e-02  4.721e-02   0.341 0.732959    
#> L1.d.asyN_ER_NN  1.762e-02  4.773e-02   0.369 0.712125    
#> L0.d.PPI         2.207e-02  8.615e-03   2.561 0.010751 *  
#> L1.d.PPI        -1.508e-02  1.100e-02  -1.371 0.170929    
#> L2.d.PPI        -3.872e-02  1.027e-02  -3.770 0.000185 ***
#> L3.d.PPI        -3.269e-02  8.718e-03  -3.749 0.000200 ***
#> covid            7.771e-03  3.919e-03   1.983 0.047998 *  
#> trend           -5.595e-05  1.314e-04  -0.426 0.670546    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 0.01486 on 449 degrees of freedom
#>   (4 observations deleted due to missingness)
#> Multiple R-squared:  0.6498, Adjusted R-squared:  0.6373 
#> F-statistic: 52.06 on 16 and 449 DF,  p-value: < 2.2e-16
#> 

# using '.' in the formula means that all variables in the data will be used

kardl(formula=CPI~.+deterministic(covid),criterion = "BIC")
#> Optimal lags for each variable ( BIC ):
#> CPI: 1, ER: 1, PPI: 1 
#> 
#> Call:
#> L0.d.CPI ~ L1.CPI + L1.ER + L1.PPI + L1.d.CPI + L0.d.ER + L1.d.ER + 
#>     L0.d.PPI + L1.d.PPI + covid
#> 
#> Coefficients:
#> (Intercept)       L1.CPI        L1.ER       L1.PPI     L1.d.CPI      L0.d.ER  
#>   0.0721925   -0.0151379    0.0156144   -0.0017714    0.4453614    0.0995449  
#>     L1.d.ER     L0.d.PPI     L1.d.PPI        covid  
#>   0.0871452    0.0058383    0.0238530    0.0008534  
#> 

# Setting max lag instead of default value [4]
kardl(imf_example_data,
      CPI~ER+PPI+Lasymmetric(ER),
      maxlag = 3, mode = "grid_custom")
#> Optimal lags for each variable ( AIC ):
#> CPI: 2, ER: 1, PPI: 2 
#> 
#> Call:
#> L0.d.CPI ~ L1.CPI + L1.asyP_ER_PP + L1.asyN_ER_NN + L1.PPI + 
#>     L1.d.CPI + L2.d.CPI + L0.d.ER + L1.d.ER + L0.d.PPI + L1.d.PPI + 
#>     L2.d.PPI
#> 
#> Coefficients:
#>   (Intercept)         L1.CPI  L1.asyP_ER_PP  L1.asyN_ER_NN         L1.PPI  
#>     -0.236543      -0.020302       0.018788       0.035472       0.044559  
#>      L1.d.CPI       L2.d.CPI        L0.d.ER        L1.d.ER       L0.d.PPI  
#>      0.414961      -0.073800       0.089085       0.078131       0.018947  
#>      L1.d.PPI       L2.d.PPI  
#>     -0.002251      -0.019242  
#> 

# Using another criterion for finding the best lag
kardl_set(criterion = "HQ") # setting the criterion to HQ
kardl( mode = "grid_custom")
#> Optimal lags for each variable ( HQ ):
#> CPI: 1, asyP_ER_PP: 1, asyN_ER_NN: 0, PPI: 0 
#> 
#> Call:
#> L0.d.CPI ~ L1.CPI + L1.asyP_ER_PP + L1.asyN_ER_NN + L1.PPI + 
#>     L1.d.CPI + L0.d.asyP_ER_PP + L1.d.asyP_ER_PP + L0.d.asyN_ER_NN + 
#>     L0.d.PPI + covid + trend
#> 
#> Coefficients:
#>     (Intercept)           L1.CPI    L1.asyP_ER_PP    L1.asyN_ER_NN  
#>      -1.945e-01       -1.466e-02        1.080e-02        3.356e-02  
#>          L1.PPI         L1.d.CPI  L0.d.asyP_ER_PP  L1.d.asyP_ER_PP  
#>       3.945e-02        3.471e-01        1.073e-01        9.488e-02  
#> L0.d.asyN_ER_NN         L0.d.PPI            covid            trend  
#>       6.284e-03        1.649e-02        7.898e-03        9.699e-05  
#> 

# using default values of lags
kardl( mode=c(1,2,3,0))
#> Optimal lags for each variable ( HQ ):
#> CPI: 1, asyP_ER_PP: 2, asyN_ER_NN: 3, PPI: 0 
#> 
#> Call:
#> L0.d.CPI ~ L1.CPI + L1.asyP_ER_PP + L1.asyN_ER_NN + L1.PPI + 
#>     L1.d.CPI + L0.d.asyP_ER_PP + L1.d.asyP_ER_PP + L2.d.asyP_ER_PP + 
#>     L0.d.asyN_ER_NN + L1.d.asyN_ER_NN + L2.d.asyN_ER_NN + L3.d.asyN_ER_NN + 
#>     L0.d.PPI + covid + trend
#> 
#> Coefficients:
#>     (Intercept)           L1.CPI    L1.asyP_ER_PP    L1.asyN_ER_NN  
#>      -1.915e-01       -1.471e-02        1.214e-02        3.064e-02  
#>          L1.PPI         L1.d.CPI  L0.d.asyP_ER_PP  L1.d.asyP_ER_PP  
#>       3.903e-02        3.401e-01        1.079e-01        8.900e-02  
#> L2.d.asyP_ER_PP  L0.d.asyN_ER_NN  L1.d.asyN_ER_NN  L2.d.asyN_ER_NN  
#>       1.156e-02        5.755e-03        2.990e-02        2.946e-02  
#> L3.d.asyN_ER_NN         L0.d.PPI            covid            trend  
#>       2.328e-02        1.650e-02        7.229e-03        3.733e-05  
#> 

# For using different lag values for negative and positive decompositions of non-linear variables
# setting the same lags for positive and negative decompositions.

same<-kardl(formula=CPI~Asymmetric(ER),maxlag=2, mode = "grid_custom",differentAsymLag = FALSE)
dif<-kardl(formula=CPI~Sasymmetric(ER),maxlag=2, mode = "grid_custom",differentAsymLag = TRUE)

same$lagInfo$OptLag
#>        CPI asyP_ER_PP asyN_ER_NN 
#>          1          1          1 
dif$lagInfo$OptLag
#>        CPI asyP_ER_PP asyN_ER_NN 
#>          1          1          0 

# Optional: use magrittr if available
library(magrittr)
  kardl_model_pipe <-  imf_example_data %>%
    kardl(mode = "grid_custom")

  kardl_model_pipe
#> Optimal lags for each variable ( HQ ):
#> CPI: 1, asyP_ER_PP: 1, asyN_ER_NN: 0, PPI: 0 
#> 
#> Call:
#> L0.d.CPI ~ L1.CPI + L1.asyP_ER_PP + L1.asyN_ER_NN + L1.PPI + 
#>     L1.d.CPI + L0.d.asyP_ER_PP + L1.d.asyP_ER_PP + L0.d.asyN_ER_NN + 
#>     L0.d.PPI + covid + trend
#> 
#> Coefficients:
#>     (Intercept)           L1.CPI    L1.asyP_ER_PP    L1.asyN_ER_NN  
#>      -1.945e-01       -1.466e-02        1.080e-02        3.356e-02  
#>          L1.PPI         L1.d.CPI  L0.d.asyP_ER_PP  L1.d.asyP_ER_PP  
#>       3.945e-02        3.471e-01        1.073e-01        9.488e-02  
#> L0.d.asyN_ER_NN         L0.d.PPI            covid            trend  
#>       6.284e-03        1.649e-02        7.898e-03        9.699e-05  
#> 
```

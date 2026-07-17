# Introduction to kardl

## Introduction

The `kardl` package is an R tool for estimating symmetric and asymmetric
Autoregressive Distributed Lag (ARDL) and Nonlinear ARDL (NARDL) models,
designed for econometricians and researchers analyzing cointegration and
dynamic relationships in time series data. It offers flexible model
specifications, allowing users to include deterministic variables,
asymmetric effects for short- and long-run dynamics, and trend
components. The package supports customizable lag structures, model
selection criteria (AIC, BIC, AICc, HQ), and parallel processing for
computational efficiency. Key features include:

- **Flexible Formula Specification**: Use `asymmetric()`,
  `lasymmetric()`, and `sasymmetric()` to model asymmetric effects in
  short- and long-run dynamics, and `deterministic()` for dummy
  variables.
- **Lag Optimization**: Choose between automatic lag selection
  (`"quick"`, `"grid"`, `"grid_custom"`) or user-defined lags.
- **Dynamic Analysis**: Compute long-run coefficients, perform
  cointegration tests (PSS F, PSS t, Narayan), and ECM estimation.

This vignette demonstrates how to use the
[`kardl()`](https://karamelikli.github.io/kardl/reference/kardl.md)
function to estimate an asymmetric ARDL model, perform diagnostic tests,
and visualize results, using economic data from Turkey.

## Installation

`kardl` in R can easily be installed from its CRAN repository:

``` r

install.packages("kardl")
library(kardl)
```

Alternatively, you can use the `devtools` package to load directly from
GitHub:

``` r

# Install required packages
install.packages(c(
  "stats", "msm", "lmtest", "nlWaldTest", "car", "strucchange",
  "utils", "ggplot2"
))
# Install kardl from GitHub
install.packages("devtools")
devtools::install_github("karamelikli/kardl")
```

Load the package:

``` r

library(kardl)
```

## Unique Features and Methodological Contributions

The `kardl` package implements several methodological extensions and
improvements for ARDL/NARDL modelling that go beyond standard
implementations available in R and other software:

- **different_asym_lag** option: Enables different lag orders for the
  positive and negative partial sum decompositions in NARDL models. This
  provides greater flexibility than the common symmetric lag restriction
  used in most existing packages.
- **Narayan cointegration test**
  ([`narayan()`](https://karamelikli.github.io/kardl/reference/narayan.md)):
  A dedicated small-sample bounds test (Narayan, 2005) with automatic
  handling of critical values for cases II–V. While the test exists in
  the literature, its seamless integration into a full ARDL/NARDL
  workflow is unique in R.
- **Symmetry tests**
  ([`symmetrytest()`](https://karamelikli.github.io/kardl/reference/symmetrytest.md)):
  Comprehensive Wald tests for both short-run and long-run symmetry in
  NARDL models.
- **Bootstrap confidence intervals for dynamic multipliers**: Robust
  uncertainty quantification around short-run, long-run, and impact
  multipliers through resampling methods, fully integrated with
  asymmetric multiplier estimation.
- Additional advanced capabilities include highly flexible formula
  parsing for mixed symmetric/asymmetric regressors, multiple model
  selection criteria (AIC, BIC, AICc, HQ), parallel processing support,
  and extensive post-estimation tools tailored for asymmetric analysis.

These features make `kardl` particularly suitable for researchers
needing fine-grained control over asymmetric dynamics and small-sample
inference.

## Estimating an asymmetric ARDL Model

This example estimates an asymmetric ARDL model to analyze the impact of
petrol prices and driving patterns on road fatalities in the UK, using
the built-in `Seatbelts` dataset with variables for DriversKilled,
PetrolPrice, drivers, kms, and a seatbelt law dummy variable.

### Step 1: Data Preparation

The `Seatbelts` dataset contains monthly data on road casualties in
Great Britain from 1969 to 1984. We convert it to a data frame for
analysis.

Note: The `Seatbelts` dataset is a built-in R dataset included in the
`datasets` package. The data can be accessed directly by converting the
time series object to a data frame.

### Step 2: Define the Model Formula

We define the model formula using R’s formula syntax, incorporating
asymmetric effects and deterministic variables. We use `asymmetric()`
for variables with both short- and long-run asymmetry, `lasymmetric()`
for long-run asymmetry, `sasymmetric()` for short-run asymmetry, and
`deterministic()` for fixed dummy variables. The `trend` term includes a
linear time trend in the model.

``` r

# Define the model formula
my_formula <- DriversKilled ~ PetrolPrice + drivers +
  asymmetric(PetrolPrice + drivers) + deterministic(law) + trend
```

Indeed, the formula syntax is flexible, allowing for various
combinations of asymmetric and deterministic variables. The following
variations of the formula are equivalent and will yield the same model
specification:

``` r

same_formula <- y ~ asymmetric(x1) +
  sasymmetric(x2 + x3) +
  lasymmetric(x4 + x5) +
  deterministic(dummy1) + trend
same_formula <- y ~ asymmetric(x1) +
  sasymmetric(x2 + x3) +
  lasymmetric(x4 + x5) +
  deterministic(dummy1) + trend
same_formula <- y ~ asym(x1) + sasym(x2 + x3) + lasym(x4 + x5) +
  det(dummy1) + trend
same_formula <- y ~ a(x1) + s(x2 + x3) + l(x4 + x5) + d(dummy1) + trend
```

### Step 3: Model Estimation

We estimate the ARDL model using different `mode` settings to
demonstrate flexibility in lag selection. The
[`kardl()`](https://karamelikli.github.io/kardl/reference/kardl.md)
function supports various modes: `"grid"`, `"grid_custom"`, `"quick"`,
or a user-defined lag vector.

#### Using `mode = "grid"`

The `"grid"` mode evaluates all lag combinations up to `maxlag` and
provides console feedback.

``` r

# Set model options
kardl_set(criterion = "BIC", different_asym_lag = TRUE, data = Seatbelts)
# Estimate model with grid mode
kardl_model <- kardl(
  data = Seatbelts, formula = my_formula,
  maxlag = 4, mode = "grid"
)
```

``` r

# View results
kardl_model
```

    ## Optimal lags for each variable ( BIC ):
    ## DriversKilled: 1, PetrolPrice_POS: 0, PetrolPrice_NEG: 0, drivers_POS: 0, drivers_NEG: 0 
    ## 
    ## Call:
    ## lm(formula = my_formula, data = model_data)
    ## 
    ## Coefficients:
    ##          (Intercept)      L1.DriversKilled    L1.PetrolPrice_POS  
    ##            134.84809              -1.11975             -58.85309  
    ##   L1.PetrolPrice_NEG        L1.drivers_POS        L1.drivers_NEG  
    ##            -43.01060               0.08228               0.08886  
    ##   L1.d.DriversKilled  L0.d.PetrolPrice_POS  L0.d.PetrolPrice_NEG  
    ##              0.13726            -285.05578            1028.03999  
    ##     L0.d.drivers_POS      L0.d.drivers_NEG                   law  
    ##              0.07562               0.07997              -0.61874  
    ##                trend  
    ##              0.63798

Summary of the model provides detailed information about the estimated
coefficients, standard errors, t-values, and significance levels.

``` r

# Display model summary
summary(kardl_model)
```

    ## 
    ## Call:
    ## lm(formula = my_formula, data = model_data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -27.425  -7.449  -1.070   7.966  34.134 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)           1.348e+02  1.151e+01  11.720  < 2e-16 ***
    ## L1.DriversKilled     -1.120e+00  8.866e-02 -12.630  < 2e-16 ***
    ## L1.PetrolPrice_POS   -5.885e+01  9.949e+01  -0.592  0.55489    
    ## L1.PetrolPrice_NEG   -4.301e+01  1.633e+02  -0.263  0.79252    
    ## L1.drivers_POS        8.228e-02  9.405e-03   8.748 1.66e-15 ***
    ## L1.drivers_NEG        8.886e-02  8.341e-03  10.653  < 2e-16 ***
    ## L1.d.DriversKilled    1.373e-01  4.699e-02   2.921  0.00394 ** 
    ## L0.d.PetrolPrice_POS -2.851e+02  3.313e+02  -0.860  0.39069    
    ## L0.d.PetrolPrice_NEG  1.028e+03  9.132e+02   1.126  0.26179    
    ## L0.d.drivers_POS      7.562e-02  8.643e-03   8.750 1.65e-15 ***
    ## L0.d.drivers_NEG      7.997e-02  7.494e-03  10.672  < 2e-16 ***
    ## law                  -6.187e-01  4.503e+00  -0.137  0.89086    
    ## trend                 6.380e-01  5.028e-01   1.269  0.20620    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 11.36 on 177 degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## Multiple R-squared:  0.7486, Adjusted R-squared:  0.7316 
    ## F-statistic: 43.93 on 12 and 177 DF,  p-value: < 2.2e-16

#### Using User-Defined Lags

Specify custom lags to bypass automatic lag selection:

``` r

kardl_model2 <- kardl(
  data = Seatbelts, my_formula,
  mode = c(2, 1, 1, 3, 0)
)
# View results
kardl_extract(kardl_model2, "opt_lag")
```

    ##   DriversKilled PetrolPrice_POS PetrolPrice_NEG     drivers_POS     drivers_NEG 
    ##               2               1               1               3               0

``` r

# Display model summary
summary(kardl_model2)
```

    ## 
    ## Call:
    ## lm(formula = my_formula, data = model_data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -26.5337  -7.5749  -0.5198   7.5158  31.0525 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)           1.297e+02  1.462e+01   8.873 1.03e-15 ***
    ## L1.DriversKilled     -1.077e+00  1.060e-01 -10.159  < 2e-16 ***
    ## L1.PetrolPrice_POS   -8.365e+01  1.167e+02  -0.717   0.4744    
    ## L1.PetrolPrice_NEG   -4.826e+01  1.801e+02  -0.268   0.7891    
    ## L1.drivers_POS        7.411e-02  1.255e-02   5.904 1.92e-08 ***
    ## L1.drivers_NEG        8.073e-02  1.019e-02   7.926 3.00e-13 ***
    ## L1.d.DriversKilled    1.281e-01  6.923e-02   1.851   0.0660 .  
    ## L2.d.DriversKilled    7.203e-02  5.195e-02   1.387   0.1674    
    ## L0.d.PetrolPrice_POS -3.189e+02  3.390e+02  -0.941   0.3482    
    ## L1.d.PetrolPrice_POS -5.298e+01  3.639e+02  -0.146   0.8844    
    ## L0.d.PetrolPrice_NEG  7.248e+02  9.573e+02   0.757   0.4500    
    ## L1.d.PetrolPrice_NEG  6.564e+01  8.720e+02   0.075   0.9401    
    ## L0.d.drivers_POS      7.499e-02  8.845e-03   8.478 1.12e-14 ***
    ## L1.d.drivers_POS      1.236e-02  1.272e-02   0.972   0.3327    
    ## L2.d.drivers_POS     -1.852e-02  1.111e-02  -1.668   0.0972 .  
    ## L3.d.drivers_POS      7.892e-03  9.504e-03   0.830   0.4075    
    ## L0.d.drivers_NEG      7.555e-02  7.797e-03   9.689  < 2e-16 ***
    ## law                  -1.353e+00  5.002e+00  -0.270   0.7871    
    ## trend                 6.507e-01  5.907e-01   1.101   0.2723    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 11.31 on 168 degrees of freedom
    ##   (5 observations deleted due to missingness)
    ## Multiple R-squared:  0.7601, Adjusted R-squared:  0.7344 
    ## F-statistic: 29.57 on 18 and 168 DF,  p-value: < 2.2e-16

#### Using All Variables

Use the `.` operator to include all variables except the dependent
variable:

``` r

kardl_set(data = Seatbelts)
kardl(formula = DriversKilled ~ . + deterministic(law), maxlag = 2)
```

    ## Optimal lags for each variable ( BIC ):
    ## DriversKilled: 1, drivers: 0, front: 0, rear: 0, kms: 0, PetrolPrice: 0, VanKilled: 0 
    ## 
    ## Call:
    ## lm(formula = my_formula, data = model_data)
    ## 
    ## Coefficients:
    ##        (Intercept)    L1.DriversKilled          L1.drivers            L1.front  
    ##         -5.480e+00          -1.107e+00           8.236e-02           3.755e-03  
    ##            L1.rear              L1.kms      L1.PetrolPrice        L1.VanKilled  
    ##         -5.576e-03           4.442e-04          -5.069e+01           1.232e-01  
    ## L1.d.DriversKilled        L0.d.drivers          L0.d.front           L0.d.rear  
    ##          1.380e-01           7.928e-02          -5.884e-04          -6.872e-03  
    ##           L0.d.kms    L0.d.PetrolPrice      L0.d.VanKilled                 law  
    ##         -7.470e-04          -3.693e+01          -1.215e-01           4.680e+00

#### Visualizing Lag Criteria

The `lag_criteria` component contains lag combinations and their
criterion values. We visualize these to compare model selection criteria
(AIC, BIC, HQ).

``` r

library(dplyr)
library(tidyr)
library(ggplot2)
# Convert lag_criteria to a data frame
lag_criteria <- as.data.frame(kardl_extract(kardl_model, "lag_criteria"))
lag_criteria <- lag_criteria |> mutate(across(c(AIC, BIC, HQ), as.numeric))

# Pivot to long format
lag_criteria_long <- lag_criteria |>
  select(-c(AICc, AdjR2)) |>
  pivot_longer(
    cols = c(AIC, BIC, HQ),
    names_to = "Criteria",
    values_to = "Value"
  )

# Find minimum values
min_values <- lag_criteria_long |>
  group_by(Criteria) |>
  slice_min(order_by = Value) |>
  ungroup()

# Plot
ggplot(
  lag_criteria_long,
  aes(x = lag, y = Value, color = Criteria, group = Criteria)
) +
  geom_line() +
  geom_point(
    data = min_values, aes(x = lag, y = Value),
    color = "red", size = 3, shape = 8
  ) +
  geom_text(
    data = min_values, aes(x = lag, y = Value, label = lag),
    vjust = 1.5, color = "black", size = 3.5
  ) +
  scale_x_discrete(
    breaks = lag_criteria$lag[seq(1, nrow(lag_criteria), by = 20)]
  ) +
  labs(
    title = "Lag Criteria Comparison",
    x = "Lag Configuration",
    y = "Criteria Value"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](intro_files/figure-html/lag-criteria-1.png)

#### Error Correction Model (ECM) Estimation

The [`ecm()`](https://karamelikli.github.io/kardl/reference/ecm.md)
function estimates a Restricted ECM for cointegration testing. We
specify the same formula and lag structure as in the ARDL model.

``` r

ecm_model <- ecm(
  data = Seatbelts, formula = my_formula,
  maxlag = 4, mode = "grid_custom"
)
# View results
summary(ecm_model)
```

    ## 
    ## Call:
    ## lm(formula = shortrun_eq, data = ecm_data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -28.232  -7.445  -0.876   7.570  34.774 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)           1.882e+00  2.152e+00   0.875  0.38295    
    ## EcmRes               -1.116e+00  8.791e-02 -12.691  < 2e-16 ***
    ## L1.d.DriversKilled    1.279e-01  4.407e-02   2.903  0.00415 ** 
    ## L0.d.PetrolPrice_POS -1.892e+02  3.158e+02  -0.599  0.54987    
    ## L0.d.PetrolPrice_NEG  8.670e+02  8.224e+02   1.054  0.29322    
    ## L0.d.drivers_POS      7.794e-02  8.380e-03   9.300  < 2e-16 ***
    ## L0.d.drivers_NEG      8.337e-02  6.251e-03  13.338  < 2e-16 ***
    ## law                   3.554e+00  3.218e+00   1.104  0.27088    
    ## trend                -7.841e-03  1.873e-02  -0.419  0.67599    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 11.3 on 181 degrees of freedom
    ## Multiple R-squared:  0.7455, Adjusted R-squared:  0.7342 
    ## F-statistic: 66.27 on 8 and 181 DF,  p-value: < 2.2e-16

### Step 4: Long-Run Coefficients

We calculate long-run coefficients using
[`kardl_longrun()`](https://karamelikli.github.io/kardl/reference/kardl_longrun.md),
which standardizes coefficients by dividing them by the negative of the
dependent variable’s long-run parameter.

``` r

# Long-run coefficients
my_long <- kardl_longrun(kardl_model)
my_long
```

    ## 
    ## Call:
    ## kardl_longrun.kardl_lm(kardl_model = kardl_model)
    ## 
    ## Coefficients:
    ## L1.PetrolPrice_POS  L1.PetrolPrice_NEG      L1.drivers_POS      L1.drivers_NEG  
    ##          -52.55915           -38.41091             0.07348             0.07935

The [`summary()`](https://rdrr.io/r/base/summary.html) function provides
detailed information about the long-run coefficients, including standard
errors, t-values, and significance levels.

``` r

# Summary of long-run coefficients
summary(my_long)
```

    ## 
    ## Call:
    ## kardl_longrun.kardl_lm(kardl_model = kardl_model)
    ## 
    ## Estimation type:
    ## Long-run multipliers 
    ## 
    ## Coefficients:
    ##                       Estimate  Std. Error t value Pr(>|t|)    
    ## L1.PetrolPrice_POS -52.5591494  88.7809404 -0.5920   0.5546    
    ## L1.PetrolPrice_NEG -38.4109100 145.6977929 -0.2636   0.7924    
    ## L1.drivers_POS       0.0734767   0.0056809 12.9340   <2e-16 ***
    ## L1.drivers_NEG       0.0793527   0.0041634 19.0594   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

### Step 5: Symmetry Test

The
[`symmetrytest()`](https://karamelikli.github.io/kardl/reference/symmetrytest.md)
function performs Wald tests to assess short- and long-run asymmetry in
the model.

``` r

ast <- Seatbelts |>
  kardl(
    DriversKilled ~ PetrolPrice + drivers + asymmetric(PetrolPrice + drivers) +
      deterministic(law) + trend,
    mode = c(1, 2, 3, 0, 1),
    data = _
  ) |>
  symmetrytest()
ast
```

    ## 
    ## KARDL Symmetry Test Results
    ## Symmetry Test Results - Long-run:
    ## =======================
    ##             Df Sum of Sq Mean Sq F value Pr(>F)
    ## PetrolPrice  1     2.663   2.663   0.020 0.8877
    ## drivers      1   175.149 175.149   1.316 0.2530
    ## 
    ## Symmetry Test Results - Short-run:
    ## =======================
    ##             Df Sum of Sq Mean Sq F value Pr(>F)
    ## PetrolPrice  1     2.649   2.649  0.0199 0.8880
    ## drivers      1    35.235  35.235  0.2647 0.6076

Summary of the symmetry test provides detailed results for both long-run
and short-run asymmetry tests, including F-values, p-values, hypotheses,
and test decisions.

``` r

# Summary of symmetry test
summary(ast)
```

    ## 
    ## Long-run symmetry tests
    ## -----------------------
    ##                   F  Pr(>F)
    ## PetrolPrice 0.02001 0.88768
    ## drivers     1.31595 0.25295
    ## 
    ## Hypotheses and decisions:
    ## 
    ## Variable: PetrolPrice 
    ## H0: - Coef(L1.PetrolPrice_POS)/Coef(L1.DriversKilled) = - Coef(L1.PetrolPrice_NEG)/Coef(L1.DriversKilled) 
    ## H1: At least one coefficient differs from zero. 
    ## Decision: Fail to Reject H0 at 5% level. Indicating long-run symmetry for variable PetrolPrice. 
    ## 
    ## Variable: drivers 
    ## H0: - Coef(L1.drivers_POS)/Coef(L1.DriversKilled) = - Coef(L1.drivers_NEG)/Coef(L1.DriversKilled) 
    ## H1: At least one coefficient differs from zero. 
    ## Decision: Fail to Reject H0 at 5% level. Indicating long-run symmetry for variable drivers. 
    ## 
    ## 
    ## Short-run symmetry tests
    ## ------------------------
    ##                   F  Pr(>F)
    ## PetrolPrice 0.01990 0.88797
    ## drivers     0.26473 0.60756
    ## 
    ## Hypotheses and decisions:
    ## 
    ## Variable: PetrolPrice 
    ## H0: Coef(L0.d.PetrolPrice_POS) + Coef(L1.d.PetrolPrice_POS) + Coef(L2.d.PetrolPrice_POS) = Coef(L0.d.PetrolPrice_NEG) + Coef(L1.d.PetrolPrice_NEG) + Coef(L2.d.PetrolPrice_NEG) + Coef(L3.d.PetrolPrice_NEG) 
    ## H1: Coef(L0.d.PetrolPrice_POS) + Coef(L1.d.PetrolPrice_POS) + Coef(L2.d.PetrolPrice_POS) ≠ Coef(L0.d.PetrolPrice_NEG) + Coef(L1.d.PetrolPrice_NEG) + Coef(L2.d.PetrolPrice_NEG) + Coef(L3.d.PetrolPrice_NEG) 
    ## Decision: Fail to Reject H0 at 5% level. Indicating short-run symmetry for variable PetrolPrice. 
    ## 
    ## Variable: drivers 
    ## H0: Coef(L0.d.drivers_POS) = Coef(L0.d.drivers_NEG) + Coef(L1.d.drivers_NEG) 
    ## H1: Coef(L0.d.drivers_POS) ≠ Coef(L0.d.drivers_NEG) + Coef(L1.d.drivers_NEG) 
    ## Decision: Fail to Reject H0 at 5% level. Indicating short-run symmetry for variable drivers.

### Step 6: Cointegration Tests

We perform cointegration tests to assess long-term relationships using
[`pssf()`](https://karamelikli.github.io/kardl/reference/pssf.md),
[`psst()`](https://karamelikli.github.io/kardl/reference/psst.md), and
[`narayan()`](https://karamelikli.github.io/kardl/reference/narayan.md).

#### PSS F Bound Test

The [`pssf()`](https://karamelikli.github.io/kardl/reference/pssf.md)
function tests for cointegration using the Pesaran, Shin, and Smith F
Bound test.

``` r

test_result <- kardl_model |> pssf(case = 3, signif_level = "0.05")
test_result
```

    ## 
    ##  Pesaran-Shin-Smith (PSS) Bounds F-test for cointegration
    ## 
    ## data:  kardl_model
    ## F = 32.345
    ## alternative hypothesis: Cointegrating relationship exists

Summary of the PSS F Bound test provides detailed information about the
test statistic, critical values, hypotheses, and decision regarding
cointegration.

``` r

summary(test_result)
```

    ## 
    ## ========================================
    ## KARDL Cointegration Test Results
    ## ========================================
    ## 
    ##  Decision: Reject H0 → Cointegration (at 5% level)
    ## 
    ##  Test Statistic:
    ##   F: 32.3445539
    ## 
    ##  Critical Values (Lower & Upper Bounds):
    ##           L    U
    ##   10%  3.03 4.06
    ##   5%   3.47 4.57
    ##   2.5% 3.89 5.07
    ##   1%   4.40 5.72
    ## 
    ## 
    ##  Comparison:
    ##   At the 5% significance level, F (32.3445539) exceeds the upper bound (4.57).
    ##   This indicates that the variables tend to move together over  time.
    ##   Conclusion: There is strong evidence of a long-run relationship  (cointegration).
    ## 
    ##  Hypotheses:
    ## H0: Coef(L1.DriversKilled) = Coef(L1.PetrolPrice_POS) = Coef(L1.PetrolPrice_NEG) = Coef(L1.drivers_POS) = Coef(L1.drivers_NEG) = 0 
    ## H1: Not all of Coef(L1.DriversKilled), Coef(L1.PetrolPrice_POS), Coef(L1.PetrolPrice_NEG), Coef(L1.drivers_POS), Coef(L1.drivers_NEG) are zero. 
    ## 
    ##  Model Details:
    ##   Number of regressors (k): 4
    ##   Case: V 
    ## 
    ## ========================================

#### PSS t Bound Test

The [`psst()`](https://karamelikli.github.io/kardl/reference/psst.md)
function tests the significance of the lagged dependent variable’s
coefficient.

``` r

test_result <- kardl_model |> psst(case = 3, signif_level = "0.05")
test_result
```

    ## 
    ##  Pesaran-Shin-Smith (PSS) Bounds t-test for cointegration
    ## 
    ## data:  model
    ## t = -12.63
    ## alternative hypothesis: Cointegrating relationship exists

Summary of the PSS t Bound test provides detailed information about the
test statistic, critical values, hypotheses, and decision regarding
cointegration.

``` r

summary(test_result)
```

    ## 
    ## ========================================
    ## KARDL Cointegration Test Results
    ## ========================================
    ## 
    ##  Decision: Reject H0 → Cointegration (at 5% level)
    ## 
    ##  Test Statistic:
    ##   t: -12.6296459
    ## 
    ##  Critical Values (Lower & Upper Bounds):
    ##            L     U
    ##   10%  -3.13 -4.04
    ##   5%   -3.41 -4.36
    ##   2.5% -3.65 -4.62
    ##   1%   -3.96 -4.96
    ## 
    ## 
    ##  Comparison:
    ##   At the 5% significance level, t (12.6296459) exceeds the upper bound (4.36).
    ##   This indicates that the variables tend to move together over  time.
    ##   Conclusion: There is strong evidence of a long-run relationship  (cointegration).
    ## 
    ##  Hypotheses:
    ## H0: Coef(L1.DriversKilled) = 0 
    ## H1: Coef(L1.DriversKilled) ≠ 0 
    ## 
    ##  Model Details:
    ##   Number of regressors (k): 4
    ##   Case: V 
    ## 
    ## ========================================

#### Narayan Test

The
[`narayan()`](https://karamelikli.github.io/kardl/reference/narayan.md)
function is tailored for small sample sizes. It tests for cointegration
using critical values optimized for small samples.

``` r

test_result <- kardl_model |> narayan(case = 3, signif_level = "0.05")
test_result
```

    ## 
    ##  Narayan F Test for Cointegration
    ## 
    ## data:  model
    ## F = 32.345
    ## alternative hypothesis: Cointegrating relationship exists

Summary of the Narayan test provides detailed information about the test
statistic, critical values, hypotheses, and decision regarding
cointegration.

``` r

summary(test_result)
```

    ## 
    ## ========================================
    ## KARDL Cointegration Test Results
    ## ========================================
    ## 
    ##  Decision: Reject H0 → Cointegration (at 5% level)
    ## 
    ##  Test Statistic:
    ##   F: 32.3445539
    ## 
    ##  Critical Values (Lower & Upper Bounds):
    ##           L     U
    ##   10% 3.160 4.230
    ##   5%  3.678 4.840
    ##   1%  4.890 6.164
    ## 
    ## 
    ##  Comparison:
    ##   At the 5% significance level, F (32.3445539) exceeds the upper bound (4.84).
    ##   This indicates that the variables tend to move together over  time.
    ##   Conclusion: There is strong evidence of a long-run relationship  (cointegration).
    ## 
    ##  Hypotheses:
    ## H0: Coef(L1.DriversKilled) = Coef(L1.PetrolPrice_POS) = Coef(L1.PetrolPrice_NEG) = Coef(L1.drivers_POS) = Coef(L1.drivers_NEG) = 0 
    ## H1: Not all of Coef(L1.DriversKilled), Coef(L1.PetrolPrice_POS), Coef(L1.PetrolPrice_NEG), Coef(L1.drivers_POS), Coef(L1.drivers_NEG) are zero. 
    ## 
    ##  Model Details:
    ##   Number of regressors (k): 4
    ##   Case: V 
    ## 
    ## 
    ##  Note:The number of observations exceeds the maximum limit for the critical valuestable. Using the critical values for 80 observations.
    ## ========================================

### Step 7: Dynamic Multipliers

The
[`mplier()`](https://karamelikli.github.io/kardl/reference/mplier.md)
function calculates dynamic multipliers for the model, showing how
changes in independent variables affect the dependent variable over
time.

``` r

multipliers <- kardl_model |> mplier()
# View multipliers of the model
head(kardl_extract(multipliers, "multipliers"))
```

    ##      h PetrolPrice_POS PetrolPrice_NEG PetrolPrice_dif drivers_POS drivers_NEG
    ## [1,] 0      -285.05578     -1028.03999    -1313.095765  0.07562484 -0.07997262
    ## [2,] 1       -63.84422        25.01033      -38.833893  0.08359961 -0.09025538
    ## [3,] 2       -20.84449       184.55623      163.711736  0.07335905 -0.07945846
    ## [4,] 3       -50.45487        42.80916       -7.645706  0.07208514 -0.07785801
    ## [5,] 4       -56.87543        18.42816      -38.447262  0.07346844 -0.07931196
    ## [6,] 5       -52.92356        37.45733      -15.466230  0.07366752 -0.07955710
    ##       drivers_dif
    ## [1,] -0.004347775
    ## [2,] -0.006655767
    ## [3,] -0.006099407
    ## [4,] -0.005772873
    ## [5,] -0.005843521
    ## [6,] -0.005889578

``` r

# View long-run multipliers
kardl_extract(multipliers, "omega")
```

    ## [1]  0.01750931 -0.13725897

``` r

# View short-run multipliers
head(kardl_extract(multipliers, "lambda"))
```

    ##      PetrolPrice_POS PetrolPrice_NEG drivers_POS drivers_NEG
    ## [1,]       -285.0558        1028.040 0.075624845  0.07997262
    ## [2,]        226.2027       -1071.051 0.006650624  0.00888249
    ## [3,]          0.0000           0.000 0.000000000  0.00000000
    ## [4,]          0.0000           0.000 0.000000000  0.00000000
    ## [5,]          0.0000           0.000 0.000000000  0.00000000
    ## [6,]          0.0000           0.000 0.000000000  0.00000000

Plotting dynamic multipliers for specific variables can be done using
the [`plot()`](https://rdrr.io/r/graphics/plot.default.html) function,
which visualizes the response of the dependent variable to changes in
independent variables over time.

``` r

plot(multipliers, variables = c("PetrolPrice", "drivers"))
```

![](intro_files/figure-html/plot-multipliers-1.png)

To handle a large number of variables, you can specify a subset of
variables to plot or use `variables = "all"` to visualize all dynamic
multipliers.

Bootstrap confidence intervals for dynamic multipliers can be calculated
using the
[`bootstrap()`](https://karamelikli.github.io/kardl/reference/bootstrap.md)
function, which provides robust estimates of uncertainty around the
multipliers.

``` r

bootstrap_results <- kardl_model |>
  bootstrap(horizon = 12, replications = 10)
# View bootstrap summary
summary(bootstrap_results)
```

    ## Summary of Dynamic Multipliers
    ## Horizon: 12 
    ## 
    ##        h      PetrolPrice_POS   PetrolPrice_NEG    PetrolPrice_dif   
    ##  Min.   : 0   Min.   :-285.06   Min.   :-1028.04   Min.   :-1313.10  
    ##  1st Qu.: 3   1st Qu.: -52.92   1st Qu.:   37.46   1st Qu.:  -15.47  
    ##  Median : 6   Median : -52.56   Median :   38.40   Median :  -14.16  
    ##  Mean   : 6   Mean   : -69.03   Mean   :  -34.49   Mean   : -103.52  
    ##  3rd Qu.: 9   3rd Qu.: -52.50   3rd Qu.:   38.59   3rd Qu.:  -13.91  
    ##  Max.   :12   Max.   : -20.84   Max.   :  184.56   Max.   :  163.71  
    ##   drivers_POS       drivers_NEG        drivers_dif        PetrolPrice_CI_upper
    ##  Min.   :0.07209   Min.   :-0.09026   Min.   :-0.006656   Min.   :167.7       
    ##  1st Qu.:0.07347   1st Qu.:-0.07946   1st Qu.:-0.005881   1st Qu.:224.4       
    ##  Median :0.07348   Median :-0.07935   Median :-0.005876   Median :225.6       
    ##  Mean   :0.07432   Mean   :-0.08014   Mean   :-0.005826   Mean   :286.2       
    ##  3rd Qu.:0.07348   3rd Qu.:-0.07935   3rd Qu.:-0.005874   3rd Qu.:233.8       
    ##  Max.   :0.08360   Max.   :-0.07786   Max.   :-0.004348   Max.   :638.4       
    ##  PetrolPrice_CI_lower drivers_CI_upper     drivers_CI_lower  
    ##  Min.   :-3769.4      Min.   :-0.0023941   Min.   :-0.02943  
    ##  1st Qu.: -299.5      1st Qu.:-0.0021219   1st Qu.:-0.01328  
    ##  Median : -298.7      Median :-0.0021181   Median :-0.01327  
    ##  Mean   : -561.1      Mean   :-0.0008551   Mean   :-0.01448  
    ##  3rd Qu.: -297.4      3rd Qu.:-0.0021174   3rd Qu.:-0.01323  
    ##  Max.   : -255.9      Max.   : 0.0146270   Max.   :-0.01113

Visualize bootstrap results for specific variables to understand the
variability and confidence intervals of the dynamic multipliers.

``` r

plot(bootstrap_results, variables = "drivers")
```

![](intro_files/figure-html/plot-bootstrap-multipliers-1.png)

### Step 8: Customizing asymmetric Variables

We demonstrate how to customize prefixes and suffixes for asymmetric
variables using
[`kardl_set()`](https://karamelikli.github.io/kardl/reference/kardl_set.md).

``` r

# Set custom prefixes and suffixes
kardl_reset()
kardl_set(asym_prefix = c("asyP_", "asyN_"), asym_suffix = c("_PP", "_NN"))
kardl_custom <- kardl(data = Seatbelts, my_formula)
kardl_custom
```

    ## Optimal lags for each variable ( AIC ):
    ## DriversKilled: 1, asyP_PetrolPrice_PP: 0, asyN_PetrolPrice_NN: 0, asyP_drivers_PP: 2, asyN_drivers_NN: 0 
    ## 
    ## Call:
    ## lm(formula = my_formula, data = model_data)
    ## 
    ## Coefficients:
    ##              (Intercept)          L1.DriversKilled    L1.asyP_PetrolPrice_PP  
    ##                123.16543                  -1.02076                 -64.65463  
    ##   L1.asyN_PetrolPrice_NN        L1.asyP_drivers_PP        L1.asyN_drivers_NN  
    ##                -67.68843                   0.07313                   0.07991  
    ##       L1.d.DriversKilled  L0.d.asyP_PetrolPrice_PP  L0.d.asyN_PetrolPrice_NN  
    ##                  0.07062                -314.39843                 805.57820  
    ##     L0.d.asyP_drivers_PP      L1.d.asyP_drivers_PP      L2.d.asyP_drivers_PP  
    ##                  0.07503                   0.01619                  -0.01497  
    ##     L0.d.asyN_drivers_NN                       law                     trend  
    ##                  0.07760                  -0.99053                   0.63124

## Key Functions and Parameters

- **`kardl(data, model, maxlag, mode, ...)`**:

  - `data`: A time series dataset (e.g., a data frame with
    DriversKilled, PetrolPrice, drivers).
  - `formula`: A formula specifying the long-run equation, e.g.,
    `y ~ x + z + asymmetric(z) + lasymmetric(x2 + x3) + sasymmetric(x3 + x4) + deterministic(dummy1 + dummy2) + trend`.
    Supports:
    - `asymmetric()`: asymmetric effects for both short- and long-run
      dynamics.
    - `lasymmetric()`: Long-run asymmetric variables.
    - `sasymmetric()`: Short-run asymmetric variables.
    - `deterministic()`: Fixed dummy variables.
    - `trend`: Linear time trend.
  - `maxlag`: Maximum number of lags (default: 4). Use smaller values
    (e.g., 2) for small datasets, larger values (e.g., 8) for long-term
    dependencies.
  - `mode`: Estimation mode:
    - `"quick"`: Verbose output for interactive use.
    - `"grid"`: Verbose output with lag optimization.
    - `"grid_custom"`: Silent, efficient execution.
    - User-defined vector (e.g., `c(1, 2, 4, 5)` or
      `c(DriversKilled = 2, PetrolPrice_POS = 3, PetrolPrice_NEG = 1, drivers = 3)`).
  - Returns a list with components: `inputs`, `finalModel`,
    `start_time`, `end_time`, `properLag`, `time_span`, `opt_lag`,
    `lag_criteria`, `type` (“kardlmodel”).

- **`kardl_set(...)`**: Configures options like `criterion` (AIC, BIC,
  AICc, HQ), `different_asym_lag`, `asym_prefix`, `Sasymuffix`,
  `short_coef`, and `long_coef`. Use
  [`kardl_get()`](https://karamelikli.github.io/kardl/reference/kardl_get.md)
  to retrieve settings and
  [`kardl_reset()`](https://karamelikli.github.io/kardl/reference/kardl_reset.md)
  to restore defaults.

- **`kardl_longrun(model)`**: Calculates standardized long-run
  coefficients, returning `type` (“kardl_longrun”), `coef`, `delta_se`,
  `results`, and `starsDesc`.

- **`symmetrytest(model)`**: Performs Wald tests for short- and long-run
  asymmetry, returning `Lhypotheses`, `Lwald`, `Shypotheses`, `Swald`,
  and `type` (“symmetrytest”).

- **`pssf(model, case, signif_level)`**: Performs the Pesaran, Shin, and
  Smith F Bound test for cointegration, supporting cases 1–5 and
  significance levels (“auto”, 0.01, 0.025, 0.05, 0.1, 0.10).

- **`psst(model, case, signif_level)`**: Performs the PSS t Bound test,
  focusing on the lagged dependent variable’s coefficient.

- **`narayan(model, case, signif_level)`**: Conducts the Narayan test
  for cointegration, optimized for small samples (cases 2–5).

- **`ecm(data, model, maxlag, mode, ...)`**: Conducts the Restricted ECM
  test for cointegration, with similar parameters to
  [`kardl()`](https://karamelikli.github.io/kardl/reference/kardl.md)
  and case/significance level options.

For detailed documentation, use
[`?kardl`](https://karamelikli.github.io/kardl/reference/kardl.md),
[`?kardl_set`](https://karamelikli.github.io/kardl/reference/kardl_set.md),
[`?kardl_longrun`](https://karamelikli.github.io/kardl/reference/kardl_longrun.md),
[`?symmetrytest`](https://karamelikli.github.io/kardl/reference/symmetrytest.md),
[`?pssf`](https://karamelikli.github.io/kardl/reference/pssf.md),
[`?psst`](https://karamelikli.github.io/kardl/reference/psst.md),
[`?narayan`](https://karamelikli.github.io/kardl/reference/narayan.md),
or [`?ecm`](https://karamelikli.github.io/kardl/reference/ecm.md).

## Conclusion

The `kardl` package is a versatile tool for econometric analysis,
offering robust support for symmetric and asymmetric ARDL/NARDL
modeling, cointegration tests, stability diagnostics, and
heteroskedasticity checks. Its flexible formula specification, lag
optimization, and support for parallel processing make it ideal for
studying complex economic relationships. For more information, visit
<https://github.com/karamelikli/kardl> or contact the authors at
<hakperest@gmail.com>.

------------------------------------------------------------------------

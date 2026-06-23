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

This example estimates an asymmetric ARDL model to analyze the dynamics
of exchange rate pass-through to domestic prices in Turkey, using a
sample dataset (`imf_example_data`) with variables for Consumer Price
Index (CPI), Exchange Rate (ER), Producer Price Index (PPI), and a
COVID-19 dummy variable.

### Step 1: Data Preparation

Assume `imf_example_data` contains monthly data for CPI, ER, PPI, and a
COVID dummy variable. We prepare the data by ensuring proper formatting
and adding the dummy variable. We retrieve data from the IMF’s
International Financial Statistics (IFS) dataset and prepare it for
analysis.

Note: The `imf_example_data` is a placeholder for demonstration
purposes. You should replace it with your actual dataset. The data can
be loaded by `readxl` or other data import functions.

### Step 2: Define the Model Formula

We define the model formula using R’s formula syntax, incorporating
asymmetric effects and deterministic variables. We use `asymmetric()`
for variables with both short- and long-run asymmetry, `lasymmetric()`
for long-run asymmetry, `sasymmetric()` for short-run asymmetry, and
`deterministic()` for fixed dummy variables. The `trend` term includes a
linear time trend in the model.

``` r

# Define the model formula
my_formula <- CPI ~ ER + PPI + asymmetric(ER + PPI) + deterministic(covid) +
  trend
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
kardl_set(criterion = "BIC", different_asym_lag = TRUE, data = imf_example_data)
# Estimate model with grid mode
kardl_model <- kardl(
  data = imf_example_data, formula = my_formula,
  maxlag = 4, mode = "grid"
)
```

``` r

# View results
kardl_model
```

    ## Optimal lags for each variable ( BIC ):
    ## CPI: 1, ER_POS: 1, ER_NEG: 0, PPI_POS: 3, PPI_NEG: 0 
    ## 
    ## Call:
    ## lm(formula = my_formula, data = model_data)
    ## 
    ## Coefficients:
    ##  (Intercept)        L1.CPI     L1.ER_POS     L1.ER_NEG    L1.PPI_POS  
    ##   -0.0662799    -0.0167504     0.0138795     0.0346587     0.0420485  
    ##   L1.PPI_NEG      L1.d.CPI   L0.d.ER_POS   L0.d.ER_NEG  L0.d.PPI_POS  
    ##    0.0396081     0.3969313     0.1242241    -0.0229369     0.0420020  
    ## L0.d.PPI_NEG         covid         trend  
    ##   -0.0039507     0.0036707    -0.0000213

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
    ##       Min        1Q    Median        3Q       Max 
    ## -0.044033 -0.009369 -0.001300  0.007786  0.113556 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  -0.0662799  0.0233710  -2.836 0.004772 ** 
    ## L1.CPI       -0.0167504  0.0048668  -3.442 0.000631 ***
    ## L1.ER_POS     0.0138795  0.0052938   2.622 0.009039 ** 
    ## L1.ER_NEG     0.0346587  0.0081800   4.237 2.74e-05 ***
    ## L1.PPI_POS    0.0420485  0.0096352   4.364 1.58e-05 ***
    ## L1.PPI_NEG    0.0396081  0.0110457   3.586 0.000372 ***
    ## L1.d.CPI      0.3969313  0.0399609   9.933  < 2e-16 ***
    ## L0.d.ER_POS   0.1242241  0.0185934   6.681 6.94e-11 ***
    ## L0.d.ER_NEG  -0.0229369  0.0495520  -0.463 0.643668    
    ## L0.d.PPI_POS  0.0420020  0.0166326   2.525 0.011899 *  
    ## L0.d.PPI_NEG -0.0039507  0.0139364  -0.283 0.776936    
    ## covid         0.0036707  0.0053298   0.689 0.491354    
    ## trend        -0.0000213  0.0002533  -0.084 0.933010    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.0156 on 455 degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## Multiple R-squared:  0.6099, Adjusted R-squared:  0.5996 
    ## F-statistic: 59.28 on 12 and 455 DF,  p-value: < 2.2e-16

#### Using User-Defined Lags

Specify custom lags to bypass automatic lag selection:

``` r

kardl_model2 <- kardl(
  data = imf_example_data, my_formula,
  mode = c(2, 1, 1, 3, 0)
)
# View results
kardl_extract(kardl_model2, "opt_lag")
```

    ##     CPI  ER_POS  ER_NEG PPI_POS PPI_NEG 
    ##       2       1       1       3       0

``` r

# Display model summary
summary(kardl_model2)
```

    ## 
    ## Call:
    ## lm(formula = my_formula, data = model_data)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.054516 -0.008329 -0.001178  0.006787  0.104278 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  -0.0382834  0.0226694  -1.689 0.091962 .  
    ## L1.CPI       -0.0123942  0.0047187  -2.627 0.008921 ** 
    ## L1.ER_POS     0.0110505  0.0051609   2.141 0.032798 *  
    ## L1.ER_NEG     0.0268623  0.0080054   3.356 0.000860 ***
    ## L1.PPI_POS    0.0532642  0.0096979   5.492 6.67e-08 ***
    ## L1.PPI_NEG    0.0452663  0.0107751   4.201 3.21e-05 ***
    ## L1.d.CPI      0.3750078  0.0444934   8.428 4.88e-16 ***
    ## L2.d.CPI     -0.0865443  0.0423947  -2.041 0.041800 *  
    ## L0.d.ER_POS   0.1119077  0.0179998   6.217 1.16e-09 ***
    ## L1.d.ER_POS   0.0889105  0.0189930   4.681 3.79e-06 ***
    ## L0.d.ER_NEG  -0.0051813  0.0476798  -0.109 0.913515    
    ## L1.d.ER_NEG   0.0047909  0.0475913   0.101 0.919860    
    ## L0.d.PPI_POS  0.0487238  0.0160447   3.037 0.002532 ** 
    ## L1.d.PPI_POS -0.0010288  0.0144643  -0.071 0.943329    
    ## L2.d.PPI_POS -0.0544024  0.0143719  -3.785 0.000174 ***
    ## L3.d.PPI_POS -0.0499891  0.0137240  -3.642 0.000302 ***
    ## L0.d.PPI_NEG  0.0056668  0.0134844   0.420 0.674505    
    ## covid         0.0031412  0.0050898   0.617 0.537448    
    ## trend        -0.0003309  0.0002472  -1.338 0.181531    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.01479 on 446 degrees of freedom
    ##   (5 observations deleted due to missingness)
    ## Multiple R-squared:  0.655,  Adjusted R-squared:  0.641 
    ## F-statistic: 47.03 on 18 and 446 DF,  p-value: < 2.2e-16

#### Using All Variables

Use the `.` operator to include all variables except the dependent
variable:

``` r

kardl_set(data = imf_example_data)
kardl(formula = CPI ~ . + deterministic(covid), mode = "grid")
```

    ## Optimal lags for each variable ( BIC ):
    ## CPI: 1, ER: 1, PPI: 1 
    ## 
    ## Call:
    ## lm(formula = my_formula, data = model_data)
    ## 
    ## Coefficients:
    ## (Intercept)       L1.CPI        L1.ER       L1.PPI     L1.d.CPI      L0.d.ER  
    ##    0.075004    -0.019754     0.020240     0.001600     0.489064     0.113932  
    ##    L0.d.PPI        covid  
    ##   -0.001353    -0.002127

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
colnames(lag_criteria) <- c("lag", "AIC", "BIC", "AICc", "HQ")
lag_criteria <- lag_criteria |> mutate(across(c(AIC, BIC, HQ), as.numeric))

# Pivot to long format
lag_criteria_long <- lag_criteria |>
  select(-AICc) |>
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
  data = imf_example_data, formula = my_formula,
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
    ##       Min        1Q    Median        3Q       Max 
    ## -0.069034 -0.008966 -0.000360  0.007383  0.098004 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   1.840e-02  2.767e-03   6.650 8.50e-11 ***
    ## EcmRes       -7.988e-03  4.168e-03  -1.916 0.055947 .  
    ## L1.d.CPI      4.566e-01  3.738e-02  12.217  < 2e-16 ***
    ## L0.d.ER_POS   1.237e-01  1.863e-02   6.643 8.86e-11 ***
    ## L1.d.ER_POS   9.888e-02  1.895e-02   5.218 2.76e-07 ***
    ## L0.d.ER_NEG   4.782e-02  4.861e-02   0.984 0.325705    
    ## L0.d.PPI_POS  1.974e-02  1.524e-02   1.296 0.195793    
    ## L1.d.PPI_POS  2.079e-02  1.461e-02   1.423 0.155302    
    ## L2.d.PPI_POS -3.840e-02  1.460e-02  -2.631 0.008815 ** 
    ## L3.d.PPI_POS -3.676e-02  1.403e-02  -2.620 0.009085 ** 
    ## L0.d.PPI_NEG -6.570e-04  1.387e-02  -0.047 0.962248    
    ## covid         1.182e-02  3.231e-03   3.657 0.000285 ***
    ## trend        -4.622e-05  7.953e-06  -5.812 1.17e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.0156 on 452 degrees of freedom
    ## Multiple R-squared:  0.611,  Adjusted R-squared:  0.6007 
    ## F-statistic: 59.16 on 12 and 452 DF,  p-value: < 2.2e-16

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
    ##  L1.ER_POS   L1.ER_NEG  L1.PPI_POS  L1.PPI_NEG  
    ##     0.8286      2.0691      2.5103      2.3646

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
    ##            Estimate Std. Error t value  Pr(>|t|)    
    ## L1.ER_POS   0.82861    0.15902  5.2107 2.858e-07 ***
    ## L1.ER_NEG   2.06913    0.41649  4.9680 9.593e-07 ***
    ## L1.PPI_POS  2.51030    0.85303  2.9428 0.0034187 ** 
    ## L1.PPI_NEG  2.36460    0.71337  3.3147 0.0009908 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

### Step 5: Symmetry Test

The
[`symmetrytest()`](https://karamelikli.github.io/kardl/reference/symmetrytest.md)
function performs Wald tests to assess short- and long-run asymmetry in
the model.

``` r

ast <- imf_example_data |>
  kardl(
    CPI ~ ER + PPI + asymmetric(ER + PPI) +
      deterministic(covid) + trend,
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
    ##     Df  Sum of Sq    Mean Sq F value Pr(>F)
    ## ER   1 0.00049756 0.00049756  2.1649 0.1419
    ## PPI  1 0.00016439 0.00016439  0.7153 0.3982
    ## 
    ## Symmetry Test Results - Short-run:
    ## =======================
    ##     Df  Sum of Sq    Mean Sq F value Pr(>F)
    ## ER   1 0.00038607 0.00038607  1.6798 0.1956
    ## PPI  1 0.00017426 0.00017426  0.7582 0.3844

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
    ##           F  Pr(>F)
    ## ER  2.16491 0.14190
    ## PPI 0.71527 0.39815
    ## 
    ## Hypotheses and decisions:
    ## 
    ## Variable: ER 
    ## H0: - Coef(L1.ER_POS)/Coef(L1.CPI) = - Coef(L1.ER_NEG)/Coef(L1.CPI) 
    ## H1: At least one coefficient differs from zero. 
    ## Decision: Fail to Reject H0 at 5% level. Indicating long-run symmetry for variable ER. 
    ## 
    ## Variable: PPI 
    ## H0: - Coef(L1.PPI_POS)/Coef(L1.CPI) = - Coef(L1.PPI_NEG)/Coef(L1.CPI) 
    ## H1: At least one coefficient differs from zero. 
    ## Decision: Fail to Reject H0 at 5% level. Indicating long-run symmetry for variable PPI. 
    ## 
    ## 
    ## Short-run symmetry tests
    ## ------------------------
    ##           F  Pr(>F)
    ## ER  1.67981 0.19562
    ## PPI 0.75820 0.38436
    ## 
    ## Hypotheses and decisions:
    ## 
    ## Variable: ER 
    ## H0: Coef(L0.d.ER_POS) + Coef(L1.d.ER_POS) + Coef(L2.d.ER_POS) = Coef(L0.d.ER_NEG) + Coef(L1.d.ER_NEG) + Coef(L2.d.ER_NEG) + Coef(L3.d.ER_NEG) 
    ## H1: Coef(L0.d.ER_POS) + Coef(L1.d.ER_POS) + Coef(L2.d.ER_POS) ≠ Coef(L0.d.ER_NEG) + Coef(L1.d.ER_NEG) + Coef(L2.d.ER_NEG) + Coef(L3.d.ER_NEG) 
    ## Decision: Fail to Reject H0 at 5% level. Indicating short-run symmetry for variable ER. 
    ## 
    ## Variable: PPI 
    ## H0: Coef(L0.d.PPI_POS) = Coef(L0.d.PPI_NEG) + Coef(L1.d.PPI_NEG) 
    ## H1: Coef(L0.d.PPI_POS) ≠ Coef(L0.d.PPI_NEG) + Coef(L1.d.PPI_NEG) 
    ## Decision: Fail to Reject H0 at 5% level. Indicating short-run symmetry for variable PPI.

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
    ## F = 10.792
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
    ##   F: 10.7918266
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
    ##   At the 5% significance level, F (10.7918266) exceeds the upper bound (4.57).
    ##   This indicates that the variables tend to move together over  time.
    ##   Conclusion: There is strong evidence of a long-run relationship  (cointegration).
    ## 
    ##  Hypotheses:
    ## H0: Coef(L1.CPI) = Coef(L1.ER_POS) = Coef(L1.ER_NEG) = Coef(L1.PPI_POS) = Coef(L1.PPI_NEG) = 0 
    ## H1: Not all of Coef(L1.CPI), Coef(L1.ER_POS), Coef(L1.ER_NEG), Coef(L1.PPI_POS), Coef(L1.PPI_NEG) are zero. 
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
    ## t = -3.4418
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
    ##  Decision: Inconclusive
    ## 
    ##  Test Statistic:
    ##   t: -3.4418095
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
    ##   At the 5% significance level, t (3.4418095) falls between the lower bound (3.41) and upper bound (4.36).
    ##   This is an inconclusive zone where we cannot make a definitive  judgment.
    ##   Conclusion: The test does not provide clear evidence either way.
    ## 
    ##  Hypotheses:
    ## H0: Coef(L1.CPI) = 0 
    ## H1: Coef(L1.CPI) ≠ 0 
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
    ## F = 10.792
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
    ##   F: 10.7918266
    ## 
    ##  Critical Values (Lower & Upper Bounds):
    ##           L     U
    ##   10% 3.160 4.230
    ##   5%  3.678 4.840
    ##   1%  4.890 6.164
    ## 
    ## 
    ##  Comparison:
    ##   At the 5% significance level, F (10.7918266) exceeds the upper bound (4.84).
    ##   This indicates that the variables tend to move together over  time.
    ##   Conclusion: There is strong evidence of a long-run relationship  (cointegration).
    ## 
    ##  Hypotheses:
    ## H0: Coef(L1.CPI) = Coef(L1.ER_POS) = Coef(L1.ER_NEG) = Coef(L1.PPI_POS) = Coef(L1.PPI_NEG) = 0 
    ## H1: Not all of Coef(L1.CPI), Coef(L1.ER_POS), Coef(L1.ER_NEG), Coef(L1.PPI_POS), Coef(L1.PPI_NEG) are zero. 
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

    ##      h    ER_POS       ER_NEG     ER_dif    PPI_POS      PPI_NEG     PPI_dif
    ## [1,] 0 0.1242241  0.022936894 0.14716099 0.04200204  0.003950724  0.04595277
    ## [2,] 1 0.1853312 -0.003001657 0.18232956 0.05797042 -0.034155367  0.02381505
    ## [3,] 2 0.2203617 -0.047905918 0.17245575 0.06333773 -0.088316829 -0.02497909
    ## [4,] 3 0.2444547 -0.099586097 0.14486860 0.06440726 -0.147943943 -0.08353668
    ## [5,] 4 0.2638028 -0.153090186 0.11071257 0.06375294 -0.208741767 -0.14498883
    ## [6,] 5 0.2809433 -0.206422027 0.07452128 0.06242533 -0.268985893 -0.20656056

``` r

# View long-run multipliers
kardl_extract(multipliers, "omega")
```

    ## [1]  1.3801808 -0.3969313

``` r

# View short-run multipliers
head(kardl_extract(multipliers, "lambda"))
```

    ##          ER_POS      ER_NEG     PPI_POS      PPI_NEG
    ## [1,]  0.1242241 -0.02293689  0.04200204 -0.003950724
    ## [2,] -0.1103446  0.05759561 -0.04200204  0.043558804
    ## [3,]  0.0000000  0.00000000  0.00000000  0.000000000
    ## [4,]  0.0000000  0.00000000  0.00000000  0.000000000
    ## [5,]  0.0000000  0.00000000  0.00000000  0.000000000
    ## [6,]  0.0000000  0.00000000  0.00000000  0.000000000

Plotting dynamic multipliers for specific variables can be done using
the [`plot()`](https://rdrr.io/r/graphics/plot.default.html) function,
which visualizes the response of the dependent variable to changes in
independent variables over time.

``` r

plot(multipliers, variables = c("ER", "PPI"))
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
    ##        h          ER_POS           ER_NEG             ER_dif        
    ##  Min.   : 0   Min.   :0.1242   Min.   :-0.54519   Min.   :-0.16380  
    ##  1st Qu.: 3   1st Qu.:0.2445   1st Qu.:-0.40804   1st Qu.:-0.06693  
    ##  Median : 6   Median :0.2969   Median :-0.25879   Median : 0.03813  
    ##  Mean   : 6   Mean   :0.2847   Mean   :-0.25573   Mean   : 0.02895  
    ##  3rd Qu.: 9   3rd Qu.:0.3411   3rd Qu.:-0.09959   3rd Qu.: 0.14487  
    ##  Max.   :12   Max.   :0.3814   Max.   : 0.02294   Max.   : 0.18233  
    ##     PPI_POS           PPI_NEG             PPI_dif          ER_CI_upper    
    ##  Min.   :0.04200   Min.   :-0.650249   Min.   :-0.59891   Min.   :0.1689  
    ##  1st Qu.:0.05437   1st Qu.:-0.495965   1st Qu.:-0.44002   1st Qu.:0.1745  
    ##  Median :0.05797   Median :-0.328001   Median :-0.26715   Median :0.2723  
    ##  Mean   :0.05739   Mean   :-0.322665   Mean   :-0.26528   Mean   :0.2682  
    ##  3rd Qu.:0.06243   3rd Qu.:-0.147944   3rd Qu.:-0.08354   3rd Qu.:0.3491  
    ##  Max.   :0.06441   Max.   : 0.003951   Max.   : 0.04595   Max.   :0.3977  
    ##   ER_CI_lower         PPI_CI_upper       PPI_CI_lower      
    ##  Min.   :-0.456249   Min.   :-0.49442   Min.   :-0.789906  
    ##  1st Qu.:-0.302609   1st Qu.:-0.36962   1st Qu.:-0.615389  
    ##  Median :-0.133211   Median :-0.23505   Median :-0.418502  
    ##  Mean   :-0.159183   Mean   :-0.21177   Mean   :-0.407860  
    ##  3rd Qu.: 0.003771   3rd Qu.:-0.04240   3rd Qu.:-0.191493  
    ##  Max.   : 0.049601   Max.   : 0.07719   Max.   :-0.006423

Visualize bootstrap results for specific variables to understand the
variability and confidence intervals of the dynamic multipliers.

``` r

plot(bootstrap_results, variables = "ER")
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
kardl_custom <- kardl(data = imf_example_data, my_formula)
kardl_custom
```

    ## Optimal lags for each variable ( AIC ):
    ## CPI: 2, asyP_ER_PP: 1, asyN_ER_NN: 0, asyP_PPI_PP: 4, asyN_PPI_NN: 0 
    ## 
    ## Call:
    ## lm(formula = my_formula, data = model_data)
    ## 
    ## Coefficients:
    ##      (Intercept)            L1.CPI     L1.asyP_ER_PP     L1.asyN_ER_NN  
    ##       -0.0404958        -0.0128695         0.0120635         0.0258122  
    ##   L1.asyP_PPI_PP    L1.asyN_PPI_NN          L1.d.CPI          L2.d.CPI  
    ##        0.0511977         0.0433195         0.3830822        -0.0920420  
    ##  L0.d.asyP_ER_PP   L1.d.asyP_ER_PP   L0.d.asyN_ER_NN  L0.d.asyP_PPI_PP  
    ##        0.1122301         0.0880910        -0.0017889         0.0509193  
    ## L1.d.asyP_PPI_PP  L2.d.asyP_PPI_PP  L3.d.asyP_PPI_PP  L4.d.asyP_PPI_PP  
    ##        0.0003349        -0.0535510        -0.0435529         0.0118365  
    ## L0.d.asyN_PPI_NN             covid             trend  
    ##       -0.0011170         0.0026926        -0.0003479

## Key Functions and Parameters

- **`kardl(data, model, maxlag, mode, ...)`**:

  - `data`: A time series dataset (e.g., a data frame with CPI, ER,
    PPI).
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
      `c(CPI = 2, ER_POS = 3, ER_NEG = 1, PPI = 3)`).
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

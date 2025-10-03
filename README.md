# KARDL <img src="man/figures/KARDL.png" align="right"/>

## Introduction

The `kardl` package is an R tool for estimating symmetric and asymmetric Autoregressive Distributed Lag (ARDL) and Nonlinear ARDL (NARDL) models, designed for econometricians and researchers analyzing cointegration and dynamic relationships in time series data. It offers flexible model specifications, allowing users to include deterministic variables, asymmetric effects for short- and long-run dynamics, and trend components. The package supports customizable lag structures, model selection criteria (AIC, BIC, AICc, HQ), and parallel processing for computational efficiency. Key features include:

- **Flexible Formula Specification**: Use `asym()`, `asymL()`, and `asymS()` to model asymmetric effects in short- and long-run dynamics, and `deterministic()` for dummy variables.
- **Lag Optimization**: Choose between automatic lag selection (`"quick"`, `"grid"`, `"grid_custom"`) or user-defined lags.
- **Dynamic Analysis**: Compute long-run coefficients, perform cointegration tests (PSS F, PSS t, Narayan, Banerjee, ECM), and test for heteroskedasticity (ARCH).


This vignette demonstrates how to use the `kardl()` function to estimate an asymmetric ARDL model, perform diagnostic tests, and visualize results, using economic data from Turkey.

## Installation


`kardl` in R can easily be installed from its CRAN repository:

``` r

install.packages("kardl")
library(kardl)

```

Alternatively, you can use the `devtools` package to load directly from GitHub:

``` r

# Install required packages
install.packages(c("stats", "msm", "lmtest", "nlWaldTest", "car", "strucchange", "utils"))
# Install kardl from GitHub
install.packages("devtools")
devtools::install_github("karamelikli/kardl")

```

Load the package:

``` r

library(kardl)

```

## Estimating an Asymmetric ARDL Model

This example estimates an asymmetric ARDL model to analyze the dynamics of exchange rate pass-through to domestic prices in Turkey, using a sample dataset (`imf_example_data`) with variables for Consumer Price Index (CPI), Exchange Rate (ER), Producer Price Index (PPI), and a COVID-19 dummy variable.

### Step 1: Data Preparation

Assume `imf_example_data` contains monthly data for CPI, ER, PPI, and a COVID dummy variable. We prepare the data by ensuring proper formatting and adding the dummy variable. We retrieve data from the IMF’s International Financial Statistics (IFS) dataset and prepare it for analysis.

Note: The `imf_example_data` is a placeholder for demonstration purposes. You should replace it with your actual dataset.
The data can be loaded by `readxl` or other data import functions.


### Step 2: Define the Model Formula

We define the model formula using R's formula syntax, incorporating asymmetric effects and deterministic variables. We use `asym()` for variables with both short- and long-run asymmetry, `deterministic()` for fixed effects, and `trend` for a linear time trend.

``` r

# Define the model formula
MyFormula <- CPI ~ ER + PPI + asym(ER + PPI) + deterministic(covid) + trend

```


### Step 3: Model Estimation

We estimate the ARDL model using different `mode` settings to demonstrate flexibility in lag selection. The `kardl()` function supports various modes: `"grid"`, `"grid_custom"`, `"quick"`, or a user-defined lag vector.

#### Using `mode = "grid"`
The `"grid"` mode evaluates all lag combinations up to `maxlag` and provides console feedback.

``` r

# Set model options
kardl_set(criterion = "BIC", differentAsymLag = TRUE, data=imf_example_data)
# Estimate model with grid mode
kardl_model <- kardl(data=imf_example_data,model= MyFormula, maxlag = 4, mode = "grid")
# View results
kardl_model
# Display model summary
summary(kardl_model)

```

#### Using User-Defined Lags

Specify custom lags to bypass automatic lag selection:

``` r

kardl_model2 <- kardl(data=imf_example_data, MyFormula, mode = c(2, 1, 1, 3, 0))
# View results
kardl_model2$properLag

```

#### Using All Variables
Use the `.` operator to include all variables except the dependent variable:

``` r

kardl_set(data=imf_example_data)
kardl(model =  CPI ~ . + deterministic(covid), mode = "grid")

```

#### Visualizing Lag Criteria

The `LagCriteria` component contains lag combinations and their criterion values. We visualize these to compare model selection criteria (AIC, BIC, HQ).

``` r

library(dplyr)
library(tidyr)
library(ggplot2)
# Convert LagCriteria to a data frame
LagCriteria <- as.data.frame(kardl_model[["LagCriteria"]])
colnames(LagCriteria) <- c("lag", "AIC", "BIC", "AICc", "HQ")
LagCriteria <- LagCriteria %>% mutate(across(c(AIC, BIC, HQ), as.numeric))

# Pivot to long format
LagCriteria_long <- LagCriteria %>%
  select(-AICc) %>%
  pivot_longer(cols = c(AIC, BIC, HQ), names_to = "Criteria", values_to = "Value")

# Find minimum values
min_values <- LagCriteria_long %>%
  group_by(Criteria) %>%
  slice_min(order_by = Value) %>%
  ungroup()

# Plot
ggplot(LagCriteria_long, aes(x = lag, y = Value, color = Criteria, group = Criteria)) +
  geom_line() +
  geom_point(data = min_values, aes(x = lag, y = Value), color = "red", size = 3, shape = 8) +
  geom_text(data = min_values, aes(x = lag, y = Value, label = lag), vjust = 1.5, color = "black", size = 3.5) +
  labs(title = "Lag Criteria Comparison", x = "Lag Configuration", y = "Criteria Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

```

### Step 4: Long-Run Coefficients

We calculate long-run coefficients using `kardl_longrun()`, which standardizes coefficients by dividing them by the negative of the dependent variable’s long-run parameter.

``` r

# Long-run coefficients
mylong <- kardl_longrun(kardl_model)
mylong

```



### Step 5: Asymmetry Test

The `asymmetrytest()` function performs Wald tests to assess short- and long-run asymmetry in the model.

``` r

ast <- imf_example_data %>% kardl(CPI ~ ER + PPI + asym(ER + PPI) + deterministic(covid) + trend, mode = c(1, 2, 3, 0, 1)) %>% asymmetrytest()
ast
# View long-run hypotheses
ast$Lhypotheses
# Detailed results
summary(ast)

```

### Step 6: Cointegration Tests

We perform cointegration tests to assess long-term relationships using `pssf()`, `psst()`, `narayan()`, `banerjee()`, and `recmt()`.

#### PSS F Bound Test

The `pssf()` function tests for cointegration using the Pesaran, Shin, and Smith F Bound test.

``` r

A <- kardl_model %>% pssf(case = 3, signif_level = "0.05")
cat(paste0("The F statistic = ", A$statistic, " where k = ", A$k, "."))
cat(paste0("\nWe found '", A$Cont, "' at ", A$siglvl, "."))
A$criticalValues
summary(A)

```

#### PSS t Bound Test

The `psst()` function tests the significance of the lagged dependent variable’s coefficient.

``` r

A <- kardl_model %>% psst(case = 3, signif_level = "0.05")
cat(paste0("The t statistic = ", A$statistic, " where k = ", A$k, "."))
cat(paste0("\nWe found '", A$Cont, "' at ", A$siglvl, "."))
A$criticalValues
summary(A)

```

#### Narayan Test

The `narayan()` function is tailored for small sample sizes.

``` r

A <- kardl_model %>% narayan(case = 3, signif_level = "0.05")
cat(paste0("The F statistic = ", A$statistic, " where k = ", A$k, "."))
cat(paste0("\nWe found '", A$Cont, "' at ", A$siglvl, "."))
A$criticalValues
summary(A)

```

#### Banerjee Test

The `banerjee()` function is designed for small datasets (≤100 observations).

``` r

A <- kardl_model %>% banerjee(signif_level = "0.05")
cat(paste0("The ECM parameter = ", A$coef, ", k = ", A$k, ", t statistic = ", A$statistic, "."))
cat(paste0("\nWe found '", A$Cont, "' at ", A$siglvl, "."))
A$criticalValues
summary(A)

```

#### Restricted ECM Test

The `recmt()` function tests for cointegration using an Error Correction Model.

``` r

recmt_model <- imf_example_data %>% recmt(MyFormula, mode = "grid_custom", case = 3)
recmt_model
# View results
summary(recmt_model)

```

### Step 7: ARCH Test

The `archtest()` function checks for autoregressive conditional heteroskedasticity in the model residuals.

``` r

arch_result <- archtest(kardl_model$finalModel$model$residuals, q = 2)
summary(arch_result)

```

### Step 8: Customizing Asymmetric Variables

We demonstrate how to customize prefixes and suffixes for asymmetric variables using `kardl_set()`.

``` r

# Set custom prefixes and suffixes
kardl_reset()
kardl_set(AsymPrefix = c("asyP_", "asyN_"), AsymSuffix = c("_PP", "_NN"))
kardl_custom <- kardl(data=imf_example_data, MyFormula, mode = "grid_custom")
kardl_custom$properLag

```

## Key Functions and Parameters

- **`kardl(data, model, maxlag, mode, ...)`**:
  - `data`: A time series dataset (e.g., a data frame with CPI, ER, PPI).
  - `model`: A formula specifying the long-run equation, e.g., `y ~ x + z + asym(z) + asymL(x2 + x3) + asymS(x3 + x4) + deterministic(dummy1 + dummy2) + trend`. Supports:
    - `asym()`: Asymmetric effects for both short- and long-run dynamics.
    - `asymL()`: Long-run asymmetric variables.
    - `asymS()`: Short-run asymmetric variables.
    - `deterministic()`: Fixed dummy variables.
    - `trend`: Linear time trend.
  - `maxlag`: Maximum number of lags (default: 4). Use smaller values (e.g., 2) for small datasets, larger values (e.g., 8) for long-term dependencies.
  - `mode`: Estimation mode:
    - `"quick"`: Verbose output for interactive use.
    - `"grid"`: Verbose output with lag optimization.
    - `"grid_custom"`: Silent, efficient execution.
    - User-defined vector (e.g., `c(1, 2, 4, 5)` or `c(CPI = 2, ER_POS = 3, ER_NEG = 1, PPI = 3)`).
  - Returns a list with components: `inputs`, `finalModel`, `start_time`, `end_time`, `properLag`, `TimeSpan`, `OptLag`, `LagCriteria`, `type` ("kardlmodel").

- **`kardl_set(...)`**: Configures options like `criterion` (AIC, BIC, AICc, HQ), `differentAsymLag`, `AsymPrefix`, `AsymSuffix`, `ShortCoef`, and `LongCoef`. Use `kardl_get()` to retrieve settings and `kardl_reset()` to restore defaults.

- **`kardl_longrun(model)`**: Calculates standardized long-run coefficients, returning `type` ("kardl_longrun"), `coef`, `delta_se`, `results`, and `starsDesc`.

- **`asymmetrytest(model)`**: Performs Wald tests for short- and long-run asymmetry, returning `Lhypotheses`, `Lwald`, `Shypotheses`, `Swald`, and `type` ("asymmetrytest").

- **`pssf(model, case, signif_level)`**: Performs the Pesaran, Shin, and Smith F Bound test for cointegration, supporting cases 1–5 and significance levels ("auto", 0.01, 0.025, 0.05, 0.1, 0.10).

- **`psst(model, case, signif_level)`**: Performs the PSS t Bound test, focusing on the lagged dependent variable’s coefficient.

- **`narayan(model, case, signif_level)`**: Conducts the Narayan test for cointegration, optimized for small samples (cases 2–5).

- **`banerjee(model, signif_level)`**: Performs the Banerjee cointegration test for small datasets (≤100 observations).

- **`recmt(data, model, maxlag, mode, case, signif_level, ...)`**: Conducts the Restricted ECM test for cointegration, with similar parameters to `kardl()` and case/significance level options.

- **`archtest(resid, q)`**: Tests for ARCH effects in model residuals, returning `type`, `statistic`, `parameter`, `p.value`, and `Fval`.

For detailed documentation, use `?kardl`, `?kardl_set`, `?kardl_longrun`, `?asymmetrytest`, `?pssf`, `?psst`, `?narayan`, `?banerjee`, `?recmt`, or `?archtest`.


# Options

The options for the KARDL package are set by the `kardl_set()` function in R.
The default values are set in the `kardl_set` list.
You can change the options by using the `kardl_set()` function with the desired parameters.
The following options are available:

| Option Name | Default | Description |
|------------------------|------------------------|------------------------|
| data | NULL | The data to be used for the model estimation |
| model | NULL | The formula to be used for the model estimation |
| maxlag | 4 | The maximum number of lags to be considered for the model estimation |
| mode | "quick" | The mode of the model estimation, can be "quick", "grid", "grid_custom" or a user-defined vector |
| criterion | "AIC" | The criterion for model selection, can be "AIC", "BIC", "HQ" or a user-defined function |
| differentAsymLag | FALSE | If TRUE, the asymmetry lags will be different for positive and negative shocks |
| AsymPrefix | c() | Prefix for asymmetry variables, default is empty |
| AsymSuffix | c("\_POS", "\_NEG") | Suffix for asymmetry variables, default is "\_POS" and "\_NEG" |
| LongCoef | "L{lag}.{varName}" | Prefix for long-run coefficients, default is "L1." |
| ShortCoef | "L{lag}.d.{varName}" | Prefix for short-run coefficients, default is "L1.d." |
| batch | "1/1" | Batch size for parallel processing, default is "1/1" |

The details of the options are as follows:

### 1. data

`data` is a data frame or time series object containing the variables to be used in the model estimation.
The default value is `NULL`, which means that the user must provide a dataset when calling the `kardl()` function.

####     Details

The `data` parameter is essential for the `kardl()` function to perform model estimation.
It should contain all the variables specified in the model formula, including the dependent variable and any independent variables, asymmetric components, and deterministic variables defined in the formula.
The trend will be generated automatically if specified in the formula.
Input data can be in the form of a data frame, tibble, or time series object (e.g., `ts`, `xts`, `zoo`).

When providing the `data`, ensure that: - The dataset is clean and free of missing values for the variables used in the model.
- The variables are appropriately formatted (e.g., numeric for continuous variables).
- The time series data is ordered correctly, especially if the analysis involves lagged variables.

### 2. model

`model` is a formula object specifying the model to be estimated.
The default value is `NULL`, which means that the user must provide a model formula when calling the `kardl()` function.

####      Details

The `model` parameter defines the structure of the ARDL or NARDL model to be estimated.
It should include the dependent variable on the left side of the formula and the independent variables, asymmetric components, deterministic variables, and trend (if applicable) on the right side.
The formula can include: - `asym()`: To specify variables with asymmetric effects in both short- and long -run dynamics.
- `asymL()`: To specify variables with asymmetric effects only in the long-run -dynamics.
- `asymS()`: To specify variables with asymmetric effects only in the short-run -dynamics.
- `deterministic()`: To include fixed dummy variables (e.g., seasonal d -ummies, event dummies).
- `trend`: To include a linear time trend in the model.
When constructing the `model` formula, ensure that: - All variables used in the formula are present in the `data` provided.
- The formula is syntactically correct and follows R's formula conventions.
- The use of asymmetric and deterministic functions is appropriate for the research question and data characteristics.

### 3. maxlag

`maxlag` is an integer value specifying the maximum number of lags to be considered for the model estimation.
The default value is `4`.

####      Details

The `maxlag` parameter sets the upper limit for the number of lags that the `kardl()` function will evaluate when optimizing the lag structure of the model.
This is particularly important when using modes like `"grid"` or `"grid_custom"`, where the function systematically tests different lag combinations up to the specified maximum.
When choosing a value for `maxlag`, consider the following: - **Data Frequency**: For monthly data, a `maxlag` of 4 is often sufficient to capture short-term dynamics.
For quarterly data, a lower `maxlag` ( e.g., 2) may be appropriate, while for daily data, a higher `maxlag` (e.g., 8 or more) might be necessary.
- **Sample Size**: A larger `maxlag` increases the number of parameters to estimate, which can be problematic with small sample sizes.
Ensure that the sample size is adequate to support the number of lags being considered.
- **Model Complexity**: Higher `maxlag` values lead to more complex models, which may overfit the data.
Balance the need for capturing dynamics with the risk of overfitting.
- **Computational Resources**: Evaluating a large number of lag combinations can be computationally intensive.
Consider the available resources and time constraints when setting `maxlag`.

### 4. mode

`mode` is a character string or numeric vector specifying the mode of the model estimation.
The default value is `"quick"`.
The available options are:\
- **"quick"**: This mode provides a fast estimation of the model without optimizing the lags.
It is suitable for initial explorations or when the user has a predefined lag structure.
- **"grid"**: This mode performs a grid search over all possible lag combinations up to the specified `maxlag`.
It provides verbose output, including the lag criteria for each combination, and is useful for thorough lag optimization.
- **"grid_custom"**: Similar to `"grid"`, but with silent execution.
It is more efficient for large datasets or when the user wants to avoid console output during the lag optimization process.
- **User-defined vector**: The user can specify a custom lag structure by providing a numeric vector (e.g., `c(1, 2, 4, 5)`) or a named vector (e.g., `c(CPI = 2, ER_POS = 3, ER_NEG = 1, PPI = 3)`).
This allows for complete control over the lag selection process.

####      Details

The `mode` parameter determines how the `kardl()` function approaches the estimation of the ARDL or NARDL model.
Each mode has its advantages and is suited to different scenarios: - Use `"quick"` for rapid assessments when the lag structure is already known or when computational speed is a priority.
- Use `"grid"` for comprehensive lag optimization, especially when the optimal lag structure is unknown.
This mode is ideal for exploratory analysis and model selection.
- Use `"grid_custom"` for efficient lag optimization without console output, particularly for large datasets or when running multiple models in batch mode.
- Use a user-defined vector when the user has specific knowledge about the appropriate lags for each variable, allowing for tailored model specifications.
When using `"grid"` or `"grid_custom"`, ensure that the `maxlag` parameter is set appropriately to balance the thoroughness of the search with computational feasibility.

### 5. criterion

`criterion` is a character string specifying the criterion to be used for selecting the optimal lags.
The default value is `"AIC"`.
The available options are:

-   **User defined function**: For detailed information on the model selection criteria used in the methods, see the documentation for the `modelCriterion` function.
-   **AIC**: Akaike Information Criterion (AIC). This criterion balances model fit and complexity, favoring models with a lower AIC value. It is widely used in time series analysis and model selection.
-   **BIC**: Schwarz Criterion (SC), also known as the Bayesian Information Criterion (BIC). This criterion imposes a stricter penalty for model complexity compared to AIC, often leading to simpler models when data size is large.
-   **AICc**: Corrected Akaike Information Criterion. This is a modification of AIC that accounts for small sample sizes. It is more reliable than AIC when the number of observations is limited.
-   **HQ**: Hannan-Quinn Criterion. This criterion is similar to AIC and BIC but uses a logarithmic penalty term that grows more slowly than BIC. It is often used in econometric applications.

####      Details

For detailed information on the model selection criteria used in the methods, see the documentation for the `modelCriterion` function.

The choice of the criterion can significantly impact the selected lag length and, consequently, the performance of the model.
Each criterion has its strengths and is suited to specific scenarios:

-   Use `"AIC"` for general purposes, especially when prioritizing a good fit over simplicity.
-   Use `"BIC"` when you prefer a more parsimonious model, particularly with large datasets.
-   Use `"AICc"` when working with small sample sizes to avoid overfitting.
-   Use `"HQ"` for a balance between AIC and BIC, often in econometrics or time series models.

Ensure that the selected criterion aligns with the goals of your analysis and the characteristics of your data.

####      Examples

#####      Default criterion (AIC)

``` r

kardl_set(criterion = "AIC")
kardl(data, MyFormula)
```

#####      Using BIC for lag selection

``` r

kardl_set(criterion = "BIC")
kardl(data, MyFormula)
```

#####      Using AICc for small sample sizes

``` r

kardl_set(criterion = "AICc")
data %>% kardl(MyFormula)
```

#####      Using HQ criterion

``` r

kardl_set(criterion = "HQ")
kardl(data, MyFormula)
```

### 6. differentAsymLag

`differentAsymLag` is a logical value (`TRUE` or `FALSE`) indicating whether positive and negative asymmetric variables should be assigned different lags during the estimation process.
The default value is `FALSE`, meaning that both positive and negative components will use the same lag.

####       Details

Asymmetric decomposition separates a variable into its positive and negative changes.
In some models, it may be desirable to assign different lags to these components to capture distinct dynamic behaviors.
Setting `differentAsymLag = TRUE` allows the function to optimize lags for positive and negative components independently.
When `differentAsymLag = FALSE`, both components will share the same lag.

This parameter is particularly useful when:

-   Positive and negative changes in a variable are expected to have different impacts over time.
-   The user wants more flexibility in modeling asymmetric responses.

**Attention!**

-   When `differentAsymLag = TRUE`, ensure that the model has sufficient data to estimate separate lags reliably.
-   For models with limited observations or a high number of variables, `differentAsymLag = FALSE` may be more robust and computationally efficient.

####      Examples

#####      Using the same lag for positive and negative components (default)

``` r

kadrl_set(differentAsymLag = FALSE)
kardl(data, MyFormula)
```

#####      Assigning different lags for positive and negative components

``` r

kardl_set(differentAsymLag = TRUE)
kardl(data, MyFormula)
```

### 7. AsymPrefix

`AsymPrefix` is a character vector specifying the prefixes used for naming asymmetric variables created during positive and negative decomposition.
The default value is an empty vector `c()`, indicating that no prefixes are added by default.

When specified, the prefixes are added to the beginning of variable names to represent the positive and negative decomposition:

-   The first element in the vector corresponds to the positive decomposition.
-   The second element in the vector corresponds to the negative decomposition.

####       Details


Asymmetric decomposition is used to analyze the separate effects of positive and negative changes in a variable.
For example, given a variable `X`, prefixes can be used to generate `POS_X` and `NEG_X` for the positive and negative components, respectively.

By default, no prefixes are applied (`AsymPrefix = c()`).
However, users can define custom prefixes by providing a vector with two elements.
For example:

-   `kardl_set(AsymPrefix = c("POS_", "NEG_"))` results in variable names such as `POS_X` and `NEG_X`.
-   `kardl_set(AsymPrefix = c("Increase_", "Decrease_"))` results in variable names such as `Increase_X` and `Decrease_X`.

**Attention!**

-   The vector must contain exactly two elements: the first for the positive decomposition and the second for the negative decomposition.
-   If prefixes are used in combination with suffixes (via `AsymSuffix`), ensure that the resulting variable names are meaningful and do not conflict.

###      Examples

####        Using default (no prefixes)

``` r

kardl_set( AsymPrefix = c())
```

####        Custom prefixes for positive and negative decomposition

``` r

kardl_set(AsymPrefix = c("POS_", "NEG_"))
```

####        Combining prefixes with suffixes

``` r

kardl_set( AsymPrefix = c("Change_", "Fall_"), AsymSuffix = c("_High", "_Low"))
```

### 8. AsymSuffix

`AsymSuffix` is a character vector specifying the suffixes used for naming asymmetric variables created during positive and negative decomposition.
The default value is `c("_POS", "_NEG")`, where:

-   `"_POS"` is the suffix appended to variables representing the positive decomposition.
-   `"_NEG"` is the suffix appended to variables representing the negative decomposition.

#####       Details

The order of the suffixes is important:

-   The first element in the vector corresponds to the positive decomposition.
-   The second element in the vector corresponds to the negative decomposition.

Asymmetric decomposition is commonly used in models to separate the effects of positive and negative changes in a variable.
For example, given a variable `X`, the decomposition may result in `X_POS` and `X_NEG` to represent its positive and negative components, respectively.

By default, the suffixes `"_POS"` and `"_NEG"` are used, but users can customize them as needed by providing a custom vector.
For example:

-   `AsymSuffix = c("_Increase", "_Decrease")` results in variable names such as `X_Increase` and `X_Decrease`.
-   `AsymSuffix = c("_Up", "_Down")` results in variable names such as `X_Up` and `X_Down`.

**Attention!**

-   Ensure that the custom suffix vector has exactly two elements:
    -   The first element must always represent the positive decomposition.
    -   The second element must always represent the negative decomposition.
-   Providing more or fewer elements or incorrect ordering may cause errors in variable naming.


### 9. LongCoef

`LongCoef` is a character string specifying the prefix format for naming long-run coefficients in the model output.
The default value is `"L{lag}.{varName}"`, where:

-   `{lag}` is a placeholder for the lag number.
-   `{varName}` is a placeholder for the variable name.

This format generates names like `L1.X` for the first lag of variable `X`.

####       Details

Long-run coefficients represent the long-term relationships between the dependent variable and independent variables in an ARDL or NARDL model.
The `LongCoef` parameter allows users to customize how these coefficients are named in the output, making it easier to identify and interpret them.
The default format `"L{lag}.{varName}"` is widely used and provides clear information about the lag and variable associated with each coefficient.
Users can modify the format by changing the `LongCoef` string.
For example:

-   `LongCoef = "LongRun_{varName}_Lag{lag}"` results in names like `LongRun_X_Lag1`.
-   `LongCoef = "LR_{varName}_L{lag}"` results in names like `LR_X_L1`.

**Attention!**

-   Ensure that the placeholders `{lag}` and `{varName}` are included in the custom format to maintain clarity in the coefficient names.
-   Avoid using special characters or spaces in the format that may cause issues in R variable naming conventions.

### 10. ShortCoef

`ShortCoef` is a character string specifying the prefix format for naming short-run coefficients in the model output.
The default value is `"L{lag}.d.{varName}"`, where:

-   `{lag}` is a placeholder for the lag number.
-   `{varName}` is a placeholder for the variable name.

This format generates names like `L1.d.X` for the first lag of the differenced variable `X`.

####       Details

Short-run coefficients capture the immediate effects of changes in independent variables on the dependent variable in an ARDL or NARDL model.
The `ShortCoef` parameter allows users to customize how these coefficients are named in the output, facilitating easier identification and interpretation.
The default format `"L{lag}.d.{varName}"` is commonly used and provides clear information about the lag, differencing, and variable associated with each coefficient.
Users can modify the format by changing the `ShortCoef` string.
For example:

-   `ShortCoef = "ShortRun_{varName}_Lag{lag}"` results in names like `ShortRun_X_Lag1`.
-   `ShortCoef = "SR_{varName}_L{lag}"` results in names like `SR_X_L1`.

**Attention!**

-   Ensure that the placeholders `{lag}` and `{varName}` are included in the custom format to maintain clarity in the coefficient names.
-   Avoid using special characters or spaces in the format that may cause issues in R variable naming conventions.



### 11. batch
`batch` is a character string specifying the batch size for parallel processing during model estimation.
The default value is `"1/1"`, indicating that the model estimation will be executed as a single job without batching.

####       Details

The `batch` parameter is particularly useful when dealing with large datasets or complex models that require significant computational resources.
By specifying a batch size, users can divide the model estimation process into smaller, more manageable segments, which can be processed in parallel.
The format for the `batch` parameter is `"m/n"`, where:

-   `m` is the number of batches to be processed in parallel.
-   `n` is the total number of batches.

For example, setting `batch = "2/4"` would divide the estimation into 4 batches, with 2 batches being processed simultaneously.
This can significantly reduce computation time, especially for models with extensive lag structures or large numbers of variables.

**Attention!**

-   Ensure that the specified batch size is appropriate for the available computational resources (e.g., CPU cores, memory).
-   Overloading the system with too many parallel processes may lead to performance degradation or failures.
-   The effectiveness of batching may vary depending on the specific model and dataset characteristics.

####      Examples

#####      Default batch size (no batching)

``` r
kardl_set(batch = "1/1")
kardl(data, MyFormula)
```

#####      Using a batch size of 2 out of 4

``` r
kardl_set(batch = "2/4")
kardl(data, MyFormula)
```

#####      Using a batch size of 3 out of 6
``` r
kardl_set(batch = "3/6")
kardl(data, MyFormula)
```






## Conclusion

The `kardl` package is a versatile tool for econometric analysis, offering robust support for symmetric and asymmetric ARDL/NARDL modeling, cointegration tests, stability diagnostics, and heteroskedasticity checks.
Its flexible formula specification, lag optimization, and support for parallel processing make it ideal for studying complex economic relationships.
For more information, visit <https://github.com/karamelikli/kardl> or contact the authors at [hakperest\@gmail.com](mailto:hakperest@gmail.com).

## Disclosures

-   The logo created by **canva.com** free account.

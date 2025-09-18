# KARDL <img src="man/figures/KARDL.png" align="right">

## Introduction

The `kardl` package is an R tool for estimating symmetric and asymmetric Autoregressive Distributed Lag (ARDL) and Nonlinear ARDL (NARDL) models, designed for econometricians and researchers analyzing cointegration and dynamic relationships in time series data. It offers flexible model specifications, allowing users to include deterministic variables, asymmetric effects for short- and long-run dynamics, and trend components. The package supports customizable lag structures, model selection criteria (AIC, BIC, AICc, HQ), and parallel processing for computational efficiency. Key features include:

- **Flexible Formula Specification**: Use `asym()`, `asymL()`, and `asymS()` to model asymmetric effects in short- and long-run dynamics, and `deterministic()` for dummy variables.
- **Lag Optimization**: Choose between automatic lag selection (`"quick"`, `"grid"`, `"grid_custom"`) or user-defined lags.
- **Dynamic Analysis**: Compute long-run coefficients, perform cointegration tests (PSS F, PSS t, Narayan, Banerjee, ECM), assess parameter stability (CUSUM, CUSUMQ), and test for heteroskedasticity (ARCH).
- **Visualization and Reporting**: Generate plots for lag criteria, and stability tests.

This vignette demonstrates how to use the `kardl()` function to estimate an asymmetric ARDL model, perform diagnostic tests, and visualize results, using economic data from Turkey.

## Installation


`kardl` in R can easily be installed from its CRAN repository:
```
install.packages("kardl")
library(kardl)
```

Alternatively, you can use the `devtools` package to load directly from GitHub:

```{r install, eval=FALSE}
# Install required packages
install.packages(c("stats", "msm", "lmtest", "nlWaldTest", "car", "strucchange", "utils"))
# Install kardl from GitHub
install.packages("devtools")
devtools::install_github("karamelikli/kardl")
```

Load the package:

```{r load}
library(kardl)
library(magrittr)
```

## Example: Estimating an Asymmetric ARDL Model

This example estimates an asymmetric ARDL model to analyze the dynamics of exchange rate pass-through to domestic prices in Turkey, using a sample dataset (`imf_example_data`) with variables for Consumer Price Index (CPI), Exchange Rate (ER), Producer Price Index (PPI), and a COVID-19 dummy variable.

### Step 1: Data Preparation

Assume `imf_example_data` contains monthly data for CPI, ER, PPI, and a COVID dummy variable. We prepare the data by ensuring proper formatting and adding the dummy variable. We retrieve data from the IMF’s International Financial Statistics (IFS) dataset and prepare it for analysis.

Note: The `imf_example_data` is a placeholder for demonstration purposes. You should replace it with your actual dataset.

```{r data-prep}
# Load required packages
library(ggplot2)
library(dplyr)
library(tidyr)

 trdata <- imf_example_data

# Define the model formula
MyFormula <- CPI ~ ER + PPI + asym(ER) + deterministic(covid) + trend
```

### Step 2: Model Estimation

We estimate the ARDL model using different `mode` settings to demonstrate flexibility in lag selection. The `kardl()` function supports various modes: `"grid"`, `"grid_custom"`, `"quick"`, or a user-defined lag vector.

#### Using `mode = "grid"`
The `"grid"` mode evaluates all lag combinations up to `maxlag` and provides console feedback.

```{r model-grid}
# Set model options
kardl_set(criterion = "BIC", DifferentAsymLag = TRUE)
# Estimate model with grid mode
kardl_model <- kardl(trdata, MyFormula, maxlag = 4, mode = "grid")
# View results
kardl_model
# Display model summary
summary(kardl_model)
```
#### Using User-Defined Lags
Specify custom lags to bypass automatic lag selection:

```{r model-user-defined}
kardl_model2 <- kardl(trdata, MyFormula, mode = c(2, 1, 1, 3))
kardl_model2$properLag
```

#### Using All Variables
Use the `.` operator to include all variables except the dependent variable:

```{r model-all-vars}
kardl(trdata, CPI ~ . + deterministic(covid), mode = "grid")
```

### Step 3: Visualizing Lag Criteria

The `LagCriteria` component contains lag combinations and their criterion values. We visualize these to compare model selection criteria (AIC, BIC, HQ).

```{r lag-criteria}
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

```{r long-run}
# Long-run coefficients
mylong <- kardl_longrun(kardl_model)
mylong
```

### Step 5: Parameter Stability Tests

We assess the stability of regression coefficients and variance using `cusum()` and `cusumq()`.

#### CUSUM Test
The `cusum()` function plots the Cumulative Sum of recursive residuals to check for parameter stability.

```{r cusum}
cDF <- cusum(kardl_model)
# View CUSUM data frame
head(cDF$dataframe)
# Custom plot with ggplot2
ggplot(cDF$dataframe, aes(x = X, y = Y)) +
  geom_line(color = "blue") +
  geom_line(aes(y = Lower), color = "red") +
  geom_line(aes(y = Upper), color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ggtitle("CUSUM Test") +
  xlab("") +
  ylab("") +
  ylim(range(cDF$dataframe$Lower, cDF$dataframe$Upper)) +
  scale_color_manual(values = c("blue", "red")) +
  guides(color = guide_legend(override.aes = list(linetype = c(1, 1)))) +
  theme(
    legend.position = "topleft",
    legend.title = element_blank(),
    plot.margin = margin(5, 4, 4, 1, "pt"),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )
```

#### CUSUM of Squares Test
The `cusumq()` function plots the Cumulative Sum of Squares to assess variance stability.

```{r cusumq}
cDFq <- cusumq(kardl_model)
# View CUSUMQ data frame
head(cDFq$dataframe)
# Custom plot with ggplot2
ggplot(cDFq$dataframe, aes(x = X, y = Y)) +
  geom_line(color = "blue") +
  geom_line(aes(y = Lower), color = "red") +
  geom_line(aes(y = Upper), color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ggtitle("CUSUM of Squares Test") +
  xlab("") +
  ylab("") +
  ylim(range(cDFq$dataframe$Lower, cDFq$dataframe$Upper)) +
  scale_color_manual(values = c("blue", "red")) +
  guides(color = guide_legend(override.aes = list(linetype = c(1, 1)))) +
  theme(
    legend.position = "topleft",
    legend.title = element_blank(),
    plot.margin = margin(5, 4, 4, 1, "pt"),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )
```

### Step 6: Asymmetry Test

The `asymmetrytest()` function performs Wald tests to assess short- and long-run asymmetry in the model.

```{r asymmetry-test}
ast <- trdata %>% kardl(CPI ~ ER + PPI + asym(ER + PPI) + deterministic(covid) + trend, mode = c(1, 2, 3, 0, 1)) %>% asymmetrytest()
ast
# View long-run hypotheses
ast$Lhypotheses
# Detailed results
summary(ast)
```

### Step 7: Cointegration Tests

We perform cointegration tests to assess long-term relationships using `pssf()`, `psst()`, `narayan()`, `banerjee()`, and `recmt()`.

#### PSS F Bound Test
The `pssf()` function tests for cointegration using the Pesaran, Shin, and Smith F Bound test.

```{r pssf}
A <- kardl_model %>% pssf(case = 3, signif_level = "0.05")
cat(paste0("The F statistic = ", A$statistic, " where k = ", A$k, "."))
cat(paste0("\nWe found '", A$Cont, "' at ", A$siglvl, "."))
A$criticalValues
summary(A)
```

#### PSS t Bound Test
The `psst()` function tests the significance of the lagged dependent variable’s coefficient.

```{r psst}
A <- kardl_model %>% psst(case = 3, signif_level = "0.05")
cat(paste0("The t statistic = ", A$statistic, " where k = ", A$k, "."))
cat(paste0("\nWe found '", A$Cont, "' at ", A$siglvl, "."))
A$criticalValues
summary(A)
```

#### Narayan Test
The `narayan()` function is tailored for small sample sizes.

```{r narayan}
A <- kardl_model %>% narayan(case = 3, signif_level = "0.05")
cat(paste0("The F statistic = ", A$statistic, " where k = ", A$k, "."))
cat(paste0("\nWe found '", A$Cont, "' at ", A$siglvl, "."))
A$criticalValues
summary(A)
```

#### Banerjee Test
The `banerjee()` function is designed for small datasets (≤100 observations).

```{r banerjee}
A <- kardl_model %>% banerjee(signif_level = "0.05")
cat(paste0("The ECM parameter = ", A$coef, ", k = ", A$k, ", t statistic = ", A$statistic, "."))
cat(paste0("\nWe found '", A$Cont, "' at ", A$siglvl, "."))
A$criticalValues
summary(A)
```

#### Restricted ECM Test

The `recmt()` function tests for cointegration using an Error Correction Model.

```{r recmt}
recmt_model <- trdata %>% recmt(MyFormula, mode = "grid_custom", case = 3)
recmt_model
# View results
summary(recmt_model)
```

### Step 8: ARCH Test

The `archtest()` function checks for autoregressive conditional heteroskedasticity in the model residuals.

```{r arch-test}
arch_result <- archtest(kardl_model$finalModel$model$residuals, q = 2)
summary(arch_result)
```

### Step 9: Customizing Asymmetric Variables

We demonstrate how to customize prefixes and suffixes for asymmetric variables using `kardl_set()`.

```{r asym-custom}
# Set custom prefixes and suffixes
kardl_reset()
kardl_set(AsymPrefix = c("asyP_", "asyN_"), AsymSuffix = c("_PP", "_NN"))
kardl_custom <- kardl(trdata, MyFormula, mode = "grid_custom")
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

- **`kardl_set(...)`**: Configures options like `criterion` (AIC, BIC, AICc, HQ), `DifferentAsymLag`, `AsymPrefix`, `AsymSuffix`, `ShortCoef`, and `LongCoef`. Use `kardl_get()` to retrieve settings and `kardl_reset()` to restore defaults.

- **`kardl_longrun(model)`**: Calculates standardized long-run coefficients, returning `type` ("kardl_longrun"), `coef`, `delta_se`, `results`, and `starsDesc`.

- **`cusum(inputs_, saveToFile)`**: Plots the Cumulative Sum test for parameter stability, returning a list with a data frame for plotting.

- **`cusumq(inputs_, saveToFile)`**: Plots the Cumulative Sum of Squares test for variance stability, returning a similar list.

- **`asymmetrytest(model)`**: Performs Wald tests for short- and long-run asymmetry, returning `Lhypotheses`, `Lwald`, `Shypotheses`, `Swald`, and `type` ("asymmetrytest").

- **`pssf(model, case, signif_level)`**: Performs the Pesaran, Shin, and Smith F Bound test for cointegration, supporting cases 1–5 and significance levels ("auto", 0.01, 0.025, 0.05, 0.1, 0.10).

- **`psst(model, case, signif_level)`**: Performs the PSS t Bound test, focusing on the lagged dependent variable’s coefficient.

- **`narayan(model, case, signif_level)`**: Conducts the Narayan test for cointegration, optimized for small samples (cases 2–5).

- **`banerjee(model, signif_level)`**: Performs the Banerjee cointegration test for small datasets (≤100 observations).

- **`recmt(data, model, maxlag, mode, case, signif_level, ...)`**: Conducts the Restricted ECM test for cointegration, with similar parameters to `kardl()` and case/significance level options.

- **`archtest(resid, q)`**: Tests for ARCH effects in model residuals, returning `type`, `statistic`, `parameter`, `p.value`, and `Fval`.

For detailed documentation, use `?kardl`, `?kardl_set`, `?kardl_longrun`, `?cusum`, `?cusumq`, `?asymmetrytest`, `?pssf`, `?psst`, `?narayan`, `?banerjee`, `?recmt`, or `?archtest`.


# Options

The options for the KARDL package are set by the `kardl_set()` function in R. The default values are set in the `kardl_set` list. You can change the options by using the `kardl_set()` function with the desired parameters.
The following options are available:

| Option Name | Default | Description |
|-------------|---------|-------------|
| criterion | "AIC" | The criterion for model selection, can be "AIC", "BIC", "HQ" or a user-defined function |
| DifferentAsymLag | FALSE | If TRUE, the asymmetry lags will be different for positive and negative shocks |
| AsymPrefix | character() | Prefix for asymmetry variables, default is empty |
| AsymSuffix | c("_POS", "_NEG") | Suffix for asymmetry variables, default is "_POS" and "_NEG" |
| LongCoef | "L{lag}.{varName}" | Prefix for long-run coefficients, default is "L1." |
| ShortCoef | "L{lag}.d.{varName}" | Prefix for short-run coefficients, default is "L1.d." |
|  batch      | "1/1" | Batch size for parallel processing, default is "1/1" |

The details of the options are as follows:


### criterion

`criterion` is a character string specifying the criterion to be used for selecting the optimal lags. The default value is `"AIC"`. The available options are:

- **User defined function**: For detailed information on the model selection criteria used in the methods, see the documentation for the `modelCriterion` function.
- **AIC**: Akaike Information Criterion (AIC). This criterion balances model fit and complexity, favoring models with a lower AIC value. It is widely used in time series analysis and model selection.
- **BIC**: Schwarz Criterion (SC), also known as the Bayesian Information Criterion (BIC). This criterion imposes a stricter penalty for model complexity compared to AIC, often leading to simpler models when data size is large.
- **AICc**: Corrected Akaike Information Criterion. This is a modification of AIC that accounts for small sample sizes. It is more reliable than AIC when the number of observations is limited.
- **HQ**: Hannan-Quinn Criterion. This criterion is similar to AIC and BIC but uses a logarithmic penalty term that grows more slowly than BIC. It is often used in econometric applications.

#### Details

For detailed information on the model selection criteria used in the methods, see the documentation for the `modelCriterion` function.

The choice of the criterion can significantly impact the selected lag length and, consequently, the performance of the model. Each criterion has its strengths and is suited to specific scenarios:

- Use `"AIC"` for general purposes, especially when prioritizing a good fit over simplicity.
- Use `"BIC"` when you prefer a more parsimonious model, particularly with large datasets.
- Use `"AICc"` when working with small sample sizes to avoid overfitting.
- Use `"HQ"` for a balance between AIC and BIC, often in econometrics or time series models.

Ensure that the selected criterion aligns with the goals of your analysis and the characteristics of your data.

#### Examples

##### Default criterion (AIC)
```R
kardl_set(criterion = "AIC")
kardl(data, MyFormula)
```

##### Using BIC for lag selection
```R
kardl_set(criterion = "BIC")
kardl(data, MyFormula)
```

##### Using AICc for small sample sizes
```R
kardl_set(criterion = "AICc")
data %>% kardl(MyFormula)
```

##### Using HQ criterion
```R
kardl_set(criterion = "HQ")
kardl(data, MyFormula)
```

### DifferentAsymLag

`DifferentAsymLag` is a logical value (`TRUE` or `FALSE`) indicating whether positive and negative asymmetric variables should be assigned different lags during the estimation process. The default value is `FALSE`, meaning that both positive and negative components will use the same lag.

#### Details

Asymmetric decomposition separates a variable into its positive and negative changes. In some models, it may be desirable to assign different lags to these components to capture distinct dynamic behaviors. Setting `DifferentAsymLag = TRUE` allows the function to optimize lags for positive and negative components independently. When `DifferentAsymLag = FALSE`, both components will share the same lag.

This parameter is particularly useful when:

- Positive and negative changes in a variable are expected to have different impacts over time.
- The user wants more flexibility in modeling asymmetric responses.

**Attention!**

- When `DifferentAsymLag = TRUE`, ensure that the model has sufficient data to estimate separate lags reliably.
- For models with limited observations or a high number of variables, `DifferentAsymLag = FALSE` may be more robust and computationally efficient.

#### Examples

##### Using the same lag for positive and negative components (default)
```R
kadrl_set(DifferentAsymLag = FALSE)
kardl(data, MyFormula)
```

##### Assigning different lags for positive and negative components
```R
kardl_set(DifferentAsymLag = TRUE)
kardl(data, MyFormula)

```





### AsymPrefix

`AsymPrefix` is a character vector specifying the prefixes used for naming asymmetric variables created during positive and negative decomposition. The default value is an empty vector `c()`, indicating that no prefixes are added by default.

When specified, the prefixes are added to the beginning of variable names to represent the positive and negative decomposition:

- The first element in the vector corresponds to the positive decomposition.
- The second element in the vector corresponds to the negative decomposition.

Asymmetric decomposition is used to analyze the separate effects of positive and negative changes in a variable. For example, given a variable `X`, prefixes can be used to generate `POS_X` and `NEG_X` for the positive and negative components, respectively.

By default, no prefixes are applied (`AsymPrefix = c()`). However, users can define custom prefixes by providing a vector with two elements. For example:

- `kardl_set(AsymPrefix = c("POS_", "NEG_"))` results in variable names such as `POS_X` and `NEG_X`.
- `kardl_set(AsymPrefix = c("Increase_", "Decrease_"))` results in variable names such as `Increase_X` and `Decrease_X`.

**Attention!**

- The vector must contain exactly two elements: the first for the positive decomposition and the second for the negative decomposition.
- If prefixes are used in combination with suffixes (via `AsymSuffix`), ensure that the resulting variable names are meaningful and do not conflict.

### Examples

#### Using default (no prefixes)
```R
kardl_set( AsymPrefix = c())
```

#### Custom prefixes for positive and negative decomposition
```R
kardl_set(AsymPrefix = c("POS_", "NEG_"))
```

#### Combining prefixes with suffixes
```R
kardl_set( AsymPrefix = c("Change_", "Fall_"), AsymSuffix = c("_High", "_Low"))
```

### AsymSuffix

`AsymSuffix` is a character vector specifying the suffixes used for naming asymmetric variables created during positive and negative decomposition. The default value is `c("_POS", "_NEG")`, where:

- `"_POS"` is the suffix appended to variables representing the positive decomposition.
- `"_NEG"` is the suffix appended to variables representing the negative decomposition.

The order of the suffixes is important:

- The first element in the vector corresponds to the positive decomposition.
- The second element in the vector corresponds to the negative decomposition.

Asymmetric decomposition is commonly used in models to separate the effects of positive and negative changes in a variable. For example, given a variable `X`, the decomposition may result in `X_POS` and `X_NEG` to represent its positive and negative components, respectively.

By default, the suffixes `"_POS"` and `"_NEG"` are used, but users can customize them as needed by providing a custom vector. For example:

- `AsymSuffix = c("_Increase", "_Decrease")` results in variable names such as `X_Increase` and `X_Decrease`.
- `AsymSuffix = c("_Up", "_Down")` results in variable names such as `X_Up` and `X_Down`.

**Attention!**

- Ensure that the custom suffix vector has exactly two elements:
  - The first element must always represent the positive decomposition.
  - The second element must always represent the negative decomposition.
- Providing more or fewer elements or incorrect ordering may cause errors in variable naming.


### batch 

Parallel processing batch size for model estimation. This method can be ısed for grid and grid_custom functions, where the model estimation is divided into smaller parts to manage resource allocation and parallel execution. The Slurm workload Manager is used to manage the parallel processing of the model estimation. 

This option allows you to specify how the model estimation process should be divided into batches. It can be set as a single integer or a fraction in the format `current_batch/total_batches`.

A numeric value or fraction indicating the current batch and total number of batches for dividing the model estimation process into smaller, manageable parts.

For example:

- If specified as a single integer (e.g., `4`), it indicates the total number of batches into which the model estimation process will be divided. In this case, the parameter assumes sequential execution from batch 1 to batch 4.
- If specified as a fraction (e.g., `3/15`), it represents the current batch (`3`) and the total number of batches (`15`). This is useful when distributing tasks across multiple jobs or machines to parallelize the process.

The default value is `1/1`, which indicates that the model estimation will not be divided into batches and will be executed as a single job.
        
Dividing the model estimation process into batches can be particularly useful when dealing with a large dataset or when multiple parameter combinations need to be estimated. By specifying the `batch` parameter, the user can:

- Manage resource allocation better by processing smaller portions of data at a time.
- Distribute the estimation workload across multiple machines or sessions for parallel execution.

For example:

- If `batch = 1/4`, it means the user is running the first out of four total batches.
- If `batch = 1/40`, it indicates that the model will be divided into 40 sequential batches.

Ensure that the logic for handling batches within the function aligns with the batch specification, such as using `floor()` to calculate the range of lags or data subsets corresponding to each batch.
To apply the options, you can use the following code:

kardl_set(batch = "1/2")

If BatchTotal is numeric, then this option means the ith batch. The default value is "1/1".

## Conclusion

The `kardl` package is a versatile tool for econometric analysis, offering robust support for symmetric and asymmetric ARDL/NARDL modeling, cointegration tests, stability diagnostics, and heteroskedasticity checks. Its flexible formula specification, lag optimization, and support for parallel processing make it ideal for studying complex economic relationships. For more information, visit [https://github.com/karamelikli/kardl](https://github.com/karamelikli/kardl) or contact the authors at [hakperest@gmail.com](mailto:hakperest@gmail.com).


## Disclosures

* The logo created by **canva.com** free account.

# KARDL <img src="man/figures/KARDL.png"  align="right" >

KARDL package performs cointegration tests for linear and nonlinear models.

## Usage
```r 
library(ggplot2)
install.packages("imf.data")
library("imf.data")
IFS <- load_datasets("IFS")
# PCPI_IX             Prices, Consumer Price Index, All items, Index
# AIP_IX              Economic Activity, Industrial Production, Index
# ENDE_XDC_USD_RATE   Exchange Rates, Domestic Currency per U.S. Dollar, End of Period, Rate
trdata<-IFS$get_series(freq = "M", ref_area = "TR", indicator = c("PCPI_IX","AIP_IX","ENDE_XDC_USD_RATE"),start_period = "1985-01",end_period = "2024-02")
PeriodRow<-trdata[,1]
trdata[,1]<-NULL
colnames(trdata)<-c("ER","CPI","PPI")
trdata<-log(as.data.frame(lapply(trdata, function(x) as.numeric(x))))
rownames(trdata)<-PeriodRow

# Inserting covid dummy variable
trdata<-cbind(trdata,covid=0)
trdata[420:470,4]<-1
inputs_ <-KARDL(CPI~ER+PPI+asym(ER)+deterministic(covid)+trend,trdata,userdefined=c(1,2,2,0))

m<-mplier(inputs_,40)
mydata<-as.data.frame( m$mpsi)
ggplot(mydata, aes(x = 1:nrow(mydata))) +
  geom_line(aes(y = ER_POS), color = "blue", linetype = "solid") +
  geom_line(aes(y = ER_NEG), color = "red", linetype = "dashed") +
  geom_line(aes(y = ER_dif), color = "black", linetype = "dotted") +
  labs(x = "Time", y = "Value", title = "Dynamic multipliers with ggplot2") +
  theme_minimal()

```


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


## Disclosures

* The logo created by [**canva.com**](https://www.canva.com) free account

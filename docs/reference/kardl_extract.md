# Extract Components from kardl Objects

`kardl_extract()` is a generic accessor for retrieving selected
documented components from objects produced by the **kardl** package.

## Usage

``` r
kardl_extract(kardl_object, what, variable = NULL, component = NULL)
```

## Arguments

- kardl_object:

  A supported object produced by the **kardl** package.

- what:

  A character string specifying the component to extract. The available
  options depend on the class of `object` and are documented in the
  sections below.

- variable:

  An optional character string specifying a particular variable to
  extract results for, when applicable. The available options depend on
  the selected `what` value and are documented in the relevant sections
  below. This argument is available just for `kardl_symmetric` objects,
  where users can extract results for specific variables included in the
  symmetry test. For example, if a symmetry test includes variables "ER"
  and "PPI", users can specify `variable = "ER"` to extract results
  related to the "ER" variable, or `variable = c("ER", "PPI")` to
  extract results for both variables. If `variable` is not specified
  when extracting components that include variable-specific results, the
  function will return results for all variables included in the
  symmetry test.

- component:

  An optional character string specifying a particular component to
  extract, when applicable. This argument is available just for
  `kardl_symmetric` objects, where users can specify whether they want
  to extract long-run or short-run results from the symmetry test. For
  example, if `what` is set to `"long_wald_tests"`, users can specify
  `component = "longrun"` to extract long-run Wald test results, or
  `component = "H0"` to extract the long-run null hypothesis
  description. If `component` is not specified when extracting
  components that include multiple subcomponents (e.g., both long-run
  and short-run results), the function will return all available
  subcomponents for the selected `what` value.

## Value

The requested component. The returned object depends on the class of
`object` and the selected value of `what`.

## Details

It provides a stable user-facing interface for accessing important
results without requiring users to rely on the internal list structure
of returned objects.

## Supported classes

`kardl_extract()` currently supports:

- `kardl_lm`

- `kardl_mplier`

- `kardl_boot`

- `kardl_test`

- `kardl_test_summary`

- `kardl_symmetric`

## Components for `kardl_lm` objects

For fitted `kardl_lm` model objects returned by
[`kardl()`](https://karamelikli.github.io/kardl/reference/kardl.md),
`what` may be one of:

- **data_ts_info**: time-series information about the input data.

- **data_is_ts**: whether the input data is a time series.

- **data_class**: class of the input data.

- **data_start**: starting time of the input data.

- **data_end**: ending time of the input data.

- **data_frequency**: frequency of the input data.

- **data_deltat**: delta time of the input data.

- **data_tsp**: time-series properties of the input data.

- **data_time**: time index of the input data.

- **no_constant**: Whether the model excludes an intercept.

- **trend**: Trend specification.

- **asym_long_vars**: Variables with long-run asymmetry.

- **asym_short_vars**: Variables with short-run asymmetry.

- **deterministic**: Deterministic regressors.

- **dependent_var**: Dependent variable.

- **independent_vars**: Independent variables.

- **all_vars**: All variables used in the model.

- **all_asym_vars**: All asymmetric variables.

- **indep_as_excluded**: Short-run asymmetric variables excluded from
  the linear set.

- **indep_al_excluded**: Long-run asymmetric variables excluded from the
  linear set.

- **short_run_vars**: Short-run variables.

- **long_run_vars**: Long-run variables.

- **shortrun_length**: Number of short-run terms.

- **lag_rows_number**: Number of rows lost due to lag construction.

- **model_type**: Model type.

- **data**: Prepared data used internally.

- **start_time**: Starting time of the estimation sample.

- **end_time**: Ending time of the estimation sample.

- **span**: Time span of the estimation sample.

- **opt_lag**: Selected optimal lag order.

- **lag_criteria**: Lag-selection criterion values.

- **all_cr_lags**: Candidate lag combinations and criterion values.

- **k**: Number of regressors used in relevant post-estimation
  procedures.

- **n**: Effective sample size.

## Components for `kardl_mplier` objects

For dynamic multiplier objects returned by
[`mplier()`](https://karamelikli.github.io/kardl/reference/mplier.md),
`what` may be one of:

- **multipliers**: Estimated dynamic multipliers.

- **omega**: Adjustment-related multiplier matrix.

- **lambda**: Short-run coefficient matrix used in multiplier
  construction.

- **vars**: Variables included in the multiplier calculation.

- **horizon**: Multiplier horizon.

## Components for `kardl_boot` objects

For bootstrapped multiplier objects returned by
[`bootstrap()`](https://karamelikli.github.io/kardl/reference/bootstrap.md),
`what` may be one of:

- **multipliers**: Bootstrapped dynamic multiplier results.

- **level**: Confidence level used for bootstrap intervals.

- **replications**: Number of bootstrap replications.

- **vars**: Variables included in the bootstrap procedure.

- **horizon**: Multiplier horizon.

## Components for `kardl_test` objects

the `kardl_test` class includes objects returned by
[`pssf()`](https://karamelikli.github.io/kardl/reference/pssf.md),
[`psst()`](https://karamelikli.github.io/kardl/reference/psst.md), and
[`narayan()`](https://karamelikli.github.io/kardl/reference/narayan.md)

For test objects, `what` may be one of:

- **type**: Type of test.

- **case**: Deterministic case used in the test.

- **statistic**: Test statistic.

- **method**: Test method description.

- **alternative**: Alternative hypothesis.

- **data.name**: Name of the data or model object.

- **sample.size**: Effective sample size.

- **hypotheses**: Textual description of the null and alternative
  hypotheses.

- **var_names**: Variables involved in the test.

- **k**: Number of regressors entering the bounds test.

- **n**: Sample size, when available.

- **sig**: Significance-level information.

- **notes**: Additional notes.

## Components for `kardl_test_summary` objects

the `kardl_test_summary` class includes summary objects produced by
[`summary()`](https://rdrr.io/r/base/summary.html) methods for
`kardl_test` objects, such as those produced by `summary.pssf()`,
`summary.psst()`, and `summary.narayan()`.

For test summary objects, `what` may be one of:

- **statistic**: Test statistic.

- **case**: Textual description of the deterministic case.

- **variables**: Variables included in the tested restriction.

- **decision**: Textual test decision.

- **hypotheses**: Textual description of the null and alternative
  hypotheses.

- **numeric_decision**: Numeric encoding of the test decision.

- **significance_level**: Significance level used for the decision.

- **critical_values**: Lower and upper critical-value bounds.

- **k**: Number of regressors entering the bounds test.

- **notes**: Additional notes.

## Components for `kardl_symmetric` objects

The `kardl_symmetric` class includes objects returned by the
[`symmetrytest()`](https://karamelikli.github.io/kardl/reference/symmetrytest.md)
function, which tests for long-run and short-run asymmetry in models
fitted with
[`kardl()`](https://karamelikli.github.io/kardl/reference/kardl.md) that
include asymmetric terms. For symmetry test objects, `what` may be one
of:

- **long_wald_summary**: Summary of long-run Wald test results.

- **long_hypotheses**: Textual description of long-run null and
  alternative hypotheses.

- **short_wald_summary**: Summary of short-run Wald test results.

- **short_hypotheses**: Textual description of short-run null and
  alternative hypotheses.

- **long_wald_tests**: Detailed long-run Wald test results.

- **short_wald_tests**: Detailed short-run Wald test results.

- **vars**: Variables included in the symmetry test.

- **type**: Type of symmetry test.

- **call**: Original function call that produced the object.

`component` and `variable` arguments allow for further subsetting of the
symmetry test results, enabling users to extract specific components
(long-run or short-run) and/or results for specific variables of
interest.

## Examples

``` r
kardl_model <- kardl(CPI ~ asym(ER + PPI), data = imf_example_data)

# Examples of extracting components from a fitted kardl_lm model object
# kardl_extract(kardl_model, what = "data_ts_info")
kardl_extract(kardl_model, what = "data_is_ts")
#> [1] TRUE
kardl_extract(kardl_model, what = "data_class")
#> [1] "mts"    "ts"     "matrix" "array" 
kardl_extract(kardl_model, what = "data_start")
#> [1] 1985    1
kardl_extract(kardl_model, what = "data_end")
#> [1] 2024    2
kardl_extract(kardl_model, what = "data_frequency")
#> [1] 12
kardl_extract(kardl_model, what = "data_deltat")
#> [1] 0.08333333
kardl_extract(kardl_model, what = "data_tsp")
#> [1] 1985.000 2024.083   12.000
head(kardl_extract(kardl_model, what = "data_time"))
#>           Jan      Feb      Mar      Apr      May      Jun
#> 1985 1985.000 1985.083 1985.167 1985.250 1985.333 1985.417
kardl_extract(kardl_model, what = "no_constant")
#> [1] FALSE
kardl_extract(kardl_model, what = "trend")
#> [1] FALSE
kardl_extract(kardl_model, what = "asym_long_vars")
#> [1] "ER"  "PPI"
kardl_extract(kardl_model, what = "asym_short_vars")
#> [1] "ER"  "PPI"
kardl_extract(kardl_model, what = "deterministic")
#> character(0)
kardl_extract(kardl_model, what = "dependent_var")
#> [1] "CPI"
kardl_extract(kardl_model, what = "independent_vars")
#> [1] "ER"  "PPI"
kardl_extract(kardl_model, what = "all_vars")
#> [1] "CPI" "ER"  "PPI"
kardl_extract(kardl_model, what = "all_asym_vars")
#> [1] "ER"  "PPI"
kardl_extract(kardl_model, what = "indep_as_excluded")
#> character(0)
kardl_extract(kardl_model, what = "indep_al_excluded")
#> character(0)
kardl_extract(kardl_model, what = "short_run_vars")
#> [1] "CPI"         "asyP_ER_PP"  "asyN_ER_NN"  "asyP_PPI_PP" "asyN_PPI_NN"
kardl_extract(kardl_model, what = "long_run_vars")
#> [1] "CPI"         "asyP_ER_PP"  "asyN_ER_NN"  "asyP_PPI_PP" "asyN_PPI_NN"
kardl_extract(kardl_model, what = "shortrun_length")
#> [1] 4
kardl_extract(kardl_model, what = "lag_rows_number")
#> [1] 16
#> attr(,"source")
#> [1] "kardl_set"
kardl_extract(kardl_model, what = "model_type")
#> [1] "NN"
# kardl_extract(kardl_model, what = "data")
kardl_extract(kardl_model, what = "start_time")
#> [1] "2026-06-23 12:52:46 +03"
kardl_extract(kardl_model, what = "end_time")
#> [1] "2026-06-23 12:52:46 +03"
kardl_extract(kardl_model, what = "span")
#> Time difference of 0.1266224 secs
kardl_extract(kardl_model, what = "opt_lag")
#>         CPI  asyP_ER_PP  asyN_ER_NN asyP_PPI_PP asyN_PPI_NN 
#>           2           1           0           2           0 
kardl_extract(kardl_model, what = "lag_criteria")
#>       CPI asyP_ER_PP asyN_ER_NN asyP_PPI_PP asyN_PPI_NN criterion_value
#>  [1,]   1          1          1           1           1       -5.454878
#>  [2,]   1          1          0           1           1       -5.461673
#>  [3,]   1          1          0           2           1       -5.470084
#>  [4,]   1          1          0           2           0       -5.476656
#>  [5,]   2          2          2           2           2       -5.445008
#>  [6,]   2          1          2           2           2       -5.452059
#>  [7,]   2          1          1           2           2       -5.458604
#>  [8,]   2          1          1           2           1       -5.464976
#>  [9,]   2          1          0           2           1       -5.472368
#> [10,]   2          1          0           2           0       -5.478817
kardl_extract(kardl_model, what = "all_cr_lags")
#> NULL
kardl_extract(kardl_model, what = "model_formula")
#> L0.d.CPI ~ L1.CPI + L1.asyP_ER_PP + L1.asyN_ER_NN + L1.asyP_PPI_PP + 
#>     L1.asyN_PPI_NN + L1.d.CPI + L2.d.CPI + L0.d.asyP_ER_PP + 
#>     L1.d.asyP_ER_PP + L0.d.asyN_ER_NN + L0.d.asyP_PPI_PP + L1.d.asyP_PPI_PP + 
#>     L2.d.asyP_PPI_PP + L0.d.asyN_PPI_NN
#> <environment: 0x5e0fb5fcfc00>
kardl_extract(kardl_model, what = "k")
#> [1] 15
kardl_extract(kardl_model, what = "n")
#> [1] 466




# Examples of extracting components from a kardl_mplier object

m <- mplier(kardl_model, horizon = 40)
head(kardl_extract(m, what = "multipliers"))
#>      h asyP_ER_PP   asyN_ER_NN     ER_dif asyP_PPI_PP   asyN_PPI_NN    PPI_dif
#> [1,] 0  0.1057965 -0.006028027 0.09976851  0.04297899 -0.0004242681 0.04255472
#> [2,] 1  0.2453959 -0.045465559 0.19993033  0.10372436 -0.0379403879 0.06578397
#> [3,] 2  0.3001933 -0.097080173 0.20311316  0.12828159 -0.0896455500 0.03863604
#> [4,] 3  0.3170421 -0.149695553 0.16734650  0.17178502 -0.1428250573 0.02895996
#> [5,] 4  0.3260237 -0.200725384 0.12529835  0.22539686 -0.1944183210 0.03097854
#> [6,] 5  0.3351129 -0.250165085 0.08494783  0.28044714 -0.2443690451 0.03607809
kardl_extract(m, what = "omega")
#> [1]  1.38376029 -0.49062085  0.08991344
head(kardl_extract(m, what = "lambda"))
#>        asyP_ER_PP  asyN_ER_NN  asyP_PPI_PP  asyN_PPI_NN
#> [1,]  0.105796542 0.006028027  0.042978991 0.0004242681
#> [2,] -0.006797711 0.031096187  0.001272744 0.0369290345
#> [3,] -0.086468588 0.000000000 -0.038413400 0.0000000000
#> [4,]  0.000000000 0.000000000  0.035460649 0.0000000000
#> [5,]  0.000000000 0.000000000  0.000000000 0.0000000000
#> [6,]  0.000000000 0.000000000  0.000000000 0.0000000000
kardl_extract(m, what = "horizon")
#> [1] 40

# Examples of extracting components from a kardl_boot object
boot_results <- bootstrap(kardl_model, horizon = 40, replications = 100)
head(kardl_extract(boot_results, what = "multipliers"))
#>   h asyP_ER_PP   asyN_ER_NN     ER_dif asyP_PPI_PP   asyN_PPI_NN    PPI_dif
#> 1 0  0.1057965 -0.006028027 0.09976851  0.04297899 -0.0004242681 0.04255472
#> 2 1  0.2453959 -0.045465559 0.19993033  0.10372436 -0.0379403879 0.06578397
#> 3 2  0.3001933 -0.097080173 0.20311316  0.12828159 -0.0896455500 0.03863604
#> 4 3  0.3170421 -0.149695553 0.16734650  0.17178502 -0.1428250573 0.02895996
#> 5 4  0.3260237 -0.200725384 0.12529835  0.22539686 -0.1944183210 0.03097854
#> 6 5  0.3351129 -0.250165085 0.08494783  0.28044714 -0.2443690451 0.03607809
#>   ER_CI_upper  ER_CI_lower PPI_CI_upper PPI_CI_lower
#> 1   0.2254097  0.002611729   0.09247899 -0.011620596
#> 2   0.3703336  0.057705118   0.14383395 -0.008644037
#> 3   0.3688363  0.054623643   0.13442562 -0.054535138
#> 4   0.3225953  0.013908883   0.12176919 -0.066785643
#> 5   0.2804250 -0.026865966   0.12449582 -0.065102667
#> 6   0.2400915 -0.063614397   0.12950162 -0.061786937
kardl_extract(boot_results, what = "level")
#> [1] 95
kardl_extract(boot_results, what = "replications")
#> [1] 100
kardl_extract(boot_results, what = "horizon")
#> [1] 40

# Examples of extracting components from a kardl_test object
test_results <- psst(kardl_model)
kardl_extract(test_results, what = "type")
#> [1] "cointegration"
kardl_extract(test_results, what = "case")
#> [1] 3
kardl_extract(test_results, what = "statistic")
#>         t 
#> -4.416905 
kardl_extract(test_results, what = "method")
#> [1] "Pesaran-Shin-Smith (PSS) Bounds t-test for cointegration"
kardl_extract(test_results, what = "alternative")
#> [1] "Cointegrating relationship exists"
kardl_extract(test_results, what = "data.name")
#> [1] "model"
kardl_extract(test_results, what = "sample.size")
#> [1] 469
#> attr(,"source")
#> [1] "kardl_set"
kardl_extract(test_results, what = "hypotheses")
#> 
#> Hypotheses:
#> H0: Coef(L1.CPI) = 0 
#> H1: Coef(L1.CPI) ≠ 0 
kardl_extract(test_results, what = "var_names")
#> [1] "L1.CPI"
kardl_extract(test_results, what = "k")
#> [1] 4
kardl_extract(test_results, what = "n")
#> [1] 470
kardl_extract(test_results, what = "sig")
#> [1] "auto"
kardl_extract(test_results, what = "notes")
#> NULL

# Examples of extracting components from a kardl_test_summary object
test_summary <- summary(test_results)
kardl_extract(test_summary, what = "statistic")
#>         t 
#> -4.416905 
kardl_extract(test_summary, what = "case")
#> [1] "III"
kardl_extract(test_summary, what = "variables")
#> [1] "L1.CPI"
kardl_extract(test_summary, what = "decision")
#> [1] "Reject H0 → Cointegration (at 2.5% level)"
kardl_extract(test_summary, what = "hypotheses")
#> 
#> Hypotheses:
#> H0: Coef(L1.CPI) = 0 
#> H1: Coef(L1.CPI) ≠ 0 
kardl_extract(test_summary, what = "numeric_decision")
#> [1] 1
kardl_extract(test_summary, what = "significance_level")
#> [1] "0.025"
kardl_extract(test_summary, what = "critical_values")
#>           L     U
#> 0.10  -2.57 -3.66
#> 0.05  -2.86 -3.99
#> 0.025 -3.13 -4.26
#> 0.01  -3.43 -4.60
kardl_extract(test_summary, what = "k")
#> [1] 4
kardl_extract(test_summary, what = "notes")
#> NULL

# Examples of extracting components from a kardl_symmetric object
symmetry_results <- symmetrytest(kardl_model)
kardl_extract(symmetry_results, what = "long_wald_summary")
#> Symmetry Test Results - Long-run:
#> =======================
#>     Df  Sum of Sq    Mean Sq F value   Pr(>F)   
#> ER   1 0.00160447 0.00160447  7.1413 0.007806 **
#> PPI  1 0.00037012 0.00037012  1.6474 0.199979   
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
kardl_extract(symmetry_results, what = "long_hypotheses")
#> 
#> Hypotheses:
#> 
#> Variable: ER 
#> H0: - Coef(L1.asyP_ER_PP)/Coef(L1.CPI) = - Coef(L1.asyN_ER_NN)/Coef(L1.CPI) 
#> H1: At least one coefficient differs from zero. 
#> 
#> 
#> Variable: PPI 
#> H0: - Coef(L1.asyP_PPI_PP)/Coef(L1.CPI) = - Coef(L1.asyN_PPI_NN)/Coef(L1.CPI) 
#> H1: At least one coefficient differs from zero. 
#> 
kardl_extract(symmetry_results, what = "short_wald_summary")
#> Symmetry Test Results - Short-run:
#> =======================
#>     Df  Sum of Sq    Mean Sq F value    Pr(>F)    
#> ER   1 0.00256486 0.00256486 11.4158 0.0007914 ***
#> PPI  1 0.00001981 0.00001981  0.0882 0.7666595    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
kardl_extract(symmetry_results, what = "short_hypotheses")
#> 
#> Hypotheses:
#> 
#> Variable: ER 
#> H0: Coef(L0.d.asyP_ER_PP) + Coef(L1.d.asyP_ER_PP) = Coef(L0.d.asyN_ER_NN) 
#> H1: Coef(L0.d.asyP_ER_PP) + Coef(L1.d.asyP_ER_PP) ≠ Coef(L0.d.asyN_ER_NN) 
#> 
#> 
#> Variable: PPI 
#> H0: Coef(L0.d.asyP_PPI_PP) + Coef(L1.d.asyP_PPI_PP) + Coef(L2.d.asyP_PPI_PP) = Coef(L0.d.asyN_PPI_NN) 
#> H1: Coef(L0.d.asyP_PPI_PP) + Coef(L1.d.asyP_PPI_PP) + Coef(L2.d.asyP_PPI_PP) ≠ Coef(L0.d.asyN_PPI_NN) 
#> 
kardl_extract(symmetry_results, what = "long_wald_tests")
#> $ER
#> 
#>  Wald F test of a restriction on model parameters
#> 
#> data:  kardl_model
#> F = 7.1413, df1 = 1, df2 = 451, p-value = 0.007806
#> 
#> 
#> $PPI
#> 
#>  Wald F test of a restriction on model parameters
#> 
#> data:  kardl_model
#> F = 1.6474, df1 = 1, df2 = 451, p-value = 0.2
#> 
#> 
kardl_extract(symmetry_results, what = "short_wald_tests")
#> $ER
#> 
#> Linear hypothesis test:
#> L0.d.asyP_ER_PP  + L1.d.asyP_ER_PP - L0.d.asyN_ER_NN = 0
#> 
#> Model 1: restricted model
#> Model 2: L0.d.CPI ~ L1.CPI + L1.asyP_ER_PP + L1.asyN_ER_NN + L1.asyP_PPI_PP + 
#>     L1.asyN_PPI_NN + L1.d.CPI + L2.d.CPI + L0.d.asyP_ER_PP + 
#>     L1.d.asyP_ER_PP + L0.d.asyN_ER_NN + L0.d.asyP_PPI_PP + L1.d.asyP_PPI_PP + 
#>     L2.d.asyP_PPI_PP + L0.d.asyN_PPI_NN
#> 
#>   Res.Df     RSS Df Sum of Sq      F    Pr(>F)    
#> 1    452 0.10389                                  
#> 2    451 0.10133  1 0.0025649 11.416 0.0007914 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> $PPI
#> 
#> Linear hypothesis test:
#> L0.d.asyP_PPI_PP  + L1.d.asyP_PPI_PP  + L2.d.asyP_PPI_PP - L0.d.asyN_PPI_NN = 0
#> 
#> Model 1: restricted model
#> Model 2: L0.d.CPI ~ L1.CPI + L1.asyP_ER_PP + L1.asyN_ER_NN + L1.asyP_PPI_PP + 
#>     L1.asyN_PPI_NN + L1.d.CPI + L2.d.CPI + L0.d.asyP_ER_PP + 
#>     L1.d.asyP_ER_PP + L0.d.asyN_ER_NN + L0.d.asyP_PPI_PP + L1.d.asyP_PPI_PP + 
#>     L2.d.asyP_PPI_PP + L0.d.asyN_PPI_NN
#> 
#>   Res.Df     RSS Df  Sum of Sq      F Pr(>F)
#> 1    452 0.10135                            
#> 2    451 0.10133  1 1.9809e-05 0.0882 0.7667
#> 
kardl_extract(symmetry_results, what = "vars")
#> [1] "ER"  "PPI"
kardl_extract(symmetry_results, what = "type")
#> [1] "F"
kardl_extract(symmetry_results, what = "call")
#> symmetrytest.kardl_lm(kardl_model = kardl_model)

# Example of extracting specific components from symmetry test results
kardl_extract(symmetry_results, what = "short_wald_tests", variable = "PPI")
#> 
#> Linear hypothesis test:
#> L0.d.asyP_PPI_PP  + L1.d.asyP_PPI_PP  + L2.d.asyP_PPI_PP - L0.d.asyN_PPI_NN = 0
#> 
#> Model 1: restricted model
#> Model 2: L0.d.CPI ~ L1.CPI + L1.asyP_ER_PP + L1.asyN_ER_NN + L1.asyP_PPI_PP + 
#>     L1.asyN_PPI_NN + L1.d.CPI + L2.d.CPI + L0.d.asyP_ER_PP + 
#>     L1.d.asyP_ER_PP + L0.d.asyN_ER_NN + L0.d.asyP_PPI_PP + L1.d.asyP_PPI_PP + 
#>     L2.d.asyP_PPI_PP + L0.d.asyN_PPI_NN
#> 
#>   Res.Df     RSS Df  Sum of Sq      F Pr(>F)
#> 1    452 0.10135                            
#> 2    451 0.10133  1 1.9809e-05 0.0882 0.7667
kardl_extract(symmetry_results, what = "long_wald_tests", variable = "PPI")
#> 
#>  Wald F test of a restriction on model parameters
#> 
#> data:  kardl_model
#> F = 1.6474, df1 = 1, df2 = 451, p-value = 0.2
#> 
kardl_extract(symmetry_results, what = "long_hypotheses", variable = "PPI")
#> 
#> Hypotheses:
#> 
#> Variable: PPI 
#> H0: - Coef(L1.asyP_PPI_PP)/Coef(L1.CPI) = - Coef(L1.asyN_PPI_NN)/Coef(L1.CPI) 
#> H1: At least one coefficient differs from zero. 
#> 
kardl_extract(symmetry_results, what = "short_hypotheses", variable = "PPI")
#> 
#> Hypotheses:
#> 
#> Variable: PPI 
#> H0: Coef(L0.d.asyP_PPI_PP) + Coef(L1.d.asyP_PPI_PP) + Coef(L2.d.asyP_PPI_PP) = Coef(L0.d.asyN_PPI_NN) 
#> H1: Coef(L0.d.asyP_PPI_PP) + Coef(L1.d.asyP_PPI_PP) + Coef(L2.d.asyP_PPI_PP) ≠ Coef(L0.d.asyN_PPI_NN) 
#> 
kardl_extract(symmetry_results, what = "short_hypotheses", component = "H0")
#> 
#> Hypotheses:
#> 
#> Variable: ER 
#> H0: Coef(L0.d.asyP_ER_PP) + Coef(L1.d.asyP_ER_PP) = Coef(L0.d.asyN_ER_NN) 
#> 
#> Variable: PPI 
#> H0: Coef(L0.d.asyP_PPI_PP) + Coef(L1.d.asyP_PPI_PP) + Coef(L2.d.asyP_PPI_PP) = Coef(L0.d.asyN_PPI_NN) 
kardl_extract(symmetry_results, what = "short_hypotheses", component = "H1")
#> 
#> Hypotheses:
#> 
#> Variable: ER 
#> H1: Coef(L0.d.asyP_ER_PP) + Coef(L1.d.asyP_ER_PP) ≠ Coef(L0.d.asyN_ER_NN) 
#> 
#> 
#> Variable: PPI 
#> H1: Coef(L0.d.asyP_PPI_PP) + Coef(L1.d.asyP_PPI_PP) + Coef(L2.d.asyP_PPI_PP) ≠ Coef(L0.d.asyN_PPI_NN) 
#> 

kardl_extract(symmetry_results,
  what = "short_hypotheses",
  variable = "PPI", component = "H0"
)
#> 
#> Hypotheses:
#> 
#> Variable: PPI 
#> H0: Coef(L0.d.asyP_PPI_PP) + Coef(L1.d.asyP_PPI_PP) + Coef(L2.d.asyP_PPI_PP) = Coef(L0.d.asyN_PPI_NN) 
```

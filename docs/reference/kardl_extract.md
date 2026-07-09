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
kardl_model <- kardl(DriversKilled ~ asym(PetrolPrice + drivers), data = Seatbelts,
mode = c(2, 1, 0, 4, 0 ))

# Examples of extracting components from a fitted kardl_lm model object
# kardl_extract(kardl_model, what = "data_ts_info")
kardl_extract(kardl_model, what = "data_is_ts")
#> [1] TRUE
kardl_extract(kardl_model, what = "data_class")
#> [1] "mts"    "ts"     "matrix" "array" 
kardl_extract(kardl_model, what = "data_start")
#> [1] 1969    1
kardl_extract(kardl_model, what = "data_end")
#> [1] 1984   12
kardl_extract(kardl_model, what = "data_frequency")
#> [1] 12
kardl_extract(kardl_model, what = "data_deltat")
#> [1] 0.08333333
kardl_extract(kardl_model, what = "data_tsp")
#> [1] 1969.000 1984.917   12.000
head(kardl_extract(kardl_model, what = "data_time"))
#>           Jan      Feb      Mar      Apr      May      Jun
#> 1969 1969.000 1969.083 1969.167 1969.250 1969.333 1969.417
kardl_extract(kardl_model, what = "no_constant")
#> [1] FALSE
kardl_extract(kardl_model, what = "trend")
#> [1] FALSE
kardl_extract(kardl_model, what = "asym_long_vars")
#> [1] "PetrolPrice" "drivers"    
kardl_extract(kardl_model, what = "asym_short_vars")
#> [1] "PetrolPrice" "drivers"    
kardl_extract(kardl_model, what = "deterministic")
#> character(0)
kardl_extract(kardl_model, what = "dependent_var")
#> [1] "DriversKilled"
kardl_extract(kardl_model, what = "independent_vars")
#> [1] "PetrolPrice" "drivers"    
kardl_extract(kardl_model, what = "all_vars")
#> [1] "DriversKilled" "PetrolPrice"   "drivers"      
kardl_extract(kardl_model, what = "all_asym_vars")
#> [1] "PetrolPrice" "drivers"    
kardl_extract(kardl_model, what = "indep_as_excluded")
#> character(0)
kardl_extract(kardl_model, what = "indep_al_excluded")
#> character(0)
kardl_extract(kardl_model, what = "short_run_vars")
#> [1] "DriversKilled"       "asyP_PetrolPrice_PP" "asyN_PetrolPrice_NN"
#> [4] "asyP_drivers_PP"     "asyN_drivers_NN"    
kardl_extract(kardl_model, what = "long_run_vars")
#> [1] "DriversKilled"       "asyP_PetrolPrice_PP" "asyN_PetrolPrice_NN"
#> [4] "asyP_drivers_PP"     "asyN_drivers_NN"    
kardl_extract(kardl_model, what = "shortrun_length")
#> [1] 4
kardl_extract(kardl_model, what = "lag_rows_number")
#> [1] 768
kardl_extract(kardl_model, what = "model_type")
#> [1] "NN"
# kardl_extract(kardl_model, what = "data")
kardl_extract(kardl_model, what = "start_time")
#> [1] "2026-07-09 15:34:39 +03"
kardl_extract(kardl_model, what = "end_time")
#> [1] "2026-07-09 15:34:39 +03"
kardl_extract(kardl_model, what = "span")
#> Time difference of 0.004216909 secs
kardl_extract(kardl_model, what = "opt_lag")
#>       DriversKilled asyP_PetrolPrice_PP asyN_PetrolPrice_NN     asyP_drivers_PP 
#>                   2                   1                   0                   4 
#>     asyN_drivers_NN 
#>                   0 
kardl_extract(kardl_model, what = "lag_criteria")
#> NULL
kardl_extract(kardl_model, what = "all_cr_lags")
#> NULL
kardl_extract(kardl_model, what = "model_formula")
#> L0.d.DriversKilled ~ L1.DriversKilled + L1.asyP_PetrolPrice_PP + 
#>     L1.asyN_PetrolPrice_NN + L1.asyP_drivers_PP + L1.asyN_drivers_NN + 
#>     L1.d.DriversKilled + L2.d.DriversKilled + L0.d.asyP_PetrolPrice_PP + 
#>     L1.d.asyP_PetrolPrice_PP + L0.d.asyN_PetrolPrice_NN + L0.d.asyP_drivers_PP + 
#>     L1.d.asyP_drivers_PP + L2.d.asyP_drivers_PP + L3.d.asyP_drivers_PP + 
#>     L4.d.asyP_drivers_PP + L0.d.asyN_drivers_NN
#> <environment: 0x5ac63b82e8b8>
kardl_extract(kardl_model, what = "k")
#> [1] 17
kardl_extract(kardl_model, what = "n")
#> [1] 186




# Examples of extracting components from a kardl_mplier object
# \donttest{
# This long-running example won't be tested by CRAN
m <- mplier(kardl_model, horizon = 40)
head(kardl_extract(m, what = "multipliers"))
#>      h asyP_PetrolPrice_PP asyN_PetrolPrice_NN PetrolPrice_dif asyP_drivers_PP
#> [1,] 0         -352.594515         -1036.63715     -1389.23167      0.07657003
#> [2,] 1         -128.915185            18.50466      -110.41053      0.09324058
#> [3,] 2            7.693244           138.57168       146.26492      0.05889118
#> [4,] 3           30.569985           166.80773       197.37771      0.07772017
#> [5,] 4            6.221079            77.68728        83.90836      0.07421712
#> [6,] 5           -7.338373            61.43681        54.09843      0.07541212
#>      asyN_drivers_NN   drivers_dif
#> [1,]     -0.07535387  0.0012161619
#> [2,]     -0.08524853  0.0079920489
#> [3,]     -0.08146087 -0.0225696959
#> [4,]     -0.07468890  0.0030312672
#> [5,]     -0.07373551  0.0004816077
#> [6,]     -0.07437305  0.0010390648
kardl_extract(m, what = "omega")
#> [1]  0.05704919 -0.05775601 -0.07941748
head(kardl_extract(m, what = "lambda"))
#>      asyP_PetrolPrice_PP asyN_PetrolPrice_NN asyP_drivers_PP asyN_drivers_NN
#> [1,]           -352.5945            1036.637     0.076570029     0.075353867
#> [2,]            243.7946           -1114.281     0.012302290     0.005595784
#> [3,]            103.4832               0.000    -0.030878063     0.000000000
#> [4,]              0.0000               0.000     0.027832419     0.000000000
#> [5,]              0.0000               0.000    -0.005237174     0.000000000
#> [6,]              0.0000               0.000    -0.000245614     0.000000000
kardl_extract(m, what = "horizon")
#> [1] 40

# Examples of extracting components from a kardl_boot object
boot_results <- bootstrap(kardl_model, horizon = 40, replications = 2)
#> Warning: The data is not a time series. It has been converted to a time series with start = 1 and frequency = 1.
#> Warning: The data is not a time series. It has been converted to a time series with start = 1 and frequency = 1.
head(kardl_extract(boot_results, what = "multipliers"))
#>   h asyP_PetrolPrice_PP asyN_PetrolPrice_NN PetrolPrice_dif asyP_drivers_PP
#> 1 0         -352.594515         -1036.63715     -1389.23167      0.07657003
#> 2 1         -128.915185            18.50466      -110.41053      0.09324058
#> 3 2            7.693244           138.57168       146.26492      0.05889118
#> 4 3           30.569985           166.80773       197.37771      0.07772017
#> 5 4            6.221079            77.68728        83.90836      0.07421712
#> 6 5           -7.338373            61.43681        54.09843      0.07541212
#>   asyN_drivers_NN   drivers_dif PetrolPrice_CI_upper PetrolPrice_CI_lower
#> 1     -0.07535387  0.0012161619         -2352.949800          -2518.00455
#> 2     -0.08524853  0.0079920489          -335.193323           -466.44350
#> 3     -0.08146087 -0.0225696959           302.096843            -23.46873
#> 4     -0.07468890  0.0030312672           369.427212            -99.46575
#> 5     -0.07373551  0.0004816077             8.623113           -184.60401
#> 6     -0.07437305  0.0010390648           -96.039434           -191.78185
#>   drivers_CI_upper drivers_CI_lower
#> 1      0.011152324    -0.0072414082
#> 2      0.003340704    -0.0086973935
#> 3     -0.009304857    -0.0203781998
#> 4      0.021895374     0.0084294684
#> 5      0.007754456     0.0009541362
#> 6      0.002461729    -0.0013641216
kardl_extract(boot_results, what = "level")
#> [1] 95
kardl_extract(boot_results, what = "replications")
#> [1] 2
kardl_extract(boot_results, what = "horizon")
#> [1] 40
# }
# Examples of extracting components from a kardl_test object
test_results <- psst(kardl_model)
kardl_extract(test_results, what = "type")
#> [1] "cointegration"
kardl_extract(test_results, what = "case")
#> [1] 3
kardl_extract(test_results, what = "statistic")
#>         t 
#> -9.917541 
kardl_extract(test_results, what = "method")
#> [1] "Pesaran-Shin-Smith (PSS) Bounds t-test for cointegration"
kardl_extract(test_results, what = "alternative")
#> [1] "Cointegrating relationship exists"
kardl_extract(test_results, what = "data.name")
#> [1] "model"
kardl_extract(test_results, what = "sample.size")
#> [1] 191
kardl_extract(test_results, what = "hypotheses")
#> 
#> Hypotheses:
#> H0: Coef(L1.DriversKilled) = 0 
#> H1: Coef(L1.DriversKilled) ≠ 0 
kardl_extract(test_results, what = "var_names")
#> [1] "L1.DriversKilled"
kardl_extract(test_results, what = "k")
#> [1] 4
kardl_extract(test_results, what = "n")
#> [1] 192
kardl_extract(test_results, what = "sig")
#> [1] "auto"
kardl_extract(test_results, what = "notes")
#> NULL

# Examples of extracting components from a kardl_test_summary object
test_summary <- summary(test_results)
kardl_extract(test_summary, what = "statistic")
#>         t 
#> -9.917541 
kardl_extract(test_summary, what = "case")
#> [1] "III"
kardl_extract(test_summary, what = "variables")
#> [1] "L1.DriversKilled"
kardl_extract(test_summary, what = "decision")
#> [1] "Reject H0 → Cointegration (at 1% level)"
kardl_extract(test_summary, what = "hypotheses")
#> 
#> Hypotheses:
#> H0: Coef(L1.DriversKilled) = 0 
#> H1: Coef(L1.DriversKilled) ≠ 0 
kardl_extract(test_summary, what = "numeric_decision")
#> [1] 1
kardl_extract(test_summary, what = "significance_level")
#> [1] "0.01"
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
#>             Df Sum of Sq Mean Sq F value Pr(>F)
#> PetrolPrice  1    33.094  33.094  0.2587 0.6117
#> drivers      1    19.680  19.680  0.1538 0.6954
kardl_extract(symmetry_results, what = "long_hypotheses")
#> 
#> Hypotheses:
#> 
#> Variable: PetrolPrice 
#> H0: - Coef(L1.asyP_PetrolPrice_PP)/Coef(L1.DriversKilled) = - Coef(L1.asyN_PetrolPrice_NN)/Coef(L1.DriversKilled) 
#> H1: At least one coefficient differs from zero. 
#> 
#> 
#> Variable: drivers 
#> H0: - Coef(L1.asyP_drivers_PP)/Coef(L1.DriversKilled) = - Coef(L1.asyN_drivers_NN)/Coef(L1.DriversKilled) 
#> H1: At least one coefficient differs from zero. 
#> 
kardl_extract(symmetry_results, what = "short_wald_summary")
#> Symmetry Test Results - Short-run:
#> =======================
#>             Df Sum of Sq Mean Sq F value Pr(>F)
#> PetrolPrice  1    203.66  203.66  1.5919 0.2088
#> drivers      1      7.90    7.90  0.0618 0.8041
kardl_extract(symmetry_results, what = "short_hypotheses")
#> 
#> Hypotheses:
#> 
#> Variable: PetrolPrice 
#> H0: Coef(L0.d.asyP_PetrolPrice_PP) + Coef(L1.d.asyP_PetrolPrice_PP) = Coef(L0.d.asyN_PetrolPrice_NN) 
#> H1: Coef(L0.d.asyP_PetrolPrice_PP) + Coef(L1.d.asyP_PetrolPrice_PP) ≠ Coef(L0.d.asyN_PetrolPrice_NN) 
#> 
#> 
#> Variable: drivers 
#> H0: Coef(L0.d.asyP_drivers_PP) + Coef(L1.d.asyP_drivers_PP) + Coef(L2.d.asyP_drivers_PP) + Coef(L3.d.asyP_drivers_PP) + Coef(L4.d.asyP_drivers_PP) = Coef(L0.d.asyN_drivers_NN) 
#> H1: Coef(L0.d.asyP_drivers_PP) + Coef(L1.d.asyP_drivers_PP) + Coef(L2.d.asyP_drivers_PP) + Coef(L3.d.asyP_drivers_PP) + Coef(L4.d.asyP_drivers_PP) ≠ Coef(L0.d.asyN_drivers_NN) 
#> 
kardl_extract(symmetry_results, what = "long_wald_tests")
#> $PetrolPrice
#> 
#>  Wald F test of a restriction on model parameters
#> 
#> data:  kardl_model
#> F = 0.25868, df1 = 1, df2 = 169, p-value = 0.6117
#> 
#> 
#> $drivers
#> 
#>  Wald F test of a restriction on model parameters
#> 
#> data:  kardl_model
#> F = 0.15383, df1 = 1, df2 = 169, p-value = 0.6954
#> 
#> 
kardl_extract(symmetry_results, what = "short_wald_tests")
#> $PetrolPrice
#> 
#> Linear hypothesis test:
#> L0.d.asyP_PetrolPrice_PP  + L1.d.asyP_PetrolPrice_PP - L0.d.asyN_PetrolPrice_NN = 0
#> 
#> Model 1: restricted model
#> Model 2: L0.d.DriversKilled ~ L1.DriversKilled + L1.asyP_PetrolPrice_PP + 
#>     L1.asyN_PetrolPrice_NN + L1.asyP_drivers_PP + L1.asyN_drivers_NN + 
#>     L1.d.DriversKilled + L2.d.DriversKilled + L0.d.asyP_PetrolPrice_PP + 
#>     L1.d.asyP_PetrolPrice_PP + L0.d.asyN_PetrolPrice_NN + L0.d.asyP_drivers_PP + 
#>     L1.d.asyP_drivers_PP + L2.d.asyP_drivers_PP + L3.d.asyP_drivers_PP + 
#>     L4.d.asyP_drivers_PP + L0.d.asyN_drivers_NN
#> 
#>   Res.Df   RSS Df Sum of Sq      F Pr(>F)
#> 1    170 21824                           
#> 2    169 21620  1    203.66 1.5919 0.2088
#> 
#> $drivers
#> 
#> Linear hypothesis test:
#> L0.d.asyP_drivers_PP  + L1.d.asyP_drivers_PP  + L2.d.asyP_drivers_PP  + L3.d.asyP_drivers_PP  + L4.d.asyP_drivers_PP - L0.d.asyN_drivers_NN = 0
#> 
#> Model 1: restricted model
#> Model 2: L0.d.DriversKilled ~ L1.DriversKilled + L1.asyP_PetrolPrice_PP + 
#>     L1.asyN_PetrolPrice_NN + L1.asyP_drivers_PP + L1.asyN_drivers_NN + 
#>     L1.d.DriversKilled + L2.d.DriversKilled + L0.d.asyP_PetrolPrice_PP + 
#>     L1.d.asyP_PetrolPrice_PP + L0.d.asyN_PetrolPrice_NN + L0.d.asyP_drivers_PP + 
#>     L1.d.asyP_drivers_PP + L2.d.asyP_drivers_PP + L3.d.asyP_drivers_PP + 
#>     L4.d.asyP_drivers_PP + L0.d.asyN_drivers_NN
#> 
#>   Res.Df   RSS Df Sum of Sq      F Pr(>F)
#> 1    170 21628                           
#> 2    169 21620  1    7.8998 0.0618 0.8041
#> 
kardl_extract(symmetry_results, what = "vars")
#> [1] "PetrolPrice" "drivers"    
kardl_extract(symmetry_results, what = "type")
#> [1] "F"
kardl_extract(symmetry_results, what = "call")
#> symmetrytest.kardl_lm(kardl_model = kardl_model)

# Example of extracting specific components from symmetry test results
kardl_extract(symmetry_results, what = "short_wald_tests", variable = "PPI")
#> Error: Variable(s) not found: PPI. Available variables: PetrolPrice, drivers
kardl_extract(symmetry_results, what = "long_wald_tests", variable = "PPI")
#> Error: Variable(s) not found: PPI. Available variables: PetrolPrice, drivers
kardl_extract(symmetry_results, what = "long_hypotheses", variable = "PPI")
#> Error: Variable(s) not found: PPI. Available variables: PetrolPrice, drivers
kardl_extract(symmetry_results, what = "short_hypotheses", variable = "PPI")
#> Error: Variable(s) not found: PPI. Available variables: PetrolPrice, drivers
kardl_extract(symmetry_results, what = "short_hypotheses", component = "H0")
#> 
#> Hypotheses:
#> 
#> Variable: PetrolPrice 
#> H0: Coef(L0.d.asyP_PetrolPrice_PP) + Coef(L1.d.asyP_PetrolPrice_PP) = Coef(L0.d.asyN_PetrolPrice_NN) 
#> 
#> Variable: drivers 
#> H0: Coef(L0.d.asyP_drivers_PP) + Coef(L1.d.asyP_drivers_PP) + Coef(L2.d.asyP_drivers_PP) + Coef(L3.d.asyP_drivers_PP) + Coef(L4.d.asyP_drivers_PP) = Coef(L0.d.asyN_drivers_NN) 
kardl_extract(symmetry_results, what = "short_hypotheses", component = "H1")
#> 
#> Hypotheses:
#> 
#> Variable: PetrolPrice 
#> H1: Coef(L0.d.asyP_PetrolPrice_PP) + Coef(L1.d.asyP_PetrolPrice_PP) ≠ Coef(L0.d.asyN_PetrolPrice_NN) 
#> 
#> 
#> Variable: drivers 
#> H1: Coef(L0.d.asyP_drivers_PP) + Coef(L1.d.asyP_drivers_PP) + Coef(L2.d.asyP_drivers_PP) + Coef(L3.d.asyP_drivers_PP) + Coef(L4.d.asyP_drivers_PP) ≠ Coef(L0.d.asyN_drivers_NN) 
#> 

kardl_extract(symmetry_results,
  what = "short_hypotheses",
  variable = "PPI", component = "H0"
)
#> Error: Variable(s) not found: PPI. Available variables: PetrolPrice, drivers
```

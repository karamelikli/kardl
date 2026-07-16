#' Extract Components from kardl Objects
#'
#' `kardl_extract()` is a generic accessor for retrieving selected documented
#' components from objects produced by the **kardl** package.
#'
#' It provides a stable user-facing interface for accessing important results
#' without requiring users to rely on the internal list structure of returned
#' objects.
#'
#' @param kardl_object A supported object produced by the **kardl** package.
#' @param what A character string specifying the component to extract. The
#' available options depend on the class of `object` and are documented in the
#' sections below.
#' @param component An optional character string specifying a particular
#' component to extract, when applicable. This argument is available just for
#' `kardl_symmetric` objects, where users can specify whether they want to
#' extract long-run or short-run results from the symmetry test.
#' For example, if `what` is set to `"long_wald_tests"`, users can specify
#' `component = "longrun"` to extract long-run Wald test results, or
#' `component = "H0"` to extract the long-run null hypothesis description.
#' If `component` is not specified when extracting components that include
#' multiple subcomponents (e.g., both long-run and short-run results),
#' the function will return all available subcomponents for the selected
#' `what` value.
#'
#' @param variable An optional character string specifying a particular variable
#'  to extract results for, when applicable. The available options depend on the
#' selected `what` value and are documented in the relevant sections below. This
#' argument is available just for `kardl_symmetric` objects, where users can
#' extract results for specific variables included in the symmetry test. For
#' example, if a symmetry test includes variables "drivers" and "PetrolPrice",
#' users can specify `variable = "drivers"` to extract results related to the
#' "drivers" variable, or `variable = c("drivers", "PetrolPrice")` to extract
#' results for both variables.
#' If `variable` is not specified when extracting components that include
#' variable-specific results, the function will return results for all
#' variables included in the symmetry test.
#'
#' @return
#' The requested component. The returned object depends on the class of `object`
#' and the selected value of `what`.
#'
#' @section Supported classes:
#' `kardl_extract()` currently supports:
#' \itemize{
#'   \item `kardl_lm`
#'   \item `kardl_mplier`
#'   \item `kardl_boot`
#'   \item `kardl_test`
#'   \item `kardl_test_summary`
#'   \item `kardl_symmetric`
#' }
#'
#' @section Components for `kardl_lm` objects:
#' For fitted `kardl_lm` model objects returned by [kardl()], `what`
#' may be one of:
#' \itemize{
#'   \item{**data_ts_info**:  time-series information about the input data.}
#'   \item{**data_is_ts**:  whether the input data is a time series.}
#'   \item{**data_class**:  class of the input data.}
#'   \item{**data_start**:  starting time of the input data.}
#'   \item{**data_end**:  ending time of the input data.}
#'   \item{**data_frequency**:  frequency of the input data.}
#'   \item{**data_deltat**:  delta time of the input data.}
#'   \item{**data_tsp**:  time-series properties of the input data.}
#'   \item{**data_time**:  time index of the input data.}
#'   \item{**no_constant**:  Whether the model excludes an intercept.}
#'   \item{**trend**:  Trend specification.}
#'   \item{**asym_long_vars**:  Variables with long-run asymmetry.}
#'   \item{**asym_short_vars**:  Variables with short-run asymmetry.}
#'   \item{**deterministic**:  Deterministic regressors.}
#'   \item{**dependent_var**:  Dependent variable.}
#'   \item{**independent_vars**:  Independent variables.}
#'   \item{**all_vars**:  All variables used in the model.}
#'   \item{**all_asym_vars**:  All asymmetric variables.}
#'   \item{**indep_as_excluded**:  Short-run asymmetric variables excluded from
#'   the linear set.}
#'   \item{**indep_al_excluded**:  Long-run asymmetric variables excluded from
#'   the linear set.}
#'   \item{**short_run_vars**:  Short-run variables.}
#'   \item{**long_run_vars**:  Long-run variables.}
#'   \item{**shortrun_length**:  Number of short-run terms.}
#'   \item{**lag_rows_number**:  Number of rows lost due to lag construction.}
#'   \item{**model_type**:  Model type.}
#'   \item{**data**:  Prepared data used internally.}
#'   \item{**start_time**:  Starting time of the estimation sample.}
#'   \item{**end_time**:  Ending time of the estimation sample.}
#'   \item{**span**:  Time span of the estimation sample.}
#'   \item{**opt_lag**:  Selected optimal lag order.}
#'   \item{**lag_criteria**:  Lag-selection criterion values.}
#'   \item{**all_cr_lags**:  Candidate lag combinations and criterion values.}
#'   \item{**k**:  Number of regressors used in relevant post-estimation
#'   procedures.}
#'   \item{**n**:  Effective sample size.}
#' }
#'
#' @section Components for `kardl_mplier` objects:
#' For dynamic multiplier objects returned by [mplier()], `what` may be one of:
#' \itemize{
#'   \item{**multipliers**: Estimated dynamic multipliers.}
#'   \item{**omega**:  Adjustment-related multiplier matrix.}
#'   \item{**lambda**:  Short-run coefficient matrix used in multiplier
#'   construction.}
#'   \item{**vars**:  Variables included in the multiplier calculation.}
#'   \item{**horizon**:  Multiplier horizon.}
#' }
#'
#' @section Components for `kardl_boot` objects:
#' For bootstrapped multiplier objects returned by [bootstrap()], `what` may be
#' one of:
#' \itemize{
#'   \item{**multipliers**:  Bootstrapped dynamic multiplier results.}
#'   \item{**level**:  Confidence level used for bootstrap intervals.}
#'   \item{**replications**:  Number of bootstrap replications.}
#'   \item{**vars**:  Variables included in the bootstrap procedure.}
#'   \item{**horizon**:  Multiplier horizon.}
#' }
#'
#' @section Components for `kardl_test` objects:
#'  the `kardl_test` class includes objects returned by [pssf()], [psst()],
#'  and [narayan()]
#'
#' For test objects, `what` may be one of:
#' \itemize{
#'   \item{**type**:  Type of test.}
#'   \item{**case**:  Deterministic case used in the test.}
#'   \item{**statistic**:  Test statistic.}
#'   \item{**method**:  Test method description.}
#'   \item{**alternative**:  Alternative hypothesis.}
#'   \item{**data.name**:  Name of the data or model object.}
#'   \item{**sample.size**:  Effective sample size.}
#'   \item{**hypotheses**:  Textual description of the null and alternative
#'    hypotheses.}
#'   \item{**var_names**:  Variables involved in the test.}
#'   \item{**k**:  Number of regressors entering the bounds test.}
#'   \item{**n**:  Sample size, when available.}
#'   \item{**sig**:  Significance-level information.}
#'   \item{**notes**:  Additional notes.}
#' }
#'
#' @section Components for `kardl_test_summary` objects :
#'
#' the `kardl_test_summary` class includes summary objects produced by
#' `summary()` methods for `kardl_test` objects, such as those produced
#' by `summary.pssf()`, `summary.psst()`, and `summary.narayan()`.
#'
#'
#' For test summary objects, `what` may be one of:
#' \itemize{
#'   \item{**statistic**:  Test statistic.}
#'   \item{**case**:  Textual description of the deterministic case.}
#'   \item{**variables**:  Variables included in the tested restriction.}
#'   \item{**decision**:  Textual test decision.}
#'   \item{**hypotheses**:  Textual description of the null and alternative
#'   hypotheses.}
#'   \item{**numeric_decision**:  Numeric encoding of the test decision.}
#'   \item{**significance_level**:  Significance level used for the decision.}
#'   \item{**critical_values**:  Lower and upper critical-value bounds.}
#'   \item{**k**:  Number of regressors entering the bounds test.}
#'   \item{**notes**:  Additional notes.}
#' }
#'
#' @section Components for `kardl_symmetric` objects:
#' The `kardl_symmetric` class includes objects returned by the
#' [symmetrytest()] function, which tests for long-run and short-run asymmetry
#' in models fitted with [kardl()] that include asymmetric terms.
#' For symmetry test objects, `what` may be one of:
#' \itemize{
#'  \item{**long_wald_summary**:  Summary of long-run Wald test results.}
#'  \item{**long_hypotheses**:  Textual description of
#'  long-run null and alternative hypotheses.}
#'  \item{**short_wald_summary**:  Summary of short-run Wald test results.}
#'  \item{**short_hypotheses**:  Textual description of
#'  short-run null and alternative hypotheses.}
#'  \item{**long_wald_tests**:  Detailed long-run Wald test results.}
#'  \item{**short_wald_tests**:  Detailed short-run Wald test results.}
#'  \item{**vars**:  Variables included in the symmetry test.}
#'  \item{**type**:  Type of symmetry test.}
#'  \item{**call**:  Original function call that produced the object.}
#'  }
#'  `component` and `variable` arguments allow for further subsetting of the
#'  symmetry test results, enabling users to extract specific components
#'  (long-run or short-run) and/or results for specific variables of interest.
#'
#' @examples
#' kardl_model <- kardl(DriversKilled ~ asym(PetrolPrice + drivers),
#'   data = Seatbelts,
#'   mode = c(2, 1, 0, 4, 0)
#' )
#'
#' # Examples of extracting components from a fitted kardl_lm model object
#' # kardl_extract(kardl_model, what = "data_ts_info")
#' kardl_extract(kardl_model, what = "data_is_ts")
#' kardl_extract(kardl_model, what = "data_class")
#' kardl_extract(kardl_model, what = "data_start")
#' kardl_extract(kardl_model, what = "data_end")
#' kardl_extract(kardl_model, what = "data_frequency")
#' kardl_extract(kardl_model, what = "data_deltat")
#' kardl_extract(kardl_model, what = "data_tsp")
#' head(kardl_extract(kardl_model, what = "data_time"))
#' kardl_extract(kardl_model, what = "no_constant")
#' kardl_extract(kardl_model, what = "trend")
#' kardl_extract(kardl_model, what = "asym_long_vars")
#' kardl_extract(kardl_model, what = "asym_short_vars")
#' kardl_extract(kardl_model, what = "deterministic")
#' kardl_extract(kardl_model, what = "dependent_var")
#' kardl_extract(kardl_model, what = "independent_vars")
#' kardl_extract(kardl_model, what = "all_vars")
#' kardl_extract(kardl_model, what = "all_asym_vars")
#' kardl_extract(kardl_model, what = "indep_as_excluded")
#' kardl_extract(kardl_model, what = "indep_al_excluded")
#' kardl_extract(kardl_model, what = "short_run_vars")
#' kardl_extract(kardl_model, what = "long_run_vars")
#' kardl_extract(kardl_model, what = "shortrun_length")
#' kardl_extract(kardl_model, what = "lag_rows_number")
#' kardl_extract(kardl_model, what = "model_type")
#' # kardl_extract(kardl_model, what = "data")
#' kardl_extract(kardl_model, what = "start_time")
#' kardl_extract(kardl_model, what = "end_time")
#' kardl_extract(kardl_model, what = "span")
#' kardl_extract(kardl_model, what = "opt_lag")
#' kardl_extract(kardl_model, what = "lag_criteria")
#' kardl_extract(kardl_model, what = "all_cr_lags")
#' kardl_extract(kardl_model, what = "model_formula")
#' kardl_extract(kardl_model, what = "k")
#' kardl_extract(kardl_model, what = "n")
#'
#'
#' # Examples of extracting components from a kardl_mplier object
#' \donttest{
#' # This long-running example won't be tested by CRAN
#' m <- mplier(kardl_model, horizon = 40)
#' head(kardl_extract(m, what = "multipliers"))
#' kardl_extract(m, what = "omega")
#' head(kardl_extract(m, what = "lambda"))
#' kardl_extract(m, what = "horizon")
#'
#' # Examples of extracting components from a kardl_boot object
#' boot_results <- bootstrap(kardl_model, horizon = 40, replications = 2)
#' head(kardl_extract(boot_results, what = "multipliers"))
#' kardl_extract(boot_results, what = "level")
#' kardl_extract(boot_results, what = "replications")
#' kardl_extract(boot_results, what = "horizon")
#' }
#' # Examples of extracting components from a kardl_test object
#' test_results <- psst(kardl_model)
#' kardl_extract(test_results, what = "type")
#' kardl_extract(test_results, what = "case")
#' kardl_extract(test_results, what = "statistic")
#' kardl_extract(test_results, what = "method")
#' kardl_extract(test_results, what = "alternative")
#' kardl_extract(test_results, what = "data.name")
#' kardl_extract(test_results, what = "sample.size")
#' kardl_extract(test_results, what = "hypotheses")
#' kardl_extract(test_results, what = "var_names")
#' kardl_extract(test_results, what = "k")
#' kardl_extract(test_results, what = "n")
#' kardl_extract(test_results, what = "sig")
#' kardl_extract(test_results, what = "notes")
#'
#' # Examples of extracting components from a kardl_test_summary object
#' test_summary <- summary(test_results)
#' kardl_extract(test_summary, what = "statistic")
#' kardl_extract(test_summary, what = "case")
#' kardl_extract(test_summary, what = "variables")
#' kardl_extract(test_summary, what = "decision")
#' kardl_extract(test_summary, what = "hypotheses")
#' kardl_extract(test_summary, what = "numeric_decision")
#' kardl_extract(test_summary, what = "significance_level")
#' kardl_extract(test_summary, what = "critical_values")
#' kardl_extract(test_summary, what = "k")
#' kardl_extract(test_summary, what = "notes")
#'
#' # Examples of extracting components from a kardl_symmetric object
#' symmetry_results <- symmetrytest(kardl_model)
#' kardl_extract(symmetry_results, what = "long_wald_summary")
#' kardl_extract(symmetry_results, what = "long_hypotheses")
#' kardl_extract(symmetry_results, what = "short_wald_summary")
#' kardl_extract(symmetry_results, what = "short_hypotheses")
#' kardl_extract(symmetry_results, what = "long_wald_tests")
#' kardl_extract(symmetry_results, what = "short_wald_tests")
#' kardl_extract(symmetry_results, what = "vars")
#' kardl_extract(symmetry_results, what = "type")
#' kardl_extract(symmetry_results, what = "call")
#'
#' # Example of extracting specific components from symmetry test results
#' kardl_extract(symmetry_results,
#'   what = "short_wald_tests",
#'   variable = "PetrolPrice"
#' )
#' kardl_extract(symmetry_results,
#'   what = "long_wald_tests",
#'   variable = "PetrolPrice"
#' )
#' kardl_extract(symmetry_results,
#'   what = "long_hypotheses",
#'   variable = "PetrolPrice"
#' )
#' kardl_extract(symmetry_results,
#'   what = "short_hypotheses",
#'   variable = "PetrolPrice"
#' )
#' kardl_extract(symmetry_results, what = "short_hypotheses", component = "H0")
#' kardl_extract(symmetry_results, what = "short_hypotheses", component = "H1")
#'
#' kardl_extract(symmetry_results,
#'   what = "short_hypotheses",
#'   variable = "PetrolPrice", component = "H0"
#' )
#'
#' @export
kardl_extract <- function(
  kardl_object, what, variable = NULL, component = NULL
) {
  UseMethod("kardl_extract")
}

#' Default method for kardl_extract()
#'
#' @param kardl_object An object for which no `kardl_extract()` method is
#' available.
#'
#' @return
#' This function does not return a value. It stops with an informative error.
#'
#' @export
#' @method kardl_extract default
#' @noRd
kardl_extract.default <- function(
  kardl_object, what, variable = NULL, component = NULL
) {
  stop(
    "No kardl_extract() method for objects of class: ",
    paste(class(kardl_object), collapse = ", "),
    call. = FALSE
  )
}

#' Extract components from dynamic multiplier objects
#'
#' Extracts selected components from a `kardl_mplier` object returned by
#' [mplier()].
#'
#' @param kardl_object An object of class `kardl_mplier`.
#' @param what A character string specifying the component to extract.
#'   One of:
#'   \itemize{
#'     \item **multipliers**: estimated dynamic multipliers.
#'     \item **omega**: adjustment-related multiplier matrix.
#'     \item **lambda**: short-run coefficient matrix used in multiplier
#'     construction.
#'     \item **horizon**: multiplier horizon.
#'   }
#'
#' @return
#' The selected component from the `kardl_mplier` object.
#'
#' @examples
#' kardl_model <- kardl(DriversKilled ~ PetrolPrice, data = Seatbelts)
#' m <- mplier(kardl_model, horizon = 40)
#'
#' head(kardl_extract(m, what = "multipliers"))
#' kardl_extract(m, what = "horizon")
#' @export
#' @method kardl_extract kardl_mplier
#' @noRd
kardl_extract.kardl_mplier <- function(
  kardl_object, what, variable = NULL, component = NULL
) {
  what <- match.arg(what, c(
    "multipliers",
    "omega",
    "lambda",
    "horizon"
  ))

  switch(what,
    multipliers = kardl_object$mpsi,
    omega = kardl_object$omega,
    lambda = kardl_object$lambda,
    horizon = kardl_object$horizon
  )
}

#' Extract components from bootstrapped multiplier objects
#'
#' Extracts selected components from a `kardl_boot` object returned by
#' [bootstrap()].
#'
#' @param kardl_object An object of class `kardl_boot`.
#' @param what A character string specifying the component to extract.
#'   One of:
#'   \itemize{
#'     \item **multipliers**: bootstrapped dynamic multiplier results.
#'     \item **level**: confidence level used for bootstrap intervals.
#'     \item **replications**: number of bootstrap replications.
#'     \item **horizon**: multiplier horizon.
#'   }
#'
#' @return
#' The selected component from the `kardl_boot` object.
#'
#' @export
#' @method kardl_extract kardl_boot
#' @noRd
kardl_extract.kardl_boot <- function(
  kardl_object,
  what, variable = NULL, component = NULL
) {
  what <- match.arg(
    what,
    c(
      "multipliers",
      "level",
      "replications",
      "horizon"
    )
  )

  switch(what,
    multipliers = kardl_object$mpsi,
    level = kardl_object$level,
    replications = kardl_object$replications,
    horizon = kardl_object$horizon
  )
}
#' Extract components from kardl test objects
#'
#' Extracts selected components from a `kardl_test` object, such as objects
#' produced by cointegration and bounds-testing functions.
#'
#' @param kardl_object An object of class `kardl_test`.
#' @param what A character string specifying the component to extract.
#'   One of:
#'   \itemize{
#'     \item **type**: type of test.
#'     \item **case**: deterministic case used in the test.
#'     \item **statistic**: test statistic.
#'     \item **method**: test method description.
#'     \item **alternative**: alternative hypothesis.
#'     \item **data.name**: name of the data/model object.
#'     \item **sample.size**: effective sample size.
#'     \item **var_names**: variables involved in the test.
#'     \item **k**: number of regressors entering the bounds test.
#'     \item **n**: sample size, when available.
#'     \item **sig**: significance level or significance-related output.
#'     \item **notes**: additional notes.
#'   }
#'
#' @return
#' The selected component from the `kardl_test` object.
#'
#' @export
#' @method kardl_extract kardl_test
#' @noRd
kardl_extract.kardl_test <- function(
  kardl_object,
  what, variable = NULL, component = NULL
) {
  what <- match.arg(
    what,
    c(
      "type", "case", "statistic", "method", "alternative", "data.name",
      "sample.size", "hypotheses", "var_names", "k", "n", "sig", "notes"
    )
  )

  switch(what,
    type = kardl_object$type,
    case = kardl_object$case,
    statistic = kardl_object$statistic,
    method = kardl_object$method,
    alternative = kardl_object$alternative,
    data.name = kardl_object$data.name,
    sample.size = kardl_object$sample.size,
    hypotheses = kardl_object$hypotheses,
    var_names = kardl_object$var_names,
    k = kardl_object$k,
    n = kardl_object$n,
    sig = kardl_object$sig,
    notes = kardl_object$notes
  )
}

#' Extract components from kardl test summaries
#'
#' Extracts selected components from a `kardl_test_summary` object.
#'
#' @param kardl_object An object of class `kardl_test_summary`.
#' @param what A character string specifying the component to extract.
#'   One of:
#'   \itemize{
#'     \item **statistic**: test statistic.
#'     \item **case**: textual description of the deterministic case.
#'     \item **variables**: variables included in the tested restriction.
#'     \item **decision**: textual test decision.
#'     \item **numeric_decision**: numeric encoding of the test decision.
#'     \item **significance_level**: significance level used for the decision.
#'     \item **critical_values**: lower and upper critical-value bounds.
#'     \item **k**: number of regressors entering the bounds test.
#'     \item **notes**: additional notes.
#'   }
#'
#' @return
#' The selected component from the `kardl_test_summary` object.
#'
#' @export
#' @method kardl_extract kardl_test_summary
#' @noRd
kardl_extract.kardl_test_summary <- function(
  kardl_object,
  what, variable = NULL, component = NULL
) {
  what <- match.arg(what, c(
    "statistic",
    "case",
    "variables",
    "decision",
    "hypotheses",
    "numeric_decision",
    "significance_level",
    "critical_values",
    "k",
    "notes"
  ))

  switch(what,
    statistic = kardl_object$statistic,
    case = kardl_object$case_txt,
    variables = kardl_object$var_names,
    decision = kardl_object$decision,
    numeric_decision = kardl_object$numeric_decision,
    significance_level = kardl_object$significance_level,
    hypotheses = kardl_object$hypotheses,
    critical_values = kardl_object$cr_vals,
    k = kardl_object$k,
    notes = kardl_object$notes
  )
}

#' Extracts selected model, lag-selection, time-series, and estimation
#' components from a fitted `kardl_lm` object returned by [kardl()] or related
#' model-fitting functions.
#'
#' @param kardl_object An object of class `kardl_lm`.
#' @param what A character string specifying the component to extract.
#'   One of:
#'   \itemize{
#'     \item **data_ts_info**: time-series information about the input data.
#'     \item **data_is_ts**: whether the input data is a time series.
#'     \item **data_class**: class of the input data.
#'     \item **data_start**: starting time of the input data.
#'     \item **data_end**: ending time of the input data.
#'     \item **data_frequency**: frequency of the input data.
#'     \item **data_deltat**: delta time of the input data.
#'     \item **data_tsp**: time-series properties of the input data.
#'     \item **data_time**: time index of the input data.
#'     \item **no_constant**: whether the model excludes an intercept.
#'     \item **trend**: trend specification.
#'     \item **asym_long_vars**: variables with long-run asymmetry.
#'     \item **asym_short_vars**: variables with short-run asymmetry.
#'     \item **deterministic**: deterministic regressors.
#'     \item **dependent_var**: dependent variable.
#'     \item **independent_vars**: independent variables.
#'     \item **all_vars**: all variables used in the model.
#'     \item **all_asym_vars**: all asymmetric variables.
#'     \item **indep_as_excluded**: short-run asymmetric variables excluded from
#'      the linear set.
#'     \item **indep_al_excluded**: long-run asymmetric variables excluded from
#'     the linear set.
#'     \item **short_run_vars**: short-run variables.
#'     \item **long_run_vars**: long-run variables.
#'     \item **shortrun_length**: number of short-run terms.
#'     \item **lag_rows_number**: number of rows lost due to lag construction.
#'     \item **model_type**: model type.
#'     \item **data**: data used internally after preparation.
#'     \item **start_time**: starting time of the estimation sample.
#'     \item **end_time**: ending time of the estimation sample.
#'     \item **span**: time span of the estimation sample.
#'     \item **opt_lag**: selected optimal lag order.
#'     \item **lag_criteria**: lag-selection criterion values.
#'     \item **all_cr_lags**: all candidate lag combinations and criterion
#'     values.
#'     \item **model_formula**: model formula used in the estimation.
#'     \item **k**: number of estimated regressors or bounds-test regressors,
#'     depending on context.
#'     \item **n**: effective sample size.
#'   }
#'
#' @return
#' The selected component from the fitted `kardl_lm` object.
#'
#' @examples
#' kardl_model <- kardl(DriversKilled ~ PetrolPrice + drivers, data = Seatbelts)
#'
#' kardl_extract(kardl_model, what = "dependent_var")
#' kardl_extract(kardl_model, what = "independent_vars")
#' kardl_extract(kardl_model, what = "opt_lag")
#' kardl_extract(kardl_model, what = "n")
#'
#' # Extracting time-series information from the fitted model
#' kardl_extract(kardl_model, what = "data_ts_info")
#'
#' kardl_extract(kardl_model, what = "data_start")
#'
#' # Extracting model formula used in the estimation
#' kardl_extract(kardl_model, what = "model_formula")
#'
#' @export
#' @method kardl_extract kardl_lm
#' @noRd
#'

kardl_extract.kardl_lm <- function(
  kardl_object,
  what, variable = NULL, component = NULL
) {
  what <- match.arg(
    what,
    c(
      "data_ts_info", "data_is_ts", "data_class", "data_start", "data_end",
      "data_frequency", "data_deltat", "data_tsp", "data_time", "no_constant",
      "trend", "asym_long_vars", "asym_short_vars", "deterministic",
      "dependent_var", "independent_vars", "all_vars", "all_asym_vars",
      "indep_as_excluded", "indep_al_excluded", "short_run_vars",
      "long_run_vars", "shortrun_length", "lag_rows_number", "model_type",
      "data", "start_time", "end_time", "span", "opt_lag", "lag_criteria",
      "all_cr_lags", "model_formula", "k", "n"
    )
  )

  switch(what,
    data_ts_info = kardl_object$args_info$data_ts_info,
    data_is_ts = kardl_object$args_info$data_ts_info$data_is_ts,
    data_class = kardl_object$args_info$data_ts_info$data_class,
    data_start = kardl_object$args_info$data_ts_info$data_start,
    data_end = kardl_object$args_info$data_ts_info$data_end,
    data_frequency = kardl_object$args_info$data_ts_info$data_frequency,
    data_deltat = kardl_object$args_info$data_ts_info$data_deltat,
    data_tsp = kardl_object$args_info$data_ts_info$data_tsp,
    data_time = kardl_object$args_info$data_ts_info$data_time,
    no_constant = kardl_object$extracted_info$no_constant,
    trend = kardl_object$extracted_info$trend,
    asym_long_vars = kardl_object$extracted_info$asym_long_vars,
    asym_short_vars = kardl_object$extracted_info$asym_short_vars,
    deterministic = kardl_object$extracted_info$deterministic,
    dependent_var = kardl_object$extracted_info$dependent_var,
    independent_vars = kardl_object$extracted_info$independent_vars,
    all_vars = kardl_object$extracted_info$all_vars,
    all_asym_vars = kardl_object$extracted_info$all_asym_vars,
    indep_as_excluded = kardl_object$extracted_info$indep_as_excluded,
    indep_al_excluded = kardl_object$extracted_info$indep_al_excluded,
    short_run_vars = kardl_object$extracted_info$short_run_vars,
    long_run_vars = kardl_object$extracted_info$long_run_vars,
    shortrun_length = kardl_object$extracted_info$shortrun_length,
    lag_rows_number = kardl_object$extracted_info$lag_rows_number,
    model_type = kardl_object$extracted_info$model_type,
    data = kardl_object$extracted_info$data,
    start_time = kardl_object$time_info$start_time,
    end_time = kardl_object$time_info$end_time,
    span = kardl_object$time_info$span,
    opt_lag = kardl_object$lag_info$opt_lag,
    lag_criteria = kardl_object$lag_info$lag_criteria,
    all_cr_lags = kardl_object$lag_info$all_cr_lags,
    model_formula = kardl_object$est_info$model_formula,
    k = kardl_object$est_info$k,
    n = kardl_object$est_info$n
  )
}

#' Extracts selected components from symmetry test results
#'
#' This function provides a convenient interface for extracting specific
#' components from symmetry test results produced by the [symmetrytest()]
#' function. Users can specify whether they want to extract long-run, short-run,
#'  or both types of results, as well as specific variables of interest.
#'
#' @param kardl_object An object of class `kardl_symmetric`, typically returned
#'  by the [symmetrytest()] function.
#' @param what A character string specifying the component to extract. One of:
#'  \itemize{
#'  \item **long_wald_summary**: summary of long-run Wald test results.
#'  \item **long_hypotheses**: textual description of long-run null and
#'  alternative hypotheses.
#'  \item **short_wald_summary**: summary of short-run Wald test results.
#'  \item **short_hypotheses**: textual description of short-run null
#'  and alternative hypotheses.
#'  \item **long_wald_tests**: detailed long-run Wald test results.
#'  \item **short_wald_tests**: detailed short-run Wald test results.
#'  \item **vars**: variables included in the symmetry test.
#'  \item **type**: type of symmetry test.
#'  \item **call**: original function call that produced the object.
#'  }
#' @param variable An optional character string or vector specifying which
#' variable(s) to extract results for when `what` is set to "long_wald_tests",
#' "short_wald_tests", "long_hypotheses", or "short_hypotheses". If not
#' specified, results for all variables included in the symmetry test are
#' returned. If specified, must be one or more variable names that are included
#' in the symmetry test results.
#' @param component An optional character string specifying which component(s)
#' to extract when `what` is set to "long_hypotheses" or "short_hypotheses".
#' If not specified, both null and alternative hypotheses are returned. If
#' specified, must be one of "H0" (for null hypotheses) or "H1" (for alternative
#'  hypotheses).
#' @return
#'   The extracted component(s) from the symmetry test results. The structure of
#'   the returned object depends on the specified `component` and `variable`
#'   arguments. If `component` is "both", a list containing both long-run
#'   and short-run results is returned. If `variable` is specified, only the
#'   results for that variable are returned.
#' @examples
#' # Example usage:
#' my_asymmetry_model <- kardl(DriversKilled ~ asym(PetrolPrice + drivers),
#'   data = Seatbelts,
#'   max_lags = 1
#' )
#' symmetry_results <- symmetrytest(my_asymmetry_model)
#'
#' kardl_extract(symmetry_results, what = "short_wald_summary")
#' kardl_extract(symmetry_results, what = "long_wald_summary")
#' kardl_extract(symmetry_results, what = "vars")
#' kardl_extract(symmetry_results, what = "type")
#' kardl_extract(symmetry_results, what = "call")
#'
#' kardl_extract(symmetry_results, what = "short_wald_tests")
#' kardl_extract(symmetry_results,
#'   what = "short_wald_tests",
#'   variable = "PetrolPrice"
#' )
#'
#' kardl_extract(symmetry_results, what = "long_wald_tests")
#' kardl_extract(symmetry_results,
#'   what = "long_wald_tests",
#'   variable = "PetrolPrice"
#' )
#'
#' kardl_extract(symmetry_results, what = "long_hypotheses")
#' kardl_extract(symmetry_results,
#'   what = "long_hypotheses",
#'   variable = "PetrolPrice"
#' )
#'
#' kardl_extract(symmetry_results, what = "short_hypotheses")
#' kardl_extract(symmetry_results,
#'   what = "short_hypotheses",
#'   variable = "PetrolPrice"
#' )
#'
#' kardl_extract(symmetry_results, what = "short_hypotheses", component = "H0")
#' kardl_extract(symmetry_results, what = "short_hypotheses", component = "H1")
#'
#' kardl_extract(symmetry_results,
#'   what = "short_hypotheses",
#'   variable = "PetrolPrice", component = "H0"
#' )
#'
#' @export
#' @method kardl_extract kardl_symmetric
#' @noRd
#'
kardl_extract.kardl_symmetric <- function(
  kardl_object,
  what,
  variable = NULL, component = NULL
) {
  what <- match.arg(
    what,
    c(
      "short_wald_summary", "long_wald_summary", "vars", "type", "call",
      "short_wald_tests", "long_wald_tests",
      "long_hypotheses", "short_hypotheses"
    )
  )
  if (what %in% c(
    "short_wald_summary", "long_wald_summary", "vars", "type", "call"
  )) {
    return(switch(what,
      short_wald_summary = kardl_object$short_wald_summary,
      long_wald_summary = kardl_object$long_wald_summary,
      vars = kardl_object$vars,
      type = kardl_object$type,
      call = kardl_object$call
    ))
  }

  if (what %in% c("short_wald_tests", "long_wald_tests")) {
    out <- switch(what,
      short_wald_tests = kardl_object$short_wald_tests,
      long_wald_tests = kardl_object$long_wald_tests
    )
    if (!is.null(variable)) {
      missing_vars <- setdiff(variable, names(out))

      if (length(missing_vars) > 0) {
        stop(
          "Variable(s) not found: ",
          paste(missing_vars, collapse = ", "),
          ". Available variables: ",
          paste(names(out), collapse = ", "),
          call. = FALSE
        )
      }
      if (length(variable) == 1) {
        out <- out[[variable]]
      } else {
        out <- out[variable]
      }
    }
    return(out)
  }


  if (what %in% c("long_hypotheses", "short_hypotheses")) {
    out <- switch(what,
      long_hypotheses = kardl_object$long_hypotheses,
      short_hypotheses = kardl_object$short_hypotheses
    )
    if (!is.null(variable)) {
      missing_vars <- setdiff(variable, names(out$H0))

      if (length(missing_vars) > 0) {
        stop(
          "Variable(s) not found: ",
          paste(missing_vars, collapse = ", "),
          ". Available variables: ",
          paste(names(out$H0), collapse = ", "),
          call. = FALSE
        )
      }

      out <- list(
        H0 = out$H0[variable],
        H1 = out$H1[variable]
      )
    }

    if (!is.null(component)) {
      component <- match.arg(toupper(component), c("H0", "H1"))
      out <- setNames(list(out[[component]]), component)
    }
    class(out) <- c("kardl_hypotheses", "list")
    out
  }
}

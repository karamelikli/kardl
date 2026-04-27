#' srr_stats
#'
#' Package-specific compliance statements for the srr statistical software standards.
#'
#' @srrstatsVerbose TRUE
#'
#' @srrstats {G1.0} The package documentation and manuscript cite the ARDL bounds-testing and nonlinear ARDL literature on which the implemented estimators, asymmetry decomposition, bounds tests, and dynamic multipliers are based.
#' @srrstats {G1.1} Statistical Software should document whether the algorithm(s) it implements are: the first implementation of a novel algorithm; or the first implementation within R of an algorithm which has previously been implemented in other languages or contexts; or an improvement on other implementations of similar algorithms in R.
#'
#' The **kardl** package implements several methodological innovations and extensions
#' that are not available in other R packages for ARDL/NARDL modelling:
#'
#' * **differentAsymLag** option: Allows different lag orders for the positive and negative
#'   partial sum decompositions in NARDL models. This provides greater flexibility
#'   compared to standard symmetric lag restrictions in existing packages.
#'
#' * **Narayan test** (`narayan()`): Small-sample cointegration test optimised for
#'   cases 2–5, following Narayan (2005). While the test statistic itself exists in
#'   the literature, its dedicated, user-friendly implementation with automatic
#'   critical value handling for small samples is unique within the R ecosystem
#'   for ARDL/NARDL workflows.
#'
#' * **Symmetry test** (`symmetrytest()`): Dedicated test for long-run and short-run
#'   symmetry in NARDL models.
#'
#' * **Bootstrap confidence intervals for dynamic multipliers** (`bootstrap()` and
#'   associated `plot.kardl_mplier()` methods): Provides robust uncertainty quantification
#'   around dynamic multipliers through resampling. While bootstrap methods exist in
#'   the broader econometric literature, this integrated implementation within a
#'   complete ARDL/NARDL workflow (including asymmetric multipliers) represents
#'   a novel and practical contribution in R.
#'
#' * Additional unique features include flexible formula parsing, multiple model
#'   selection criteria (AIC, BIC, AICc, HQ, and user-defined), and comprehensive
#'   post-estimation tools tailored specifically for asymmetric ARDL analysis.
#'
#' These extensions go beyond the standard Shin et al. (2014) NARDL framework
#' and improve upon existing R implementations (e.g., `nardl`, `ardl.nardl`,
#' `dynamac`) by offering greater customisation and additional diagnostic/testing
#' capabilities.
#'
#' **Prior Art / Comparable Implementations**
#' - Shin, Y., Yu, B., & Greenwood-Nimmo, M. (2014). Modelling Asymmetric Cointegration
#'   and Dynamic Multipliers in a Nonlinear ARDL Framework.
#' - Narayan, P. K. (2005). The saving and investment nexus for China: evidence from
#'   cointegration tests.
#' - Other R packages: `nardl`, `ardl.nardl`, `dynamac`, and more recent packages
#'   such as `ardlverse` and `bootCT` (which focus on bootstrap bounds testing
#'   rather than multiplier uncertainty in NARDL).
#'
#' Users are encouraged to compare results with these packages where overlapping
#' functionality exists.
#' @srrstats {G1.2} The README, NEWS file, CONTRIBUTING.md, and package website describe the current development state, recent releases, and intended future maintenance of the package.
#' @srrstats {G1.3} Statistical terms such as ARDL, NARDL, ECM, bounds testing, short-run asymmetry, long-run asymmetry, and dynamic multipliers are defined in function documentation and vignettes.
#' @srrstats {G1.4} User-facing functions and S3 methods are documented with `roxygen2`, and the generated Rd files are kept under version control through the package documentation workflow.
#' @srrstats {G1.4a} Internal helper functions are documented with roxygen comments where clarification is needed and are hidden from the public help index with `@noRd` or marked as internal.
#' @srrstats {G1.5} Examples, tests, and vignettes include executable code using package data so that users can reproduce the estimation and multiplier outputs reported in the documentation.
#' @srrstats {G1.6} The related-software discussion compares the package with alternative ARDL and NARDL implementations in other software environments and explains the reproducibility advantages of the R implementation.
#' @srrstats {G2.0} Main modelling functions validate the expected scalar and vector inputs, including model formulae, lag choices, criteria, horizon values, and object classes.
#' @srrstats {G2.0a} Function documentation explains length expectations for important arguments such as formula input, lag structures, selected criteria, and dynamic multiplier horizons.
#' @srrstats {G2.1} The package checks that model input is supplied in the expected data and formula form and that S3 methods receive compatible `kardl` model objects.
#' @srrstats {G2.1a} Documentation describes the expected data structure for variables used in time-series regression and clarifies that model variables should be numeric where estimation requires numeric series.
#' @srrstats {G2.2} Model specifications restrict dependent-variable input to a single response variable and process independent variables through the formula parser.
#' @srrstats {G2.3} Character-valued options are limited to recognised choices through package option handlers and argument matching routines.
#' @srrstats {G2.3a} Selection arguments such as information criteria and asymmetry options are matched against predefined acceptable values.
#' @srrstats {G2.3b} Formula terms and asymmetry constructors are processed consistently so that documented model syntax is interpreted as intended.
#' @srrstats {G2.4} Data and formula-processing routines standardise model inputs before estimation so that subsequent estimation functions work with a consistent internal representation.
#' @srrstats {G2.4a} Integer-valued quantities such as lags and horizons are treated as discrete counts before they are used to construct lagged regressors or multiplier paths.
#' @srrstats {G2.4b} Numeric model variables are passed to estimation routines as numeric vectors or matrix columns suitable for ordinary least squares and post-estimation algebra.
#' @srrstats {G2.4c} Variable names and parsed formula terms are handled as character vectors when constructing lagged-variable names and asymmetric components.
#' @srrstats {G2.4d} The package does not require factor-valued model variables for ARDL estimation, and categorical effects should be supplied through suitable deterministic or dummy variables.
#' @srrstats {G2.4e} Factor inputs are not part of the core estimation algorithm; users are expected to provide numeric series or explicit dummy variables when categorical controls are required.
#' @srrstats {G2.5} The modelling interface is designed for numeric time-series regressors rather than ordered or unordered factor responses.
#' @srrstats {G2.6} One-dimensional variables selected through the formula are extracted and aligned through the model-preparation routines before lagged terms are generated.
#' @srrstats {G2.7} The modelling functions operate on standard tabular data supplied by users, including data frames containing the time-series variables referenced in the formula.
#' @srrstats {G2.8} Initial preprocessing converts the user formula and data into a common internal structure containing dependent variables, independent variables, lag information, and generated regressors.
#' @srrstats {G2.9} The package constructs generated variable names for lagged, differenced, and asymmetric components in a transparent way so users can inspect the resulting model object.
#' @srrstats {G2.10} Variables are selected by name from the supplied data through formula parsing rather than by position-dependent assumptions.
#' @srrstats {G2.11} The package tests standard data-frame inputs used in examples and vignettes and processes columns required by the model formula.
#' @srrstats {G2.12} The package is intended for numeric time-series columns and does not treat list columns as valid model variables for estimation.
#' @srrstats {G2.13} Missing observations introduced by lagging and differencing are handled during model-frame construction before estimation is performed.
#' @srrstats {G2.14} Missing data behaviour follows the documented estimation workflow, where observations unavailable after lag construction are excluded from the estimation sample.
#' @srrstats {G2.14a} Invalid model inputs that cannot be transformed into an estimable ARDL specification generate errors before post-estimation routines are run.
#' @srrstats {G2.14b} The estimation workflow removes structurally missing lagged observations created by differencing and lag construction.
#' @srrstats {G2.14c} The package does not impute missing time-series observations because imputation would change the dynamic structure of the fitted ARDL model.
#' @srrstats {G2.15} Estimation and post-estimation calculations are based on the cleaned model object rather than passing unprocessed missing values to lower-level numeric routines.
#' @srrstats {G2.16} Non-finite values are not treated as valid observations for the fitted regression sample and should be removed or corrected before estimation.
#' @srrstats {G3.0} Numerical comparisons in tests and post-estimation checks use tolerance-based expectations where exact floating-point equality would be inappropriate.
#' @srrstats {G3.1} Long-run multiplier inference uses the variance-covariance matrix of the fitted model object, allowing users to work with the covariance structure attached to the estimation result.
#' @srrstats {G3.1a} Multiplier documentation describes standard-error calculation from the fitted model covariance matrix and examples show how users can inspect the resulting estimates.
#' @srrstats {G5.0} Tests and examples use the package example data and fixed model specifications with known expected structure.
#' @srrstats {G5.1} The example data used in documentation and tests are included with the package so users can reproduce examples and test calculations.
#' @srrstats {G5.2} The test suite exercises representative successful calls and selected error conditions for model estimation, multiplier calculation, and S3 methods.
#' @srrstats {G5.2a} Diagnostic messages are written to identify the specific invalid input or unsupported object class encountered by the user.
#' @srrstats {G5.2b} Tests include error expectations for incompatible object classes and invalid post-estimation inputs.
#' @srrstats {G5.3} Tests verify that key returned objects, including fitted model components and multiplier matrices, have the expected dimensions, names, and classes.
#' @srrstats {G5.4} Correctness tests compare selected outputs against expected structural properties of ARDL and NARDL models, including lag information, long-run coefficients, and multiplier matrices.
#' @srrstats {G5.4a} Where exact external reference values are not available, tests use simple deterministic cases and expected object properties to verify implementation behaviour.
#' @srrstats {G5.4b} Validation work compares package results with established ARDL and NARDL software workflows where comparable outputs are available.
#' @srrstats {G5.4c} Published ARDL and NARDL formulae provide the basis for the implemented coefficient transformations and dynamic multiplier recursion.
#' @srrstats {G5.5} Tests involving simulated or random inputs set a random seed before execution.
#' @srrstats {G5.6} Parameter-recovery checks are implemented through deterministic model examples and comparisons of estimated quantities with expected algebraic relationships.
#' @srrstats {G5.6a} Numeric tests use tolerances where floating-point arithmetic or regression estimation makes exact equality inappropriate.
#' @srrstats {G5.6b} The core estimation functions are deterministic for a fixed data set and model specification, so repeated seeds are not required for standard tests.
#' @srrstats {G5.7} Tests examine behaviour across different model specifications, including linear, short-run asymmetric, long-run asymmetric, and fully asymmetric cases.
#' @srrstats {G5.8} Edge-condition tests check that invalid object classes and unsupported inputs produce clear errors rather than silent failures.
#' @srrstats {G5.8a} Zero-length or insufficient model inputs are rejected through the model-preparation and estimation checks.
#' @srrstats {G5.8b} Unsupported non-numeric model variables are not valid inputs for the regression calculations and are expected to fail before estimation.
#' @srrstats {G5.8c} Degenerate input cases are handled through the underlying regression and model-validation workflow.
#' @srrstats {G5.8d} Model specifications outside the estimable sample size or lag structure are rejected by estimation and lag-selection routines.
#' @srrstats {G5.9} The \code{bootstrap()} function tests for expected stochastic behaviour: confidence intervals are stable under both trivial noise perturbations (G5.9a) and different random seeds (G5.9b), as verified in \code{tests/testthat/test-bootstrap-noise.R}.
#' @srrstats {G5.9a} Adding noise at the scale of \code{.Machine$double.eps} to input data does not meaningfully change the dynamic multiplier estimates or bootstrap confidence intervals produced by \code{bootstrap()}.
#' @srrstats {G5.9b} The \code{bootstrap()} function accepts a \code{seed} argument passed to \code{set.seed()} before resampling; running under different random seeds does not meaningfully change results, and the same seed reproduces identical output, as tested in \code{tests/testthat/test-bootstrap-noise.R}.
#' @srrstats {TS1.0} The package accepts time-ordered regression data and constructs lagged and differenced variables internally for ARDL and NARDL estimation.
#' @srrstats {TS1.1} Function documentation and vignettes describe the accepted input form for time-series variables used in `kardl` model formulae.
#' @srrstats {TS1.2} Model-preparation routines check that the supplied variables can be used to construct the lagged and differenced terms required by the specification.
#' @srrstats {TS1.3} The package uses a central preparation workflow to parse formulae, construct lagged variables, and return a uniform internal model data structure.
#' @srrstats {TS1.4} Observational order is preserved during lag and difference construction so that the dynamic structure corresponds to the input time ordering.
#' @srrstats {TS1.5} Lag construction assumes that rows are supplied in the intended chronological order, and examples document this ordering requirement.
#' @srrstats {TS1.6} Users are expected to sort data before estimation; violations of chronological ordering would change the constructed lags and therefore the estimated dynamic model.
#' @srrstats {TS1.8} The package works with ordered observations rather than calendar arithmetic, so period interpretation is determined by the frequency and ordering of the user-supplied data.
#' @srrstats {TS2.0} ARDL estimation assumes a regular ordered sequence after preprocessing; missing observations created by lagging are handled explicitly in the model sample.
#' @srrstats {TS2.1} Missing-data handling is documented as part of model preparation and estimation rather than as a separate imputation system.
#' @srrstats {TS2.1a} Inputs that cannot form an estimable model after missing-value handling generate errors through the estimation workflow.
#' @srrstats {TS2.1b} Structurally missing observations caused by lagging and differencing are removed from the fitted sample.
#' @srrstats {TS2.1c} The package does not impute missing time-series observations because that would impose additional assumptions not inherent in ARDL estimation.
#' @srrstats {TS2.2} The documentation discusses stationarity requirements in the context of ARDL bounds testing and the usual I(0) and I(1) integration-order framework.
#' @srrstats {TS2.3} Vignettes and function documentation explain that ARDL bounds testing is intended for variables integrated of order zero or one, not for I(2) processes.
#' @srrstats {TS2.4} Users are expected to assess integration order before applying bounds-test interpretations, and the documentation states these modelling assumptions.
#' @srrstats {TS2.4a} The package reports model diagnostics and test results to help users assess the fitted dynamic specification.
#' @srrstats {TS2.4b} Users can transform variables before modelling, including differencing, logs, or asymmetric partial sums when appropriate for the research design.
#' @srrstats {TS2.5} The package does not compute autocovariance matrices as primary outputs; lag order and model sample information are stored in the fitted object.
#' @srrstats {TS2.6} The package does not compute autocovariance matrices with physical units; time interpretation follows the user's ordered data.
#' @srrstats {TS4.0} Fitted models and post-estimation results are returned as classed objects with dedicated print, summary, plot, and extraction methods.
#' @srrstats {TS4.0a} The package returns model-specific objects rather than converting outputs back to the original data class.
#' @srrstats {TS4.0b} Return values use explicit classes such as `kardl_lm`, `kardl_mplier`, and related summary classes.
#' @srrstats {TS4.1} Units are not inferred or altered by the package; coefficient and multiplier interpretation follows the units of the user-supplied variables.
#' @srrstats {TS4.2} Documentation describes the list components and classes returned by fitted model, long-run, test, and multiplier functions.
#' @srrstats {TS4.3} Dynamic multiplier outputs include the horizon index so that multiplier paths can be interpreted over the modelled time steps.
#' @srrstats {TS4.4} The package does not automatically transform forecast data; any transformations applied by the user remain part of the model interpretation.
#' @srrstats {TS4.5} The package documents coefficient and multiplier interpretation rather than providing automatic back-transformation of user-transformed data.
#' @srrstats {TS4.5a} Users who transform variables before estimation are responsible for interpreting or back-transforming results according to their research design.
#' @srrstats {TS4.5b} Examples show estimation and multiplier calculation on the scale of the variables supplied to the model.
#' @srrstats {TS4.5c} Limitations of interpreting transformed variables are discussed through the model documentation and vignettes.
#' @srrstats {TS5.0} The package implements plot methods for classed outputs, including long-run results and dynamic multiplier objects.
#' @srrstats {TS5.1} Dynamic multiplier plots label the horizontal axis as the horizon or time-step index.
#' @srrstats {TS5.2} The horizon or time-step variable is plotted on the horizontal axis in dynamic multiplier visualisations.
#' @srrstats {TS5.3} Multiplier plots display the horizon index used in the model output, with interpretation tied to the data frequency supplied by the user.
#' @srrstats {TS5.5} Plot methods operate on computed model and multiplier outputs after missing-value handling in the fitted object.
#' @srrstats {TS5.7} Dynamic multiplier plots display the computed response path from the fitted model over the selected horizon.
#' @srrstats {TS5.8} Plot methods distinguish multiplier components by variable and shock direction where asymmetric effects are present.
#' @noRd
NULL

#' NA_standards
#'
#' Non-applicable srr standards for kardl.
#'
#' @srrstatsNA {G4.0} The package does not write statistical outputs to local files; users receive R objects that can be saved with standard R tools if needed.
#' @srrstatsNA {G5.10} The package test suite does not require long-running extended tests beyond the standard `testthat` workflow.
#' @srrstatsNA {G5.11} The package tests use included data and do not require external large data downloads.
#' @srrstatsNA {G5.11a} The package does not download external test data, so failed downloads are not part of the testing workflow.
#' @srrstatsNA {G5.12} No special platform, memory, runtime, or manual-inspection requirements are needed for extended tests.
#' @srrstatsNA {TS1.7} The package does not use `units` objects; time-series interpretation is based on ordered observations and user-supplied variable scales.
#' @srrstatsNA {TS3.0} The package estimates ARDL and NARDL models and computes dynamic multipliers; it does not implement out-of-sample forecast intervals.
#' @srrstatsNA {TS3.1} Forecast-error widening tests are not applicable because the package does not produce forecast distributions.
#' @srrstatsNA {TS3.2} Forecast-error drivers are not documented because forecasting is outside the package scope.
#' @srrstatsNA {TS3.3} Forecast trimming is not applicable because the package does not generate forecasts with error margins.
#' @srrstatsNA {TS3.3a} The package does not provide forecast-error examples or trimming procedures.
#' @srrstatsNA {TS3.3b} The package does not include explicit forecast trimming because it does not implement forecasting.
#' @srrstatsNA {TS4.6} The package does not return forecast distributions or prediction intervals.
#' @srrstatsNA {TS4.6a} Distribution objects are not applicable because the package does not implement probabilistic forecasting.
#' @srrstatsNA {TS4.6b} Forecast means and forecast standard errors are not returned because forecasting is outside the package scope.
#' @srrstatsNA {TS4.6c} General forecast-error measures are not returned because the package is focused on estimation and post-estimation multipliers.
#' @srrstatsNA {TS4.7} Distinguishing fitted and forecast values is not applicable because the package does not return forecast objects.
#' @srrstatsNA {TS4.7a} Forecast-only outputs are not produced by the package.
#' @srrstatsNA {TS4.7b} Separate model and forecast output lists are not produced by the package.
#' @srrstatsNA {TS4.7c} Combined observed and forecast data objects are not produced by the package.
#' @srrstatsNA {TS5.4} The package does not implement frequency-domain or spectral visualisation, so frequency-axis conventions are not applicable.
#' @srrstatsNA {TS5.6} Forecast distributional limits are not plotted because the package does not implement forecast plots.
#' @noRd
NULL

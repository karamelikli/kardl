# kardl 2.0.1
## New Features

* Added S3 methods for major `kardl` classes, improving integration with
  standard R workflows.

* Added `summary()` methods for:
  - `kardl_test`
  - `kardl_symmetric`

* Added dedicated print methods for:
  - `kardl_decision`
  - `kardl_mplier`

* Introduced the new generic accessor function `kardl_extract()`
  to retrieve documented components from `kardl` objects without
  relying on internal object structures.

* Added extraction methods for:
  - `kardl_mplier`
  - `kardl_symmetric`
  - `kardl_test_summary`

  allowing users to access multipliers, hypotheses, Wald test results,
  critical values, decisions, and other model components through a
  unified interface.

## Improvements

* Standardized object output across `kardl` classes using S3 methods.

* Improved consistency of hypothesis reporting and decision summaries
  for cointegration and symmetry tests.

* Enhanced printing of critical values, test statistics, and multiplier
  objects to better respect console width and improve readability.


```r
  kardl(formula, data)
````


# kardl 1.3.2
## Changes in this version

* Renamed `combineVarTypes()` to `parse_model_specs()`.
    * **Lifecycle Update**: While previously internal, `parse_model_specs()` is now exported to support developers using `kardl` as a dependency for their own packages.
* Refactored `parse_model_specs()` (formerly `combineVarTypes`):
    * Several input validation and initial check procedures have been moved to the internal utility function `CheckInputs()`.
    * `CheckInputs()` remains an internal function and is not exported.
    
# kardl 1.3.1
## Changes in this version

- **autotest compliance:** Fixed errors and warnings in `autotest` example parsing (e.g., `subscript out of bounds`, `recursive indexing failed`). Updated example structures to be atomic and assignment-based, improving automated test generation and reproducibility.
- **seed argument added to bootstrap:** The `bootstrap()` function now includes an optional `seed` argument, allowing users to set a random seed for reproducibility of bootstrap results.
- **kardl_reset updated:** The `kardl_reset()` function has been updated. Exclude argument is now optional and defaults to `NULL`, allowing users to reset all settings except those explicitly specified.
- **Print method improvements:** Standard output handling in print methods has been revised. Functions now rely on standard R messaging mechanisms instead of direct console output calls, improving compatibility with CRAN checks and allowing users to control output behavior more effectively.
- **Documentation fixes:** Corrected Rd syntax issues such as mismatched braces, non-ASCII characters, and inconsistent list environments (`\itemize{}` vs `\describe{}`). Improved clarity and consistency across help pages.
- **Examples refactored:** Rewrote examples to avoid fragile constructs such as chained pipelines (`%>%`) without proper imports, standalone expressions, and state-dependent workflows. Examples now follow a minimal and deterministic structure.
- **Function interface refinement:** Updated function signatures (e.g., removal of nonstandard argument names such as `.`) to improve compatibility with automated tools and standard R practices.
- **Dependency handling:** Improved handling of suggested packages in examples using `@examplesIf`, ensuring optional dependencies (e.g., `magrittr`, `dplyr`) do not break checks.
- **Minor fixes and cleanup:** General code cleanup, improved formatting, and consistency updates across the package.


# kardl 1.3.0
## Changes in this version
- **symmetry test:** The symmetry test has been updated to include a new method for calculating the test statistic, Chi-squared, in addition to the existing t-test method.
- **output update:** Colors in summary output have been removed to improve readability and ensure that the output is clear and accessible to all users, regardless of their display settings or preferences.
- **lower and upper limits of confidence intervals:** The lower and upper limits of confidence intervals in the summary output have been updated to be more accurate and reflective of the underlying data, providing users with more reliable information for inference.
- **settings update:** The settings for the model were added to kardl objects, allowing users to easily access and review the configuration of their models. 




# kardl 1.2.1

## Changes in this version
- **Documentation update:** The documentation for psst, pssf, narayana, and ECM methods has been updated to include the delta method for long-term multipliers. This addition provides users with a more comprehensive understanding of the estimation process and the interpretation of long-term effects.
- **Rename variables names in mpliers:** The variable names in the multipliers function have been renamed to be more descriptive and consistent with the rest of the package. This change enhances readability and helps users better understand the output of the function.
- **Updating readme:** The README file has been updated to reflect the changes in the documentation and variable names, ensuring that users have access to the most current information about the package's features and usage.


# kardl 1.2.0

## Changes in this version
- **t Upper value fixed:** The upper value of the bound for the t statistic in the `summary()` output has been corrected.
- **no intercept in ECM:** The error correction model (ECM) method has been updated to exclude the intercept term, improving the accuracy of the estimation.
- **ECM method updated:** The ECM method has been further refined to enhance estimation accuracy and better integrate with the new class structure.
- **Long-term multipliers:** Delta method for long-term multipliers has been added in the function's documentation. The summary output now includes the long-term multipliers along with their standard errors and confidence intervals, providing users with more comprehensive information about the estimated effects.
- **Dynamic multipliers:** The dynamic multipliers are now available in all symmetric, asymmetric and, mixed models. All LL, NN, AS and SA models' mplier are now available.



# kardl 1.1.0

## Changes in this version

- **Standard classes:** All tests and estimation routines have been rewritten to use standard R classes such as `htest`, `anova`, and `lm`. This improves compatibility with generic methods and downstream analyses.

- **Multipliers & bootstrap:** Added functionality to compute dynamic multipliers along with a bootstrap-based inference method, allowing more robust uncertainty quantification.

- **ECM method updated:** The error correction model (ECM) method has been modified to enhance estimation accuracy and better integrate with the new class structure.

- **Internal improvements:** Minor internal code improvements and documentation updates to support new features and maintain CRAN compliance.

- **Windows compatibility:** Fixed an issue where the `summary()` output was not fully compatible with Windows in earlier versions.

- **Documentation:** Updated documentation to reflect changes in function names and new features, ensuring clarity for users.

- **Examples:** Updated examples in the documentation to demonstrate the new features and changes in function usage.

- **Testing:** Added new unit tests to cover the new features and ensure the robustness of the package.

- **Performance:** Improved performance of the estimation routines, particularly for larger datasets, through optimized code and better use of R's vectorized operations.

- **Error handling:** Enhanced error handling to provide more informative messages when users encounter issues with input data or function usage.

- **Vignettes:** Updated vignettes to include examples of the new features and to provide a comprehensive overview of the package's capabilities.

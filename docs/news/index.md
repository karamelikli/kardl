# Changelog

## kardl 1.3.0

### Changes in this version

- **symmetry test:** The symmetry test has been updated to include a new
  method for calculating the test statistic, Chi-squared, in addition to
  the existing t-test method.
- **output update:** Colors in summary output have been removed to
  improve readability and ensure that the output is clear and accessible
  to all users, regardless of their display settings or preferences.
- **lower and upper limits of confidence intervals:** The lower and
  upper limits of confidence intervals in the summary output have been
  updated to be more accurate and reflective of the underlying data,
  providing users with more reliable information for inference.
- **settings update:** The settings for the model were added to kardl
  objects, allowing users to easily access and review the configuration
  of their models.

## kardl 1.2.1

### Changes in this version

- **Documentation update:** The documentation for psst, pssf, narayana,
  and ECM methods has been updated to include the delta method for
  long-term multipliers. This addition provides users with a more
  comprehensive understanding of the estimation process and the
  interpretation of long-term effects.
- **Rename variables names in mpliers:** The variable names in the
  multipliers function have been renamed to be more descriptive and
  consistent with the rest of the package. This change enhances
  readability and helps users better understand the output of the
  function.
- **Updating readme:** The README file has been updated to reflect the
  changes in the documentation and variable names, ensuring that users
  have access to the most current information about the package’s
  features and usage.

## kardl 1.2.0

### Changes in this version

- **t Upper value fixed:** The upper value of the bound for the t
  statistic in the [`summary()`](https://rdrr.io/r/base/summary.html)
  output has been corrected.
- **no intercept in ECM:** The error correction model (ECM) method has
  been updated to exclude the intercept term, improving the accuracy of
  the estimation.
- **ECM method updated:** The ECM method has been further refined to
  enhance estimation accuracy and better integrate with the new class
  structure.
- **Long-term multipliers:** Delta method for long-term multipliers has
  been added in the function’s documentation. The summary output now
  includes the long-term multipliers along with their standard errors
  and confidence intervals, providing users with more comprehensive
  information about the estimated effects.
- **Dynamic multipliers:** The dynamic multipliers are now available in
  all symmetric, asymmetric and, mixed models. All LL, NN, AS and SA
  models’ mplier are now available.

## kardl 1.1.0

CRAN release: 2026-03-09

### Changes in this version

- **Standard classes:** All tests and estimation routines have been
  rewritten to use standard R classes such as `htest`, `anova`, and
  `lm`. This improves compatibility with generic methods and downstream
  analyses.

- **Multipliers & bootstrap:** Added functionality to compute dynamic
  multipliers along with a bootstrap-based inference method, allowing
  more robust uncertainty quantification.

- **ECM method updated:** The error correction model (ECM) method has
  been modified to enhance estimation accuracy and better integrate with
  the new class structure.

- **Internal improvements:** Minor internal code improvements and
  documentation updates to support new features and maintain CRAN
  compliance.

- **Windows compatibility:** Fixed an issue where the
  [`summary()`](https://rdrr.io/r/base/summary.html) output was not
  fully compatible with Windows in earlier versions.

- **Documentation:** Updated documentation to reflect changes in
  function names and new features, ensuring clarity for users.

- **Examples:** Updated examples in the documentation to demonstrate the
  new features and changes in function usage.

- **Testing:** Added new unit tests to cover the new features and ensure
  the robustness of the package.

- **Performance:** Improved performance of the estimation routines,
  particularly for larger datasets, through optimized code and better
  use of R’s vectorized operations.

- **Error handling:** Enhanced error handling to provide more
  informative messages when users encounter issues with input data or
  function usage.

- **Vignettes:** Updated vignettes to include examples of the new
  features and to provide a comprehensive overview of the package’s
  capabilities.

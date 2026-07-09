# Contributing to `kardl`

Thank you for your interest in contributing to `kardl`.

This document outlines the recommended workflow for reporting issues, improving documentation, and contributing code. Many of these practices follow established recommendations for R package development and the tidyverse contribution workflow.

For general guidance on contributing to R packages, see the tidyverse development guide:

- https://rstd.io/tidy-contrib
- https://code-review.tidyverse.org/

## Before contributing

Please check existing issues and pull requests before opening a new one to avoid duplicate work.

For substantial changes, feature requests, API modifications, or changes affecting model behavior, please open an issue first so the proposed changes can be discussed before implementation.

Repository:

https://github.com/karamelikli/kardl

---

## Reporting bugs

Bug reports should include a minimal reproducible example (reprex) whenever possible.

Helpful bug reports generally include:

- A short description of the problem
- Expected behavior
- Actual behavior
- A minimal reproducible example
- Relevant error messages or warnings
- Session information (`sessionInfo()`) when appropriate

For guidance on writing effective issues, see:

https://code-review.tidyverse.org/issues/

---

## Fixing documentation issues

Small documentation improvements such as typo corrections, grammar fixes, formatting improvements, or clarification edits can be made directly through the GitHub web interface.

Please edit the source `.R` files containing the `roxygen2` comments rather than the generated `.Rd` files.

Useful references:

- https://roxygen2.r-lib.org/articles/roxygen2.html
- https://cran.r-project.org/package=roxygen2

---

## Development setup

We recommend using the `usethis` workflow for package development.

### 1. Fork and clone the repository

```r
usethis::create_from_github("karamelikli/kardl", fork = TRUE)
```

### 2. Install development dependencies

```r
devtools::install_dev_deps()
```

### 3. Verify package checks

Before making changes, ensure the package builds successfully:

```r
devtools::check()
```

If `R CMD check` fails for reasons unrelated to your changes, please open an issue before continuing.

---

## Pull request workflow

### Create a development branch

We recommend creating a dedicated branch for each pull request:

```r
usethis::pr_init("brief-description-of-change")
```

### Implement your changes

Before submitting a pull request, please:

- Run package tests
- Run `devtools::check()`
- Update documentation if necessary
- Add or update unit tests whenever appropriate

### Submit your pull request

Push your branch and create a pull request:

```r
usethis::pr_push()
```

Please ensure that:

- The pull request title clearly summarizes the change.
- The pull request description references related issues using:

```text
Fixes #issue-number
```

---

## NEWS.md updates

User-facing changes should be added to the top section of `NEWS.md`.

Please follow the tidyverse NEWS style guide:

https://style.tidyverse.org/news.html

---

## Code style

New code should follow the tidyverse style guide:

https://style.tidyverse.org

You may use **Air** to automatically format code:

https://posit-dev.github.io/air/

Please avoid reformatting unrelated code in the same pull request.

### Documentation

`kardl` uses:

- `roxygen2` for documentation
- Markdown syntax within roxygen comments

After modifying documentation, regenerate the Rd files using:

```r
devtools::document()
```

### Testing

Unit tests are written using **testthat**.

Contributions that include appropriate test cases are easier to review and merge.

Useful reference:

https://testthat.r-lib.org

---

## Package checks

Before submitting a pull request, contributors are encouraged to verify that the package passes all checks without errors or warnings:

```r
devtools::document()
devtools::test()
devtools::check()
```

---

## Code of Conduct

Please note that the `kardl` project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md).

By participating in this project, you agree to abide by its terms and help maintain a welcoming and respectful environment for everyone.

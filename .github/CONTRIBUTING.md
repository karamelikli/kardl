# Contributing to `kardl`

Thank you for your interest in contributing to `kardl`.
This document outlines the recommended workflow for proposing changes, reporting issues, and contributing code or documentation improvements.

## Before contributing

Please first check the existing issues and pull requests to avoid duplicate work.

For substantial changes, feature requests, or API modifications, open an issue before starting development so the proposed change can be discussed.

Repository:

[kardl GitHub repository](https://github.com/karamelikli/kardl)

---

## Reporting bugs

Bug reports should include a minimal reproducible example using a reprex whenever possible.

Helpful bug reports generally include:

* A short description of the problem
* Expected behavior
* Actual behavior
* A minimal reproducible example
* Relevant error messages or warnings

---

## Fixing documentation issues

Small documentation improvements such as typo corrections, grammar fixes, or clarification edits can be made directly through the GitHub web interface.

Please edit the source `.R` files containing the `roxygen2` comments rather than the generated `.Rd` files.

Useful references:

* [roxygen2 documentation](https://roxygen2.r-lib.org/articles/roxygen2.html)
* [Roxygen markdown formatting](https://cran.r-project.org/web/packages/roxygen2/vignettes/rd-formatting.html)

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

### 3. Run package checks

Before making changes, ensure the package passes checks locally:

```r
devtools::check()
```

If checks fail for reasons unrelated to your changes, please open an issue before continuing.

---

## Pull request workflow

### Create a development branch

We recommend creating a dedicated branch for each pull request:

```r
usethis::pr_init("brief-description-of-change")
```

### Make changes

After implementing your changes:

* Run tests
* Run `devtools::check()`
* Update documentation if necessary
* Add or update unit tests when appropriate

### Submit the pull request

Push your branch and open a pull request:

```r
usethis::pr_push()
```

Please ensure that:

* The PR title clearly summarizes the change
* The PR description references related issues using:

```text
Fixes #issue-number
```

---

## NEWS.md updates

User-facing changes should be added to the top section of `NEWS.md`.

Please follow the tidyverse news style guide:

[tidyverse NEWS style guide](https://style.tidyverse.org/news.html)

---

## Code style guidelines

### Documentation

The package uses:

* `roxygen2` for documentation
* Markdown syntax within roxygen comments

### Testing

Unit tests use the `testthat` framework.

Contributions that include tests are easier to review and merge.

Useful references:

* [testthat documentation](https://testthat.r-lib.org)

---

## Package checks and standards

Before submitting a pull request, contributors are encouraged to verify that:

```r
devtools::check()
```

runs successfully without errors or warnings.

If applicable, also check:

```r
devtools::document()
devtools::test()
```



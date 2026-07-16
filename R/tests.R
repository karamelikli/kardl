#' Generate Hypothesis Statements for Bounds Tests
#'
#' Internal function to create formatted null and alternative hypothesis
#' statements for coefficient-based tests in the kardl framework. Used
#' primarily in bounds testing and cointegration procedures.
#'
#' @param var_names Character vector of variable names to be included in the
#'   hypothesis statements.
#' @param ... Additional arguments (currently unused).
#'
#' @return A list of class `kardl_hypotheses` containing:
#'   \item{H0}{Character string describing the null hypothesis that all
#'     specified coefficients equal zero.}
#'   \item{H1}{Character string describing the alternative hypothesis that
#'     not all specified coefficients are zero.}
#'
#' @details
#' This function constructs human-readable hypothesis statements for testing
#' the significance of model coefficients. For a single variable, it creates
#' a simple equality/inequality statement. For multiple variables, it creates
#' a joint hypothesis statement indicating that not all coefficients are zero.
#'
#' The returned object has class `kardl_hypotheses` which has its own print
#' method for clean display.
#'
#' @srrstats {G1.3} Statistical terminology (null hypothesis, alternative
#'   hypothesis) is explicitly defined in the function documentation.
#' @srrstats {G2.4c} Variable names are handled as character vectors using
#'   as.character() conversion and paste operations.
#'
#' @keywords internal
#' @noRd
kardl_hypotheses <- function(var_names, ...) {
  h0 <- paste0(
    "H0: Coef(",
    paste(var_names, collapse = ") = Coef("),
    ") = 0"
  )

  h1 <- paste0(
    "H1: ",
    ifelse(
      length(var_names) > 1,
      paste0(
        "Not all of ",
        paste0("Coef(", var_names, ")", collapse = ", "),
        " are zero."
      ),
      paste0("Coef(", var_names, ") \u2260 0")
    )
  )
  structure(
    list(H0 = h0, H1 = h1),
    class = c("kardl_hypotheses", "list")
  )
}


#' @export
#' @noRd
summary.kardl_test <- function(object, ...) {
  if (!inherits(object, "kardl_test") || !inherits(object, "htest")) {
    stop(
      "Input object must be of class 'kardl_test' and 'htest' ",
      call. = FALSE
    )
  }
  if (!object$test.func %in% c("pssf", "psst", "narayan")) {
    stop(
      "kardl_decision() is only applicable to test objects from ",
      "pssf, psst, or narayan functions.",
      call. = FALSE
    )
  }
  switch(object$test.func,
    pssf = {
      test_cr_vals <- kardl_critvals(object)
      test_decision <- pssf_decision(
        object$statistic[["F"]],
        test_cr_vals$crit_vals,
        object$sig
      )
    },
    psst = {
      test_cr_vals <- kardl_critvals(object)
      notes <- test_cr_vals$note
      test_decision <- psst_decision(
        object$statistic[["t"]],
        test_cr_vals$crit_vals,
        object$sig
      )
    },
    narayan = {
      test_cr_vals <- kardl_critvals(object)
      notes <- test_cr_vals$note
      test_decision <- narayan_decision(
        object$statistic[["F"]],
        test_cr_vals$crit_vals,
        object$sig
      )
    }
  )
  case_txt <- switch(object$case,
    "1" = "I",
    "2" = "II",
    "3" = "III",
    "4" = "IV",
    "5" = "V"
  )
  structure(
    list(
      statistic = object$statistic,
      case_txt = case_txt,
      var_names = object$var_names,
      decision = test_decision$decision,
      hypotheses = object$hypotheses,
      numeric_decision = test_decision$numeric_decision,
      significance_level = test_decision$significance_level,
      cr_vals = test_cr_vals$crit_vals,
      k = test_cr_vals$k,
      notes = if (exists("notes")) notes else NULL
    ),
    class = c("kardl_test_summary", "list")
  )
}

#' Make Statistical Decision for Narayan Bounds Test
#'
#' Internal function to interpret the Narayan F-statistic against critical
#' value bounds and produce a statistical decision regarding cointegration.
#'
#' @param f_stat Numeric. The computed F-statistic from the bounds test.
#' @param cr_vals Data frame containing lower (L) and upper (U) critical
#'   value bounds for different significance levels.
#' @param signif_level Character string specifying the significance level.
#'   Either "auto" for automatic selection based on the test statistic,
#'   or a specific value like "0.10", "0.05", or "0.01".
#'
#' @return A list containing:
#'   \item{statistic}{The input F-statistic.}
#'   \item{decision}{Character string describing the test decision.}
#'   \item{numeric_decision}{Numeric code: 1 (reject H0), 0 (inconclusive),
#'     or -1 (fail to reject H0).}
#'   \item{significance_level}{The significance level used for the decision.}
#'
#' @details
#' This function implements the decision rule for the Narayan bounds test:
#' \itemize{
#'   \item If F-statistic > upper bound: reject null, conclude cointegration
#'   \item If F-statistic < lower bound: fail to reject null, no cointegration
#'   \item If lower bound <= F-statistic <= upper bound: inconclusive
#' }
#'
#' When signif_level is "auto", the function selects the most stringent
#' significance level at which the null can be rejected.
#'
#' @srrstats {G1.3} Statistical decision criteria are explicitly defined.
#' @srrstats {G2.3b} Significance level input is case-insensitive through
#'   standardized string matching.
#' @srrstats {G3.0} All comparisons use appropriate inequality operators
#'   for numeric values.
#'
#' @keywords internal
#' @noRd
narayan_decision <- function(f_stat, cr_vals, signif_level) {
  if (signif_level == "auto") {
    numeric_dec <- 1

    if (f_stat >= cr_vals[3, "U"]) {
      decision <- "Reject H0 \u2192 Cointegration (at 1% level)"
      sig <- "0.01"
    } else if (f_stat >= cr_vals[2, "U"]) {
      decision <- "Reject H0 \u2192 Cointegration (at 5% level)"
      sig <- "0.05"
    } else if (f_stat >= cr_vals[1, "U"]) {
      decision <- paste0(
        "Reject H0 \u2192 Weak evidence of cointegration ",
        "(at 10% level)"
      )
      sig <- "0.10"
    } else {
      sig <- "0.10"
      if (f_stat >= cr_vals[1, "L"]) {
        decision <- "Inconclusive"
        numeric_dec <- 0
      } else {
        decision <- "Fail to reject H0 \u2192 No Cointegration"
        numeric_dec <- -1
      }
    }
  } else {
    sig <- signif_level
    bu_row <- switch(signif_level,
      "0.1" = 1,
      "0.10" = 1,
      "0.05" = 2,
      "0.01" = 3
    )
    if (f_stat >= cr_vals[bu_row, "U"]) {
      if (bu_row == 1) {
        decision <- paste0(
          "Reject H0 \u2192 Weak evidence of cointegration",
          " (at 10% level)"
        )
      } else {
        decision <- paste0(
          "Reject H0 \u2192 Cointegration (at ",
          switch(bu_row,
            "1" = "10%",
            "2" = "5%",
            "3" = "0.01"
          ),
          " level)"
        )
      }
      numeric_dec <- 1
    } else if (f_stat >= cr_vals[bu_row, "L"]) {
      decision <- "Inconclusive"
      numeric_dec <- 0
    } else {
      decision <- "Fail to reject H0 \u2192 No Cointegration"
      numeric_dec <- -1
    }
  }
  list(
    statistic = f_stat,
    decision = decision,
    numeric_decision = numeric_dec,
    significance_level = sig
  )
}

#' Make Statistical Decision for Pesaran-Shin-Smith F-test
#'
#' Internal function to interpret the Pesaran-Shin-Smith F-statistic against
#' critical value bounds and produce a statistical decision regarding
#' cointegration.
#'
#' @param pss_f_stat Numeric. The computed F-statistic from the PSS bounds
#'   test.
#' @param cr_vals Data frame containing lower (L) and upper (U) critical
#'   value bounds for different significance levels (0.10, 0.05, 0.025, 0.01).
#' @param signif_level Character string specifying the significance level.
#'   Either "auto" for automatic selection based on the test statistic,
#'   or a specific value like "0.10", "0.05", "0.025", or "0.01".
#'
#' @return A list containing:
#'   \item{statistic}{The input F-statistic.}
#'   \item{decision}{Character string describing the test decision.}
#'   \item{numeric_decision}{Numeric code: 1 (reject H0), 0 (inconclusive),
#'     or -1 (fail to reject H0).}
#'   \item{significance_level}{The significance level used for the decision.}
#'
#' @details
#' This function implements the decision rule for the PSS F-test bounds test:
#' \itemize{
#'   \item If F-statistic > upper bound: reject null, conclude cointegration
#'   \item If F-statistic < lower bound: fail to reject null, no cointegration
#'   \item If lower bound <= F-statistic <= upper bound: inconclusive
#' }
#'
#' The PSS F-test supports four significance levels (10%, 5%, 2.5%, and 1%),
#' providing more granular decision-making than the t-test version.
#'
#' @srrstats {G1.3} Statistical decision criteria and significance levels
#'   are explicitly defined.
#' @srrstats {G2.3a} Uses match/switch logic to validate significance level
#'   inputs.
#' @srrstats {G3.0} All comparisons use appropriate inequality operators
#'   for numeric values.
#'
#' @keywords internal
#' @noRd
pssf_decision <- function(pss_f_stat, cr_vals, signif_level) {
  if (signif_level == "auto") {
    numeric_dec <- 1

    if (pss_f_stat >= cr_vals[4, "U"]) {
      decision <- "Reject H0 \u2192 Cointegration (at 1% level)"
      sig <- "0.01"
    } else if (pss_f_stat >= cr_vals[3, "U"]) {
      decision <- "Reject H0 \u2192 Cointegration (at 2.5% level)"
      sig <- "0.025"
    } else if (pss_f_stat >= cr_vals[2, "U"]) {
      decision <- "Reject H0 \u2192 Cointegration (at 5% level)"
      sig <- "0.05"
    } else if (pss_f_stat >= cr_vals[1, "U"]) {
      decision <- paste0(
        "Reject H0 \u2192 Weak evidence of cointegration ",
        "(at 10% level)"
      )
      sig <- "0.10"
    } else {
      sig <- "0.10"
      if (pss_f_stat >= cr_vals[1, "L"]) {
        decision <- "Inconclusive"
        numeric_dec <- 0
      } else {
        decision <- "Fail to reject H0 \u2192 No Cointegration"
        numeric_dec <- -1
      }
    }
  } else {
    sig <- signif_level
    bu_row <- switch(signif_level,
      "0.1" = 1,
      "0.10" = 1,
      "0.05" = 2,
      "0.025" = 3,
      "0.01" = 4
    )
    if (pss_f_stat >= cr_vals[bu_row, "U"]) {
      if (bu_row == 1) {
        decision <- paste0(
          "Reject H0 \u2192 Weak evidence of cointegration ",
          "(at 10% level)"
        )
      } else {
        decision <- paste0(
          "Reject H0 \u2192 Cointegration (at ",
          switch(bu_row,
            "1" = "10%",
            "2" = "5%",
            "3" = "2.5%",
            "4" = "0.01"
          ),
          " level)"
        )
      }
      numeric_dec <- 1
    } else if (pss_f_stat >= cr_vals[bu_row, "L"]) {
      decision <- "Inconclusive"
      numeric_dec <- 0
    } else {
      decision <- "Fail to reject H0 \u2192 No Cointegration"
      numeric_dec <- -1
    }
  }
  # Placeholder for pssf decision logic
  list(
    statistic = pss_f_stat,
    decision = decision,
    numeric_decision = numeric_dec,
    significance_level = sig
  )
}

#' Make Statistical Decision for Pesaran-Shin-Smith t-test
#'
#' Internal function to interpret the Pesaran-Shin-Smith t-statistic against
#' critical value bounds and produce a statistical decision regarding the
#' significance of the error correction term in cointegration testing.
#'
#' @param pss_t_stat Numeric. The computed t-statistic from the PSS bounds test.
#' @param psst_cr_vals Data frame containing lower (L) and upper (U) critical
#'   value bounds for different significance levels (0.10, 0.05, 0.025, 0.01).
#' @param signif_level Character string specifying the significance level.
#'   Either "auto" for automatic selection based on the test statistic,
#'   or a specific value like "0.10", "0.05", "0.025", or "0.01".
#'
#' @return A list containing:
#'   \item{statistic}{The input t-statistic.}
#'   \item{decision}{Character string describing the test decision.}
#'   \item{numeric_decision}{Numeric code: 1 (reject H0), 0 (inconclusive),
#'     or -1 (fail to reject H0).}
#'   \item{significance_level}{The significance level used for the decision.}
#'
#' @details
#' This function implements the decision rule for the PSS t-test bounds test.
#' Note that t-statistics are typically negative when testing the error
#' correction term, so the decision rules are:
#' \itemize{
#'   \item If t-statistic < lower bound: reject null, conclude cointegration
#'   \item If t-statistic > upper bound: fail to reject null, no cointegration
#'   \item If lower bound <= t-statistic <= upper bound: inconclusive
#' }
#'
#' The PSS t-test provides an alternative to the F-test, focusing on the
#' significance of the lagged level of the dependent variable in the ARDL
#' specification.
#'
#' @srrstats {G1.3} Statistical decision criteria are explicitly defined
#'   with attention to the directional nature of t-statistics.
#' @srrstats {G2.3a} Uses match/switch logic to validate significance level
#'   inputs.
#' @srrstats {G3.0} All comparisons use appropriate inequality operators
#'   for numeric values, accounting for negative t-statistics.
#'
#' @keywords internal
#' @noRd
psst_decision <- function(pss_t_stat, psst_cr_vals, signif_level) {
  if (signif_level == "auto") {
    numeric_dec <- 1

    if (pss_t_stat <= psst_cr_vals[4, "U"]) {
      decision <- "Reject H0 \u2192 Cointegration (at 1% level)"
      sig <- "0.01"
    } else if (pss_t_stat <= psst_cr_vals[3, "U"]) {
      decision <- "Reject H0 \u2192 Cointegration (at 2.5% level)"
      sig <- "0.025"
    } else if (pss_t_stat <= psst_cr_vals[2, "U"]) {
      decision <- "Reject H0 \u2192 Cointegration (at 5% level)"
      sig <- "0.05"
    } else if (pss_t_stat <= psst_cr_vals[1, "U"]) {
      # Due to the lint warning about the length of the line, I have split the
      # string into two parts and concatenated them using paste0.
      decision <- paste0(
        "Reject H0 \u2192 Weak evidence of cointegration",
        " (at 10% level)"
      )
      sig <- "0.10"
    } else {
      sig <- "0.10"
      if (pss_t_stat <= psst_cr_vals[1, "L"]) {
        decision <- "Inconclusive"
        numeric_dec <- 0
      } else {
        decision <- "Fail to reject H0 \u2192 No Cointegration"
        numeric_dec <- -1
      }
    }
  } else {
    sig <- signif_level
    bu_row <- switch(signif_level,
      "0.1" = 1,
      "0.10" = 1,
      "0.05" = 2,
      "0.025" = 3,
      "0.01" = 4
    )
    if (pss_t_stat <= psst_cr_vals[bu_row, "U"]) {
      if (bu_row == 1) {
        # Due to the lint warning about the length of the line, I have split the
        # string into two parts and concatenated them using paste0.
        decision <- paste0(
          "Reject H0 \u2192 Weak evidence of cointegration",
          " (at 10% level)"
        )
      } else {
        decision <- paste0(
          "Reject H0 \u2192 Cointegration (at ",
          switch(bu_row,
            "1" = "10%",
            "2" = "5%",
            "3" = "2.5%",
            "4" = "0.01"
          ),
          " level)"
        )
      }
      numeric_dec <- 1
    } else if (pss_t_stat <= psst_cr_vals[bu_row, "L"]) {
      decision <- "Inconclusive"
      numeric_dec <- 0
    } else {
      decision <- "Fail to reject H0 \u2192 No Cointegration"
      numeric_dec <- -1
    }
  }
  list(
    statistic = pss_t_stat,
    decision = decision,
    numeric_decision = numeric_dec,
    significance_level = sig
  )
}

#' Extract Critical Values for KARDL Bounds Tests
#'
#' Internal function to retrieve the appropriate critical values for
#' the Pesaran-Shin-Smith F-test, Pesaran-Shin-Smith t-test, or Narayan bounds
#' test based on the test object. This function checks the class and test type
#' of the input object and then extracts the critical values from the
#' corresponding tables, adjusting for the number of regressors and observations
#'  as needed.
#'
#'  @param x An object of class 'kardl_test
#'  @param ... Additional arguments (currently unused).
#'  @return A list of class 'kardl_critvals' containing the critical values,
#'  test function type, number of regressors, and any notes regarding
#'  adjustments made for the critical values.
#'
#' @noRd

kardl_critvals <- function(x, ...) {
  if (!inherits(x, "kardl_test") || !inherits(x, "htest")) {
    stop(
      "Input object must be of class 'kardl_test' and 'htest' ",
      call. = FALSE
    )
  }
  if (!x$test.func %in% c("pssf", "psst", "narayan")) {
    stop(
      "kardl_critvals() is only applicable to test objects from ",
      "pssf, psst, or narayan functions.",
      call. = FALSE
    )
  }

  switch(x$test.func,
    pssf = {
      k <- x$k
      crit_vals <- pssf_critvals(x$case, k)
    },
    psst = {
      k <- x$k
      if (k > 10) {
        k <- 10
        notes <- paste0(
          "The number of regressors exceeds the maximum limit for",
          " the critical values table. Using the critical values",
          " for 10 regressors."
        )
      }
      crit_vals <- psst_critvals(x$case, k)
    },
    narayan = {
      obs_number <- round((x$n - 25) / 5)
      if (obs_number > 11) {
        obs_number <- 11
        notes <-
          paste0(
            "The number of observations exceeds the maximum limit ",
            "for the critical values table. Using the critical ",
            "values for 80 observations."
          )
      }
      k <- x$k
      if (k > 7) {
        k <- 7
        notes <- c(
          if (exists("notes")) notes else NULL,
          paste0(
            "The number of regressors exceeds the maximum limit for",
            " the critical values table. Using the critical values",
            " for 7 regressors."
          )
        )
      }
      crit_vals <- narayan_critvals(x$case, k, obs_number)
    },
    stop("Unknown test function for extracting critical values.", call. = FALSE)
  )
  structure(
    list(
      crit_vals = crit_vals,
      test.func = x$test.func,
      k = k,
      note = if (exists("notes")) notes else NULL
    ),
    class = c("kardl_critvals", "list")
  )
}


#' Symmetry Test for Nonlinear kardl Models
#'
#' This function performs symmetry tests on non-linear KARDL models to assess
#' whether the effects of positive and negative changes in independent variables
#' are statistically different.
#'
#' This function evaluates whether the inclusion of a particular variable in the
#' model follows a linear relationship or exhibits a non-linear pattern. By
#' analyzing the behavior of the variable, the function helps to identify if the
#' relationship between the variable and the outcome of interest adheres to a
#' straight-line assumption or if it deviates, indicating a non-linear
#' interaction. This distinction is important in model specification, as it
#' ensures that the variable is appropriately represented, which can enhance the
#' model's accuracy and predictive performance.
#'
#' @description
#' The symmetry test is a statistical procedure used to assess the presence of
#' symmetry in the relationship between variables in a model. It is particularly
#' useful in econometric analysis, where it helps to identify whether the
#' effects of changes in one variable on another are symmetric or asymmetric.
#' The test involves estimating a model that includes both positive and negative
#' components of the variables and then performing a Wald test to determine if
#' the coefficients of these components are significantly different from each
#' other.
#' If the test indicates significant differences, it suggests that the
#' relationship is asymmetric, meaning that the impact of increases and
#' decreases in the variables differs.
#' This test returns results for both long-run and short-run variables in a
#' KARDL model. Where applicable, it provides the Wald test statistics,
#' p-values, degrees of freedom, sum of squares, and mean squares for each
#' variable tested.
#' If the null hypothesis of symmetry is rejected, it indicates that the effects
#' of positive and negative changes in the variable are significantly different,
#' suggesting an asymmetric relationship.
#'
#' The non-linear model with one asymmetric variables is specified as follows:
#' \deqn{
#'  \Delta{y_{t}} = \psi + \eta_{0}y_{t - 1} + \eta^{+}_{1} x^{+}_{t - 1}
#'  + \eta^{-}_{1} x^{-}_{t - 1} + \sum_{j = 1}^{p}{\gamma_{j}\Delta y_{t - j}}
#'  + \sum_{j = 0}^{q}{\beta^{+}_{j}\Delta x^{+}_{t - j}}
#'  + \sum_{j = 0}^{m}{\beta^{-}_{j}\Delta x^{-}_{t - j}} + e_{t}
#' }
#'
#' This function performs the symmetry test both for long-run and short-run
#' variables in a kardl model. It uses the \code{\link[nlWaldTest]{nlWaldtest}}
#' function from the \pkg{nlWaldTest} package for long-run variables and the
#' \code{\link[car]{linearHypothesis}} function from the \pkg{car} package for
#' short-run variables.
#' The hypotheses for the long-run variables are:
#' \deqn{
#'     H_{0}: -\frac{\eta^{+}_{1}}{\eta_{0}} = -\frac{\eta^{-}_{1}}{\eta_{0}}
#' }
#' \deqn{
#'     H_{1}: -\frac{\eta^{+}_{1}}{\eta_{0}} \neq -\frac{\eta^{-}_{1}}{\eta_{0}}
#' }
#'
#' The hypotheses for the short-run variables are:
#' \deqn{
#'   H_{0}: \sum_{j = 0}^{q}{\beta^{+}_{j}} = \sum_{j = 0}^{m}{\beta^{-}_{j}}
#' }
#' \deqn{
#'   H_{1}: \sum_{j = 0}^{q}{\beta^{+}_{j}} \neq \sum_{j = 0}^{m}{\beta^{-}_{j}}
#' }
#'
#' @param kardl_model An object of class \code{kardl_lm} produced by
#'   \code{\link{kardl}} or an object of class \code{kardl_longrun}
#'   produced by \code{\link{kardl_longrun}}.
#' @param selected_vars A character vector specifying the names of the variables
#'  to be included in the symmetry test. If NULL (the default), all eligible
#'  variables will be tested. The variable names should match those used in the
#'   KARDL model, and they can be either long-run or short-run variables. If any
#'   specified variable is not found in the model, an error will be raised.
#' @param component A character string specifying which component(s) of the
#'   model to test for symmetry. The options are "both" (default), "shortrun",
#'   or "longrun". If "both" is selected, the function will perform symmetry
#'   tests for both long-run and short-run variables. If "shortrun" is selected,
#'   only short-run variables will be tested, and if "longrun" is selected, only
#'   long-run variables will be tested. Invalid values will result in an error.
#' @param type A character string specifying the type of test statistic to be
#'   used in the Wald tests. The options are "F" (default) or "Chisq". If "F" is
#'   selected, the function will perform an F-test, which is appropriate for
#'   smaller sample sizes. If "Chisq" is selected, the function will perform a
#'   chi-squared test, which is more suitable for larger sample sizes. Invalid
#'   values will result in an error.
#' @param ... Additional arguments to be passed to the underlying test
#'   functions, such as \code{\link[nlWaldTest]{nlWaldtest}} for long-run tests
#'   or \code{\link[car]{linearHypothesis}} for short-run tests. These arguments
#'   can include options for controlling the behavior of the tests, such as
#'   specifying the type of test statistic or adjusting for multiple
#'   comparisons.
#'
#' @return A list with class "kardl" containing the following components:
#' \itemize{
#' \item \code{long_wald_summary:} A data frame containing the Wald test results
#'  for the long-run variables, including F-statistic, p-value, degrees of
#'  freedom, and residual degrees of freedom.
#' \item \code{long_hypotheses:} A list containing the null and alternative
#'   hypotheses for the long-run variables.
#' \item \code{short_wald_summary:} A data frame containing the Wald test
#' results for the short-run variables, including F-statistic, p-value, degrees
#' of freedom, residual degrees of freedom, and sum of squares.
#' \item \code{short_hypotheses:} A list containing the null and alternative
#'   hypotheses for the short-run variables.
#' }
#' @srrstats {G1.0} This function cites the original source of the symmetry
#'   test for non-linear ARDL models, which is based on the work of Shin, Yu,
#'   and Greenwood-Nimmo (2014).
#' @srrstats {G1.1} The function implements the symmetry test for non-linear
#'   ARDL models as described in the original paper by Shin, Yu, and
#'   Greenwood-Nimmo (2014). It uses the appropriate test statistics and
#'   follows the correct procedures for conducting the tests.
#' @srrstats {G1.3} Statistical terms such as NULL hypothesis, alternative
#'   hypothesis, test statistic are defined in the context of the symmetry test
#'   for non-linear ARDL models.
#' @srrstats {G2.0} The function validates that `kardl_model` is of class
#' `kardl_lm` and rejects ECM objects that are not suitable for symmetry testing
#' .
#' @srrstats {G2.3} The `component` argument is matched against
#'   `c("both", "shortrun", "longrun")` and `type` against `c("F", "Chisq")`
#'   using `match.arg()`.
#' @srrstats {G5.2b} Tests include error expectations for incompatible object
#'   classes and invalid post-estimation inputs.
#' @export
#'
#' @import nlWaldTest
#' @importFrom car linearHypothesis
#' @references
#' Shin, Y., Yu, B., & Greenwood-Nimmo, M. (2014). Modelling asymmetric
#' cointegration and dynamic multipliers in a nonlinear ARDL framework.
#' Festschrift in honor of Peter Schmidt: Econometric methods and applications,
#' 281-314.
#' @seealso \code{\link{kardl}}, \code{\link{pssf}}, \code{\link{psst}},
#'   \code{\link{ecm}}, \code{\link{narayan}}
#' @examples
#' kardl_model <- kardl(
#'   DriversKilled ~ Lasym(drivers + PetrolPrice) + Sas(PetrolPrice) +
#'     deterministic(law) + trend,
#'   Seatbelts
#' )
#' ast <- symmetrytest(kardl_model)
#' ast
#' # Detailed results of the test:
#' summary(ast)
#' # The null hypothesis of the test is that the model is symmetric, while the
#' # alternative hypothesis is that the model is asymmetric. The test statistic
#' # and p-value are provided in the output. If the p-value is less than a
#' # chosen significance level (e.g., 0.05), we reject the null hypothesis and
#' # conclude that there is evidence of asymmetry in the model.
#'
#' # The default significance level is 0.05, but you can specify a different
#' # level using the 'level' argument in the summary function. For example, to
#' # use a significance level of 0.01, you can use the following code:
#' summary(ast, level = 0.01)
#'
#' # To get symmetry test results in long-run, you can use the following code:
#' kardl_extract(ast, what = "long_wald_summary")
#'
#' # To get symmetry test results in short-run, you can use the following code:
#' kardl_extract(ast, what = "short_wald_summary")
#'
#' # To get the null and alternative hypotheses of the test in long-run,
#' # you can use the following code:
#' kardl_extract(ast, what = "long_hypotheses")
#'
#' # To get the null and alternative hypotheses of the test in short-run,
#' # you can use the following code:
#' kardl_extract(ast, what = "short_hypotheses")
#'
#' # Alternatively, you can also use the symmetrytest function with the
#' # component argument to specify whether you want to test for long-run or
#' # short-run symmetry. For example, to test for long-run symmetry, you can use
#' # the following code:
#' symmetrytest(kardl_model, component = "longrun")
#'
#' # To test for short-run symmetry, you can use the following code:
#' symmetrytest(kardl_model, component = "shortrun")
#'
#' # If you want to test for symmetry with respect to a specific variable,
#' # you can use the selected_vars argument in the symmetrytest function. For
#' # example, to test for symmetry with respect to the drivers variable, you can
#' # use the following code:
#' symmetrytest(kardl_model, selected_vars = "drivers")
#'
#' # To test for symmetry with respect to multiple variables, you can provide
#' # a vector of variable names to the selected_vars argument. For example, to
#' # test for symmetry with respect to both drivers and PetrolPrice, you can use
#' # the following code:
#' symmetrytest(kardl_model, selected_vars = c("drivers", "PetrolPrice"))
#'
#' # Finally, you can also specify the type of test statistic to be used in the
#' # symmetry test. By default, the function uses the Wald test F statistic,
#' # but you can also choose to use the chi-squared test statistic.
#' # For example, to use the chi-squared test statistic, you can use the
#' # following code:
#' symmetrytest(kardl_model, type = "Chisq")
symmetrytest <- function(kardl_model,
                         selected_vars = NULL,
                         component = "both",
                         type = "F",
                         ...) {
  UseMethod("symmetrytest")
}

#' @exportS3Method
symmetrytest.default <- function(kardl_model,
                                 selected_vars = NULL,
                                 component = "both",
                                 type = "F",
                                 ...) {
  stop(
    "symmetrytest() requires a kardl_lm object. ",
    "Please estimate a model using kardl() first.",
    call. = FALSE
  )
}

#' @export
#' @method symmetrytest kardl_longrun
symmetrytest.kardl_longrun <- function(
  kardl_model,
  selected_vars = NULL,
  component = "both",
  type = "F",
  ...
) {
  symmetrytest.kardl_lm(
    kardl_model = kardl_model$original_model,
    selected_vars = selected_vars,
    component = component,
    type = type,
    ...
  )
}

#' Symmetry Test for Nonlinear KARDL Models
#'
#' This function performs symmetry tests on non-linear KARDL models to assess
#' whether the effects of positive and negative changes in independent variables
#' are statistically different. The function evaluates both long-run and
#' short-run variables in the model, providing Wald test statistics, p-values,
#' degrees of freedom, and sum of squares where applicable. The null hypothesis
#' of the test is that the model is symmetric, while the alternative hypothesis
#' is that the model is asymmetric. If the p-value is less than a chosen
#' significance level, we reject the null hypothesis and conclude that there is
#' evidence of asymmetry in the model. The function allows users to specify
#' which variables to test for symmetry, whether to test long-run or short-run
#' components, and the type of test statistic to use (F or chi-squared).
#' Additional arguments can be passed to the underlying test functions for
#' further customization.
#'
#' @param kardl_model The KARDL model object to be tested for symmetry.
#' @param selected_vars A character vector specifying the names of the variables
#' to be included in the symmetry test. If NULL (the default), all eligible
#' variables will be tested. The variable names should match those used in the
#' KARDL model, and they can be either long-run or short-run variables.
#' @param component A character string specifying which component(s) of the
#' model to test for symmetry. The options are "both" (default),
#' "shortrun", or "longrun". If "both" is selected, the function will perform
#'  symmetry  tests for both long-run and short-run variables. If "shortrun" is
#'  selected, only short-run variables will be tested, and if "longrun" is
#'  selected, only long-run variables will be tested. Invalid values will result
#'   in an error.
#' @param type A character string specifying the type of test statistic to be
#' used in the Wald tests. The options are "F" (default) or "Chisq". If "F" is
#' selected, the function will perform an F-test, which is appropriate for
#' smaller sample sizes. If "Chisq" is selected, the function will perform a
#' chi-squared test, which is more suitable for larger sample sizes. Invalid
#' values will result in an error.
#' @param ... Additional arguments to be passed to the underlying test
#' functions, such as \code{\link[nlWaldTest]{nlWaldtest}} for long-run tests or
#'  \code{\link[car]{linearHypothesis}} for short-run tests. These arguments can
#'   include options for controlling the behavior of the tests, such as
#'  specifying the type of test statistic or adjusting for multiple comparisons.
#'
#' @noRd
#' @method symmetrytest kardl_lm
#'
#' @export
symmetrytest.kardl_lm <- function(
  kardl_model,
  selected_vars = NULL,
  component = "both",
  type = "F",
  ...
) {
  component <- match.arg(
    tolower(component),
    choices = c("both", "shortrun", "longrun")
  )
  # to let users input "f" or "chisq" in any case format,
  # we will standardize the input to "F" or "Chisq"
  new_type <- paste0(
    toupper(substr(type, 1, 1)),
    tolower(substr(type, 2, nchar(type)))
  )
  type <- match.arg(new_type, choices = c("F", "Chisq"))

  if (!inherits(kardl_model, "kardl_lm")) {
    stop("Not suitable input.", call. = FALSE)
  }

  if (kardl_model$est_info$type == "ecm") {
    stop("Symmetry test is not suitable for ECM models.", call. = FALSE)
  }

  coef_names <- names(kardl_model$coefficients)
  kardl_wald <- list()

  #--------------------------------------------------
  # Variable selection
  #--------------------------------------------------
  all_sr_vars <- kardl_model$extracted_info$asym_short_vars
  all_lr_vars <- kardl_model$extracted_info$asym_long_vars
  all_testable_vars <- unique(c(all_sr_vars, all_lr_vars))

  if (is.null(selected_vars)) {
    selected_vars <- all_testable_vars
  } else {
    if (!is.character(selected_vars)) {
      stop("'selected_vars' must be NULL or a character vector.", call. = FALSE)
    }
    bad_vars <- setdiff(selected_vars, all_testable_vars)
    if (length(bad_vars) > 0) {
      stop(
        "These variables are not available for symmetry testing: ",
        toString(bad_vars),
        call. = FALSE
      )
    }
  }

  selected_sr_vars <- intersect(selected_vars, all_sr_vars)
  selected_lr_vars <- intersect(selected_vars, all_lr_vars)

  #--------------------------------------------------
  # Long-run symmetry
  #--------------------------------------------------
  if (component %in% c("both", "longrun") && length(selected_lr_vars) > 0) {
    long_wald_tests <- list()

    long_wald_summary <- data.frame()
    long_hypotheses <- list(H0 = list(), H1 = list())

    long_dep <- replace_lag_var(
      .kardl_settings_env$long_coef,
      kardl_model$extracted_info$dependent_var,
      1
    )

    dependent_id <- which(long_dep == coef_names)

    if (length(dependent_id) == 0) {
      stop(
        "Long-run dependent variable coefficient was not found.",
        call. = FALSE
      )
    }

    ms_res <- deviance(kardl_model) / df.residual(kardl_model)

    long_as <- data.frame()

    for (x in selected_lr_vars) {
      pos_param <- replace_lag_var(
        .kardl_settings_env$long_coef,
        paste0(
          .kardl_settings_env$asym_prefix[1],
          x,
          .kardl_settings_env$asym_suffix[1]
        ),
        1
      )
      neg_param <- replace_lag_var(
        .kardl_settings_env$long_coef,
        paste0(
          .kardl_settings_env$asym_prefix[2],
          x,
          .kardl_settings_env$asym_suffix[2]
        ),
        1
      )

      pos_index <- which(pos_param == coef_names)
      neg_index <- which(neg_param == coef_names)

      if (length(pos_index) == 0 || length(neg_index) == 0) {
        next
      }

      long_as <- rbind(
        long_as,
        data.frame(
          var_name = x,
          pos_param = pos_param,
          pos_index = pos_index,
          neg_param = neg_param,
          neg_index = neg_index,
          stringsAsFactors = FALSE
        )
      )
    }

    if (nrow(long_as) > 0) {
      for (v in seq_len(nrow(long_as))) {
        x <- long_as[v, ]

        l_test <- paste0(
          "-b[",
          x$pos_index,
          "]/b[",
          dependent_id,
          "] = -b[",
          x$neg_index,
          "]/b[",
          dependent_id,
          "]"
        )

        long_hypotheses$H0[[x$var_name]] <- paste0(
          "- Coef(",
          x$pos_param,
          ")/Coef(",
          long_dep,
          ") = - Coef(",
          x$neg_param,
          ")/Coef(",
          long_dep,
          ")"
        )
        long_hypotheses$H1[[
          x$var_name
        ]] <- "At least one coefficient differs from zero."
        class(long_hypotheses) <- c("kardl_hypotheses", "list")

        if (identical(type, "F")) {
          # include ... in nlWaldtest
          new_lw <- nlWaldtest(kardl_model, texts = l_test, df2 = TRUE, ...)
          long_wald_tests[[x$var_name]] <- new_lw
          f_val <- unname(new_lw$statistic[["F"]])
          df1 <- unname(new_lw$parameter[[1]])

          sum_sq_approx <- f_val * df1 * ms_res
          mean_sq_approx <- f_val * ms_res

          long_wald_summary <- rbind(
            long_wald_summary,
            data.frame(
              Df = df1,
              `Sum of Sq` = sum_sq_approx,
              `Mean Sq` = mean_sq_approx,
              `F value` = f_val,
              `Pr(>F)` = new_lw$p.value,
              row.names = x$var_name,
              check.names = FALSE
            )
          )
        } else {
          new_lw <- nlWaldtest(kardl_model, texts = l_test)
          long_wald_tests[[x$var_name]] <- new_lw
          chisq_val <- unname(new_lw$statistic[[1]])
          df1 <- unname(new_lw$parameter[[1]])

          long_wald_summary <- rbind(
            long_wald_summary,
            data.frame(
              Df = df1,
              Chisq = chisq_val,
              `Pr(>Chisq)` = new_lw$p.value,
              row.names = x$var_name,
              check.names = FALSE
            )
          )
        }
      }

      if (nrow(long_wald_summary) > 0) {
        if (identical(type, "F")) {
          class(long_wald_summary) <- c("anova", "data.frame")
          attr(long_wald_summary, "note") <- paste0(
            "Long-run tests based on normalized",
            "coefficients."
          )
        } else {
          class(long_wald_summary) <- c("anova", "data.frame")
        }
        attr(long_wald_summary, "heading") <- c(
          "Symmetry Test Results - Long-run:",
          "======================="
        )
        attr(long_wald_summary, "type") <- "Symmetry Test"
        attr(long_wald_summary, "method") <- paste("Wald Test", type)
        attr(long_wald_summary, "signif.legend") <-
          "Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1"

        kardl_wald$long_wald_summary <- long_wald_summary
        kardl_wald$long_hypotheses <- long_hypotheses
      }
    }
  }

  #--------------------------------------------------
  # Short-run symmetry
  #--------------------------------------------------
  if (component %in% c("both", "shortrun") && length(selected_sr_vars) > 0) {
    short_wald_tests <- list()
    short_wald_summary <- data.frame()
    short_hypotheses <- list(H0 = list(), H1 = list())

    for (v in selected_sr_vars) {
      coef_pos <- coef_neg <- neg_hyp <- pos_hyp <- character(0)

      for (i in 0:kardl_model$args_info$maxlag) {
        pos_lagged <- replace_lag_var(
          .kardl_settings_env$short_coef,
          paste0(
            .kardl_settings_env$asym_prefix[1],
            v,
            .kardl_settings_env$asym_suffix[1]
          ),
          i
        )
        neg_lagged <- replace_lag_var(
          .kardl_settings_env$short_coef,
          paste0(
            .kardl_settings_env$asym_prefix[2],
            v,
            .kardl_settings_env$asym_suffix[2]
          ),
          i
        )

        if (pos_lagged %in% coef_names) {
          coef_pos <- c(coef_pos, pos_lagged)
          pos_hyp <- c(pos_hyp, paste0("Coef(", pos_lagged, ")"))
        }
        if (neg_lagged %in% coef_names) {
          coef_neg <- c(coef_neg, neg_lagged)
          neg_hyp <- c(neg_hyp, paste0("Coef(", neg_lagged, ")"))
        }
      }

      if (length(coef_pos) == 0 || length(coef_neg) == 0) {
        next
      }

      sw <- paste0(
        paste(coef_pos, collapse = " + "),
        " = ",
        paste(coef_neg, collapse = " + ")
      )

      sh_pos_h0 <- paste(pos_hyp, collapse = " + ")
      sh_neg_h0 <- paste(neg_hyp, collapse = " + ")

      short_hypotheses$H0[[v]] <- paste0(sh_pos_h0, " = ", sh_neg_h0)
      short_hypotheses$H1[[v]] <- enc2utf8(paste0(
        sh_pos_h0,
        " \u2260 ", sh_neg_h0
      ))
      class(short_hypotheses) <- c("kardl_hypotheses", "list")

      sw2 <- car::linearHypothesis(
        kardl_model,
        hypothesis.matrix = sw,
        test = if (identical(type, "F")) "F" else "Chisq",
        ...
      )
      short_wald_tests[[v]] <- sw2

      if (identical(type, "F")) {
        short_wald_summary <- rbind(
          short_wald_summary,
          data.frame(
            Df = sw2[2, 3],
            `Sum of Sq` = sw2[2, 4],
            `Mean Sq` = sw2[2, 4] / sw2[2, 3],
            `F value` = sw2[2, 5],
            `Pr(>F)` = sw2[2, 6],
            row.names = v,
            check.names = FALSE
          )
        )
      } else {
        short_wald_summary <- rbind(
          short_wald_summary,
          data.frame(
            Df = sw2[2, "Df"],
            `Sum of Sq` = sw2[2, "Sum of Sq"],
            Chisq = sw2[2, "Chisq"],
            `Pr(>Chisq)` = sw2[2, "Pr(>Chisq)"],
            row.names = v,
            check.names = FALSE
          )
        )
      }
    }

    if (nrow(short_wald_summary) > 0) {
      class(short_wald_summary) <- c("anova", "data.frame")
      attr(short_wald_summary, "signif.legend") <-
        "Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1"
      attr(short_wald_summary, "heading") <- c(
        "Symmetry Test Results - Short-run:",
        "======================="
      )
      attr(short_wald_summary, "type") <- "Symmetry Test"
      attr(short_wald_summary, "method") <- paste("Wald Test", type)

      kardl_wald$short_wald_summary <- short_wald_summary
      kardl_wald$short_hypotheses <- short_hypotheses
    }
  }

  #--------------------------------------------------
  # Final checks
  #--------------------------------------------------
  if (length(kardl_wald) == 0) {
    stop(
      "No valid symmetry test could be computed for the requested component",
      " and variables.",
      call. = FALSE
    )
  }

  kardl_wald$long_wald_tests <- if (exists("long_wald_tests")) {
    long_wald_tests
  } else {
    NULL
  }
  kardl_wald$short_wald_tests <- if (exists("short_wald_tests")) {
    short_wald_tests
  } else {
    NULL
  }
  kardl_wald$component <- component
  kardl_wald$vars <- selected_vars
  kardl_wald$type <- type
  kardl_wald$call <- match.call()

  class(kardl_wald) <- c("kardl_symmetric", "list")
  kardl_wald
}


#' Pesaran, Shin, and Smith Bounds F-Test
#'
#' This function performs the Pesaran, Shin, and Smith (PSS) F Bound test to
#' assess the presence of a long-term relationship (cointegration) between
#' variables in the context of an autoregressive distributed lag (ARDL) model.
#' The PSS F Bound test examines the joint significance of lagged levels of the
#' variables in the model. It provides critical values for both the upper and
#' lower bounds, which help determine whether the variables are cointegrated.
#' If the calculated F-statistic falls outside these bounds, it indicates the
#' existence of a long-term equilibrium relationship. This test is particularly
#' useful when the underlying data includes a mix of stationary and
#' non-stationary variables.
#'
#' @param kardl_model An object of class \code{kardl_lm} produced by
#'   \code{\link{kardl}} or an object of class \code{kardl_longrun}
#'   produced by \code{\link{kardl_longrun}}.
#' @param case Numeric or character. Specifies the case of the test to be used
#'  in the function. Acceptable values are 1, 2, 3, 4, 5, and "auto". If
#'  "auto" is chosen, the function determines the case automatically
#'  based on the model's characteristics. Invalid values will result in
#'  an error.
#'  \itemize{
#'      \item \code{1}: No intercept and no trend
#'      \item \code{2}: Restricted intercept and no trend
#'      \item \code{3}: Unrestricted intercept and no trend
#'      \item \code{4}: Unrestricted intercept and restricted trend
#'      \item \code{5}: Unrestricted intercept and unrestricted trend
#'  }
#' @param signif_level Character or numeric. Specifies the significance level
#' to be used in the function. Acceptable values are "auto", "0.10",
#' "0.1", "0.05", "0.025", and "0.01". If a numeric value is provided,
#'  it will be converted to a character string. When \code{"auto"} is selected,
#' the function determines the significance level sequentially, starting from
#' the most stringent level (\code{"0.01"}) and proceeding to \code{"0.025"},
#' \code{"0.05"}, and \code{"0.10"} until a suitable level is identified.
#' Invalid values will result in an error.
#'
#' @param ... Additional arguments (currently not used).
#'
#' @section Hypothesis testing:
#' The null hypothesis (H0) of the F Bound test is that there is no
#' cointegration among the variables in the model. In other words, it tests
#' whether the long-term relationship between the variables is statistically
#' significant. If the calculated F-statistic exceeds the upper critical value,
#' we reject the null hypothesis and conclude that there is cointegration.
#' Conversely, if the F-statistic falls below the lower critical value, we fail
#' to reject the null hypothesis, indicating no evidence of cointegration. If
#' the F-statistic lies between the two critical values, the result is
#' inconclusive.
#'
#' \deqn{
#'   \Delta {y}_t =  \psi  + \varphi t  + \eta _0   {y}_{t-1}  +
#'   \sum_{i=1}^{k} {  \eta _i   {x}_{i,t-1} }  +
#'   \sum_{j=1}^{p} { \gamma_{j}  \Delta {y}_{t-j} }+
#'   \sum_{i=1}^{k} {\sum_{j=0}^{q_i} { \beta_{ij}   \Delta {x}_{i,t-j} } }+ e_t
#' }
#'
#' \describe{
#'   \item{Cases 1, 3, 5:}{
#'      \deqn{\mathbf{H_{0}:} \eta_0  = \eta_1  = \dots = \eta_k  = 0}
#'      \deqn{\mathbf{H_{1}:} \eta_{0} \neq \eta_{1} \neq \dots \neq \eta_{k}
#'            \neq 0}
#'   }
#'   \item{Case 2:}{
#'      \deqn{\mathbf{H_{0}:} \eta_0  = \eta_1  = \dots = \eta_k =  \psi  = 0}
#'      \deqn{\mathbf{H_{1}:} \eta_{0} \neq \eta_{1} \neq \dots \neq \eta_{k}
#'            \neq \psi \neq 0}
#'   }
#'   \item{Case 4:}{
#'      \deqn{\mathbf{H_{0}:} \eta_0  = \eta_1  = \dots = \eta_k = \varphi = 0}
#'      \deqn{\mathbf{H_{1}:} \eta_{0} \neq \eta_{1} \neq \dots \neq \eta_{k}
#'            \neq \varphi \neq 0}
#'   }
#' }
#'
#' @return A list with class "htest" containing the following components:
#' \itemize{
#' \item \code{statistic}: The calculated F-statistic for the test.
#' \item \code{case_txt}: A character string describing the case used for the
#'       test, based on the specified case parameter.
#' \item \code{alternative}: A character string describing the alternative
#'       hypothesis of the test.
#' \item \code{sample.size}: The number of observations used in the test.
#' \item \code{var_names}: A character vector containing the names of the
#'       dependent variable and independent variables used in the test.
#' \item \code{k}: The number of independent variables (excluding the dependent
#'       variable) included in the test.
#' \item \code{sig}: The significance level used for the test, either specified
#'       by the user or determined automatically.
#' \item \code{notes}: A character vector containing any notes or warnings
#'       related to the test, such as the suitability of the test for small
#'       sample sizes or any adjustments made to the case based on the model's
#'       characteristics.
#' }
#'
#' @srrstats {G1.0} This function cites the original source of the Pesaran,
#'           Shin, and Smith (2001) bounds testing approach for cointegration
#'           analysis, which is a widely used method in econometrics for
#'           testing the existence of long-term relationships between variables
#'           in autoregressive distributed lag (ARDL) models. The function also
#'           provides guidance on the suitability of the test for small sample
#'           sizes and suggests alternative approaches when necessary.
#' @srrstats {G1.3} Statistical terms such as NULL hypothesis, alternative
#'           hypothesis, test statistic are defined in the context of the
#'           Pesaran, Shin, and Smith (2001) bounds testing approach for
#'           cointegration analysis.
#' @srrstats {G2.0} Input argument `kardl_model` is validated as a `kardl_lm`
#'           object; `case` and `signif_level` are validated against acceptable
#'           values before the test is performed.
#' @srrstats {G2.3} The `case` argument is validated against
#'           `c(1,2,3,4,5,"auto")`; `signif_level` against the set of accepted
#'           strings.
#' @srrstats {TS2.2} The documentation discusses stationarity requirements in
#'           the context of ARDL bounds testing and the I(0)/I(1) framework.
#' @srrstats {TS2.4} Users are expected to assess integration order before
#'           applying bounds-test interpretations; the documentation states
#'           these modelling assumptions.
#'
#' @importFrom car linearHypothesis
#' @seealso \code{\link{psst}} \code{\link{ecm}} \code{\link{narayan}}
#' @references Pesaran, M. H., Shin, Y. and Smith, R. (2001), "Bounds Testing
#'             Approaches to the Analysis of Level Relationship", Journal of
#'             Applied Econometrics, 16(3), 289-326.
#'
#' @examples
#'
#' kardl_model <- kardl(
#'   DriversKilled ~ PetrolPrice + drivers + asym(PetrolPrice) +
#'     deterministic(law) + trend,
#'   Seatbelts,
#'   mode = c(1, 2, 3, 0)
#' )
#' my_pssF <- pssf(kardl_model, case = "auto", signif_level = "auto")
#' # Getting the results of the test.
#' my_pssF
#' # Getting details of the test.
#' my_summary <- summary(my_pssF)
#' my_summary
#'
#' # Getting the critical values of the test.
#' kardl_extract(my_summary, what = "critical_values")
#'
#' # Using magrittr :
#'
#' @examplesIf requireNamespace("magrittr", quietly = TRUE)
#' library(magrittr)
#' Seatbelts %>%
#'   kardl(
#'     DriversKilled ~ PetrolPrice + drivers + asym(PetrolPrice) +
#'       deterministic(law) + trend,
#'     mode = c(1, 2, 3, 0), data = .
#'   ) %>%
#'   pssf()
#'
#' # Getting details of the test results using magrittr:
#' Seatbelts %>%
#'   kardl(
#'     DriversKilled ~ PetrolPrice + drivers + asym(PetrolPrice) +
#'       deterministic(law) + trend,
#'     mode = c(1, 2, 3, 0), data = .
#'   ) %>%
#'   pssf() %>%
#'   summary()
#'
#' @export
#'
pssf <- function(kardl_model,
                 case = "auto",
                 signif_level = "auto",
                 ...) {
  UseMethod("pssf")
}

#' @export
#' @method pssf kardl_longrun
pssf.kardl_longrun <- function(
  kardl_model, case = "auto", signif_level = "auto",
  ...
) {
  pssf.kardl_lm(
    kardl_model = kardl_model$original_model,
    case = case,
    signif_level = signif_level,
    ...
  )
}


#' @exportS3Method
pssf.default <- function(kardl_model,
                         case = "auto",
                         signif_level = "auto",
                         ...) {
  stop(
    "pssf() requires a kardl_lm object. ",
    "Please estimate a model using kardl() first.",
    call. = FALSE
  )
}

#' @export
#' @method pssf kardl_lm
pssf.kardl_lm <- function(kardl_model, case = "auto", signif_level = "auto",
                          ...) {
  if (!case %in% c(1, 2, 3, 4, 5, "auto")) {
    stop("Invalid 'case' specification. ", call. = FALSE)
  }
  if (is.numeric(signif_level)) {
    signif_level <- as.character(signif_level)
  }
  if (!signif_level %in% c("auto", "0.10", "0.1", "0.05", "0.025", "0.01")) {
    stop(
      "Invalid significance level. ",
      "Valid options are: 'auto', 0.10, 0.05, 0.025, or 0.01.",
      call. = FALSE
    )
  }
  if (inherits(kardl_model, "kardl_lm")) {
    if (kardl_model$est_info$type == "ecm") {
      stop(
        "PSS F test is not applicable for ECM models. Please use 'psst()'",
        " function for residual-based tests.",
        call. = FALSE
      )
    }
  } else {
    stop("Input object must be of class 'kardl_lm'.", call. = FALSE)
  }

  notes_array <- c()
  if (kardl_model$est_info$time_span < 80) {
    notes_array <- c(
      notes_array,
      paste0(
        "The Pesaran et al. (2001) bounds F-test is designed for large ",
        "samples. Your model uses only ",
        kardl_model$est_info$time_span,
        " observations. ",
        "For greater accuracy with small samples, consider using the critical ",
        "values from Narayan (2005) or use 'narayan()' function.Results are",
        " provided here using the asymptotic Pesaran et al. (2001) bounds."
      )
    )
  }

  if (case == "auto") {
    # Default case is 3, which means unrestricted intercept and no trend
    case <- 3
  }

  if (kardl_model$extracted_info$no_constant) {
    if (case > 1) {
      notes_array <- c(
        notes_array,
        paste0(
          "No constant term is included in the model. Case automatically",
          " adjusted to 1 (no intercept, no trend)."
        )
      )
    }
    case <- 1
  } else {
    if (case == 1) {
      # If case is 1, it means that the model has a constant.
      # If the constant is not included, it should be set to 2.
      if (!kardl_model$extracted_info$trend) {
        case <- 2
        notes_array <- c(
          notes_array,
          paste0(
            "Constant included but no trend. Case automatically adjusted ",
            "to 2 (restricted intercept, no trend)."
          )
        )
      } else {
        # If case is 1, it means that the model has a constant and no trend.
        # If the trend is included, it should be set to 5.
        case <- 5
        notes_array <- c(
          notes_array,
          paste0(
            "Both constant and trend are included. Case automatically",
            " adjusted to 5 (unrestricted intercept and trend)."
          )
        )
      }
    } else if (case > 3) {
      # if case is greater than 3, it means that the model has a trend.
      #  If the trend is not included, it should be set to 3.
      if (!kardl_model$extracted_info$trend) {
        case <- 3
        notes_array <- c(
          notes_array,
          paste0(
            "No trend is included. Case automatically adjusted to 3 ",
            "(unrestricted intercept, no trend)."
          )
        )
      }
    } else if (case < 4 && kardl_model$extracted_info$trend) {
      # if case is less than 4, it means that the model does not have a trend.
      # If the trend is included, it should be set to 4.
      case <- 5
      notes_array <- c(
        notes_array,
        paste0(
          "Trend detected in the model. Case automatically adjusted to 5 ",
          "(unrestricted intercept and trend)."
        )
      )
    }
  }

  longrun_names <- replace_lag_var(
    .kardl_settings_env$long_coef,
    kardl_model$extracted_info$long_run_vars,
    1
  )
  # check if case is 1,3,5
  if (case == 2) {
    longrun_names <- c(longrun_names, "(Intercept)")
  }
  if (case == 4) {
    longrun_names <- c(longrun_names, "trend")
  }

  kisit <- unique(c(paste(unlist(longrun_names), "=0")))
  f_model <- linearHypothesis(kardl_model, kisit, test = "F")
  pss_f_stat <- f_model$F[2]

  k <- length(kardl_model$extracted_info$long_run_vars) - 1
  if (k > 10) {
    k <- 10
    notes_array <- c(
      notes_array,
      paste0(
        "The number of long-run variables exceeds 10. Critical values are",
        " not available for more than 10 variables. Results are provided ",
        "using the critical values for 10 variables, but interpret with",
        " caution."
      )
    )
  }


  structure(
    list(
      type = "cointegration",
      case = case,
      statistic = c("F" = pss_f_stat),
      method = "Pesaran-Shin-Smith (PSS) Bounds F-test for cointegration",
      alternative = "Cointegrating relationship exists",
      data.name = deparse(substitute(kardl_model)),
      sample.size = kardl_model$est_info$time_span,
      hypotheses = kardl_hypotheses(longrun_names),
      var_names = longrun_names,
      k = k,
      n = kardl_model$est_info$end,
      sig = signif_level,
      notes = notes_array,
      test.func = "pssf"
    ),
    class = c("kardl_test", "htest")
  )
}

#' Narayan Bounds Test
#'
#' This function performs the Narayan test, which is designed to assess
#' cointegration using critical values specifically tailored for small sample
#' sizes. Unlike traditional cointegration tests that may rely on asymptotic
#' distributions, the Narayan test adjusts for the limitations of small samples,
#'  providing more accurate results in such contexts. This makes the test
#'  particularly useful for studies with fewer observations, as it accounts for
#'  sample size constraints when determining the presence of a long-term
#'  equilibrium relationship between variables.
#'
#' @param kardl_model An object of class \code{kardl_lm} produced by
#'   \code{\link{kardl}} or an object of class \code{kardl_longrun}
#'   produced by \code{\link{kardl_longrun}}.
#' @param case Numeric or character. Specifies the case of the test to be used
#' in the function.
#' Acceptable values are 1, 2, 3, 4, 5, and "auto". If "auto" is chosen, the
#' function determines the case automatically based on the model's
#' characteristics. Invalid values will result in an error.
#'
#' \itemize{
#' \item \code{1}: No intercept and no trend. This case is not supported by the
#' Narayan test.
#' \item \code{2}: Restricted intercept and no trend.
#' \item \code{3}: Unrestricted intercept and no trend.
#' \item \code{4}: Unrestricted intercept and restricted trend.
#' \item \code{5}: Unrestricted intercept and unrestricted trend.
#' }
#'
#'
#' @param signif_level Character or numeric. Specifies the significance level to
#'  be used in the function.
#' Acceptable values are "auto", "0.10", "0.1", "0.05", "0.025", and "0.01".
#' If a numeric value is provided, it will be converted to a character string.
#'
#' When \code{"auto"} is selected, the function determines the significance
#' level sequentially, starting from the most stringent level (\code{"0.01"})
#' and proceeding to \code{"0.025"}, \code{"0.05"}, and \code{"0.10"} until a
#' suitable level is identified.
#' Invalid values will result in an error.
#'
#' @param ... Additional arguments (currently not used).
#'
#' @inheritSection pssf Hypothesis testing
#' @inherit pssf return
#' @importFrom car linearHypothesis
#'
#' @srrstats {G1.0} This function cites the original source of the
#' Narayan (2005) bounds testing approach for cointegration analysis in small
#' samples, which provides critical values specifically designed for such
#' contexts.
#' @srrstats {G1.3}  Statistical terms such as NULL hypothesis, alternative
#' hypothesis, test statistic are defined in the context of the Narayan (2005)
#' bounds testing approach for cointegration analysis in small samples.
#' @srrstats {G2.0} Input argument `kardl_model` is validated as a `kardl_lm`
#' object; `case` and `signif_level` are validated against acceptable values
#' before the test is performed.
#' @srrstats {G2.3} The `case` argument is validated against
#' `c(2,3,4,5,"auto")`; `signif_level` against the set of accepted strings.
#' @srrstats {TS2.3} The documentation notes that the Narayan test is designed
#' for small samples (n < 100) and suggests `pssf()` for larger samples.
#' @references Narayan, P. K. (2005). The saving and investment nexus for China:
#'  evidence from cointegration tests. Applied economics, 37(17), 1979-1990.
#' @export
#' @seealso \code{\link{pssf}}  \code{\link{psst}}  \code{\link{ecm}}
#' @examples

#' kardl_model<-kardl(
#'                    DriversKilled ~ PetrolPrice + drivers + asym(PetrolPrice)+
#'                        deterministic(law)+trend,
#'                    Seatbelts,
#'                    mode=c(1,2,3,0))
#' my_test<-narayan(kardl_model, case=3, signif_level="auto")
#' # Getting the results of the test.
#' my_test
#' # Getting details of the test.
#' my_summary<-summary(my_test)
#' my_summary
#'
#' # Getting the critical values of the test.
#' kardl_extract(my_summary, what = "critical_values")
#'
#'
#'
#'
#' # Using magrittr :
#'
#' @examplesIf requireNamespace("magrittr", quietly = TRUE)
#' library(magrittr)
#' Seatbelts %>%
#'   kardl(
#'     DriversKilled ~ PetrolPrice + drivers + asym(PetrolPrice) +
#'       deterministic(law) + trend,
#'     mode = c(1, 2, 3, 0), data = .
#'   ) %>%
#'   narayan()
#'
#' # Getting details of the test results using magrittr:
#' Seatbelts %>%
#'   kardl(
#'     DriversKilled ~ PetrolPrice + drivers + asym(PetrolPrice) +
#'       deterministic(law) + trend,
#'     mode = c(1, 2, 3, 0), data = .
#'   ) %>%
#'   narayan() %>%
#'   summary()
#'
narayan <- function(kardl_model,
                    case = "auto", signif_level = "auto", ...) {
  UseMethod("narayan")
}

#' @exportS3Method
narayan.default <- function(kardl_model,
                            case = "auto", signif_level = "auto", ...) {
  stop(
    "narayan() requires a kardl_lm object. ",
    "Please estimate a model using kardl() first.",
    call. = FALSE
  )
}

#' @export
#' @method narayan kardl_longrun
#'
narayan.kardl_longrun <- function(kardl_model,
                                  case = "auto", signif_level = "auto", ...) {
  narayan.kardl_lm(
    kardl_model = kardl_model$original_model,
    case = case,
    signif_level = signif_level,
    ...
  )
}


#' Narayan Bounds Test
#'
#' @param kardl_model An object of class \code{kardl_lm} produced by
#'   \code{\link{kardl}} or an object of class \code{kardl_longrun}
#'   produced by \code{\link{kardl_longrun}}.
#' @param case Numeric or character. Specifies the case of the test to be used
#' in the function.
#' @param signif_level Character or numeric. Specifies the significance level to
#'  be used in the function.
#' @param ... Additional arguments passed to methods.
#' @return A `kardl_test` object.
#' @noRd
#' @export
#' @method narayan kardl_lm
narayan.kardl_lm <- function(kardl_model,
                             case = "auto", signif_level = "auto", ...) {
  if (!case %in% c(2, 3, 4, 5, "auto")) {
    stop(
      "Invalid 'case' specification. ",
      "Please choose from: 2, 3, 4, 5, or 'auto'.",
      call. = FALSE
    )
  }
  if (is.numeric(signif_level)) {
    signif_level <- as.character(signif_level)
  }
  if (!signif_level %in% c("auto", "0.10", "0.1", "0.05", "0.01")) {
    stop(
      "Invalid significance level. ",
      "Valid options are: 'auto', 0.10, 0.05, or 0.01.",
      call. = FALSE
    )
  }
  if (kardl_model$est_info$type == "ecm") {
    stop(
      "Narayan F test is not applicable for ECM models. Please use ",
      "'psst()' function for residual-based tests.",
      call. = FALSE
    )
  }

  notes_array <- c()
  if (kardl_model$est_info$time_span >= 100) {
    notes_array <- c(
      notes_array,
      paste0(
        "The Narayan F-test is designed for small samples. ",
        "Your model uses only ",
        kardl_model$est_info$time_span,
        " observations. ",
        "For greater accuracy with large samples, consider pssf() function. "
      )
    )
  }

  if (case == "auto") {
    # Default case is 5, which means unrestricted intercept and
    # unrestricted trend
    case <- 3
  }

  if (kardl_model$extracted_info$no_constant) {
    notes_array <- c(
      notes_array,
      paste0(
        "No constant term is included in the ",
        "model. The Narayan test does not support models ",
        "without a constant. ",
        "Please include a constant term."
      )
    )
  }

  if (case == 1) {
    # if case is 1, it means that the model has a constant.
    # If the constant is not included, it should be set to 2.
    if (!kardl_model$extracted_info$trend) {
      case <- 2
      notes_array <- c(
        notes_array,
        paste0(
          "No trend is used in the model. ",
          "The case was set to 2."
        )
      )
    } else {
      # if case is 1, it means that the model has a constant and no trend.
      # If the trend is included, it should be set to 5.
      case <- 5
      notes_array <- c(
        notes_array,
        paste0(
          "Both constant and trend are included. Case automatically ",
          "adjusted to 5 (unrestricted intercept and trend)."
        )
      )
    }
  } else if (case > 3) {
    # if case is greater than 3, it means that the model has a trend.
    # If the trend is not included, it should be set to 3.
    if (!kardl_model$extracted_info$trend) {
      case <- 3
      notes_array <- c(
        notes_array,
        paste0(
          "No trend is included. Case automatically adjusted to 3 ",
          "(unrestricted intercept, no trend)."
        )
      )
    }
  } else if (case < 4 && kardl_model$extracted_info$trend) {
    # if case is less than 4, it means that the model does not have a trend.
    # If the trend is included, it should be set to 4.
    case <- 5
    notes_array <- c(
      notes_array,
      paste0(
        "Trend detected in the model. Case automatically adjusted to 5 ",
        "(unrestricted intercept and trend)."
      )
    )
  }

  longrun_names <- replace_lag_var(
    .kardl_settings_env$long_coef,
    kardl_model$extracted_info$long_run_vars,
    1
  )
  # check if case is 1,3,5
  if (case == 2) {
    longrun_names <- c(longrun_names, "(Intercept)")
  }
  if (case == 4) {
    longrun_names <- c(longrun_names, "trend")
  }

  # Null Hypothesis (H0)
  kisit <- c(paste(unlist(longrun_names), "=0"))

  f_model <- linearHypothesis(kardl_model, kisit, test = "F")
  pss_f_stat <- f_model$F[2]
  sayi <- length(kardl_model$extracted_info$long_run_vars) - 1

  structure(
    list(
      type = "cointegration",
      case = case,
      statistic = c("F" = pss_f_stat),
      method = "Narayan F Test for Cointegration",
      alternative = "Cointegrating relationship exists",
      data.name = deparse(substitute(model)),
      sample.size = kardl_model$est_info$time_span,
      hypotheses = kardl_hypotheses(longrun_names),
      var_names = longrun_names,
      k = sayi,
      n = kardl_model$est_info$end,
      sig = signif_level,
      notes = notes_array,
      test.func = "narayan"
    ),
    class = c("kardl_test", "htest")
  )
}


#' Pesaran, Shin, and Smith t Bounds Test
#'
#' This function performs the Pesaran t Bound test
#'
#' This function performs the Pesaran, Shin, and Smith (PSS) t Bound test, which
#'  is used to detect the existence of a long-term relationship (cointegration)
#'  between variables in an autoregressive distributed lag (ARDL) model. The t
#'  Bound test specifically focuses on the significance of the coefficient of
#'  the lagged dependent variable, helping to assess whether the variable
#'  reverts to its long-term equilibrium after short-term deviations. The test
#'  provides critical values for both upper and lower bounds. If the t-statistic
#'   falls within the appropriate range, it confirms the presence of
#'   cointegration. This test is particularly useful when working with datasets
#'   containing both stationary and non-stationary variables.
#' @inheritParams pssf
#' @param vcov A variance-covariance matrix to be used for the test. If not
#' provided, the default variance-covariance matrix from the model will be used.
#' @param ... Additional arguments.
#'
#' @section Hypothesis testing:
#' The PSS t Bound test evaluates the null hypothesis that the long-run
#' coefficients of the model are equal to zero against the alternative
#' hypothesis that at least one of them is non-zero. The test is conducted under
#'  different cases, depending on the model specification.
#'
#'
#'  \deqn{
#'   \Delta {y}_t =  \psi  + \varphi t  + \eta _0   {y}_{t-1}  +
#'   \sum_{i=1}^{k} {  \eta _i   {x}_{i,t-1} }  +
#'   \sum_{j=1}^{p} { \gamma_{j}  \Delta {y}_{t-j} }+
#'   \sum_{i=1}^{k} {\sum_{j=0}^{q_i} { \beta_{ij}   \Delta {x}_{i,t-j} } }+ e_t
#' }
#'
#'   \deqn{\mathbf{H_{0}:} \eta_0   = 0}
#'   \deqn{\mathbf{H_{1}:} \eta_{0}  \neq 0}
#'
#'
#' @return The function returns an object of class "htest" containing the
#' following components:
#' \describe{
#' \item{statistic}{The calculated t-statistic for the test.}
#' \item{method}{A description of the test performed.}
#' \item{data.name}{The name of the data used in the test.}
#' \item{k}{The number of independent variables in the model.}
#' \item{notes}{Any notes or warnings related to the test results, such as
#' sample size considerations or adjustments made to the case based on model
#' characteristics.}
#' \item{sig}{The significance level used for the test, either specified by the
#' user or determined automatically.}
#' \item{alternative}{The alternative hypothesis being tested.}
#' \item{case}{The case used for the test, either specified by the user or
#' determined automatically based on the model's characteristics.}
#' }
#'
#' @srrstats {G1.0} This function cite the original source of the test, which is
#'  Pesaran, M. H., Shin, Y. and Smith, R. (2001), "Bounds Testing Approaches to
#'   the Analysis of Level Relationship", Journal of Applied Econometrics, 16(3)
#'   , 289-326.
#' @srrstats {G1.3}  Statistical terms such as NULL hypothesis, alternative
#' hypothesis, test statistic are defined in the documentation.
#' @srrstats {G2.0} Input argument `kardl_model` is validated as a `kardl_lm`
#' object; `case` and `signif_level` are validated before the test is performed.
#' @srrstats {G2.3} The `case` argument is validated against
#' `c(1,2,3,4,5,"auto")`; `signif_level` against the set of accepted character
#' strings.
#' @srrstats {G3.1} Variance-covariance matrix is used to calculate the
#' t-statistic for the test.
#' @srrstats {TS2.2} The PSS t-test evaluates the error-correction coefficient,
#' which is relevant to the cointegration and stationarity assumptions of the
#' ARDL framework.
#'
#' @export
#' @seealso \code{\link{pssf}}   \code{\link{ecm}}  \code{\link{narayan}}
#' @importFrom stats vcov
#' @importFrom lmtest coeftest
#' @references Pesaran, M. H., Shin, Y. and Smith, R. (2001), "Bounds Testing
#' Approaches to the Analysis of Level Relationship", Journal of Applied
#' Econometrics, 16(3), 289-326.
#' @examples

#' kardl_model<-kardl(
#'                    DriversKilled~PetrolPrice+drivers+asym(PetrolPrice)+
#'                          deterministic(law)+trend,
#'                    Seatbelts,
#'                    mode=c(1,2,3,0))
#' my_test<-psst(kardl_model)
#' # Getting the results of the test.
#' my_test
#' # Getting details of the test.
#' my_summary<-summary(my_test)
#' my_summary
#'
#' # Getting the critical values of the test.
#' kardl_extract(my_summary, what = "critical_values")
#'
#' # Using a robust variance-covariance matrix for the test:
#' if (requireNamespace("sandwich", quietly = TRUE)) {
#'     vcov_matrix <- sandwich::vcovHC(kardl_model)
#'     my_test_robust <- psst(kardl_model,
#'                            case = 1,
#'                            signif_level =0.01,
#'                            vcov = vcov_matrix)
#'   my_test_robust
#' }
#'
#'
#' # Using magrittr :
#'
#' @examplesIf requireNamespace("magrittr", quietly = TRUE)
#' library(magrittr)
#' Seatbelts %>%
#'   kardl(
#'     DriversKilled ~ PetrolPrice + drivers + asym(PetrolPrice) +
#'       deterministic(law) + trend,
#'     mode = c(1, 2, 3, 0), data = .
#'   ) %>%
#'   psst()
#'
#' # Getting details of the test results using magrittr:
#' Seatbelts %>%
#'   kardl(
#'     DriversKilled ~ PetrolPrice + drivers + asym(PetrolPrice) +
#'       deterministic(law) + trend,
#'     mode = c(1, 2, 3, 0), data = .
#'   ) %>%
#'   psst() %>%
#'   summary()
#'
psst <- function(kardl_model, case = "auto",
                 signif_level = "auto", vcov = NULL, ...) {
  UseMethod("psst")
}
#' Default method for psst function
#'
#' This function is called when the input object is not of class 'kardl_lm'.
#'
#' @noRd
#' @export
psst.default <- function(kardl_model, case = "auto",
                         signif_level = "auto", vcov = NULL, ...) {
  stop(
    "psst() requires a kardl_lm object. ",
    "Please estimate a model using kardl() first.",
    call. = FALSE
  )
}

#' @export
#' @method psst kardl_longrun
psst.kardl_longrun <- function(kardl_model, case = "auto",
                               signif_level = "auto", vcov = NULL, ...) {
  psst.kardl_lm(
    kardl_model = kardl_model$original_model,
    case = case,
    signif_level = signif_level, vcov = vcov,
    ...
  )
}

#' Pesaran, Shin, and Smith t Bounds Test
#'
#' This function performs the Pesaran, Shin, and Smith (PSS) t Bound test to
#' assess the presence of a long-term relationship (cointegration) between
#' variables in the context of an autoregressive distributed lag (ARDL) model.
#' @param kardl_model An object of class \code{kardl_lm} produced by
#'   \code{\link{kardl}} or an object of class \code{kardl_longrun}
#'   produced by \code{\link{kardl_longrun}}.
#' @param ... Additional arguments passed to methods.
#' @return A `kardl_test` object.
#' @method psst kardl_lm
#' @noRd
#' @export
psst.kardl_lm <- function(kardl_model, case = "auto", signif_level = "auto",
                          vcov = NULL,
                          ...) {
  notes_array <- c()
  if (!case %in% c(1, 2, 3, 4, 5, "auto")) {
    stop(
      "Invalid 'case' specification. ",
      "Please choose from: 1, 2, 3, 4, 5, or 'auto'.",
      call. = FALSE
    )
  }
  if (is.numeric(signif_level)) {
    signif_level <- as.character(signif_level)
  }
  if (!signif_level %in% c("auto", "0.10", "0.1", "0.05", "0.025", "0.01")) {
    stop(
      "Invalid significance level. ",
      "Valid options are: 'auto', 0.10, 0.05, 0.025, or 0.01.",
      call. = FALSE
    )
  }

  if (case == "auto") {
    # Default case is 3, which means unrestricted intercept and no trend
    case <- 3
  }

  if (kardl_model$extracted_info$no_constant) {
    if (case > 1) {
      notes_array <- c(
        notes_array,
        paste0(
          "No constant term is included in the model. Case automatically",
          " adjusted to 1 (no intercept, no trend)."
        )
      )
    }
    case <- 1
  } else if (kardl_model$extracted_info$trend) {
    if (case < 4) {
      notes_array <- c(
        notes_array,
        paste0(
          "Trend detected in the model. Case automatically adjusted to 5",
          " (unrestricted intercept and trend)."
        )
      )
    }
    case <- 5
  } else {
    if (case != 3) {
      # model has not trend but have constant
      notes_array <- c(
        notes_array,
        paste0(
          "No trend is included. Case automatically adjusted to 3 ",
          "(unrestricted intercept, no trend)."
        )
      )
    }
    case <- 3
  }

  if (!is.null(vcov)) {
    vcov_matrix <- vcov
  } else {
    vcov_matrix <- stats::vcov(kardl_model, ...)
  }
  coef_test <- lmtest::coeftest(kardl_model, vcov = vcov_matrix, ...)

  if (kardl_model$est_info$type == "ecm") {
    test_var_name <- "EcmRes"
  } else {
    test_var_name <- replace_lag_var(
      .kardl_settings_env$long_coef,
      kardl_model$extracted_info$dependent_var,
      1
    )
  }

  pss_t_stat <- coef_test[test_var_name, 3]
  k <- length(kardl_model$extracted_info$long_run_vars) - 1

  structure(
    list(
      type = "cointegration",
      case = case,
      statistic = c("t" = pss_t_stat),
      method = "Pesaran-Shin-Smith (PSS) Bounds t-test for cointegration",
      alternative = "Cointegrating relationship exists",
      data.name = deparse(substitute(model)),
      sample.size = kardl_model$est_info$time_span,
      hypotheses = kardl_hypotheses(test_var_name),
      var_names = test_var_name,
      k = k,
      n = kardl_model$est_info$end,
      sig = signif_level,
      notes = notes_array,
      test.func = "psst"
    ),
    class = c("kardl_test", "htest")
  )
}

#' Print and summary methods for kardl objects
#' @description
#' These methods provide custom print and summary outputs for various kardl
#' object classes, including `
#' kardl_mplier`, `kardl_boot`, `kardl_longrun`, `kardl_symmetric`, and
#' `kardl_test`. The print methods display key information about the object,
#' while the summary methods provide detailed summaries of the results, i
#' ncluding statistical tests and decisions.
#'
#'
#' @export
#' @method print kardl_mplier
#' @noRd
print.kardl_mplier <- function(x, ...) {
  cat("kardl Dynamic Multiplier Object\n")
  cat("Horizon:", x$horizon, "\n")
  print(x$mpsi, ...)
  invisible(x)
}


#' @export
#' @method summary kardl_mplier
#' @noRd
summary.kardl_mplier <- function(object, ...) {
  out <- list(
    horizon = object$horizon,
    summary = summary(object$mpsi, ...)
  )
  class(out) <- "summary.kardl_mplier"
  out
}

#' @export
#' @method print summary.kardl_mplier
#' @noRd
print.summary.kardl_mplier <- function(x, ...) {
  cat("Summary of Dynamic Multipliers\n")
  cat("Horizon:", x$horizon, "\n\n")
  print(x$summary, ...)
  invisible(x)
}

#' @export
#' @method print kardl_boot
#' @noRd
print.kardl_boot <- function(x, ...) {
  cat("kardl Bootstrap Results\n")
  cat("Confidence level:", x$level, "%\n")
  cat("Horizon:", x$horizon, "\n")
  print(x$mpsi, ...)
  invisible(x)
}

#' @export
#' @method summary kardl_boot
#' @noRd
summary.kardl_boot <- function(object, ...) {
  out <- list(
    horizon = object$horizon,
    summary = summary(object$mpsi, ...)
  )
  class(out) <- "summary.kardl_boot"
  out
}

#' @export
#' @method print summary.kardl_boot
#' @noRd
print.summary.kardl_boot <- function(x, ...) {
  cat("Summary of Dynamic Multipliers\n")
  cat("Horizon:", x$horizon, "\n\n")
  print(x$summary, ...)
  invisible(x)
}

#' @export
#' @method plot kardl_mplier
#' @srrstats {TS5.0} A plot method is implemented for `kardl_mplier` objects
#' returned by `mplier()`.
#' @srrstats {TS5.1} The horizontal axis of multiplier plots is labelled as the
#' horizon or time-step index.
#' @srrstats {TS5.2} The horizon variable is plotted on the horizontal axis in
#' dynamic multiplier visualisations.
#' @srrstats {TS5.3} Multiplier plots display the horizon index used in the
#' model output.
#' @srrstats {TS5.8} Multiplier plots distinguish positive and negative shock
#' components by variable where asymmetric effects are present.
#' @noRd
plot.kardl_mplier <- function(x, variables = "all", title = NULL, ...) {
  mpsi <- x$mpsi
  plots <- list()

  if (all(variables == "all")) {
    for (variable in x$vars$independent_vars) {
      plots[[variable]] <- mplierggplot(mpsi, x$vars, variable, title)
    }
  } else {
    for (variable in variables) {
      if (!variable %in% x$vars$independent_vars) {
        stop(
          variable,
          " is not exits among independent variables!",
          call. = FALSE
        )
      } else {
        plots[[variable]] <- mplierggplot(mpsi, x$vars, variable, title)
      }
    }
  }
  if (length(plots) > 1) {
    warning(
      "Multiple variables selected. Only the first one will be plotted.",
      call. = FALSE, ...
    )
  }
  print(plots[[1]])
  invisible(plots)
}

#' @export
#' @method print kardl_hypotheses
print.kardl_hypotheses <- function(x, ...) {
  cat("\nHypotheses:\n")
  vars <- names(x[[1]])
  if (is.null(vars)) {
    cat(strwrap(x$H0, width = getOption("width")), "\n")
    cat(strwrap(x$H1, width = getOption("width")), "\n")
  } else {
    for (v in vars) {
      cat("\nVariable:", v, "\n")
      if (!is.null(x$H0[[v]])) {
        cat(
          strwrap(paste0("  H0: ", x$H0[[v]]), width = getOption("width")),
          "\n"
        )
      }
      if (!is.null(x$H1[[v]])) {
        cat(strwrap(
          paste0("  H1: ", enc2utf8(x$H1[[v]])),
          width = getOption("width")
        ), "\n\n")
      }
    }
  }
  invisible(x)
}


#' @export
#' @method print summary_kardl_longrun
#' @noRd
print.summary_kardl_longrun <- function(x, ...) {
  cat("\nCall:\n", paste(deparse(x$call), collapse = "\n"), "\n", sep = "")

  cat("\nEstimation type:\n")
  cat(x$estimation_type, "\n")

  cat("\nCoefficients:\n")
  printCoefmat(x$coefficients, ...)


  invisible(x)
}

#' @export
#' @method print kardl_symmetric
#' @noRd
print.kardl_symmetric <- function(x, ...) {
  cat("\n", "KARDL Symmetry Test Results\n", sep = "")
  if (!is.null(x$long_wald_summary) && nrow(x$long_wald_summary) > 0) {
    print(x$long_wald_summary, ...)
    cat("\n")
  }

  if (!is.null(x$short_wald_summary) && nrow(x$short_wald_summary) > 0) {
    print(x$short_wald_summary, ...)
    cat("\n")
  }
  invisible(x)
}

#' @export
#' @method summary kardl_symmetric
#' @noRd
summary.kardl_symmetric <- function(object, level = 0.05, ...) {
  decision <- list()
  pval_type <- ifelse(object$type == "Chisq", "Pr(>Chisq)", "Pr(>F)")
  if (!is.null(object$long_wald_summary)) {
    decision$long_run <- list()
    for (v in rownames(object$long_wald_summary)) {
      temp_txt <- paste0(
        "Fail to Reject H0 at ",
        level * 100,
        "% level. ",
        "Indicating long-run symmetry for variable ",
        v,
        "."
      )
      if (object$long_wald_summary[v, pval_type] < level) {
        temp_txt <- paste0(
          "Reject H0 at ",
          level * 100,
          "% level. ",
          "Indicating long-run asymmetry for variable ",
          v,
          "."
        )
      }
      decision$long_run[[v]] <- temp_txt
    }
  }

  if (!is.null(object$short_wald_summary)) {
    decision$short_run <- list()
    for (v in rownames(object$short_wald_summary)) {
      temp_txt <- paste0(
        "Fail to Reject H0 at ",
        level * 100,
        "% level. ",
        "Indicating short-run symmetry for variable ",
        v,
        "."
      )
      if (object$short_wald_summary[v, pval_type] < level) {
        temp_txt <- paste0(
          "Reject H0 at ",
          level * 100,
          "% level. ",
          "Indicating short-run asymmetry for variable ",
          v,
          "."
        )
      }
      decision$short_run[[v]] <- temp_txt
    }
  }
  out <- list(
    long_wald_summary = object$long_wald_summary,
    short_wald_summary = object$short_wald_summary,
    long_hypotheses = object$long_hypotheses,
    short_hypotheses = object$short_hypotheses,
    type = object$type,
    decision = decision
  )
  class(out) <- "summary.kardl_symmetric"
  out
}

#' @export
#' @method print summary.kardl_symmetric
#' @noRd
print.summary.kardl_symmetric <- function(x,
                                          digits = getOption("digits"), ...) {
  if (identical(x$type, "Chisq")) {
    stat_col <- "Chisq"
    p_col <- "Pr(>Chisq)"
    title_stat <- "Chi-squared"
  } else {
    stat_col <- "F value"
    p_col <- "Pr(>F)"
    title_stat <- "F"
  }

  print_block <- function(wald, hypotheses, decision, title) {
    if (is.null(wald) || nrow(wald) == 0) {
      return(invisible(NULL))
    }

    cat("\n", title, "\n", sep = "")
    cat(strrep("-", nchar(title)), "\n", sep = "")

    tab <- as.data.frame(wald[, c(stat_col, p_col), drop = FALSE])
    names(tab) <- c(title_stat, p_col)

    printCoefmat(
      as.matrix(tab),
      digits = digits,
      signif.stars = TRUE,
      has.Pvalue = TRUE,
      P.values = TRUE,
      ...
    )

    cat("\nHypotheses and decisions:\n")

    for (v in rownames(wald)) {
      cat("\nVariable:", v, "\n")
      cat(strwrap(
        paste0("  H0: ", hypotheses$H0[[v]]),
        width = getOption("width")
      ), "\n")
      cat(strwrap(
        paste0("  H1: ", enc2utf8(hypotheses$H1[[v]])),
        width = getOption("width")
      ), "\n")
      cat(
        strwrap(
          paste0("  Decision: ", decision[[v]]),
          width = getOption("width")
        ),
        "\n"
      )
    }

    invisible(NULL)
  }

  print_block(
    x$long_wald_summary,
    x$long_hypotheses,
    x$decision$long_run,
    "Long-run symmetry tests"
  )

  if (!is.null(x$long_wald_summary) && !is.null(x$short_wald_summary)) {
    cat("\n")
  }

  print_block(
    x$short_wald_summary,
    x$short_hypotheses,
    x$decision$short_run,
    "Short-run symmetry tests"
  )

  invisible(x)
}

#' @export
#' @noRd
print.kardl_test_summary <- function(x, digits = getOption("digits"), ...) {
  cat("\n========================================")
  cat("\nKARDL Cointegration Test Results")
  cat("\n========================================\n")

  stat_name <- names(x$statistic)

  sig_level <- x$significance_level
  crit_lower <- x$cr_vals[sig_level, "L"]
  crit_upper <- x$cr_vals[sig_level, "U"]

  # Decision
  cat("\n Decision:", x$decision)

  # Test statistic
  cat("\n\n Test Statistic:")
  cat(
    sprintf(
      paste0("\n  %s: %.", digits, "f"),
      stat_name,
      unname(x$statistic)
    )
  )

  # Critical values
  cat("\n\n Critical Values (Lower & Upper Bounds):\n")

  cv_table <- x$cr_vals
  rownames(cv_table) <- paste0(
    as.numeric(rownames(cv_table)) * 100,
    "%"
  )
  cv_output <- capture.output(print(cv_table, digits = digits, ...))
  cv_output <- paste0("  ", cv_output)
  cat(cv_output, sep = "\n")

  # Comparison
  cat("\n\n Comparison:")

  if (abs(x$statistic) < abs(crit_lower)) {
    cat(
      sprintf(
        paste0(
          "\n  At the %.0f%% significance level, ",
          "%s (%.", digits, "f) is below the lower bound (%s)."
        ),
        as.numeric(sig_level) * 100,
        stat_name,
        abs(x$statistic),
        abs(crit_lower)
      )
    )

    cat(
      "\n  This indicates that the variables do not move together in",
      " the long run."
    )
    cat("\n  Conclusion: No evidence of cointegration.")
  } else if (abs(x$statistic) > abs(crit_upper)) {
    cat(
      sprintf(
        paste0(
          "\n  At the %.0f%% significance level, ",
          "%s (%.", digits, "f) exceeds the upper bound (%s)."
        ),
        as.numeric(sig_level) * 100,
        stat_name,
        abs(x$statistic),
        abs(crit_upper)
      )
    )

    cat(
      "\n  This indicates that the variables tend to move together over",
      " time."
    )
    cat(
      "\n  Conclusion: There is strong evidence of a long-run relationship",
      " (cointegration)."
    )
  } else {
    cat(
      sprintf(
        paste0(
          "\n  At the %.0f%% significance level, ",
          "%s (%.", digits, "f) falls between the lower bound (%s) and",
          " upper bound (%s)."
        ),
        as.numeric(sig_level) * 100,
        stat_name,
        abs(x$statistic),
        abs(crit_lower),
        abs(crit_upper)
      )
    )

    cat(
      "\n  This is an inconclusive zone where we cannot make a definitive",
      " judgment."
    )
    cat("\n  Conclusion: The test does not provide clear evidence either way.")
  }

  cat("\n\n Hypotheses:\n")

  cat(
    strwrap(paste0("  ", x$hypotheses$H0), width = getOption("width")),
    "\n"
  )
  cat(
    strwrap(paste0("  ", x$hypotheses$H1, "\n"), width = getOption("width")),
    "\n"
  )

  cat("\n Model Details:")
  cat(sprintf("\n  Number of regressors (k): %d", x$k))
  cat("\n  Case:", x$case, "\n")

  if (!is.null(x$notes) && nzchar(x$notes)) {
    cat("\n\n Note:")
    cat(strwrap(paste0("\n ", x$notes), width = getOption("width")), sep = "")
  }

  cat("\n========================================\n")

  invisible(x)
}


#' @export
#' @method print kardl_long_run
#' @noRd
print.kardl_long_run <- function(x, ...) {
  cat("\n========================================\n")
  cat("Long-run multiplier estimate\n")
  cat("\n========================================\n")

  NextMethod() # continues with normal coefficient printing
  cat("Note:", strwrap(attr(x, "note"), width = getOption("width")), "\n\n")
  invisible(x)
}

#' @export
#' @method plot kardl_long_run
#' @noRd
plot.kardl_long_run <- function(x, ...) {
  warning(
    "Residual diagnostic plots are meaningless for long-run estimators",
    call. = FALSE, ...
  )
  if (interactive()) {
    ans <- readline("Show plots anyway? (y/n): ")
    if (tolower(ans) != "y") {
      return(invisible(NULL))
    }
  }
  NextMethod("plot")
}


#' @export
#' @method print kardl_lm
#' @noRd
print.kardl_lm <- function(x, ...) {
  cat("Optimal lags for each variable (", x$args_info$criterion, "):\n")
  cat(
    toString(sprintf("%s: %d", names(x$lag_info$opt_lag), x$lag_info$opt_lag)),
    "\n"
  )
  NextMethod()
  if (!is.null(x$notes)) {
    cat("\nNotes:\n")
    for (note in x$notes) {
      cat(strwrap(paste0("   \u2022 ", note), width = getOption("width")),
        "\n",
        sep = ""
      )
    }
    cat("\n")
  }
  invisible(x)
}

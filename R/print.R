#' Print method for kardl objects with wrapped output
#'
#' This function captures the output of the print method for kardl objects and
#' wraps lines that exceed a specified width. It is useful for ensuring that
#' printed output is readable and fits within a desired width, especially
#' in console environments.
#'
#' @param x The kardl object to be printed.
#' @param width The maximum width of the output lines. Lines exceeding this
#'  width will be wrapped. Default is 80 characters.
#' @param ... Additional arguments passed to the print method of the kardl
#'  object.
#'
#' @return Invisibly returns the original kardl object after printing.
#' @noRd

kardl_print_wrapped <- function(expr) {
  wrap_width <- kardl_get("print_wrap")

  if (is.null(wrap_width)) {
    force(expr)
  } else {
    out <- capture.output(force(expr))

    out <- unlist(lapply(out, function(line) {
      if (nchar(line, type = "width") > wrap_width) {
        strwrap(line, width = wrap_width, exdent = 2)
      } else {
        line
      }
    }), use.names = FALSE)

    cat(out, sep = "\n")
  }

  invisible(NULL)
}

#' Concatenates and prints text with optional wrapping
#'
#' This function concatenates the provided text arguments and prints them to
#' the console. If a wrapping width is set using
#' `kardl_set("print_wrap", width)`, the output will be wrapped to fit within
#' that width. Otherwise, it will print normally.
#'
#' @param ... Text arguments to be concatenated and printed.
#' @param sep A character string to separate the terms. Default is an empty
#' string.
#' @param fill A logical value indicating whether to fill the output. Default
#' is FALSE.
#' @param indent An integer specifying the number of spaces to indent the
#' output.
#'
#' @return Invisibly returns NULL after printing the concatenated text.
#' @noRd
kardl_cat <- function(..., sep = "\n", fill = FALSE, indent = 0L,
                      exdent = indent) {
  wrap_width <- kardl_get("print_wrap")

  txt <- paste(...)

  if (is.null(wrap_width)) {
    cat(strrep(" ", indent), txt, "\n", sep = "", fill = fill)
  } else {
    cat(
      strwrap(txt,
        width = wrap_width, indent = indent,
        exdent = exdent
      ),
      sep = sep,
      fill = fill
    )
  }

  invisible(NULL)
}
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
  kardl_print_wrapped({
    cat("kardl Dynamic Multiplier Object\n")
    cat("Horizon:", x$horizon, "\n")
    print(x$mpsi, ...)
  })
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
  kardl_print_wrapped({
    cat("Summary of Dynamic Multipliers\n")
    cat("Horizon:", x$horizon, "\n\n")
    print(x$summary, ...)
  })
  invisible(x)
}

#' @export
#' @method print kardl_boot
#' @noRd
print.kardl_boot <- function(x, ...) {
  kardl_print_wrapped({
    cat("kardl Bootstrap Results\n")
    cat("Confidence level:", x$level, "%\n")
    cat("Horizon:", x$horizon, "\n")
    print(x$mpsi, ...)
  })
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
  kardl_print_wrapped({
    cat("Summary of Dynamic Multipliers\n")
    cat("Horizon:", x$horizon, "\n\n")
    print(x$summary, ...)
  })
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
  kardl_print_wrapped({
    cat("Hypotheses:\n\n")
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
  })
  invisible(x)
}


#' @export
#' @method print summary_kardl_longrun
#' @noRd
print.summary_kardl_longrun <- function(x, ...) {
  kardl_print_wrapped({
    kardl_cat("Call:\n", paste(deparse(x$call), collapse = "\n"), "\n",
      sep = ""
    )

    cat("\nEstimation type:\n")
    cat(x$estimation_type, "\n")

    cat("\nCoefficients:\n")
    printCoefmat(x$coefficients, ...)
  })

  invisible(x)
}

#' @export
#' @method print kardl_symmetric
#' @noRd
print.kardl_symmetric <- function(x, ...) {
  kardl_print_wrapped({
    cat("kardl Symmetry Test \n\n")
    if (!is.null(x$long_wald_summary) && nrow(x$long_wald_summary) > 0) {
      print(x$long_wald_summary, ...)
      cat("\n")
    }

    if (!is.null(x$short_wald_summary) && nrow(x$short_wald_summary) > 0) {
      print(x$short_wald_summary, ...)
      cat("\n")
    }
  })
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
  cat("Symmetry Test Summary\n\n")
  if (!is.null(x$long_wald_summary) && nrow(x$long_wald_summary) > 0) {
    kardl_print_wrapped({
      print(x$long_wald_summary, ...)
    })

    cat("\nHypotheses:\n\n")

    for (v in rownames(x$long_wald_summary)) {
      kardl_cat(v, indent = 2)
      kardl_cat(paste0("H0: ", x$long_hypotheses$H0[[v]]), indent = 3)
      kardl_cat(paste0("H1: ", enc2utf8(x$long_hypotheses$H1[[v]])), indent = 3)
      kardl_cat(paste0("Decision: ", x$decision$long_run[[v]]), indent = 3)
      cat("\n")
    }
  }

  if (!is.null(x$short_wald_summary) && nrow(x$short_wald_summary) > 0) {
    cat("\n")
    kardl_print_wrapped({
      print(x$short_wald_summary, ...)
    })

    cat("\nHypotheses:\n\n")
    for (v in rownames(x$short_wald_summary)) {
      kardl_cat(v, indent = 2)
      kardl_cat(paste0("H0: ", x$short_hypotheses$H0[[v]]), indent = 3)
      kardl_cat(paste0("H1: ", enc2utf8(x$short_hypotheses$H1[[v]])),
        indent = 3
      )
      kardl_cat(paste0("Decision: ", x$decision$short_run[[v]]), indent = 3)

      cat("\n")
    }
  }

  invisible(x)
}

#' @export
#' @noRd
print.kardl_test_summary <- function(x, digits = getOption("digits"), ...) {
  kardl_print_wrapped({
    cat("KARDL Cointegration Test Summary\n\n")
    kardl_cat(x$method)
    cat("\n")

    stat_name <- names(x$statistic)
    cat(
      sprintf(
        paste0("%s statistic = %.", digits, "f"),
        stat_name,
        unname(x$statistic)
      )
    )
    cat("\n\n")
    kardl_cat("Critical Values (Lower & Upper Bounds):", indent = 0)

    cv_table <- x$cr_vals
    rownames(cv_table) <- paste0(
      as.numeric(rownames(cv_table)) * 100,
      "%"
    )
    out <- capture.output(print(cv_table, digits = digits, ...))
    cat(paste0("  ", out), sep = "\n")

    sig_level <- x$significance_level
    crit_lower <- x$cr_vals[sig_level, "L"]
    crit_upper <- x$cr_vals[sig_level, "U"]

    cat("\n")
    # Decision
    cat("Decision:\n")
    kardl_cat(x$decision, indent = 2)


    cat("\n")
    cat("Comparison:\n")

    if (abs(x$statistic) < abs(crit_lower)) {
      kardl_cat(
        sprintf(
          paste0(
            "At the %.0f%% significance level, ",
            "%s (%.", digits, "f) is below the lower bound (%s)."
          ),
          as.numeric(sig_level) * 100,
          stat_name,
          abs(x$statistic),
          abs(crit_lower)
        ),
        indent = 2
      )

      kardl_cat(
        "This indicates that the variables do not move together in",
        " the long run.",
        indent = 2
      )

      kardl_cat("Conclusion: No evidence of cointegration.", indent = 2)
    } else if (abs(x$statistic) > abs(crit_upper)) {
      kardl_cat(
        sprintf(
          paste0(
            "At the %.0f%% significance level, ",
            "%s (%.", digits, "f) exceeds the upper bound (%s)."
          ),
          as.numeric(sig_level) * 100,
          stat_name,
          abs(x$statistic),
          abs(crit_upper)
        ),
        indent = 2
      )

      kardl_cat(
        "This indicates that the variables tend to move together over",
        " time.",
        indent = 2
      )

      kardl_cat(
        "Conclusion: There is strong evidence of a long-run relationship",
        " (cointegration).",
        indent = 2
      )
    } else {
      kardl_cat(
        sprintf(
          paste0(
            "At the %.0f%% significance level, ",
            "%s (%.", digits, "f) falls between the lower bound (%s) and",
            " upper bound (%s)."
          ),
          as.numeric(sig_level) * 100,
          stat_name,
          abs(x$statistic),
          abs(crit_lower),
          abs(crit_upper)
        ),
        indent = 2
      )

      kardl_cat(
        "This is an inconclusive zone where we cannot make a definitive",
        " judgment.",
        indent = 2
      )
      kardl_cat(
        "Conclusion: The test does not provide clear evidence ",
        "either way.",
        indent = 2
      )
    }

    cat("\nHypotheses:\n")

    kardl_cat(x$hypotheses$H0, indent = 2)
    kardl_cat(x$hypotheses$H1, indent = 2)

    cat("\nModel Details:\n")
    kardl_cat(sprintf("Number of regressors (k): %d", x$k), indent = 2)
    cat("  Case:", x$case, "\n")

    if (!is.null(x$notes) && nzchar(x$notes)) {
      cat("\nNote:\n")
      kardl_cat(x$notes, indent = 2)
    }
  })
  invisible(x)
}


#' @export
#' @method print kardl_long_run
#' @noRd
print.kardl_long_run <- function(x, ...) {
  kardl_print_wrapped({
    cat("Long-run multiplier estimate\n")

    NextMethod() # continues with normal coefficient printing
    kardl_cat("Note:", attr(x, "note"), "\n")
  })
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
  kardl_print_wrapped({
    kardl_cat("Optimal lags for each variable (", x$args_info$criterion, "):\n")
    kardl_cat(
      toString(sprintf(
        "%s: %d",
        names(x$lag_info$opt_lag),
        x$lag_info$opt_lag
      )),
      "\n"
    )
    NextMethod()
    if (!is.null(x$notes)) {
      cat("\nNotes:\n")
      for (note in x$notes) {
        kardl_cat(paste0("   \u2022 ", note), "\n", sep = "")
      }
      cat("\n")
    }
  })
  invisible(x)
}

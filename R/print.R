
#' @export
#' @method print kardl_mplier
print.kardl_mplier <- function(x, ...) {
  cat("kardl Dynamic Multiplier Object\n")
  cat("Horizon:", x$horizon, "\n")
  print(x$mpsi, ...)
}

#' @export
#' @method summary kardl_mplier
summary.kardl_mplier <- function(object, ...) {

  out <- list(
    horizon = object$horizon,
    summary=summary(object$mpsi)
  )
  class(out) <- "summary.kardl_mplier"
  out
}

#' @export
#' @method print summary.kardl_mplier
print.summary.kardl_mplier <- function(x, ...) {
  cat("Summary of Dynamic Multipliers\n")
  cat("Horizon:", x$horizon, "\n\n")
  print( x$summary, ...)

}

#' @export
#' @method print kardl_boot
print.kardl_boot <- function(x, ...) {
  cat("kardl Bootstrap Results\n")
  cat("Confidence level:", x$level, "%\n")
  cat("Horizon:", x$horizon, "\n")
  print(x$mpsi, ...)
}

#' @export
#' @method summary kardl_boot
summary.kardl_boot <- function(object, ...) {

  out <- list(
    horizon = object$horizon,
    summary=summary(object$mpsi)
  )
  class(out) <- "summary.kardl_boot"
  out
}

#' @export
#' @method print summary.kardl_boot
print.summary.kardl_boot <- function(x, ...) {
  cat("Summary of Dynamic Multipliers\n")
  cat("Horizon:", x$horizon, "\n\n")
  print( x$summary, ...)
}

#' @export
#' @method plot kardl_mplier
plot.kardl_mplier <- function(x, variables ="all" , ...) {
  mpsi <- x$mpsi
  plots<-list()

    if(all(variables == "all")){
      for (variable in  x$vars$independentVars) {
        plots[[variable]] <- mplierggplot(mpsi, x$vars,variable)
      }
    }else{
      for (variable in variables) {
        if(! variable %in% x$vars$independentVars){
          warning(paste0( variable," is not exits among independent variables!"), call.=F)
        }else{
          plots[[variable]] <- mplierggplot(mpsi, x$vars,variable)
        }
      }
    }
  for (p in plots) {
    print(p)
  }

}

#' @import stats
#' @export
#' @method  summary kardl_longrun
summary.kardl_longrun <- function(object, ...) {
  objcoef <- object$original_model$coefficients

  vcov_matrix = stats::vcov( object$original_model)

  my_dep <- object$depvar
  my_indep <- object$indepvars

  multipliers_coef <- object$coefficients

  A <- - 1 / objcoef[my_dep]

  lr_var <- sapply(names(multipliers_coef), function(i) {
    B <- objcoef[i] / (objcoef[my_dep]^2)
    (A^2) * vcov_matrix[i, i] +
      2 * A * B * vcov_matrix[i, my_dep] +
      (B^2) * vcov_matrix[my_dep, my_dep]
  })

  multipliers_coef_se <- sqrt(as.vector(lr_var))
  tvals <- multipliers_coef / multipliers_coef_se
  pvals <- 2 * stats::pt(-abs(tvals), df = stats::df.residual(object$original_model))

  coef_table <- cbind(
    Estimate = multipliers_coef,
    `Std. Error` = multipliers_coef_se,
    `t value` = tvals,
    `Pr(>|t|)` = pvals
  )

  ans <- list(
    call = object$call,
    coefficients = coef_table,
    sigma = sqrt(sum(object$residuals^2) / object$df.residual),
    df = c(object$rank, object$df.residual, length(object$coefficients)),
    r.squared = NA_real_,
    adj.r.squared = NA_real_,
    fstatistic = NA,
    cov.unscaled = NA,
    note = attr(object, "note"),
    estimation_type = attr(object, "estimation_type")
  )

  class(ans) <- c("summary_kardl_longrun", "summary.lm")
  ans
}

#' @export
#' @method print summary_kardl_longrun
print.summary_kardl_longrun <- function(x, ...) {
  cat("\nCall:\n", paste(deparse(x$call), collapse = "\n"), "\n", sep = "")

  cat("\nEstimation type:\n")
  cat(x$estimation_type, "\n")

  cat("\nCoefficients:\n")
  printCoefmat(x$coefficients, ...)

  cat("\nNote:\n")
  cat(x$note, "\n")

  invisible(x)
}

#' @export
#' @method print kardl_symmetric
print.kardl_symmetric <- function(x, ...) {
  #print(x$allWald)
  # cat("\n==============================\n")
  cat("\n")
  if (!is.null(x$Lwald) && nrow(x$Lwald) > 0) {
    #    cat("Long-run tests only:\n")
    print(x$Lwald, ...)
    cat("\n")
  }

  if (!is.null(x$Swald) && nrow(x$Swald) > 0) {
    # cat("Short-run tests only:\n")
    print(x$Swald, ...)
    cat("\n")
  }
}

#' @export
#' @method summary kardl_symmetric
summary.kardl_symmetric <- function(object,level=0.05 ,...) {
  decision <- list()
  pValType<-ifelse(object$type == "Chisq","Pr(>Chisq)", "Pr(>F)")
   if(!is.null(object$Lwald)){
     decision$long_run <-list()
       for(v in rownames(object$Lwald)){
         decision$long_run[[v]]<- ifelse( object$Lwald[v,pValType] < level,  paste0("Reject H0 at ",level*100,"% level. Indicating long-run asymmetry for variable ",v,"."),
                                          paste0("Fail to Reject H0 at ",level*100,"% level. Indicating long-run symmetry for variable ",v,"."))
       }
     }

   if(!is.null(object$Swald)){
     decision$short_run <-list()
     for(v in rownames(object$Swald)){
       decision$short_run[[v]]<- ifelse( object$Swald[v,pValType] < level,  paste0("Reject H0 at ",level*100,"% level. Indicating short-run asymmetry for variable ",v,"."),
                                        paste0("Fail to Reject H0 at ",level*100,"% level. Indicating short-run symmetry for variable ",v,"."))
     }
   }
  out <- list(
    Lwald = object$Lwald,
    Swald = object$Swald,
    Lhypotheses = object$Lhypotheses,
    Shypotheses = object$Shypotheses,
    type = object$type,
    decision = decision
  )
  class(out) <- "summary.kardl_symmetric"
  out
}

#' @export
#' @method print summary.kardl_symmetric
print.summary.kardl_symmetric <- function(x, ...){
  if(x$type == "Chisq"){
    pValType <- "Pr(>Chisq)"
    Valtype <- "Chi-squared"
    testLable <- "Chisq"
  } else if(x$type == "F"){
    pValType <- "Pr(>F)"
    Valtype <- "F"
    testLable <- "F value"
  }

  if (!is.null(x$Lwald) && nrow(x$Lwald) > 0) {
    cat("Long-run symmetry tests:\n\n")
    for(v in rownames(x$Lwald)){
      cat("Test for variable: ",v,"\n")
      cat(Valtype, " statistic: ",
          format(x$Lwald[v, testLable], digits = getOption("digits")),
          ", p-value: ",
          format(x$Lwald[v, pValType], digits = getOption("digits")),
          "\n", sep = "")
      cat("Test Decision: ", x$decision$long_run[[v]], "\n")
      cat("Hypotheses:\n")
      cat(paste0("H0: ", x$Lhypotheses$H0[[v]], "\n"))
      cat(paste0("H1: ", x$Lhypotheses$H1[[v]], "\n\n"))
      # write F value and p-value

    }

  }
  if(!is.null(x$Swald) && !is.null(x$Lwald)){  cat("\n_____________________________\n") }
  if (!is.null(x$Swald) && nrow(x$Swald) > 0) {
    cat("Short-run symmetry tests:\n\n")
    for(v in rownames(x$Swald)){
      cat("Test for variable: ",v,"\n")

      cat(Valtype," statistic: ",
          format(x$Swald[v,testLable], digits = getOption("digits")),
          ", p-value: ",
          format(x$Swald[v,pValType], digits = getOption("digits")),
          "\n")
      cat("Test Decision: ", x$decision$short_run[[v]], "\n")
      cat("Hypotheses:\n")
      cat(paste0("H0: ", x$Shypotheses$H0[[v]], "\n"))
      cat(paste0("H1: ", x$Shypotheses$H1[[v]], "\n\n"))
      # write F value and p-value

    }
  }

}


#' @export
#' @method summary kardl_test
summary.kardl_test <- function(object, ...){
    htestsummary(object,...)

}

#' @export
#' @method print summary_htest
print.summary_htest <- function(x, ...){
  cat(x$method, "\n")
  cat(names(x$statistic), " = ", format(unname(x$statistic), digits = getOption("digits")),"\n")
  cat("k = ", x$k, "\n")



  cat("\nHypotheses:\n")
  cat( paste0("H0: Coef(", paste0(x$varnames,collapse = ") = Coef(" ),") = 0"), "\n")
  cat(paste0("H1: Coef(", paste0(x$varnames,collapse = ") \u2260 Coef(" ),")\u2260",  " 0"), "\n")
   # add x$decision
  cat("\nTest Decision: ", x$decision, "\n")
  cat("\nCritical Values (Case ",x$case,"):\n")
  print(x$crit_vals, ...)
  if (!is.null(x$notes)) {
    cat("\nNotes:\n")
    for (note in x$notes) {
      cat("   \u2022 ", note, "\n", sep = "")
    }
    cat("\n")
  }
}

#' @export
#' @method print kardl_long_run
print.kardl_long_run<- function(x, ...) {
  cat("Long-run multiplier estimate\n")
  cat("=================================\n")

  NextMethod()  # continues with normal coefficient printing
  cat("Note:",attr(x, "note"), "\n\n")
  invisible(x)
}

#' @export
#' @method plot kardl_long_run
plot.kardl_long_run <- function(x,  ...) {
  cat("WARNING: Residual diagnostic plots are meaningless for long-run estimators\n")
  if (interactive()) {
    ans <- readline("Show plots anyway? (y/n): ")
    if (tolower(ans) != "y") return(invisible(NULL))
  }
  NextMethod("plot")
}


#' @export
#' @method print kardl_lm
print.kardl_lm<- function(x, ...) {
  cat("Optimal lags for each variable (",x$argsInfo$criterion,"):\n")
  cat(paste(sprintf("%s: %d", names(x$lagInfo$OptLag), x$lagInfo$OptLag), collapse = ", "  ),"\n")
  NextMethod()  # continues with normal coefficient printing
  if (!is.null(x$notes)) {
    cat("\nNotes:\n")
    for (note in x$notes) {
      cat("   \u2022 ", note, "\n", sep = "")
    }
    cat("\n")
  }
}


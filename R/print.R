
#' @export
#' @method print kardl_mplier
print.kardl_mplier <- function(x, ...) {
  cat("kardl Dynamic Multiplier Object\n")
  cat("Horizon:", x$horizon, "\n")
  print(x$mpsi)
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
  print( x$summary)
}

#' @export
#' @method print kardl_boot
print.kardl_boot <- function(x, ...) {
  cat("kardl Bootstrap Results\n")
  cat("Confidence level:", x$level, "%\n")
  cat("Horizon:", x$horizon, "\n")
  print(x$mpsi)
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
  print( x$summary)
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
   if(!is.null(object$Lwald)){
     decision$long_run <-list()
       for(v in rownames(object$Lwald)){
         decision$long_run[[v]]<- ifelse( object$Lwald[v,"Pr(>F)"] < level,  paste0("Reject H0 at ",level*100,"% level. Indicating long-run asymmetry for variable ",v,"."),
                                          paste0("Fail to Reject H0 at ",level*100,"% level. Indicating long-run symmetry for variable ",v,"."))
       }
     }

   if(!is.null(object$Swald)){
     decision$short_run <-list()
     for(v in rownames(object$Swald)){
       decision$short_run[[v]]<- ifelse( object$Swald[v,"Pr(>F)"] < level,  paste0("Reject H0 at ",level*100,"% level. Indicating short-run asymmetry for variable ",v,"."),
                                        paste0("Fail to Reject H0 at ",level*100,"% level. Indicating short-run symmetry for variable ",v,"."))
     }
   }
  out <- list(
    Lwald = object$Lwald,
    Swald = object$Swald,
    Lhypotheses = object$Lhypotheses,
    Shypotheses = object$Shypotheses,
    decision = decision
  )
  class(out) <- "summary.kardl_symmetric"
  out
}

#' @export
#' @method print summary.kardl_symmetric
print.summary.kardl_symmetric <- function(x, ...){

  if (!is.null(x$Lwald) && nrow(x$Lwald) > 0) {
    cat("Long-run symmetry tests:\n\n")
    for(v in rownames(x$Lwald)){
      cat("Test for variable: ",v,"\n")
      cat("F-value: ", x$Lwald[v,"F value"], ", p-value: ", x$Lwald[v,"Pr(>F)"], "\n")
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
      cat("F-value: ", x$Swald[v,"F value"], ", p-value: ", x$Swald[v,"Pr(>F)"], "\n")
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
  cat(names(x$statistic), " = ", unname( x$statistic),"\n")
  cat("k = ", x$k, "\n")



  cat("\nHypotheses:\n")
  cat( paste0("H0: Coef(", paste0(x$varnames,collapse = ") = Coef(" ),") = 0"), "\n")
  cat(paste0("H1: Coef(", paste0(x$varnames,collapse = ") \u2260 Coef(" ),")\u2260",  " 0"), "\n")
   # add x$decision
  cat("\nTest Decision: ", x$decision, "\n")
  cat("\nCritical Values (Case ",x$case,"):\n")
  print(x$crit_vals)
  if (!is.null(x$notes)) {
    cat("\n\033[1;33mNotes:\033[0m\n")
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
  cat("\033[1;33mWARNING: Residual diagnostic plots are meaningless for long-run estimators\033[0m\n")
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
    cat("\n\033[1;33mNotes:\033[0m\n")
    for (note in x$notes) {
      cat("   \u2022 ", note, "\n", sep = "")
    }
    cat("\n")
  }
}


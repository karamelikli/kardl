#' Compute Long-Run Multipliers from a kardl Model
#'
#' This function calculates the long-run parameters of a KARDL model estimated
#' using the \code{kardl} function. The long-run parameters are calculated by
#' dividing the negative of the coefficients of the independent variables by
#' the coefficient of the dependent variable. If an intercept is included in
#' the model, it is also standardized by dividing it by the negative of the
#' long-run parameter of the dependent variable.
#'
#' The function also calculates the standard errors of the long-run multipliers
#' using the delta method, which accounts for the covariance between the
#' coefficients. The fitted values and residuals of the long-run model are
#' calculated based on the original data and the long-run multipliers.
#'
#' The function returns an object of class \code{kardl_long_run}, which contains
#' the long-run multipliers, their standard errors, t-statistics, p-values,
#' fitted values, residuals, and other relevant information for further
#' analysis and diagnostics.
#'
#' Note that the fitted values and residuals from the long-run model are not
#' centered (i.e., they do not have a mean of zero) by design, which means that
#' diagnostic plots and residual-based tests may not be valid for this model.
#' The primary focus of this function is on the estimation of the long-run
#' multipliers and their associated statistics.
#'
#' The long-run multipliers are calculated using the formula:
#' \deqn{LRM_i = -\frac{\eta_i}{\eta_0}}.
#'
#' t-values and p-values are calculated using the standard errors obtained from
#' the delta method, which accounts for the covariance between the coefficients.
#' Delta method formula for standard errors of long-run multipliers:
#' \deqn{SE(LR_i) = \sqrt{(A^2) \cdot Var(\eta_i) + 2 \cdot A \cdot B \cdot
#' Cov(\eta_i, \eta_0) + (B^2) \cdot Var(\eta_0)}}
#' where \deqn{A = \frac{\partial LRM_i}{\partial \eta_i} = -\frac{1}{\eta_0}}
#' and \deqn{B = \frac{\partial LRM_i}{\partial \eta_0} =
#' \frac{\eta_i}{\eta_0^2}}.
#' Hence, \eqn{\eta_i} is the coefficient of the independent variable and
#' \eqn{\eta_0} is the coefficient of the dependent variable in the original
#' KARDL model.
#'
#' @param kardl_model An object of class \code{kardl} estimated using the
#'        \code{kardl} function.
#' @param ... Additional arguments (currently not used).
#'
#' @return An object of class \code{kardl_long_run}, which is a list containing:
#' \itemize{
#'    \item \code{coefficients}: A named vector of long-run multipliers.
#'    \item \code{residuals}: A vector of residuals from the long-run model.
#'    \item \code{effects}: A vector of effects from the long-run model.
#'    \item \code{rank}: The rank of the long-run model.
#'    \item \code{fitted.values}: A vector of fitted values from the long-run
#'          model.
#'    \item \code{assign}: A vector indicating the assignment of coefficients
#'          to terms in the long-run model.
#'    \item \code{qr}: The QR decomposition of the design matrix of the
#'          long-run model.
#'    \item \code{df.residual}: The degrees of freedom of the residuals of the
#'          long-run model.
#'    \item \code{xlevels}: A list of factor levels used in the long-run model.
#'    \item \code{call}: The matched call used to create the long-run model.
#'    \item \code{terms}: The terms object of the long-run model.
#'    \item \code{model}: The data frame used in the long-run model.
#' }
#'
#' @export
#' @import stats
#' @importFrom msm deltamethod
#'
#' @srrstats {G1.3} Statistical terms such as "long-run multipliers",
#'           "standard errors", "t-statistics", and "p-values" are clearly
#'           defined and used consistently throughout the documentation and
#'           examples.
#' @srrstats {G2.0} The function validates that `model` is a `kardl_lm` object
#'           before extracting coefficients and covariance matrices.
#' @srrstats {G3.1} Long-run multiplier standard errors are computed using the
#'           delta method applied to the fitted model variance-covariance
#'           matrix.
#' @srrstats {G3.1a} Documentation describes the delta-method formula for
#'           standard errors and the partial derivatives A and B used in the
#'           calculation.
#' @srrstats {TS4.2} The return value is documented: `coefficients`,
#'           `residuals`, `fitted.values`, `df.residual`, `call`, `terms`,
#'           and `model` are all described.
#' @srrstats {TS4.0b} The return value uses the explicit class
#'           `kardl_longrun`.
#'
#' @seealso \code{\link{kardl}}, \code{\link{pssf}}, \code{\link{psst}}
#'
#' @examples
#'
#' kardl_model <- kardl(
#'   DriversKilled ~ PetrolPrice + drivers + asym(PetrolPrice) +
#'     deterministic(law) + trend,
#'   Seatbelts,
#'   mode = c(1, 2, 3, 0)
#' )
#' long <- kardl_longrun(kardl_model)
#'
#' # Calculate the long-run multipliers
#' long
#'
#' # Details of the long-run multipliers
#' summary(long)
#'
#' # Using magrittr
#' @examplesIf requireNamespace("magrittr", quietly = TRUE)
#' library(magrittr)
#' Seatbelts %>%
#'   kardl(
#'     DriversKilled ~ PetrolPrice + drivers + asym(PetrolPrice) +
#'       deterministic(law) + trend,
#'     mode = c(1, 2, 3, 0), data = .
#'   ) %>%
#'   kardl_longrun() %>%
#'   summary()
kardl_longrun <- function(kardl_model, ...) {
  UseMethod("kardl_longrun")
}

#' @exportS3Method
kardl_longrun.default <- function(kardl_model, ...) {
  stop(
    "kardl_longrun() requires a kardl_lm object. ",
    "Please estimate a model using kardl() first.",
    call. = FALSE
  )
}


#' Compute Long-Run Multipliers from a kardl Model
#'
#' This function calculates the long-run parameters of a KARDL model estimated
#' using the \code{kardl} function. The long-run parameters are calculated by
#' dividing the negative of the coefficients of the independent variables by
#' the coefficient of the dependent variable. If an intercept is included in
#' the model, it is also standardized by dividing it by the negative of the
#' long-run parameter of the dependent variable.
#'
#' @noRd
#' @export
#' @method  kardl_longrun kardl_lm

kardl_longrun.kardl_lm <- function(kardl_model, ...) {
  obj_coef <- kardl_model$coefficients

  my_vars <- replace_lag_var(
    .kardl_settings_env$long_coef,
    kardl_model$extracted_info$long_run_vars,
    1
  )

  my_dep <- my_vars[1]
  my_indep_vars <- my_vars[-1]
  my_assign <- rep(1, length(my_indep_vars))

  multipliers_coef <- -obj_coef[my_indep_vars] / obj_coef[my_dep]

  my_data <- kardl_model$model[, my_vars, drop = FALSE]
  my_data <- as.data.frame(my_data)

  indep_matrix <- as.matrix(
    my_data[, !names(my_data) %in% my_dep, drop = FALSE]
  )
  fitted_values <- indep_matrix %*% as.vector(multipliers_coef)
  residuals_values <- as.numeric(my_data[, my_dep] - fitted_values)

  tol <- 1e-7
  qr_x <- qr(indep_matrix, tol = tol, LAPACK = FALSE)
  qr_x$tol <- tol

  qty <- qr.qty(qr_x, my_data[, my_dep])
  eff <- numeric(length(my_data[, my_dep]))
  eff[qr_x$pivot[1:qr_x$rank]] <- qty[1:qr_x$rank]
  eff[-seq_len(ncol(qr_x$qr))] <- qty[-(1:qr_x$rank)]
  names(eff) <- names(multipliers_coef)

  my_terms <- as.formula(
    paste(my_dep, "~", paste(my_indep_vars, collapse = " + "), "-1")
  )
  proper_terms <- terms(my_terms, data = my_data)

  lm_obj <- list(
    original_model = kardl_model,
    coefficients = multipliers_coef,
    residuals = residuals_values,
    effects = eff,
    rank = length(multipliers_coef),
    fitted.values = as.vector(fitted_values),
    assign = my_assign,
    qr = qr_x,
    df.residual = length(residuals_values) - length(multipliers_coef),
    xlevels = kardl_model$xlevels,
    call = match.call(),
    terms = proper_terms,
    model = my_data,
    depvar = my_dep,
    indepvars = my_indep_vars
  )

  class(lm_obj) <- c("kardl_longrun", "lm")

  attr(lm_obj, "estimation_type") <- "Long-run multipliers"
  warning(
    "Coefficients, standard errors, t-statistics and p-values are reliably ",
    "estimated.\n",
    "Fitted values and residuals are NOT centered (E(u) \u2260 0 by design) ",
    "\u2192 diagnostic plots and residual-based tests are invalid.",
    call. = FALSE
  )

  lm_obj
}


#' @import stats
#' @export
#' @method  summary kardl_longrun
#' @srrstats {G3.1} Long-run multiplier standard errors returned in the summary
#' are computed via the delta method using the fitted model's
#' variance-covariance matrix.
#' @srrstats {TS4.2} The summary documents coefficients, standard errors,
#' t-values, and p-values for each long-run multiplier.
#' @noRd
summary.kardl_longrun <- function(object, vcov = NULL, ...) {
  obj_coef <- object$original_model$coefficients

  if (is.null(vcov)) {
    vcov_matrix <- stats::vcov(object$original_model)
  } else {
    vcov_matrix <- vcov
  }


  my_dep <- object$depvar

  multipliers_coef <- object$coefficients

  first_part <- -1 / obj_coef[my_dep]

  lr_var <- vapply(
    names(multipliers_coef),
    function(i) {
      second_part <- obj_coef[i] / (obj_coef[my_dep]^2)
      (first_part^2) *
        vcov_matrix[i, i] +
        2 * first_part * second_part * vcov_matrix[i, my_dep] +
        (second_part^2) * vcov_matrix[my_dep, my_dep]
    },
    numeric(1)
  )

  multipliers_coef_se <- sqrt(as.vector(lr_var))
  tvals <- multipliers_coef / multipliers_coef_se
  pvals <- 2 * stats::pt(-abs(tvals),
    df = stats::df.residual(object$original_model)
  )

  coef_table <- cbind(
    Estimate = multipliers_coef,
    `Std. Error` = multipliers_coef_se,
    `t value` = tvals,
    `Pr(>|t|)` = pvals
  )

  ans <- list(
    call = object$call,
    coefficients = coef_table,
    sigma = sqrt(
      sum(object$residuals^2) / object$df.residual
    ),
    df = c(
      object$rank,
      object$df.residual,
      length(object$coefficients)
    ),
    r.squared = NA_real_,
    adj.r.squared = NA_real_,
    fstatistic = NA,
    cov.unscaled = NA,
    estimation_type = attr(object, "estimation_type")
  )

  class(ans) <- c("summary_kardl_longrun", "summary.lm")

  ans
}



#' Compute Long-Run Multipliers from a kardl Model
#'
#' This function calculates the long-run parameters of a KARDL model estimated using the \code{kardl} function. The long-run parameters are calculated by dividing the negative of the coefficients of the independent variables by the coefficient of the dependent variable. If an intercept is included in the model, it is also standardized by dividing it by the negative of the long-run parameter of the dependent variable.
#'
#' The function also calculates the standard errors of the long-run multipliers using the delta method, which accounts for the covariance between the coefficients. The fitted values and residuals of the long-run model are calculated based on the original data and the long-run multipliers.
#'
#' The function returns an object of class \code{kardl_long_run}, which contains the long-run multipliers, their standard errors, t-statistics, p-values, fitted values, residuals, and other relevant information for further analysis and diagnostics.
#'
#' Note that the fitted values and residuals from the long-run model are not centered (i.e., they do not have a mean of zero) by design, which means that diagnostic plots and residual-based tests may not be valid for this model. The primary focus of this function is on the estimation of the long-run multipliers and their associated statistics.
#'
#' The long-run multipliers are calculated using the formula:
#' \deqn{LRM_i = -\frac{\eta_i}{\eta_0}}.
#'
#' t-values and p-values are calculated using the standard errors obtained from the delta method, which accounts for the covariance between the coefficients.
#' Delta method formula for standard errors of long-run multipliers:
#' \deqn{SE(LR_i) = \sqrt{(A^2) \cdot Var(\eta_i) + 2 \cdot A \cdot B \cdot Cov(\eta_i, \eta_0) + (B^2) \cdot Var(\eta_0)}} where \deqn{A = \frac{\partial LRM_i}{\partial \eta_i} = -\frac{1}{\eta_0}} and \deqn{B = \frac{\partial LRM_i}{\partial \eta_0} = \frac{\eta_i}{\eta_0^2}} .
#' Hence, \eqn{\eta_i} is the coefficient of the independent variable and \eqn{\eta_0} is the coefficient of the dependent variable in the original KARDL model.
#'
#'
#'
#' @param model An object of class \code{kardl} estimated using the \code{kardl} function.
#'
#'
#' @return An object of class \code{kardl_long_run}, which is a list containing:
#' \itemize{
#'    \item \code{coefficients}: A named vector of long-run multipliers.
#'    \item \code{residuals}: A vector of residuals from the long-run model.
#'    \item \code{effects}: A vector of effects from the long-run model.
#'    \item \code{rank}: The rank of the long-run model.
#'    \item \code{fitted.values}: A vector of fitted values from the long-run model.
#'    \item \code{assign}: A vector indicating the assignment of coefficients to terms in the long-run model.
#'    \item \code{qr}: The QR decomposition of the design matrix of the long-run model.
#'    \item \code{df.residual}: The degrees of freedom of the residuals of the long-run model.
#'    \item \code{xlevels}: A list of factor levels used in the long-run model.
#'    \item \code{call}: The matched call used to create the long-run model.
#'    \item \code{terms}: The terms object of the long-run model.
#'    \item \code{model}: The data frame used in the long-run model.
#' }
#' @export
#' @import stats
#' @importFrom msm deltamethod
#' @seealso \code{\link{kardl}}, \code{\link{pssf}}, \code{\link{psst}}
#' @examples

#' kardl_model<-kardl(imf_example_data,
#'                    CPI~ER+PPI+asym(ER)+deterministic(covid)+trend,
#'                    mode=c(1,2,3,0))
#' long<-kardl_longrun(kardl_model)
#'
#' # Calculate the long-run multipliers
#' long
#' # Details of the long-run multipliers
#' summary(long)
#'
#'
#' # Using magrittr
#'
#' @examplesIf requireNamespace("magrittr", quietly = TRUE)
#' library(magrittr)
#'      imf_example_data %>%
#'      kardl(CPI~ER+PPI+asym(ER)+deterministic(covid)+trend, mode=c(1,2,3,0)) %>%
#'      kardl_longrun() %>% summary()
#'

kardl_longrun <- function(model) {
  objcoef <- model$coefficients

  myvars <- replace_lag_var(
    .kardl_Settings_env$LongCoef,
    model$extractedInfo$longRunVars,
    1
  )

  my_dep <- myvars[1]
  my_indep <- myvars[-1]
  my_assign <- rep(1, length(my_indep))

  multipliers_coef <- -objcoef[my_indep] / objcoef[my_dep]

  mydata <- model$model[, myvars, drop = FALSE]
  mydata <- as.data.frame(mydata)

  indepMatrix <- as.matrix(mydata[, !names(mydata) %in% my_dep, drop = FALSE])
  fitted_values <- indepMatrix %*% as.vector(multipliers_coef)
  residuals_values <- as.numeric(mydata[, my_dep] - fitted_values)

  tol <- 1e-7
  qr_X <- qr(indepMatrix, tol = tol, LAPACK = FALSE)
  qr_X$tol <- tol

  qty <- qr.qty(qr_X, mydata[, my_dep])
  eff <- numeric(length(mydata[, my_dep]))
  eff[qr_X$pivot[1:qr_X$rank]] <- qty[1:qr_X$rank]
  eff[-(1:ncol(qr_X$qr))] <- qty[-(1:qr_X$rank)]
  names(eff) <- names(multipliers_coef)

  my_terms <- as.formula(
    paste(my_dep, "~", paste(my_indep, collapse = " + "), "-1")
  )
  proper_terms <- terms(my_terms, data = mydata)

  lm_obj <- list(
    original_model = model,
    coefficients = multipliers_coef,
    residuals = residuals_values,
    effects = eff,
    rank = length(multipliers_coef),
    fitted.values = as.vector(fitted_values),
    assign = my_assign,
    qr = qr_X,
    df.residual = length(residuals_values) - length(multipliers_coef),
    xlevels = model$xlevels,
    call = match.call(),
    terms = proper_terms,
    model = mydata,
    depvar = my_dep,
    indepvars = my_indep
  )

  class(lm_obj) <- c("kardl_longrun", "lm")

  attr(lm_obj, "estimation_type") <- "Long-run multipliers"
  attr(lm_obj, "note") <- paste0(
    "Coefficients, standard errors, t-statistics and p-values are reliably estimated.\n",
    "Fitted values and residuals are NOT centered (E(u) \u2260 0 by design) \u2192 ",
    "diagnostic plots and residual-based tests are invalid."
  )

  lm_obj
}


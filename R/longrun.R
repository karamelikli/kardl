

#' Calculate long-run multipliers from a KARDL model
#'
#' This function calculates the long-run parameters of a KARDL model estimated using the \code{kardl} function. The long-run parameters are calculated by dividing the negative of the coefficients of the independent variables by the coefficient of the dependent variable. If an intercept is included in the model, it is also standardized by dividing it by the negative of the long-run parameter of the dependent variable.
#'
#' The function also calculates the standard errors of the long-run multipliers using the delta method, which accounts for the covariance between the coefficients. The fitted values and residuals of the long-run model are calculated based on the original data and the long-run multipliers.
#'
#' The function returns an object of class \code{kardl_long_run}, which contains the long-run multipliers, their standard errors, t-statistics, p-values, fitted values, residuals, and other relevant information for further analysis and diagnostics.
#'
#' Note that the fitted values and residuals from the long-run model are not centered (i.e., they do not have a mean of zero) by design, which means that diagnostic plots and residual-based tests may not be valid for this model. The primary focus of this function is on the estimation of the long-run multipliers and their associated statistics.
#' \deqn{LongRunMultiplier_i = -\frac{\beta_i}{\beta_{dep}}}. t-values and p-values are calculated using the standard errors obtained from the delta method, which accounts for the covariance between the coefficients.
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
#' library(magrittr)
#'
#' imf_example_data %>% kardl(CPI~ER+PPI+asym(ER)+deterministic(covid)+trend,
#'                            mode=c(1,2,3,0)) %>% kardl_longrun() %>% summary()
#'

kardl_longrun<-function(model){
  objcoef <- model$coefficients
  myvars<- replace_lag_var(.kardl_Settings_env$LongCoef ,model$extractedInfo$longRunVars,1)
  my_dep<-myvars[1]
  my_indep<-myvars[-1]
  my_assign<-c()
  my_terms<-""
  multipliers_coef <- c(
    -objcoef[my_indep] / objcoef[my_dep]
  )
  if("(Intercept)" %in% names(objcoef)){
    my_assign<-c(0)
    multipliers_coef <- c(
      -objcoef[["(Intercept)"]] / objcoef[my_dep],multipliers_coef
    )
    names(multipliers_coef)[1]<-"(Intercept)"
  }else{
    my_terms<-"-1"
  }
  my_assign<-c(my_assign, rep(1,length(my_indep)))
  vcov_matrix <- stats::vcov(model)
  A<- 1/objcoef[my_dep]
  lr_se <- sapply(names(multipliers_coef), function(i) {
    B<- -objcoef[i]/(objcoef[my_dep]^2)
    (A^2) *vcov_matrix[i,i]+2*A*B*vcov_matrix[i,my_dep]+(B^2 )* vcov_matrix[my_dep,my_dep]
  })
  multipliers_coef_se<-as.vector( sqrt(lr_se))
  tvals <- multipliers_coef / multipliers_coef_se
  pvals <- 2 * stats::pt(-abs(tvals), df = stats::df.residual(model))
  mydata <- model$model[ , myvars, drop = FALSE]
  mydata<-cbind(1,as.data.frame(mydata))
  indepMatrix<- as.matrix(mydata[,!names(mydata) %in% my_dep])
  fitted_values <- indepMatrix %*% as.vector(multipliers_coef)
  residuals_values <- as.numeric( mydata[,my_dep]- fitted_values)

  ### Calculating QR decomposition for the long-run model
  tol <- attr(model$model, "contrasts") %||% 1e-7   # usually 1e-7
  qr_X <- qr( indepMatrix, tol = tol, LAPACK = FALSE)   # <-- THIS IS CRUCIAL
  qr_X$tol<- tol
  # Q<-qr.Q(qr_X, complete = TRUE)
  # eff2<-t(Q) %*% mydata[,my_dep]
  qty <- qr.qty(qr_X, mydata[,my_dep])
  eff <- numeric(length(mydata[,my_dep]) )
  eff[qr_X$pivot[1:qr_X$rank]] <- qty[1:qr_X$rank]
  eff[-(1:ncol(qr_X$qr))] <- qty[-(1:qr_X$rank)]
  names(eff) <- names(model$effects)

  my_terms <- as.formula(paste(my_dep, "~", paste(my_indep, collapse = "+"), my_terms))
  proper_terms <- terms( my_terms, data = mydata)

  lm_obj<-list(
    coefficients =multipliers_coef,
    residuals = residuals_values,
    effects=eff,
    rank=length(multipliers_coef),
    fitted.values = fitted_values,
    assign=my_assign,
    qr=qr_X,
    df.residual = length(residuals_values) - length(multipliers_coef),
    xlevels= model$xlevels,
    call = match.call(),
    terms=proper_terms,
    model=mydata
  )
  class(lm_obj) <- c("kardl_long_run", "lm")

  # Clear, permanent attribute
  attr(lm_obj, "estimation_type") <- "Long-run multipliers"
  attr(lm_obj, "note") <- paste0(
    "Coefficients, standard errors, t-statistics and p-values are reliably estimated.\n",
    "Fitted values and residuals are NOT centered (E(u) \u2260 0 by design) \u2192 ",
    "diagnostic plots and residual-based tests are invalid."
  )
  return(lm_obj)
  #return (multipliers)
}



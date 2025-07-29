

#' Calculating the long-run parameters
#'
#' All long-run estimated parameters are standardized by dividing them by the negative of the long-run parameter of the dependent variable.
#'
#' This function is used to calculate the long-run parameters of a model produced by the \code{kardl} function.
#'
#' It calculates the long-run multipliers and their standard errors, and returns a list containing the kardl_longrun coefficients, standard errors, and a data frame with the results.
#'
#' @param model the model produced by kardl
#'
#'
#' @return a list of class kardl with the following elements:
#' \itemize{
#' \item type: the type of the model, in this case "kardl_longrun"
#' \item coef: the estimated coefficients of the long-run parameters
#' \item delta_se: the standard errors of the long-run parameters
#' \item results: a data frame with the estimated coefficients, standard errors, t-values, p-values, and significance stars
#' \item starsDesc: a description of the significance stars used in the results
#' }
#' @export
#' @import stats
#' @importFrom msm deltamethod
#' @examples
#' kardl_model<-kardl(imf_example_data,
#'                   CPI~ER+PPI+asym(ER)+deterministic(covid)+trend,
#'                   mode=c(1,2,3,0))
#' kardl_longrun(kardl_model)
#'
#' # Using magrittr
#' library(magrittr)
#'
#' imf_example_data %>% kardl(CPI~ER+PPI+asym(ER)+deterministic(covid)+trend,
#'                   mode=c(1,2,3,0)) %>% kardl_longrun()
kardl_longrun<-function(model){

    if(inherits(model,"kardl")){
      vars<- model$inputs
      ardl<-model$finalModel$model
    }else{
      stop("Not suitable input.")
    }

  objcoef <- ardl$coefficients
  multipliers_coef <- c(
    -objcoef[1] / objcoef[2],
    -objcoef[(3):(length(vars$longRunVars) + 1)] / objcoef[2]
  )
  # vcov_matrix <- stats::vcov(ardl)
  # multipliers_coef_se <- delta_method(ardl, vars,vcov_matrix = vcov_matrix)
  multipliers_coef_se<-my_delta(ardl, vars)
  multipliers <- data.frame(estimate = multipliers_coef, std.error = multipliers_coef_se, t.statistic = multipliers_coef/multipliers_coef_se,
                            "p.val"  = 2 * stats::pt(-abs(multipliers_coef/multipliers_coef_se), df = stats::df.residual(ardl)))



  Mystars<-makeStars(multipliers[,4] )
  multipliers<-cbind(multipliers,Mystars$star)
 # multipliers <- data.frame(term = rownames(multipliers), multipliers)
    colnames(multipliers)<-c("Estimate", "Std. Error", "t value", "Pr(>|t|)",""   )
  # rownames(multipliers) <- 1:nrow(multipliers)
  normOutput<-list(
    type="kardl_longrun",
    coef=multipliers_coef,
    delta_se=multipliers_coef_se,
    results=multipliers,
  starsDesc=Mystars$desc
)
  class(normOutput)<-"kardl"
  normOutput
  #return (multipliers)
}
makeStars<-function(value,starsList=list( "0.001"= "***", "0.01"= "**", "0.05" ="*", "0.1" =".")){


  cr<-as.numeric(names( starsList))
  names(starsList)<-cr ## for removing all names containing zeroes at the end of names
  sortedLables<-starsList[as.character( sort(cr))]
  label<-paste0("Signif. codes: ",paste(names(sortedLables), "=", sortedLables, collapse = ", "))
  myStar<-c()
  for (s in 1:length(value)) {
    nVal <- value[s]
    crTrue <- cr[which(nVal <= cr)]
    if (length(crTrue) < 1) {
      myStar <- c(myStar,"")
    } else{
      myStar <- c(myStar,starsList[as.character(min(crTrue))][[1]])
    }
  }

  list(
    star=myStar,
    desc=label
  )

}
my_delta<-function(object,vars ) {
  vcov_matrix <- stats::vcov(object)
  estmean <- stats::coef(object)
  coefsNo<-c(1, 3:(length(vars$longRunVars) + 1))
  coefs<-estmean[coefsNo]

  A<- 1/estmean[2]

  lr_se <- sapply(coefsNo, function(i) {
    B<- -estmean[i]/(estmean[2]^2)
    (A^2) *vcov_matrix[i,i]+2*A*B*vcov_matrix[i,2]+(B^2 )* vcov_matrix[2,2]
  })
  as.vector( sqrt(lr_se))
}

delta_method <- function(object,vars, vcov_matrix = NULL) {
  if (is.null(vcov_matrix)) vcov_matrix <- stats::vcov(object)
  estmean <- stats::coef(object)
  estvar <- vcov_matrix

  restrictions <- paste0("~ -x", c(1, 3:(length(vars$longRunVars) + 1)), " / x2")
  lr_se <- sapply(1:length(restrictions), function(i) {
    msm::deltamethod(stats::formula(restrictions[i]), estmean, estvar)
  })
  return(lr_se)
}

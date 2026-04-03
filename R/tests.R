#' Symmetry Test for non-linear KARDL Models
#'
#' This function performs symmetry tests on non-linear KARDL models to assess whether the effects of positive and negative changes in independent variables are statistically different.
#'
#' This function evaluates whether the inclusion of a particular variable in the model follows a linear relationship or exhibits a non-linear pattern. By analyzing the behavior of the variable, the function helps to identify if the relationship between the variable and the outcome of interest adheres to a straight-line assumption or if it deviates, indicating a non-linear interaction. This distinction is important in model specification, as it ensures that the variable is appropriately represented, which can enhance the model's accuracy and predictive performance.
#'
#' @description
#' The symmetry test is a statistical procedure used to assess the presence of symmetry in the relationship between variables in a model. It is particularly useful in econometric analysis, where it helps to identify whether the effects of changes in one variable on another are symmetric or asymmetric. The test involves estimating a model that includes both positive and negative components of the variables and then performing a Wald test to determine if the coefficients of these components are significantly different from each other.
#' If the test indicates significant differences, it suggests that the relationship is asymmetric, meaning that the impact of increases and decreases in the variables differs.
#' This test returns results for both long-run and short-run variables in a KARDL model. Where applicable, it provides the Wald test statistics, p-values, degrees of freedom, sum of squares, and mean squares for each variable tested.
#' If the null hypothesis of symmetry is rejected, it indicates that the effects of positive and negative changes in the variable are significantly different, suggesting an asymmetric relationship.
#'
#' The non-linear model with one asymmetric variables is specified as follows:
#' \deqn{
#'  \Delta{y_{t}} = \psi + \eta_{0}y_{t - 1} + \eta^{+}_{1} x^{+}_{t - 1}+ \eta^{-}_{1} x^{-}_{t - 1} + \sum_{j = 1}^{p}{\gamma_{j}\Delta y_{t - j}} + \sum_{j = 0}^{q}{\beta^{+}_{j}\Delta x^{+}_{t - j}}  + \sum_{j = 0}^{m}{\beta^{-}_{j}\Delta x^{-}_{t - j}} + e_{t}
#' }
#'
#' This function performs the symmetry test both for long-run and short-run variables in a kardl model. It uses the \code{\link[nlWaldTest]{nlWaldtest}} function from the \pkg{nlWaldTest} package for long-run variables and the \code{\link[car]{linearHypothesis}} function from the \pkg{car} package for short-run variables.
#' The hypotheses for the long-run variables are:
#' \deqn{
#'     H_{0}: -\frac{\eta^{+}_{1}}{\eta_{0}} = -\frac{\eta^{-}_{1}}{\eta_{0}} }
#' \deqn{    H_{1}: -\frac{\eta^{+}_{1}}{\eta_{0}} \neq -\frac{\eta^{-}_{1}}{\eta_{0}}
#'  }
#'
#'  The hypotheses for the short-run variables are:
#'   \deqn{
#'   H_{0}: \sum_{j = 0}^{q}{\beta^{+}_{j}} = \sum_{j = 0}^{m}{\beta^{-}_{j}} }
#'   \deqn{H_{1}: \sum_{j = 0}^{q}{\beta^{+}_{j}} \neq \sum_{j = 0}^{m}{\beta^{-}_{j}}
#'   }
#'
#'
#'
#' @param kmodel The kardl obejct
#' @return A list with class "kardl" containing the following components:
#' \itemize{
#' \item \code{Lwald:} A data frame containing the Wald test results for the long-run variables, including F-statistic, p-value, degrees of freedom, and residual degrees of freedom.
#' \item \code{Lhypotheses:} A list containing the null and alternative hypotheses for the long-run variables.
#' \item \code{Swald:} A data frame containing the Wald test results for the short-run variables, including F-statistic, p-value, degrees of freedom, residual degrees of freedom, and sum of squares.
#' \item \code{Shypotheses:} A list containing the null and alternative hypotheses for the short-run variables.
#' }
#' @export
#' @import nlWaldTest
#' @importFrom car linearHypothesis
#' @references Shin, Y., Yu, B., & Greenwood-Nimmo, M. (2014). Modelling asymmetric cointegration and dynamic multipliers in a nonlinear ARDL framework. Festschrift in honor of Peter Schmidt: Econometric methods and applications, 281-314.
#' @seealso   \code{\link{kardl}}, \code{\link{pssf}}, \code{\link{psst}}, \code{\link{ecm}}, \code{\link{narayan}}
#' @examples
#'
#' kardl_model<-kardl(imf_example_data,
#'                    CPI~Lasym(PPI+ER)+Sas(ER)+deterministic(covid)+trend)
#' ast<- symmetrytest(kardl_model)
#' ast
#' # Detailed results of the test:
#' summary(ast)
#' # The null hypothesis of the test is that the model is symmetric, while the alternative
#' # hypothesis is that the model is asymmetric. The test statistic and p-value are provided
#' # in the output. If the p-value is less than a chosen significance level (e.g., 0.05),
#' # we reject the null hypothesis and conclude that there is evidence of asymmetry in the model.
#'
#' # To get symmetry test results in long-run, you can use the following code:
#' ast$Lwald
#'
#' # To get symmetry test results in short-run, you can use the following code:
#' ast$Swald
#'
#' # To get the null and alternative hypotheses of the test in long-run,
#' # you can use the following code:
#'
#' ast$Lhypotheses
#'
#' # To get the null and alternative hypotheses of the test in short-run,
#' # you can use the following code:
#'
#' ast$Shypotheses
#'
#' # Using magrittr package
#' library(magrittr)
#' imf_example_data %>% kardl(CPI~ER+PPI+asym(ER+PPI)+deterministic(covid)+trend,
#'                            mode=c(1,0,1,1,0)) %>% symmetrytest()
#'
#' # To get the summary of the symmetry test results in one line, you can use the following code:
#' imf_example_data %>% kardl(CPI~ER+PPI+asym(ER+PPI)+deterministic(covid)+trend,
#'                            mode=c(1,0,1,1,0)) %>% symmetrytest() %>% summary()
#'
symmetrytest<-function(kmodel ){

  if(inherits(kmodel, "kardl_lm")){
    if(kmodel$estInfo$type=="ecm"){
      stop("Asymmetry test is not suitable for ECM models.",call. = FALSE)
    }
    vars<- kmodel$extractedInfo
  }else{
    stop("Not suitable input.",call. = FALSE)
  }
  kardlWald<-  list()
  Lwald<-data.frame()
  LwaldName<-c()
  Lhypotheses<-list()
  coefNames<-names(kmodel$coefficients)
  LongDep<- replace_lag_var(.kardl_Settings_env$LongCoef ,kmodel$extractedInfo$dependentVar,1)  # paste0("L1.",kmodel$extractedInfo$dependentVar)
  dependentID<-which(LongDep==coefNames)
  ms_res <- deviance(kmodel) / df.residual(kmodel)
  if(length(kmodel$extractedInfo$ALvars)>0){
    LongAs<-data.frame()
    for (x in kmodel$extractedInfo$ALvars) {
      POS_param<-replace_lag_var(.kardl_Settings_env$LongCoef ,paste0(.kardl_Settings_env$AsymPrefix[1],x,.kardl_Settings_env$AsymSuffix[1]),1)
      POS_index<-which(POS_param==coefNames)
      NEG_param<-replace_lag_var(.kardl_Settings_env$LongCoef ,paste0(.kardl_Settings_env$AsymPrefix[2],x,.kardl_Settings_env$AsymSuffix[2]),1)

      NEG_index<-which(NEG_param==coefNames)
      LongAs<-rbind(LongAs,data.frame(varName=x,POS_param=POS_param,POS_index=POS_index, NEG_param=NEG_param,NEG_index=NEG_index))
    }
    Lhypotheses$H0<-list()
    Lhypotheses$H1<-list()
    for (v in 1:nrow(LongAs)) {
      x<-LongAs[v,]
      Ltest<-paste0("-b[",x$POS_index,"]/b[",dependentID,"]=-b[",x$NEG_index,"]/b[",dependentID,"]")

      Lhypotheses$H0[[x$varName]]<-paste0("- Coef(",x$POS_param,")/Coef(",LongDep,") = - Coef(",x$NEG_param,")/Coef(",LongDep,")")
      Lhypotheses$H1[[x$varName]]<-paste0("- Coef(",x$POS_param,")/Coef(",LongDep,") \u2260 - Coef(",x$NEG_param,")/Coef(",LongDep,")")
      newLW<-nlWaldtest(kmodel,Ltest, df2 = T)
      F_val<-newLW$statistic[["F"]]
      df1<-newLW$parameter[[1]]
      SumSq_approx <- F_val * df1 * ms_res
      MeanSq_approx <- F_val * ms_res
      Lwald <- rbind(Lwald, data.frame(
        Df = df1,
        `Sum of Sq` = SumSq_approx,   # approximation
        `Mean Sq` = MeanSq_approx,
        `F value` = F_val,
        `Pr(>F)` = newLW$p.value,
        row.names = x$varName,#paste0("Long-run:  ", x$varName),
        check.names = FALSE
      ))


    }
    class(Lwald) <- c("anova", "data.frame")
    attr(Lwald, "note") <- "Long-run tests based on normalized coefficients."
    attr(Lwald, "heading") <-  c("Symmetry Test Results - Long-run:", "=======================")
    attr(Lwald, "type") <- "Symmetry Test"
    attr(Lwald, "method") <- "Wald Test"
    kardlWald$Lwald<-Lwald
    kardlWald$Lhypotheses<-Lhypotheses
  }


  Shypotheses<-list()
  Swald<-data.frame()
  SwaldName<-c()
  if(length(kmodel$extractedInfo$ASvars)>0){
    Shypotheses$H0<-list()
    Shypotheses$H1<-list()
    for (v in kmodel$extractedInfo$ASvars) {
      ceofPOS<-ceofNEG<-NegHyp<-PosHyp<-c();
      for (i in 0:kmodel$argsInfo$maxlag) {
        posLagged<-replace_lag_var(.kardl_Settings_env$ShortCoef ,paste0(.kardl_Settings_env$AsymPrefix[1],v,.kardl_Settings_env$AsymSuffix[1]),i)
        negLagged<-replace_lag_var(.kardl_Settings_env$ShortCoef ,paste0(.kardl_Settings_env$AsymPrefix[2],v,.kardl_Settings_env$AsymSuffix[2]),i)
        if(posLagged %in% coefNames){ceofPOS<-c(ceofPOS,posLagged) ;PosHyp<-c(PosHyp,paste0("Coef(",posLagged,")"))}
        if(negLagged %in% coefNames){ceofNEG<-c(ceofNEG,negLagged) ;NegHyp<-c(NegHyp,paste0("Coef(",negLagged,")"))}
      }
      sw<-paste0(paste(ceofPOS,collapse = " + ")," = ",paste(ceofNEG,collapse = " + "))
      Sh_pos_H0 <- paste(PosHyp,collapse = " + ")
      Sh_neg_H0 <- paste(NegHyp,collapse = " + ")
      Shypotheses$H0[[v]]<-paste0(Sh_pos_H0," = ",Sh_neg_H0)
      Shypotheses$H1[[v]]<-paste0(Sh_pos_H0," \u2260 ",Sh_neg_H0)
      sw2<- linearHypothesis(kmodel , sw,test="F")
      Swald <- rbind(Swald, data.frame(
        Df = sw2[2, 3],
        `Sum of Sq` = sw2[2, 4],                     # exact
        `Mean Sq` = sw2[2, 4] / sw2[2, 3],
        `F value` = sw2[2, 5],
        `Pr(>F)` = sw2[2, 6],
        row.names =v, #paste0("Short-run: ", v),
        check.names = FALSE
      ))

    }
    class(Swald) <-c("anova", "data.frame")
    attr(Swald, "heading") <-  c("Symmetry Test Results - Short-run:", "=======================")
    attr(Swald, "type") <- "Symmetry Test"
    attr(Swald, "method") <- "Wald Test"
    kardlWald$Swald<-Swald
    kardlWald$Shypotheses<-Shypotheses
  }

  class(kardlWald)<-c("kardl_symmetric","list")
  kardlWald

}


#' Pesaran et al. (2001) Bounds F-Test for KARDL Models
#'
#' This function performs the Pesaran, Shin, and Smith (PSS) F Bound test to assess the presence of a long-term relationship (cointegration) between variables in the context of an autoregressive distributed lag (ARDL) model. The PSS F Bound test examines the joint significance of lagged levels of the variables in the model. It provides critical values for both the upper and lower bounds, which help determine whether the variables are cointegrated. If the calculated F-statistic falls outside these bounds, it indicates the existence of a long-term equilibrium relationship. This test is particularly useful when the underlying data includes a mix of stationary and non-stationary variables.
#'
#' @param kmodel A fitted KARDL model object of class 'kardl_lm' created using the \code{\link{kardl}} function.
#' @param case Numeric or character. Specifies the case of the test to be used in the function.
#' Acceptable values are 1, 2, 3, 4, 5, and "auto". If "auto" is chosen, the function determines the case automatically based on the model's characteristics. Invalid values will result in an error.
#' \itemize{
#' \item \code{1}: No intercept and no trend
#' \item \code{2}: Restricted intercept and no trend
#' \item \code{3}: Unrestricted intercept and no trend
#' \item \code{4}: Unrestricted intercept and restricted trend
#' \item \code{5}: Unrestricted intercept and unrestricted trend
#' }
#'
#'
#' @param signif_level Character or numeric. Specifies the significance level to be used in the function.
#' Acceptable values are "auto", "0.10", "0.1", "0.05", "0.025", and "0.01".
#' If a numeric value is provided, it will be converted to a character string.
#' If "auto" is chosen, the function determines the significance level automatically.
#' Invalid values will result in an error.
#'
#' @section Hypothesis testing:
#' The null hypothesis (H0) of the F Bound test is that there is no cointegration among the variables in the model. In other words, it tests whether the long-term relationship between the variables is statistically significant. If the calculated F-statistic exceeds the upper critical value, we reject the null hypothesis and conclude that there is cointegration. Conversely, if the F-statistic falls below the lower critical value, we fail to reject the null hypothesis, indicating no evidence of cointegration. If the F-statistic lies between the two critical values, the result is inconclusive.
#'
#'
#'  \deqn{
#'   \Delta {y}_t =  \psi  + \varphi t  + \eta _0   {y}_{t-1}  + \sum_{i=1}^{k} {  \eta _i   {x}_{i,t-1} }  +   \sum_{j=1}^{p} { \gamma_{j}  \Delta {y}_{t-j} }+ \sum_{i=1}^{k} {\sum_{j=0}^{q_i} { \beta_{ij}   \Delta {x}_{i,t-j} } }+ e_t
#' }
#'
#' \describe{
#'   \item{Cases 1, 3, 5:}{
#'      \deqn{\mathbf{H_{0}:} \eta_0  = \eta_1  = \dots = \eta_k  = 0}
#'      \deqn{\mathbf{H_{1}:} \eta_{0} \neq \eta_{1} \neq \dots \neq \eta_{k} \neq 0}
#'   }
#'   \item{Case 2:}{
#'      \deqn{\mathbf{H_{0}:} \eta_0  = \eta_1  = \dots = \eta_k =  \psi  = 0}
#'      \deqn{\mathbf{H_{1}:} \eta_{0} \neq \eta_{1} \neq \dots \neq \eta_{k}  \neq \psi \neq 0}
#'   }
#'
#'   \item{Case 4:}{
#'      \deqn{\mathbf{H_{0}:} \eta_0  = \eta_1  = \dots = \eta_k = \varphi = 0}
#'      \deqn{\mathbf{H_{1}:} \eta_{0} \neq \eta_{1} \neq \dots \neq \eta_{k}  \neq \varphi \neq 0}
#'       }
#'   }
#'
#' @return A list with class "htest" containing the following components:
#' \itemize{
#' \item \code{statistic}: The calculated F-statistic for the test.
#' \item \code{caseTxt}: A character string describing the case used for the test, based on the specified case parameter.
#' \item \code{alternative}: A character string describing the alternative hypothesis of the test.
#' \item \code{sample.size}: The number of observations used in the test.
#' \item \code{varnames}: A character vector containing the names of the dependent variable and independent variables used in the test.
#' \item \code{k}: The number of independent variables (excluding the dependent variable) included in the test.
#' \item \code{sig}: The significance level used for the test, either specified by the user or determined automatically.
#' \item \code{notes}: A character vector containing any notes or warnings related to
#' the test, such as the suitability of the test for small sample sizes or any adjustments made to the case based on the model's characteristics.
#' }
#' @export
#' @importFrom car linearHypothesis
#' @seealso   \code{\link{psst}}  \code{\link{ecm}}  \code{\link{narayan}}
#' @references Pesaran, M. H., Shin, Y. and Smith, R. (2001), "Bounds Testing Approaches to the Analysis of Level Relationship", Journal of Applied Econometrics, 16(3), 289-326.
#'
#' @examples

#' kardl_model<-kardl(imf_example_data,
#'                    CPI~ER+PPI+asym(ER)+deterministic(covid)+trend,
#'                    mode=c(1,2,3,0))
#' my_pssF<-pssf(kardl_model)
#' # Getting the results of the test.
#' my_pssF
#' # Getting details of the test.
#' my_summary<-summary(my_pssF)
#' my_summary
#'
#' # Getting the critical values of the test.
#' my_summary$crit_vals
#'
#'
#'
#'
#' # Using magrittr :
#'
#' library(magrittr)
#' imf_example_data %>% kardl(CPI~ER+PPI+asym(ER)+deterministic(covid)+trend,
#'                            mode=c(1,2,3,0)) %>% pssf()
#'
#' # Getting details of the test results using magrittr:
#' imf_example_data %>% kardl(CPI~ER+PPI+asym(ER)+deterministic(covid)+trend,
#'                            mode=c(1,2,3,0)) %>% pssf() %>% summary()
#'
#'
pssf<-function(kmodel,case=3,signif_level = "auto"){
  if(! case %in% c(1,2,3,4,5,"auto")){
    stop("Invalid 'case' specification. ", call. = FALSE)
  }
  if (is.numeric(signif_level)) {
    signif_level <- as.character(signif_level)
  }
  if (!signif_level %in% c( "auto","0.10","0.1","0.05","0.025", "0.01")) {
    stop("Invalid significance level. ",
         "Valid options are: 'auto', 0.10, 0.05, 0.025, or 0.01.", call. = FALSE )
  }
  if(inherits(kmodel, "kardl_lm")){
    if(kmodel$estInfo$type=="ecm"){
      stop("PSS F test is not applicable for ECM models. Please use 'psst()' function for residual-based tests.", call. = FALSE)
    }
  }else{
    stop("Input object must be of class 'kardl_lm'.",call. = FALSE)
  }

  notesArray<-c()
  if(kmodel$estInfo$TimeSpan<80){
    notesArray <- c(notesArray, paste0(
      "The Pesaran et al. (2001) bounds F-test is designed for large samples. ",
      "Your model uses only ", kmodel$estInfo$TimeSpan, " observations. ",
      "For greater accuracy with small samples, consider using the critical values from Narayan (2005) or use 'narayan()' function. ",
      "Results are provided here using the asymptotic Pesaran et al. (2001) bounds."
    ))
  }

  if(case == "auto"){
    case <- 3 # Default case is 3, which means unrestricted intercept and no trend
  }

  if(kmodel$extractedInfo$noConstant){
    if(case >1){
      notesArray <- c(notesArray,
                      "No constant term is included in the model. Case automatically adjusted to 1 (no intercept, no trend)."
      )
    }
    case<-1
  }else{
    if(case == 1){
      # if case is 1, it means that the model has a constant. If the constant is not included, it should be set to 2.
      if(! kmodel$extractedInfo$trend){
        case<-2
        notesArray <- c(notesArray,
                        "Constant included but no trend. Case automatically adjusted to 2 (restricted intercept, no trend).")
      }else{
        # if case is 1, it means that the model has a constant and no trend. If the trend is included, it should be set to 5.
        case<-5
        notesArray <- c(notesArray,
                        "Both constant and trend are included. Case automatically adjusted to 5 (unrestricted intercept and trend).")
      }
    }else if(case >3){
      # if case is greater than 3, it means that the model has a trend. If the trend is not included, it should be set to 3.
      if(! kmodel$extractedInfo$trend){
        case<-3
        notesArray <- c(notesArray,
                        "No trend is included. Case automatically adjusted to 3 (unrestricted intercept, no trend).")
      }
    }else if(case <4 && kmodel$extractedInfo$trend){
      # if case is less than 4, it means that the model does not have a trend. If the trend is included, it should be set to 4.
      case<-5
      notesArray <- c(notesArray,
                      "Trend detected in the model. Case automatically adjusted to 5 (unrestricted intercept and trend).")

    }
  }

  longrunNames<-replace_lag_var(.kardl_Settings_env$LongCoef ,kmodel$extractedInfo$longRunVars,1) # paste0("L1.",kmodel$extractedInfo$longRunVars)
  # check if case is 1,3,5
  if(case == 2){
    longrunNames<-c(longrunNames,"(Intercept)")
  }
  if(case == 4){
    longrunNames<-c(longrunNames,"trend")
  }


  kisit<-unique(c(paste(unlist(longrunNames),"=0")))
  Fmodel<-linearHypothesis(kmodel ,kisit,test="F")
  PF<-Fmodel$F[2]

  k<-length(kmodel$extractedInfo$longRunVars)-1
  if(k>10){
       k<-10
    notesArray <- c(notesArray,
                    "The number of long-run variables exceeds 10. Critical values are not available for more than 10 variables. Results are provided using the critical values for 10 variables, but interpret with caution."
    )
  }

  caseTxt <- switch(case,
                    "1"="I",
                    "2"="II",
                    "3"="III",
                    "4"="IV",
                    "5"="V"
  )

  structure(list(
    type="cointegration",
    caseTxt=caseTxt,
    case=case,
    statistic = c("F" = PF),
    method = "Pesaran-Shin-Smith (PSS) Bounds F-test for cointegration",
    alternative = "Cointegrating relationship exists",
    data.name = deparse(substitute(model)),
    sample.size=kmodel$estInfo$TimeSpan,
    varnames=longrunNames,
     k=k,
    sig=signif_level,
    notes=notesArray,
    test.func = "pssf"
  ), class = c("kardl_test","htest"))
}

htestsummary.pssf<-function(testObj,...){
  # check if the testObj is of class "kardl_test" and "htest" and if the method is "PesaranF"
  if(!inherits(testObj, "kardl_test") || !inherits(testObj, "htest") || testObj$test.func != "pssf"){

    stop("Input object must be of class 'kardl_test' and 'htest' with test.func 'pssf'.", call. = FALSE)
  }

  notes<-testObj$notes
  case<-testObj$case
  signif_level<-testObj$sig
  PF<-testObj$statistic[["F"]]
  k<-testObj$k

  crVals <- switch(case,
                   # Case I: No intercept and no trend
                   c( 0,3.00,3.00,4.20,4.20,5.47,5.47,7.17,7.17,1,2.44,3.28,3.15,4.11,3.88,4.92,4.81,6.02,2,2.17,3.19,2.72,3.83,3.22,4.50,3.88,5.30,3,2.01,3.10,2.45,3.63,2.87,4.16,3.42,4.84,4,1.90,3.01,2.26,3.48,2.62,3.90,3.07,4.44,5,1.81,2.93,2.14,3.34,2.44,3.71,2.82,4.21,6,1.75,2.87,2.04,3.24,2.32,3.59,2.66,4.05,7,1.70,2.83,1.97,3.18,2.22,3.49,2.54,3.91,8,1.66,2.79,1.91,3.11,2.15,3.40,2.45,3.79,9,1.63,2.75,1.86,3.05,2.08,3.33,2.34,3.68,10,1.60,2.72,1.82,2.99,2.02,3.27,2.26,3.60),
                   # Case II: Restricted intercept and no trend
                   c(0,3.80,3.80,4.60,4.60,5.39,5.39,6.44,6.44,1,3.02,3.51,3.62,4.16,4.18,4.79,4.94,5.58,2,2.63,3.35,3.10,3.87,3.55,4.38,4.13,5.00,3,2.37,3.20,2.79,3.67,3.15,4.08,3.65,4.66,4,2.20,3.09,2.56,3.49,2.88,3.87,3.29,4.37,5,2.08,3.00,2.39,3.38,2.70,3.73,3.06,4.15,6,1.99,2.94,2.27,3.28,2.55,3.61,2.88,3.99,7,1.92,2.89,2.17,3.21,2.43,3.51,2.73,3.90,8,1.85,2.85,2.11,3.15,2.33,3.42,2.62,3.77,9,1.80,2.80,2.04,3.08,2.24,3.35,2.50,3.68,10,1.76,2.77,1.98,3.04,2.18,3.28,2.41,3.61),
                   # Case III: Unrestricted intercept and no trend
                   c(0,6.58,6.58,8.21,8.21,9.80,9.80,11.79,11.79,1,4.04,4.78,4.94,5.73,5.77,6.68,6.84,7.84,2,3.17,4.14,3.79,4.85,4.41,5.52,5.15,6.36,3,2.72,3.77,3.23,4.35,3.69,4.89,4.29,5.61,4,2.45,3.52,2.86,4.01,3.25,4.49,3.74,5.06,5,2.26,3.35,2.62,3.79,2.96,4.18,3.41,4.68,6,2.12,3.23,2.45,3.61,2.75,3.99,3.15,4.43,7,2.03,3.13,2.32,3.50,2.60,3.84,2.96,4.26,8,1.95,3.06,2.22,3.39,2.48,3.70,2.79,4.10,9,1.88,2.99,2.14,3.30,2.37,3.60,2.65,3.97,10,1.83,2.94,2.06,3.24,2.28,3.50,2.54,3.86),
                   # Case IV: Unrestricted intercept and restricted trend
                   c(0,5.37,5.37,6.29,6.29,7.14,7.14,8.26,8.26,1,4.05,4.49,4.68,5.15,5.30,5.83,6.10,6.73,2,3.38,4.02,3.88,4.61,4.37,5.16,4.99,5.85,3,2.97,3.74,3.38,4.23,3.80,4.68,4.30,5.23,4,2.68,3.53,3.05,3.97,3.40,4.36,3.81,4.92,5,2.49,3.38,2.81,3.76,3.11,4.13,3.50,4.63,6,2.33,3.25,2.63,3.62,2.90,3.94,3.27,4.39,7,2.22,3.17,2.50,3.50,2.76,3.81,3.07,4.23,8,2.13,3.09,2.38,3.41,2.62,3.70,2.93,4.06,9,2.05,3.02,2.30,3.33,2.52,3.60,2.79,3.93,10,1.98,2.97,2.21,3.25,2.42,3.52,2.68,3.84),
                   # Case V: Unrestricted intercept and unrestricted trend
                   c(0,9.81,9.81,11.64,11.64,13.36,13.36,15.73,15.73,1,5.59,6.26,6.56,7.30,7.46,8.27,8.74,9.63,2,4.19,5.06,4.87,5.85,5.49,6.59,6.34,7.52,3,3.47,4.45,4.01,5.07,4.52,5.62,5.17,6.36,4,3.03,4.06,3.47,4.57,3.89,5.07,4.40,5.72,5,2.75,3.79,3.12,4.25,3.47,4.67,3.93,5.23,6,2.53,3.59,2.87,4.00,3.19,4.38,3.60,4.90,7,2.38,3.45,2.69,3.83,2.98,4.16,3.34,4.63,8,2.26,3.34,2.55,3.68,2.82,4.02,3.15,4.43,9,2.16,3.24,2.43,3.56,2.67,3.87,2.97,4.24,10,2.07,3.16,2.33,3.46,2.56,3.76,2.84,4.10)
  )



  MatName=c("k","0.10L","0.10U","0.05L","0.05U","0.025L","0.025U","0.01L","0.01U")
  PSSCrVals<-t(matrix(crVals,9,11))




  colnames(PSSCrVals)<-MatName


  bu<-data.frame( L = PSSCrVals[k+1,c(2,4,6,8)],
                  U = PSSCrVals[k+1,c(3,5,7,9)]
  )
  rownames(bu)<-c("0.10","0.05","0.025","0.01")

  if(signif_level == "auto"){
    numericDec<-1

    if(PF>=bu[4,"U"]){
      decision<-"Reject H0 \u2192 Cointegration (at 1% level)";
      sig<-"0.01"
    }else if(PF>=bu[3,"U"]){
      decision<-"Reject H0 \u2192 Cointegration (at 2.5% level)";
      sig<-"0.025"
    }else if(PF>=bu[2,"U"]){
      decision<-"Reject H0 \u2192 Cointegration (at 5% level)";
      sig<-"0.05"
    }else if(PF>=bu[1,"U"]){
      decision<-"Reject H0 \u2192 Weak evidence of cointegration (at 10% level)";
      sig<-"0.10"
    }else{
      sig<-""
      if(PF>=bu[1,"L"]){
        decision<-"Inconclusive";
        numericDec=0
      }else{
        decision<-"Fail to reject H0 \u2192 No Cointegration";
        numericDec=-1
      }
    }
  }else{
    sig<-signif_level
    buRow<-switch (signif_level,"0.1"=1, "0.10"=1,"0.05"=2,"0.025"=3,"0.01"=4)
    if(PF>=bu[buRow,"U"]){
      if(buRow==1){
        decision<-"Reject H0 \u2192 Weak evidence of cointegration (at 10% level)";
      }else {
        decision<-paste0("Reject H0 \u2192 Cointegration (at ",switch (buRow,"1"="10%","2"="5%","3"="2.5%","4"="0.01")," level)");
      }
      numericDec=1
    }else if(PF>=bu[buRow,"L"]){
      decision<-"Inconclusive";numericDec=0
    }else{
      decision<-"Fail to reject H0 \u2192 No Cointegration";
      numericDec=-1
    }
  }


  caseTxt <- switch(case,
                    "1"="I",
                    "2"="II",
                    "3"="III",
                    "4"="IV",
                    "5"="V"
  )


  structure(list(
    type="cointegration",
    case=caseTxt,
    statistic = c("F" = PF),
    method = "Pesaran-Shin-Smith (PSS) Bounds F-test for cointegration",
    crit_vals   = bu,
    k=k,
    varnames=testObj$varnames,
    decision=decision,
    numericDec=numericDec, # -1: fail to reject, 0: inconclusive, 1: reject
    siglvl=sig,
    notes=notes
  ), class = "summary_htest")
}

htestsummary<-function(inputs,...){

  switch(inputs$test.func,
         "pssf"=htestsummary.pssf(inputs,...),
         "narayan"=htestsummary.narayan(inputs,...),
         "psst"=htestsummary.psst(inputs,...),
         stop("No summary method available for this test type.", call. = FALSE)
  )

}

#' Narayan Test
#'
#' This function performs the Narayan test, which is designed to assess cointegration using critical values specifically tailored for small sample sizes. Unlike traditional cointegration tests that may rely on asymptotic distributions, the Narayan test adjusts for the limitations of small samples, providing more accurate results in such contexts. This makes the test particularly useful for studies with fewer observations, as it accounts for sample size constraints when determining the presence of a long-term equilibrium relationship between variables.
#'
#' @param kmodel The kardl obejct
#' @param case Numeric or character. Specifies the case of the test to be used in the function.
#' Acceptable values are 1, 2, 3, 4, 5, and "auto". If "auto" is chosen, the function determines the case automatically based on the model's characteristics. Invalid values will result in an error.
#'
#' \itemize{
#' \item \code{1}: No intercept and no trend. This case is not supported by the Narayan test.
#' \item \code{2}: Restricted intercept and no trend.
#' \item \code{3}: Unrestricted intercept and no trend.
#' \item \code{4}: Unrestricted intercept and restricted trend.
#' \item \code{5}: Unrestricted intercept and unrestricted trend.
#' }
#'
#'
#' @param signif_level Character or numeric. Specifies the significance level to be used in the function.
#'
#' Acceptable values are "auto", "0.10", "0.1", "0.05", "0.025", and "0.01".
#' If a numeric value is provided, it will be converted to a character string.
#' If "auto" is chosen, the function determines the significance level automatically.
#' Invalid values will result in an error.
#'
#'
#' @inheritSection pssf Hypothesis testing
#' @inherit pssf return
#' @importFrom car linearHypothesis
#' @references Narayan, P. K. (2005). The saving and investment nexus for China: evidence from cointegration tests. Applied economics, 37(17), 1979-1990.
#' @export
#' @seealso \code{\link{pssf}}  \code{\link{psst}}  \code{\link{ecm}}
#' @examples

#' kardl_model<-kardl(imf_example_data,
#'                    CPI~ER+PPI+asym(ER)+deterministic(covid)+trend,
#'                    mode=c(1,2,3,0))
#' my_test<-narayan(kardl_model)
#' # Getting the results of the test.
#' my_test
#' # Getting details of the test.
#' my_summary<-summary(my_test)
#' my_summary
#'
#' # Getting the critical values of the test.
#' my_summary$crit_vals
#'
#'
#'
#'
#' # Using magrittr :
#'
#' library(magrittr)
#' imf_example_data %>% kardl(CPI~ER+PPI+asym(ER)+deterministic(covid)+trend,
#'                            mode=c(1,2,3,0)) %>% narayan()
#'
#' # Getting details of the test results using magrittr:
#' imf_example_data %>% kardl(CPI~ER+PPI+asym(ER)+deterministic(covid)+trend,
#'                            mode=c(1,2,3,0)) %>% narayan() %>% summary()
#'
#'
narayan<-function(kmodel,case=3,signif_level = "auto"){
  if(! case %in% c(2,3,4,5,"auto")){
    stop("Invalid 'case' specification. ",
         "Please choose from: 2, 3, 4, 5, or 'auto'.",call. = FALSE)
  }
  if (is.numeric(signif_level)) {
    signif_level <- as.character(signif_level)
  }
  if (!signif_level %in% c( "auto","0.10","0.1","0.05","0.01")) {
    stop("Invalid significance level. ",
         "Valid options are: 'auto', 0.10, 0.05, or 0.01.",call. = FALSE)
  }
  if(inherits(kmodel, "kardl_lm")){
    if(kmodel$estInfo$type=="ecm"){
      stop("Narayan F test is not applicable for ECM models. Please use 'psst()' function for residual-based tests.", call. = FALSE)
    }
  }else{
    stop("Input object must be of class 'kardl_lm'.",call. = FALSE)
  }


  notesArray<-c()
  if(kmodel$estInfo$TimeSpan>=100){
    notesArray <- c(notesArray, paste0(
      "The Narayan F-test is designed for small samples. ",
      "Your model uses only ", kmodel$estInfo$TimeSpan, " observations. ",
      "For greater accuracy with large samples, consider pssf() function. "
    ))
  }

  if(case == "auto"){
    case <- 3 # Default case is 5, which means unrestricted intercept and unrestricted trend
  }

  if(kmodel$extractedInfo$noConstant){
    notesArray <- c(notesArray, "No constant term is included in the model. The Narayan test does not support models without a constant. Please include a constant term.")

  }

  if(case == 1){
    # if case is 1, it means that the model has a constant. If the constant is not included, it should be set to 2.
    if(! kmodel$extractedInfo$trend){
      case<-2
      notesArray <- c(notesArray, "No trend is used in the model. The case was set to 2.")
    }else{
      # if case is 1, it means that the model has a constant and no trend. If the trend is included, it should be set to 5.
      case<-5
      notesArray <- c(notesArray,
                      "Both constant and trend are included. Case automatically adjusted to 5 (unrestricted intercept and trend).")
    }
  } else  if(case >3){
    # if case is greater than 3, it means that the model has a trend. If the trend is not included, it should be set to 3.
    if(! kmodel$extractedInfo$trend){
      case<-3
      notesArray <- c(notesArray,
                      "No trend is included. Case automatically adjusted to 3 (unrestricted intercept, no trend).")
    }
  } else   if(case <4 && kmodel$extractedInfo$trend){
    # if case is less than 4, it means that the model does not have a trend. If the trend is included, it should be set to 4.
    case<-5
    notesArray <- c(notesArray,
                    "Trend detected in the model. Case automatically adjusted to 5 (unrestricted intercept and trend).")

  }





  longrunNames<-replace_lag_var(.kardl_Settings_env$LongCoef ,kmodel$extractedInfo$longRunVars,1) # paste0("L1.",kmodel$extractedInfo$longRunVars)
  # check if case is 1,3,5
  if(case == 2){
    longrunNames<-c(longrunNames,"(Intercept)")
  }
  if(case == 4){
    longrunNames<-c(longrunNames,"trend")
  }


  # Null Hypothesis (H0)
  kisit<-c(paste(unlist(longrunNames),"=0"))

  Fmodel<-linearHypothesis(kmodel ,kisit,test="F")
  PF<-Fmodel$F[2]
 sayi<-length(kmodel$extractedInfo$longRunVars)-1


  structure(list(
    type="cointegration",
    case=case,
    statistic = c("F" = PF),
    method = "Narayan F Test for Cointegration",
    alternative = "Cointegrating relationship exists",
    data.name = deparse(substitute(model)),
    sample.size=kmodel$estInfo$TimeSpan,
    varnames=longrunNames,
   k=sayi,
   n=kmodel$estInfo$end,
   sig=signif_level,
    notes=notesArray,
 test.func  = "narayan"
  ), class = c("kardl_test","htest"))
}

htestsummary.narayan<-function(testObj,...){
  # check if the testObj is of class "kardl_test" and "htest" and if the method is "PesaranF"
  if(!inherits(testObj, "kardl_test") || !inherits(testObj, "htest") || testObj$test.func != "narayan"){

    stop("Input object must be of class 'kardl_test' and 'htest' with test.func 'pssf'.", call. = FALSE)
  }

  notes<-testObj$notes
  case<-testObj$case
  signif_level<-testObj$sig
  PF<-testObj$statistic[["F"]]
  k<-testObj$k
  crVals <- switch(case,
                   # Case I: No intercept and no trend
                   c(),
                   # Case II: Restricted intercept and no trend
                   c(7.595,7.595,6.027,6.76,5.155,6.265,4.614,5.966,4.28,5.84,4.134,5.761,3.976,5.691,3.864,5.694,7.35,7.35,5.763,6.48,4.948,6.028,4.428,5.816,4.093,5.532,3.9,5.419,3.713,5.326,3.599,5.23,7.22,7.22,5.593,6.333,4.77,5.855,4.31,5.544,3.967,5.455,3.657,5.256,3.505,5.121,3.402,5.031,7.265,7.265,5.607,6.193,4.8,5.725,4.27,5.412,3.892,5.173,3.674,5.019,3.54,4.931,3.383,4.832,7.065,7.065,5.503,6.24,4.695,5.758,4.188,5.328,3.845,5.15,3.593,4.981,3.424,4.88,3.282,4.73,6.965,6.965,5.377,6.047,4.61,5.563,4.118,5.2,3.738,4.947,3.543,4.839,3.33,4.708,3.194,4.562,6.96,6.96,5.383,6.033,4.558,5.59,4.068,5.25,3.71,4.965,3.451,4.764,3.293,4.615,3.129,4.507,6.825,6.825,5.35,6.017,4.538,5.475,4.056,5.158,3.725,4.94,3.43,4.721,3.225,4.571,3.092,4.478,6.74,6.74,5.157,5.957,4.398,5.463,3.916,5.088,3.608,4.86,3.373,4.717,3.18,4.596,3.034,4.426,6.915,6.915,5.26,5.957,4.458,5.41,4.048,5.092,3.687,4.842,3.427,4.62,3.219,4.526,3.057,4.413,6.695,6.695,5.157,5.917,4.358,5.393,3.908,5.004,3.602,4.787,3.351,4.587,3.173,4.485,3.021,4.35,5.07,5.07,4.09,4.663,3.538,4.428,3.272,4.306,3.058,4.223,2.91,4.193,2.794,4.148,2.73,4.163,4.945,4.945,3.957,4.53,3.478,4.335,3.164,4.194,2.947,4.088,2.804,4.013,2.685,3.96,2.597,3.907,4.96,4.96,3.937,4.523,3.435,4.26,3.1,4.088,2.893,4,2.734,3.92,2.618,3.863,2.523,3.829,4.895,4.895,3.877,4.46,3.368,4.203,3.078,4.022,2.85,3.905,2.694,3.829,2.591,3.766,2.504,3.723,4.815,4.815,3.86,4.44,3.368,4.178,3.048,4.002,2.823,3.872,2.67,3.781,2.55,3.708,2.457,3.65,4.795,4.795,3.79,4.393,3.303,4.1,2.982,3.942,2.763,3.813,2.617,3.743,2.49,3.658,2.414,3.608,4.78,4.78,3.803,4.363,3.288,4.07,2.962,3.91,2.743,3.792,2.589,3.683,2.456,3.598,2.373,3.54,4.78,4.78,3.787,4.343,3.285,4.07,2.976,3.896,2.75,3.755,2.596,3.677,2.473,3.583,2.373,3.519,4.75,4.75,3.78,4.327,3.243,4.043,2.924,3.86,2.725,3.718,2.564,3.65,2.451,3.559,2.351,3.498,4.76,4.76,3.777,4.32,3.253,4.065,2.946,3.862,2.725,3.718,2.574,3.641,2.449,3.55,2.36,3.478,4.725,4.725,3.74,4.303,3.235,4.053,2.92,3.838,2.688,3.698,2.55,3.606,2.431,3.518,2.336,3.458,4.025,4.025,3.303,3.797,2.915,3.695,2.676,3.586,2.525,3.56,2.407,3.517,2.334,3.515,2.277,3.498,3.98,3.98,3.223,3.757,2.845,3.623,2.618,3.532,2.46,3.46,2.331,3.417,2.254,3.388,2.196,3.37,3.955,3.955,3.21,3.73,2.835,3.585,2.592,3.454,2.427,3.395,2.306,3.353,2.218,3.314,2.152,3.296,3.95,3.95,3.19,3.73,2.788,3.54,2.56,3.428,2.402,3.345,2.276,3.297,2.188,3.254,2.131,3.223,3.935,3.935,3.177,3.653,2.788,3.513,2.538,3.398,2.372,3.32,2.259,3.264,2.17,3.22,2.099,3.181,3.9,3.9,3.143,3.67,2.748,3.495,2.508,3.356,2.345,3.28,2.226,3.241,2.139,3.204,2.069,3.148,3.88,3.88,3.127,3.65,2.738,3.465,2.496,3.346,2.323,3.273,2.204,3.21,2.114,3.153,2.044,3.104,3.88,3.88,3.143,3.623,2.74,3.455,2.492,3.35,2.335,3.252,2.209,3.201,2.12,3.145,2.043,3.094,3.875,3.875,3.12,3.623,2.73,3.445,2.482,3.31,2.32,3.232,2.193,3.161,2.1,3.121,2.024,3.079,3.895,3.895,3.133,3.597,2.725,3.455,2.482,3.334,2.313,3.228,2.196,3.166,2.103,3.111,2.023,3.068,3.87,3.87,3.113,3.61,2.713,3.453,2.474,3.312,2.303,3.22,2.303,3.154,2.088,3.103,2.017,3.052)
                   ,
                   # Case III: Unrestricted intercept and no trend
                   c(13.68,13.68,8.17,9.285,6.183,7.873,5.333,7.063,4.768,6.67,4.537,6.37,4.27,6.211,4.104,6.151,13.29,13.29,7.87,8.96,6.14,7.607,5.198,6.845,4.59,6.368,4.257,6.04,4.016,5.797,3.841,5.686,13.07,13.07,7.625,8.825,5.893,7.337,5.018,6.61,4.428,6.25,4.045,5.898,3.8,5.643,3.644,5.464,12.93,12.93,7.74,8.65,5.92,7.197,4.983,6.423,4.394,5.914,4.03,5.598,3.79,5.411,3.595,5.225,12.73,12.73,7.56,8.685,5.817,7.303,4.865,6.36,4.306,5.874,3.955,5.583,3.656,5.331,3.498,5.149,12.7,12.7,7.435,8.46,5.707,6.977,4.828,6.195,4.244,5.726,3.928,5.408,3.636,5.169,3.424,4.989,12.49,12.49,7.4,8.51,5.697,6.987,4.748,6.188,4.176,5.676,3.783,5.338,3.531,5.081,3.346,4.895,12.4,12.4,7.32,8.435,5.583,6.853,4.69,6.143,4.188,5.694,3.783,5.3,3.501,5.051,3.31,4.871,12.24,12.24,7.17,8.405,5.487,6.88,4.635,6.055,4.098,5.57,3.747,5.285,3.436,5.044,3.261,4.821,12.54,12.54,7.225,8.3,5.513,6.86,4.725,6.08,4.168,5.548,3.772,5.213,3.496,4.966,3.266,4.801,12.12,12.12,7.095,8.26,5.407,6.783,4.568,5.96,4.096,5.512,3.725,5.163,3.457,4.943,3.233,4.76,8.77,8.77,5.395,6.35,4.267,5.473,3.71,5.018,3.354,4.774,3.125,4.608,2.97,4.499,2.875,4.445,8.64,8.64,5.29,6.175,4.183,5.333,3.615,4.913,3.276,4.63,3.037,4.443,2.864,4.324,2.753,4.209,8.57,8.57,5.26,6.16,4.133,5.26,3.548,4.803,3.202,4.544,2.962,4.338,2.797,4.211,2.676,4.13,8.59,8.59,5.235,6.135,4.083,5.207,3.535,4.733,3.178,4.45,2.922,4.268,2.764,4.123,2.643,4.004,8.51,8.51,5.22,6.07,4.07,5.19,3.5,4.7,3.136,4.416,2.9,4.218,2.726,4.057,2.593,3.941,8.39,8.39,5.125,6.045,3.987,5.09,3.408,4.623,3.068,4.334,2.848,4.16,2.676,3.999,2.556,3.904,8.46,8.46,5.125,6,4,5.057,3.415,4.615,3.062,4.314,2.817,4.097,2.643,3.939,2.513,3.823,8.49,8.49,5.13,5.98,4.01,5.08,3.435,4.583,3.068,4.274,2.835,4.09,2.647,3.921,2.525,3.808,8.37,8.37,5.055,5.915,3.947,5.02,3.37,4.545,3.022,4.256,2.788,4.073,2.629,3.906,2.494,3.786,8.42,8.42,5.14,5.92,3.983,5.06,3.408,4.55,3.042,4.244,2.802,4.065,2.637,3.9,2.503,3.768,8.4,8.4,5.06,5.93,3.94,5.043,3.363,4.515,3.01,4.216,2.787,4.015,2.627,3.864,2.476,3.746,6.84,6.84,4.29,5.08,3.437,4.47,3.008,4.15,2.752,3.994,2.578,3.858,2.457,3.797,2.384,3.728,6.81,6.81,4.225,5.05,3.393,4.41,2.958,4.1,2.696,3.898,2.508,3.763,2.387,3.671,2.3,3.606,6.76,6.76,4.235,5,3.373,4.377,2.933,4.02,2.66,3.838,2.483,3.708,2.353,3.599,2.26,3.534,6.76,6.76,4.225,5.02,3.33,4.347,2.893,3.983,2.638,3.772,2.458,3.647,2.327,3.541,2.238,3.461,6.74,6.74,4.19,4.94,3.333,4.313,2.873,3.973,2.614,3.746,2.435,3.6,2.309,3.507,2.205,3.421,6.7,6.7,4.155,4.925,3.28,4.273,2.843,3.92,2.578,3.71,2.393,3.583,2.27,3.486,2.181,3.398,6.7,6.7,4.145,4.95,3.27,4.26,2.838,3.923,2.568,3.712,2.385,3.565,2.253,3.436,2.155,3.353,6.74,6.74,4.175,4.93,3.3,4.25,2.843,3.923,2.574,3.682,2.397,3.543,2.256,3.43,2.156,3.334,6.67,6.67,4.125,4.88,3.25,4.237,2.818,3.88,2.552,3.648,2.363,3.51,2.233,3.407,2.138,3.325,6.72,6.72,4.15,4.885,3.277,4.243,2.838,3.898,2.558,3.654,2.38,3.515,2.244,3.397,2.134,3.313,6.72,6.72,4.135,4.895,3.26,4.247,2.823,3.885,2.548,3.644,2.355,3.5,2.236,3.381,2.129,3.289)
                   ,
                   # Case IV: Unrestricted intercept and restricted trend
                   c(10.2,10.2,7.593,8.35,6.428,7.505,5.666,6.988,5.205,6.64,4.85,6.473,4.689,6.358,4.49,6.328,9.975,9.975,7.477,8.213,6.328,7.408,5.654,6.926,5.147,6.617,4.849,6.511,4.629,5.698,4.489,5.064,9.575,9.575,7.207,7.86,5.98,6.973,5.258,6.526,4.763,6.2,4.427,5.837,4.154,5.699,3.971,5.486,9.555,9.555,7.133,7.82,5.878,6.87,5.15,6.28,4.628,5.865,4.251,5.596,3.998,5.463,3.829,5.313,9.32,9.32,7.017,7.727,5.805,6.79,5.05,6.182,4.557,5.793,4.214,5.52,3.983,5.345,3.762,5.172,9.3,9.3,6.893,7.537,5.678,6.578,4.99,6.018,4.455,5.615,4.111,5.329,3.87,5.171,3.643,5.021,9.245,9.245,6.78,7.377,5.62,6.503,4.928,5.95,4.412,5.545,4.013,5.269,3.775,5.086,3.584,4.922,8.96,8.96,6.707,7.36,5.545,6.453,4.848,5.842,4.347,5.552,4.02,5.263,3.758,5.04,3.557,4.902,8.89,8.89,6.577,7.313,5.448,6.435,4.76,5.798,4.293,5.46,3.966,5.234,3.72,5.004,3.509,4.808,8.905,8.905,6.613,7.253,5.505,6.298,4.808,5.786,4.3,5.377,3.984,5.153,3.728,4.954,3.511,4.789,6.695,6.695,5.157,5.917,4.358,5.393,3.908,5.004,3.602,4.787,3.351,4.587,3.173,4.485,3.021,4.35,7.04,7.04,5.377,5.963,4.535,5.415,4.048,5.09,3.715,4.878,3.504,4.743,3.326,4.653,3.194,4.604,6.9,6.9,5.233,5.777,4.433,5.245,3.936,4.918,3.578,4.668,3.353,4.5,3.174,4.383,3.057,4.319,6.87,6.87,5.18,5.733,4.36,5.138,3.85,4.782,3.512,4.587,3.257,4.431,3.07,4.309,2.933,4.224,6.75,6.75,5.13,5.68,4.335,5.078,3.822,4.714,3.47,4.47,3.211,4.309,3.025,4.198,2.899,4.087,6.685,6.685,5.043,5.607,4.225,5.03,3.73,4.666,3.383,4.432,3.149,4.293,2.975,4.143,2.832,4.012,6.66,6.66,5.013,5.547,4.183,4.955,3.692,4.582,3.358,4.365,3.131,4.206,2.946,4.065,2.791,3.95,6.63,6.63,4.98,5.527,4.18,4.938,3.684,4.584,3.323,4.333,3.086,4.154,2.9,3.999,2.756,3.892,6.55,6.55,4.95,5.467,4.123,4.903,3.626,4.538,3.3,4.28,3.063,4.123,2.88,3.978,2.73,3.879,6.53,6.53,4.93,5.457,4.1,4.9,3.6,4.512,3.272,4.272,3.043,4.1,2.86,3.951,2.711,3.842,6.58,6.58,4.937,5.443,4.12,4.855,3.624,4.488,3.298,4.26,3.054,4.079,2.874,3.914,2.718,3.807,4.725,4.725,3.74,4.303,3.235,4.053,2.92,3.838,2.688,3.698,2.55,3.606,2.431,3.518,2.336,3.458,5.785,5.785,4.427,4.957,3.77,4.535,3.378,4.274,3.097,4.118,2.907,4.01,2.781,3.941,2.681,3.887,5.69,5.69,4.38,4.867,3.698,4.42,3.29,4.176,3.035,3.997,2.831,3.879,2.685,3.785,2.578,3.71,5.68,5.68,4.343,4.823,3.663,4.378,3.264,4.094,2.985,3.918,2.781,3.813,2.634,3.719,2.517,3.65,5.625,5.625,4.3,4.78,3.625,4.33,3.226,4.054,2.95,3.862,2.75,3.739,2.606,3.644,2.484,3.57,5.57,5.57,4.23,4.74,3.573,4.288,3.174,4.004,2.905,3.822,2.703,3.697,2.55,3.609,2.44,3.523,5.57,5.57,4.23,4.73,3.553,4.238,3.132,3.956,2.868,3.782,2.674,3.659,2.538,3.56,2.42,3.481,5.555,5.555,4.203,4.693,3.54,4.235,3.13,3.968,2.852,3.773,2.653,3.637,2.51,3.519,2.392,3.444,5.51,5.51,4.187,4.66,3.535,4.208,3.122,3.942,2.848,3.743,2.647,3.603,2.499,3.49,2.379,3.406,5.53,5.53,4.173,4.647,3.505,4.198,3.098,3.92,2.832,3.738,2.631,3.589,2.485,3.473,2.363,3.394,5.53,5.53,4.193,4.647,3.505,4.213,3.11,3.9,2.832,3.717,2.636,3.579,2.486,3.469,2.372,3.37,3.87,3.87,3.113,3.61,2.713,3.453,2.474,3.312,2.303,3.22,2.18,3.154,2.088,3.103,2.017,3.052)
                   ,
                   # Case V: Unrestricted intercept and unrestricted trend
                   c(18.56,18.56,10.605,11.65,7.977,9.413,6.643,8.313,5.856,7.578,5.347,7.242,5.046,6.93,4.779,6.821,18.02,18.02,10.365,11.295,7.643,9.063,6.38,7.73,5.604,7.172,5.095,6.77,4.704,6.537,4.459,6.206,17.91,17.91,10.15,11.23,7.527,8.803,6.238,7.74,5.376,7.092,4.885,6.55,4.527,6.263,4.31,5.965,17.5,17.5,9.89,10.965,7.317,8.72,6.053,7.458,5.224,6.696,4.715,6.293,4.364,6.006,4.109,5.785,17.53,17.53,9.895,10.965,7.337,8.643,5.995,7.335,5.184,6.684,4.672,6.232,4.31,5.881,4.055,5.64,17.48,17.48,9.8,10.675,7.227,8.34,5.955,7.225,5.108,6.494,4.608,5.977,4.23,5.713,3.955,5.474,17.02,17.02,9.585,10.42,7.057,8.243,5.835,7.108,5.066,6.394,4.505,5.92,4.117,5.597,3.87,5.338,16.85,16.85,9.475,10.515,7.013,8.23,5.795,7.053,4.974,6.378,4.482,5.923,4.111,5.586,3.835,5.339,16.66,16.66,9.37,10.32,6.873,8.163,5.663,6.953,4.922,6.328,4.428,5.898,4.07,5.534,3.774,5.248,16.61,16.61,9.325,10.325,6.93,8.027,5.698,6.97,4.932,6.224,4.393,5.788,4.06,5.459,3.768,5.229,16.6,16.6,9.17,10.24,6.73,8.053,5.62,6.908,4.89,6.164,4.375,5.703,4,5.397,3.728,5.16,12.74,12.74,7.36,8.265,5.55,6.747,4.683,5.98,4.154,5.54,3.818,5.253,3.576,5.065,3.394,4.939,12.58,12.58,7.21,8.055,5.457,6.57,4.568,5.795,4.036,5.304,3.673,5.002,3.426,4.79,3.251,4.64,12.51,12.51,7.135,7.98,5.387,6.437,4.51,5.643,3.958,5.226,3.577,4.923,3.327,4.7,3.121,4.564,12.4,12.4,7.08,7.91,5.36,6.373,4.45,5.56,3.89,5.104,3.532,4.8,3.267,4.584,3.091,4.413,12.17,12.17,6.985,7.86,5.247,6.303,4.368,5.545,3.834,5.064,3.48,4.782,3.229,4.536,3.039,4.339,12.17,12.17,6.93,7.785,5.19,6.223,4.313,5.425,3.794,4.986,3.442,4.69,3.197,4.46,2.989,4.271,12.2,12.2,6.905,7.735,5.19,6.2,4.298,5.445,3.772,4.956,3.407,4.632,3.137,4.393,2.956,4.23,11.96,11.96,6.89,7.66,5.137,6.173,4.268,5.415,3.732,4.92,3.372,4.613,3.137,4.363,2.924,4.206,12,12,6.86,7.645,5.11,6.19,4.235,5.363,3.72,4.904,3.368,4.59,3.107,4.343,2.913,4.168,12.08,12.08,6.88,7.675,5.14,6.153,4.253,5.333,3.724,4.88,3.382,4.567,3.111,4.31,2.915,4.143,12.06,12.06,6.82,7.67,5.067,6.103,4.203,5.32,3.678,4.84,3.335,4.535,3.077,4.284,2.885,4.111,10.34,10.34,6.01,6.78,4.577,5.6,3.868,4.965,3.43,4.624,3.157,4.412,2.977,4.26,2.843,4.16,10.24,10.24,5.95,6.68,4.517,5.48,3.8,4.888,3.374,4.512,3.087,4.277,2.879,4.114,2.729,3.985,10.16,10.16,5.915,6.63,4.477,5.42,3.76,4.795,3.334,4.438,3.032,4.213,2.831,4.04,2.668,3.92,10.15,10.15,5.88,6.64,4.437,5.377,3.74,4.78,3.298,4.378,3.012,4.147,2.796,3.97,2.635,3.838,10.02,10.02,5.78,6.54,4.38,5.35,3.673,4.715,3.24,4.35,2.95,4.11,2.75,3.944,2.59,3.789,10.11,10.11,5.8,6.515,4.37,5.303,3.64,4.67,3.21,4.294,2.927,4.068,2.724,3.893,2.573,3.76,10.03,10.03,5.765,6.5,4.35,5.283,3.645,4.678,3.2,4.31,2.912,4.047,2.709,3.856,2.551,3.716,9.97,9.97,5.755,6.47,4.353,5.257,3.638,4.643,3.196,4.262,2.897,4.022,2.69,3.83,2.531,3.685,10.02,10.02,5.765,6.455,4.33,5.243,3.615,4.635,3.182,4.258,2.893,4.008,2.683,3.807,2.519,3.669,10.03,10.03,5.765,6.47,4.323,5.273,3.618,4.63,3.182,4.248,2.89,3.993,2.681,3.8,2.53,3.648,9.96,9.96,5.725,6.45,4.307,5.223,3.588,4.605,3.16,4.23,2.867,3.975,2.657,3.776,2.504,3.631)
  )

  bas<-c("L0" ,"U0", "L1", "U1", "L2", "U2", "L3", "U3", "L4", "U4", "L5", "U5", "L6", "U6", "L7", "U7")
  sat<-seq(30,80,by=5)




  obsnumber<-round((testObj$n-25)/5)
  if(obsnumber>11){
    obsnumber<-11
    notes<-c(notes,"The number of observations exceeds the maximum limit for the critical values table. Using the critical values for 80 observations.")

  }

  if(k>7){
    k<-7
    notes<-c(notes,"The number of regressors exceeds the maximum limit for the critical values table. Using the critical values for 7 regressors.")
  }


  out.01<-matrix(crVals[1:176],11,16,byrow = T)
  rownames(out.01)<-sat
  colnames(out.01)<-bas
  out.05<-matrix(crVals[177: 352],11,16,byrow = T)
  rownames(out.05)<-sat
  colnames(out.05)<-bas
  out.10<-matrix(crVals[353: 528],11,16,byrow = T)
  rownames(out.10)<-sat
  colnames(out.10)<-bas

  sutun1<-k*2+1
  sutun2<-k*2+2

  bu<-data.frame( L = c(out.10[obsnumber,sutun1],
                        out.05[obsnumber,sutun1],
                        out.01[obsnumber,sutun1]),
                  U = c(out.10[obsnumber,sutun2],
                        out.05[obsnumber,sutun2],
                        out.01[obsnumber,sutun2])
  )

  rownames(bu)<-c("0.10","0.05","0.01")


  if(signif_level == "auto"){
    numericDec<-1

    if(PF>=bu[3,"U"]){
      decision<-"Reject H0 \u2192 Cointegration (at 1% level)";
      sig<-"0.01"
    }else if(PF>=bu[2,"U"]){
      decision<-"Reject H0 \u2192 Cointegration (at 5% level)";
      sig<-"0.05"
    }else if(PF>=bu[1,"U"]){
      decision<-"Reject H0 \u2192 Weak evidence of cointegration (at 10% level)";
      sig<-"0.10"
    }else{
      sig<-""
      if(PF>=bu[1,"L"]){
        decision<-"Inconclusive";
        numericDec=0
      }else{
        decision<-"Fail to reject H0 \u2192 No Cointegration";
        numericDec=-1
      }
    }
  }else{
    sig<-signif_level
    buRow<-switch (signif_level,"0.1"=1, "0.10"=1,"0.05"=2,"0.01"=3)
    if(PF>=bu[buRow,"U"]){
      if(buRow==1){
        decision<-"Reject H0 \u2192 Weak evidence of cointegration (at 10% level)";
      }else {
        decision<-paste0("Reject H0 \u2192 Cointegration (at ",switch (buRow,"1"="10%","2"="5%","3"="0.01")," level)");
      }
      numericDec=1
    }else if(PF>=bu[buRow,"L"]){
      decision<-"Inconclusive";numericDec=0
    }else{
      decision<-"Fail to reject H0 \u2192 No Cointegration";
      numericDec=-1
    }
  }
  caseTxt <- switch(case,
                    "1"="I",
                    "2"="II",
                    "3"="III",
                    "4"="IV",
                    "5"="V"
  )


  structure(list(
    type="cointegration",
    case=caseTxt,
    statistic = c("F" = PF),
    method = testObj$method,
    crit_vals   = bu,
    k=k,
    varnames=testObj$varnames,
    decision=decision,
    numericDec=numericDec, # -1: fail to reject, 0: inconclusive, 1: reject
    siglvl=sig,
    notes=notes
  ), class = "summary_htest")


}


#' PSS t Bound Test
#'
#' This function performs the Pesaran t Bound test
#'
#' This function performs the Pesaran, Shin, and Smith (PSS) t Bound test, which is used to detect the existence of a long-term relationship (cointegration) between variables in an autoregressive distributed lag (ARDL) model. The t Bound test specifically focuses on the significance of the coefficient of the lagged dependent variable, helping to assess whether the variable reverts to its long-term equilibrium after short-term deviations. The test provides critical values for both upper and lower bounds. If the t-statistic falls within the appropriate range, it confirms the presence of cointegration. This test is particularly useful when working with datasets containing both stationary and non-stationary variables.
#' @inheritParams pssf
#'

#' @section Hypothesis testing:
#' The PSS t Bound test evaluates the null hypothesis that the long-run coefficients of the model are equal to zero against the alternative hypothesis that at least one of them is non-zero. The test is conducted under different cases, depending on the model specification.
#'
#'
#'  \deqn{
#'   \Delta {y}_t =  \psi  + \varphi t  + \eta _0   {y}_{t-1}  + \sum_{i=1}^{k} {  \eta _i   {x}_{i,t-1} }  +   \sum_{j=1}^{p} { \gamma_{j}  \Delta {y}_{t-j} }+ \sum_{i=1}^{k} {\sum_{j=0}^{q_i} { \beta_{ij}   \Delta {x}_{i,t-j} } }+ e_t
#' }
#'
#'   \deqn{\mathbf{H_{0}:} \eta_0   = 0}
#'   \deqn{\mathbf{H_{1}:} \eta_{0}  \neq 0}
#'
#'
#' @return The function returns an object of class "htest" containing the following components:
#' \itemize{
#' \item{statistic: The calculated t-statistic for the test.}
#' \item{method: A description of the test performed.}
#' \item{data.name: The name of the data used in the test.}
#' \item{k: The number of independent variables in the model.}
#' \item{notes: Any notes or warnings related to the test results, such as
#' sample size considerations or adjustments made to the case based on model characteristics.}
#' \item{sig: The significance level used for the test, either specified by the user or determined automatically.}
#' \item{alternative: The alternative hypothesis being tested.}
#' \item{case: The case used for the test, either specified by the user or determined automatically based on the model's characteristics.}
#' }
#'

#'
#'
#'
#'
#' @export
#' @seealso \code{\link{pssf}}   \code{\link{ecm}}  \code{\link{narayan}}
#' @importFrom stats vcov
#' @importFrom lmtest coeftest
#' @references Pesaran, M. H., Shin, Y. and Smith, R. (2001), "Bounds Testing Approaches to the Analysis of Level Relationship", Journal of Applied Econometrics, 16(3), 289-326.
#' @examples
#'

#' kardl_model<-kardl(imf_example_data,
#'                    CPI~ER+PPI+asym(ER)+deterministic(covid)+trend,
#'                    mode=c(1,2,3,0))
#' my_test<-psst(kardl_model)
#' # Getting the results of the test.
#' my_test
#' # Getting details of the test.
#' my_summary<-summary(my_test)
#' my_summary
#'
#' # Getting the critical values of the test.
#' my_summary$crit_vals
#'
#'
#'
#'
#' # Using magrittr :
#'
#' library(magrittr)
#' imf_example_data %>% kardl(CPI~ER+PPI+asym(ER)+deterministic(covid)+trend,
#'                            mode=c(1,2,3,0)) %>% psst()
#'
#' # Getting details of the test results using magrittr:
#' imf_example_data %>% kardl(CPI~ER+PPI+asym(ER)+deterministic(covid)+trend,
#'                            mode=c(1,2,3,0)) %>% psst() %>% summary()
#'
#'

psst<-function(kmodel,case=3,signif_level = "auto"){
  notesArray<-c()
  if(! inherits(kmodel, "kardl_lm"))  {
    stop("Input object must be of class 'kardl_lm'.",call. = FALSE)
  }
  if(! case %in% c(1,2,3,4,5,"auto")){
    stop("Invalid 'case' specification. ",
         "Please choose from: 1, 2, 3, 4, 5, or 'auto'.",call. = FALSE)
  }
  if (is.numeric(signif_level)) {
    signif_level <- as.character(signif_level)
  }
  if (!signif_level %in% c( "auto","0.10","0.1","0.05","0.025", "0.01")) {
    stop("Invalid significance level. ",
         "Valid options are: 'auto', 0.10, 0.05, 0.025, or 0.01.",call. = FALSE)
  }

  if(case == "auto"){
    case <- 3 # Default case is 3, which means unrestricted intercept and no trend
  }

  if(kmodel$extractedInfo$noConstant){
    if(case >1){
      notesArray <- c(notesArray,
                      "No constant term is included in the model. Case automatically adjusted to 1 (no intercept, no trend)."
      )
    }
    case<-1
  }else if(kmodel$extractedInfo$trend){
    if(case <4){
      notesArray <- c(notesArray,
                      "Trend detected in the model. Case automatically adjusted to 5 (unrestricted intercept and trend).")
    }
    case<-5

  }else{
    if(!case ==3 ){
      # model has not trend but have constant
      notesArray <- c(notesArray,
                      "No trend is included. Case automatically adjusted to 3 (unrestricted intercept, no trend).")
    }
    case<-3
  }


  vcov_matrix <- stats::vcov(kmodel)
  CoefTest<-lmtest::coeftest(kmodel, vcov = vcov_matrix)
  # null_values <- 0

  if(kmodel$estInfo$type=="ecm"){
    testVarName<-"EcmRes"
    # names(null_values) <- "EcmRes"
  }else{
    testVarName<-replace_lag_var(.kardl_Settings_env$LongCoef ,kmodel$extractedInfo$dependentVar,1)
    # names(null_values) <- replace_lag_var(.kardl_Settings_env$LongCoef ,kmodel$extractedInfo$dependentVar,1)
  }

  Pt <- CoefTest[testVarName ,3]
  k <-length(kmodel$extractedInfo$longRunVars)-1

  structure(list(
    type="cointegration",
    case=case,
    statistic = c("t" = Pt),
    method = "Pesaran-Shin-Smith (PSS) Bounds t-test for cointegration",
    alternative = "Cointegrating relationship exists",
    data.name = deparse(substitute(model)),
    sample.size=kmodel$estInfo$TimeSpan,
    varnames=testVarName,
    k=k,
    sig=signif_level,
    notes=notesArray,
    test.func="psst"
  ), class = c("kardl_test","htest") )

}


htestsummary.psst<-function(testObj,...){
  # check if the testObj is of class "kardl_test" and "htest" and if the method is "PesaranF"
  if(!inherits(testObj, "kardl_test") || !inherits(testObj, "htest") || testObj$test.func != "psst"){

    stop("Input object must be of class 'kardl_test' and 'htest' with test.func 'pssf'.", call. = FALSE)
  }
  notes<-testObj$notes
  case<-testObj$case
  signif_level<-testObj$sig
  Pt<-testObj$statistic[["t"]]
  k<-testObj$k

  if(case == 1){
    crVals <-c(-1.62, -1.62, -1.95, -1.95, -2.24, -2.24, -2.58, -2.58,
               -1.62, -2.28, -1.95, -2.60, -2.24, -2.90, -2.58, -3.22,
               -1.62, -2.68, -1.95, -3.02, -2.24, -3.31, -2.58, -3.66,
               -1.62, -3.00, -1.95, -3.33, -2.24, -3.64, -2.58, -3.97,
               -1.62, -3.26, -1.95, -3.60, -2.24, -3.89, -2.58, -4.23,
               -1.62, -3.49, -1.95, -3.83, -2.24, -4.12, -2.58, -4.44,
               -1.62, -3.70, -1.95, -4.04, -2.24, -4.34, -2.58, -4.67,
               -1.62, -3.90, -1.95, -4.23, -2.24, -4.54, -2.58, -4.88,
               -1.62, -4.09, -1.95, -4.43, -2.24, -4.72, -2.58, -5.07,
               -1.62, -4.26, -1.95, -4.61, -2.24, -4.89, -2.58, -5.25,
               -1.62, -4.42, -1.95, -4.76, -2.24, -5.06, -2.58, -5.44)
  }else if(case %in% c(2,3)){
    crVals <- c(-2.57, -2.57, -2.86, -2.86, -3.13, -3.13, -3.43, -3.43,-2.57, -2.91, -2.86, -3.22, -3.13, -3.50, -3.43, -3.82,-2.57, -3.21, -2.86, -3.53, -3.13, -3.80, -3.43, -4.10,-2.57, -3.46, -2.86, -3.78, -3.13, -4.05, -3.43, -4.37,-2.57, -3.66, -2.86, -3.99, -3.13, -4.26, -3.43, -4.60,-2.57, -3.86, -2.86, -4.19, -3.13, -4.46, -3.43, -4.79,-2.57, -4.04, -2.86, -4.38, -3.13, -4.66, -3.43, -4.99,-2.57, -4.23, -2.86, -4.57, -3.13, -4.85, -3.43, -5.19,-2.57, -4.40, -2.86, -4.72, -3.13, -5.02, -3.43, -5.37,-2.57, -4.56, -2.86, -4.88, -3.13, -5.18, -3.42, -5.54,-2.57, -4.69, -2.86, -5.03, -3.13, -5.34, -3.43, -5.68
    )
    case <- 3 # Default case is 3, which means unrestricted intercept and no trend
  }else if(case %in% c(4,5)){
    crVals <-c(-3.13, -3.13, -3.41, -3.41, -3.65, -3.66, -3.96, -3.97,-3.13, -3.40, -3.41, -3.69, -3.65, -3.96, -3.96, -4.26,-3.13, -3.63, -3.41, -3.95, -3.65, -4.20, -3.96, -4.53,-3.13, -3.84, -3.41, -4.16, -3.65, -4.42, -3.96, -4.73,-3.13, -4.04, -3.41, -4.36, -3.65, -4.62, -3.96, -4.96,-3.13, -4.21, -3.41, -4.52, -3.65, -4.79, -3.96, -5.13,-3.13, -4.37, -3.41, -4.69, -3.65, -4.96, -3.96, -5.31,-3.13, -4.53, -3.41, -4.85, -3.65, -5.14, -3.96, -5.49,-3.13, -4.68, -3.41, -5.01, -3.65, -5.30, -3.96, -5.65,-3.13, -4.82, -3.41, -5.15, -3.65, -5.44, -3.96, -5.79,-3.13, -4.96, -3.41, -5.29, -3.65, -5.59, -3.96, -5.94
    )
    case <- 5 # Default case is 5, which means unrestricted intercept and trend
  }

  #  crVals <- switch(case,

  PSSCrVals<-t(matrix(crVals,8,11))
  PSSCrVals<-cbind(0:10,PSSCrVals)
  MatName=c("k","0.10L","0.10U","0.05L","0.05U","0.025L","0.025U","0.01L","0.01U")
  colnames(PSSCrVals)<-MatName

  if(k>10){
    k<-10
    notes<-c(notes,"The number of regressors exceeds the maximum limit for the critical values table. Using the critical values for 10 regressors.")
  }
  bu<-data.frame( L = PSSCrVals[k+1,c(2,4,6,8)],
                  U = PSSCrVals[k+1,c(3,5,7,9)]
  )
  rownames(bu)<-c("0.10","0.05","0.025","0.01")

  # Pt<- CoefTest_tidy[3]

  if(signif_level == "auto"){
    numericDec<-1

    if(Pt>=bu[4,"U"]){
      decision<-"Reject H0 \u2192 Cointegration (at 1% level)";
      sig<-"0.01"
    }else if(Pt>=bu[3,"U"]){
      decision<-"Reject H0 \u2192 Cointegration (at 2.5% level)";
      sig<-"0.025"
    }else if(Pt>=bu[2,"U"]){
      decision<-"Reject H0 \u2192 Cointegration (at 5% level)";
      sig<-"0.05"
    }else if(Pt>=bu[1,"U"]){
      decision<-"Reject H0 \u2192 Weak evidence of cointegration (at 10% level)";
      sig<-"0.10"
    }else{
      sig<-""
      if(Pt>=bu[1,"L"]){
        decision<-"Inconclusive";
        numericDec=0
      }else{
        decision<-"Fail to reject H0 \u2192 No Cointegration";
        numericDec=-1
      }
    }
  }else{
    sig<-signif_level
    buRow<-switch (signif_level,"0.1"=1, "0.10"=1,"0.05"=2,"0.025"=3,"0.01"=4)
    if(Pt>=bu[buRow,"U"]){
      if(buRow==1){
        decision<-"Reject H0 \u2192 Weak evidence of cointegration (at 10% level)";
      }else {
        decision<-paste0("Reject H0 \u2192 Cointegration (at ",switch (buRow,"1"="10%","2"="5%","3"="2.5%","4"="0.01")," level)");
      }
      numericDec=1
    }else if(Pt>=bu[buRow,"L"]){
      decision<-"Inconclusive";numericDec=0
    }else{
      decision<-"Fail to reject H0 \u2192 No Cointegration";
      numericDec=-1
    }
  }

  caseTxt <- switch(case,
                    "1"="I",
                    "2"="II",
                    "3"="III",
                    "4"="IV",
                    "5"="V"
  )
  structure(list(
    type="cointegration",
    case=caseTxt,
    statistic = c("t" = Pt),
    method = testObj$method,
    crit_vals   = bu,
    k=k,
    varnames=testObj$varnames,
    decision=decision,
    numericDec=numericDec, # -1: fail to reject, 0: inconclusive, 1: reject
    siglvl=sig,
    notes=notes
  ), class = "summary_htest")
}







#' Estimate ARDL and NARDL Models with Automatic Lag Selection
#'
#' This function estimates an Autoregressive Distributed Lag (ARDL) or Nonlinear ARDL (NARDL) model based on the provided data and model formula.
#' It allows for flexible specification of variables, including deterministic terms, asymmetric variables, and trend components.
#' The function also supports automatic lag selection using various information criteria.
#'
#' @details
#' The general formula for the long-run model is specified as follows:
#' \deqn{
#' \begin{aligned}
#'  \Delta {y}_t =  c  +  \eta _0   {y}_{t-1} + \sum_{j=1}^{p} { \gamma_{j}  \Delta {y}_{t-j} }   +\sum_{i=1}^{m} {( \eta ^{+}_i   {x}^{+}_{i,t-1 } + \eta ^{-}_i   {x}^{-}_{i,t-1 } ) }    +  \sum_{i=m+1}^{k} {\eta _i   {x}_{i,t-1 } }
#' +  \newline  \sum_{i=1}^{m} {\sum_{j=0}^{q^+_i} { \beta^+_{ij}   \Delta {x}^+_{i,t-j} } } + \sum_{i=1}^{m} {\sum_{j=0}^{q^-_i} { \beta^-_{ij}   \Delta {x}^-_{i,t-j} } }  + \sum_{i=m+1}^{k} {\sum_{j=0}^{q_i} { \beta_{ij}   \Delta {x}_{i,t-j} } }+ e_t
#' \end{aligned}
#'  }
#'
#' Where:
#' \itemize{
#' \item \eqn{y_t} is the dependent variable at time t.
#' \item \eqn{c} is the constant term.
#' \item \eqn{\eta_0} is the coefficient of the lagged dependent variable.
#' \item \eqn{\gamma_j} are the coefficients of the lagged differences of the dependent variable.
#' \item \eqn{\eta^+_i} and \eqn{\eta^-_i} are the coefficients of the positive and negative decompositions of the independent variables, respectively.
#' \item \eqn{\eta_i} are the coefficients of the independent variables that do not have asymmetric decompositions.
#' \item \eqn{\beta^+_{ij}} and \eqn{\beta^-_{ij}} are the coefficients of the lagged differences of the positive and negative decompositions of the independent variables, respectively.
#' \item \eqn{\beta_{ij}} are the coefficients of the lagged differences of the independent variables that do not have asymmetric decompositions.
#' \item \eqn{e_t} is the error term at time t.
#' \item \eqn{p} is the maximum lag length for the dependent variable.
#' \item \eqn{q^+_i} and \eqn{q^-_i} are the maximum lag lengths for the positive and negative decompositions of the independent variables, respectively.
#' \item \eqn{q_i} is the maximum lag length for the independent variables that do not have asymmetric decompositions.
#' \item \eqn{m} is the number of independent variables with asymmetric decompositions.
#' \item \eqn{k} is the total number of independent variables.
#' \item \eqn{\Delta} denotes the first difference operator.
#' \item \eqn{x^+_{i,t}} and \eqn{x^-_{i,t}} represent the positive and negative decompositions of the independent variable \eqn{x_i} at time t, respectively.
#' }
#'
#' @section Notation of reported coefficients:
#'
#' In the reported coefficients, the prefix \code{L} denotes lagged variables,
#' where the accompanying number indicates the lag order, and \code{.d.} denotes
#' first differences. Accordingly, \code{L1.PPI} represents the first lag of the
#' level of \code{PPI} (long-run component), while \code{L3.d.PPI} denotes the
#' third lag of the first-differenced \code{PPI} (short-run component).
#'
#' In addition, the suffixes \code{_POS} and \code{_NEG} indicate the positive
#' and negative partial sum components of a variable, respectively. This notation
#' is used by default and remains valid unless modified through the
#' \code{kardl_set()} function.
#'
#'
#' @param data The data of analysis
#' @param formula A formula specifying the long-run model equation. This formula defines the relationships
#'        between the dependent variable and explanatory variables, including options for deterministic terms,
#'        asymmetric variables, and a trend component.
#'
#'        Example formula:
#'        \code{y ~ x + z + Asymmetric(z) + Lasymmetric(x2 + x3) + Sasymmetric(x3 + x4) + deterministic(dummy1 + dummy2) + trend}
#'
#'
#'
#' \strong{\emph{Details}}
#'
#' The formula allows flexible specification of variables and their roles:
#'
#' \itemize{
#' \item \strong{Deterministic variables:}
#' Deterministic regressors (e.g., dummy variables) can be included using
#' \code{deterministic()}. Multiple deterministic variables may be supplied
#' using \code{+}, for example \code{deterministic(dummy1 + dummy2)}.
#' These variables are treated as fixed components and are not associated
#' with short-run or long-run dynamics.
#' \item \strong{Asymmetric variables:}
#' Asymmetric decompositions can be specified for short-run and/or
#' long-run dynamics:
#' \itemize{
#' \item \strong{Sasymmetric}: Specifies short-run asymmetric variables.
#' For example, \code{Sasymmetric(x1 + x2)} applies short-run
#' asymmetric decomposition to \code{x1} and \code{x2}.
#' \item \strong{Lasymmetric}: Specifies long-run asymmetric variables.
#' For example, \code{Lasymmetric(x1 + x3)} applies long-run
#' asymmetric decomposition to \code{x1} and \code{x3}.
#' \item \strong{Asymmetric}: Specifies variables that enter both
#' short-run and long-run asymmetric decompositions.
#' For example, \code{Asymmetric(x1 + x3)} applies asymmetric
#' decomposition in both dynamics.
#' }
#' }
#'
#' A \strong{trend} term may be included to capture deterministic linear
#' time trends by simply adding \code{trend} to the formula.
#'
#' The formula also supports the use of \code{.} to represent all available
#' regressors in the supplied data (excluding the dependent variable),
#' following standard R formula conventions.
#'
#' All of the operators \code{Deterministic()}, \code{Sasymmetric()},
#' \code{Lasymmetric()}, and \code{Asymmetric()} follow the same usage
#' rules:
#'
#' \itemize{
#' \item They can be freely combined within a single formula, for example:
#'   \preformatted{
#'   y ~ . +
#'     Asymmetric(z) +
#'     Lasymmetric(x2 + x3) +
#'     Sasymmetric(x3 + x4) +
#'     deterministic(dummy1 + dummy2) +
#'     trend
#'   }
#'
#' \item They must not be nested within one another. Valid usage:
#'   \code{y ~ x + deterministic(dummy) + Asymmetric(z)}.
#'   Invalid usage (to be avoided):
#'   \code{y ~ x + deterministic(Asymmetric(z))} or
#'   \code{y ~ x + Asymmetric(deterministic(dummy))}.
#'
#' \item Where applicable, arguments are validated internally using
#'   \code{match.arg()}. Consequently, abbreviated inputs are accepted
#'   provided they uniquely identify a valid option. For example, if
#'   \code{"asymmetric"} is an admissible value, specifying \code{"a"}
#'   is sufficient. For clarity and reproducibility, however, full
#'   argument names are recommended.
#' }
#'
#' These components may therefore be combined flexibly to construct
#' a specification tailored to the empirical analysis.
#'
#'
#' @param maxlag An integer specifying the maximum number of lags to be considered for the model.
#'        The default value is \code{4}. This parameter sets an upper limit on the lag length during
#'        the model estimation process.
#'
#'
#' \strong{\emph{details}}
#'
#'
#'        The \code{maxlag} parameter is crucial for defining the maximum lag length that the model
#'        will evaluate when selecting the optimal lag structure based on the specified \code{criterion}.
#'        It controls the computational effort and helps prevent overfitting by restricting the search
#'        space for lag selection.
#' \itemize{
#' \item  If the data has a short time horizon or is prone to overfitting, consider reducing \code{maxlag}.
#' \item  If the data is expected to have long-term dependencies, increasing \code{maxlag} may be necessary to capture the relevant dynamics.
#' }
#'
#'        Setting an appropriate value for \code{maxlag} depends on the nature of your dataset and the
#'        context of the analysis:
#' \itemize{
#' \item For small datasets or quick tests, use smaller values (e.g., \code{maxlag = 2}).
#' \item For datasets with more observations or longer-term patterns, larger values (e.g., \code{maxlag = 8})  may be appropriate, though this increases computational time.
#' }
#'
#'
#' \strong{\emph{examples}}
#'
#'
#' Using the default maximum lag (4)
#'
#' \code{kardl(data, MyFormula, maxlag = 4)}
#'
#' Reducing the maximum lag to 2 for faster computation
#'
#' \code{kardl(data, MyFormula, maxlag = 2)}
#'
#' Increasing the maximum lag to 8 for datasets with longer dependencies
#'
#' \code{kardl(data, MyFormula, maxlag = 8)}



#'
#' @param mode Specifies the mode of estimation and output control. This parameter determines how
#'        the function handles lag estimation and what kind of feedback or control is provided during
#'        the process. The available options are:
#'
#' \itemize{
#' \item \strong{"quick"} (default):
#'         Displays progress and messages in the console while the function estimates the optimal lag values.
#'         This mode is suitable for interactive use or for users who want to monitor the estimation process
#'         in real-time. It provides detailed feedback for debugging or observation but may use additional
#'         resources due to verbose output.
#'
#' \item \strong{"grid"} :
#'         Displays progress and messages in the console while the function estimates the optimal lag values.
#'         This mode is suitable for interactive use or for users who want to monitor the estimation process
#'         in real-time. It provides detailed feedback for debugging or observation but may use additional
#'         resources due to verbose output.
#'
#' \item \strong{"grid_custom"}:
#'         Suppresses most or all console output, prioritizing faster execution and reduced resource usage
#'         on PCs or servers. This mode is recommended for high-performance scenarios, batch processing,
#'         or when the estimation process does not require user monitoring. Suitable for large-scale or
#'         repeated runs where output is unnecessary.
#'
#' \item \strong{User-defined vector}:
#'         A numeric vector of lag values specified by the user, allowing full customization of the lag
#'         structure used in model estimation. When a user-defined vector is provided (e.g., `c(1, 2, 4, 5)`),
#'         the function skips the lag optimization process and directly uses the specified lags.
#'
#'          Users can define lag values directly as a numeric vector. For example:
#'           \code{mode = c(1, 2, 4, 5)} assigns lags of 1, 2, 4, and 5 to variables in the specified order.
#'          Alternatively, lag values can be assigned to variables by name for clarity and control. For example:
#'           \code{mode = c(CPI = 2, ER_POS = 3, ER_NEG = 1, PPI = 3)} assigns lags to variables explicitly.
#'          Ensure that the lags are correctly designated by verifying the result using
#'           \code{kardl_model$properLag} after estimation.
#'
#'         \strong{\emph{Attention!}}
#'         A function-based criterion or user-defined function can be specified
#'          for model selection, but this is only supported for \code{mode = "grid_custom"}
#'          and \code{mode = "quick"}. The \code{mode = "grid"} option is restricted to
#'          predefined criteria (e.g., AIC or BIC). For more information on available criteria,
#'          see the \code{\link{modelCriterion}} function documentation.
#'         - When using a numeric vector, ensure the order of lag values matches the variables in your formula.
#'         - If using named vectors, double-check the variable names to avoid mismatches or unintended results.
#'         - This mode bypasses the automatic lag optimization and assumes the user-defined lags are correct.
#' }
#'
#'
#'        The `mode` parameter provides flexibility for different use cases:
#'        - Use `"grid"` mode for debugging or interactive use where progress visibility is important.
#'        - Use `"grid_custom"` mode to minimize overhead in computationally intensive tasks.
#'        - Specify a user-defined vector to customize the lag structure based on prior knowledge or analysis.
#'
#'        Selecting the appropriate mode can improve the efficiency and usability of the function depending
#'        on the user's requirements and the computational environment.
#' @param criterion A string specifying the information criterion to be used for selecting the optimal lag structure.
#'       The available options are:
#' \itemize{
#' \item \strong{"AIC"}: Akaike Information Criterion (default). This criterion balances model fit and complexity,
#'       favoring models that explain the data well with fewer parameters.
#' \item \strong{"BIC"}: Bayesian Information Criterion. This criterion imposes a stronger penalty for model complexity
#'       than AIC, making it more conservative in selecting models with fewer parameters.
#' \item \strong{"AICc"}: Corrected Akaike Information Criterion. This is an adjusted version of AIC that accounts for small sample sizes,
#'       making it more suitable when the number of observations is limited relative to the number of parameters.
#' \item \strong{"HQ"}: Hannan-Quinn Information Criterion. This criterion provides a compromise between AIC and BIC,
#'       favoring models that balance fit and complexity without being overly conservative.
#' }
#'       The criterion can be specified as a string (e.g., \code{"AIC"}) or as a user-defined function that takes a fitted model object.
#'       Please visit the \code{\link{modelCriterion}} function documentation for more details on using custom criteria.
#'
#' @param differentAsymLag A logical value indicating whether to allow different lag lengths for positive and negative decompositions.
#' @param batch A string specifying the batch processing configuration in the format "current_batch/total_batches".
#' If a user utilize grid or grid_custom mode and want to split the lag search into multiple batches, this parameter can be used to define the current batch and the total number of batches.
#'       For example, "2/5" indicates that the current batch is the second out of a total of five batches.
#'       The default value is "1/1", meaning that the entire lag search is performed in a single batch.
#'
#' @param ... Additional arguments that can be passed to the function. These arguments can be used to specify other settings or parameters that are not explicitly defined in the main arguments.
#'
#'@return An object of class \code{kardl_lm} containing the estimated ARDL or NARDL model.
#' The object includes the following components:
#' \describe{
#' \item{argsInfo}{A list of input arguments used for the estimation. It includes the data, formula, maxlag, mode, criterion, differentAsymLag, and batch settings.}
#' \item{extractedInfo}{A list containing extracted information from the input data and formula, such as variable names, deterministic terms, asymmetric variables, and the prepared dataset for estimation.}
#' \item{timeInfo}{A list containing timing information for the estimation process, including start time, end time, and total duration.}
#' \item{lagInfo}{A list containing lag selection information, including the optimal lag configuration and criteria values for different lag combinations.}
#' \item{estInfo}{A list containing estimation details, such as the type of model, estimation method, model formula, number of parameters (k), number of observations (n), start and end points of the fitted values, and total time span.}
#' \item{model}{The fitted linear model object of class \code{lm} representing the estimated ARDL or NARDL model.}
#' }
#'
#'
#' @seealso  \code{\link{ecm}}, \code{\link{kardl_set}}, \code{\link{kardl_get}}, \code{\link{kardl_reset}}, \code{\link{modelCriterion}}
#' @export
#'
#' @import lmtest stats
#' @importFrom utils  methods  capture.output tail flush.console
#'
#' @examples
#'
#'
#' # Sample article: THE DYNAMICS OF EXCHANGE RATE PASS-THROUGH TO DOMESTIC PRICES IN TURKEY
#'
#' kardl_set(formula =CPI~ER+PPI+Asymmetr(ER)+deterministic(covid)+trend ,
#'           data=imf_example_data,
#'           maxlag=2
#' ) # setting the default values of the kardl function
#'
#'
#' # using the grid_custom mode with batch processing
#'
#' kardl_model_grid<-kardl( mode = "grid_custom",batch = "2/3")
#' kardl_model_grid
#'
#' kardl_model2<-kardl(mode = c( 2    ,  1    ,  1   ,   3 ))
#'
#' # Getting the results
#' kardl_model2
#'
#' # Getting the summary of the results
#' summary(kardl_model2)
#'
#' # using '.' in the formula means that all variables in the data will be used
#'
#' kardl(formula=CPI~.+deterministic(covid),criterion = "BIC")
#'
#' # Setting max lag instead of default value [4]
#' kardl(imf_example_data,
#'       CPI~ER+PPI+Lasymmetric(ER),
#'       maxlag = 3, mode = "grid_custom")
#'
#' # Using another criterion for finding the best lag
#' kardl_set(criterion = "HQ") # setting the criterion to HQ
#' kardl( mode = "grid_custom")
#'
#' # using default values of lags
#' kardl( mode=c(1,2,3,0))
#'
#' # For using different lag values for negative and positive decompositions of non-linear variables
#' # setting the same lags for positive and negative decompositions.
#'
#' same<-kardl(formula=CPI~Asymmetric(ER),maxlag=2, mode = "grid_custom",differentAsymLag = FALSE)
#' dif<-kardl(formula=CPI~Sasymmetric(ER),maxlag=2, mode = "grid_custom",differentAsymLag = TRUE)
#'
#' same$lagInfo$OptLag
#' dif$lagInfo$OptLag
#'
#' # Optional: use magrittr if available
#' @examplesIf requireNamespace("magrittr", quietly = TRUE)
#' library(magrittr)
#'   kardl_model_pipe <-  imf_example_data %>%
#'     kardl(mode = "grid_custom")
#'
#'   kardl_model_pipe
#'


kardl <- function(data = NULL, formula = NULL,
                  maxlag  = NULL,
                  mode    = NULL,
                  criterion = NULL,
                  differentAsymLag = NULL,
                  batch = NULL,
                  ...
){
  argsInfo <- list(data=data, formula=formula, maxlag=maxlag, mode=mode,
                   criterion=criterion,differentAsymLag=differentAsymLag,batch=batch)
  otherargsInfo <- list(...)
  kardlVars <- lmerge(argsInfo,otherargsInfo)
  myNames <- names(kardlVars)
  for (i in 1:length(kardlVars)) {
    name <- myNames[i]
    if(nzchar(name)==FALSE) {
      kardlVars[[i]] <- NULL
      next
    }
    if (is.null(kardlVars[[name]])) {
      kardlVars[[name]] <- kardl_get(name)
      if (is.null(kardlVars[[name]])) {
        stop(paste0("No ",name," provided. Please supply `",name,"` or set it with kardl_set(",name," = ...)."),
             call. = FALSE)
      }
      attr(kardlVars[[name]], "source") <- "kardl_set"
    }else{
      attr(kardlVars[[name]], "source") <- "argument"
    }
  }
  spec<-prepare(kardlVars)
  # inputs<<-kardlVars
  makemodel(spec,...)
}


#' Estimate a Restricted ECM Model
#'
#' The `ecm` function estimates a restricted Error Correction Model (ECM) based on the provided data and model specification. This function is designed to test for cointegration using the PSS t Bound test, which assesses the presence of a long-term equilibrium relationship between the dependent variable and the independent variables in the model.
#'
#' @inheritParams kardl
#' @inheritSection kardl Notation of reported coefficients
#' @section Hypothesis testing:
#'
#'
#' The null and alternative hypotheses for the restricted ECM test are as follows:
#'
#'   \deqn{\mathbf{H_{0}:} \theta  =   0}
#'   \deqn{\mathbf{H_{1}:} \theta  \neq 0}
#'
#'   The null hypothesis (\eqn{H_{0}}) states that there is no cointegration in the model, meaning that the long-run relationship between the variables is not significant. The alternative hypothesis (\eqn{H_{1}}) suggests that there is cointegration, indicating a significant long-term relationship between the variables.
#'
#'   The test statistic is calculated as the t-statistic of the coefficient of the error correction term (\eqn{\theta}) in the ECM model. If the absolute value of the t-statistic exceeds the critical value from the PSS t Bound table, we reject the null hypothesis in favor of the alternative hypothesis, indicating that cointegration is present.
#'
#'   The cases for the restricted ECM Bound test are defined as follows:
#'
#'
#'
#' \itemize{
#' \item \code{case 1}: No constant, no trend.
#'
#'   This case is used when the model does not include a constant term or a trend term. It is suitable for models where the variables are stationary and do not exhibit any long-term trends.
#'
#'   The model is specified as follows:
#'
#'   \deqn{
#' \begin{aligned}
#' \Delta y_t =  \sum_{j=1}^{p} \gamma_j \Delta y_{t-j} + \sum_{i=1}^{k} \sum_{j=0}^{q_i} \beta_{ij} \Delta x_{i,t-j} + \theta (y_{t-1}  - \sum_{i=1}^{k} \alpha_i x_{i,t-1} ) + e_t
#' \end{aligned}
#' }
#'
#' \item \code{case 2}: Restricted constant, no trend.
#'
#'   This case is used when the model includes a constant term but no trend term. It is suitable for models where the variables exhibit a long-term relationship but do not have a trend component.
#'   The model is specified as follows:
#'   \deqn{
#'   \begin{aligned}
#'   \Delta y_t &= \sum_{j=1}^{p} \gamma_j \Delta y_{t-j} + \sum_{i=1}^{k} \sum_{j=0}^{q_i} \beta_{ij} \Delta x_{i,t-j} + \theta (y_{t-1} - \alpha_0 - \sum_{i=1}^{k} \alpha_i x_{i,t-1} ) + e_t
#'   \end{aligned}
#'   }
#'
#' \item \code{case 3}: Unrestricted constant, no trend.
#'
#'   This case is used when the model includes an unrestricted constant term but no trend term. It is suitable for models where the variables exhibit a long-term relationship with a constant but do not have a trend component.
#'
#'   The model is specified as follows:
#'
#'   \deqn{
#'   \begin{aligned}
#'   \Delta y_t &= \sum_{j=1}^{p} \gamma_j \Delta y_{t-j} + \sum_{i=1}^{k} \sum_{j=0}^{q_i} \beta_{ij} \Delta x_{i,t-j} + \theta (y_{t-1} - \alpha_0 - \sum_{i=1}^{k} \alpha_i x_{i,t-1} ) + e_t
#'   \end{aligned}
#'     }
#' \item \code{case 4}: Unrestricted Constant, restricted trend.
#'
#'   This case is used when the model includes an unrestricted constant term and a restricted trend term. It is suitable for models where the variables exhibit a long-term relationship with a constant and a trend component.
#'
#'   The model is specified as follows:
#'
#'   \deqn{
#'   \begin{aligned}
#'   \Delta y_t &= \phi + \sum_{j=1}^{p} \gamma_j \Delta y_{t-j} + \sum_{i=1}^{k} \sum_{j=0}^{q_i} \beta_{ij} \Delta x_{i,t-j} + \theta (y_{t-1} - \pi (t-1) - \sum_{i=1}^{k} \alpha_i x_{i,t-1} ) + e_t
#'   \end{aligned}
#'    }
#' \item \code{case 5}: Unrestricted constant, unrestricted trend.
#' }
#'

#'
#'  The Error Correction Model (ECM) is specified as follows:
#' \deqn{
#' \begin{aligned}
#' \Delta y_t &= \phi + \varphi t +  \sum_{j=1}^{p} \gamma_j \Delta y_{t-j} + \sum_{i=1}^{k} \sum_{j=0}^{q_i} \beta_{ij} \Delta x_{i,t-j} + \theta (y_{t-1}  - \sum_{i=1}^{k} \alpha_i x_{i,t-1} ) + e_t
#' \end{aligned}
#' }
#'

#'
#' @return A list containing the results of the restricted ECM test, including:
#' \itemize{
#' \item \code{ecm}: The estimated ECM model objects including:
#' \itemize{
#' \item \code{longrunEQ}: The estimated long-run model equation object.
#' \item \code{shortrunEQ}: The estimated short-run model equation
#' \item \code{ecmL}: The estimated long-run model object.
#' }
#' \item \strong{argsInfo}: A list of input arguments used for the estimation. It includes the data, formula, maxlag, mode, criterion, differentAsymLag, and batch settings.
#' \item \strong{extractedInfo}: A list containing extracted information from the input data and formula, such as variable names, deterministic terms, asymmetric variables, and the prepared dataset for estimation.
#' \item \strong{timeInfo}: A list containing timing information for the estimation process, including start time, end time, and total duration.
#' \item \strong{lagInfo}: A list containing lag selection information, including the optimal lag configuration and criteria values for different lag combinations.
#' \item \strong{estInfo}: A list containing estimation details, such as the type of model, estimation method, model formula, number of parameters (k), number of observations (n), start and end points of the fitted values, and total time span.
#' \item \strong{model}: The fitted linear model object of class \code{lm} representing the estimated ARDL or NARDL model.
#' }
#'
#'
#'
#'
#'
#' @export
#' @seealso \code{\link{kardl}} \code{\link{pssf}}  \code{\link{psst}}   \code{\link{ecm}}  \code{\link{narayan}}
#'
#'
#' @examples
#'
#' # Sample article: THE DYNAMICS OF EXCHANGE RATE PASS-THROUGH TO DOMESTIC PRICES IN TURKEY
#' kardl_set(
#'   formula = CPI ~ ER + PPI + asym(ER) + deterministic(covid) + trend,
#'   data = imf_example_data,
#'   maxlag = 3
#' )
#'
#' # Using the grid mode with batch processing to decrease execution time
#' ecm_model_grid <- ecm(mode = "grid")
#' ecm_model_grid
#'
#' # Checking the cointegration test results using Pesaran t test
#' psst(ecm_model_grid)
#'
#' # Getting the details of psst result
#' summary(psst(ecm_model_grid))
#'
#' # Using the grid_custom mode for faster execution without console output
#' ecm_model <- ecm(imf_example_data, mode = "grid_custom", criterion = "HQ", batch = "2/3")
#' ecm_model
#'
#' # Estimating the model with user-defined lag values
#' ecm_model2 <- ecm(mode = c(2, 1, 1, 3))
#'
#' # Getting the results
#' ecm_model2
#'
#' # Getting the summary of the results
#' summary(ecm_model2)
#'
#' # Alternative specification
#' summary(ecm(imf_example_data, CPI ~ PPI + asym(ER) + trend, case = 4))
#'
#' # For increasing the performance of finding the most fitted lag vector
#' ecm(mode = "grid_custom")
#'
#' # Setting max lag instead of default value [4]
#' ecm(maxlag = 2, mode = "grid_custom")
#'
#' # Using another criterion for finding the best lag
#' ecm(criterion = "HQ", mode = "grid_custom")
#'
#' # For using different lag values for positive and negative decompositions
#' # Setting the same lags for positive and negative decompositions
#' kardl_set(differentAsymLag = FALSE)
#'
#' diffAsymLags <- ecm(mode = "grid_custom")
#' diffAsymLags$lagInfo$OptLag
#'
#' # Setting different lags for positive and negative decompositions
#' sameAsymLags <- ecm(differentAsymLag = TRUE, mode = "grid_custom")
#' sameAsymLags$lagInfo$OptLag
#'
#' # Setting the prefixes and suffixes for nonlinear variables
#' kardl_reset()
#' kardl_set(AsymPrefix = c("asyP_", "asyN_"), AsymSuffix = c("_PP", "_NN"))
#' customizedNames <- ecm(imf_example_data, CPI ~ ER + PPI + asym(ER))
#' customizedNames
#'
#' # Optional plotting example requiring suggested packages
#' if (requireNamespace("dplyr", quietly = TRUE) &&
#'     requireNamespace("tidyr", quietly = TRUE) &&
#'     requireNamespace("ggplot2", quietly = TRUE)) {
#'
#'   LagCriteria <-  ecm_model_grid$lagInfo$LagCriteria
#'   colnames(LagCriteria) <- c("lag", "AIC", "BIC", "AICc", "HQ")
#'
#'   LagCriteria <- dplyr::mutate(
#'     LagCriteria,
#'     dplyr::across(c(AIC, BIC, HQ), as.numeric)
#'   )
#'
#'   LagCriteria_long <- LagCriteria |>
#'     dplyr::select(-AICc) |>
#'     tidyr::pivot_longer(
#'       cols = c(AIC, BIC, HQ),
#'       names_to = "Criteria",
#'       values_to = "Value"
#'     )
#'
#'   min_values <- LagCriteria_long |>
#'     dplyr::group_by(Criteria) |>
#'     dplyr::slice_min(order_by = Value) |>
#'     dplyr::ungroup()
#'
#'   ggplot2::ggplot(
#'     LagCriteria_long,
#'     ggplot2::aes(x = lag, y = Value, color = Criteria, group = Criteria)
#'   ) +
#'     ggplot2::geom_line() +
#'     ggplot2::geom_point(
#'       data = min_values,
#'       ggplot2::aes(x = lag, y = Value),
#'       color = "red",
#'       size = 3,
#'       shape = 8
#'     ) +
#'     ggplot2::geom_text(
#'       data = min_values,
#'       ggplot2::aes(x = lag, y = Value, label = lag),
#'       vjust = 1.5,
#'       color = "black",
#'       size = 3.5
#'     ) +
#'     ggplot2::labs(
#'       title = "Lag Criteria Comparison",
#'       x = "Lag Configuration",
#'       y = "Criteria Value"
#'     ) +
#'     ggplot2::theme_minimal() +
#'     ggplot2::theme(
#'       axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
#'     )
#' }
ecm<-function(data = NULL, formula = NULL,
              maxlag  = NULL,
              mode    = NULL,
              criterion = NULL,
              differentAsymLag = NULL,
              batch = NULL,
              ...  ){

  notesArray<-c()
  get_proper <-kardl(data, formula =formula,  maxlag = maxlag, mode =mode,criterion = criterion,
               differentAsymLag = differentAsymLag,   batch = batch,list(...))

EcmData<-get_proper$model

longrunEQ<- paste0(replace_lag_var(.kardl_Settings_env$LongCoef ,get_proper$extractedInfo$dependentVar,1)  ," ~ " , paste0( replace_lag_var(.kardl_Settings_env$LongCoef ,get_proper$extractedInfo$longRunVars[-1],1) ,collapse = " + "))
longrunEQ<- as.formula(longrunEQ)

  ecmL<-lm(longrunEQ,EcmData)
  ecmL$call <- longrunEQ

EcmData$EcmRes<-residuals(ecmL)

f <- get_proper$call
to_remove <- all.vars(longrunEQ)
tt <- terms(f)
rhs_terms <- attr(tt, "term.labels")
new_rhs <- setdiff(rhs_terms, to_remove)
shortrunEQ <- reformulate(new_rhs, response = as.character(f[[2]]))
if (attr(tt, "intercept") == 0) {
  shortrunEQ <- update(shortrunEQ, . ~ . - 1)
}

shortrunEQ<-update(shortrunEQ, . ~ EcmRes+ . )
ecmS <-lm( shortrunEQ, EcmData)



  ecmList<-list(ecm=list(
    longrunEQ=longrunEQ,
    shortrunEQ=shortrunEQ,
    ecmL=ecmL
  ),
  argsInfo=get_proper$argsInfo,
  extractedInfo=get_proper$extractedInfo,
  timeInfo=get_proper$timeInfo,
  estInfo=get_proper$estInfo,
  lagInfo=get_proper$lagInfo
  )

  ecmList$estInfo$type="ecm"
  ecmOutput<-lmerge(ecmList,ecmS)
  if(coef(ecmS)[["EcmRes"]]>=0){
    notesArray <- c(notesArray, "The coefficient of the error correction term (EcmRes) is non-negative. This may indicate a lack of long-run equilibrium adjustment.")
  }
  if(coef(ecmS)[["EcmRes"]] < -1){
    notesArray <- c(notesArray, "The coefficient of the error correction term (EcmRes) is less than -1. This may suggest over-adjustment or instability in the long-run relationship.")
  }

  ecmOutput$notes<-notesArray
  class(ecmOutput) <- c("kardl_lm","lm")
  return(ecmOutput)

}


#' Model Selection Criteria
#'
#' Computes a model selection criterion (AIC, BIC, AICc, or HQ) or applies a user-defined function
#' to evaluate a statistical model.
#'
#'
#' @param estModel An object containing the fitted model. The object should include at least:
#' \itemize{
#' \item \code{estModel$model} - the actual fitted model object (e.g., from \code{lm}, \code{glm}).
#' \item \code{k} - the number of estimated parameters.
#' \item \code{n} - the sample size.
#' }
#' @param cr A character string specifying the criterion to compute.
#'           Options are \code{"AIC"}, \code{"BIC"}, \code{"AICc"}, and \code{"HQ"}. Alternatively,
#'           a user-defined function can be provided. See details below for more information on using custom criteria.
#' @param ... Additional arguments passed to the user-defined criterion function if \code{cr} is a function.
#'
#' @return A numeric value representing the selected criterion, normalized by the sample size if one of the predefined options is used.
#'
#' @details
#' This function returns model selection criteria used to compare the quality of different models.
#' All criteria are defined such that \strong{lower values indicate better models} (i.e., the goal is minimization).
#'
#' If you wish to compare models using a maximization approach (e.g., log-likelihood),
#' you can multiply the result by \code{-1}.
#'
#' Note: The predefined string options (e.g., \code{"AIC"}) are \strong{not} the same as the built-in R functions \code{AIC()} or \code{BIC()}.
#' In particular, the values returned by this function are adjusted by dividing by the sample size \code{n}
#' (i.e., normalized AIC/BIC), which makes it more comparable across datasets of different sizes.
#'
#' The function returns:
#' \itemize{
#' \item \strong{"AIC"}:  \eqn{ \frac{2k - 2\ell}{n} } Akaike Information Criterion divided by \code{n}.
#' \item \strong{"BIC"}:  \eqn{ \frac{\log(n) \cdot k - 2\ell}{n} } Bayesian Information Criterion divided by \code{n}.
#' \item \strong{"AICc"}: \eqn{ \frac{2k(k+1)}{n - k - 1} + \frac{2k - 2\ell}{n} } Corrected Akaike Information Criterion divided by \code{n}.
#' \item \strong{"HQ"}: \eqn{ \frac{2 \log(\log(n)) \cdot k - 2\ell}{n} } Hannan-Quinn Criterion divided by \code{n}.
#' }
#'
#' where:
#' \itemize{
#' \item \eqn{k} is the number of parameters,
#' \item \eqn{n} is the sample size,
#' \item \eqn{\ell} is the log-likelihood of the model.
#' }
#'
#' If \code{cr} is a function, it is called with the fitted model and any additional arguments passed through \code{...}.
#' @seealso \code{\link{kardl}}
#' @examples
#'
#' # Example usage of modelCriterion function with a simple linear model
#' mylm<- lm(mpg ~ wt + hp, data = mtcars)
#' modelCriterion(mylm, AIC )
#' modelCriterion(mylm, "AIC" )
#'
#'  # Example usage of modelCriterion function with a kardl model
#'  kardl_model <- kardl(imf_example_data,
#'                       CPI ~ ER + PPI + asym(ER) + deterministic(covid) + trend,
#'                       mode = c(1, 2, 3, 0))
#'
#'  # Using AIC as the kardl package's built-in criterion function which is different from the base R AIC function.
#'  modelCriterion(kardl_model, "AIC")
#'
#'  # Using the base R AIC function directly on the fitted model object
#'  modelCriterion(kardl_model, AIC)
#'  # Using the base R AIC function outside of modelCriterion to compute AIC for the fitted model
#'  AIC(kardl_model)
#'
#'  # Using BIC as the criterion for the kardl model which is different from the base R BIC function.
#'  modelCriterion(kardl_model, "BIC")
#'
#'  # Using a custom criterion function that divides AIC by the sample size
#'  my_cr_fun <- function(mod, ...) { AIC(mod) / length(mod$model[[1]]) }
#'  modelCriterion(kardl_model, my_cr_fun)
#'
#' @export
modelCriterion<-function(estModel,cr,...){
  if (is.function(cr)) {
    do.call(cr,list(estModel, ...))
  } else {
    myCr<- match.arg( cr ,c("AIC","BIC","AICc","HQ"))
    k<-length(estModel$coefficients) #k
    n<-length(estModel$residuals)
    llh<-logLik(estModel)
    val<-switch (myCr,"AIC"=((2*k-2*llh)/n),"BIC"=((log(n)*k-2*llh)/n ),"AICc"=(((2 * k * (k + 1))/(n - k - 1))+(2*k-2*llh)/n),"HQ"=((2*log(log(n))*k-2*llh)/n) )
    as.numeric( val)
  }
}



makemodel<-function(spec,...){

  myMethod<-T
  # if(is.vector(inputs$mode))
  if(is.vector(spec$argsInfo$mode[1]) && is.numeric(spec$argsInfo$mode) && isFALSE(isFALSE(spec$argsInfo$mode)) )  {
    class(myMethod)<- "user"
  }else{
    class(myMethod)<- match.arg(tolower(spec$argsInfo$mode) ,c("quick","grid_custom","grid"))
  }
  UseMethod("makemodel",myMethod)
}



# Estimation the model by quick lags
#
#If the lags of short-run variables are determined by user, the results will be obtained with \code{userDefinedModel}
# @param inputs All inputs of making model.
# @param ... Other inputs
#
# @return

#' @export
makemodel.quick<-function(spec , ...  ){#model,data,inputs){
  start_time <- Sys.time()
  # inputs<-prepare(lmerge(list( model=model,data=data),inputs))

  preModel<-makeLongrunMOdel(spec) #retruns LS_longrun   LS_dependent


  #fmodel<-paste0(LS_dependent,"~",paste(LS_longrun,makeShortrunMOdel(spec$extractedInfo$shortRunVars,LagsList,deterministic),sep="+"))

  Xlength<-length( spec$extractedInfo$shortRunVars)

  failed_checks<-matrix(NA,1,Xlength)[-1,]
  colnames(failed_checks)<- spec$extractedInfo$shortRunVars

  toporders<-matrix(0,1,Xlength+1)[-1,]
  colnames(toporders)<- c(spec$extractedInfo$shortRunVars,"criterion_value")

  #internal func
  myEst<-function(order){

    # function(shortRunVars,LagsList,deterministic,LS_dependent,LS_longrun,thisData)
      theResults<-lm(as.formula(paste0(preModel$LS_dependent,"~",paste(preModel$LS_longrun,
                      makeShortrunMOdel(spec$extractedInfo$shortRunVars,order,spec$extractedInfo$deterministic),sep="+")) ) ,spec$extractedInfo$data)

    # theResults<-makeEstimation(spec$extractedInfo$shortRunVars,
    #                            order,spec$extractedInfo$deterministic,preModel$LS_dependent,preModel$LS_longrun,spec$extractedInfo$data)
    modelCriterion(theResults,spec$argsInfo$criterion)
  }

  for (i in 1:spec$argsInfo$maxlag) { # for each parse
    ardl_converge <- FALSE
    order1 <- rep(i, Xlength)
    Base_cr <- myEst(order1)
    toporders<-rbind(toporders,c(order1 ,Base_cr))
    while (ardl_converge == FALSE) {
      for(j in 1:(Xlength-1)) {
        order2_back <- order1
        order2_forth <- order1
        if (order1[j + 1] == 0) {
          # order2_back is the order1
          order2_forth[j + 1] <- order1[j + 1] + 1
        } else {
          if (order1[j + 1] == spec$argsInfo$maxlag) {
            order2_back[j + 1] <- order1[j + 1] - 1
          }else{
            order2_back[j + 1] <- order1[j + 1] - 1
            order2_forth[j + 1] <- order1[j + 1] + 1
          }
        }
        model0 <- myEst(order2_back)
        model1 <- myEst(order2_forth)
        minCr<-min(model0,model1)
        if(Base_cr>minCr){
          if( model0 >  model1){
            order1<-order2_forth
          }else{
            order1<- order2_back
          }
          toporders<-rbind(toporders,c(order1 ,minCr))
          Base_cr<-minCr
          ardl_converge =F
        }else{
          if(any(apply(failed_checks, 1, function(row) all(row == order1)))){
            ardl_converge =T
          }else {
            failed_checks<-rbind(failed_checks,order1)
          }
        }
      }
    }
  }

   minValue<-toporders[  which.min(toporders[, "criterion_value"]), , drop = FALSE]

  best_order <- minValue[,-ncol(minValue)]
  MyFormula<-as.formula(paste0(preModel$LS_dependent,"~",paste(preModel$LS_longrun,
                      makeShortrunMOdel(spec$extractedInfo$shortRunVars,best_order,spec$extractedInfo$deterministic),sep="+")) )
  theResults<-lm(MyFormula ,spec$extractedInfo$data)
  theResults$call <- MyFormula

  # model_tidy <- summary(theResults$formula)$coefficients %>%   as_tibble(rownames = "term")
  # colnames(model_tidy) <- c("term", "estimate", "std.error", "statistic", "p.value")

  end_time <- Sys.time()
  spec$timeInfo<-list(
    start_time=start_time,
    end_time=end_time,
    span = difftime(end_time, start_time, units = "secs")
  )
  spec$lagInfo<-list(
    OptLag=best_order,
    LagCriteria=toporders
  )
  fittedVars<-fitted(theResults)
  k<-length(theResults$coefficients)
  n<-length(theResults$residuals)
  spec$estInfo<- list(type="kardlmodel",
                      method = "quick",
                      ModelFormula = theResults$call,
                      k=k ,
                      n=n,
                      start=as.numeric(names(fittedVars[1])),
                      end=as.numeric(names(tail(fittedVars,n=1))),
                      TimeSpan=n+spec$argsInfo$maxlag+1
  )


  Karamelikli<-lmerge(spec,theResults)
  class(Karamelikli) <- c("kardl_lm","lm")
  Karamelikli
}

# Estimation the model by User-defined lags
#
#If the lags of short-run variables are determined by user, the results will be obtained with \code{userDefinedModel}
# @param inputs All inputs of making model.
# @param ... Other inputs
#
# @return
#' @export
#'
makemodel.user <-function(spec, ...  ){#model,data,inputs){
  # spec$argsInfo$mode<-"user"
  start_time <- Sys.time()

  # inputs<-prepare(lmerge(list( model=model,data=data),inputs))

  preModel<-makeLongrunMOdel(spec) #retruns LS_longrun   LS_dependent

  MyFormula<- as.formula(paste0(preModel$LS_dependent,"~",paste(preModel$LS_longrun,
                      makeShortrunMOdel(spec$extractedInfo$shortRunVars,spec$argsInfo$mode,spec$extractedInfo$deterministic),sep="+")) )
  theResults<- lm(MyFormula ,spec$extractedInfo$data)
  theResults$call <- MyFormula

    #makeEstimation(spec$extractedInfo$shortRunVars,LagsList=spec$argsInfo$mode,spec$extractedInfo$deterministic,preModel$LS_dependent,preModel$LS_longrun,spec$extractedInfo$data)


  # model_tidy <- summary(theResults$formula)$coefficients %>%   as_tibble(rownames = "term")
  # colnames(model_tidy) <- c("term", "estimate", "std.error", "statistic", "p.value")
  OptLag<-spec$argsInfo$mode
  attr( OptLag,"source")<- NULL
  attr( OptLag,"description")<- NULL

  end_time <- Sys.time()

  spec$timeInfo<-list(
    start_time=start_time,
    end_time=end_time,
    span = difftime(end_time, start_time, units = "secs")
  )
  spec$lagInfo<-list(
    OptLag=OptLag
  )
  fittedVars<-fitted(theResults)
  k<-length(theResults$coefficients) #k
  n<-length(theResults$residuals)
  spec$estInfo<- list(
    type="kardlmodel",
    method = "user",
    ModelFormula = theResults$call,
    k=k ,
    n=n,
    start=as.numeric(names(fittedVars[1])),
    end=as.numeric(names(tail(fittedVars,n=1))),
    TimeSpan=n+max(OptLag)+1
  )


  Karamelikli<-lmerge(spec,theResults)
  class(Karamelikli) <- c("kardl_lm","lm")
  Karamelikli
}

# Find Optimum Lags level  by maximizing Performance
#
# To reduce server load for finding optimum lag, this function can find it.
# Notice! nothing will be print during estimations.
# @param inputs All inputs of making model.
# @param ... Other inputs
#
# @return
#
#' @export
makemodel.grid_custom<-function(spec, ...  ){ #model ,  data,inputs  ){
  # inputs$mode<-"grid_custom"
  start_time <- Sys.time()
  # inputs<-prepare(lmerge(list( model=model,data=data),inputs))

  # inputs<-prepare(model=model,data=data  ,inputs)
  # cat("\r",paste0("Note: Performance mode will only have output once all estimations are finished.  Starting of estimation of the model: ",inputs$formulaName))

  preModel<-makeLongrunMOdel(spec) #retruns LS_longrun   LS_dependent
  batch<-BatchControl(spec)
  spec$extractedInfo  <- lmerge(batch,spec$extractedInfo)

  OrderForShortRun <- rep(list((spec$argsInfo$maxlag-1):0),spec$extractedInfo$shortrunLength) # I am reversing orders due to alerting about the insufficiency of the degree of freedom.
  OrderForInd<-list((spec$argsInfo$maxlag-1):1)
  GeneralOrder<-append(OrderForShortRun,OrderForInd) # p<-append(m,b)
  LagQueue<-rev(expand.grid(GeneralOrder))  # q<-expand.grid(p)

  if(length(spec$extractedInfo$ASvars)>0 && ! spec$argsInfo$differentAsymLag) {
    i<-1 # satrt from the second var
    for (x in  spec$extractedInfo$independentVars){
      i<-i+1
      if((x %in% spec$extractedInfo$ASvars)){
        LagQueue<-cbind(LagQueue[,1:i],LagQueue[,i:ncol(LagQueue)])
        i<-i+1
      }
    }
  }
  colnames(LagQueue)<-spec$extractedInfo$shortRunVars
  ## Note: This part will be updated to reduce RAM requirement to keep huge list. It will be cut by the batch function's return.
  Mincr<-1000000 # max value for be compared with criteria.
  OptRow<-0 # The row with contain the most proper lag orders

  for(i in batch$startRow:batch$endRow)  {

    MyFormula<- as.formula(paste0(preModel$LS_dependent,"~",paste(preModel$LS_longrun,
                      makeShortrunMOdel(spec$extractedInfo$shortRunVars,unlist(LagQueue[i,]),spec$extractedInfo$deterministic),sep="+")) )
    theResults<- lm(MyFormula ,spec$extractedInfo$data)
    cr<-modelCriterion(theResults, spec$argsInfo$criterion, ...)
   if(cr<Mincr){
      Mincr<-cr
      OptRow<-i
    }
  }

  endLag  <-paste(LagQueue[batch$endRow,],collapse = ",")
  startLag<-paste(LagQueue[batch$startRow,],collapse = ",")
  properRow<-1

  finalLags<-data.frame(c( paste0(LagQueue[OptRow,],collapse = ","),Mincr)  )
  if(!is.function(spec$argsInfo$criterion)){
    colnames(finalLags)<-spec$argsInfo$criterion
  }
  rownames(finalLags)<-c("lag","value")
  Mincr<-NULL

  MyFormula<- as.formula(paste0(preModel$LS_dependent,"~",paste(preModel$LS_longrun,
                      makeShortrunMOdel(spec$extractedInfo$shortRunVars,unlist(LagQueue[OptRow,]),spec$extractedInfo$deterministic),sep="+")) )
  theResults<- lm(MyFormula ,spec$extractedInfo$data)
  theResults$call <- MyFormula
  fittedVars<-fitted(theResults)
  properLag <- unlist(LagQueue[OptRow,])
  MaxLag<-max( properLag)
  end_time <- Sys.time()




  end_time <- Sys.time()
  spec$timeInfo<-list(
    start_time=start_time,
    end_time=end_time,
    span = difftime(end_time, start_time, units = "secs")
  )

  fittedVars<-fitted(theResults)
  k<-length(theResults$coefficients) #k
  n<-length(theResults$residuals)
  spec$estInfo<- list(
    type="kardlmodel",
    method = "grid_custom",
    ModelFormula = theResults$call,
    k=k ,
    n=n,
    start=as.numeric(names(fittedVars[1])),
    end=as.numeric(names(tail(fittedVars,n=1))),
    TimeSpan=n+MaxLag+1
  )
  spec$lagInfo<-list(
    OptLag= properLag,
    allCrLaga = finalLags,
    properRow=OptRow,
    LagsFrom=startLag
  )

  Karamelikli<-lmerge(spec,theResults)
  class(Karamelikli) <- c("kardl_lm","lm")
  Karamelikli
}

# Find Optimum Lags level  by visualization of Estimations
#
# Current job status and remained estimations with progress bar.
# Notice! Users should check the validity of the model and data utilizing this function.
# Appearance mode will have outputs during estimations.
#
# @param inputs All inputs of making model.
# @param ... Other inputs
#
# @return
#
#' @export
makemodel.grid<-function(spec , ... ){#model ,  data,inputs  ){ # makemodel.default
  # inputs$mode<-"grid"

  start_time <- Sys.time()
  # inputs<-prepare(lmerge(list( model=model,data=data),inputs))

  # inputs<-prepare(model=model,data=data  ,inputs)
  #cat("\r",paste0("Appearance mode will have outputs during estimations. Starting of estimation of the model: ",inputs$formulaName))

  preModel<-makeLongrunMOdel(spec) #retruns LS_longrun   LS_dependent
  batch<-BatchControl(spec)

  LagCriteria<-matrix(NA,spec$extractedInfo$lagRowsNumber  ,5) # rows: lag , dependent,LongrunTerms, Shortrun terms, AIC, BIC ,SC,HQ
  colnames(LagCriteria) <- c("lag" , "AIC", "BIC" ,"AICc","HQ")
  bir<-rep(rep(c(1:(spec$argsInfo$maxlag-1)), each = (spec$argsInfo$maxlag)^(spec$extractedInfo$shortrunLength)),time=1)
  for (i in 1:spec$extractedInfo$shortrunLength) {
    r<-rep(rep(c(0:(spec$argsInfo$maxlag-1)), each = (spec$argsInfo$maxlag)^(spec$extractedInfo$shortrunLength-i)),time=(spec$argsInfo$maxlag^i -spec$argsInfo$maxlag^(i-1)) )
    if(length(spec$extractedInfo$ASvars)>0 && (spec$extractedInfo$independentVars[i] %in% spec$extractedInfo$ASvars)){
      if(! spec$argsInfo$differentAsymLag) {
        bir<-cbind(bir,r)
      }
    }
    bir<-cbind(bir,r)
  }
  colnames(bir)<-spec$extractedInfo$shortRunVars
  LagMatrix <-bir[nrow(bir):1,] # I am reversing orders due to alerting about the insufficiency of the degree of freedom.
  colNum=ncol(LagMatrix)
  for (i in 1:nrow(LagMatrix)){
    LagCriteria[i,1] <- paste0(LagMatrix[i,],collapse=",")
  }
  for(i in batch$startRow:batch$endRow)  {
    MyFormula <- as.formula(paste0(preModel$LS_dependent,"~",paste(preModel$LS_longrun,
                      makeShortrunMOdel(spec$extractedInfo$shortRunVars,unlist(LagMatrix[i,]),spec$extractedInfo$deterministic),sep="+")) )
    theResults <- lm(MyFormula ,spec$extractedInfo$data)
    theResults$call <- MyFormula
    # theResults<- makeEstimation(spec$extractedInfo$shortRunVars,LagsList=unlist(LagMatrix[i,]),spec$extractedInfo$deterministic,preModel$LS_dependent,preModel$LS_longrun,spec$extractedInfo$data)


    k<-length(theResults$coefficients) #k
    n<-length(theResults$residuals)  # T
    llh<-logLik(theResults)
    aic<- (2*k-2*llh)/n
    bic<- (log(n)*k-2*llh)/n
    hq<- (2*log(log(n))*k-2*llh)/n
    LagCriteria[i,2]<-aic
    LagCriteria[i,3]<-bic #BIC(est)
    LagCriteria[i,4]<- aic + ((2 * k * (k + 1))/(n - k - 1)) # AICc(est)
    LagCriteria[i,5]<-hq #   -2 * as.numeric(llh) + 2 * k * log(log(n))
    progressBar(i,batch$endRow,as.character(LagCriteria[i,1]))
  }
  cat("\n")
  endLag  <- LagMatrix[batch$endRow ,]# paste(LagMatrix[batch$startRow,],collapse = ",")
  startLag<- LagMatrix[batch$startRow ,]#paste(LagMatrix[batch$endRow,],collapse = ",")
  aicRow<-which(LagCriteria[,2]==min(as.numeric(LagCriteria[,2]), na.rm=T), arr.ind=T)
  bicRow<-which(LagCriteria[,3]==min(as.numeric(LagCriteria[,3]), na.rm=T), arr.ind=T)
  aiccRow<-which(LagCriteria[,4]==min(as.numeric(LagCriteria[,4]), na.rm=T), arr.ind=T)
  hqRow<-which(LagCriteria[,5]==min(as.numeric(LagCriteria[,5]), na.rm=T), arr.ind=T)
  finalLags<-data.frame("AIC"=c(LagCriteria[aicRow,1],LagCriteria[aicRow,2]),
                        "BIC"=c(LagCriteria[bicRow,1],LagCriteria[bicRow,3])   ,
                        "AICc"=c(LagCriteria[aiccRow,1],LagCriteria[aiccRow,4]),
                        "HQ"=c(LagCriteria[hqRow,1],LagCriteria[hqRow,5])
  )

  rownames(finalLags)<-c("lag","value")
  properRow<-switch (spec$argsInfo$criterion,"AIC"=aicRow,"BIC"=bicRow ,"AICc"=aiccRow,"HQ"=hqRow )
  properLag <-LagMatrix[properRow,]
  MyFormula <- as.formula(paste0(preModel$LS_dependent,"~",paste(preModel$LS_longrun,
                      makeShortrunMOdel(spec$extractedInfo$shortRunVars,unlist(properLag),spec$extractedInfo$deterministic),sep="+")) )
  theResults <- lm(MyFormula ,spec$extractedInfo$data)
  theResults$call <- MyFormula

  # theResults<- makeEstimation(spec$extractedInfo$shortRunVars,LagsList=unlist(properLag),spec$extractedInfo$deterministic,preModel$LS_dependent,preModel$LS_longrun,spec$extractedInfo$data)
  k<-length(theResults$coefficients) #k
  n<-length(theResults$residuals)  # T
  fittedVars<-fitted(theResults)
  # model_tidy <- summary(theResults$formula)$coefficients %>%   as_tibble(rownames = "term")
  # colnames(model_tidy) <- c("term", "estimate", "std.error", "statistic", "p.value")


  MaxLag<-max(properLag)
  end_time<- Sys.time()
  #cat("\r The collapsed time is: ", secondsToPeriod(as.numeric(difftime(end_time, start_time, units = "secs"))))

  end_time <- Sys.time()
  spec$timeInfo<-list(
    start_time=start_time,
    end_time=end_time,
    span = difftime(end_time, start_time, units = "secs")
  )

  fittedVars<-fitted(theResults)
  spec$estInfo<- list(
    type="kardlmodel",
    method = "grid",
    ModelFormula = theResults$call,
    k=k ,
    n=n,
    start=as.numeric(names(fittedVars[1])),
    end=as.numeric(names(tail(fittedVars,n=1))),
    TimeSpan=n+MaxLag+1
  )
  spec$lagInfo<-list(
    OptLag= LagMatrix[properRow,],
    allCrLaga = finalLags,
    properRow=properRow,
    Criterion=spec$argsInfo$criterion,
    LagsFrom=startLag,
    LagsTo=endLag,
    LagCriteria=as.data.frame( LagCriteria),
    LagMatrix=LagMatrix
  )

  Karamelikli<-lmerge(spec,theResults)
  class(Karamelikli) <- c("kardl_lm","lm")
  Karamelikli
}



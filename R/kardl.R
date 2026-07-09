#' Estimate ARDL and NARDL Models with Automatic Lag Selection
#'
#' This function estimates an Autoregressive Distributed Lag (ARDL) or Nonlinear
#' ARDL (NARDL) model based on the provided data and model formula.
#' It allows for flexible specification of variables, including deterministic
#' terms, asymmetric variables, and trend components.
#' The function also supports automatic lag selection using various information
#' criteria.
#'
#' @details
#' The general formula for the long-run model is specified as follows:
#' \deqn{
#' \begin{aligned}
#'  \Delta {y}_t =  c  +  \eta _0   {y}_{t-1} + \sum_{j=1}^{p}
#'  { \gamma_{j}  \Delta {y}_{t-j} }   +\sum_{i=1}^{m}
#'  {( \eta ^{+}_i   {x}^{+}_{i,t-1 } + \eta ^{-}_i   {x}^{-}_{i,t-1 } ) }
#'  +  \sum_{i=m+1}^{k} {\eta _i   {x}_{i,t-1 } }
#' +  \newline  \sum_{i=1}^{m} {\sum_{j=0}^{q^+_i}
#' { \beta^+_{ij}   \Delta {x}^+_{i,t-j} } } +
#' \sum_{i=1}^{m} {\sum_{j=0}^{q^-_i} { \beta^-_{ij}   \Delta {x}^-_{i,t-j} } }
#' + \sum_{i=m+1}^{k} {\sum_{j=0}^{q_i} { \beta_{ij}   \Delta {x}_{i,t-j} } }
#' + e_t
#' \end{aligned}
#'  }
#'
#' Where:
#' \itemize{
#' \item \eqn{y_t} is the dependent variable at time t.
#' \item \eqn{c} is the constant term.
#' \item \eqn{\eta_0} is the coefficient of the lagged dependent variable.
#' \item \eqn{\gamma_j} are the coefficients of the lagged differences of the
#' dependent variable.
#' \item \eqn{\eta^+_i} and \eqn{\eta^-_i} are the coefficients of the positive
#' and negative decompositions of the independent variables, respectively.
#' \item \eqn{\eta_i} are the coefficients of the independent variables that
#' do not have asymmetric decompositions.
#' \item \eqn{\beta^+_{ij}} and \eqn{\beta^-_{ij}} are the coefficients of the
#' lagged differences of the positive and negative decompositions of the
#' independent variables, respectively.
#' \item \eqn{\beta_{ij}} are the coefficients of the lagged differences of
#' the independent variables that do not have asymmetric decompositions.
#' \item \eqn{e_t} is the error term at time t.
#' \item \eqn{p} is the maximum lag length for the dependent variable.
#' \item \eqn{q^+_i} and \eqn{q^-_i} are the maximum lag lengths for the
#' positive and negative decompositions of the independent variables,
#' respectively.
#' \item \eqn{q_i} is the maximum lag length for the independent variables
#' that do not have asymmetric decompositions.
#' \item \eqn{m} is the number of independent variables with asymmetric
#' decompositions.
#' \item \eqn{k} is the total number of independent variables.
#' \item \eqn{\Delta} denotes the first difference operator.
#' \item \eqn{x^+_{i,t}} and \eqn{x^-_{i,t}} represent the positive and
#' negative decompositions of the independent variable \eqn{x_i} at time t,
#' respectively.
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
#' and negative partial sum components of a variable, respectively.
#' This notation is used by default and remains valid unless modified through
#' the \code{kardl_set()} function.
#'
#'
#' @param formula A formula specifying the long-run model equation. This formula
#'        defines the relationships between the dependent variable and
#'        explanatory variables, including options for deterministic terms,
#'        asymmetric variables, and a trend component.
#'        Example formula:
#'        \code{y ~ x + z + Asymmetric(z) + Lasymmetric(x2 + x3) +
#'        Sasymmetric(x3 + x4) + deterministic(dummy1 + dummy2) + trend}
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
#' @param data The data of analysis, which should be a data frame containing the
#'  variables referenced in the \code{formula}. The function will check that all
#'   variables specified in the formula are present in the data before
#'   proceeding with estimation. The data should be time-ordered, and the
#'   function will internally handle the construction of lagged and differenced
#'    variables as needed for ARDL and NARDL estimation.
#'
#' @param maxlag An integer specifying the maximum number of lags to be
#'        considered for the model. The default value is \code{4}. This
#'        parameter sets an upper limit on the lag length during the model
#'        estimation process.
#'
#'
#' \strong{\emph{details}}
#'
#'
#' The \code{maxlag} parameter is crucial for defining the maximum lag length
#' that the model will evaluate when selecting the optimal lag structure based
#' on the specified \code{criterion}. It controls the computational effort and
#' helps prevent overfitting by restricting the search space for lag selection.
#' \itemize{
#' \item If the data has a short time horizon or is prone to overfitting,
#'   consider reducing \code{maxlag}.
#' \item If the data is expected to have long-term dependencies, increasing
#'   \code{maxlag} may be necessary to capture the relevant dynamics.
#' }
#'
#' Setting an appropriate value for \code{maxlag} depends on the nature of your
#' dataset and the context of the analysis:
#' \itemize{
#' \item For small datasets or quick tests, use smaller values
#'   (e.g., \code{maxlag = 2}).
#' \item For datasets with more observations or longer-term patterns, larger
#'   values (e.g., \code{maxlag = 8}) may be appropriate, though this increases
#'   computational time.
#' }
#'
#'
#' \strong{\emph{examples}}
#'
#' Using the default maximum lag (4)
#'
#' \code{kardl(data, my_formula, maxlag = 4)}
#'
#' Reducing the maximum lag to 2 for faster computation
#'
#' \code{kardl(data, my_formula, maxlag = 2)}
#'
#' Increasing the maximum lag to 8 for datasets with longer dependencies
#'
#' \code{kardl(data, my_formula, maxlag = 8)}
#'
#' @param mode Specifies the mode of estimation and output control. This
#'        parameter determines how the function handles lag estimation and what
#'        kind of feedback or control is provided during the process. The
#'        available options are:
#'
#' \itemize{
#' \item \strong{"quick"} (default):
#'         Displays progress and messages in the console while the function
#'         estimates the optimal lag values. This mode is suitable for
#'         interactive use or for users who want to monitor the estimation
#'         process in real-time. It provides detailed feedback for debugging or
#'         observation but may use additional resources due to verbose output.
#'
#' \item \strong{"grid"} :
#'         Displays progress and messages in the console while the function
#'         estimates the optimal lag values. This mode is suitable for
#'         interactive use or for users who want to monitor the estimation
#'         process in real-time. It provides detailed feedback for debugging or
#'         observation but may use additional resources due to verbose output.
#'
#' \item \strong{"grid_custom"}:
#'         Suppresses most or all console output, prioritizing faster execution
#'         and reduced resource usage on PCs or servers. This mode is
#'         recommended for high-performance scenarios, batch processing,
#'         or when the estimation process does not require user monitoring.
#'         Suitable for large-scale or repeated runs where output is
#'         unnecessary.
#'
#' \item \strong{User-defined vector}:
#' A numeric vector of lag values specified by the user, allowing full
#' customization of the lag structure used in model estimation. When a
#' user-defined vector is provided (e.g., `c(1, 2, 4, 5)`), the function skips
#' the lag optimization process and directly uses the specified lags.
#'
#' Users can define lag values directly as a numeric vector. For example:
#' \code{mode = c(1, 2, 4, 5)} assigns lags of 1, 2, 4, and 5 to variables in
#' the specified order. Alternatively, lag values can be assigned to variables
#' by name for clarity and control. For example:
#' \code{mode = c(CPI = 2, ER_POS = 3, ER_NEG = 1, PPI = 3)} assigns lags to
#' variables explicitly. Ensure that the lags are correctly designated by
#' verifying the result using \code{kardl_model$proper_lag} after estimation.
#'
#' \strong{\emph{Attention!}}
#' A function-based criterion or user-defined function can be specified
#' for model selection, but this is only supported for
#'  \code{mode = "grid_custom"} and \code{mode = "quick"}. The
#'  \code{mode = "grid"} option is restricted to predefined criteria (e.g., AIC
#'  or BIC). For more information on available criteria, see the
#'  \code{\link{model_criterion}} function documentation.
#' \itemize{
#' \item When using a numeric vector, ensure the order of lag values matches
#'   the variables in your formula.
#' \item If using named vectors, double-check the variable names to avoid
#'   mismatches or unintended results.
#' \item This mode bypasses the automatic lag optimization and assumes the
#'   user-defined lags are correct.
#' }
#' }
#'
#'
#' @param criterion A string specifying the information criterion to be used
#'        for selecting the optimal lag structure. The available options are:
#' \itemize{
#' \item \strong{"AIC"}: Akaike Information Criterion (default). This criterion
#'       balances model fit and complexity, favoring models that explain the
#'       data well with fewer parameters.
#' \item \strong{"BIC"}: Bayesian Information Criterion. This criterion imposes
#'       a stronger penalty for model complexity than AIC, making it more
#'       conservative in selecting models with fewer parameters.
#' \item \strong{"AICc"}: Corrected Akaike Information Criterion. This is an
#'       adjusted version of AIC that accounts for small sample sizes,
#'       making it more suitable when the number of observations is limited
#'       relative to the number of parameters.
#' \item \strong{"HQ"}: Hannan-Quinn Information Criterion. This criterion
#'       provides a compromise between AIC and BIC, favoring models that
#'       balance fit and complexity without being overly conservative.
#' }
#'       The criterion can be specified as a string (e.g., \code{"AIC"}) or as
#'       a user-defined function that takes a fitted model object.
#'       Please visit the \code{\link{model_criterion}} function documentation
#'       for more details on using custom criteria.
#'
#' @param different_asym_lag A logical value indicating whether to allow
#'        different lag lengths for positive and negative decompositions.
#' @param batch A string specifying the batch processing configuration in the
#'        format "current_batch/total_batches". If a user utilize grid or
#'        grid_custom mode and want to split the lag search into multiple
#'        batches, this parameter can be used to define the current batch and
#'        the total number of batches. For example, "2/5" indicates that the
#'        current batch is the second out of a total of five batches.
#'        The default value is "1/1", meaning that the entire lag search is
#'        performed in a single batch.
#'
#' @param ... Additional arguments that can be passed to the function. These
#'        arguments can be used to specify other settings or parameters that
#'        are not explicitly defined in the main arguments.
#'
#' @return An object of class \code{kardl_lm} containing the estimated ARDL or
#'        NARDL model. The object includes the following components:
#' \describe{
#' \item{args_info}{A list of input arguments used for the estimation. It
#'       includes the data, formula, maxlag, mode, criterion,
#'       different_asym_lag, and batch settings.}
#' \item{extracted_info}{A list containing extracted information from the input
#'       data and formula, such as variable names, deterministic terms,
#'       asymmetric variables, and the prepared dataset for estimation.}
#' \item{time_info}{A list containing timing information for the estimation
#'       process, including start time, end time, and total duration.}
#' \item{lag_info}{A list containing lag selection information, including the
#'       optimal lag configuration and criteria values for different lag
#'       combinations.}
#' \item{est_info}{A list containing estimation details, such as the type of
#'       model, estimation method, model formula, number of parameters (k),
#'       number of observations (n), start and end points of the fitted values,
#'       and total time span.}
#' \item{model}{The fitted linear model object of class \code{lm} representing
#'       the estimated ARDL or NARDL model.}
#' }
#'
#'
#' @seealso  \code{\link{ecm}}, \code{\link{kardl_set}},
#'           \code{\link{kardl_get}}, \code{\link{kardl_reset}},
#'           \code{\link{model_criterion}}
#'
#' @srrstats {G1.1} The package documentation describes `kardl` as an R
#'           implementation and extension of ARDL and NARDL workflows, with
#'           emphasis on mixed symmetric and asymmetric regressors, flexible
#'           lag selection, and dynamic multiplier methods.
#' @srrstats {G2.0} Main input arguments including `formula`, `data`, `maxlag`,
#'           `mode`, `criterion`, and `different_asym_lag` are validated before
#'           estimation proceeds.
#' @srrstats {G2.0a} The `formula` argument expects a single language object
#'           specifying the long-run ARDL equation; `maxlag` must be a positive
#'           integer; `mode` accepts predefined strings or a numeric lag vector.
#' @srrstats {G2.1} The function checks that `formula` is a language object and
#'           that `data` contains all variables referenced in the formula before
#'           estimation.
#' @srrstats {G2.3} Character-valued arguments such as `mode` and `criterion`
#'           are matched against predefined acceptable values via `match.arg()`
#'           internally.
#' @srrstats {G2.3a} The `criterion` argument is matched against
#'           `c("AIC", "BIC", "AICc", "HQ")`; the `mode` argument against
#'           `c("quick", "grid", "grid_custom")`.
#' @srrstats {G2.4} Formula parsing and data alignment are performed before
#'           estimation so all subsequent routines operate on a consistent
#'           internal representation.
#' @srrstats {G2.4a} The `maxlag` argument is treated as an integer count
#'           before constructing lagged regressors.
#' @srrstats {G2.13} Missing observations introduced by lagging and differencing
#'           are handled during model-frame construction before estimation.
#' @srrstats {G2.14} Observations unavailable after lag construction are
#'           excluded from the estimation sample.
#' @srrstats {TS1.0} The function accepts time-ordered data and constructs
#'           lagged and differenced variables internally for ARDL and NARDL
#'           estimation.
#' @srrstats {TS1.3} A central preparation workflow parses the formula,
#'           constructs lagged variables, and returns a uniform internal model
#'           data structure.
#' @srrstats {TS4.0} The fitted model is returned as an object of class
#'           `kardl_lm` with dedicated `print`, `summary`, and accessor methods.
#' @srrstats {TS4.0b} The return value uses the explicit class `kardl_lm`.
#'
#' @export
#'
#' @import lmtest stats lifecycle
#' @importFrom utils methods capture.output tail flush.console
#'
#' @examples
#'
#'
#' # Example: Road safety analysis using UK Seatbelts data
#' # Analyzing the effect of seatbelt law on driver deaths
#'
#' kardl_set(
#'   formula = DriversKilled ~ PetrolPrice + drivers + Asymmetr(PetrolPrice) +
#'     deterministic(law) + trend,
#'   data = Seatbelts,
#'   maxlag = 2
#' ) # setting the default values of the kardl function
#'
#'
#' # using the grid_custom mode with batch processing
#'
#' kardl_model_grid <- kardl(
#'   mode = "grid_custom",
#'   batch = "2/3",
#'   criterion = "BIC"
#' )
#' kardl_model_grid
#'
#' kardl_model2 <- kardl(mode = c(2, 1, 1, 3))
#'
#' # Getting the results
#' kardl_model2
#'
#' # Getting the summary of the results
#' summary(kardl_model2)
#'
#' # using '.' in the formula means that all variables in the data will be used
#'
#' fit_bic <- kardl(formula = DriversKilled ~ . + deterministic(law))
#' fit_bic
#'
#' # Setting max lag instead of default value [4]
#' kardl(DriversKilled ~ PetrolPrice + drivers + Lasymmetric(PetrolPrice),
#'   Seatbelts,
#'   maxlag = 3, mode = "grid_custom"
#' )
#'
#' # Using another criterion for finding the best lag
#' kardl_set(criterion = "HQ") # setting the criterion to HQ
#' kardl(mode = "grid_custom")
#'
#' # using default values of lags
#' kardl(mode = c(1, 2, 3, 0))
#'
#' # For using different lag values for negative and positive decompositions
#' # of non-linear variables setting the same lags for positive and negative
#' # decompositions.
#'
#' same <- kardl(
#'   formula = DriversKilled ~ Asymmetric(PetrolPrice),
#'   maxlag = 2, mode = "grid_custom",
#'   different_asym_lag = FALSE
#' )
#' dif <- kardl(
#'   formula = DriversKilled ~ Sasymmetric(PetrolPrice),
#'   maxlag = 2, mode = "grid_custom",
#'   different_asym_lag = TRUE
#' )
#' kardl_extract(same, "opt_lag")
#' kardl_extract(dif, "opt_lag")
#'
#' # Optional: use magrittr if available
#' @examplesIf requireNamespace("magrittr", quietly = TRUE)
#' library(magrittr)
#' kardl_model_pipe <- Seatbelts %>%
#'   kardl(mode = "grid_custom", data = .)
#' kardl_model_pipe
#'
kardl <- function(
  formula = NULL,
  data = NULL,
  maxlag = NULL,
  mode = NULL,
  criterion = NULL,
  different_asym_lag = NULL,
  batch = NULL,
  ...
) {
  # backward compatibility
  if (inherits(data, "formula")) {
    lifecycle::deprecate_warn(
      when = "2.0.1",
      what = I("Calling `kardl()` as `kardl(data, formula)`"),
      with = I("`kardl(formula, data)`"),
      details = paste(
        "The argument order of `formula` and `data` has been",
        "revised to follow standard R modeling conventions.",
        "Please update your code to use `kardl(formula, data)`",
        "instead of `kardl(data, formula)`. The previous",
        " argument order remains supported for backward",
        "compatibility but will be deprecated in future versions."
      )
    )

    tmp <- formula
    formula <- data
    data <- tmp
  }
  args_info <- list(
    data = data,
    formula = formula,
    maxlag = maxlag,
    mode = mode,
    criterion = criterion,
    different_asym_lag = different_asym_lag,
    batch = batch
  )
  otherargs_info <- list(...)
  kardl_vars <- lmerge(args_info, otherargs_info)
  my_names <- names(kardl_vars)
  for (i in seq_along(kardl_vars)) {
    name <- my_names[i]
    if (!nzchar(name)) {
      kardl_vars[[i]] <- NULL
      next
    }
    if (is.null(kardl_vars[[name]])) {
      kardl_vars[[name]] <- kardl_get(name)
      if (is.null(kardl_vars[[name]])) {
        stop(
          "No ",
          name,
          " provided. Please supply `",
          name,
          "` or set it with kardl_set(",
          name,
          " = ...).",
          call. = FALSE
        )
      }
      attr(kardl_vars[[name]], "source") <- "kardl_set"
    } else {
      attr(kardl_vars[[name]], "source") <- "argument"
    }
  }
  spec <- prepare(kardl_vars)
  makemodel(spec, ...)
}
#' Estimate a Restricted ECM Model
#'
#' The `ecm` function estimates a restricted Error Correction Model (ECM) based
#' on the provided data and model specification. This function is designed to
#' test for cointegration using the PSS t Bound test, which assesses the
#' presence of a long-term equilibrium relationship between the dependent
#' variable and the independent variables in the model.
#'
#' @inheritParams kardl
#' @inheritSection kardl Notation of reported coefficients
#'
#' @section Hypothesis testing:
#'
#' The null and alternative hypotheses for the restricted ECM test are as
#' follows:
#'
#'   \deqn{\mathbf{H_{0}:} \theta  =   0}
#'   \deqn{\mathbf{H_{1}:} \theta  \neq 0}
#'
#'   The null hypothesis (\eqn{H_{0}}) states that there is no cointegration in
#'   the model, meaning that the long-run relationship between the variables is
#'   not significant. The alternative hypothesis (\eqn{H_{1}}) suggests that
#'   there is cointegration, indicating a significant long-term relationship
#'   between the variables.
#'
#'   The test statistic is calculated as the t-statistic of the coefficient of
#'   the error correction term (\eqn{\theta}) in the ECM model. If the absolute
#'   value of the t-statistic exceeds the critical value from the PSS t Bound
#'   table, we reject the null hypothesis in favor of the alternative
#'   hypothesis, indicating that cointegration is present.
#'
#'   The cases for the restricted ECM Bound test are defined as follows:
#'
#' \itemize{
#' \item \code{case 1}: No constant, no trend.
#'
#'   This case is used when the model does not include a constant term or a
#'   trend term. It is suitable for models where the variables are stationary
#'   and do not exhibit any long-term trends.
#'
#'   The model is specified as follows:
#'
#'   \deqn{
#' \begin{aligned}
#' \Delta y_t =  \sum_{j=1}^{p} \gamma_j \Delta y_{t-j} +
#' \sum_{i=1}^{k} \sum_{j=0}^{q_i} \beta_{ij} \Delta x_{i,t-j} +
#' \theta (y_{t-1}  - \sum_{i=1}^{k} \alpha_i x_{i,t-1} ) + e_t
#' \end{aligned}
#' }
#'
#' \item \code{case 2}: Restricted constant, no trend.
#'
#'   This case is used when the model includes a constant term but no trend
#'   term. It is suitable for models where the variables exhibit a long-term
#'   relationship but do not have a trend component.
#'
#'   The model is specified as follows:
#'
#'   \deqn{
#'   \begin{aligned}
#'   \Delta y_t &= \sum_{j=1}^{p} \gamma_j \Delta y_{t-j} +
#'   \sum_{i=1}^{k} \sum_{j=0}^{q_i} \beta_{ij} \Delta x_{i,t-j} +
#'   \theta (y_{t-1} - \alpha_0 - \sum_{i=1}^{k} \alpha_i x_{i,t-1} ) + e_t
#'   \end{aligned}
#'   }
#'
#' \item \code{case 3}: Unrestricted constant, no trend.
#'
#'   This case is used when the model includes an unrestricted constant term
#'   but no trend term. It is suitable for models where the variables exhibit
#'   a long-term relationship with a constant but do not have a trend
#'   component.
#'
#'   The model is specified as follows:
#'
#'   \deqn{
#'   \begin{aligned}
#'   \Delta y_t &= \sum_{j=1}^{p} \gamma_j \Delta y_{t-j} +
#'   \sum_{i=1}^{k} \sum_{j=0}^{q_i} \beta_{ij} \Delta x_{i,t-j} +
#'   \theta (y_{t-1} - \alpha_0 - \sum_{i=1}^{k} \alpha_i x_{i,t-1} ) + e_t
#'   \end{aligned}
#'     }
#'
#' \item \code{case 4}: Unrestricted Constant, restricted trend.
#'
#'   This case is used when the model includes an unrestricted constant term
#'   and a restricted trend term. It is suitable for models where the variables
#'   exhibit a long-term relationship with a constant and a trend component.
#'
#'   The model is specified as follows:
#'
#'   \deqn{
#'   \begin{aligned}
#'   \Delta y_t &= \phi + \sum_{j=1}^{p} \gamma_j \Delta y_{t-j} +
#'   \sum_{i=1}^{k} \sum_{j=0}^{q_i} \beta_{ij} \Delta x_{i,t-j} +
#'   \theta (y_{t-1} - \pi (t-1) - \sum_{i=1}^{k} \alpha_i x_{i,t-1} ) + e_t
#'   \end{aligned}
#'    }
#'
#' \item \code{case 5}: Unrestricted constant, unrestricted trend.
#' }
#'
#' The Error Correction Model (ECM) is specified as follows:
#'
#' \deqn{
#' \begin{aligned}
#' \Delta y_t &= \phi + \varphi t + \sum_{j=1}^{p} \gamma_j \Delta y_{t-j} +
#' \sum_{i=1}^{k} \sum_{j=0}^{q_i} \beta_{ij} \Delta x_{i,t-j} +
#' \theta (y_{t-1} - \sum_{i=1}^{k} \alpha_i x_{i,t-1} ) + e_t
#' \end{aligned}
#' }
#'
#' @return A list containing the results of the restricted ECM test, including:
#' \itemize{
#' \item \code{ecm}: The estimated ECM model objects including:
#' \itemize{
#' \item \code{longrun_eq}: The estimated long-run model equation object.
#' \item \code{shortrun_eq}: The estimated short-run model equation.
#' \item \code{ecm_l}: The estimated long-run model object.
#' }
#' \item \strong{args_info}: A list of input arguments used for the estimation.
#'       It includes the data, formula, maxlag, mode, criterion,
#'       different_asym_lag, and batch settings.
#' \item \strong{extracted_info}: A list containing extracted information from
#'       the input data and formula, such as variable names, deterministic
#'       terms, asymmetric variables, and the prepared dataset for estimation.
#' \item \strong{time_info}: A list containing timing information for the
#'       estimation process, including start time, end time, and total duration.
#' \item \strong{lag_info}: A list containing lag selection information,
#'       including the optimal lag configuration and criteria values for
#'       different lag combinations.
#' \item \strong{est_info}: A list containing estimation details, such as the
#'       type of model, estimation method, model formula, number of parameters
#'       (k), number of observations (n), start and end points of the fitted
#'       values, and total time span.
#' \item \strong{model}: The fitted linear model object of class \code{lm}
#'       representing the estimated ARDL or NARDL model.
#' }
#'
#' @srrstats {G2.0} Input arguments `formula`, `data`, `maxlag`, `mode`,
#'           `criterion`, `different_asym_lag`, and `batch` are validated before
#'           the ECM is estimated.
#' @srrstats {G2.1} The function checks that `formula` is a valid language
#'           object and that `data` contains all model variables before
#'           proceeding.
#' @srrstats {TS2.0} ECM estimation assumes a regular ordered sequence after
#'           preprocessing; structurally missing observations created by
#'           lagging are handled explicitly.
#' @srrstats {TS2.1} Missing-data handling is part of the model-preparation
#'           step, not a separate imputation system.
#' @srrstats {TS4.0} The fitted ECM is returned as an object of class
#'           `kardl_lm` with dedicated `print` and `summary` methods.
#'
#' @export
#' @seealso \code{\link{kardl}} \code{\link{pssf}} \code{\link{psst}}
#'          \code{\link{ecm}} \code{\link{narayan}}
#'
#' @examples
#'
#' # Example: Road safety analysis using UK Seatbelts data
#' # Analyzing the effect of seatbelt law on driver deaths
#' kardl_set(
#'   formula = DriversKilled ~ PetrolPrice + drivers + asym(PetrolPrice) + deterministic(law) + trend,
#'   data = Seatbelts,
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
#' ecm_model <- ecm(
#'   mode = "grid_custom",
#'   criterion = "HQ", batch = "2/3"
#' )
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
#' summary(ecm(DriversKilled ~ drivers + asym(PetrolPrice) + trend, Seatbelts))
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
#' kardl_set(different_asym_lag = FALSE)
#'
#' diffAsymLags <- ecm(mode = "grid_custom")
#' kardl_extract(diffAsymLags, "opt_lag")
#'
#' # Setting different lags for positive and negative decompositions
#' sameAsymLags <- ecm(different_asym_lag = TRUE, mode = "grid_custom")
#' kardl_extract(sameAsymLags, "opt_lag")
#'
#' # Setting the prefixes and suffixes for nonlinear variables
#' kardl_reset()
#' kardl_set(
#'   asym_prefix = c("asyP_", "asyN_"),
#'   asym_suffix = c("_PP", "_NN")
#' )
#' customizedNames <- ecm(DriversKilled ~ PetrolPrice + drivers + asym(PetrolPrice), Seatbelts)
#' customizedNames
#'
ecm <- function(
  formula = NULL,
  data = NULL,
  maxlag = NULL,
  mode = NULL,
  criterion = NULL,
  different_asym_lag = NULL,
  batch = NULL,
  ...
) {
  if (inherits(data, "formula")) {
    lifecycle::deprecate_warn(
      when = "2.0.1",
      what = I("Calling `ecm()` as `ecm(data, formula)`"),
      with = I("`ecm(formula, data)`"),
      details = paste(
        "The argument order of `formula` and `data` has been",
        "revised to follow standard R modeling conventions.",
        "Please update your code to use `ecm(formula, data)`",
        "instead of `ecm(data, formula)`. The previous",
        " argument order remains supported for backward",
        "compatibility but will be deprecated in future versions."
      )
    )

    tmp <- formula
    formula <- data
    data <- tmp
  }
  notes_array <- c()
  get_proper <- kardl(
    formula = formula,
    data,
    maxlag = maxlag,
    mode = mode,
    criterion = criterion,
    different_asym_lag = different_asym_lag,
    batch = batch,
    ...
  )

  ecm_data <- get_proper$model

  longrun_eq <- paste0(
    replace_lag_var(
      .kardl_settings_env$long_coef,
      get_proper$extracted_info$dependent_var,
      1
    ),
    " ~ ",
    paste(
      replace_lag_var(
        .kardl_settings_env$long_coef,
        get_proper$extracted_info$long_run_vars[-1],
        1
      ),
      collapse = " + "
    )
  )
  longrun_eq <- as.formula(longrun_eq)

  ecm_l <- lm(longrun_eq, ecm_data, ...)
  ecm_l$call <- longrun_eq

  ecm_data$EcmRes <- residuals(ecm_l)

  f <- get_proper$terms
  to_remove <- all.vars(longrun_eq)
  tt <- terms(f)
  rhs_terms <- attr(tt, "term.labels")
  new_rhs <- setdiff(rhs_terms, to_remove)
  shortrun_eq <- reformulate(new_rhs, response = as.character(f[[2]]))
  if (attr(tt, "intercept") == 0) {
    shortrun_eq <- update(shortrun_eq, . ~ . - 1)
  }

  shortrun_eq <- update(shortrun_eq, . ~ EcmRes + .)
  ecm_s <- lm(shortrun_eq, ecm_data, ...)

  ecm_list <- list(
    ecm = list(
      longrun_eq = longrun_eq,
      shortrun_eq = shortrun_eq,
      ecm_l = ecm_l
    ),
    args_info = get_proper$args_info,
    extracted_info = get_proper$extracted_info,
    time_info = get_proper$time_info,
    est_info = get_proper$est_info,
    lag_info = get_proper$lag_info
  )

  ecm_list$est_info$type <- "ecm"
  ecm_output <- lmerge(ecm_list, ecm_s)
  if (coef(ecm_s)[["EcmRes"]] >= 0) {
    notes_array <- c(
      notes_array,
      paste0(
        "The coefficient of the error correction term (EcmRes) is ",
        "non-negative. This may indicate a lack of long-run equilibrium ",
        "adjustment."
      )
    )
  }
  if (coef(ecm_s)[["EcmRes"]] < -1) {
    notes_array <- c(
      notes_array,
      paste0(
        "The coefficient of the error correction term (EcmRes) is less ",
        "than -1. This may suggest over-adjustment or instability in the ",
        "long-run relationship."
      )
    )
  }

  ecm_output$notes <- notes_array
  class(ecm_output) <- c("kardl_lm", "lm")
  ecm_output
}

#' Predict method for kardl models
#'
#' @param object A fitted object of class `"kardl_lm"`.
#' @param newdata Optional data frame for prediction. If omitted, fitted values
#'   from the original model are returned.
#' @param interval Prediction interval type. Passed to [stats::predict.lm()].
#' @param level Confidence level for intervals.
#' @param ... Further arguments passed to [stats::predict.lm()].
#'
#' @return A numeric vector or matrix of predicted values.
#' @noRd
#' @export
predict.kardl_lm <- function(object, newdata = NULL, ...) {
  if (is.null(newdata)) {
    newdata <- object$model
  } else {
    object$args_info$data <- newdata

    new_object <- create_new_vars(object)
    newdata <- new_object$extracted_info$data
  }


  stats::predict.lm(
    object = object,
    newdata = newdata,
    ...
  )
}

#' Model Selection Criteria
#'
#' Computes a model selection criterion (AIC, BIC, AICc, or HQ) or applies
#' a user-defined function to evaluate a statistical model.
#'
#'
#' @param lm_model An object containing the fitted model. The object should
#'        include at least:
#' \itemize{
#' \item \code{lm_model$model} - the actual fitted model object
#'       (e.g., from \code{lm}, \code{glm}).
#' \item \code{k} - the number of estimated parameters.
#' \item \code{n} - the sample size.
#' }
#' @param cr A character string specifying the criterion to compute.
#'           Options are \code{"AIC"}, \code{"BIC"}, \code{"AICc"}, and
#'           \code{"HQ"}. Alternatively, a user-defined function can be
#'           provided. See details below for more information on using custom
#'            criteria.
#' @param ... Additional arguments passed to the user-defined criterion
#'        function if \code{cr} is a function.
#'
#' @return A numeric value representing the selected criterion, normalized
#'         by the sample size if one of the predefined options is used.
#'
#' @details
#' This function returns model selection criteria used to compare the quality of
#'  different models.
#' All criteria are defined such that \strong{lower values indicate better
#' models} (i.e., the goal is minimization).
#'
#' If you wish to compare models using a maximization approach (e.g.,
#'  log-likelihood),
#' you can multiply the result by \code{-1}.
#'
#' Note: The predefined string options (e.g., \code{"AIC"}) are \strong{not}
#' the same as the built-in R functions \code{AIC()} or \code{BIC()}.
#' In particular, the values returned by this function are adjusted by
#' dividing by the sample size \code{n} (i.e., normalized AIC/BIC), which
#' makes it more comparable across datasets of different sizes.
#'
#' The function returns:
#' \itemize{
#' \item \strong{"AIC"}:  \eqn{ \frac{2k - 2\ell}{n} } Akaike Information
#'       Criterion divided by \code{n}.
#' \item \strong{"BIC"}:  \eqn{ \frac{\log(n) \cdot k - 2\ell}{n} } Bayesian
#'       Information Criterion divided by \code{n}.
#' \item \strong{"AICc"}: \eqn{ \frac{2k(k+1)}{n - k - 1} +
#'  \frac{2k - 2\ell}{n} }
#'       Corrected Akaike Information Criterion divided by \code{n}.
#' \item \strong{"HQ"}: \eqn{ \frac{2 \log(\log(n)) \cdot k - 2\ell}{n} }
#'       Hannan-Quinn Criterion divided by \code{n}.
#' }
#'
#' where:
#' \itemize{
#' \item \eqn{k} is the number of parameters,
#' \item \eqn{n} is the sample size,
#' \item \eqn{\ell} is the log-likelihood of the model.
#' }
#'
#' If \code{cr} is a function, it is called with the fitted model and any
#' additional arguments passed through \code{...}.
#' @seealso \code{\link{kardl}}
#'
#' @srrstats {G1.6} This function provides criteria for model selection,
#'           including AIC, BIC, AICc, and HQ, which can be used in R based
#'           model selection processes too.
#' @srrstats {G2.3} The `cr` argument is matched against
#'           `c("AIC", "BIC", "AICc", "HQ")` via `match.arg()` when a string
#'           is supplied.
#' @srrstats {G2.3a} `match.arg()` is used to validate the `cr` argument when it
#'            is a character string.
#' @srrstats {G2.3b} Using `tolower()` to make the `cr` argument
#'            case-insensitive.
#' @srrstats {G3.0} Numerical comparisons in criterion calculations use
#'           tolerance-based floating-point arithmetic where appropriate.
#'
#' @export
#' @examples
#'
#' # Example usage of model_criterion function with a simple linear model
#' mylm <- lm(mpg ~ wt + hp, data = mtcars)
#' model_criterion(mylm, AIC)
#' model_criterion(mylm, "AIC")
#'
#' # Example usage of model_criterion function with a kardl model
#' kardl_model <- kardl(
#'   DriversKilled ~ PetrolPrice + drivers + asym(PetrolPrice) + deterministic(law) + trend,
#'   Seatbelts,
#'   mode = c(1, 2, 3, 0)
#' )
#'
#' # Using AIC as the kardl package's built-in criterion function which is
#' # different from the base R AIC function.
#' model_criterion(kardl_model, "AIC")
#'
#' # Using the base R AIC function directly on the fitted model object
#' model_criterion(kardl_model, AIC)
#' # Using the base R AIC function outside of model_criterion to compute AIC for
#' #  the fitted model
#' AIC(kardl_model)
#'
#' # Using BIC as the criterion for the kardl model which is different from the
#' # base R BIC function.
#' model_criterion(kardl_model, "BIC")
#'
#' # Using a custom criterion function that divides AIC by the sample size
#' my_cr_fun <- function(mod, ...) {
#'   AIC(mod) / length(mod$model[[1]])
#' }
#' model_criterion(kardl_model, my_cr_fun)
#'
model_criterion <- function(lm_model, cr, ...) {
  UseMethod("model_criterion")
}

#' Default method for model_criterion
#'
#' This is the default method for the `model_criterion` function. It is called
#' when the class of `lm_model` does not have a specific method defined. The
#' function throws an error indicating that no method is available for the class
#'  of the fitted model object and suggests providing a fitted model object of
#'  class `lm`.
#'
#' @exportS3Method
#' @noRd
#'
model_criterion.default <- function(lm_model, cr, ...) {
  stop(
    "No method available for objects of class '",
    class(lm_model),
    "'. Please provide a fitted model object of class 'lm'."
  )
}

#' Model Selection Criterion for Linear Models
#'
#' This function computes the specified model selection criterion (AIC, BIC,
#' AICc, or HQ) for a fitted linear model object of class `lm`. It can also
#' accept a user-defined function as the criterion. The function returns the
#' computed criterion value normalized by the sample size.
#'
#' @noRd
#' @export
#' @method model_criterion lm
model_criterion.lm <- function(lm_model, cr, ...) {
  if (is.function(cr)) {
    do.call(cr, list(lm_model, ...))
  } else {
    cr <- tolower(cr)
    my_cr <- match.arg(cr, c("aic", "bic", "aicc", "hq"))
    k <- length(lm_model$coefficients) # k
    n <- length(lm_model$residuals)
    llh <- logLik(lm_model)
    val <- switch(my_cr,
      "aic" = ((2 * k - 2 * llh) / n),
      "bic" = ((log(n) * k - 2 * llh) / n),
      "aicc" = (((2 * k * (k + 1)) / (n - k - 1)) + (2 * k - 2 * llh) / n),
      "hq" = ((2 *
        log(log(
          n
        )) *
        k -
        2 * llh) /
        n)
    )
    as.numeric(val)
  }
}


#'  Model Estimation Dispatcher
#'
#' This function serves as a dispatcher for estimating the ARDL or NARDL model
#' based on the specified mode in the input arguments. It determines the
#' appropriate estimation method to use (e.g., "quick", "grid_custom", "grid",
#' or user-defined) and calls the corresponding S3 method for model estimation.
#'
#' @param spec A list containing the prepared specifications for model
#'        estimation, including extracted information and arguments.
#' @param ... Additional arguments that can be passed to the specific
#'        estimation methods.
#' @return The estimated model object based on the specified mode.
#' @noRd

makemodel <- function(spec, ...) {
  my_method <- "quick"
  # if(is.vector(inputs$mode))
  if (
    is.vector(spec$args_info$mode[1]) &&
      is.numeric(spec$args_info$mode) &&
      isFALSE(isFALSE(spec$args_info$mode))
  ) {
    class(my_method) <- "user"
  } else {
    class(my_method) <- match.arg(
      tolower(spec$args_info$mode),
      c("quick", "grid_custom", "grid")
    )
  }
  UseMethod("makemodel", my_method)
}


#' Quick Model Estimation Method
#'
#' This function implements the "quick" estimation method for ARDL or NARDL
#' models. It performs a stepwise search to find the optimal lag structure
#' based on the specified criterion. The function iteratively tests different
#' lag combinations and evaluates the model using the provided criterion
#' until it converges to the best lag configuration.
#'
#' @param spec A list containing the prepared specifications for model
#'        estimation, including extracted information and arguments.
#' including extracted information and arguments.
#' @param ... Additional arguments that can be passed to the function.
#' @return An object of class \code{kardl_lm} containing the estimated
#' ARDL or NARDL model based on the "quick" estimation method.
#' @noRd
#'
#' @export
makemodel.quick <- function(spec, ...) {
  start_time <- Sys.time()
  pre_model <- make_longrun_model(spec)

  # for faster access to the extracted information in the loop
  s_info <- spec$extracted_info

  x_length <- length(s_info$short_run_vars)
  lags_list <- rep(0, x_length)
  lags_list <- as.numeric(lags_list)
  names(lags_list) <- s_info$short_run_vars

  failed_checks <- matrix(NA, 1, x_length)[-1, ]
  colnames(failed_checks) <- s_info$short_run_vars

  toporders <- matrix(0, 1, x_length + 1)[-1, ]
  colnames(toporders) <- c(
    s_info$short_run_vars,
    "criterion_value"
  )

  my_est <- function(order) {
    the_results <- lm(
      as.formula(paste0(
        pre_model$ls_dependent,
        "~",
        paste(
          pre_model$ls_longrun,
          make_shortrun_model(
            s_info$short_run_vars,
            order,
            s_info$deterministic
          ),
          sep = "+"
        )
      )),
      s_info$data, ...
    )
    model_criterion(the_results, spec$args_info$criterion)
  }

  for (i in 1:spec$args_info$maxlag) {
    ardl_converge <- FALSE
    order1 <- rep(i, x_length)
    base_cr <- my_est(order1)
    toporders <- rbind(toporders, c(order1, base_cr))
    while (!ardl_converge) {
      for (j in 1:(x_length - 1)) {
        order2_back <- order1
        order2_forth <- order1
        if (order1[j + 1] == 0) {
          order2_forth[j + 1] <- order1[j + 1] + 1
        } else {
          if (order1[j + 1] == spec$args_info$maxlag) {
            order2_back[j + 1] <- order1[j + 1] - 1
          } else {
            order2_back[j + 1] <- order1[j + 1] - 1
            order2_forth[j + 1] <- order1[j + 1] + 1
          }
        }
        model0 <- my_est(order2_back)
        model1 <- my_est(order2_forth)
        min_cr <- min(model0, model1)
        if (base_cr > min_cr) {
          if (model0 > model1) {
            order1 <- order2_forth
          } else {
            order1 <- order2_back
          }
          toporders <- rbind(toporders, c(order1, min_cr))
          base_cr <- min_cr
          ardl_converge <- FALSE
        } else {
          if (
            any(apply(failed_checks, 1, function(row) {
              all(row == order1)
            }))
          ) {
            ardl_converge <- TRUE
          } else {
            failed_checks <- rbind(failed_checks, order1)
          }
        }
      }
    }
  }

  min_value <- toporders[
    which.min(toporders[, "criterion_value"]), ,
    drop = FALSE
  ]

  best_order <- min_value[, -ncol(min_value)]
  my_formula <- as.formula(paste0(
    pre_model$ls_dependent,
    "~",
    paste(
      pre_model$ls_longrun,
      make_shortrun_model(
        s_info$short_run_vars,
        best_order,
        s_info$deterministic
      ),
      sep = "+"
    )
  ))
  model_data <- s_info$data

  the_results <- lm(
    formula = my_formula,
    data = model_data,
    ...
  )

  end_time <- Sys.time()
  spec$time_info <- list(
    start_time = start_time,
    end_time = end_time,
    span = difftime(end_time, start_time, units = "secs")
  )
  spec$lag_info <- list(opt_lag = best_order, lag_criteria = toporders)
  fitted_vars <- fitted(the_results)
  k <- length(the_results$coefficients)
  n <- length(the_results$residuals)
  spec$est_info <- list(
    type = "kardlmodel",
    method = "quick",
    model_formula = my_formula,
    k = k,
    n = n,
    start = as.numeric(names(fitted_vars[1])),
    end = as.numeric(names(tail(fitted_vars, n = 1))),
    time_span = n + spec$args_info$maxlag + 1
  )

  karamelikli <- lmerge(spec, the_results)
  class(karamelikli) <- c("kardl_lm", "lm")
  karamelikli
}

#' User-Defined Model Estimation Method
#'
#' This function implements the estimation method for ARDL or NARDL models when
#' the user provides a specific lag structure. It estimates the model using the
#' provided lag configuration and evaluates it based on the specified
#' criterion.
#'
#' @param spec A list containing the prepared specifications for model
#'        estimation, including extracted information and arguments.
#' @param ... Additional arguments that can be passed to the function.
#' @return An object of class \code{kardl_lm} and \code{lm} containing the
#' estimated ARDL or NARDL model based on the user-defined lag structure.
#' @noRd
#' @export
makemodel.user <- function(spec, ...) {
  # for faster access to the extracted information in the loop
  s_info <- spec$extracted_info
  start_time <- Sys.time()
  pre_model <- make_longrun_model(spec)
  my_formula <- as.formula(paste0(
    pre_model$ls_dependent,
    "~",
    paste(
      pre_model$ls_longrun,
      make_shortrun_model(
        s_info$short_run_vars,
        spec$args_info$mode,
        s_info$deterministic
      ),
      sep = "+"
    )
  ))

  model_data <- s_info$data

  the_results <- lm(
    formula = my_formula,
    data = model_data,
    ...
  )


  opt_lag <- spec$args_info$mode
  attr(opt_lag, "source") <- NULL
  attr(opt_lag, "description") <- NULL

  end_time <- Sys.time()

  spec$time_info <- list(
    start_time = start_time,
    end_time = end_time,
    span = difftime(end_time, start_time, units = "secs")
  )
  spec$lag_info <- list(opt_lag = opt_lag)
  fitted_vars <- fitted(the_results)
  k <- length(the_results$coefficients)
  n <- length(the_results$residuals)
  spec$est_info <- list(
    type = "kardlmodel",
    method = "user",
    model_formula = my_formula,
    k = k,
    n = n,
    start = as.numeric(names(fitted_vars[1])),
    end = as.numeric(names(tail(fitted_vars, n = 1))),
    time_span = n + max(opt_lag) + 1
  )

  karamelikli <- lmerge(spec, the_results)
  class(karamelikli) <- c("kardl_lm", "lm")
  karamelikli
}

#' Grid Custom Model Estimation Method
#'
#' This function implements the "grid_custom" estimation method for ARDL or
#' NARDL models. It performs a grid search over possible lag combinations to
#' find the optimal lag structure based on the specified criterion.
#'
#' @param spec A list containing the prepared specifications for model
#'        estimation, including extracted information and arguments.
#' @return An object of class \code{kardl_lm} containing the estimated model.
#' @noRd
#' @export
makemodel.grid_custom <- function(spec, ...) {
  start_time <- Sys.time()
  pre_model <- make_longrun_model(spec)
  batch <- batch_control(spec)
  spec$extracted_info <- lmerge(batch, spec$extracted_info)
  # for faster access to the extracted information in the loop
  s_info <- spec$extracted_info

  order_for_short_run <- rep(
    list((spec$args_info$maxlag - 1):0),
    s_info$shortrun_length
  )
  order_for_ind <- list((spec$args_info$maxlag - 1):1)
  general_order <- append(order_for_short_run, order_for_ind)
  lag_queue <- rev(expand.grid(general_order))

  if (
    length(s_info$asym_short_vars) > 0 &&
      !spec$args_info$different_asym_lag
  ) {
    i <- 1
    for (x in s_info$independent_vars) {
      i <- i + 1
      if ((x %in% s_info$asym_short_vars)) {
        lag_queue <- cbind(lag_queue[, 1:i], lag_queue[, i:ncol(lag_queue)])
        i <- i + 1
      }
    }
  }
  colnames(lag_queue) <- s_info$short_run_vars
  min_cr <- 1000000
  opt_row <- 0

  for (i in batch$start_row:batch$end_row) {
    my_formula <- as.formula(paste0(
      pre_model$ls_dependent,
      "~",
      paste(
        pre_model$ls_longrun,
        make_shortrun_model(
          s_info$short_run_vars,
          unlist(lag_queue[i, ]),
          s_info$deterministic
        ),
        sep = "+"
      )
    ))
    the_results <- lm(my_formula, s_info$data, ...)
    cr <- model_criterion(the_results, spec$args_info$criterion, ...)
    if (cr < min_cr) {
      min_cr <- cr
      opt_row <- i
    }
  }

  end_lag <- paste(lag_queue[batch$end_row, ], collapse = ",")
  start_lag <- paste(lag_queue[batch$start_row, ], collapse = ",")

  final_lags <- data.frame(c(
    paste(lag_queue[opt_row, ], collapse = ","),
    min_cr
  ))
  if (!is.function(spec$args_info$criterion)) {
    colnames(final_lags) <- spec$args_info$criterion
  }
  rownames(final_lags) <- c("lag", "value")
  min_cr <- NULL

  my_formula <- as.formula(paste0(
    pre_model$ls_dependent,
    "~",
    paste(
      pre_model$ls_longrun,
      make_shortrun_model(
        s_info$short_run_vars,
        unlist(lag_queue[opt_row, ]),
        s_info$deterministic
      ),
      sep = "+"
    )
  ))
  model_data <- s_info$data

  the_results <- lm(
    formula = my_formula,
    data = model_data,
    ...
  )

  fitted_vars <- fitted(the_results)
  proper_lag <- unlist(lag_queue[opt_row, ])
  max_lag <- max(proper_lag)
  end_time <- Sys.time()

  spec$time_info <- list(
    start_time = start_time,
    end_time = end_time,
    span = difftime(end_time, start_time, units = "secs")
  )

  fitted_vars <- fitted(the_results)
  k <- length(the_results$coefficients)
  n <- length(the_results$residuals)
  spec$est_info <- list(
    type = "kardlmodel",
    method = "grid_custom",
    model_formula = my_formula,
    k = k,
    n = n,
    start = as.numeric(names(fitted_vars[1])),
    end = as.numeric(names(tail(fitted_vars, n = 1))),
    time_span = n + max_lag + 1
  )
  spec$lag_info <- list(
    opt_lag = proper_lag,
    all_cr_lags = final_lags,
    proper_row = opt_row,
    lags_from = start_lag,
    lag_to = end_lag
  )

  karamelikli <- lmerge(spec, the_results)
  class(karamelikli) <- c("kardl_lm", "lm")
  karamelikli
}

#' Grid Model Estimation Method
#'
#' This function implements the "grid" estimation method for ARDL or NARDL
#' models. It performs a grid search over possible lag combinations to find the
#' optimal lag structure based on the specified criterion.
#'
#' @param spec A list containing the prepared specifications for model
#'        estimation, including extracted information and arguments.
#' @return An object of class \code{kardl_lm} containing the estimated model.
#' @noRd
#'
#' @export
makemodel.grid <- function(spec, ...) {
  start_time <- Sys.time()
  pre_model <- make_longrun_model(spec)
  batch <- batch_control(spec)

  # for faster access to the extracted information in the loop
  s_info <- spec$extracted_info

  lag_criteria <- matrix(NA, s_info$lag_rows_number, 5)
  colnames(lag_criteria) <- c("lag", "AIC", "BIC", "AICc", "HQ")
  bir <- rep(
    rep(
      c(1:(spec$args_info$maxlag - 1)),
      each = (spec$args_info$maxlag)^(s_info$shortrun_length)
    ),
    time = 1
  )
  for (i in 1:s_info$shortrun_length) {
    r <- rep(
      rep(
        c(0:(spec$args_info$maxlag - 1)),
        each = (spec$args_info$maxlag)^(s_info$shortrun_length - i)
      ),
      time = (spec$args_info$maxlag^i - spec$args_info$maxlag^(i - 1))
    )
    if (
      length(s_info$asym_short_vars) > 0 &&
        (s_info$independent_vars[i] %in% s_info$asym_short_vars)
    ) {
      if (!spec$args_info$different_asym_lag) {
        bir <- cbind(bir, r)
      }
    }
    bir <- cbind(bir, r)
  }
  colnames(bir) <- s_info$short_run_vars
  lag_matrix <- bir[rev(seq_len(nrow(bir))), ]
  for (i in seq_len(nrow(lag_matrix))) {
    lag_criteria[i, 1] <- paste(lag_matrix[i, ], collapse = ",")
  }
  for (i in batch$start_row:batch$end_row) {
    my_formula <- as.formula(paste0(
      pre_model$ls_dependent,
      "~",
      paste(
        pre_model$ls_longrun,
        make_shortrun_model(
          s_info$short_run_vars,
          unlist(lag_matrix[i, ]),
          s_info$deterministic
        ),
        sep = "+"
      )
    ))
    model_data <- s_info$data

    the_results <- lm(
      formula = my_formula,
      data = model_data,
      ...
    )


    k <- length(the_results$coefficients)
    n <- length(the_results$residuals)
    llh <- logLik(the_results)
    aic <- (2 * k - 2 * llh) / n
    bic <- (log(n) * k - 2 * llh) / n
    hq <- (2 * log(log(n)) * k - 2 * llh) / n
    lag_criteria[i, 2] <- aic
    lag_criteria[i, 3] <- bic
    lag_criteria[i, 4] <- aic + ((2 * k * (k + 1)) / (n - k - 1))
    lag_criteria[i, 5] <- hq
    progress_bar(i, batch$end_row, as.character(lag_criteria[i, 1]))
  }
  cat("\n")
  end_lag <- lag_matrix[batch$end_row, ]
  start_lag <- lag_matrix[batch$start_row, ]
  aic_row <- which(
    lag_criteria[, 2] == min(as.numeric(lag_criteria[, 2]), na.rm = TRUE),
    arr.ind = TRUE
  )[1]
  bic_row <- which(
    lag_criteria[, 3] == min(as.numeric(lag_criteria[, 3]), na.rm = TRUE),
    arr.ind = TRUE
  )[1]
  aicc_row <- which(
    lag_criteria[, 4] == min(as.numeric(lag_criteria[, 4]), na.rm = TRUE),
    arr.ind = TRUE
  )[1]
  hq_row <- which(
    lag_criteria[, 5] == min(as.numeric(lag_criteria[, 5]), na.rm = TRUE),
    arr.ind = TRUE
  )[1]
  final_lags <- data.frame(
    "AIC" = c(lag_criteria[aic_row, 1], lag_criteria[aic_row, 2]),
    "BIC" = c(lag_criteria[bic_row, 1], lag_criteria[bic_row, 3]),
    "AICc" = c(lag_criteria[aicc_row, 1], lag_criteria[aicc_row, 4]),
    "HQ" = c(lag_criteria[hq_row, 1], lag_criteria[hq_row, 5])
  )

  rownames(final_lags) <- c("lag", "value")
  proper_row <- switch(spec$args_info$criterion,
    "AIC" = aic_row,
    "BIC" = bic_row,
    "AICc" = aicc_row,
    "HQ" = hq_row
  )
  proper_lag <- lag_matrix[proper_row, ]

  max_lag <- max(proper_lag)
  end_time <- Sys.time()
  spec$time_info <- list(
    start_time = start_time,
    end_time = end_time,
    span = difftime(end_time, start_time, units = "secs")
  )

  fitted_vars <- fitted(the_results)
  spec$est_info <- list(
    type = "kardlmodel",
    method = "grid",
    model_formula = my_formula,
    k = k,
    n = n,
    start = as.numeric(names(fitted_vars[1])),
    end = as.numeric(names(tail(fitted_vars, n = 1))),
    time_span = n + max_lag + 1
  )
  spec$lag_info <- list(
    opt_lag = lag_matrix[proper_row, ],
    all_cr_lags = final_lags,
    proper_row = proper_row,
    criterion = spec$args_info$criterion,
    lags_from = start_lag,
    lags_to = end_lag,
    lag_criteria = as.data.frame(lag_criteria),
    lag_matrix = lag_matrix
  )

  karamelikli <- lmerge(spec, the_results)
  class(karamelikli) <- c("kardl_lm", "lm")
  karamelikli
}

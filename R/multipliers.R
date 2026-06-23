#' Compute Dynamic Multipliers for kardl Models
#'
#' Computes cumulative dynamic multipliers based on a model estimated using the
#' \code{kardl} framework. The function supports different configurations of
#' linearity and asymmetry in both short-run and long-run dynamics.
#'
#' The asymmetry structure is determined internally:
#' \itemize{
#'   \item Variables in \code{extracted_info$asym_short_vars} are treated as
#'   asymmetric in the short run.
#'   \item Variables in \code{extracted_info$asym_long_vars} are treated as
#'   asymmetric in the long run.
#' }
#'
#' This allows four possible configurations:
#' \itemize{
#'   \item \strong{LL}: Linear in both short-run and long-run
#'   \item \strong{NN}: Asymmetric in both short-run and long-run
#'   \item \strong{SA}: Short-run linear, long-run asymmetric
#'   \item \strong{AS}: Short-run asymmetric, long-run linear
#' }
#'
#' When a component is linear, the same coefficient path is used for both
#' positive and negative changes. When asymmetric, separate positive and
#' negative effects are computed.
#'
#' @param kardl_model An object of class \code{kardl_lm} produced by the
#'        \code{kardl} function.
#' @param horizon Integer. Number of periods ahead for which dynamic multipliers
#'        are computed.
#' @param min_prob Numeric. Minimum p-value threshold for including coefficients
#'        in the calculation. Coefficients with p-values above this threshold
#'        will be set to zero. Default is \code{0} (no threshold). This
#'        parameter allows users to control the inclusion of coefficients in
#'        the calculation based on their statistical significance. Setting a
#'        threshold can help focus the analysis on more relevant variables,
#'        but it may also exclude potentially important effects if set too
#'        stringently.
#' @param ... Additional arguments (currently not used).
#'
#' @return A list of class \code{kardl_mplier} containing:
#' \itemize{
#'   \item \strong{mpsi}: Matrix of cumulative dynamic multipliers.
#'   \item \strong{omega}: Vector of omega coefficients (persistence structure).
#'   \item \strong{lambda}: Matrix of short-run dynamic coefficients.
#'   \item \strong{horizon}: Forecast horizon used.
#'   \item \strong{vars}: Extracted model variable structure.
#' }
#'
#' @details
#'
#' The \code{mplier} function computes dynamic multipliers based on the
#' coefficients and lag structure of a model estimated using the \code{kardl}
#' package. The function extracts necessary information from the model, such
#' as coefficients, lag structure, and variable names, to compute the dynamic
#' multipliers. It calculates the short-run coefficients, Lambda values, and
#' omega values based on the model's parameters and lag structure. The output
#' includes a matrix of dynamic multipliers (mpsi), which can be used for
#' further analysis or visualization. The dynamic multipliers provide insight
#' into how changes in the independent variables affect the dependent variable
#' over time, allowing for a deeper understanding of the relationships captured
#' by the model. The function also allows users to set a minimum p-value
#' threshold for including coefficients in the calculation, providing
#' flexibility in focusing on statistically significant effects.
#'
#' The function constructs dynamic multipliers based on the recursive
#' relationship:
#'
#' \deqn{
#' \psi_{h}^{+} = \sum_{i=0}^{h} \frac{\partial y_{t+i}}{\partial x_{t}^{+}},
#' \quad
#' \psi_{h}^{-} = \sum_{i=0}^{h} \frac{\partial y_{t+i}}{\partial x_{t}^{-}}
#' }
#'
#' where \eqn{\psi_h^{+}} and \eqn{\psi_h^{-}} represent cumulative responses to
#' positive and negative shocks.
#'
#' The recursion is defined as:
#'
#' \deqn{
#' \psi_h = \lambda_h + \sum_{j=1}^{p} \omega_j \psi_{h-j}
#' }
#'
#' where \eqn{\lambda_h} captures short-run effects and \eqn{\omega_j} reflects
#' persistence through lagged dependent variables.
#'
#' When asymmetry is present, positive and negative shocks are propagated
#' separately. Otherwise, the same dynamic path is used.
#'
#' @seealso \code{\link{bootstrap}}
#'
#' @import stats msm
#'
#' @srrstats {G1.3} Statistical terms such as "dynamic multipliers" are clearly
#'           defined and used consistently throughout the documentation and
#'           examples.
#' @srrstats {G2.0} The function validates that `kardl_model` is of class
#'           `kardl_lm` before extracting lag and coefficient information.
#' @srrstats {G3.1} Long-run multiplier inference uses the variance-covariance
#'           matrix of the fitted model object.
#' @srrstats {G3.1a} Documentation describes the recursive formula used to
#'           compute cumulative multipliers and the role of the `omega`
#'           persistence structure.
#' @srrstats {TS4.2} The return value documents the `mpsi`, `omega`, `lambda`,
#'           `horizon`, and `vars` components with their meanings.
#' @srrstats {TS4.3} Dynamic multiplier outputs include the horizon index so
#'           that multiplier paths can be interpreted over modelled time steps.
#' @srrstats {TS5.0} A `plot` method is implemented for `kardl_mplier` objects.
#' @srrstats {TS5.7} The plot method displays the computed response path from
#'           the fitted model over the selected horizon.
#' @srrstats {TS5.8} Plot methods distinguish multiplier components by variable
#'           and shock direction where asymmetric effects are present.
#' @export
#'
#' @examples
#'
#' # This example demonstrates how to use the mplier function to calculate
#' # dynamic multipliers from a model estimated using the kardl package.
#' # The example includes fitting a model with the kardl function,
#' # calculating the multipliers, and visualizing the results using both
#' # base R plotting and ggplot2.
#'
#' # Calculating dynamic multipliers for a linear model in short and long run
#' # (NN)
#'
#' kardl_model <- kardl(CPI ~ ER, imf_example_data)
#' m <- mplier(kardl_model, 40)
#' head(m$mpsi)
#' plot(m)
#'
#' # Calculating dynamic multipliers for a model with
#' # Short-run linear, long-run asymmetric (SA)
#' kardl_model <- kardl(CPI ~ lasym(ER), imf_example_data)
#' m <- mplier(kardl_model, horizon = 40, min_prob = 0)
#' head(kardl_extract(m, "multipliers"))
#' plot(m)
#'
#' # Calculating dynamic multipliers for a model with
#' # Short-run asymmetric, long-run linear (AS)
#' kardl_model <- kardl(CPI ~ sasym(ER), imf_example_data)
#' m <- mplier(kardl_model, 40)
#' plot(m)
#'
#' # Calculating dynamic multipliers for a model with
#' # asymmetric effects in both short and long run (NN)
#' kardl_model <- kardl(CPI ~ asym(ER) + PPI, imf_example_data)
#' m <- mplier(kardl_model, 40)
#' plot(m)
#'
#' # The multipliers matrix contains the cumulative dynamic multipliers for each
#' # variable and time horizon. The omega vector contains the persistence
#' # structure of the model, while the lambda matrix contains the short-run
#' # dynamic coefficients. You can inspect these components to understand the
#' # dynamics captured by the model.
#'
#' head(kardl_extract(m, "multipliers"))
#' head(kardl_extract(m, "omega"))
#' head(kardl_extract(m, "lambda"))
#'
#' # For plotting specific variables, you can specify them in the plot
#' # function. For example, to plot the multipliers for the variable "PPI":
#'
#' plot(m, variable = "PPI", title = "Dynamic Multipliers for PPI")
mplier <- function(kardl_model, horizon = 80, min_prob = 0, ...) {
  UseMethod("mplier")
}

#' Method for mplier when the input is not a kardl_lm object
#'
#' This method is called when the `mplier` function is invoked with an object
#' that is not of class `kardl_lm`. It stops execution and provides an
#' informative error message, guiding the user to first estimate a model
#' using the `kardl()` function before attempting to compute dynamic
#' multipliers.
#' @noRd
#' @exportS3Method
mplier.default <- function(kardl_model, horizon = 80, min_prob = 0, ...) {
  stop(
    "mplier() requires a kardl_lm object. ",
    "Please estimate a model using kardl() first.",
    call. = FALSE
  )
}


#' Compute Dynamic Multipliers for kardl Models
#'
#' This method computes cumulative dynamic multipliers for a model estimated
#' using the `kardl` package. It extracts necessary information from the model,
#' such as coefficients, lag structure, and variable names, to compute the
#' dynamic multipliers. The function calculates the short-run coefficients,
#' Lambda values, and omega values based on the model's parameters and lag
#' structure. The output includes a matrix of dynamic multipliers (mpsi),
#' which can be used for further analysis or visualization.
#'
#' @export
#' @noRd
#' @method mplier kardl_lm

mplier.kardl_lm <- function(kardl_model, horizon = 80, min_prob = 0, ...) {
  var_names <- names(coefficients(kardl_model))
  proper_lag <- kardl_model$lag_info$opt_lag
  h <- horizon + 1
  vars <- kardl_model$extracted_info

  depvar <- replace_lag_var(
    .kardl_settings_env$long_coef,
    vars$dependent_var,
    1
  )
  a_index <- which(var_names == depvar)

  gamma_lags <- proper_lag[[vars$dependent_var]]
  gamma_index <- which(
    var_names %in%
      unlist(lapply(1:gamma_lags, function(i) {
        replace_lag_var(.kardl_settings_env$short_coef, vars$dependent_var, i)
      }))
  )

  model_params <- kardl_model$coefficients

  if (min_prob > 0) {
    temp_model <- summary(kardl_model)$coefficients
    for (v in seq_len(nrow(temp_model))) {
      if (temp_model[v, 4] > min_prob) {
        model_params[v] <- 0
      }
    }
  }

  gamma_ <- model_params[gamma_index]
  p <- length(gamma_index)

  omega_ <- numeric(p + 1)
  omega_[1] <- 1 + model_params[a_index] + gamma_[1]
  if (p >= 2) {
    for (i in 2:p) {
      omega_[i] <- gamma_[i] - gamma_[i - 1]
    }
  }
  omega_[p + 1] <- -gamma_[p]

  col_num <- length(vars$independent_vars)

  col_name <- col_name_f <- c()
  for (x in vars$independent_vars) {
    pos_name <- paste0(
      .kardl_settings_env$asym_prefix[1],
      x,
      .kardl_settings_env$asym_suffix[1]
    )
    neg_name <- paste0(
      .kardl_settings_env$asym_prefix[2],
      x,
      .kardl_settings_env$asym_suffix[2]
    )
    diff_name <- paste0(x, "_dif")
    col_name_f <- c(col_name_f, pos_name, neg_name, diff_name)
    col_name <- c(col_name, pos_name, neg_name)
  }

  short_run_coefs <- matrix(0, nrow = h, ncol = col_num * 2)
  lambda_mtx <- matrix(0, nrow = h, ncol = col_num * 2)
  mpsi_mtx <- matrix(0, nrow = h, ncol = col_num * 2)
  colnames(short_run_coefs) <- colnames(lambda_mtx) <-
    colnames(mpsi_mtx) <- col_name

  for (v in seq_len(col_num)) {
    x <- vars$independent_vars[v]

    pos_name <- paste0(
      .kardl_settings_env$asym_prefix[1],
      x,
      .kardl_settings_env$asym_suffix[1]
    )
    neg_name <- paste0(
      .kardl_settings_env$asym_prefix[2],
      x,
      .kardl_settings_env$asym_suffix[2]
    )

    # decide short-run / long-run asymmetry
    sr_asym <- x %in% vars$asym_short_vars
    lr_asym <- x %in% vars$asym_long_vars

    ## names used in long-run part
    lr_pos <- if (lr_asym) {
      pos_name
    } else {
      x
    }
    lr_neg <- if (lr_asym) {
      neg_name
    } else {
      x
    }

    ## names used in short-run part
    sr_pos <- if (sr_asym) {
      pos_name
    } else {
      x
    }
    sr_neg <- if (sr_asym) {
      neg_name
    } else {
      x
    }

    ## long-run coefficient indices
    mtx_index_pos <- which(
      var_names == replace_lag_var(.kardl_settings_env$long_coef, lr_pos, 1)
    )
    mtx_index_neg <- which(
      var_names == replace_lag_var(.kardl_settings_env$long_coef, lr_neg, 1)
    )

    ## short-run lag indices
    mtx_beta_index_p <- which(
      var_names %in%
        unlist(lapply(0:proper_lag[[sr_pos]], function(i) {
          replace_lag_var(.kardl_settings_env$short_coef, sr_pos, i)
        }))
    )
    mtx_beta_index_n <- which(
      var_names %in%
        unlist(lapply(0:proper_lag[[sr_neg]], function(i) {
          replace_lag_var(.kardl_settings_env$short_coef, sr_neg, i)
        }))
    )

    mtx_beta_pos <- model_params[mtx_beta_index_p]
    mtx_beta_neg <- model_params[mtx_beta_index_n]

    temp_col_no <- (v - 1) * 2 + 1

    short_run_coefs[seq_along(mtx_beta_pos), temp_col_no] <- mtx_beta_pos
    short_run_coefs[seq_along(mtx_beta_neg), temp_col_no + 1] <- mtx_beta_neg

    lambda_mtx[1, temp_col_no] <- short_run_coefs[1, temp_col_no]
    lambda_mtx[1, temp_col_no + 1] <- short_run_coefs[1, temp_col_no + 1]

    lambda_mtx[2, temp_col_no] <-
      model_params[mtx_index_pos] -
      short_run_coefs[1, temp_col_no] +
      short_run_coefs[2, temp_col_no]

    lambda_mtx[2, temp_col_no + 1] <-
      model_params[mtx_index_neg] -
      short_run_coefs[1, temp_col_no + 1] +
      short_run_coefs[2, temp_col_no + 1]

    my_beta_lags_p <- proper_lag[[sr_pos]]
    if (my_beta_lags_p > 1) {
      for (i in 2:length(mtx_beta_pos)) {
        lambda_mtx[i + 1, temp_col_no] <- short_run_coefs[i + 1, temp_col_no] -
          short_run_coefs[i, temp_col_no]
      }
    }
    if (my_beta_lags_p > 0) {
      lambda_mtx[my_beta_lags_p + 2, temp_col_no] <-
        -short_run_coefs[my_beta_lags_p + 1, temp_col_no]
    }

    ## if short-run is linear, copy lambda from POS to NEG
    if (!sr_asym) {
      lambda_mtx[, temp_col_no + 1] <- lambda_mtx[, temp_col_no]
    } else {
      my_beta_lags_n <- proper_lag[[sr_neg]]
      if (my_beta_lags_n > 1) {
        for (i in 2:length(mtx_beta_neg)) {
          lambda_mtx[i + 1, temp_col_no + 1] <-
            short_run_coefs[i + 1, temp_col_no + 1] -
            short_run_coefs[i, temp_col_no + 1]
        }
      }
      if (my_beta_lags_n > 0) {
        lambda_mtx[my_beta_lags_n + 2, temp_col_no + 1] <-
          -short_run_coefs[my_beta_lags_n + 1, temp_col_no + 1]
      }
    }

    ## if long-run is linear, force same long-run effect in lambda[2]
    if (!lr_asym) {
      lambda_mtx[2, temp_col_no + 1] <- lambda_mtx[2, temp_col_no]
    }
  }

  mpsi_mtx[1, ] <- lambda_mtx[1, ]
  p2 <- p + 1

  for (v in 2:h) {
    n_say <- ifelse(v > p2, p2, v - 1)
    g_say <- ifelse(v - p2 <= 0, 1, v - p2)
    n_omega <- omega_[1:n_say]
    mpsi_mtx[v, ] <- lambda_mtx[v, ] + n_omega %*% mpsi_mtx[(v - 1):g_say, ]
  }

  mpsi_mtx2 <- apply(mpsi_mtx, 2, cumsum)

  mpsi <- matrix(0, nrow = h, ncol = col_num * 3)
  for (v in seq_len(col_num)) {
    col_no_2 <- v * 2 - 1
    col_no_3 <- v * 3 - 2
    mpsi[, col_no_3] <- mpsi_mtx2[, col_no_2]
    mpsi[, col_no_3 + 1] <- -mpsi_mtx2[, col_no_2 + 1]
    mpsi[, col_no_3 + 2] <- mpsi_mtx2[, col_no_2] - mpsi_mtx2[, col_no_2 + 1]
  }

  mpsi <- cbind(0:horizon, mpsi)
  colnames(mpsi) <- c("h", col_name_f)

  structure(
    list(
      call = match.call(),
      mpsi = mpsi,
      omega = omega_,
      lambda = lambda_mtx,
      horizon = horizon,
      vars = vars
    ),
    class = "kardl_mplier"
  )
}

#' Bootstrap Confidence Intervals for Dynamic Multipliers
#'
#' This function computes bootstrap confidence intervals (CI) for dynamic
#' multipliers of a specified variable in a model estimated using the
#' \code{kardl} package. The bootstrap method generates resampled datasets to
#' estimate the variability of the dynamic multipliers, providing upper and
#' lower bounds for the confidence interval.
#'
#' @param kardl_model The model produced by the \code{\link{kardl}} function.
#'        This is the model object from which the dynamic multipliers are
#'        calculated.
#' @param horizon An integer specifying the horizon over which dynamic
#'        multipliers will be computed. The horizon defines the time frame for
#'        the analysis (e.g., 40 periods).
#' @param replications An integer indicating the number of bootstrap
#'        replications to perform. Higher values increase accuracy but also
#'        computational time. Default is \code{100}.
#' @param level A numeric value specifying the confidence level for the
#'        intervals (e.g., 95 for 95% confidence). Default is \code{90}.
#' @param min_prob A numeric value specifying the minimum p-value threshold for
#'        including coefficients in the bootstrap. Coefficients with p-values
#'        above this threshold will be set to zero in the bootstrap samples.
#'        Default is \code{0} (no threshold). This parameter allows users to
#'        control the inclusion of coefficients in the bootstrap process based
#'        on their statistical significance. Setting a threshold can help focus
#'        the analysis on more relevant variables, but it may also exclude
#'        potentially important effects if set too stringently.
#' @param seed An optional integer to set the random seed for reproducibility
#'        of the bootstrap results. If not provided, the bootstrap will use
#'        the current random state.
#' @param ... Additional arguments (currently not used).
#'
#' @seealso \code{\link{mplier}} for calculating dynamic multipliers
#'
#' @return A list containing the following elements:
#' \itemize{
#' \item \strong{mpsi}: A data frame containing the dynamic multiplier
#'       estimates along with their upper and lower confidence intervals for
#'       each variable and time horizon.
#' \item \strong{level}: The confidence level used for the intervals (e.g 95).
#' \item \strong{horizon}: The horizon over which the multipliers were computed
#'       (e.g., 40).
#' \item \strong{vars}: A list of variable information extracted from the
#'       model, including dependent variable, independent variables,
#'       asymmetric variables, and deterministic terms.
#' \item \strong{replications}: The number of bootstrap replications performed.
#' \item \strong{type}: A character string indicating the type of analysis,
#'       in this case "bootstrap".
#' }
#'
#' @details
#'
#' The \code{mpsi} component of the output contains the dynamic multiplier
#' estimates along with their upper and lower confidence intervals. These
#' values are provided for each variable and at each time horizon.
#'
#' @srrstats {G5.5} Tests involving random resampling in the bootstrap set a
#'           random seed before the replication loop.
#' @srrstats {G5.6} Parameter-recovery properties of the bootstrap are assessed
#'           through deterministic examples in the test suite.
#' @srrstats {G3.1} Bootstrap standard errors for dynamic multipliers are
#'           derived from the covariance structure of the fitted model's
#'           residuals.
#' @srrstats {TS4.2} The bootstrap return value documents the `mpsi`, `level`,
#'           `horizon`, `vars`, and `replications` components.
#' @srrstats {G5.9} The documentation includes examples demonstrating the use
#'           of the bootstrap function, including how to specify the number of
#'           replications and how to interpret the output.
#'
#' @import ggplot2
#' @export
#'
#' @examples
#'
#' # Example usage of the bootstrap function
#'
#' # Fit a model using kardl
#' kardl_model <- kardl(
#'   CPI ~ ER + PPI + asy(ER) + det(covid) + trend,
#'   imf_example_data,
#'   mode = c(1, 2, 3, 0)
#' )
#'
#' # Perform bootstrap with specific variables for plotting
#' boot <- bootstrap(kardl_model,
#'   horizon = 40, level = 95, min_prob = 0,
#'   replications = 5, seed = 123L
#' )
#' # The boot object will include all plots for the specified variables
#' # Displaying the boot object provides an overview of its components
#' names(boot)
#'
#' # Inspect the first few rows of the dynamic multiplier estimates
#' head(kardl_extract(boot, "multipliers"))
#'
#' summary(boot)
#'
#' # Retrieve plots generated during the bootstrap process
#' # Accessing all plots
#' plot(boot)
#'
#' # Accessing the plot for a specific variable by its name
#' plot(boot, variable = "PPI")
#' plot(boot, variable = "ER")
#'
#' @examplesIf requireNamespace("magrittr", quietly = TRUE)
#' library(magrittr)
#'
#' imf_example_data %>%
#'   kardl(CPI ~ PPI + asym(ER) + trend, maxlag = 2, data = .) %>%
#'   bootstrap(replications = 5) %>%
#'   plot(variable = "ER")
bootstrap <- function(kardl_model,
                      horizon = 80,
                      replications = 100,
                      level = 95,
                      min_prob = 0,
                      seed = NULL,
                      ...) {
  UseMethod("bootstrap")
}

#' Method for bootstrap when the input is not a kardl_lm object
#'
#' This method is called when the `bootstrap` function is invoked with an object
#' that is not of class `kardl_lm`. It stops execution and provides an
#' informative error message, guiding the user to first estimate a model
#' using the `kardl()` function before attempting to compute bootstrap
#' confidence  intervals for dynamic multipliers.
#'
#' @noRd
#' @exportS3Method
bootstrap.default <- function(kardl_model,
                              horizon = 80,
                              replications = 100,
                              level = 95,
                              min_prob = 0,
                              seed = NULL,
                              ...) {
  stop(
    "bootstrap() requires a kardl_lm object. ",
    "Please estimate a model using kardl() first.",
    call. = FALSE
  )
}


#' Bootstrap Confidence Intervals for Dynamic Multipliers of kardl Models
#'
#' This method computes bootstrap confidence intervals for dynamic multipliers
#' of a model estimated using the `kardl` package. It generates resampled
#' datasets to estimate the variability of the dynamic multipliers, providing
#' upper and lower bounds for the confidence interval.
#'
#' @export
#' @noRd
#' @method bootstrap kardl_lm

bootstrap.kardl_lm <- function(
  kardl_model,
  horizon = 80,
  replications = 100,
  level = 95,
  min_prob = 0,
  seed = NULL,
  ...
) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  my_mp <- mplier(kardl_model, horizon, min_prob)
  mpsi <- as.data.frame(my_mp[["mpsi"]])
  vars <- kardl_model$extracted_info

  if (length(vars$asym_short_vars) > 0) {
    b0 <- coef(kardl_model)
    t_min <- kardl_model$est_info$start
    t_max <- kardl_model$est_info$end
    opt_lag <- kardl_model$lag_info$opt_lag
    nardl_res <- residuals(kardl_model)
    new_diff <- matrix(NA, nrow = 1, ncol = length(vars$asym_short_vars) + 1)
    colnames(new_diff) <- c("h", vars$asym_short_vars)
    phi <- my_mp$omega
    phi_length <- length(phi)

    q <- max(opt_lag[names(opt_lag) != vars$dependent_var]) + 2
    theta <- my_mp$lambda[1:q, ]

    # excluding symmetric independent vars
    sym_pos_names <- unlist(lapply(vars$indep_as_excluded, function(x) {
      paste0(
        .kardl_settings_env$asym_prefix[1],
        x,
        .kardl_settings_env$asym_suffix[1]
      )
    }))
    sym_neg_names <- unlist(lapply(vars$indep_as_excluded, function(x) {
      paste0(
        .kardl_settings_env$asym_prefix[2],
        x,
        .kardl_settings_env$asym_suffix[2]
      )
    }))

    if (!is.null(sym_pos_names)) {
      theta <- theta[, colnames(theta) != sym_pos_names]
      colnames(theta)[colnames(theta) %in% sym_neg_names] <-
        vars$indep_as_excluded
    }

    ## names of the indep. variables that matter for multipliers
    indd_colnames <- colnames(theta)

    ## build the column list; add deterministic terms only if they exist
    req_cols <- c(
      vars$dependent_var,
      indd_colnames,
      if (!is.null(vars$deterministic)) vars$deterministic
    )

    vars$data <- as.data.frame(vars$data)

    ## --- NEW: make sure every requested column exists --------------------
    missing_cols <- setdiff(req_cols, colnames(vars$data))
    if (length(missing_cols)) {
      vars$data[, missing_cols] <- 0 # fill with zeros
    }
    ## --------------------------------------------------------------------

    ## subset the data safely
    my_new_data <- as.data.frame(vars$data)[, req_cols, drop = FALSE]

    # Addin dif suffix to asymmetric short-run var names

    # Replace leading NA values with 0 for each column
    for (col in colnames(my_new_data)) {
      # Find the index of first non-NA value
      first_non_na_index <- which(!is.na(my_new_data[, col]))[1]
      # Check if non-NA value exists
      if (!is.na(first_non_na_index) && first_non_na_index > 1) {
        # Replace leading NA values with 0
        my_new_data[1:(first_non_na_index - 1), col] <- 0
      }
    }

    new_formula <- kardl_model$args_info$formula
    new_dep_data <- my_new_data[, vars$dependent_var]

    for (r in 1:replications) {
      for (t in t_min:t_max) {
        # Safe indexing for dep_data lags
        idx_y <- (t - 1):(t - phi_length)
        valid_y <- idx_y > 0
        new_y1 <- sum(phi[valid_y] * new_dep_data[idx_y[valid_y]])

        # Safe indexing for indep_data lags
        idx_x <- (t - 0):(t - q + 1)
        valid_x <- idx_x > 0
        new_y2 <- sum(
          theta[valid_x, , drop = FALSE] *
            my_new_data[idx_x[valid_x], indd_colnames, drop = FALSE]
        )

        new_y3 <- sum(
          my_new_data[t, vars$deterministic] *
            b0[vars$deterministic]
        )
        new_y <- new_y1 + new_y2 + new_y3 + b0[[1]] # adding constant
        new_dep_data[t] <- new_y +
          nardl_res[sample(seq_len(NROW(nardl_res)), 1)]
      }
      if (r == 1) {
        bs_new_data <- cbind(new_dep_data, vars$data) # adding to original data
        colnames(bs_new_data) <- c("bs_tmp", colnames(vars$data))
        temp_formula <- bs_tmp ~ x + f_f
        new_formula[[2]] <- temp_formula[[2]]
        names(opt_lag)[1] <- "bs_tmp"
        opt_lag <- as.double(opt_lag)
      } else {
        bs_new_data[, "bs_tmp"] <- new_dep_data
      }

      my_model <- kardl(new_formula, bs_new_data, mode = opt_lag)
      my_mp_new <- mplier(my_model, horizon, min_prob)
      temp_val <- my_mp_new$mpsi[, paste0(vars$asym_short_vars, "_dif")]
      new_diff <- rbind(new_diff, cbind(horizon = 0:horizon, val = temp_val))
    }

    new_diff <- as.data.frame(new_diff[-1, ])
    rownames(new_diff) <- seq_len(nrow(new_diff))

    lower_z <- 0.5 * (1 - level / 100)
    r_z <- replications * lower_z
    lower_index_1 <- ceiling(r_z)
    lower_index_2 <- ifelse(lower_index_1 - r_z != 0, lower_index_1, r_z + 1)

    median_z <- 0.5
    r_z <- replications * median_z
    median_index_1 <- ceiling(r_z)
    median_index_2 <- ifelse(median_index_1 - r_z != 0, median_index_1, r_z + 1)

    upper_z <- 1 - lower_z
    r_z <- replications * upper_z
    upper_index_1 <- ceiling(r_z)
    upper_index_2 <- ifelse(upper_index_1 - r_z != 0, upper_index_1, r_z + 1)

    for (variable in vars$asym_short_vars) {
      lower_ci <- median_ci <- upper_ci <- c()
      for (jh in 0:horizon) {
        nardlbs_sub <- new_diff[new_diff$h == jh, ]
        nardlbs_sub <- nardlbs_sub[order(nardlbs_sub[, variable]), variable]
        lower_ci[jh + 1] <-
          (nardlbs_sub[lower_index_1] + nardlbs_sub[lower_index_2]) / 2
        median_ci[jh + 1] <-
          (nardlbs_sub[median_index_1] + nardlbs_sub[median_index_2]) / 2
        upper_ci[jh + 1] <-
          (nardlbs_sub[upper_index_1] + nardlbs_sub[upper_index_2]) / 2
      }
      dyn_names <- colnames(mpsi)
      mpsi <- cbind(mpsi, upper_ci, lower_ci)
      colnames(mpsi) <- c(
        dyn_names,
        paste0(variable, c("_CI_upper", "_CI_lower"))
      )
    }
  }

  my_output <- list(
    mpsi = mpsi,
    level = level,
    horizon = horizon,
    vars = vars,
    replications = replications,
    type = "bootstrap"
  )
  class(my_output) <- c("kardl_boot", "kardl_mplier")
  my_output
}

#' Plot Dynamic Multipliers with ggplot2
#'
#' This function creates a ggplot visualization of the dynamic multipliers for
#' a specified variable from a model estimated using the \code{kardl} package.
#' The plot includes lines for positive and negative changes, as well as
#' confidence intervals if available. The function allows for customization of
#' colors, line types, and legend formatting to enhance the interpretability
#' of the results.
#'
#' @param mpsi A data frame containing the dynamic multipliers and confidence
#'        intervals for the specified variable. This data frame is typically
#'        generated by the \code{bootstrap} function.
#' @param vars A list of variable information extracted from the model,
#'        including dependent variable, independent variables, asymmetric
#'        variables, and deterministic terms. This information is used to
#'        label the plot appropriately.
#' @param var_name A character string specifying the name of the variable for
#'        which the dynamic multipliers are being plotted. This variable should
#'        be one of the independent variables in the model, and it is used to
#'        construct the plot labels and legend.
#'
#' @return A ggplot object visualizing the dynamic multipliers for the
#'         specified variable, including lines for positive and negative
#'         changes, as well as confidence intervals if available. The plot is
#'         formatted with appropriate labels, colors, and legend to enhance
#'         interpretability.
#'
#' @import ggplot2
#' @noRd

mplierggplot <- function(mpsi, vars, var_name, title = NULL) {
  x_p <- paste0(
    .kardl_settings_env$asym_prefix[1],
    var_name,
    .kardl_settings_env$asym_suffix[1]
  )
  x_n <- paste0(
    .kardl_settings_env$asym_prefix[2],
    var_name,
    .kardl_settings_env$asym_suffix[2]
  )

  x_diff <- paste0(var_name, "_dif")
  upper_ci <- paste0(var_name, "_CI_upper")
  lower_ci <- paste0(var_name, "_CI_lower")

  p <- ggplot(mpsi, aes(x = .data$h)) +
    geom_line(aes(y = .data[[x_p]], color = "Positive")) +
    geom_line(aes(y = .data[[x_n]], color = "Negative"))

  my_breaks <- c("Difference", "Positive", "Negative")
  labels <- c(
    "Difference" = "Difference",
    "Positive" = "Positive Change",
    "Negative" = "Negative Change"
  )
  values <- c(
    # Specify colors for each legend item
    "Difference" = alpha("black", 1),
    "Positive" = alpha("blue", 1),
    "Negative" = alpha("red", 1)
  )
  color <- c("black", "blue", "red")
  fill <- c("transparent", "transparent", "transparent")
  linetype <- c(2, 1, 1)
  size <- c(8, 8, 8) # Increases the width of the legend key

  if (upper_ci %in% colnames(mpsi)) {
    # !is.null(mpsi[[upper_ci]])){

    p <- p +
      geom_ribbon(
        aes(
          ymin = .data[[lower_ci]],
          ymax = .data[[upper_ci]],
          color = "CI"
        ),
        fill = "blue",
        alpha = 0.2
      )

    my_breaks <- c(my_breaks, "CI")
    labels["Difference"] <- "Asymmetry"
    labels <- c(labels, "CI" = "Confidence Interval")
    values <- c(values, "CI" = alpha("blue", 0.2))
    color <- c(color, "transparent")
    fill <- c(fill, "blue")
    linetype <- c(linetype, 0)
    size <- c(size, 8)
  }
  if (is.null(title)) {
    title <- paste0(
      "Cumulative effect of ",
      var_name,
      " on ",
      vars$dependent_var
    )
  }

  p <- p +
    geom_line(
      aes(y = .data[[x_diff]], color = "Difference"),
      linetype = "dashed"
    ) +
    scale_colour_manual(
      name = "",
      breaks = my_breaks,
      labels = labels,
      values = values
    ) +
    scale_x_continuous(limits = c(0, nrow(mpsi) - 1), expand = c(0, 0)) +
    labs(x = "time", y = "") +
    ggtitle(title) +
    guides(
      color = guide_legend(
        override.aes = list(
          color = color,
          fill = fill,
          linetype = linetype,
          size = size
        )
      )
    ) +
    theme_bw() +
    theme(
      legend.key.width = unit(0, "cm"),
      legend.position = "bottom",
      legend.margin = margin(t = 0, unit = "cm")
    )

  p
}

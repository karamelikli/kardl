#' Prepare the model specification for estimation
#'
#' This function prepares the model specification for estimation by combining
#' variable types, checking user inputs, detecting variables, and creating new
#' variables based on the model formula and data. It ensures that the inputs are
#'  defined properly, checks if the data is a time series, and updates the model
#'  specification with the necessary information for estimation.
#' @param inputs A list of user inputs, including the model formula and data.
#' @return The prepared model specification ready for estimation. If the inputs
#' are not defined properly or if the data is not a time series, an error is
#' raised with a descriptive message.
#' @noRd

prepare <- function(inputs) {
  spec <- parse_model_specs(inputs)
  if (is.null(spec)) {
    stop(
      "The inputs are not defined properly. Please check the inputs again.",
      call. = FALSE
    )
  }
  ts_info <- function(x) {
    list(
      data_is_ts = is.ts(x),
      data_class = class(x),
      data_start = if (is.ts(x)) start(x) else NULL,
      data_end = if (is.ts(x)) end(x) else NULL,
      data_frequency = if (is.ts(x)) frequency(x) else NULL,
      data_deltat = if (is.ts(x)) deltat(x) else NULL,
      data_tsp = if (is.ts(x)) tsp(x) else NULL,
      data_time = if (is.ts(x)) time(x) else NULL
    )
  }
  spec$args_info$data_ts_info <- ts_info(spec$args_info$data)
  if (!is.ts(spec$args_info$data)) {
    temp_names <- colnames(spec$args_info$data)

    spec$args_info$data <- stats::ts(
      data.matrix(spec$args_info$data),
      start = 1,
      frequency = 1
    )

    colnames(spec$args_info$data) <- temp_names
    warning(
      "The data is not a time series. It has been converted to a time series ",
      "with start = 1 and frequency = 1.",
      call. = FALSE
    )
  }


  spec$args_info$data <- spec$args_info$data[, c(
    spec$extracted_info$all_vars[spec$extracted_info$all_vars != "trend"],
    spec$extracted_info$deterministic
  )]
  spec <- check_inputs(spec)
  spec <- detect_vars(spec)
  spec <- create_new_vars(spec)
  spec
}

#' Parse model specifications from user inputs
#'
#' Extracts and organizes information from model formulas including asymmetric
#' terms, deterministic components, and variable relationships.
#'
#' @param inputs A list containing at minimum a `formula` element, and
#' optionally a `data` element when using dot notation.
#'
#' @return A list containing three components:
#'   \item{args_info}{The original inputs list}
#'   \item{settings}{Package settings retrieved via \code{kardl_get()}}
#'   \item{extracted_info}{Processed model information including variables,
#'         constant terms, and asymmetric specifications}
#'
#' @srrstats {G2.0} Implements assertions on formula presence
#' @srrstats {G2.0a} Implements assertions on formula type
#' @srrstats {G2.1} Validates formula is language type
#' @srrstats {G2.1a} Validates formula is not character string
#' @srrstats {G2.3a} Uses match.arg() for validating asymmetric types
#' @srrstats {G2.13} Implements missing data checks through formula validation
#' @srrstats {G2.14} Handles missing formula with explicit error message
#'
#' @section Developer Note:
#' \strong{This function is not intended for general users of the kardl
#' package.} It is designed as a low-level utility for developers who are
#' building extensions or using \code{kardl} as a dependency in their own
#' software. General users should interact with the high-level modeling
#' functions provided by the package.
#'
#' @details
#' For package developers: This function is a crucial part of the model
#' preparation workflow. It ensures that the model formula is correctly
#' specified and that all variables are properly identified and categorized.
#' The extracted information is then used in subsequent steps of the estimation
#' process, such as constructing lagged variables and fitting the model.
#'
#' The function includes error handling to provide informative messages if the
#' model formula is missing or incorrectly specified. It first validates the
#' formula type, parses it to extract variable roles, and verifies existence
#' in the provided data.
#'
#' The function also handles the special case of the dot (.) in the formula,
#' which indicates that all variables in the data (except the dependent variable
#'  and deterministic variables) should be included as independent variables.
#'
#' @examples
#' # Example of using parse_model_specs in a development context
#' inputs <- list(
#'   formula = y ~ x + sasymmetric(z) + deterministic(w)
#' )
#' result <- parse_model_specs(inputs)
#' result
#' @export
parse_model_specs <- function(inputs) {
  if (is.null(inputs$formula)) {
    stop(
      "The model is missing! Please define the model like as: model=y~x+z",
      call. = FALSE
    )
  }
  if (typeof(inputs$formula) != "language") {
    stop(
      "The model should be a valid model without any \" or '. For y~x+z ",
      call. = FALSE
    )
  }
  extracted_info <- list()
  parse_formula <- parse_formula_vars(inputs$formula)
  extracted_info[["no_constant"]] <- !parse_formula$intercept
  extracted_info[["trend"]] <- "trend" %in% parse_formula$outside

  choices <- c("asymmetric", "sasymmetric", "lasymmetric", "deterministic")
  names(parse_formula$inside) <- tolower(names(parse_formula$inside))
  matched <- vapply(
    names(parse_formula$inside),
    match.arg,
    character(1),
    choices = choices
  )
  all_of_insides <- list()
  inside_names <- names(matched)
  for (variable in inside_names) {
    all_of_insides[[matched[[variable]]]] <- parse_formula$inside[[variable]]
  }
  extracted_info$asym_long_vars <- unique(
    trimws(c(all_of_insides$lasymmetric, all_of_insides$asymmetric))
  )
  extracted_info$asym_short_vars <- unique(
    trimws(c(all_of_insides$sasymmetric, all_of_insides$asymmetric))
  )
  extracted_info$deterministic <- unique(trimws(all_of_insides$deterministic))

  extracted_info$dependent_var <- parse_formula$response
  if (parse_formula$dot) {
    complete_vars <- colnames(inputs$data)
    complete_vars <- complete_vars[!complete_vars %in% c("trend")]
    extracted_info$independent_vars <- setdiff(
      unique(complete_vars),
      c(extracted_info$dependent_var, extracted_info$deterministic)
    )
  } else {
    extracted_info$independent_vars <- setdiff(
      unique(c(
        parse_formula$outside,
        unlist(parse_formula$inside, use.names = FALSE)
      )),
      c("trend", extracted_info$deterministic)
    )
  }

  extracted_info$all_vars <- c(
    extracted_info$dependent_var,
    extracted_info$independent_vars
  )

  attr(extracted_info, "source") <- "extracted_info"
  attr(extracted_info, "description") <-
    "This value was obtained from user inputs."

  list(
    args_info = inputs,
    settings = kardl_get(c(
      "asym_prefix",
      "asym_suffix",
      "long_coef",
      "short_coef"
    )),
    extracted_info = extracted_info
  )
}

#' Verify user-defined lags for the model specification
#'
#' This function checks the user-defined lags provided in the model
#' specification. It ensures that the lags are valid numeric values,
#' non-decimal, and match the number of variables in the model. It also handles
#' the naming of the lags based on the variable names and updates the maximum
#' lag value in the specification.
#'
#' @param spec A list containing the model specification, including user-defined
#'        lags and variable information.
#'
#' @return The updated model specification with verified user-defined lags and
#'         maximum lag value.
#'
#' @srrstats {G2.0} The function checks for the validity of user-defined lags,
#'           ensuring that they are numeric, non-decimal, and match the number
#'           of variables in the model. It also provides descriptive error
#'           messages if any of the checks fail, guiding the user to correct
#'           their input.
#' @srrstats {G2.4a} Integer-valued lag counts are enforced; non-integer values
#'           produce an informative error before any lagged regressors are
#'           constructed.
#' @srrstats {G5.2a} Diagnostic messages identify the specific invalid lag
#'           vector encountered by the user.
#'
#' @noRd

verify_user_defined_lags <- function(spec) {
  if (typeof(spec$args_info$mode) != "double") {
    stop(
      "User-defined value should have valid vector.",
      call. = FALSE
    )
  }
  if (!all(spec$args_info$mode == floor(spec$args_info$mode))) {
    stop(
      "User-defined should have valid numeric and non-decimal.",
      call. = FALSE
    )
  }

  nlist <- c()
  j <- 0
  for (i in seq_along(spec$extracted_info$all_vars)) {
    if (
      spec$extracted_info$all_vars[i] %in% spec$extracted_info$asym_short_vars
    ) {
      nlist[i + j] <- paste0(
        .kardl_settings_env$asym_prefix[1],
        spec$extracted_info$all_vars[i],
        .kardl_settings_env$asym_suffix[1]
      )
      j <- j + 1
      nlist[i + j] <- paste0(
        .kardl_settings_env$asym_prefix[2],
        spec$extracted_info$all_vars[i],
        .kardl_settings_env$asym_suffix[2]
      )
    } else {
      nlist[i + j] <- spec$extracted_info$all_vars[i]
    }
  }
  if (
    length(spec$args_info$mode) !=
      length(spec$extracted_info$all_vars) +
        length(spec$extracted_info$asym_short_vars)
  ) {
    stop(
      "User-defined should match with short-run variables.",
      call. = FALSE
    )
  }

  if (!is.null(names(spec$args_info$mode))) {
    yenisi <- c()
    for (i in seq_along(nlist)) {
      yenisi[nlist[i]] <- spec$args_info$mode[nlist[i]]
    }
    spec$args_info$mode <- yenisi
  } else {
    names(spec$args_info$mode) <- nlist
  }

  if (spec$args_info$mode[1] < 1) {
    stop(
      "User-defined should start with a digit greater than zero.",
      call. = FALSE
    )
  }
  if (!all(spec$args_info$mode >= 0)) {
    stop(
      "User-defined should containt only positive values.",
      call. = FALSE
    )
  }

  spec$args_info$maxlag <- max(spec$args_info$mode)

  spec
}

#' Check the validity of user inputs for the model specification
#'
#' This function performs various checks on the user inputs for the model
#' specification. It ensures that the model formula is defined properly, the
#' data is provided, the criterion is valid, and other parameters are of the
#' correct type and format. If any of the checks fail, it raises an error with
#' a descriptive message.
#'
#' @param spec A list containing the model specification, including the model
#'        formula, data, criterion, and other parameters.
#'
#' @return The original model specification if all checks pass. If any check
#'         fails, an error is raised with a descriptive message.
#'
#' @srrstats {G2.0} The function checks for the presence and validity of the
#'           model formula, data, criterion, and other parameters. It ensures
#'           that the model formula is defined properly as a language object,
#'           the data is provided, the criterion is either a function or one
#'           of the predefined criteria, and that other parameters are of the
#'           correct type (e.g., logical values for certain parameters). If
#'           any of these checks fail, it raises an error with a descriptive
#'           message to guide the user in correcting their input.
#' @srrstats {G2.1} Validates that `formula` is a language object and `data`
#'           is not NULL before extraction.
#' @srrstats {G2.3} The `criterion` argument is validated against the set of
#'           accepted string values `c("AIC","BIC","AICc","HQ","AdjR2")`; custom
#'           functions are also accepted.
#' @srrstats {G2.3a} Selection argument `criterion` is matched against
#'           predefined acceptable values.
#' @srrstats {G5.2a} Error messages identify the specific invalid input
#'           (e.g., wrong criterion, non-logical flag, malformed batch string).
#' @srrstats {G5.8a} Zero-length or malformed model inputs are rejected before
#'           estimation.
#'
#' @noRd

check_inputs <- function(spec) {
  if (is.null(spec$args_info$formula)) {
    stop(
      "model is missing! Please define the model like as: model=y~x+z",
      call. = FALSE
    )
  }
  if (typeof(spec$args_info$formula) != "language") {
    stop("The model is not defined properly.", call. = FALSE)
  }
  if (is.null(spec$args_info$data)) {
    stop(
      "The data is missing! Please select related data. data=data",
      call. = FALSE
    )
  }
  if (length(spec$extracted_info$all_vars) == 0) {
    stop(
      "No variable found in the model! Please check the model again.",
      call. = FALSE
    )
  }
  for (x in spec$extracted_info[["all_vars"]]) {
    if (!(x %in% colnames(spec$args_info$data))) {
      stop(
        "The variable: ",
        x,
        " not found in the data file's vars list!",
        call. = FALSE
      )
    }
  }
  if (!is.function(spec$args_info$criterion)) {
    cr <- tolower(spec$args_info$criterion)
    my_cr <- tryCatch(
      match.arg(cr, c("aic", "bic", "aicc", "hq", "adjr2")),
      error = function(e) {
        stop("The entered criterion should be a function or one of the defined",
          " criteria here. The defined criteria are AIC, BIC, AICc, HQ, AdjR2.",
          call. = FALSE
        )
      }
    )
    spec$args_info$criterion <- switch(my_cr,
      "aic" = "AIC",
      "bic" = "BIC",
      "aicc" = "AICc",
      "hq" = "HQ",
      "adjr2" = "AdjR2"
    )
  }
  if (typeof(spec$args_info$different_asym_lag) != "logical") {
    stop(
      "The entered DifferentAsymLag should be a logical value. TRUE/FALSE .",
      call. = FALSE
    )
  }
  if (!all(spec$args_info$mode %in% c("grid_custom", "grid", "quick"))) {
    spec <- verify_user_defined_lags(spec)
  }
  if (
    !is.null(spec$args_info$batch) &&
      !grepl("^\\d+/\\d+$", spec$args_info$batch)
  ) {
    stop(
      "Invalid batch format. Use 'x/y', where x is the batch number ",
      "and y is the total number of batches.",
      call. = FALSE
    )
  }
  if (!grepl("^([1-9])[0-9]*$", spec$args_info$maxlag, perl = TRUE)) {
    stop(
      "The entered maxlag should be a digit. You entered: ",
      spec$args_info$maxlag,
      call. = FALSE
    )
  }

  spec
}

#' Detect and prepare variables for the model specification
#'
#' This function detects the variables specified in the model formula, checks
#' their existence in the data, and prepares lists of short-run and long-run
#' variables based on the asymmetric variable specifications. It also determines
#'  the model type (LL, AS, SA, NN) based on the presence of asymmetric
#'  variables and calculates the number of lag rows needed for the model
#'  estimation.
#' @param spec A list containing the model specification, including extracted
#' information about variables and user inputs.
#' @return The updated model specification with detected variables, prepared
#' variable lists, and model type information. If any variable is not found in
#' the data, an error is raised with a descriptive message.
#' @srrstats {G2.10} Variables are selected by name from the supplied data
#' through formula parsing rather than by position-dependent assumptions.
#' @srrstats {G2.11} Standard data-frame inputs are processed by checking all
#' formula-referenced columns against `colnames(data)`.
#' @srrstats {G5.8a} Variables not found in the data trigger descriptive errors
#' before any estimation step.
#'
#'
#' @noRd
#'
detect_vars <- function(spec) {
  all_asym_vars <- unique(
    c(spec$extracted_info$asym_long_vars, spec$extracted_info$asym_short_vars)
  )
  if (length(spec$extracted_info$asym_short_vars) > 0) {
    for (x in spec$extracted_info$asym_short_vars) {
      if (!(x %in% spec$extracted_info$independent_vars)) {
        stop(
          "Attention! The Short-run asymmetric variable: ",
          x,
          " not found in the main vars list!",
          call. = FALSE
        )
      }
    }
  }
  if (length(spec$extracted_info$asym_long_vars) > 0) {
    for (x in spec$extracted_info$asym_long_vars) {
      if (!(x %in% spec$extracted_info$independent_vars)) {
        stop(
          "Attention! The Long-run asymmetric variable: ",
          x,
          " not found in the main vars list!",
          call. = FALSE
        )
      }
    }
  }
  for (x in spec$extracted_info$all_vars) {
    if (!(x %in% colnames(spec$args_info$data))) {
      stop(
        "Attention! The variable: ",
        x,
        " not found in the data file's vars list!",
        call. = FALSE
      )
    }
  }
  if (length(spec$extracted_info$deterministic) > 0) {
    for (x in spec$extracted_info$deterministic) {
      if ((x %in% spec$extracted_info$all_vars)) {
        stop(
          "Attention! The external variable: ",
          x,
          " FOUND in the main vars list! The exegenious variables should be",
          " excluded from the main list",
          call. = FALSE
        )
      }
      if (!(x %in% colnames(spec$args_info$data))) {
        stop(
          "Attention! The variable: ",
          x,
          " not found in the data file's vars ",
          "list!",
          call. = FALSE
        )
      }
    }
  }

  baslik <- c(spec$extracted_info$dependent_var)
  baslik2 <- c(spec$extracted_info$dependent_var)
  for (x in spec$extracted_info$independent_vars) {
    if (x %in% spec$extracted_info$asym_short_vars) {
      baslik <- c(
        baslik,
        paste0(
          .kardl_settings_env$asym_prefix[1],
          x,
          .kardl_settings_env$asym_suffix[1]
        )
      )
      baslik <- c(
        baslik,
        paste0(
          .kardl_settings_env$asym_prefix[2],
          x,
          .kardl_settings_env$asym_suffix[2]
        )
      )
    } else {
      baslik <- c(baslik, x)
    }
    if (x %in% spec$extracted_info$asym_long_vars) {
      baslik2 <- c(
        baslik2,
        paste0(
          .kardl_settings_env$asym_prefix[1],
          x,
          .kardl_settings_env$asym_suffix[1]
        )
      )
      baslik2 <- c(
        baslik2,
        paste0(
          .kardl_settings_env$asym_prefix[2],
          x,
          .kardl_settings_env$asym_suffix[2]
        )
      )
    } else {
      baslik2 <- c(baslik2, x)
    }
  }
  short_run_vars <- baslik
  long_run_vars <- baslik2

  indep_as_excluded <- spec$extracted_info$independent_vars[
    !spec$extracted_info$independent_vars %in%
      spec$extracted_info$asym_short_vars
  ]
  indep_al_excluded <- spec$extracted_info$independent_vars[
    !spec$extracted_info$independent_vars %in%
      spec$extracted_info$asym_long_vars
  ]
  method <- ""
  if (
    length(spec$extracted_info$asym_long_vars) > 0 &&
      length(spec$extracted_info$asym_short_vars) > 0
  ) {
    method <- "NN"
  } else if (length(spec$extracted_info$asym_long_vars) > 0) {
    method <- "SA"
  } else if (length(spec$extracted_info$asym_short_vars) > 0) {
    method <- "AS"
  } else {
    method <- "LL"
  }

  shortrun_length <- length(indep_as_excluded) +
    length(spec$extracted_info$asym_short_vars) *
      (if (spec$args_info$different_asym_lag) 2 else 1)
  lag_rows_number <- (spec$args_info$maxlag^(shortrun_length + 1)) -
    (spec$args_info$maxlag^(shortrun_length))


  extracted_info <- list(
    all_asym_vars = all_asym_vars,
    indep_as_excluded = indep_as_excluded,
    indep_al_excluded = indep_al_excluded,
    short_run_vars = short_run_vars,
    long_run_vars = long_run_vars,
    shortrun_length = shortrun_length,
    lag_rows_number = lag_rows_number,
    model_type = method
  )

  spec$extracted_info <- lmerge(spec$extracted_info, extracted_info)
  spec
}

#' Create new variables for the model specification
#'
#' This function creates new variables for the model specification based on the
#' extracted information about asymmetric variables and the original data. It
#' generates new variables for positive and negative changes of asymmetric
#' variables, as well as lagged differences for short-run and long-run
#' variables. It also adds a trend variable if specified in the model. The
#' function updates the data in the model specification with the newly created
#' variables and returns the updated specification.
#'
#' @param spec A list containing the model specification, including extracted
#'   information about variables and the original data.
#' @return The updated model specification with new variables created and added
#'   to the data. The data in the model specification is updated with the new
#'   variables, and the original data is removed from the specification to avoid
#'   confusion. If any variable is not found in the data, an error is raised
#'   with a descriptive message.
#' @srrstats {G2.4b} Numeric model variables are converted to numeric vectors
#'   or matrix columns suitable for OLS before estimation.
#' @srrstats {G2.4c} Variable names and parsed formula terms are handled as
#'   character vectors when constructing lagged-variable names and asymmetric
#'   components.
#' @srrstats {G2.8} The original formula and data are converted into an
#'   internal structure containing the dependent variable, regressors, lag
#'   information, and generated variables.
#' @srrstats {G2.9} Generated variable names for lagged, differenced, and
#'   asymmetric components are constructed transparently so users can inspect
#'   the resulting model object.
#' @srrstats {TS1.4} Observational order is preserved during lag and difference
#'   construction.
#' @srrstats {TS2.1b} Structurally missing observations caused by lagging and
#'   differencing are removed from the fitted sample.
#' @srrstats {TS2.1c} The function does not impute missing time-series
#'   observations.
#' @noRd

create_new_vars <- function(spec) {
  temp_data <- spec$args_info$data
  if (is.null(colnames(temp_data))) {
    stop("`data` must have column names.", call. = FALSE)
  }

  ts_start <- start(temp_data)
  ts_freq <- frequency(temp_data)
  n <- NROW(temp_data)

  ts_vec <- function(x) {
    stats::ts(as.numeric(x), start = ts_start, frequency = ts_freq)
  }

  add_ts_col <- function(data, value, name) {
    old_names <- colnames(data)

    out <- stats::ts(
      cbind(as.matrix(data), as.numeric(value)),
      start = start(data),
      frequency = frequency(data)
    )

    colnames(out) <- c(old_names, name)
    out
  }

  get_col <- function(data, x) {
    if (!x %in% colnames(data)) {
      stop(
        "Variable `", x, "` is required but not found in `data`. ",
        "Available columns are: ",
        paste(colnames(data), collapse = ", "),
        call. = FALSE
      )
    }

    data[, x]
  }

  ts_lag <- function(x, k = 1) {
    y <- rep(NA_real_, n)

    if (k == 0) {
      return(ts_vec(x))
    }

    y[(k + 1):n] <- as.numeric(x)[1:(n - k)]
    ts_vec(y)
  }

  ts_diff <- function(x) {
    ts_vec(c(NA_real_, diff(as.numeric(x))))
  }

  ts_diff_lag <- function(x, k = 0) {
    ts_lag(ts_diff(x), k)
  }

  for (x in spec$extracted_info$all_asym_vars) {
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

    if (!pos_name %in% colnames(temp_data)) {
      dx <- ts_diff(get_col(temp_data, x))

      pos <- neg <- rep(NA_real_, n)

      for (i in seq_len(n)) {
        if (is.na(dx[i])) {
          pos[i] <- neg[i] <- NA_real_
        } else if (dx[i] > 0) {
          pos[i] <- dx[i] + ifelse(i == 1 || is.na(pos[i - 1]), 0, pos[i - 1])
          neg[i] <- ifelse(i == 1 || is.na(neg[i - 1]), 0, neg[i - 1])
        } else if (dx[i] < 0) {
          neg[i] <- dx[i] + ifelse(i == 1 || is.na(neg[i - 1]), 0, neg[i - 1])
          pos[i] <- ifelse(i == 1 || is.na(pos[i - 1]), 0, pos[i - 1])
        } else {
          pos[i] <- ifelse(i == 1 || is.na(pos[i - 1]), 0, pos[i - 1])
          neg[i] <- ifelse(i == 1 || is.na(neg[i - 1]), 0, neg[i - 1])
        }
      }

      temp_data <- add_ts_col(temp_data, pos, pos_name)
      temp_data <- add_ts_col(temp_data, neg, neg_name)
    }
  }

  as_shortlar <- unlist(lapply(
    spec$extracted_info$asym_short_vars,
    function(i) {
      c(
        paste0(
          .kardl_settings_env$asym_prefix[1],
          i,
          .kardl_settings_env$asym_suffix[1]
        ),
        paste0(
          .kardl_settings_env$asym_prefix[2],
          i,
          .kardl_settings_env$asym_suffix[2]
        )
      )
    }
  ))

  short_run_vars <- c(
    spec$extracted_info$dependent_var,
    spec$extracted_info$indep_as_excluded,
    as_shortlar
  )

  for (x in short_run_vars) {
    for (j in 0:spec$args_info$maxlag) {
      new_var_name <- replace_lag_var(
        .kardl_settings_env$short_coef,
        x,
        j
      )

      if (!new_var_name %in% colnames(temp_data)) {
        y <- ts_diff_lag(get_col(temp_data, x), j)
        temp_data <- add_ts_col(temp_data, y, new_var_name)
      }
    }
  }

  as_longlar <- unlist(lapply(
    spec$extracted_info$asym_long_vars,
    function(i) {
      c(
        paste0(
          .kardl_settings_env$asym_prefix[1],
          i,
          .kardl_settings_env$asym_suffix[1]
        ),
        paste0(
          .kardl_settings_env$asym_prefix[2],
          i,
          .kardl_settings_env$asym_suffix[2]
        )
      )
    }
  ))

  long_run_vars <- c(
    spec$extracted_info$dependent_var,
    spec$extracted_info$indep_al_excluded,
    as_longlar
  )

  for (x in long_run_vars) {
    new_var_name <- replace_lag_var(
      .kardl_settings_env$long_coef,
      x,
      1
    )

    if (!new_var_name %in% colnames(temp_data)) {
      y <- ts_lag(get_col(temp_data, x), 1)
      temp_data <- add_ts_col(temp_data, y, new_var_name)
    }
  }

  if (spec$extracted_info$trend) {
    spec$extracted_info$deterministic <- c(
      spec$extracted_info$deterministic,
      "trend"
    )

    if (!"trend" %in% colnames(temp_data)) {
      temp_data <- add_ts_col(temp_data, seq_len(n), "trend")
    }
  }

  spec$args_info$data <- NULL
  spec$extracted_info$data <- temp_data

  attr(spec$extracted_info$data, "source") <- "spec$args_info$data"
  attr(spec$extracted_info$data, "description") <- paste0(
    "This value was created by adding new variables to the original data set ",
    "and removing the variables which are not used in the model."
  )

  spec
}


#' Create the short-run model formula based on the specified variables and lags
#'
#' This function generates the short-run model formula by combining the
#' specified short-run variables, their corresponding lags, and any
#' deterministic variables. It constructs the formula in a format suitable for
#' estimation, ensuring that the variable names and lag structures are correctly
#'  represented based on the model specification.
#' @param short_run_vars A vector of short-run variable names to be included in
#'  the model formula.
#' @param lags_list A list of lag values corresponding to each short-run
#' variable, indicating how many lags of each variable should be included in
#' the model.
#' @param deterministic A vector of deterministic variable names to be included
#' in the model formula, if any.
#' @return A character string representing the short-run model formula,
#' combining the specified variables, their lags, and deterministic variables in
#'  the appropriate format for estimation.
#' @noRd
#'

make_shortrun_model <- function(short_run_vars, lags_list, deterministic) {
  modd1 <- c()
  for (j in seq_along(short_run_vars)) {
    start <- as.numeric(j < 2)
    for (k in start:lags_list[j]) {
      modd1 <- c(
        modd1,
        replace_lag_var(.kardl_settings_env$short_coef, short_run_vars[j], k)
      )
    }
  }
  paste(c(modd1, deterministic), collapse = "+")
}

#' Create the long-run model formula based on the specified variables and lags
#'
#' This function generates the long-run model formula by combining the specified
#'  long-run variables, their corresponding lags, and any deterministic
#'  variables. It constructs the formula in a format suitable for estimation,
#'  ensuring that the variable names and lag structures are correctly
#'  represented based on the model specification. If a long-run part is already
#'  defined in the user inputs, it uses that instead of generating a new
#'  formula.
#' @param spec A list containing the model specification, including extracted
#' information about variables and user inputs. The function checks if a
#' long-run part is already defined in the user inputs and uses it if
#' available; otherwise, it generates the long-run model formula based on the
#' specified long-run variables and their lags.
#' @return A character string representing the long-run model formula, combining
#'  the specified variables, their lags, and deterministic variables in the
#'  appropriate format for estimation. If a long-run part is already defined in
#'  the user inputs, it returns that instead of generating a new formula.
#' @noRd
#'

make_longrun_model <- function(spec) {
  if (!is.null(spec$args_info$longRunPart)) {
    ls_longrun <- spec$args_info$longRunPart
  } else {
    modd <- lapply(spec$extracted_info$long_run_vars, function(i) {
      replace_lag_var(.kardl_settings_env$long_coef, i, 1)
    })
    ls_longrun <- paste(modd, collapse = "+")
  }
  if (spec$extracted_info$no_constant) {
    ls_longrun <- paste0(ls_longrun, "-1")
  }

  ls_dependent <- replace_lag_var(
    .kardl_settings_env$short_coef,
    spec$extracted_info$dependent_var,
    0
  )
  list(
    ls_longrun = ls_longrun,
    ls_dependent = ls_dependent
  )
}

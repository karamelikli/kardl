# =============================================================================
# Global settings environment
# =============================================================================

.kardl_settings_env <- new.env(parent = emptyenv())

# Default values stored as a list (used for reset)
.kardl_settings_env_default <- list(
  data = NULL,
  formula = NULL,
  maxlag = 4L,
  mode = "quick",
  criterion = "AIC",
  different_asym_lag = TRUE,
  asym_prefix = c(),
  asym_suffix = c("_POS", "_NEG"),
  long_coef = "L{lag}.{var_name}",
  short_coef = "L{lag}.d.{var_name}",
  batch = "1/1",
  print_wrap = NULL
)

# Initialize the environment with defaults
local({
  for (nm in names(.kardl_settings_env_default)) {
    .kardl_settings_env[[nm]] <- .kardl_settings_env_default[[nm]]
  }
})

#' Set kardl Package Options
#'
#' This function allows users to set options for the kardl package. Users can
#' specify named arguments to set options or call the function without arguments
#'  to retrieve all current settings.
#'
#' @param ... Named arguments corresponding to the options to be set. Valid
#' option names include those defined in the kardl package settings.
#'
#' @return All current settings as a list after applying any updates from the
#' provided named arguments invisibly.
#'
#'
#' @export
#' @examples
#' # Get default options
#' kardl_get("maxlag", "mode")
#' # Set options
#' kardl_set(maxlag = 5, mode = "grid")
#' # Get specific options
#' kardl_get("maxlag", "mode")
#'
#' # To have the updated settings available in the global environment, assign
#' # the output to a variable:
#' my_settings <- kardl_set(
#'   long_coef = "LongRun_{var_name}",
#'   short_coef = "ShortRun_{var_name}"
#' )
#' # Now my_settings contains the updated settings, and the kardl package
#' # will use these settings for subsequent operations.
#' my_settings$long_coef
#' my_settings$maxlag
#'
#' # Demonstrate setting print_wrap option
#' kardl_set(print_wrap = 80L)
#'
#' # Reset to defaults after demonstrating custom settings
#' kardl_reset()
#'
#' @return If no arguments are provided, returns all options as a list. If named
#'  arguments are provided, sets those options and returns the updated list.
#' @seealso  \code{\link{kardl_get}}, \code{\link{kardl_reset}}
#'
#' @srrstats {G2.3a} Package-level option names are validated against a
#' predefined list of acceptable values; invalid names produce an informative
#' error.
#' @srrstats {G2.4} Default argument values for `formula`, `data`, `maxlag`,
#' `mode`, `criterion`, and `different_asym_lag` are stored centrally and merged
#' with function arguments at call time.
#'


kardl_set <- function(...) {
  args <- list(...)

  # Set named arguments
  if (length(args) > 0 && !is.null(names(args))) {
    invalid <- setdiff(names(args), names(.kardl_settings_env_default))
    if (length(invalid) > 0) {
      stop(sprintf("Invalid option(s): %s", toString(invalid)),
        call. = FALSE
      )
    }

    # Assign new values
    for (name in names(args)) {
      .kardl_settings_env[[name]] <- args[[name]]
    }
  }

  # Return behavior

  invisible(as.list(.kardl_settings_env))
}


#' Get kardl Package Options
#'
#' This function retrieves the current settings of the kardl package. Users can
#' specify option names to get their values or call the function without
#' arguments to retrieve all current settings.
#'
#' @param ... Option names to retrieve. If no arguments are provided, all
#' options will be returned.
#'
#' @export
#' @examples
#'
#' # Get all options
#' kardl_get()
#' # Get specific options
#' kardl_get("maxlag", "mode")
#'
#' # Note: In interactive use, avoid calling kardl_get() directly to prevent
#' # cluttering the console.
#'
#' a <- kardl_get()
#' a$asym_suffix
#'
#' # To reset options and then get a default option:
#' mydefaults <- kardl_reset()
#' mydefaults$maxlag
#'
#' @return If no arguments are provided, returns all options as a list. If
#' specific option names are provided, returns their values.
#'
#' @seealso \code{\link{kardl_set}}, \code{\link{kardl_reset}}
#'
#' @srrstats {G2.3a} Requested option names are validated against the predefined
#'  settings list; unknown names produce an informative error.
#'
kardl_get <- function(...) {
  names_requested <- c(...)

  # Return all settings if no arguments
  if (length(names_requested) == 0) {
    return(as.list(.kardl_settings_env)) # nolint: return_linter.
  }

  # Check for invalid names
  invalid <- setdiff(names_requested, names(.kardl_settings_env_default))
  if (length(invalid) > 0) {
    stop(sprintf("Option(s) not found: %s", toString(invalid)),
      call. = FALSE
    )
  }

  # Extract requested values
  result <- lapply(names_requested, function(name) {
    .kardl_settings_env[[name]]
  })
  names(result) <- names_requested

  # Return single value directly if only one requested
  if (length(result) == 1) {
    result[[1]]
  } else {
    result
  }
}


#' Reset kardl Package Options to Default Values
#'
#' This function resets kardl package options to their default values.
#'
#' @param exclude Character vector of setting names that should not be reset.
#' These settings retain their current values. By default, all settings are
#' reset.
#'
#' @return A list of the settings after reset, returned invisibly.
#' @export
#' @seealso \code{\link{kardl_set}}, \code{\link{kardl_get}}
#' @examples
#' kardl_set(criterion = "BIC", different_asym_lag = FALSE)
#'
#' # Reset all settings to defaults except "criterion"
#' kardl_reset(exclude = "criterion")
#'
#' # Get the current settings to verify the reset
#' print(kardl_get("criterion"))
#'
#' # This will show "BIC" since it was excluded from the reset,
#' # while other settings will be reset to their defaults.
#' print(kardl_get("different_asym_lag"))
kardl_reset <- function(exclude = NULL) {
  # Validate input
  if (!is.null(exclude) && !is.character(exclude)) {
    stop("`exclude` must be a character vector of setting names.")
  }

  # Get all setting names
  all_names <- names(.kardl_settings_env_default)

  # Check validity of exclude
  if (!is.null(exclude)) {
    invalid <- setdiff(exclude, all_names)
    if (length(invalid) > 0) {
      stop("Invalid setting(s) in `exclude`: ", toString(invalid))
    }
  }

  to_reset <- setdiff(all_names, exclude)
  rm(list = to_reset, envir = .kardl_settings_env)

  # Repopulate defaults only for those
  for (nm in to_reset) {
    .kardl_settings_env[[nm]] <- .kardl_settings_env_default[[nm]]
  }

  invisible(as.list(.kardl_settings_env))
}

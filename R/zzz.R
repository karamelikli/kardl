# =============================================================================
# Global settings environment
# =============================================================================

.kardl_Settings_env <- new.env(parent = emptyenv())

# Default values stored as a list (used for reset)
.kardl_Settings_env_default <- list(
  data             = NULL,
  formula          = NULL,
  maxlag           = 4L,
  mode             = "quick",
  criterion        = "AIC",
  differentAsymLag = TRUE,
  AsymPrefix       = c(),
  AsymSuffix       = c("_POS", "_NEG"),
  LongCoef         = "L{lag}.{varName}",
  ShortCoef        = "L{lag}.d.{varName}",
  batch            = "1/1"
)

# Initialize the environment with defaults
local({
  for (nm in names(.kardl_Settings_env_default)) {
    .kardl_Settings_env[[nm]] <- .kardl_Settings_env_default[[nm]]
  }
})

#' Set kardl Package Options
#'
#' This function allows users to set options for the kardl package. Users can specify named arguments to set options or call the function without arguments to retrieve all current settings.
#'
#' @param ... Named arguments corresponding to the options to be set. Valid option names include those defined in the kardl package settings.
#'
#' @return All current settings as a list after applying any updates from the provided named arguments invisibly.
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
#' # To have the updated settings available in the global environment, assign the output to a variable:
#' MySettings<- kardl_set(LongCoef = "LongRun_{varName}", ShortCoef = "ShortRun_{varName}")
#' # Now MySettings contains the updated settings, and the kardl package will use these settings for subsequent operations.
#' MySettings$LongCoef
#' MySettings$maxlag
#'
#' @return If no arguments are provided, returns all options as a list. If named arguments are provided, sets those options and returns the updated list.
#' @seealso  \code{\link{kardl_get}}, \code{\link{kardl_reset}}
#'
#'



kardl_set <- function(...) {
  args <- list(...)

  # Set named arguments
  if (length(args) > 0 && !is.null(names(args))) {
    invalid <- setdiff(names(args), names(.kardl_Settings_env_default))
    if (length(invalid) > 0) {
      stop(sprintf("Invalid option(s): %s", paste(invalid, collapse = ", ")),
           call. = FALSE)
    }

    # Assign new values
    for (name in names(args)) {
      .kardl_Settings_env[[name]] <- args[[name]]
    }
  }

  # Return behavior

    return(invisible(as.list(.kardl_Settings_env)))

}



#' Get kardl Package Options
#'
#' This function retrieves the current settings of the kardl package. Users can specify option names to get their values or call the function without arguments to retrieve all current settings.
#'
#' @param ... Option names to retrieve. If no arguments are provided, all options will be returned.
#'
#' @export
#' @examples
#'
#' # Get all options
#' kardl_get()
#' # Get specific options
#' kardl_get("maxlag", "mode")
#'
#' # Note: In interactive use, avoid calling kardl_get() directly to prevent cluttering the console.
#'
#' a<-kardl_get()
#' a$AsymSuffix
#'
#' # To reset options and then get a default option:
#' mydefaults <- kardl_reset()
#' mydefaults$maxlag
#'
#' @return If no arguments are provided, returns all options as a list. If specific option names are provided, returns their values.
#'
#' @seealso \code{\link{kardl_set}}, \code{\link{kardl_reset}}
#'
kardl_get <- function(...) {
  names_requested <- c(...)

  # Return all settings if no arguments
  if (length(names_requested) == 0) {
    return(as.list(.kardl_Settings_env))
  }

  # Check for invalid names
  invalid <- setdiff(names_requested, names(.kardl_Settings_env_default))
  if (length(invalid) > 0) {
    stop(sprintf("Option(s) not found: %s", paste(invalid, collapse = ", ")),
         call. = FALSE)
  }

  # Extract requested values
  result <- lapply(names_requested, function(name) {
    .kardl_Settings_env[[name]]
  })
  names(result) <- names_requested

  # Return single value directly if only one requested
  if (length(result) == 1) {
    return(result[[1]])
  } else {
    return(result)
  }
}


#' Reset kardl Package Options to Default Values
#'
#' This function resets all options in the kardl package to their default values.
#'
#' @return A list of the default settings after reset, returned invisibly.
#' @export
#' @seealso \code{\link{kardl_set}}, \code{\link{kardl_get}}
#' @examples
#'
#' # Set some options
#' kardl_set(criterion = "BIC", differentAsymLag = TRUE)
#'
#' # Reset to default options
#' before <- kardl_get("criterion")
#' out <-kardl_reset()
#' after <- kardl_get("criterion")
#' cat("Before reset:", before, "\n")
#' cat("After reset:", after, "\n")

kardl_reset <- function() {
  # Reinitialize the environment with default settings
  # Completely reset the environment to a clean state
  rm(list = ls(envir = .kardl_Settings_env, all.names = TRUE),
     envir = .kardl_Settings_env)

  # Repopulate from defaults
  for (nm in names(.kardl_Settings_env_default)) {
    .kardl_Settings_env[[nm]] <- .kardl_Settings_env_default[[nm]]
  }


    return(invisible(as.list(.kardl_Settings_env)))
 }

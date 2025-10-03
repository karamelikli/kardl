# Create an environment to store settings
.kardl_Settings_env <- new.env(parent = emptyenv())

# Initialize settings in the environment
.kardl_Settings_env$data<-NULL
.kardl_Settings_env$model<-NULL
.kardl_Settings_env$maxlag <- 4
.kardl_Settings_env$mode<-"quick"
.kardl_Settings_env$criterion <- "AIC"
.kardl_Settings_env$differentAsymLag <- TRUE
.kardl_Settings_env$AsymPrefix <- c()
.kardl_Settings_env$AsymSuffix <- c("_POS", "_NEG")
.kardl_Settings_env$LongCoef <- "L{lag}.{varName}" #  L{lag}.{varName}    L1.LRM
.kardl_Settings_env$ShortCoef <- "L{lag}.d.{varName}" # L{lag}.d.{varName}  L2.d.LRM
.kardl_Settings_env$batch <- "1/1"

# Store default settings as a list
.kardl_Settings_env_default <- as.list(.kardl_Settings_env)

#' Function to get or set settings
#'
#' This function allows you to get or set various options related to the kardl package.
#'
#' @param . If provided, the function will return this value. If not provided or set to `FALSE`, it will return the current settings.
#' @param ... Named arguments to set options, or no arguments to get all options.
#'
#' @export
#' @examples
#' # Set options
#' kardl_set(criterion = "BIC", differentAsymLag = TRUE)
#' # Get specific options
#' kardl_get("criterion", "differentAsymLag")
#' # Get all options.
#' # Note: In interactive use, avoid calling this directly to prevent cluttering the console.
#' \donttest{
#' kardl_get()
#' }
#'
#' # Utilizing the magrittr pipe
#' library(magrittr)
#' # Set options and then get them
#'
#' MyFormula<-CPI~ER+PPI+asym(ER)+deterministic(covid)+trend
#' kardl_set(ShortCoef = "L___{lag}.d.{varName}")
#' imf_example_data %>%   kardl(MyFormula)
#'
#' kardl_reset()
#' kardl_get()
#'
#' imf_example_data %>%  kardl_set(LongCoef= "LK{lag}_{varName}",ShortCoef = "D{lag}.d.{varName}") %>%
#' kardl(MyFormula)
#' kardl_get(c("LongCoef","ShortCoef"))
#'
#' @return If no arguments are provided, returns all options as a list. If named arguments are provided, sets those options and returns the updated list.
#' @seealso  \code{\link{kardl_get}}, \code{\link{kardl_reset}}
#'
#'



kardl_set <- function(.=FALSE, ...) {
  args <- list(...)

  # Named arguments: set options
  if (!is.null(names(args))) {
    invalid <- setdiff(names(args), names(.kardl_Settings_env))
    if (length(invalid) > 0) {
      stop(sprintf("Invalid option(s): %s", paste(invalid, collapse = ", ")))
    }
    # Assign new values to the environment
    for (name in names(args)) {
      .kardl_Settings_env[[name]] <- args[[name]]
    }
  }

  # Return based on whether . exists and is non-NULL
  if (!missing(.) && !is.null(.) && !isFALSE(.)) {
    return(.)
  } else {
    return(invisible(as.list(.kardl_Settings_env)))
  }
}



#' Function to get settings
#'
#' This function retrieves options from the kardl package settings.
#'
#' @param ... Names of the options to retrieve. If none provided, all options are returned.
#'
#' @export
#' @examples
#' # Get specific options
#' kardl_get("criterion", "differentAsymLag")
#'
#' # Get all options (Note: In interactive use, avoid calling this directly to prevent cluttering the console)
#' a<-kardl_get()
#' a$AsymSuffix
#'
#' @return If no arguments are provided, returns all options as a list. If one option is requested, returns its value directly. If multiple options are requested, returns a list of those options.
#' @seealso \code{\link{kardl_set}}, \code{\link{kardl_reset}}
#'
kardl_get <- function(...) {
  names_requested <- c(...)

  # No arguments: return all options as a list
  if (length(names_requested) == 0) {
    return(as.list(.kardl_Settings_env))
  }

  # Check for invalid option names
  invalid <- setdiff(names_requested, names(.kardl_Settings_env))
  if (length(invalid) > 0) {
    stop(sprintf("Option(s) not found: %s", paste(invalid, collapse = ", ")))
  }

  # Retrieve requested options
  result <- lapply(names_requested, function(name) .kardl_Settings_env[[name]])

  # Name the result list
  names(result) <- names_requested

  # If only one option is requested, return the value directly
  if (length(result) == 1) {
    return(result[[1]])
  } else {
    return(result)
  }
}


#' Function to reset kardl package settings
#'
#' This function resets all options in the kardl package to their default values.
#'
#' @param . If provided and not `FALSE`, the function will return this value after resetting the settings. If not provided or set to `FALSE`, it will return the current settings.
#'
#' @export
#' @examples
#' # Set some options
#' kardl_set(criterion = "BIC", differentAsymLag = TRUE)
#' # Reset to default values
#' kardl_get("criterion")  # Check current settings
#' kardl_reset()
#' kardl_get("criterion")  # Check settings after reset
#'
#' library(magrittr)
#'  MyFormula<-CPI~ER+PPI+asym(ER)+deterministic(covid)+trend
#' imf_example_data %>%
#'   kardl_set(LongCoef= "K1{lag}w1{varName}",differentAsymLag= FALSE ) %>%  kardl(MyFormula ) %>%
#'     kardl_reset()
#' kardl_get()
#'
#' imf_example_data %>%
#'   kardl_reset() %>%
#'     kardl_set(LongCoef= "K2{lag}w2{varName}",differentAsymLag=FALSE ) %>%  kardl(MyFormula)
#'
#' kardl_get(c("LongCoef","differentAsymLag","ShortCoef","batch"))
#'
#' @return Returns the default options as a list.
#' @seealso \code{\link{kardl_set}}, \code{\link{kardl_get}}
#'
kardl_reset <- function(.=FALSE) {
  # Reinitialize the environment with default settings
  for (name in names(.kardl_Settings_env_default)) {
    .kardl_Settings_env[[name]] <- .kardl_Settings_env_default[[name]]
  }
  # Return based on whether . exists and is non-NULL
  if (!missing(.) && !is.null(.) && !isFALSE(.)) {
    return(.)
  } else {
    return(invisible(as.list(.kardl_Settings_env)))
  }
}

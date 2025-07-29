# Create an environment to store settings
.kardl_env <- new.env(parent = emptyenv())

# Initialize settings in the environment
.kardl_env$criterion <- "AIC"
.kardl_env$DifferentAsymLag <- TRUE
.kardl_env$AsymPrefix <- c()
.kardl_env$AsymSuffix <- c("_POS", "_NEG")
.kardl_env$LongCoef <- "L{lag}.{varName}" #  L{lag}.{varName}    L1.LRM
.kardl_env$ShortCoef <- "L{lag}.d.{varName}" # L{lag}.d.{varName}  L2.d.LRM
.kardl_env$batch <- "1/1"

# Store default settings as a list
.kardl_env_default <- as.list(.kardl_env)

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
#' kardl_set(criterion = "BIC", DifferentAsymLag = TRUE)
#' # Get specific options
#' kardl_get("criterion", "DifferentAsymLag")
#' # Get all options
#' kardl_get()
#'
#' # Utilizing the magrittr pipe
#' library(magrittr)
#' #' # Set options and then get them
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
#' kardl_get()
#'
#' @return If no arguments are provided, returns all options as a list. If named arguments are provided, sets those options and returns the updated list.
#' @seealso [kardl_get()] [kardl_reset()]
#'
#'



kardl_set <- function(.=FALSE, ...) {
  args <- list(...)

  # Named arguments: set options
  if (!is.null(names(args))) {
    invalid <- setdiff(names(args), names(.kardl_env))
    if (length(invalid) > 0) {
      stop(sprintf("Invalid option(s): %s", paste(invalid, collapse = ", ")))
    }
    # Assign new values to the environment
    for (name in names(args)) {
      .kardl_env[[name]] <- args[[name]]
    }
  }

  # Return based on whether . exists and is non-NULL
  if (!missing(.) && !is.null(.) && !isFALSE(.)) {
    return(.)
  } else {
    return(invisible(as.list(.kardl_env)))
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
#' kardl_get("criterion", "DifferentAsymLag")
#'
#' # Get all options
#' kardl_get()
#'
#' @return If no arguments are provided, returns all options as a list. If one option is requested, returns its value directly. If multiple options are requested, returns a list of those options.
#' @seealso [kardl_set()] [kardl_reset()]
#'
kardl_get <- function(...) {
  names_requested <- c(...)

  # No arguments: return all options as a list
  if (length(names_requested) == 0) {
    return(as.list(.kardl_env))
  }

  # Check for invalid option names
  invalid <- setdiff(names_requested, names(.kardl_env))
  if (length(invalid) > 0) {
    stop(sprintf("Option(s) not found: %s", paste(invalid, collapse = ", ")))
  }

  # Retrieve requested options
  result <- lapply(names_requested, function(name) .kardl_env[[name]])

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
#' kardl_set(criterion = "BIC", DifferentAsymLag = TRUE)
#' # Reset to default values
#' kardl_get()  # Check current settings
#' kardl_reset()
#' kardl_get()  # Check settings after reset
#'
#' library(magrittr)
#'  MyFormula<-CPI~ER+PPI+asym(ER)+deterministic(covid)+trend
#' imf_example_data %>%
#'   kardl_set(LongCoef= "K1{lag}w1{varName}",DifferentAsymLag= FALSE ) %>%  kardl(MyFormula ) %>%
#'     kardl_reset()
#' kardl_get()
#'
#' imf_example_data %>%
#'   kardl_reset() %>%
#'     kardl_set(LongCoef= "K2{lag}w2{varName}",DifferentAsymLag=FALSE ) %>%  kardl(MyFormula)
#'
#' kardl_get()
#'
#' @return Returns the default options as a list.
#' @seealso [kardl_set()], [kardl_get()]
#'
kardl_reset <- function(.=FALSE) {
  # Reinitialize the environment with default settings
  for (name in names(.kardl_env_default)) {
    .kardl_env[[name]] <- .kardl_env_default[[name]]
  }
  # Return based on whether . exists and is non-NULL
  if (!missing(.) && !is.null(.) && !isFALSE(.)) {
    return(.)
  } else {
    return(invisible(as.list(.kardl_env)))
  }
}

#' IMF Example Data (Deprecated)
#'
#' @description
#' `r lifecycle::badge('deprecated')`
#'
#' This dataset is \strong{deprecated} and is provided for testing purposes only
#' for versions of \pkg{kardl} before 2.0.5. It is retained for backward
#' compatibility. Now the \strong{Seatbelts} dataset is used for testing
#' purposes. Please use the Seatbelts dataset instead.
#'
#' @format A data frame with 470 rows and 4 variables:
#' \describe{
#'   \item{ER}{Numeric. Exchange rate of Turkey.}
#'   \item{CPI}{Numeric. CPI of Turkey.}
#'   \item{PPI}{Numeric. PPI of Turkey.}
#'   \item{covid}{Integer. COVID-19 dummy variable.}
#' }
#' @examples
#' data(imf_example_data)
#' head(imf_example_data)
"imf_example_data"

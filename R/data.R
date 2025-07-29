#' IMF Example Data
#'
#' This is an example data set used for testing purposes.
#' @details
#' These data obtained from "imf.data" package.
#' The sample data is not updated and obtained by following codes.:
#' 

#' \code{install.packages("imf.data")}\cr
#' \code{library("imf.data")}\cr
#' \code{IFS <- load_datasets("IFS")}
#' 
#' \itemize{
#' \item \strong{PCPI_IX}             Prices, Consumer Price Index, All items, Index\cr
#' \item \strong{AIP_IX}              Economic Activity, Industrial Production, Index\cr
#' \item \strong{ENDE_XDC_USD_RATE}   Exchange Rates, Domestic Currency per U.S. Dollar, End of Period, Rate}
#' 
#' \code{  trdata<-IFS$get_series(freq = "M", ref_area = "TR", indicator = c("PCPI_IX","AIP_IX","ENDE_XDC_USD_RATE"),start_period = "1985-01",end_period = "2024-02")}\cr
#' \code{  PeriodRow<-trdata[,1]}\cr
#' \code{  trdata[,1]<-NULL}\cr
#' \code{  colnames(trdata)<-c("ER","CPI","PPI")}\cr
#' \code{  trdata<-log(as.data.frame(lapply(trdata, function(x) as.numeric(x))))}\cr
#' \code{  rownames(trdata)<-PeriodRow}
#' 
#' \strong{Inserting covid dummy variable}
#' 
#' \code{  trdata<-cbind(trdata,covid=0)}\cr
#' \code{  trdata[420:470,4]<-1}
#'
#' @format A data frame with 470 rows and 4 variables:
#' \describe{
#'   \item{ER}{Numeric. Exchange rate of Turkey.}
#'   \item{CPI}{Numeric. CPI of Turkey.}
#'   \item{PPI}{Numeric. PPI of Turkey.}
#'   \item{covid}{Integer.Covid19 dummy variable.}
#' }
#' @examples
#' data(imf_example_data)
#' head(imf_example_data)
"imf_example_data"

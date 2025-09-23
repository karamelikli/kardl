#' CUSUM Stability Test
#'
#' Computes the CUSUM (Cumulative Sum) test for stability of regression coefficients.
#' The function returns the test statistics and confidence bands but does not plot automatically.
#'
#' @param inputs_ A fitted model object of class "lm" or "kardl".
#'
#' @return An object of class "kardl" with test results and data frame for plotting.
#' @seealso \code{\link{cusumq}}, \code{\link{plot.kardl}}
#' @export
#' @importFrom strucchange recresid
#' @examples
#' # For lm estimations
#' MyLM <- lm(CPI ~ ER + PPI, imf_example_data)
#' res_cusum <- cusum(MyLM)
#' plot(res_cusum)   # explicitly plot
#'
#' # For ARDL model
#' kardl_model <- kardl(
#'   imf_example_data,
#'   CPI ~ ER + PPI + asym(ER) + deterministic(covid) + trend,
#'   mode = c(1,2,3,0)
#' )
#' res_cusum <- cusum(kardl_model)
#' res_cusum
#' plot(res_cusum)
#'
#' # Print Cusum data frame
#' head(res_cusum$dataframe)
#'
#' # Example with ggplot2
#' library(ggplot2)
#' ggplot(res_cusum$dataframe, aes(x = X, y = Y)) +
#'   geom_line(color = "blue") +
#'   geom_line(aes(y = Lower), color = "red") +
#'   geom_line(aes(y = Upper), color = "red") +
#'   geom_hline(yintercept = 0, linetype = "dashed") +
#'   ggtitle("CUSUM Test")
cusum <- function(inputs_) {

  if (inherits(inputs_, "kardl")) {
    OptModel <- inputs_$finalModel$model
  } else if (inherits(inputs_, "lm")) {
    OptModel <- inputs_
  } else {
    stop("Not suitable input. Must be 'lm' or 'kardl' object.")
  }

  rece <- recresid(OptModel)
  sels <- summary(OptModel)
  bbst <- sels$coefficients[,1]
  k <- length(bbst[-1])
  n <- length(OptModel$residuals)

  w <- as.matrix(na.omit(rece))
  w <- cumsum(w / apply(w, 2, sd))
  c <- 0.984
  w2 <- Kseqa(c*sqrt(n-k), (2*c*sqrt(n-k))/length(w), length(w))
  w1 <- Kseqa(-c*sqrt(n-k), (-2*c*sqrt(n-k))/length(w), length(w))
  x  <- Kseqa(k,1,length(w))
  w1 <- matrix(w1, length(w1), 1)
  w  <- matrix(w, length(w), 1)
  w2 <- matrix(w2, length(w2), 1)

  Cu <- data.frame(X = c(x), Lower = w1, Y = w, Upper = w2)
  CuTest <- ifelse(Cu$Y > Cu$Lower & Cu$Y < Cu$Upper, 0, 1)
  karar <- max(CuTest)

  Karamelikli <- list(
    type = "Cusum",
    Stability = ifelse(karar > 0, "U", "S"),
    dataframe = Cu,
    karar = karar,
    method = "Cusum"
  )
  class(Karamelikli) <- "kardl"
  Karamelikli
}

#' CUSUM of Squares Stability Test
#'
#' Computes the CUSUMQ (Cumulative Sum of Squares) test for variance stability.
#' The function returns the test statistics and confidence bands but does not plot automatically.
#'
#' @param inputs_ A fitted model object of class "lm" or "kardl".
#'
#' @return An object of class "kardl" with test results and data frame for plotting.
#' @seealso \code{\link{cusum}}, \code{\link{plot.kardl}}
#' @export
#' @importFrom strucchange recresid
#' @examples
#' # For lm estimations
#' MyLM <- lm(CPI ~ ER + PPI, imf_example_data)
#' res_cusumq <- cusumq(MyLM)
#' plot(res_cusumq)   # explicitly plot
#'
#' # For ARDL model
#' kardl_model <- kardl(
#'   imf_example_data,
#'   CPI ~ ER + PPI + asym(ER) + deterministic(covid) + trend,
#'   mode = c(1,2,3,0)
#' )
#' res_cusumq <- cusumq(kardl_model)
#' res_cusumq
#' plot(res_cusumq)
#'
#' # Print CusumQ data frame
#' head(res_cusumq$dataframe)
#'
#' # Example with ggplot2
#' library(ggplot2)
#' ggplot(res_cusumq$dataframe, aes(x = X, y = Y)) +
#'   geom_line(color = "blue") +
#'   geom_line(aes(y = Lower), color = "red") +
#'   geom_line(aes(y = Upper), color = "red") +
#'   geom_hline(yintercept = 0, linetype = "dashed") +
#'   ggtitle("CUSUM of Squares Test")
cusumq <- function(inputs_) {

  if (inherits(inputs_, "kardl")) {
    OptModel <- inputs_$finalModel$model
  } else if (inherits(inputs_, "lm")) {
    OptModel <- inputs_
  } else {
    stop("Not suitable input. Must be 'lm' or 'kardl' object.")
  }

  rece <- recresid(OptModel)
  sels <- summary(OptModel)
  bbst <- sels$coefficients[,1]
  k <- length(bbst[-1])
  n <- length(OptModel$residuals)

  w <- as.matrix(na.omit(rece))
  w <- cumsum(w^2) / sum(w^2)

  m <- abs(0.5*(n-k)-1)
  c <- 0.74191 - 0.17459*log(m) - 0.26526*(1/m) + 0.0029985*m - 0.000010943*m^2
  w2 <- c + (Kseqa(k,1,length(w)) - k)/(n-k)
  w1 <- -c + (Kseqa(k,1,length(w)) - k)/(n-k)
  x  <- Kseqa(k,1,length(w))
  w1 <- matrix(w1, length(w1), 1)
  w  <- matrix(w, length(w), 1)
  w2 <- matrix(w2, length(w2), 1)

  Cuq <- data.frame(X = c(x), Lower = w1, Y = w, Upper = w2)
  CuTest <- ifelse(Cuq$Y > Cuq$Lower & Cuq$Y < Cuq$Upper, 0, 1)
  karar <- max(CuTest)

  Karamelikli <- list(
    type = "CusumQ",
    Stability = ifelse(karar > 0, "U", "S"),
    dataframe = Cuq,
    karar = karar,
    method = "CusumQ"
  )
  class(Karamelikli) <- "kardl"
  Karamelikli
}


# this function took from nardl package
Kseqa<-function(a,b,c){
  #seq=(a:b:(a+b*(c-1)))';
  se<-seq(a,(a+b*(c-1)),by=b)
  return(t(se))
}

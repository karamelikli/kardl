#' Plot CUSUSM
#'
#' This function generates a plot for the Cumulative Sum (CUSUM) test, which is used to evaluate the stability of regression coefficients over time in econometric models. The CUSUM test checks for parameter stability by examining the cumulative sum of recursive residuals, and the plot visually displays any potential deviations from stability. If the CUSUM line stays within the confidence bands, the model is stable; if it crosses the bounds, it indicates instability.
#' @param inputs_ the model
#' @param saveToFile save to file TRUE/FALSE.  File name should be have .jpg extension.
#'
#' @return list plot
#' @seealso \code{\link{cusumq}}
#' @export
#' @import graphics
#' @importFrom strucchange  recresid
#' @examples
#' # For lm estimations
#'
#' MyLM <-lm(CPI~ER+PPI,imf_example_data)
#' cusum(MyLM)
#'
#' # For ARDL model
#' kardl_model<-kardl(imf_example_data,CPI~ER+PPI+asym(ER)+deterministic(covid)+trend,mode=c(1,2,3,0))
#' cDF<-cusum(kardl_model)
#' cDF
#'
#' # Print Cusum data frame
#' head(cDF$dataframe )
#'
#' # For saving as a file:
#' cusum(kardl_model,"~/Downloads/myCusum.jpg")
#'
#' # Use your own plot utilizing ggplot
#' library(magrittr)
#'
#' myCusum<- imf_example_data %>% kardl(CPI~ER+PPI+asym(ER)+trend,mode=c(1,2,3,0)) %>% cusum( )
#'  myCusum
#' library(ggplot2)
#' ggplot(myCusum$dataframe, aes(x = X, y = Y)) +
#'   geom_line(color = "blue") +
#'   geom_line(aes(y = Lower), color = "red") +
#' geom_line(aes(y = Upper), color = "red") +
#' geom_hline(yintercept = 0, linetype = "dashed") +
#' ggtitle("CUSUM Test") +
#' xlab("") +
#' ylab("") +
#' ylim(range(myCusum$dataframe$Lower, myCusum$dataframe$Upper)) +
#' scale_color_manual(values = c("blue", "red")) +
#' guides(color = guide_legend(override.aes = list(linetype = c(1, 1)))) +
#'   theme(
#'     legend.position = "topleft",
#'     legend.title = element_blank(),
#'     plot.margin = margin(5, 4, 4, 1, "pt"),
#'     legend.background = element_blank(),
#'     legend.box.background = element_blank(),
#'     legend.key = element_blank(),
#'     plot.title = element_text(hjust = 0.5)
#'   )
cusum<-function(inputs_,saveToFile=F){

    if(inherits(inputs_,"kardl")){
      OptModel<-inputs_$finalModel$model
    }else{
      if(inherits(inputs_, "lm")){
        OptModel<-inputs_
      }else{
        stop("Not suitable input.")
      }
    }



  rece<-recresid(OptModel)
  sels<-summary(OptModel)
  bbst<-sels$coefficients[,1]
  k<-length(bbst[-1])
  n<-length(OptModel$residuals)


  if(isFALSE(isFALSE(saveToFile))){
    jpeg(saveToFile)
  }
  cu<-Kcusum(rece,k,n)

  if(isFALSE(isFALSE(saveToFile))){
    dev.off()
  }

  Karamelikli<-list(
    type="Cusum",
    Stability = ifelse(cu$karar>0,"U","S"),
    saveToFile=saveToFile,
    dataframe=cu$Cu,
    karar=cu$karar,
    method = "Cusum"
  )
  class(Karamelikli) <- "kardl"
  Karamelikli
}
#' Plot CUSUSMQ
#'
#' This function generates a plot for the Cumulative Sum of Squares (CUSUMQ) test, which is used to assess the stability of variance in regression models over time. The CUSUMQ test examines the cumulative sum of squared recursive residuals and plots these against time. If the plotted line remains within the confidence bands, the variance of the model is stable. However, if the line crosses the boundaries, it signals instability in the variance structure, indicating possible structural breaks or volatility changes in the data.
#' @param inputs_ the model
#' @param saveToFile File name with its folders.
#'
#' @return plot
#'
#' @seealso \code{\link{cusum}}
#' @export
#'
#' @examples
#'
#' # For lm estimations
#'
#' MyLM <-lm(CPI~ER+PPI,imf_example_data)
#' cusumq(MyLM)
#'
#' # For ARDL model
#' kardl_model<-kardl(imf_example_data,CPI~ER+PPI+asym(ER)+deterministic(covid)+trend,mode=c(1,2,3,0))
#' cDF<-cusum(kardl_model)
#' cDF
#'
#' # Print Cusum data frame
#' head(cDF$dataframe)
#'
#' # For saving as a file:
#' cusumq(kardl_model,"~/Downloads/myCusumQ.jpg")
#'
#' # Use your own plot utilizing ggplot
#' library(magrittr)
#' myCusum<- imf_example_data %>% kardl(CPI~ER+PPI+asym(ER)+trend,mode=c(1,2,3,0)) %>% cusumq()
#' myCusum
#'
#' library(ggplot2)
#' ggplot(myCusum$dataframe, aes(x = X, y = Y)) +
#'   geom_line(color = "blue") +
#'   geom_line(aes(y = Lower), color = "red") +
#' geom_line(aes(y = Upper), color = "red") +
#' geom_hline(yintercept = 0, linetype = "dashed") +
#' ggtitle("CUSUM of Squares Test") +
#' xlab("") +
#' ylab("") +
#' ylim(range(myCusum$dataframe$Lower, myCusum$dataframe$Upper)) +
#' scale_color_manual(values = c("blue", "red")) +
#' guides(color = guide_legend(override.aes = list(linetype = c(1, 1)))) +
#'   theme(
#'     legend.position = "topleft",
#'     legend.title = element_blank(),
#'     plot.margin = margin(5, 4, 4, 1, "pt"),
#'     legend.background = element_blank(),
#'     legend.box.background = element_blank(),
#'     legend.key = element_blank(),
#'     plot.title = element_text(hjust = 0.5)
#'   )
cusumq<-  function(inputs_,saveToFile=F){

    if(inherits(inputs_, "kardl")){
      OptModel<-inputs_$finalModel$model
    }else{
      if(inherits(inputs_,"lm")){
        OptModel<-inputs_
      }else{
        stop("Not suitable input.")
      }
    }


  rece<-recresid(OptModel)
  sels<-summary(OptModel)
  bbst<-sels$coefficients[,1]
  k<-length(bbst[-1])
  n<-length(OptModel$residuals)

  if(isFALSE(isFALSE(saveToFile))){
       jpeg(saveToFile)
  }
  cu<-Kcumsq(rece,k,n)

  if(isFALSE(isFALSE(saveToFile))){
    dev.off()
  }
  Karamelikli<-list(
    type="Cusum",
    Stability = ifelse(cu$karar>0,"U","S"),
    saveToFile=saveToFile,
    dataframe=cu$Cu,
    karar=cu$karar,
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
# this function took from nardl package
Kcumsq<-function(e,k,n){
  w<-as.matrix(na.omit(e))
  w=cumsum(w^2)/sum(w^2)
  m=abs(0.5*(n-k)-1) #abs to avoid negative log
  c=0.74191-0.17459*log(m)-0.26526*(1/m)+0.0029985*m-0.000010943*m^2
  w2=c+(Kseqa(k,1,length(w))-k)/(n-k)
  w1=-c+(Kseqa(k,1,length(w))-k)/(n-k)
  x<-Kseqa(k,1,length(w))
  w1<-matrix(w1,length(w1),1)
  w<-matrix(w,length(w),1)
  w2<-matrix(w2,length(w2),1)
  Cuq<-data.frame(X=c(x),Lower=w1,Y=w,Upper=w2)
  CuTest<-ifelse(Cuq$Y>Cuq$Lower & Cuq$Y<Cuq$Upper,0,1)
  karar<-max(CuTest)
  grange<-range(w1,w2)
  par(mar = c(5,4,4,1))
  plot(x,w,main="CUSUM of Squares Test",type = "l",ylim = grange,xlab ="",ylab = "",col="blue")#ylim = grange,xlab ="",ylab = "Emperical fluctuation process",
  lines(x,w1,col="red")
  lines(x,w2,col="red")
  abline(h=0,lty=2)
  legend("topleft",
         xpd = TRUE ,
         bty = "n",
         c("CUSUM of squares","5% significance"),
         lty = c(1, 1),
         cex=0.9,
         col=c("blue","red") )
  output<-list(
    Cu=Cuq,
    karar=karar
  )
  output
}

# this function took from nardl package
Kcusum<-function(e,k,n){
  w<-as.matrix(na.omit(e))
  #n<-length(e)
  w=cumsum(w/apply(w, 2, sd))
  c=0.984
  w2=Kseqa(c*sqrt(n-k),(2*c*sqrt(n-k))/length(w),length(w))
  w1=Kseqa(-c*sqrt(n-k),(-2*c*sqrt(n-k))/length(w),length(w))
  x<-Kseqa(k,1,length(w))
  w1<-matrix(w1,length(w1),1)
  w<-matrix(w,length(w),1)
  w2<-matrix(w2,length(w2),1)
  Cu<-data.frame(X=c(x),Lower=w1,Y=w,Upper=w2)
  CuTest<-ifelse(Cu$Y>Cu$Lower & Cu$Y<Cu$Upper,0,1)
  karar<-max(CuTest)
  grange<-range(w1,w2)
  par(mar = c(5,4,4,1))
  plot(x,w,main="CUSUM Test",type = "l",ylim = grange,xlab ="",ylab = "",col="blue") #ylim = grange,xlab = "",ylab = "Emperical fluctuation process",
  lines(x,w1,col="red")
  lines(x,w2,col="red")
  abline(h=0,lty=2)
  legend("topleft",#par("usr")[2],par("usr")[4],
         xpd = TRUE ,
         bty = "n",
         c("CUSUM ","5% significance"),
         lty = c(1, 1),
         cex=0.9,
         col=c("blue","red") )
  output<-list(
    Cu=Cu,
    karar=karar
  )
  output
}


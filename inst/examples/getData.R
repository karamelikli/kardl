library("imf.data")
IFS <- load_datasets("IFS")

#' install.packages("imf.data")
#' library(imf.data)
#' IFS <- load_datasets("IFS")
#' # PCPI_IX             Prices, Consumer Price Index, All items, Index
#' # AIP_IX              Economic Activity, Industrial Production, Index
#' # ENDE_XDC_USD_RATE   Exchange Rates, Domestic Currency per U.S. Dollar, End of Period, Rate
#' 
trdata<-IFS$get_series(freq = "M", ref_area = "TR", indicator = c("PCPI_IX","AIP_IX","ENDE_XDC_USD_RATE"),start_period = "1985-01",end_period = "2024-02")
 PeriodRow<-trdata[,1]
trdata[,1]<-NULL
colnames(trdata)<-c("ER","CPI","PPI")
trdata<-log(as.data.frame(lapply(trdata, function(x) as.numeric(x))))
rownames(trdata)<-PeriodRow
#' # Inserting covid dummy variable
trdata<-cbind(trdata,covid=0)
trdata[420:470,4]<-1
KARDL_model <-KARDL(CPI~ER+PPI+asym(ER)+deterministic(covid)+trend,trdata,userdefined=c(1,2,2,0))

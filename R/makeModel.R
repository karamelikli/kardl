
# Make model
#
# makemodel function initiate the model estimation by finding the most proper lag values or by user defined values.
# makemodel(model,data, mode=c(1,2,3,2)) will make estimation based on pre-defined lag values.
# makemodel(model,data, performance=TRUE) will trigger the performance option to save server/computer resources.
#

# @param inputs All inputs of making model.
# @param ... Other inputs
#
# @return
#
# @import stats
#' @keywords internal

makemodel<-function(inputs,...){
 # inputs<-convertListInputs(...)
  myMethod<-T
  # if(is.vector(inputs$mode))
  if(is.vector(inputs$mode[1]) && is.numeric(inputs$mode) && isFALSE(isFALSE(inputs$mode)) )  {
   class(myMethod)<- "user"
  }else{
    class(myMethod)<- match.arg(tolower(inputs$mode) ,c("quick","grid_custom","grid"))

  }

  UseMethod("makemodel",myMethod)

}


#' Model Selection Criterion
#'
#' Computes a model selection criterion (AIC, BIC, AICc, or HQ) or applies a user-defined function
#' to evaluate a statistical model.
#'
#' @param cr A character string specifying the criterion to compute.
#'           Options are \code{"AIC"}, \code{"BIC"}, \code{"AICc"}, and \code{"HQ"}. Alternatively,
#'           a user-defined function can be provided.
#' @param model An object containing the fitted model. The object should include at least:
#'              \itemize{
#'                \item \code{model$model} – the actual fitted model object (e.g., from \code{lm}, \code{glm}).
#'                \item \code{k} – the number of estimated parameters.
#'                \item \code{n} – the sample size.
#'              }
#' @param ... Additional arguments passed to the user-defined criterion function if \code{cr} is a function.
#'
#' @return A numeric value representing the selected criterion, normalized by the sample size if one of the predefined options is used.
#'
#' @details
#' This function returns model selection criteria used to compare the quality of different models.
#' All criteria are defined such that \strong{lower values indicate better models} (i.e., the goal is minimization).
#'
#' If you wish to compare models using a maximization approach (e.g., log-likelihood),
#' you can multiply the result by \code{-1}.
#'
#' Note: The predefined string options (e.g., \code{"AIC"}) are \strong{not} the same as the built-in R functions \code{AIC()} or \code{BIC()}.
#' In particular, the values returned by this function are adjusted by dividing by the sample size \code{n}
#' (i.e., normalized AIC/BIC), which makes it more comparable across datasets of different sizes.
#'
#' The function returns:
#' \itemize{
#'   \item \strong{"AIC"}:  \eqn{ \frac{2k - 2\ell}{n} } Akaike Information Criterion divided by \code{n}.
#'   \item \strong{"BIC"}:  \eqn{ \frac{\log(n) \cdot k - 2\ell}{n} } Bayesian Information Criterion divided by \code{n}.
#'   \item \strong{"AICc"}: \eqn{ \frac{2k(k+1)}{n - k - 1} + \frac{2k - 2\ell}{n} } Corrected Akaike Information Criterion divided by \code{n}.
#'   \item \strong{"HQ"}: \eqn{ \frac{2 \log(\log(n)) \cdot k - 2\ell}{n} } Hannan–Quinn Criterion divided by \code{n}.
#' }
#'
#' where:
#' \itemize{
#'   \item \eqn{k} is the number of parameters,
#'   \item \eqn{n} is the sample size,
#'   \item \eqn{\ell} is the log-likelihood of the model.
#' }
#'
#' If \code{cr} is a function, it is called with the fitted model and any additional arguments passed through \code{...}.
#' @seealso \code{\link{kardl}}
#' @examples
#' model <- list(model = lm(mpg ~ wt + hp, data = mtcars), k = 3, n = nrow(mtcars))
#' modelCriterion(AIC, model)
#' #
#' modelCriterion("AIC", model)
#' modelCriterion("BIC", model)
#'
#' # Using a custom criterion function
#' my_cr_fun <- function(mod, ...) { AIC(mod) / length(mod$model[[1]]) }
#' modelCriterion(my_cr_fun, model)
#'
#' @export
modelCriterion<-function(cr,model,...){
  if (is.function(cr)) {
    args <- list(model$model, ...)
    do.call(cr, args)
  } else {
    myCr<- match.arg( cr ,c("AIC","BIC","AICc","HQ"))
    k<-model$k
    n<-model$n
    llh<-logLik(model$model)
    val<-switch (myCr,"AIC"=((2*k-2*llh)/n),"BIC"=((log(n)*k-2*llh)/n ),"AICc"=(((2 * k * (k + 1))/(n - k - 1))+(2*k-2*llh)/n),"HQ"=((2*log(log(n))*k-2*llh)/n) )
    as.numeric( val)
  }
}


# Estimation the model by quick lags
#
#If the lags of short-run variables are determined by user, the results will be obtained with \code{userDefinedModel}
# @param inputs All inputs of making model.
# @param ... Other inputs
#
# @return

#' @export
makemodel.quick<-function(inputs , ...  ){#model,data,inputs){
  inputs$mode<-"quick"
  start_time <- Sys.time()
  # inputs<-prepare(lmerge(list( model=model,data=data),inputs))

  inputs<-prepare(inputs)
  preModel<-makeLongrunMOdel(inputs) #retruns LS_longrun   LS_dependent


  #fmodel<-paste0(LS_dependent,"~",paste(LS_longrun,makeShortrunMOdel(inputs$shortRunVars,LagsList,deterministic),sep="+"))

  Xlength<-length( inputs$shortRunVars)

  failed_checks<-matrix(NA,1,Xlength)[-1,]
  colnames(failed_checks)<- inputs$shortRunVars

  toporders<-matrix(0,1,Xlength+1)[-1,]
  colnames(toporders)<- c(inputs$shortRunVars,"criterion_value")

  #internal func
  myEst<-function(order){
    theResults<-makeEstimation(inputs$shortRunVars,order,inputs$deterministic,preModel$LS_dependent,preModel$LS_longrun,inputs$data)
    modelCriterion(inputs$criterion,theResults)
  }

  for (i in 1:inputs$maxlag) { # for each parse
    ardl_converge <- FALSE
    order1 <- rep(i, Xlength)
    Base_cr <- myEst(order1)
    toporders<-rbind(toporders,c(order1 ,Base_cr))
    while (ardl_converge == FALSE) {
      for(j in 1:(Xlength-1)) {
        order2_back <- order1
        order2_forth <- order1
        if (order1[j + 1] == 0) {
          # order2_back is the order1
          order2_forth[j + 1] <- order1[j + 1] + 1
        } else {
          if (order1[j + 1] == inputs$maxlag) {
            order2_back[j + 1] <- order1[j + 1] - 1
          }else{
            order2_back[j + 1] <- order1[j + 1] - 1
            order2_forth[j + 1] <- order1[j + 1] + 1
          }
        }
        model0 <- myEst(order2_back)
        model1 <- myEst(order2_forth)
        minCr<-min(model0,model1)
        if(Base_cr>minCr){
          if( model0 >  model1){
            order1<-order2_forth
          }else{
            order1<- order2_back
          }
          toporders<-rbind(toporders,c(order1 ,minCr))
          Base_cr<-minCr
          ardl_converge =F
        }else{
          if(any(apply(failed_checks, 1, function(row) all(row == order1)))){
            ardl_converge =T
          }else {
            failed_checks<-rbind(failed_checks,order1)
          }
        }
      }
    }
  }

  # toporders[  which.min(toporders[, "criterion_value"]),-ncol(toporders) , drop = FALSE]
  minValue<-toporders[  which.min(toporders[, "criterion_value"]), , drop = FALSE]

  best_order <- minValue[,-ncol(minValue)]
  # cnames<-colnames(best_order)

  # best_order<-as.vector(best_order)
  # names(best_order)<-cnames


  OptLag<-data.frame(c( paste0(best_order,collapse = ","),minValue[,ncol(minValue)])  )
  if(!is.function(inputs$criterion)){
    colnames(OptLag)<-inputs$criterion
  }
  rownames(OptLag)<-c("lag","value")



  theResults<-makeEstimation(inputs$shortRunVars,best_order,inputs$deterministic,preModel$LS_dependent,preModel$LS_longrun,inputs$data)
  fittedVars<-fitted(theResults$model)
  n<-theResults$n
  finalModel<-list( ModelFormula = theResults$fmodel,
                    model=theResults$model, k=theResults$k ,  n=n,  start=as.numeric(names(fittedVars[1])),
                    end=as.numeric(names(tail(fittedVars,n=1))), TimeSpan=n+inputs$maxlag+1)
  # model_tidy <- summary(theResults$model)$coefficients %>%   as_tibble(rownames = "term")
  # colnames(model_tidy) <- c("term", "estimate", "std.error", "statistic", "p.value")

  end_time <- Sys.time()

  Karamelikli<-list(
    inputs=inputs,
    finalModel=finalModel,
    start_time=start_time,
    end_time=end_time,
    properLag=best_order,
    TimeSpan=n+inputs$maxlag+1,
    OptLag=OptLag,
  LagCriteria=toporders,
    type="kardlmodel",
    method = "quick"
  )



  class(Karamelikli) <- "kardl"
  Karamelikli
}


# Estimation the model by User-defined lags
#
#If the lags of short-run variables are determined by user, the results will be obtained with \code{userDefinedModel}
# @param inputs All inputs of making model.
# @param ... Other inputs
#
# @return
#' @export
makemodel.user <-function(inputs, ...  ){#model,data,inputs){
  # inputs$mode<-"user"
  start_time <- Sys.time()
  # inputs<-prepare(lmerge(list( model=model,data=data),inputs))
  inputs<-prepare(inputs)
  preModel<-makeLongrunMOdel(inputs) #retruns LS_longrun   LS_dependent

  theResults<- makeEstimation(inputs$shortRunVars,LagsList=inputs$mode,inputs$deterministic,preModel$LS_dependent,preModel$LS_longrun,inputs$data)

  fittedVars<-fitted(theResults$model)
  MaxLag<-max(inputs$mode)
  n<-theResults$n
  finalModel<-list( ModelFormula = theResults$fmodel,
    model=theResults$model, k=theResults$k ,  n=n,  start=as.numeric(names(fittedVars[1])),
    end=as.numeric(names(tail(fittedVars,n=1))), TimeSpan=n+MaxLag+1)
  # model_tidy <- summary(theResults$model)$coefficients %>%   as_tibble(rownames = "term")
  # colnames(model_tidy) <- c("term", "estimate", "std.error", "statistic", "p.value")
  OptLag<-inputs$mode
  attr( OptLag,"source")<- NULL
  attr( OptLag,"description")<- NULL

  end_time <- Sys.time()


  Karamelikli<-list(
    inputs=inputs,
    finalModel=finalModel,
    start_time=start_time,
    end_time=end_time,
    properLag=OptLag,
    OptLag=OptLag,
    #  model_tidy=model_tidy,
    type="kardlmodel",
    method = "userdefined"
  )
  class(Karamelikli) <- "kardl"
  Karamelikli
}


# Find Optimum Lags level  by maximizing Performance
#
# To reduce server load for finding optimum lag, this function can find it.
# Notice! nothing will be print during estimations.
# @param inputs All inputs of making model.
# @param ... Other inputs
#
# @return
#
#' @export
makemodel.grid_custom<-function(inputs, ...  ){ #model ,  data,inputs  ){
  inputs$mode<-"grid_custom"
  start_time <- Sys.time()
 # inputs<-prepare(lmerge(list( model=model,data=data),inputs))
  inputs<-prepare(inputs)
 # inputs<-prepare(model=model,data=data  ,inputs)
  # cat("\r",paste0("Note: Performance mode will only have output once all estimations are finished.  Starting of estimation of the model: ",inputs$modelName))

  preModel<-makeLongrunMOdel(inputs) #retruns LS_longrun   LS_dependent
  batch<-BatchControl(inputs)
  inputs<- lmerge(inputs,batch)

  OrderForShortRun <- rep(list((inputs$maxlag-1):0),inputs$shortrunLength) # I am reversing orders due to alerting about the insufficiency of the degree of freedom.
  OrderForInd<-list((inputs$maxlag-1):1)
  GeneralOrder<-append(OrderForShortRun,OrderForInd) # p<-append(m,b)
  LagQueue<-rev(expand.grid(GeneralOrder))  # q<-expand.grid(p)

  if(length(inputs$ASvars)>0 && ! inputs$differentAsymLag) {
    i<-1 # satrt from the second var
    for (x in  inputs$independentVars){
      i<-i+1
      if((x %in% inputs$ASvars)){
        LagQueue<-cbind(LagQueue[,1:i],LagQueue[,i:ncol(LagQueue)])
        i<-i+1
      }
    }
  }
  colnames(LagQueue)<-inputs$shortRunVars
  ## Note: This part will be updated to reduce RAM requirement to keep huge list. It will be cut by the batch function's return.
  Mincr<-1000000 # max value for be compared with criteria.
  OptRow<-0 # The row with contain the most proper lag orders
  for(i in batch$startRow:batch$endRow)  {
    theResults<- makeEstimation(inputs$shortRunVars,LagsList=unlist(LagQueue[i,]),inputs$deterministic,preModel$LS_dependent,preModel$LS_longrun,inputs$data)

    cr<-modelCriterion(inputs$criterion, theResults, ...)
    # k<-theResults$k
    # n<-theResults$n
    # llh<-logLik(theResults$model)
    #
    # cr<-switch (inputs$criterion,"AIC"=((2*k-2*llh)/n),"BIC"=((log(n)*k-2*llh)/n ),"AICc"=(((2 * k * (k + 1))/(n - k - 1))+(2*k-2*llh)/n),"HQ"=((2*log(log(n))*k-2*llh)/n) )
    if(cr<Mincr){
      Mincr<-cr
      OptRow<-i
    }
  }

  endLag  <-paste(LagQueue[batch$endRow,],collapse = ",")
  startLag<-paste(LagQueue[batch$startRow,],collapse = ",")
  properRow<-1
  # OptLag<-data.frame(  LagQueue[OptRow,]
  # rownames(OptLag)<-""
  OptLag<-data.frame(c( paste0(LagQueue[OptRow,],collapse = ","),Mincr)  )
  if(!is.function(inputs$criterion)){
      colnames(OptLag)<-inputs$criterion
      }
  rownames(OptLag)<-c("lag","value")
  Mincr<-NULL

  theResults<- makeEstimation(inputs$shortRunVars,LagsList=unlist(LagQueue[OptRow,]),inputs$deterministic,preModel$LS_dependent,preModel$LS_longrun,inputs$data)
  k<-theResults$k
  n<-theResults$n
  fittedVars<-fitted(theResults$model)
  # model_tidy <- summary(theResults$model)$coefficients %>%   as_tibble(rownames = "term")
  # colnames(model_tidy) <- c("term", "estimate", "std.error", "statistic", "p.value")

  properLag <- unlist(LagQueue[OptRow,])
  MaxLag<-max( properLag)
  end_time <- Sys.time()


  finalModel<-list( ModelFormula = theResults$fmodel,
                    model=theResults$model, k=theResults$k ,  n=theResults$n,  start=as.numeric(names(fittedVars[1])),
                    end=as.numeric(names(tail(fittedVars,n=1))), TimeSpan=n+MaxLag+1)

  #cat("\rThe estimation is Finished. Please check your results.\n The collapsed time is: ", secondsToPeriod(as.numeric(difftime(end_time, start_time, units = "secs"))))
  Karamelikli<-list(
    inputs=inputs,
    finalModel=finalModel,
    start_time=start_time,
    end_time=end_time,
    properLag = properLag,
    properRow=OptRow,
    OptLag=OptLag,
    LagsFrom=startLag,
    LagsTo=endLag,
    #  model_tidy=model_tidy,
    type="kardlmodel",
    method = "performance"
  )
  class(Karamelikli) <- "kardl"
  Karamelikli
}


# Find Optimum Lags level  by visualization of Estimations
#
# Current job status and remained estimations with progress bar.
# Notice! Users should check the validity of the model and data utilizing this function.
# Appearance mode will have outputs during estimations.
#
# @param inputs All inputs of making model.
# @param ... Other inputs
#
# @return
#
#' @export
makemodel.grid<-function(inputs , ... ){#model ,  data,inputs  ){ # makemodel.default
  inputs$mode<-"grid"

  start_time <- Sys.time()
  # inputs<-prepare(lmerge(list( model=model,data=data),inputs))
  inputs<-prepare(inputs)
  # inputs<-prepare(model=model,data=data  ,inputs)
  #cat("\r",paste0("Appearance mode will have outputs during estimations. Starting of estimation of the model: ",inputs$modelName))

  preModel<-makeLongrunMOdel(inputs) #retruns LS_longrun   LS_dependent
  batch<-BatchControl(inputs)

  LagCriteria<-matrix(NA,inputs$lagRowsNumber  ,5) # rows: lag , dependent,LongrunTerms, Shortrun terms, AIC, BIC ,SC,HQ
  colnames(LagCriteria) <- c("lag" , "AIC", "BIC" ,"AICc","HQ")
  bir<-rep(rep(c(1:(inputs$maxlag-1)), each = (inputs$maxlag)^(inputs$shortrunLength)),time=1)
  for (i in 1:inputs$shortrunLength) {
    r<-rep(rep(c(0:(inputs$maxlag-1)), each = (inputs$maxlag)^(inputs$shortrunLength-i)),time=(inputs$maxlag^i -inputs$maxlag^(i-1)) )
    if(length(inputs$ASvars)>0 && (inputs$independentVars[i] %in% inputs$ASvars)){
      if(! inputs$differentAsymLag) {
        bir<-cbind(bir,r)
      }
    }
    bir<-cbind(bir,r)
  }
  colnames(bir)<-inputs$shortRunVars
  LagMatrix <-bir[nrow(bir):1,] # I am reversing orders due to alerting about the insufficiency of the degree of freedom.
  colNum=ncol(LagMatrix)
  for (i in 1:nrow(LagMatrix)){
    LagCriteria[i,1] <- paste0(LagMatrix[i,],collapse=",")
  }
  for(i in batch$startRow:batch$endRow)  {
    theResults<- makeEstimation(inputs$shortRunVars,LagsList=unlist(LagMatrix[i,]),inputs$deterministic,preModel$LS_dependent,preModel$LS_longrun,inputs$data)

    k<-theResults$k
    n<-theResults$n
    llh<-logLik(theResults$model)
    aic<- (2*k-2*llh)/n
    bic<- (log(n)*k-2*llh)/n
    hq<- (2*log(log(n))*k-2*llh)/n
    LagCriteria[i,2]<-aic
    LagCriteria[i,3]<-bic #BIC(est)
    LagCriteria[i,4]<- aic + ((2 * k * (k + 1))/(n - k - 1)) # AICc(est)
    LagCriteria[i,5]<-hq #   -2 * as.numeric(llh) + 2 * k * log(log(n))
    progressBar(i,batch$endRow,as.character(LagCriteria[i,1]))
  }
  cat("\n")
  endLag  <- LagMatrix[batch$endRow ,]# paste(LagMatrix[batch$startRow,],collapse = ",")
  startLag<- LagMatrix[batch$startRow ,]#paste(LagMatrix[batch$endRow,],collapse = ",")
  aicRow<-which(LagCriteria[,2]==min(as.numeric(LagCriteria[,2]), na.rm=T), arr.ind=T)
  bicRow<-which(LagCriteria[,3]==min(as.numeric(LagCriteria[,3]), na.rm=T), arr.ind=T)
  aiccRow<-which(LagCriteria[,4]==min(as.numeric(LagCriteria[,4]), na.rm=T), arr.ind=T)
  hqRow<-which(LagCriteria[,5]==min(as.numeric(LagCriteria[,5]), na.rm=T), arr.ind=T)
  OptLag<-data.frame("AIC"=c(LagCriteria[aicRow,1],LagCriteria[aicRow,2]),
                     "BIC"=c(LagCriteria[bicRow,1],LagCriteria[bicRow,3])   ,
                     "AICc"=c(LagCriteria[aiccRow,1],LagCriteria[aiccRow,4]),
                     "HQ"=c(LagCriteria[hqRow,1],LagCriteria[hqRow,5])
  )

  rownames(OptLag)<-c("lag","value")
  properRow<-switch (inputs$criterion,"AIC"=aicRow,"BIC"=bicRow ,"AICc"=aiccRow,"HQ"=hqRow )
  properLag <-LagMatrix[properRow,]
  theResults<- makeEstimation(inputs$shortRunVars,LagsList=unlist(properLag),inputs$deterministic,preModel$LS_dependent,preModel$LS_longrun,inputs$data)
  k<-theResults$k
  n<-theResults$n
  fittedVars<-fitted(theResults$model)
  # model_tidy <- summary(theResults$model)$coefficients %>%   as_tibble(rownames = "term")
  # colnames(model_tidy) <- c("term", "estimate", "std.error", "statistic", "p.value")


  MaxLag<-max(properLag)
  end_time<- Sys.time()
  #cat("\r The collapsed time is: ", secondsToPeriod(as.numeric(difftime(end_time, start_time, units = "secs"))))

  finalModel<-list( ModelFormula = theResults$fmodel,
                    model=theResults$model, k=theResults$k ,  n=theResults$n,  start=as.numeric(names(fittedVars[1])),
                    end=as.numeric(names(tail(fittedVars,n=1))), TimeSpan=n+MaxLag+1)

  Karamelikli<-list(
    inputs=inputs,
    finalModel=finalModel,
    start_time=start_time,
    end_time=end_time,
    properLag = LagMatrix[properRow,],
    properRow=properRow,
    Criterion=inputs$criterion,
    LagsFrom=startLag,
    LagsTo=endLag,
    TimeSpan=n+MaxLag+1,
    OptLag=OptLag,
    LagCriteria=LagCriteria,
    LagMatrix=LagMatrix,
    # model_tidy=model_tidy,
    type="kardlmodel",
    method = "appearance"
  )
  class(Karamelikli) <- "kardl"
  Karamelikli
}


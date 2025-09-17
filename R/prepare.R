

# Detect and combine asymmetric and symmetric variables into proper categories based on the entered model
#
# @param inputs All of inputs of the main function.
# This is a particular function for categorizing variables automatically.
# This function is going to exclude from exported functions after test procedure finished.
# @return list
combineVarTypes <-function(inputs){
  # checking the existence of model in gotten varaibales of the function
  if(is.null(inputs$model)){
    print("Error: The model for the model not found")
    return(NULL)
  }
  if(typeof(inputs$model) != "language"){
    print("Error: The model should be a valid model without any \" or '. For y~x+z ")
    return(NULL)
  }
  extracted<-list()
  deterministic<-parseFormula(inputs$model,"deterministic()",T)
  deterministic_<-unique(trimws(deterministic$detected));
  extracted[["deterministic"]] <- deterministic_[nzchar(deterministic_)]
  noConstant<-parseFormula(deterministic$theRest,"- 1",T)
  extracted[["noConstant"]] <-ifelse(noConstant$detected=="",F,T)
  trend<-parseFormula(noConstant$theRest,"trend",T)
  extracted[["trend"]] <-ifelse(trend$detected=="",F,T)
  DotVars<-parseFormula(trend$theRest,".",T)
  if(!is.null(DotVars$detected) && DotVars$detected=="."){
    completeVars<-colnames(inputs$data)
    completeVars<-completeVars[ ! completeVars %in% deterministic_]
  }else{
    completeVars<-c()
  }

  Allvars_<-unique(c(  trimws(c(all.vars(DotVars$theRest))),completeVars  ));
  #Allvars_<-unique(   trimws(c(all.vars(trend$theRest))));
  extracted[["Allvars"]] <-Allvars_[nzchar(Allvars_)]
  Asym<-parseFormula(trend$theRest,"asym()",T)
  AsymL<-parseFormula(Asym$theRest,"AsymL()",T)
  asymS<-parseFormula(AsymL$theRest,"asymS()",T)
  ALvars_<-unique(trimws(c(Asym$detected,AsymL$detected)))
  extracted[["ALvars"]] <-ALvars_[nzchar(ALvars_)]
  ASvars_ <-unique(trimws(c(Asym$detected,asymS$detected)))
  extracted[["ASvars"]]<-ASvars_[nzchar(ASvars_)]

  for (name in names(extracted)) {
    if(!is.null(extracted[[name]])) {
      attr(extracted[[name]], "source") <- "user_input"
      attr(extracted[[name]], "description") <-"This value was extracted from user inputs."
    }
  }

  lmerge( inputs , extracted)
  }

# User Defined lags verification
#
#  Verifying the defined lags are correct and well-defined.
# @param inputs All arguments which defined before.
# Here we need userdefined, Allvars, ASvars, AsymPrefix, and AsymSuffix arguments in inputs argument. You can define by yourself to run this function. For instance inputs$Allvars<-c("lx01", "lyru" ,"lrex" ,"lvol" ,"lx19" ,"lx25" ,"lx68")
#
# @return Proper version of defined vector
verifyUserDefinedLags<-function(inputs){

  if(typeof(inputs$mode)!= "double"){
    stop("Error: User-defined value should have valid vector. For example: c(1,0,1)")
  }
  if(all(inputs$mode==floor(inputs$mode)) != TRUE){
    stop(paste0("Error: User-defined should have valid numeric and non-decimal. Your pattern is ",paste(inputs$mode,collapse = ","),". For example: 1,0,1 "))
  }


  nlist<-c()
  j<-0
  for (i in 1:length(inputs$Allvars)) {
    if(inputs$Allvars[i] %in% inputs$ASvars){
      nlist[i+j]<-paste0(.kardl_Settings_env$AsymPrefix[1],inputs$Allvars[i],.kardl_Settings_env$AsymSuffix[1])
      j=j+1
      nlist[i+j]<-paste0(.kardl_Settings_env$AsymPrefix[2],inputs$Allvars[i],.kardl_Settings_env$AsymSuffix[2])
    }else{
      nlist[i+j]<- inputs$Allvars[i]
    }
  }
  if(length(inputs$mode)!=length(inputs$Allvars )+length(inputs$ASvars)){
    stop(paste0("Error: User-defined should match with short-run variables. User defined lags vector must has exactly ",length(inputs$Allvars )+length(inputs$ASvars)," element. Your pattern is ",paste(inputs$mode,collapse = ","),". Please define by this order: ",paste(nlist,collapse = ",")))
  }

  # If user defines by her order, the order should be redefined
  if (!is.null(names(inputs$mode))) {
    yenisi <- c()
    for (i in 1:length(nlist)) {
      yenisi[nlist[i]] <- inputs$mode[nlist[i]]
    }
    inputs$mode <- yenisi
  }else{
    names(inputs$mode) <- nlist
  }

  if(inputs$mode[1]<1){
    stop(paste0("Error: User-defined should start with a digit greater than zero. Your pattern is ",paste(inputs$mode,collapse = ","),". For example: 1,0,1 "))
  }
  if(all(inputs$mode>=0)!=T){
    stop(paste0("Error: User-defined should containt only positive values. Your pattern is ",paste(inputs$mode,collapse = ","),". For example: 1,0,1 "))
  }


  inputs$maxlag<-max(inputs$mode)

  inputs
}

# Check all arguments
#
# This function checks the inputs' format and validities.
# @param inputs
#
# @return npthing. Stop the rest of the codes to be executed if there is any input problem.

CheckInputs<-function(inputs){
  if(is.null(inputs$model)){
    stop("model is missing! Please define the model like as: model=y~x+z")
  }
  if(typeof(inputs$model)!="language"){
    stop("The model is not defined properly.")
  }
  if(is.null(inputs$data)){
    stop("The data is missing! Please select related data. data=data")
  }
  # if(typeof(inputs$dataTimeSeriesStart) != "double" || ! is.numeric(inputs$dataTimeSeriesStart)){
  #   stop("dataTimeSeriesStart should be a valid  time series start point. For example c(2010,3) ")
  # }

  # if(typeof(inputs$sig) != "character" || !inputs$sig %in% c("0.01" , "0.05" , "0.10")){
  #   stop(paste0("Error: Significance level should be a valid significant level. The valid levels here are  0.01, 0.05, 0.10."))
  # }

  if(!is.function(inputs$criterion) ){
        if(typeof(inputs$criterion) != "character" && !inputs$criterion %in% c("AIC","BIC","AICc","HQ")  ){
          stop(paste0("Error: The entered criterion should be a function or one of the defined criteria here. The defined criteria are  AIC , BIC , AICc , HQ "))
        }
  }

  # if(typeof(inputs$trend)!= "logical"){
  #   stop(paste0("Error: The entered trend should be a logical value. TRUE/FALSE ."))
  # }
  if(typeof(inputs$differentAsymLag)!= "logical"){
    stop(paste0("Error: The entered DifferentAsymLag should be a logical value. TRUE/FALSE ."))
  }
  if(!all(inputs$mode %in% c("grid_custom","grid","quick") )){
     # The predefined option for user-defined is false. Then here, we check just if it is not false.

    inputs<-verifyUserDefinedLags(inputs) # store deined names

  }
  # if(typeof(inputs$makeCusumPlots)!= "logical"){
  #   stop(paste0("Error: The entered makeCusumPlots should be a logical value. TRUE/FALSE ."))
  # }

    if (!is.null(inputs$batch) & !grepl("^\\d+/\\d+$", inputs$batch)) {
      stop("Invalid batch format. Use 'x/y', where x is the batch number and y is the total number of batches.")
    }
  # Check whether are digit or not
  if(!grepl("^([1-9])[0-9]*$", inputs$maxlag, perl = T)){
    stop(paste0("Error: The entered maxlag should be a digit. You entered: ",inputs$maxlag))
  }
  # if(!grepl("^([1-9])[0-9]*$", inputs$dataTimeSeriesFrequency, perl = T)){
  #   stop(paste0("Error: The entered dataTimeSeriesFrequency should be a digit."))
  # }

  inputs # return inputs, If there are changes in inputs, it should be returned as a new formed inputs list.
}



# Checking defined var in the dataset
#
# Detect variables specified in the model and their availability
# Make the variables list and checking their existence in the data set.
#
# @param data the data
# @param inputs All arguments which defined before.
# Here we need Allvars, ALvars, ASvars, and deterministic arguments in inputs argument. You can define by yourself to run this function. For instance inputs$Allvars<-c("lx01", "lyru" ,"lrex" ,"lvol" ,"lx19" ,"lx25" ,"lx68")
#
#
# @return a list of variables including  dependent variable, independent variables, all asymmetric vars, independent variables excluding short-run asymmetric vars,
# independent variables excluding long-run asymmetric vars, method of the model (SS: symmetry in short and long run, AS: Asymmetry in short run but Symmetry in long run,  SA: Symmetry in short run but Asymmetry in long run, AA: Asymmetry in short run and long run  )

detectVars <-function(inputs){
  dependentVar <-inputs$Allvars[1]
  independentVars <- inputs$Allvars[-1] # all variables except the first one
  AllAsymVars<-unique(c(inputs$ALvars,inputs$ASvars))
  if(length(inputs$ASvars)>0){
    for (x in inputs$ASvars ){
      if(!(x %in% independentVars )){stop(paste("Attention! The Short-run asymmetric variable: ", (x), " not found in the main vars list!"))}
    }
  }
  if(length(inputs$ALvars)>0){
    for (x in inputs$ALvars ){
      if(!(x %in% independentVars)){stop(paste("Attention! The Long-run asymmetric variable: ", (x), " not found in the main vars list!"))}
    }
  }
  for (x in inputs$Allvars ){
    if(!(x %in% colnames(inputs$data))){stop(paste("Attention! The variable: ", (x), " not found in the data file's vars list!"))}
  }
  if(length(inputs$deterministic)>0){
    for (x in inputs$deterministic ){
      if((x %in% inputs$Allvars)){stop(paste("Attention! The external variable: ", (x), "  FOUND in the main vars list! The exegenious variables should be excluded from the main list"))}
      if(!(x %in% colnames(inputs$data))){stop(paste("Attention! The variable: ", (x), " not found in the data file's vars list!"))}
    }
  }

  baslik<-c(dependentVar)
  baslik2<-c(dependentVar)
  for (x in  independentVars){
    if(!(x %in% inputs$ASvars)){
      baslik<-c(baslik,x)
    }else{
      ## c(paste0(AsymPrefix[1],i,inputs$AsymSuffix[1])
      ##  baslik<-c(baslik,paste0(x,".NEG"));baslik<-c(baslik,paste0(x,".POS"));
      baslik<-c(baslik,paste0(.kardl_Settings_env$AsymPrefix[1],x,.kardl_Settings_env$AsymSuffix[1]));
      baslik<-c(baslik,paste0(.kardl_Settings_env$AsymPrefix[2],x,.kardl_Settings_env$AsymSuffix[2]));
      #   if(DifferentAsymLag) {} else baslik<-c(baslik,x)
    }
    if(!(x %in% inputs$ALvars)){
      baslik2<-c(baslik2,x)
    }else{
      baslik2<-c(baslik2,paste0(.kardl_Settings_env$AsymPrefix[1],x,.kardl_Settings_env$AsymSuffix[1]));baslik2<-c(baslik2,paste0(.kardl_Settings_env$AsymPrefix[2],x,.kardl_Settings_env$AsymSuffix[2]));
    }
  }
  shortRunVars<-baslik
  longRunVars<-baslik2

  # independent variables short-run asymmetric excluded
  indepASexcluded<-independentVars[! independentVars %in% inputs$ASvars]
  # independent variables long-run asymmetric excluded
  indepALexcluded<-independentVars[! independentVars %in% inputs$ALvars]
  method<-""
  if(length( inputs$ALvars)>0 & length( inputs$ASvars)>0 ){  method<-"AA"}
  else if(length( inputs$ALvars)>0 ){method<-"SA"}
  else if(length( inputs$ASvars)>0 ){method<-"AS"}
  else {method<-"SS"}

  # calculating the column numbers for each row
  shortrunLength<-length(indepASexcluded)+length(inputs$ASvars)*(if(inputs$differentAsymLag) 2 else 1)
  # calculating the row numbers for all of possibilities of lags
  lagRowsNumber <- (inputs$maxlag^(shortrunLength+1))-(inputs$maxlag^(shortrunLength))

  extracted<-list(
    dependentVar=dependentVar,
    independentVars=independentVars,
    AllAsymVars=AllAsymVars,
    indepASexcluded=indepASexcluded,
    indepALexcluded=indepALexcluded,
    shortRunVars=shortRunVars,
    longRunVars=longRunVars,
    shortrunLength=shortrunLength,
    lagRowsNumber=lagRowsNumber,
    modelType=method
    # desc= "Make the varibles list and checking their existence in the data set"
  )

  for (name in names(extracted)) {
    if(!is.null(extracted[[name]])) {
      attr(extracted[[name]], "source") <- "user_input"
      attr(extracted[[name]], "description") <-"This value was extracted from user inputs."
    }
  }

  lmerge( inputs , extracted)

}




# Create new data set with lagged vars and asymmetric variables
# Here all of possible variables produced to minimize server load for reproduction during estimations.
#
# @param data  Time series data
# @param inputs All arguments which defined before. Containing AsymPrefix, AsymSuffix, ASvars, ALvars, maxlag, trend
# @param variables All variables defined before. Containing  dependentVar, indepASexcluded, indepALexcluded
# @return data

CreateNewVars <-function( inputs){
  # Create and adding asymmetric variables to the data
  for (x in  inputs$AllAsymVars ){
    posName<-paste0(.kardl_Settings_env$AsymPrefix[1],x,.kardl_Settings_env$AsymSuffix[1]) ### posName<- paste0(x,".POS")
    negName<-paste0(.kardl_Settings_env$AsymPrefix[2],x,.kardl_Settings_env$AsymSuffix[2]) ### negName<- paste0(x,".NEG")
    if(! posName %in% colnames(inputs$data)){
      varNames<-colnames(inputs$data)
      gecici<-diff(inputs$data[,x])
      KARAMELpos<-KARAMELneg<-c(NA)
      data3<-cbind(inputs$data[,1],gecici,KARAMELpos,KARAMELneg)
      pos<-data3[,"KARAMELpos"]
      neg<-data3[,"KARAMELneg"]
      gecici<-data3[,"gecici"]
      for (i in 1:nrow(inputs$data)) {
        if(is.na(gecici[i])){
          pos[i]<-neg[i]<-NA
        }
        else{
          if(  gecici[i]>0){
            pos[i]<-gecici[i]+ifelse(is.na(pos[i-1]), 0, pos[i-1])
            neg[i]<-ifelse(is.na(neg[i-1]), 0, neg[i-1])

          }
          if(  gecici[i]<0){
            neg[i]<-gecici[i]+ifelse(is.na(neg[i-1]), 0, neg[i-1])
            pos[i]<-ifelse(is.na(pos[i-1]), 0, pos[i-1])
          }
          if(  gecici[i] == 0){
            pos[i]<-pos[i-1]
            neg[i]<-neg[i-1]
          }
        }
      }
      inputs$data<-cbind(inputs$data,pos,neg)
      colnames(inputs$data)<-c(varNames,posName,negName)

    }
  }



  varAdlari<-colnames(inputs$data)
  AsShortlar<-unlist(lapply(inputs$ASvars, function(i){c(paste0(.kardl_Settings_env$AsymPrefix[1],i,.kardl_Settings_env$AsymSuffix[1]),paste0(.kardl_Settings_env$AsymPrefix[2],i,.kardl_Settings_env$AsymSuffix[2]))})) ## AsShortlar<- as.vector(outer(inputs$ASvars,c("POS","NEG"), paste, sep="."))
  shortRunVars<-c(inputs$dependentVar,inputs$indepASexcluded,AsShortlar)

  for (x in shortRunVars ){
    fark<-diff(inputs$data[,x])
    m<-fark
    for(j in 0:inputs$maxlag){
      newVarName<-replace_lag_var(.kardl_Settings_env$ShortCoef ,x,j) #paste0("L",j,".d.",x)
      if(! newVarName %in% colnames(inputs$data)){
        # y<-inputs$data[,x] # boyutunu alsın diye
        y<-c()
        y[1:(j+1)]<-NA
        y[(j+2):(length(inputs$data[,x]))]<-fark[1:(length(inputs$data[,x])-1-j)]
        y<-as.ts(y)
        inputs$data<-cbind(inputs$data,y)
        varAdlari<-c(varAdlari,newVarName)
        colnames(inputs$data)<-varAdlari
      }
    }
  }

  AsLonglar<-unlist(lapply(inputs$ALvars, function(i){c(paste0(.kardl_Settings_env$AsymPrefix[1],i,.kardl_Settings_env$AsymSuffix[1]),paste0(.kardl_Settings_env$AsymPrefix[2],i,.kardl_Settings_env$AsymSuffix[2]))}))## AsLonglar<- as.vector(outer(ALvars,c("POS","NEG"), paste, sep="."))
  longRunVars<-c(inputs$dependentVar,inputs$indepALexcluded,AsLonglar)

  for (x in longRunVars ){
    newVarName<-replace_lag_var(.kardl_Settings_env$LongCoef ,x,1) #paste0("L1.",x)
    if(! newVarName %in% colnames(inputs$data)){
      # y<-inputs$data[,x] # boyutunu alsın diye
      y<-c()
      y[1]<-NA
      y[2:length(inputs$data[,x])]<-inputs$data[,x][1:(length(inputs$data[,x])-1)]
      y<-as.ts(y)

      inputs$data<-cbind(inputs$data,y)
      varAdlari<-c(varAdlari,newVarName)
      colnames(inputs$data)<-varAdlari
    }
  }
  if(inputs$trend ){
  inputs$deterministic<-c(inputs$deterministic,"trend")
    if( ! "trend" %in% colnames(inputs$data)){
    trend1<-seq(nrow(inputs$data))
    inputs$data<-cbind(inputs$data,trend=c(trend1))
    varAdlari<-c(varAdlari,"trend")
    colnames(inputs$data)<-varAdlari
    }
  }
  inputs
}

# depriciated in ver 5.0.4
# convertListInputs<-function(...){
#   initial_<-list(...)
#   if(length(initial_)>0){
#     for (v in 1:length(initial_)) {
#       if(typeof(initial_[[v]])=="list" && all(class(initial_[[v]])=="list" )){
#         g<-initial_[[v]]
#         initial_[[v]]<-NULL
#         initial_<- lmerge(initial_,g)
#       }
#     }
#   }
#
#   initial_
# }


# Prepare the inputs
#
# Prepare all data and arguments to performing model estimation.
# @param inputs
# @return
prepare<-function(inputs){
  # inputs<-convertListInputs(...)
  inputs<-combineVarTypes(inputs) #(lmerge(initial_,defaultVars()))
  if(is.null(inputs)){
    stop("Error: The inputs are not defined properly. Please check the inputs again.")
  }

  inputs$startTime<-Sys.time()
  # here we will check whether the data is a time series or not.
  if(!is.data.frame(inputs$data)){
    inputs$data<-as.data.frame(inputs$data) # convert to data frame
  }
  inputs$data<-as.ts(inputs$data[,c(inputs$Allvars[inputs$Allvars!="trend"],inputs$deterministic)]) # excluding trend. trend is going to be defined in the data later.
  inputs<-CheckInputs(inputs ) # It captures any changes during the checking process of the inputs. Especially user-defined nominations for each variable.
  inputs<-detectVars(inputs) # dependentVar independentVars  AllAsymVars   indepASexcluded   indepALexcluded method
  inputs<-CreateNewVars( inputs)
  inputs
}

# Make estimations
#
# Make an ARDL model estimation.
# @param shortRunVars the short-run var list
# @param LagsList the list containing lags values
# @param deterministic Any external values. It means any variables that are not either in long-run or short-run vars. In other words, any variable that would be included in the model without lag.
# @param LS_dependent It is the dependent variable in ECM models. The first difference of dependent variable.
# @param LS_longrun All long-run variables with the fist differentiated forms.
# @param data The data
# @return list
makeEstimation<-function(shortRunVars,LagsList,deterministic,LS_dependent,LS_longrun,data){
  fmodel<-paste0(LS_dependent,"~",paste(LS_longrun,makeShortrunMOdel(shortRunVars,LagsList,deterministic),sep="+"))
  model0<-lm(as.formula(fmodel),data)
  k<-length(model0$coefficients) #k
  n<-length(model0$residuals)  # T
  list(fmodel=fmodel, model=model0, n=n,k=k)
}

# Make short-run formula
#
# This function provides the short-run part of ECM model using desired lag lenght
# @param shortRunVars All short-run variables
# @param LagsList The lag list
# @param deterministic External variables
# @return model
makeShortrunMOdel<-function(shortRunVars,LagsList, deterministic){
  modd1<-c()
  for (j in 1:length(shortRunVars)){ ## to enhancing the performance it can be used shortRunVarsLength instead of length(shortRunVars) to avoiding recalcuting length at each loop.
    start<-ifelse(j<2,1,0)
    # modd1<-c(modd1,lapply(start:LagsList[j], function(i,y){sprintf(paste0("L%d.d.",y),i)},y=inputs$shortRunVars[j]))
    # Using for loop has better performance than lapply
    for (k in start:LagsList[j]) {
      modd1<-c(modd1,replace_lag_var(.kardl_Settings_env$ShortCoef,shortRunVars[j],k) #paste0("L",k,".d.",shortRunVars[j])
               )
    }
  }
  paste(c(modd1,deterministic),collapse="+")
}

# Make long-run formula
#
# @param inputs the inputs parameter should be a list including longRunVars and dependentVar
# @return string
makeLongrunMOdel<-function(inputs){
  if(!is.null(inputs$longRunPart  )){
    LS_longrun<-inputs$longRunPart
  }else{
      modd<-lapply(inputs$longRunVars,function(i){ replace_lag_var(.kardl_Settings_env$LongCoef ,i,1) # paste0("L1.",i)
        } )
      LS_longrun<-paste(modd, collapse = "+")
      if(inputs$noConstant){
        LS_longrun<-paste0(LS_longrun,"-1")
      }

  }


  LS_dependent<- replace_lag_var(.kardl_Settings_env$ShortCoef,inputs$dependentVar,0) # paste0("L0.d.",inputs$dependentVar)
  list(LS_longrun=LS_longrun,
       LS_dependent=LS_dependent
  )
}



combineVarTypes <-function(inputs,checkInData=TRUE){

  # checking the existence of model in gotten varaibales of the function
  if(is.null(inputs$formula)){
    #  stop("The model for the model not found! Please define the model like as: model=y~x+z")
    stop("The model is missing! Please define the model like as: model=y~x+z", call. = FALSE)
  }
  if(typeof(inputs$formula) != "language"){
    stop("The model should be a valid model without any \" or '. For y~x+z ", call. = FALSE)
  }
  extractedInfo<-list()
  parseFormula <-   parse_formula_vars(inputs$formula)
  extractedInfo[["noConstant"]] <- !parseFormula$intercept
  extractedInfo[["trend"]] <- "trend" %in% parseFormula$outside


  choices <- c("asymmetric","sasymmetric","lasymmetric","deterministic")
  names(parseFormula$inside) <- tolower( names(parseFormula$inside))
  matched <- vapply(
    names(parseFormula$inside),
    match.arg,
    character(1),
    choices = choices
  )
  allofInsides<-list()
  insideNames<-names(matched)
  for (variable in insideNames) {
    allofInsides[[matched[[variable]]]]<-parseFormula$inside[[variable]]
  }
  extractedInfo$ALvars <-unique(trimws(c(allofInsides$lasymmetric,allofInsides$asymmetric)))
  extractedInfo$ASvars<-unique(trimws(c(allofInsides$sasymmetric,allofInsides$asymmetric)))
  extractedInfo$deterministic <-unique(trimws(allofInsides$deterministic))

 extractedInfo$dependentVar <-parseFormula$response
  if(parseFormula$dot){
    completeVars<-colnames(inputs$data)
    completeVars<-completeVars[ ! completeVars %in% c("trend")]
    extractedInfo$independentVars<-setdiff(unique(completeVars),c(extractedInfo$dependentVar,extractedInfo$deterministic))
  }else{
    setdiff(unique(c(
    parseFormula$outside,
    unlist(parseFormula$inside, use.names = FALSE)
    )), c("trend",extractedInfo$deterministic) )-> extractedInfo$independentVars
    }



  extractedInfo$Allvars<-c( extractedInfo$dependentVar, extractedInfo$independentVars)

  # stop if any of Allvars is not in the data
  if(length(extractedInfo[["Allvars"]])==0){
    stop("No variable found in the model! Please check the model again.", call. = FALSE)
  }
  if(checkInData){
    for (x in extractedInfo[["Allvars"]]) {
      if(!(x %in% colnames(inputs$data))){
        stop("The variable: ", x, " not found in the data file's vars list!", call. = FALSE)
      }
    }
  }

  attr(extractedInfo, "source") <- "extractedInfo"
  attr(extractedInfo, "description") <-"This value was extractedInfo from user inputs."


  list( argsInfo=inputs,settings= kardl_get(c("AsymPrefix","AsymSuffix","LongCoef","ShortCoef")), extractedInfo=extractedInfo)
}

verifyUserDefinedLags<-function(spec){

  if(typeof(spec$argsInfo$mode)!= "double"){
    stop("User-defined value should have valid vector. For example: c(1,0,1)", call. = FALSE)
  }
  if(!all(spec$argsInfo$mode==floor(spec$argsInfo$mode)) ){
    stop("User-defined should have valid numeric and non-decimal. Your pattern is ", paste(spec$argsInfo$mode, collapse = ","), ". For example: 1,0,1 ", call. = FALSE)
  }


  nlist<-c()
  j<-0
  for (i in seq_along(spec$extractedInfo$Allvars)) {
    if(spec$extractedInfo$Allvars[i] %in% spec$extractedInfo$ASvars){
      nlist[i+j]<-paste0(.kardl_Settings_env$AsymPrefix[1],spec$extractedInfo$Allvars[i],.kardl_Settings_env$AsymSuffix[1])
      j=j+1
      nlist[i+j]<-paste0(.kardl_Settings_env$AsymPrefix[2],spec$extractedInfo$Allvars[i],.kardl_Settings_env$AsymSuffix[2])
    }else{
      nlist[i+j]<- spec$extractedInfo$Allvars[i]
    }
  }
  if(length(spec$argsInfo$mode)!=length(spec$extractedInfo$Allvars )+length(spec$extractedInfo$ASvars)){
    stop("User-defined should match with short-run variables. User defined lags vector must has exactly ",
         length(spec$extractedInfo$Allvars) + length(spec$extractedInfo$ASvars),
         " element. Your pattern is ", paste(spec$argsInfo$mode, collapse = ","),
         ". Please define by this order: ", paste(nlist, collapse = ","), call. = FALSE)
  }

  # If user defines by her order, the order should be redefined
  if (!is.null(names(spec$argsInfo$mode))) {
    yenisi <- c()
    for (i in seq_along(nlist)) {
      yenisi[nlist[i]] <- spec$argsInfo$mode[nlist[i]]
    }
    spec$argsInfo$mode <- yenisi
  }else{
    names(spec$argsInfo$mode) <- nlist
  }

  if(spec$argsInfo$mode[1]<1){
    stop("User-defined should start with a digit greater than zero. Your pattern is ", paste(spec$argsInfo$mode, collapse = ","), ". For example: 1,0,1 ", call. = FALSE)
  }
  if(all(spec$argsInfo$mode>=0)!=T){
    stop("User-defined should containt only positive values. Your pattern is ", paste(spec$argsInfo$mode, collapse = ","), ". For example: 1,0,1 ", call. = FALSE)
  }


  spec$argsInfo$maxlag<-max(spec$argsInfo$mode)

  spec
}


CheckInputs<-function(spec){
  if(is.null(spec$argsInfo$formula)){
    stop("model is missing! Please define the model like as: model=y~x+z", call. = FALSE)
  }
  if(typeof(spec$argsInfo$formula)!="language"){
    stop("The model is not defined properly.", call. = FALSE)
  }
  if(is.null(spec$argsInfo$data)){
    stop("The data is missing! Please select related data. data=data", call. = FALSE)
  }
  # if(typeof(inputs$dataTimeSeriesStart) != "double" || ! is.numeric(inputs$dataTimeSeriesStart)){
  #   stop("dataTimeSeriesStart should be a valid  time series start point. For example c(2010,3) ")
  # }

  # if(typeof(inputs$sig) != "character" || !inputs$sig %in% c("0.01" , "0.05" , "0.10")){
  #   stop(paste0("Error: Significance level should be a valid significant level. The valid levels here are  0.01, 0.05, 0.10."))
  # }

  if(!is.function(spec$argsInfo$criterion) ){
    if(typeof(spec$argsInfo$criterion) != "character" && !spec$argsInfo$criterion %in% c("AIC","BIC","AICc","HQ")  ){
      stop("The entered criterion should be a function or one of the defined criteria here. The defined criteria are  AIC , BIC , AICc , HQ ", call. = FALSE)
    }
  }

  # if(typeof(spec$extractedInfo$trend)!= "logical"){
  #   stop(paste0("Error: The entered trend should be a logical value. TRUE/FALSE ."))
  # }
  if(typeof(spec$argsInfo$differentAsymLag)!= "logical"){
    stop("The entered DifferentAsymLag should be a logical value. TRUE/FALSE .", call. = FALSE)
  }
  if(!all(spec$argsInfo$mode %in% c("grid_custom","grid","quick") )){
    # The predefined option for user-defined is false. Then here, we check just if it is not false.

    spec<-verifyUserDefinedLags(spec) # store deined names

  }
  # if(typeof(inputs$makeCusumPlots)!= "logical"){
  #   stop(paste0("Error: The entered makeCusumPlots should be a logical value. TRUE/FALSE ."))
  # }

  if (!is.null(spec$argsInfo$batch) & !grepl("^\\d+/\\d+$", spec$argsInfo$batch)) {
    stop("Invalid batch format. Use 'x/y', where x is the batch number and y is the total number of batches.", call. = FALSE)
  }
  # Check whether are digit or not
  if(!grepl("^([1-9])[0-9]*$", spec$argsInfo$maxlag, perl = T)){
    stop("The entered maxlag should be a digit. You entered: ", spec$argsInfo$maxlag, call. = FALSE)
  }
  # if(!grepl("^([1-9])[0-9]*$", inputs$dataTimeSeriesFrequency, perl = T)){
  #   stop(paste0("Error: The entered dataTimeSeriesFrequency should be a digit."))
  # }

  spec # return inputs, If there are changes in inputs, it should be returned as a new formed inputs list.
}


detectVars <-function(spec){

  AllAsymVars<-unique(c(spec$extractedInfo$ALvars,spec$extractedInfo$ASvars))
  if(length(spec$extractedInfo$ASvars)>0){
    for (x in spec$extractedInfo$ASvars ){
      if(!(x %in% spec$extractedInfo$independentVars )){stop("Attention! The Short-run asymmetric variable: ", x, " not found in the main vars list!", call. = FALSE)}
    }
  }
  if(length(spec$extractedInfo$ALvars)>0){
    for (x in spec$extractedInfo$ALvars ){
      if(!(x %in% spec$extractedInfo$independentVars)){stop("Attention! The Long-run asymmetric variable: ", x, " not found in the main vars list!", call. = FALSE)}
    }
  }
  for (x in spec$extractedInfo$Allvars ){
    if(!(x %in% colnames(spec$argsInfo$data))){stop("Attention! The variable: ", x, " not found in the data file's vars list!", call. = FALSE)}
  }
  if(length(spec$extractedInfo$deterministic)>0){
    for (x in spec$extractedInfo$deterministic ){
      if((x %in% spec$extractedInfo$Allvars)){stop("Attention! The external variable: ", x, "  FOUND in the main vars list! The exegenious variables should be excluded from the main list", call. = FALSE)}
      if(!(x %in% colnames(spec$argsInfo$data))){stop("Attention! The variable: ", x, " not found in the data file's vars list!", call. = FALSE)}
    }
  }

  baslik<-c(spec$extractedInfo$dependentVar)
  baslik2<-c(spec$extractedInfo$dependentVar)
  for (x in  spec$extractedInfo$independentVars){
    if(!(x %in% spec$extractedInfo$ASvars)){
      baslik<-c(baslik,x)
    }else{
      ## c(paste0(AsymPrefix[1],i,inputs$AsymSuffix[1])
      ##  baslik<-c(baslik,paste0(x,".NEG"));baslik<-c(baslik,paste0(x,".POS"));
      baslik<-c(baslik,paste0(.kardl_Settings_env$AsymPrefix[1],x,.kardl_Settings_env$AsymSuffix[1]))
      baslik<-c(baslik,paste0(.kardl_Settings_env$AsymPrefix[2],x,.kardl_Settings_env$AsymSuffix[2]))
      #   if(DifferentAsymLag) {} else baslik<-c(baslik,x)
    }
    if(!(x %in% spec$extractedInfo$ALvars)){
      baslik2<-c(baslik2,x)
    }else{
      baslik2<-c(baslik2,paste0(.kardl_Settings_env$AsymPrefix[1],x,.kardl_Settings_env$AsymSuffix[1]))
      baslik2<-c(baslik2,paste0(.kardl_Settings_env$AsymPrefix[2],x,.kardl_Settings_env$AsymSuffix[2]))
    }
  }
  shortRunVars<-baslik
  longRunVars<-baslik2

  # independent variables short-run asymmetric excluded
  indepASexcluded<-spec$extractedInfo$independentVars[! spec$extractedInfo$independentVars %in% spec$extractedInfo$ASvars]
  # independent variables long-run asymmetric excluded
  indepALexcluded<-spec$extractedInfo$independentVars[! spec$extractedInfo$independentVars %in% spec$extractedInfo$ALvars]
  method<-""
  if(length( spec$extractedInfo$ALvars)>0 & length( spec$extractedInfo$ASvars)>0 ){  method<-"AA"}
  else if(length( spec$extractedInfo$ALvars)>0 ){method<-"SA"}
  else if(length( spec$extractedInfo$ASvars)>0 ){method<-"AS"}
  else {method<-"SS"}

  # calculating the column numbers for each row
  shortrunLength<-length(indepASexcluded)+length(spec$extractedInfo$ASvars)*(if(spec$argsInfo$differentAsymLag) 2 else 1)
  # calculating the row numbers for all of possibilities of lags
  lagRowsNumber <- (spec$argsInfo$maxlag^(shortrunLength+1))-(spec$argsInfo$maxlag^(shortrunLength))

  extractedInfo<-list(
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



  spec$extractedInfo <- lmerge( spec$extractedInfo , extractedInfo)
  spec
}


CreateNewVars <-function( spec){
  # Create and adding asymmetric variables to the data
  tempData<-spec$argsInfo$data
  for (x in  spec$extractedInfo$AllAsymVars ){
    posName<-paste0(.kardl_Settings_env$AsymPrefix[1],x,.kardl_Settings_env$AsymSuffix[1]) ### posName<- paste0(x,".POS")
    negName<-paste0(.kardl_Settings_env$AsymPrefix[2],x,.kardl_Settings_env$AsymSuffix[2]) ### negName<- paste0(x,".NEG")
    if(! posName %in% colnames(tempData)){
      varNames<-colnames(tempData)
      gecici<-diff(tempData[,x])
      KARAMELpos<-KARAMELneg<-c(NA)
      data3<-cbind(tempData[,1],gecici,KARAMELpos,KARAMELneg)
      pos<-data3[,"KARAMELpos"]
      neg<-data3[,"KARAMELneg"]
      gecici<-data3[,"gecici"]
      for (i in seq_len(nrow(tempData))) {
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
      tempData<-cbind(tempData,pos,neg)
      colnames(tempData)<-c(varNames,posName,negName)

    }
  }



  varAdlari<-colnames(tempData)
  AsShortlar<-unlist(lapply(spec$extractedInfo$ASvars, function(i){c(paste0(.kardl_Settings_env$AsymPrefix[1],i,.kardl_Settings_env$AsymSuffix[1]),paste0(.kardl_Settings_env$AsymPrefix[2],i,.kardl_Settings_env$AsymSuffix[2]))})) ## AsShortlar<- as.vector(outer(spec$extractedInfo$ASvars,c("POS","NEG"), paste, sep="."))
  shortRunVars<-c(spec$extractedInfo$dependentVar,spec$extractedInfo$indepASexcluded,AsShortlar)

  for (x in shortRunVars ){
    fark<-diff(tempData[,x])
    m<-fark
    for(j in 0:spec$argsInfo$maxlag){
      newVarName<-replace_lag_var(.kardl_Settings_env$ShortCoef ,x,j) #paste0("L",j,".d.",x)
      if(! newVarName %in% colnames(tempData)){
        # y<-inputs$data[,x] # boyutunu alsın diye
        y<-c()
        y[1:(j+1)]<-NA
        y[(j+2):(length(tempData[,x]))]<-fark[1:(length(tempData[,x])-1-j)]
        y<-as.ts(y)
        tempData<-cbind(tempData,y)
        varAdlari<-c(varAdlari,newVarName)
        colnames(tempData)<-varAdlari
      }
    }
  }

  AsLonglar<-unlist(lapply(spec$extractedInfo$ALvars, function(i){c(paste0(.kardl_Settings_env$AsymPrefix[1],i,.kardl_Settings_env$AsymSuffix[1]),paste0(.kardl_Settings_env$AsymPrefix[2],i,.kardl_Settings_env$AsymSuffix[2]))}))## AsLonglar<- as.vector(outer(ALvars,c("POS","NEG"), paste, sep="."))
  longRunVars<-c(spec$extractedInfo$dependentVar,spec$extractedInfo$indepALexcluded,AsLonglar)

  for (x in longRunVars ){
    newVarName<-replace_lag_var(.kardl_Settings_env$LongCoef ,x,1) #paste0("L1.",x)
    if(! newVarName %in% colnames(tempData)){
      # y<-inputs$data[,x] # boyutunu alsın diye
      y<-c()
      y[1]<-NA
      y[2:length(tempData[,x])]<-tempData[,x][1:(length(tempData[,x])-1)]
      y<-as.ts(y)

      tempData<-cbind(tempData,y)
      varAdlari<-c(varAdlari,newVarName)
      colnames(tempData)<-varAdlari
    }
  }
  if(spec$extractedInfo$trend ){
    spec$extractedInfo$deterministic<-c(spec$extractedInfo$deterministic,"trend")
    if( ! "trend" %in% colnames(tempData)){
      trend1<-seq_len(nrow(tempData))
      tempData<-cbind(tempData,trend=c(trend1))
      varAdlari<-c(varAdlari,"trend")
      colnames(tempData)<-varAdlari
    }
  }
  spec$argsInfo$data<-NULL
  spec$extractedInfo$data<-tempData
  attr(spec$extractedInfo$data, "source") <- "spec$argsInfo$data"
  attr(spec$extractedInfo$data, "description") <-"This value was created by adding new variables to the original data set and removing the variables which are not used in the model."
  spec
}

prepare<-function(inputs){
  # inputs<-convertListInputs(...)
  #estProcess <- list()
  # startTime<-Sys.time()

  spec<-combineVarTypes(inputs) #(lmerge(initial_,defaultVars()))
  #spec$timeinfo<- list(startTime=startTime)
  #spec<-lmerge(spec, estProcess) # in case of any changes in inputs during combineVarTypes function
  ####################################### BURDA
  if(is.null(spec)){
    stop("The inputs are not defined properly. Please check the inputs again.", call. = FALSE)
  }

  # here we will check whether the data is a time series or not.
  if(!is.data.frame(spec$argsInfo$data)){
    spec$argsInfo$data<-as.data.frame(spec$argsInfo$data) # convert to data frame
  }
  spec$argsInfo$data<-as.ts(spec$argsInfo$data[,c(spec$extractedInfo$Allvars[spec$extractedInfo$Allvars!="trend"],spec$extractedInfo$deterministic)]) # excluding trend. trend is going to be defined in the data later.
  spec<-CheckInputs(spec ) # It captures any changes during the checking process of the inputs. Especially user-defined nominations for each variable.
  spec<-detectVars(spec) # dependentVar independentVars  AllAsymVars   indepASexcluded   indepALexcluded method

  spec<-CreateNewVars( spec)
  spec
}


makeShortrunMOdel<-function(shortRunVars,LagsList, deterministic){
  modd1<-c()
  for (j in seq_along(shortRunVars)){ ## to enhancing the performance it can be used shortRunVarsLength instead of length(shortRunVars) to avoiding recalcuting length at each loop.
    start<-as.numeric(j<2)
    # modd1<-c(modd1,lapply(start:LagsList[j], function(i,y){sprintf(paste0("L%d.d.",y),i)},y=spec$extractedInfo$shortRunVars[j]))
    # Using for loop has better performance than lapply
    for (k in start:LagsList[j]) {
      modd1<-c(modd1,replace_lag_var(.kardl_Settings_env$ShortCoef,shortRunVars[j],k) #paste0("L",k,".d.",shortRunVars[j])
      )
    }
  }
  paste(c(modd1,deterministic),collapse="+")
}


makeLongrunMOdel<-function(spec){
  if(!is.null(spec$argsInfo$longRunPart  )){
    LS_longrun<-spec$argsInfo$longRunPart
  }else{
    modd<-lapply(spec$extractedInfo$longRunVars,function(i){ replace_lag_var(.kardl_Settings_env$LongCoef ,i,1) # paste0("L1.",i)
    } )
    LS_longrun<-paste(modd, collapse = "+")


  }
    if(spec$extractedInfo$noConstant){
      LS_longrun<-paste0(LS_longrun,"-1")
    }

  LS_dependent<- replace_lag_var(.kardl_Settings_env$ShortCoef,spec$extractedInfo$dependentVar,0) # paste0("L0.d.",spec$extractedInfo$dependentVar)
  list(LS_longrun=LS_longrun,
       LS_dependent=LS_dependent
  )
}


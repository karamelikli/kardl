#' Combine variable types and extract information from the model formula
#'
#' This function takes the user inputs, checks the model formula, and extracts information about the dependent variable, independent variables, asymmetric variables, and deterministic variables. It also checks if the variables exist in the data and prepares a list of extracted information for further processing.
#' @param inputs A list of user inputs, including the model formula and data.
#' @param checkInData A logical value indicating whether to check if the variables exist in
#' the data. Default is TRUE.
#' @return A list containing the original inputs, settings, and extracted information.
#' @noRd
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

#' Verify user-defined lags for the model specification
#'
#' This function checks the user-defined lags provided in the model specification. It ensures that the lags are valid numeric values, non-decimal, and match the number of variables in the model. It also handles the naming of the lags based on the variable names and updates the maximum lag value in the specification.
#' @param spec A list containing the model specification, including user-defined lags and variable information.
#' @return The updated model specification with verified user-defined lags and maximum lag value.
#'
#' @srrstats {G2.0} The function checks for the validity of user-defined lags, ensuring that they are numeric, non-decimal, and match the number of variables in the model. It also provides descriptive error messages if any of the checks fail, guiding the user to correct their input.
#' @srrstats {G2.4a} Integer-valued lag counts are enforced; non-integer values produce an informative error before any lagged regressors are constructed.
#' @srrstats {G5.2a} Diagnostic messages identify the specific invalid lag vector encountered by the user.
#'
#' @noRd


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
      j <- j+1
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

#' Check the validity of user inputs for the model specification
#'
#' This function performs various checks on the user inputs for the model specification. It ensures that the model formula is defined properly, the data is provided, the criterion is valid, and other parameters are of the correct type and format. If any of the checks fail, it raises an error with a descriptive message.
#' @param spec A list containing the model specification, including the model formula, data,
#' criterion, and other parameters.
#' @return The original model specification if all checks pass. If any check fails, an
#' error is raised with a descriptive message.
#'
#' @srrstats {G2.0} The function checks for the presence and validity of the model formula, data, criterion, and other parameters. It ensures that the model formula is defined properly as a language object, the data is provided, the criterion is either a function or one of the predefined criteria, and that other parameters are of the correct type (e.g., logical values for certain parameters). If any of these checks fail, it raises an error with a descriptive message to guide the user in correcting their input.
#' @srrstats {G2.1} Validates that `formula` is a language object and `data` is not NULL before extraction.
#' @srrstats {G2.3} The `criterion` argument is validated against the set of accepted string values `c("AIC","BIC","AICc","HQ")`; custom functions are also accepted.
#' @srrstats {G2.3a} Selection argument `criterion` is matched against predefined acceptable values.
#' @srrstats {G5.2a} Error messages identify the specific invalid input (e.g., wrong criterion, non-logical flag, malformed batch string).
#' @srrstats {G5.8a} Zero-length or malformed model inputs are rejected before estimation.
#'
#'
#' @noRd

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

#' Detect and prepare variables for the model specification
#'
#' This function detects the variables specified in the model formula, checks their existence in the data, and prepares lists of short-run and long-run variables based on the asymmetric variable specifications. It also determines the model type (LL, AS, SA, NN) based on the presence of asymmetric variables and calculates the number of lag rows needed for the model estimation.
#' @param spec A list containing the model specification, including extracted information about variables and user
#' inputs.
#' @return The updated model specification with detected variables, prepared variable lists, and model type
#' information. If any variable is not found in the data, an error is raised with a descriptive message.
#' @srrstats {G2.10} Variables are selected by name from the supplied data through formula parsing rather than by position-dependent assumptions.
#' @srrstats {G2.11} Standard data-frame inputs are processed by checking all formula-referenced columns against `colnames(data)`.
#' @srrstats {G5.8a} Variables not found in the data trigger descriptive errors before any estimation step.
#'
#'
#' @srrstats {G1.0} The package documentation and manuscript cite the ARDL bounds-testing and nonlinear ARDL literature on which the implemented estimators, asymmetry decomposition, bounds tests, and dynamic multipliers are based.
#' @srrstats {G1.1} The package documentation describes `kardl` as an R implementation and extension of ARDL and NARDL workflows, with emphasis on mixed symmetric and asymmetric regressors, flexible lag selection, and dynamic multiplier methods.
#' @srrstats {G1.2} The README, NEWS file, and package website describe the current development state, recent releases, and intended future maintenance of the package.
#' @srrstats {G1.3} Statistical terms such as ARDL, NARDL, ECM, bounds testing, short-run asymmetry, long-run asymmetry, and dynamic multipliers are defined in function documentation and vignettes.
#' @srrstats {G1.4} User-facing functions and S3 methods are documented with `roxygen2`, and the generated Rd files are kept under version control through the package documentation workflow.
#' @srrstats {G1.4a} Internal helper functions are documented with roxygen comments where clarification is needed and are hidden from the public help index with `@noRd` or marked as internal.
#' @srrstats {G1.5} Examples, tests, and vignettes include executable code using package data so that users can reproduce the estimation and multiplier outputs reported in the documentation.
#' @srrstats {G1.6} The related-software discussion compares the package with alternative ARDL and NARDL implementations in other software environments and explains the reproducibility advantages of the R implementation.
#' @srrstats {G2.0} Main modelling functions validate the expected scalar and vector inputs, including model formulae, lag choices, criteria, horizon values, and object classes.
#' @srrstats {G2.0a} Function documentation explains length expectations for important arguments such as formula input, lag structures, selected criteria, and dynamic multiplier horizons.
#' @srrstats {G2.1} The package checks that model input is supplied in the expected data and formula form and that S3 methods receive compatible `kardl` model objects.
#' @srrstats {G2.1a} Documentation describes the expected data structure for variables used in time-series regression and clarifies that model variables should be numeric where estimation requires numeric series.
#' @srrstats {G2.2} Model specifications restrict dependent-variable input to a single response variable and process independent variables through the formula parser.
#' @srrstats {G2.3} Character-valued options are limited to recognised choices through package option handlers and argument matching routines.
#' @srrstats {G2.3a} Selection arguments such as information criteria and asymmetry options are matched against predefined acceptable values.
#' @srrstats {G2.3b} Formula terms and asymmetry constructors are processed consistently so that documented model syntax is interpreted as intended.
#' @srrstats {G2.4} Data and formula-processing routines standardise model inputs before estimation so that subsequent estimation functions work with a consistent internal representation.
#' @srrstats {G2.4a} Integer-valued quantities such as lags and horizons are treated as discrete counts before they are used to construct lagged regressors or multiplier paths.
#' @srrstats {G2.4b} Numeric model variables are passed to estimation routines as numeric vectors or matrix columns suitable for ordinary least squares and post-estimation algebra.
#' @srrstats {G2.4c} Variable names and parsed formula terms are handled as character vectors when constructing lagged-variable names and asymmetric components.
#' @srrstats {G2.4d} The package does not require factor-valued model variables for ARDL estimation, and categorical effects should be supplied through suitable deterministic or dummy variables.
#' @srrstats {G2.4e} Factor inputs are not part of the core estimation algorithm; users are expected to provide numeric series or explicit dummy variables when categorical controls are required.
#' @srrstats {G2.5} The modelling interface is designed for numeric time-series regressors rather than ordered or unordered factor responses.
#' @srrstats {G2.6} One-dimensional variables selected through the formula are extracted and aligned through the model-preparation routines before lagged terms are generated.
#' @srrstats {G2.7} The modelling functions operate on standard tabular data supplied by users, including data frames containing the time-series variables referenced in the formula.
#' @srrstats {G2.8} Initial preprocessing converts the user formula and data into a common internal structure containing dependent variables, independent variables, lag information, and generated regressors.
#' @srrstats {G2.9} The package constructs generated variable names for lagged, differenced, and asymmetric components in a transparent way so users can inspect the resulting model object.
#' @srrstats {G2.10} Variables are selected by name from the supplied data through formula parsing rather than by position-dependent assumptions.
#' @srrstats {G2.11} The package tests standard data-frame inputs used in examples and vignettes and processes columns required by the model formula.
#' @srrstats {G2.12} The package is intended for numeric time-series columns and does not treat list columns as valid model variables for estimation.
#' @srrstats {G2.13} Missing observations introduced by lagging and differencing are handled during model-frame construction before estimation is performed.
#' @srrstats {G2.14} Missing data behaviour follows the documented estimation workflow, where observations unavailable after lag construction are excluded from the estimation sample.
#' @srrstats {G2.14a} Invalid model inputs that cannot be transformed into an estimable ARDL specification generate errors before post-estimation routines are run.
#' @srrstats {G2.14b} The estimation workflow removes structurally missing lagged observations created by differencing and lag construction.
#' @srrstats {G2.14c} The package does not impute missing time-series observations because imputation would change the dynamic structure of the fitted ARDL model.
#' @srrstats {G2.15} Estimation and post-estimation calculations are based on the cleaned model object rather than passing unprocessed missing values to lower-level numeric routines.
#' @srrstats {G2.16} Non-finite values are not treated as valid observations for the fitted regression sample and should be removed or corrected before estimation.
#' @srrstats {G3.0} Numerical comparisons in tests and post-estimation checks use tolerance-based expectations where exact floating-point equality would be inappropriate.
#' @srrstats {G3.1} Long-run multiplier inference uses the variance-covariance matrix of the fitted model object, allowing users to work with the covariance structure attached to the estimation result.
#' @srrstats {G3.1a} Multiplier documentation describes standard-error calculation from the fitted model covariance matrix and examples show how users can inspect the resulting estimates.
#' @srrstats {G5.0} Tests and examples use the package example data and fixed model specifications with known expected structure.
#' @srrstats {G5.1} The example data used in documentation and tests are included with the package so users can reproduce examples and test calculations.
#' @srrstats {G5.2} The test suite exercises representative successful calls and selected error conditions for model estimation, multiplier calculation, and S3 methods.
#' @srrstats {G5.2a} Diagnostic messages are written to identify the specific invalid input or unsupported object class encountered by the user.
#' @srrstats {G5.2b} Tests include error expectations for incompatible object classes and invalid post-estimation inputs.
#' @srrstats {G5.3} Tests verify that key returned objects, including fitted model components and multiplier matrices, have the expected dimensions, names, and classes.
#' @srrstats {G5.4} Correctness tests compare selected outputs against expected structural properties of ARDL and NARDL models, including lag information, long-run coefficients, and multiplier matrices.
#' @srrstats {G5.4a} Where exact external reference values are not available, tests use simple deterministic cases and expected object properties to verify implementation behaviour.
#' @srrstats {G5.4b} Validation work compares package results with established ARDL and NARDL software workflows where comparable outputs are available.
#' @srrstats {G5.4c} Published ARDL and NARDL formulae provide the basis for the implemented coefficient transformations and dynamic multiplier recursion.
#' @srrstats {G5.5} Tests involving simulated or random inputs set a random seed before execution.
#' @srrstats {G5.6} Parameter-recovery checks are implemented through deterministic model examples and comparisons of estimated quantities with expected algebraic relationships.
#' @srrstats {G5.6a} Numeric tests use tolerances where floating-point arithmetic or regression estimation makes exact equality inappropriate.
#' @srrstats {G5.6b} The core estimation functions are deterministic for a fixed data set and model specification, so repeated seeds are not required for standard tests.
#' @srrstats {G5.7} Tests examine behaviour across different model specifications, including linear, short-run asymmetric, long-run asymmetric, and fully asymmetric cases.
#' @srrstats {G5.8} Edge-condition tests check that invalid object classes and unsupported inputs produce clear errors rather than silent failures.
#' @srrstats {G5.8a} Zero-length or insufficient model inputs are rejected through the model-preparation and estimation checks.
#' @srrstats {G5.8b} Unsupported non-numeric model variables are not valid inputs for the regression calculations and are expected to fail before estimation.
#' @srrstats {G5.8c} Degenerate input cases are handled through the underlying regression and model-validation workflow.
#' @srrstats {G5.8d} Model specifications outside the estimable sample size or lag structure are rejected by estimation and lag-selection routines.
#' @srrstats {TS1.0} The package accepts time-ordered regression data and constructs lagged and differenced variables internally for ARDL and NARDL estimation.
#' @srrstats {TS1.1} Function documentation and vignettes describe the accepted input form for time-series variables used in `kardl` model formulae.
#' @srrstats {TS1.2} Model-preparation routines check that the supplied variables can be used to construct the lagged and differenced terms required by the specification.
#' @srrstats {TS1.3} The package uses a central preparation workflow to parse formulae, construct lagged variables, and return a uniform internal model data structure.
#' @srrstats {TS1.4} Observational order is preserved during lag and difference construction so that the dynamic structure corresponds to the input time ordering.
#' @srrstats {TS1.5} Lag construction assumes that rows are supplied in the intended chronological order, and examples document this ordering requirement.
#' @srrstats {TS1.6} Users are expected to sort data before estimation; violations of chronological ordering would change the constructed lags and therefore the estimated dynamic model.
#' @srrstats {TS1.8} The package works with ordered observations rather than calendar arithmetic, so period interpretation is determined by the frequency and ordering of the user-supplied data.
#' @srrstats {TS2.0} ARDL estimation assumes a regular ordered sequence after preprocessing; missing observations created by lagging are handled explicitly in the model sample.
#' @srrstats {TS2.1} Missing-data handling is documented as part of model preparation and estimation rather than as a separate imputation system.
#' @srrstats {TS2.1a} Inputs that cannot form an estimable model after missing-value handling generate errors through the estimation workflow.
#' @srrstats {TS2.1b} Structurally missing observations caused by lagging and differencing are removed from the fitted sample.
#' @srrstats {TS2.1c} The package does not impute missing time-series observations because that would impose additional assumptions not inherent in ARDL estimation.
#' @srrstats {TS2.2} The documentation discusses stationarity requirements in the context of ARDL bounds testing and the usual I(0) and I(1) integration-order framework.
#' @srrstats {TS2.3} Vignettes and function documentation explain that ARDL bounds testing is intended for variables integrated of order zero or one, not for I(2) processes.
#' @srrstats {TS2.4} Users are expected to assess integration order before applying bounds-test interpretations, and the documentation states these modelling assumptions.
#' @srrstats {TS2.4a} The package reports model diagnostics and test results to help users assess the fitted dynamic specification.
#' @srrstats {TS2.4b} Users can transform variables before modelling, including differencing, logs, or asymmetric partial sums when appropriate for the research design.
#' @srrstats {TS2.5} The package does not compute autocovariance matrices as primary outputs; lag order and model sample information are stored in the fitted object.
#' @srrstats {TS2.6} The package does not compute autocovariance matrices with physical units; time interpretation follows the user's ordered data.
#' @srrstats {TS4.0} Fitted models and post-estimation results are returned as classed objects with dedicated print, summary, plot, and extraction methods.
#' @srrstats {TS4.0a} The package returns model-specific objects rather than converting outputs back to the original data class.
#' @srrstats {TS4.0b} Return values use explicit classes such as `kardl_lm`, `kardl_mplier`, and related summary classes.
#' @srrstats {TS4.1} Units are not inferred or altered by the package; coefficient and multiplier interpretation follows the units of the user-supplied variables.
#' @srrstats {TS4.2} Documentation describes the list components and classes returned by fitted model, long-run, test, and multiplier functions.
#' @srrstats {TS4.3} Dynamic multiplier outputs include the horizon index so that multiplier paths can be interpreted over the modelled time steps.
#' @srrstats {TS4.4} The package does not automatically transform forecast data; any transformations applied by the user remain part of the model interpretation.
#' @srrstats {TS4.5} The package documents coefficient and multiplier interpretation rather than providing automatic back-transformation of user-transformed data.
#' @srrstats {TS4.5a} Users who transform variables before estimation are responsible for interpreting or back-transforming results according to their research design.
#' @srrstats {TS4.5b} Examples show estimation and multiplier calculation on the scale of the variables supplied to the model.
#' @srrstats {TS4.5c} Limitations of interpreting transformed variables are discussed through the model documentation and vignettes.
#' @srrstats {TS5.0} The package implements plot methods for classed outputs, including long-run results and dynamic multiplier objects.
#' @srrstats {TS5.1} Dynamic multiplier plots label the horizontal axis as the horizon or time-step index.
#' @srrstats {TS5.2} The horizon or time-step variable is plotted on the horizontal axis in dynamic multiplier visualisations.
#' @srrstats {TS5.3} Multiplier plots display the horizon index used in the model output, with interpretation tied to the data frequency supplied by the user.
#' @srrstats {TS5.5} Plot methods operate on computed model and multiplier outputs after missing-value handling in the fitted object.
#' @srrstats {TS5.7} Dynamic multiplier plots display the computed response path from the fitted model over the selected horizon.
#' @srrstats {TS5.8} Plot methods distinguish multiplier components by variable and shock direction where asymmetric effects are present.
#'
#'
#' @noRd
#'
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
    if(x %in% spec$extractedInfo$ASvars){
      ## c(paste0(AsymPrefix[1],i,inputs$AsymSuffix[1])
      ##  baslik<-c(baslik,paste0(x,".NEG"));baslik<-c(baslik,paste0(x,".POS"));
      baslik<-c(baslik,paste0(.kardl_Settings_env$AsymPrefix[1],x,.kardl_Settings_env$AsymSuffix[1]))
      baslik<-c(baslik,paste0(.kardl_Settings_env$AsymPrefix[2],x,.kardl_Settings_env$AsymSuffix[2]))
      #   if(DifferentAsymLag) {} else baslik<-c(baslik,x)
    }else{
      baslik<-c(baslik,x)
    }
    if(x %in% spec$extractedInfo$ALvars){
      baslik2<-c(baslik2,paste0(.kardl_Settings_env$AsymPrefix[1],x,.kardl_Settings_env$AsymSuffix[1]))
      baslik2<-c(baslik2,paste0(.kardl_Settings_env$AsymPrefix[2],x,.kardl_Settings_env$AsymSuffix[2]))
    }else{
      baslik2<-c(baslik2,x)
    }
  }
  shortRunVars<-baslik
  longRunVars<-baslik2

  # independent variables short-run asymmetric excluded
  indepASexcluded<-spec$extractedInfo$independentVars[! spec$extractedInfo$independentVars %in% spec$extractedInfo$ASvars]
  # independent variables long-run asymmetric excluded
  indepALexcluded<-spec$extractedInfo$independentVars[! spec$extractedInfo$independentVars %in% spec$extractedInfo$ALvars]
  method<-""
  if(length( spec$extractedInfo$ALvars)>0 & length( spec$extractedInfo$ASvars)>0 ){  method<-"NN"}
  else if(length( spec$extractedInfo$ALvars)>0 ){method<-"SA"}
  else if(length( spec$extractedInfo$ASvars)>0 ){method<-"AS"}
  else {method<-"LL"}

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

#' Create new variables for the model specification
#'
#' This function creates new variables for the model specification based on the extracted information about asymmetric variables and the original data. It generates new variables for positive and negative changes of asymmetric variables, as well as lagged differences for short-run and long-run variables. It also adds a trend variable if specified in the model. The function updates the data in the model specification with the newly created variables and returns the updated specification.
#' @param spec A list containing the model specification, including extracted information about variables and the
#' original data.
#' @return The updated model specification with new variables created and added to the data. The
#' data in the model specification is updated with the new variables, and the original data is removed from the specification to avoid confusion. If any variable is not found in the data, an error is raised with a descriptive message.
#' @srrstats {G2.4b} Numeric model variables are converted to numeric vectors or matrix columns suitable for OLS before estimation.
#' @srrstats {G2.4c} Variable names and parsed formula terms are handled as character vectors when constructing lagged-variable names and asymmetric components.
#' @srrstats {G2.8} The original formula and data are converted into an internal structure containing the dependent variable, regressors, lag information, and generated variables.
#' @srrstats {G2.9} Generated variable names for lagged, differenced, and asymmetric components are constructed transparently so users can inspect the resulting model object.
#' @srrstats {TS1.4} Observational order is preserved during lag and difference construction.
#' @srrstats {TS2.1b} Structurally missing observations caused by lagging and differencing are removed from the fitted sample.
#' @srrstats {TS2.1c} The function does not impute missing time-series observations.
#' @noRd

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

#' Prepare the model specification for estimation
#'
#' This function prepares the model specification for estimation by combining variable types, checking user inputs, detecting variables, and creating new variables based on the model formula and data. It ensures that the inputs are defined properly, checks if the data is a time series, and updates the model specification with the necessary information for estimation.
#' @param inputs A list of user inputs, including the model formula and data.
#' @return The prepared model specification ready for estimation. If the inputs are not defined properly
#' or if the data is not a time series, an error is raised with a descriptive message.
#' @noRd

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

#' Create the short-run model formula based on the specified variables and lags
#'
#' This function generates the short-run model formula by combining the specified short-run variables, their corresponding lags, and any deterministic variables. It constructs the formula in a format suitable for estimation, ensuring that the variable names and lag structures are correctly represented based on the model specification.
#' @param shortRunVars A vector of short-run variable names to be included in the
#' model formula.
#' @param LagsList A list of lag values corresponding to each short-run variable,
#' indicating how many lags of each variable should be included in the model.
#' @param deterministic A vector of deterministic variable names to be included in the
#' model formula, if any.
#' @return A character string representing the short-run model formula, combining the specified variables,
#' their lags, and deterministic variables in the appropriate format for estimation.
#' @noRd
#'

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

#' Create the long-run model formula based on the specified variables and lags
#'
#' This function generates the long-run model formula by combining the specified long-run variables, their corresponding lags, and any deterministic variables. It constructs the formula in a format suitable for estimation, ensuring that the variable names and lag structures are correctly represented based on the model specification. If a long-run part is already defined in the user inputs, it uses that instead of generating a new formula.
#' @param spec A list containing the model specification, including extracted information about variables and user
#' inputs. The function checks if a long-run part is already defined in the user inputs and uses it if available; otherwise, it generates the long-run model formula based on the specified long-run variables and their lags.
#' @return A character string representing the long-run model formula, combining the specified variables,
#' their lags, and deterministic variables in the appropriate format for estimation. If a long-run part is already defined in the user inputs, it returns that instead of generating a new formula.
#' @noRd
#'

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


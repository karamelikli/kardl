


#' Replace Patterns with Evaluated Values
#'
#' The \code{replaceValues()} function allows you to replace placeholders in text with variable values or evaluated expressions.
#' Placeholders are marked by \strong{\{\}} brackets, and the function supports capturing both global and non-global variables,
#' making it useful for templating and dynamic string generation in R.
#'
#'
#' @param original_text  A string containing expressions within \code{{}} to be evaluated and replaced.
#' @param x Non-global variables to be evaluated within the function's context.
#' @param capture_warnings  Logical. If \code{TRUE}, captures errors and warnings during evaluation and continues with replacements.
#'        If \code{FALSE}, errors and warnings are not captured.
#'
#' @return A string with all \code{{}} placeholders replaced by evaluated values.
#' @export
#'
#' @examples
#'
#'  # Basic example with global variables
#' orjTXT <- "My F value is {thisF} and  its criterion is {CrF}"
#' thisF <- 2.33
#' CrF <- 32.43
#' replaceValues(orjTXT)
#' cat(replaceValues("The summary ls: {summary(lm(mpg ~ wt + hp, data = mtcars))}\n This is example"))
#'
#'  # Nested function example for non-global variables
#' globalY <- 10
#' bb <- function(x){
#'   y <-  x+2;
#'  secondFunc <- function(x){
#'    replaceValues("The global value of Y+3 is {globalY+3}.  The non global value is {x}",x)
#'   }
#'   secondFunc(y)
#' }
#' bb(5)
#'
#'
#'  # Nested variables example. Something like $$ in PHP
#' FirstVar <- "First"
#' SecondVar <- "Second"
#' FirstSecondVar <- "_____________"
#' orjTXT <- "First var is {FirstVar}. The combinations of var names \"FirstSecondVar\" "
#' orjTXT <- paste0(orjTXT,"is= {{FirstVar}{SecondVar}Var}. Where SecondVar = {SecondVar} .")
#' replaceValues( orjTXT)
#'
#' # Using mathematical expressions
#' MyVar = 3
#' replaceValues( "First var multiplying to 12 is {MyVar*12} and its power is {MyVar^12}.")
#'
#' # Using templates from external files
#' # file_contents <- readLines(system.file("template", templateName, package = getPackageName()))
#' file_contents <- readLines(system.file("template", "printkardl.txt", package = "kardl"))
#' data(imf_example_data)
#'  MyFormula<-CPI~ER+PPI+asym(ER)+deterministic(covid)+trend
#'  kardl_model<-kardl(imf_example_data,MyFormula,mode="quick")
#' output<- replaceValues(file_contents,kardl_model)
#' cat(output)

replaceValues <- function(original_text, x=FALSE, capture_warnings = TRUE) {
  original_text<- paste(original_text, collapse = "\n")

  # variable_names <- gsub("\\{(.+?)\\}", "\\1", regmatches( original_text, gregexpr("\\{(.+?)\\}", original_text, perl = T))[[1]] , perl = T)
  variable_names <- gsub(
    "\\{([^\\{]+?)\\}",    # Updated regex: [^\\{] ensures that \code{{} inside the braces is not included
    "\\1",
    regmatches(
      original_text,
      gregexpr("\\{([^\\{]+?)\\}", original_text, perl = TRUE)
    )[[1]],
    perl = TRUE
  )

  # Create a list with variable names and values
  newList <- list()
  for (key in variable_names) {
    value <- ""
    # Use tryCatch to evaluate the expression and handle errors/warnings
    tryCatch({
      # Attempt to evaluate the expression
      value <- eval(parse(text = key))
    }, error = function(e) {
      # to continue the rest of replacements regardless of accruing any error during execution
      if (capture_warnings) {
        warning(paste("Error:", e$message),call. = F)
      }
    }, warning = function(w) {
      if (capture_warnings) {
        warning(paste("Warning:", w$message),call. = F)
      }
    })

    newOutput <-paste(sub("^\\[1\\] ", "", capture.output(value )), collapse = "\n")
    if (grepl("^\".*\"$", newOutput)) {
      newOutput<- gsub("^\"|\"$", "", newOutput)
    }
    newList[key] <- gsub("\\\\n","\n", newOutput)
  }
  for (key in names(newList)) {
    # to avoiding about intrtsection of regular expression with R commands inside {}
    original_text <-gsub(gsub("([\\$\\[\\]\\)\\(\\+\\-\\~\\.])", "\\\\\\1",
                              paste0("\\{",
                                     gsub("([\\*\\/\\^])", "\\\\\\1",   key) #  For preventing ignorance of mathematical signs
                                     , "\\}"),
                              perl = T),
                         newList[[key]],
                         original_text,
                         perl = T)
  }

  original_text<- gsub("033\\[", "\\\033\\[", original_text)
  if(grepl("\\{.*\\}", original_text)){
    original_text <-replaceValues(original_text, x, capture_warnings)
  }
  original_text
}






#' Merge Two lists accenting the fist list.
#'
#' The first list of values takes precedence. When both lists have items with the same names, the values from the first list will be applied. In merging the two lists, priority is given to the left list, so if there are overlapping items, the corresponding value from the left list will be used in the merged result.
#' @param first list or vector
#' @param second list or vector
#' @param ... other lists or vectors
#'
#' @return list
#' @export
#'
#' @examples
#'
#' a<-list("a"="first a","b"="second a","c"=list("w"=12,"k"=c(1,3,6)))
#' b<-list("a"="first b","b"="second b","d"=14,"e"=45)
#' theResult<- lmerge(a,b)
#' unlist(theResult)
#'
#' # for right merge
#' lmerge(b,a)
#'
#' # Unisted return
#' theResult<- lmerge(a,b,c("v1"=11,22,3,"v5"=5))
#' theResult
#'
#' m2<-list("m1"="kk2","m1.2.3"=list("m1.1.1"=333,"m.1.4"=918,"m.1.5"=982,"m.1.6"=981,"m.1.7"=928))
#' m3<-list("m1"="kk23","m2.3"=2233,"m1.2.4"=list("m1.1.1"=333444,"m.1.5"=982,"m.1.6"=91,"m.1.7"=928))
#' a<-c(32,34,542,"k"=35)
#' b<-c(65,"k"=34)
#'
#' h1<-lmerge(a, m2)
#' unlist( h1)
#' h2<-lmerge(a,b,m2,m3,list("m1.1"=4))
#' unlist(h2)


lmerge<-function(first,second,...){
  a<-first;
  b<-second
  otherArgs<-list(...)
  for (v in 1:length(a)) {
    n<-names(a[v])
    if(isFALSE( is.list( b[n]))){
      if(nzchar(n)){
        no <- which(names(b) == n)
        if(length(no)>0){
          b <- b[-no]
        }
      }
    }else{
      b[n]<-NULL
    }

  }
  o<-c(a,b)
  if(length(otherArgs)>0){
    for (q in 1:length(otherArgs)) {
      b2<-otherArgs[q][[1]]
      for (v in 1:length(o)) {
        n<-names(o[v])
        if(isFALSE( is.list( b2[n]))){
          if(nzchar(n)){
            no <- which(names(b2) == n)
            if(length(no)>0)
              b2 <- b2[-no]
          }
        }else{
          b2[n]<-NULL
        }
      }
      o<- c(o,b2)
    }
  }
  o
}


#' Extract Variables from a Formula
#'
#' The \code{parseFormula()} unction extracts and lists variables from specific parts of a formula, especially those within parentheses of specified function types. This enables users to isolate and analyze segments of a model formula, such as variables within custom functions, with an option to ignore case sensitivity in pattern matching.
#'
#'
#' @param formula_ The initial formula for the model, typically specified using R's formula syntax (e.g.,  \code{y ~ x + f(x1 + x2)}).
#' @param matchPattern A string or vector of strings that specifies the function(s) in the formula from which to extract variables. For example, \code{matchPattern = "f()"} will target variables within \code{f(...)} in the formula.
#' @param ignore.case Logical; if \code{FALSE} (default), pattern matching is case-sensitive; if \code{TRUE}, case is ignored during matching.
#'
#' A logical value indicating whether pattern matching should be case-sensitive (\code{FALSE}) or case-insensitive (\code{TRUE}).
#'
#' @return A list containing:
#' \itemize{
#'    \item   Parsed variables: Variables that are located within the specified \code{matchPattern} part of the formula.
#'    \item   Remaining model: The rest of the formula, excluding the parsed variables.
#'    }
#' @export
#'
#' @examples
#'
#' # Identify variables within specific functions, such as 'asymS'
#' parsed<-parseFormula(y ~ x +det(s + d) + asymS(d + s),"asymS()")
#' parsed
#'
#'  # Parse formulas containing various collection types like () or []
#' formula_ <- y ~ x +det(s -gg- d) + asymS(d2 -rr+ s)-mm[y1+y2+y3]+asym[k1+k2+k3]+trend-huseyin
#'
#' # Extract variables in the 'asymS' function
#' parseFormula(formula_,"asymS")
#'
#' # Use multiple functions to extract variables. If a specified function, such as "uuu",
#' # is not found, nothing is returned for it
#' a<-parseFormula(formula_,c("mm","det","uuu"))
#'
#'  # To obtain variables not enclosed within any function, specify them directly
#'  parseFormula(formula_,c("trend","huseyin"))
#'
#' # By default, 'parseFormula()' is case-sensitive.
#' # For case-insensitive matching, set 'ignore.case = TRUE'
#'
#' parseFormula(formula_,"asyms", TRUE)

parseFormula<- function(formula_, matchPattern = "asym",ignore.case=FALSE) {

  if(typeof(formula_)=="language"){
    indVars <- formula_[3]
    depVar<-formula_[2]
    txt <- as.character(indVars)
    detected<-theRest<-c()
    for (varType in matchPattern) {
      if(varType == "." ){
        if("." %in% all.vars(formula_)){
          detected<-c(detected,".")
          foundPart1<- sub("^\\s*\\+*","", sub("\\+*\\s*\\.","",indVars))
          if(trimws(foundPart1) == ""){
            theRest <- ""}
          else{
            theRest <- as.formula(paste0(depVar,"~",foundPart1))
          }
        }else{
          detected<-c(detected,"")
          theRest <- formula_
        }
      }else{
        thisPatterns<- sub("\\s+$", "",sub("^\\s+", "", sub("[\\[(].*$", "", varType))) # remove ( or [ in addition to trim left and right of the varType
        # patterns<-c(patterns,thisPatterns)
        pattern <- paste0("\\s*[\\+\\-]*\\s*",thisPatterns,"(?![\\w\\d])([\\[(](.+?)[\\])])?")
        m <- gregexpr(pattern, txt,ignore.case ,perl = TRUE)
        foundPart <- regmatches(txt, m)
        if(is.na(foundPart[[1]][1])){
          detected<-c(detected,"")
          theRest <- formula_
        }else{
          theOthers <- regmatches(txt, m, invert = TRUE)[[1]]
          theRest <- paste(theOthers[theOthers != ""], collapse = '')
          if(theRest!=""){
            txt<-theRest
            theRest<- formula_<-as.formula( paste0(depVar ,"~",theRest))
          }else{
            theRest=""
          }
          m1 <- gregexpr("[\\[(].+?[\\])]", foundPart[[1]],ignore.case ,perl = TRUE)
          foundPart1 <- regmatches(foundPart[[1]], m1)
          if(is.na(foundPart1[[1]][1])){
            detected <-  c(detected,thisPatterns)
          }else{
            theLast <- substr(foundPart1, 2, nchar(foundPart1) - 1)
            detected <-  c(detected,  trimws(unlist(strsplit(theLast, "[\\+\\-]", perl = TRUE))))
          }

        }


      }

    }
  }else{
    detected<-matchPattern<-theRest<-""
    #   warning("The formula is not well constructed!",call. = F)
  }
  listDetect<-detected
  if(length(detected)>1){
    listDetect<-unique(detected[detected !=""])
  }
  list(detected = listDetect,  theRest =theRest,   matchPattern=matchPattern)

}




# Batch Control
#
# This function divides a large estimation task into smaller, manageable batches.
# If a job requires numerous estimations (e.g., 1,000,000), this function can partition it into batches # nolint: line_length_linter.
# and determine the start and end points for each batch.
#
# @param inputs A list containing:
#   - BatchTotal: Total number of batches planned.
#   - BatchCurrent: The current batch number being processed.
#   - lagRowsNumber: The total number of estimations required for the job.
#
# @return A list with the following:
#   - startRow: The starting row for the current batch.
#   - endRow: The ending row for the current batch.
#

BatchControl<-function(inputs){
  if ( inputs$batch =="1/1") {
    startRow<-1
    endRow<-inputs$lagRowsNumber # Default: all tasks if batch is NULL
    batch_size <- inputs$lagRowsNumber
  } else
  {
    if (!grepl("^\\d+/\\d+$", inputs$batch)) {
      stop("Invalid batch format. Use 'x/y', where x is the batch number and y is the total number of batches.")
    }

    # Extract batch number and total batches
    batch_parts <- as.numeric(strsplit(inputs$batch, "/")[[1]])
    current_batch <- batch_parts[1]
    total_batches <- batch_parts[2]

    # Validate batch numbers
    if (current_batch < 1 || current_batch > total_batches) {
      warning("Batch number must be between 1 and the total number of batches. Current batch changed to 1.",call.=F)
      current_batch<-1
    }

    # Calculate the range of tasks for the current batch
    batch_size <- ceiling(inputs$lagRowsNumber / total_batches)
    startRow <- (current_batch - 1) * batch_size + 1
    endRow <- min(current_batch * batch_size, inputs$lagRowsNumber)
    if(startRow>inputs$lagRowsNumber){
      startRow<-endRow<-0
    }
    #task_range <- seq(start_task, end_task)


  }
  list(startRow=startRow,
       endRow=endRow,batch_size=batch_size
  )

}




## Simple Progress Bar
##
## \code{progressBar} is a simple and highly modifiable function to present the progress of queued jobs. If you need most sophisticated functions to this propose, please visit \code{\link[progressr]{progressr}}.
## @param current Current job no. For instance, if the 23rd estimation out of 400 is running, this value is 23.
## @param total Total jobs which should be performed.
## @param additionalStrings Any additional text desired to be included at the end of the output.
##
## @return print the progress bar
## @seealso  \code{\link[progressr]{progressr}}
## @examples
## progressBar(23,400)
progressBar<-function(current, total,additionalStrings=""){
  if (!interactive()) return(invisible()) # Skip in non-interactive environments

  persentage<-floor(current/total*100)
  totalLines<-50
  #displayArr<-c("\\","|","/","-","=")
  displayArr<-c(" ","_"," ")
  if(current==total){
    animate<-displayArr[3]
  }else{
    animate<-displayArr[(current %% 2)+1]
  }

  theFirstTimes<-floor(persentage*totalLines/100)
  theSecondTimes<-totalLines-theFirstTimes # floor((100-persentage)*totalLines/100)
  theFirstPart<-paste0( rep("#",times=theFirstTimes)  ,collapse = "")
  theSecondPart<-paste0(rep(" ",times=theSecondTimes),collapse = "")

  cat("\r",paste0("% ",persentage, " [ ", theFirstPart,animate,theSecondPart, "] ",current,"/",total," ",additionalStrings))
}

# Change the lag value in a string

replace_lag_var <- function(string, varName, new_lag) {
  # Ensure varName is a character vector
  varName <- as.character(varName)

  # If varName is a single value, return a single string
  if (length(varName) == 1) {
    result <- gsub("\\{varName\\}", varName, gsub("\\{lag\\}", new_lag, string))
    return(result)
  }

  # If varName is a vector, return a vector of replaced strings
  result <- sapply(varName, function(var) {
    gsub("\\{varName\\}", var, gsub("\\{lag\\}", new_lag, string))
  })

  return(result)
}



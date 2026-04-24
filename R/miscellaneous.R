
#' Merge Lists with Priority to the First Argument
#'
#' The first list of values takes precedence. When both lists have items with the same names, the values from the first list will be applied. In merging the two lists, priority is given to the left list, so if there are overlapping items, the corresponding value from the left list will be used in the merged result.
#' @param first The first list
#' @param second The second list
#' @param ... Additional lists to merge
#'
#' @return A merged list with unique names, prioritizing values from the first list in case of name conflicts.
#' @export
#' @seealso  \code{\link[base]{append}}
#' @details
#' The \code{lmerge()} function is designed to merge multiple lists while giving precedence to the values in the first list.
#' This function is particularly useful when you want to combine settings or parameters from multiple sources while ensuring that the primary source (the first list) takes priority over others.
#'
#' For right merge, a user can simply swap the order of the lists to give priority to the second list. For example, \code{lmerge(b, a)} will prioritize values from list \code{b} over those in list \code{a}.
#'
#' @examples
#'
#' a<-list("a"="first a","b"="second a","c"=list("w"=12,"k"=c(1,3,6)))
#' b<-list("a"="first b","b"="second b","d"=14,"e"=45)
#' myMerged<- lmerge(a,b)
#' print(unlist(myMerged))
#'
#' # for right merge
#' myMerged<- lmerge(b,a)
#' print(unlist(myMerged))
#'
#' # for more than two lists
#' myMerged<- lmerge(a,b,c("v1"=11,22,3,"v5"=5))
#' print(unlist(myMerged))
#'
#' # for more than two lists with nested lists
#' m2<-list("m1"="kk2","m1.2.3"=list("m1.1.1"=333,"m.1.4"=918,"m.1.5"=982,"m.1.6"=981,"m.1.7"=928))
#' m3<-list("m1"="kk23","m2.3"=2233,"m1.2.4"=list("m1.1.1"=333444,"m.1.5"=982,"m.1.6"=91,"m.1.7"=928))
#' a<-c(32,34,542,"k"=35)
#' b<-c(65,"k"=34)
#'
#' h1<-lmerge(a, m2)
#' print(unlist(h1))
#'
#' h2<-lmerge(a,b,m2,m3,list("m1.1"=4))
#' print(unlist(h2))
#'

lmerge<-function(first,second,...){
  a<-first
  b<-second
  otherArgs<-list(...)
  for (v in seq_along(a)) {
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
    for (q in seq_along(otherArgs)) {
      b2<-otherArgs[q][[1]]
      for (v in seq_along(o)) {
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


#' Parse Formula Variables
#'
#' The \code{parseFormula()} function analyzes a given formula to identify and extract variables that match specified patterns. It is particularly useful for isolating variables enclosed within certain functions or constructs in the formula, such as \code{asym()}, \code{det()}, or any user-defined patterns.
#'
#'
#' @param formula The initial formula for the model, typically specified using R's formula syntax (e.g.,  \code{y ~ x + f(x1 + x2)}).
#'
#'
#' @return A list containing:
#' \itemize{
#' \item \code{response}: The response variable(s) extracted from the formula.
#' \item \code{intercept}: A logical value indicating whether the formula includes an intercept (default is TRUE).
#' \item \code{dot}: A logical value indicating whether the formula includes a dot (.) representing all other variables (default is FALSE).
#' \item \code{outside}: A vector of variables that are outside any specified patterns. It includes the variables that are not detected within the specified patterns in the formula.
#' \item \code{inside}: A list where each element corresponds to a detected pattern (e.g., function name) and contains the variables found inside that pattern. For example, if the formula includes \code{asym(x1 + x2)}, the \code{inside} list will have an element named "asym" containing the variables "x1" and "x2". This allows for easy identification of variables that are part of specific constructs in the formula.
#' }
#' @export
#'
#' @seealso \code{\link[stats]{formula}} and \code{\link[base]{gregexpr}}
#'
#' @examples
#'
#' # Parse formulas containing various collection types like ()
#' formula_ <- y ~ x +det(s -gg- d) + asymS(d2 -rr+ s)-mm(y1+y2+y3)+asym(k1+k2+k3)+trend-huseyin
#' # Extract variables
#' parse_formula_vars(formula_)
#'
#'

parse_formula_vars <- function(formula) {
  if (is.character(formula)) {
    formula <- as.formula(formula)
  }

  response <- all.vars(formula[[2]])
  rhs <- formula[[3]]

  res0 <- list(
    response  = response,
    intercept = TRUE,
    dot       = FALSE,
    outside   = character(0),
    inside    = list()
  )

  parse_rhs <- function(expr, current_fun = NULL, res) {

    # Symbol
    if (is.symbol(expr)) {
      v <- as.character(expr)

      if (v == ".") {
        res$dot <- TRUE
        return(res)
      }

      if (is.null(current_fun)) {
        res$outside <- unique(c(res$outside, v))
      } else {
        res$inside[[current_fun]] <-
          unique(c(res$inside[[current_fun]], v))
      }
      return(res)
    }

    # Numeric constants (0 or 1)
    if (is.numeric(expr)) {
      if (expr == 0) res$intercept <- FALSE
      return(res)
    }

    # Call
    if (is.call(expr)) {
      fname <- as.character(expr[[1]])

      # +
      if (fname == "+") {
        for (i in 2:length(expr)) {
          res <- parse_rhs(expr[[i]], current_fun, res)
        }
        return(res)
      }

      # -
      if (fname == "-") {
        res <- parse_rhs(expr[[2]], current_fun, res)
        if (length(expr) == 3) {
          if (is.numeric(expr[[3]]) && expr[[3]] == 1) {
            res$intercept <- FALSE
          } else {
            res <- parse_rhs(expr[[3]], current_fun, res)
          }
        }
        return(res)
      }

      # Function calls (myu(), hjj(), etc.)
      if (!(fname %in% c("*", "/", "^", ":"))) {
        if (is.null(res$inside[[fname]])) {
          res$inside[[fname]] <- character(0)
        }
        for (i in 2:length(expr)) {
          res <- parse_rhs(expr[[i]], fname, res)
        }
        return(res)
      }
    }

    res
  }

  parse_rhs(rhs, NULL, res0)
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


BatchControl<-function(spec){
  if ( spec$argsInfo$batch =="1/1") {
    startRow<-1
    endRow<-spec$extractedInfo$lagRowsNumber # Default: all tasks if batch is NULL
    batch_size <- spec$extractedInfo$lagRowsNumber
  } else
  {
    if (!grepl("^\\d+/\\d+$", spec$argsInfo$batch)) {
      stop("Invalid batch format. Use 'x/y', where x is the batch number and y is the total number of batches.",call. = FALSE)
    }
    # Extract batch number and total batches
    batch_parts <- as.numeric(strsplit(spec$argsInfo$batch, "/", fixed = TRUE)[[1]])
    current_batch <- batch_parts[1]
    total_batches <- batch_parts[2]

    # Validate batch numbers
    if (current_batch < 1 || current_batch > total_batches) {
      warning("Batch number must be between 1 and the total number of batches. Current batch changed to 1.",call.=F)
      current_batch<-1
    }

    # Calculate the range of tasks for the current batch
    batch_size <- ceiling(spec$extractedInfo$lagRowsNumber / total_batches)
    startRow <- (current_batch - 1) * batch_size + 1
    endRow <- min(current_batch * batch_size, spec$extractedInfo$lagRowsNumber)
    if(startRow>spec$extractedInfo$lagRowsNumber){
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
progressBar<-function(current, total,additionalStrings="",
                      verbose = TRUE, use_message = FALSE){
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
  txt <- sprintf("%% %s [ %s%s%s ] %s/%s %s",
                 persentage, theFirstPart, animate, theSecondPart,
                 current, total, additionalStrings)
  if (verbose) {
    if (use_message) {
      message(txt)   # CRAN-friendly but no overwrite
    } else {
      cat("\r", txt) # Overwrite in console
      flush.console()
    }
  }
 # cat("\r",paste0("% ",persentage, " [ ", theFirstPart,animate,theSecondPart, "] ",current,"/",total," ",additionalStrings))
}

# Change the lag value in a string

replace_lag_var <- function(string, varName, new_lag) {
  # Ensure varName is a character vector
  varName <- as.character(varName)

  # If varName is a single value, return a single string
  if (length(varName) == 1) {
    result <- gsub("{varName}", varName, gsub("{lag}", new_lag, string, fixed = TRUE), fixed = TRUE)
    return(result)
  }

  # If varName is a vector, return a vector of replaced strings
  result <- sapply(varName, function(var) {
    gsub("{varName}", var, gsub("{lag}", new_lag, string, fixed = TRUE), fixed = TRUE)
  })

  return(result)
}



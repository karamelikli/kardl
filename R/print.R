#' Plot method for CUSUM and CUSUMQ tests
#'
#' @param x An object of class "kardl" created by \code{\link{cusum}} or \code{\link{cusumq}}.
#' @param ... Additional arguments passed to \code{plot()}.
#' @export
plot.kardl <- function(x, ...) {
  if (x$type %in% c("Cusum", "CusumQ")) {
    Cu <- x$dataframe
    grange <- range(Cu$Lower, Cu$Upper)

    op <- par(mar = c(5,4,4,1))
    on.exit(par(op))

    plot(Cu$X, Cu$Y, type = "l", col = "blue",
         main = paste(x$method, "Test"),
         xlab = "", ylab = "", ylim = grange, ...)
    lines(Cu$X, Cu$Lower, col = "red")
    lines(Cu$X, Cu$Upper, col = "red")
    abline(h=0, lty=2)
    legend("topleft", xpd = TRUE, bty = "n",
           legend = c(x$method, "5% significance"),
           lty = c(1,1), cex = 0.9,
           col = c("blue","red"))
  } else {
    stop("No plot method for this object type.")
  }
}

#' @export
summary.kardl<-function(object, saveToFile=FALSE, ...){
  x <- object
template<-""
  switch (x$type,
    "kardlmodel" =
      {
        x$summary=summary(x$model)
        template<-"summary.kardl.txt"
        },
    "cointegration"={
      template<-"summary.cointegration.txt"
      x$H0<- paste0("H0: Coef(", paste0(x$parameter,collapse = ") = Coef(" ),") = 0")
      x$H1<- paste0("H1: Coef(", paste0(x$parameter,collapse = ") \u2260 Coef(" ),")\u2260",  " 0")
      x$Significant <- names(x$result$star)
          switch (x$method ,
                  "Banerjee" ={
                    x$CointText<-paste0("The coeffient of ECM is ",x$coef," , t=",x$statistic," k=",x$k)
                  },
                  "recmt"=      {
                    # This function was performed in two steps. In the first step, the ECM was calculated and in the second step, the PSS test was performed.
                    x$CointText<-paste0("Attention: This function was performed in two steps.\nIn the first step, the ECM was calculated and in the second step, the PSS test was performed.\n")
                    x$CointText<-paste0(x$CointText,"\nThe coeffient of ECM is ",x$coef," , t=",x$statistic," k=",x$k)
                  },
                  "Pesarant"= {
                    x$CointText<- paste0("The t statistics = ",x$statistic," where k = ",x$k,".")
                  },
                  "Narayan"= {
                    x$CointText<-paste0("The F statistics = ",x$statistic," where k = ",x$k,".\nThe proboblity is = ",sprintf("%.10f",x$Fmodel[["Pr(>F)"]][2]))
                  },
                  "PesaranF"= {
                    x$CointText<-paste0("The F statistics = ",x$statistic," where k = ",x$k,". \nThe proboblity is = ",sprintf("%.10f",x$Fmodel[["Pr(>F)"]][2]))
                  },
                  "AARDL"=    {
                    x$CointText<-paste0("AARDL test.\nThe calculated AARDL Wald F is ",x$statistic," and  k=",x$k,"\n The calculated Wald F for PSS is ",x$PF$statistic,"\n The calculated PSS t is ",x$Pt$statistic,"\n")


                    }
          )
          #  yaz<-printCointegration(x,saveToFile,...)
    },
    "ARCH"={
      template<-"summary.ARCH.txt"
    },
    "asymmetrytest"={
      template<-"summary.asymmetrytest.txt"

      if(!is.null(x$Lwald)){
        x$Ltitle<-"Asymmetries in the long run\n"
        x$LH<-""
        for (v in 1:length( x$Lhypotheses$H0)) {
          x$LH<-paste0(x$LH,"H0: ", x$Lhypotheses$H0[v] , "\n")
          x$LH<-paste0(x$LH,"H1: ", x$Lhypotheses$H1[v] , "\n\n")

        }
      }
      if(!is.null(x$Swald) && !is.null(x$Lwald)){  x$Line<-"\n_____________________________\n" }
      if(!is.null(x$Swald)){
        x$Stitle<-"Asymmetries in the short run\n\n"
        x$SH<-""
        for (v in 1:length( x$Shypotheses$H0)) {
          x$SH<-paste0(x$SH,"H0: ", x$Shypotheses$H0[v] , "\n")
          x$SH<-paste0(x$SH,"H1: ", x$Shypotheses$H1[v] , "\n\n")

        }
      }
    }
  )
# private function to print the template
yaz<- printTemplate(x,template,saveToFile,...)
  class(yaz)<-"kardlPrint"
  yaz
}


#' @export
print.kardlPrint<-function(x,...){
  cat(x$text)
}

#' @export
print.writemath<-function(x,...){
  cat(x)
}

#' @export
print.kardl<-function(x,saveToFile=FALSE,...){
  switch (x$type,
          "kardlmodel" ={
            yaz<- printTemplate(x,"printkardl.txt",saveToFile,...)
            },
          "Cusum"=  {
            yaz<-list()
            if(isFALSE(isFALSE(x$saveToFile))){
              yaz$text<-paste0("The file saved in: ",x$saveToFile)
            }else{
              yaz$text<-paste0(x$method," results is ",ifelse(x$Stability=="U","Unstable","Stable"))
            } },
          "kardl_longrun"={
           # returnList <- list(Normalized=x$results, Desc=x$starsDesc)
             yaz<-printTemplate(x,"printkardl_longrun.txt",saveToFile,...)
            },
          "cointegration"={
            yaz<-list()
            if(x$method == "recm" ){
              yaz<- printTemplate(x,"printkardl.txt",saveToFile,...)
            }

            switch (x$method ,
                    "Banerjee" ={yaz$text<-paste0(x$method ," test.\nThe coeffient of ECM is ",x$coef," , t=",x$statistic," k=",x$k,"\n")},
                    "ECM"=      {yaz$text<-paste0(x$method ," test.\nAttention: this function is deprecated.\nThe coeffient of ECM is ",x$coef," , t=",x$statistic," k=",x$k,"\n")},
                    "Pesarant"= {yaz$text<- paste0("PSS t test. Case: ",x$case,"\nThe t statistics = ",x$statistic," where k = ",x$k,".","\n")},
                    "recmt"= {yaz$text<-  paste0(yaz$text,"\nRESM test. Case: ",x$case,"\nThe t statistics = ",x$statistic," where k = ",x$k,".","\n")},
                    "Narayan"= {yaz$text<-paste0(x$method ," F test. Case: ",x$case,"\nThe F statistics = ",x$statistic," where k = ",x$k,". \nThe proboblity is = ",sprintf("%.10f",x$Fmodel[["Pr(>F)"]][2]),"\n")},
                    "PesaranF"= {yaz$text<-paste0("PSS F test. Case: ",x$case,"\nThe F statistics = ",x$statistic," where k = ",x$k,". \nThe proboblity is = ",sprintf("%.10f",x$Fmodel[["Pr(>F)"]][2]),"\n")},
                    "AARDL"=    {yaz$text<-paste0("AARDL test.\nThe calculated AARDL Wald F is ",x$statistic," and  k=",x$k,"\n The calculated Wald F for PSS is ",x$PF$statistic,"\n The calculated PSS t is ",x$Pt$statistic,"\n")}
            )
            # Checking if there are warnings
            if (!is.null(x$warnings)) {
              yaz$text <- paste0(yaz$text, "\nWarnings:\n", paste(x$warnings, collapse = "\n"))
            }
          #  yaz<-printCointegration(x,saveToFile,...)
            },
          "ARCH"={
            yaz<-printTemplate(x,"printARCH.txt",saveToFile,...)
          },
          "asymmetrytest"={
            yaz<-list()
            yaz$text<-""
            if(!is.null(x$Lwald)){
              cat("Asymmetries in the long run\n\n")
              print(x$Lwald[,1:2])
            }
            if(!is.null(x$Swald) && !is.null(x$Lwald)){  cat("________________________________\n")}
            if(!is.null(x$Swald)){
              cat("Asymmetries in the short run\n\n")
              print(x$Swald[,1:2])
            }
          },
          "bootstrap"={
            if (length(x$plots) == 0) {
              print("The list of plots is empty.")
            } else {
            for (variable in x$plots) {
              print(variable)
            }
            }
            return()
          },
         {  yaz<-print(x)} #  Select unnamed element in the case of no match DEFAULT
  )

  cat(yaz$text)
}

# print function to replace values in the template
printTemplate<-function(x,templateName,saveToFile,...){
  file_contents <- readLines(system.file("template", templateName, package = "kardl"))
  output<- replaceValues(file_contents,x)
  x$text<-output
  class(x)<-"kardlFormat"
  if(isFALSE(isFALSE(saveToFile))){
    cat(output,file=saveToFile,...)
  }else{
    x
  }
}


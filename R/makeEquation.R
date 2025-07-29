
#' Make Equations of used model in Latex or Office
#'
#' The \strong{writemath} function generates all necessary equations for analysis, including those for ARDL, NARDL, short-run and long-run nonlinearities, and dynamic multipliers. It produces these equations in multiple formats: as text and as files compatible with MS Word, Markdown, LaTeX, and LibreOffice. This function provides a comprehensive set of formulas essential for econometric analysis, facilitating easy access to equations across various document formats for efficient reporting and presentation.
#' @param formula_ The \strong{formula_} parameter allows a user to specify a model formula, such as \code{CPI~ER+PPI+asym(ER)+deterministic(covid)+trend}, to define the relationships in the analysis. Alternatively, if a user provide a kardl output object, the function will automatically extract all necessary variables and parameters from this object, simplifying the setup process. This flexibility enables users to define the model structure either through a formula or directly from an existing analysis object.
#' @param output The output parameter specifies the path and file name for saving output equations.
#' \itemize{
#'  \item \strong{MS Word Output}: If output is set with a \strong{.docx} extension, the equations will be saved as a \strong{Word} document.
#'  \item \strong{LaTeX Output}: If specified with a \strong{.tex} extension, the equations will be saved as a \strong{LaTeX} file.
#'  \item \strong{Markdown Output}: If specified with a \strong{.rm} extension, the equations will be saved as a \strong{Markdown} file.
#'  \item \strong{PDF Output}: If specified with a \strong{.pdf} extension, the equations will be saved as a \strong{PDF} file.
#'  \item \strong{Display on terminal}
#'  \itemize{
#'      \item \strong{1}: Using \strong{1} as the output displays \strong{LaTeX} formatted equations on the screen without saving to a file.
#'      \item \strong{2}: Setting output to \strong{2} displays equations in \strong{Markdown} on terminal.
#'      \item \strong{3}: Setting output to \strong{3} displays the equations as \strong{OpenOffice}-compatible math formulas on terminal.
#'          }
#' }
#'
#' If \emph{output} is specified without directory paths (i.e., without / for Linux or \\\\ for Windows), the file will be saved in the current working directory, which can be identified by calling the \code{getwd()} function. This feature ensures that, in the absence of a specified path, files are saved to a known default location.
#'
#' See examples for details
#'
#'
#' @return print or save
#' @export
#'
#' @examples
#'
#'
#' # Following commad saves LibreOffice formulas.
#' # Copy and paste formulas there and after selecting related formula click on formula icon.
#'
#' kardl_model<-kardl(imf_example_data,CPI~ER+PPI+asym(ER)+deterministic(covid)+trend,mode=c(1,2,3,0))
#'
#' # Following command writes all equations in a latex file in cuurent directory.
#'
#' tmp <- tempfile(fileext = ".tex")
#' writemath(kardl_model,tmp)
#' unlink(tmp)
#'
#' # For printing the equations in Latex fomrat assignning of file name to 1 is required.
#' # A user's formula can be written directly ib the function.
#' writemath(y~x+z+asym(v+w)+asymL(q+a)+asymS(m),1)
#' # To saving outputs for different output type, in a list is available.
#' a<-writemath(y~x+z+asym(v+w)+asymL(q+a)+asymS(m),1)
#' # For printing the equations in Markdown fomrat assignning of file name to 2 is required.
#' writemath(y~x+z+asym(v+w)+asymL(q+a)+asymS(m),2)
#' # For printing the equations in OpenOffice fomrat assignning of file name to 3 is required.
#' writemath(y~x+z+asym(v+w)+asymL(q+a)+asymS(m),3)
#' # For saving all equations in a MS word document, \strong{docx}.
#' # The directory can be added alongside of the file name.
#' # For Windows OS directories could be written as \\ not /.
#'
#' writemath(y~x+z+asym(v)+asymL(q+a),"~/Downloads/myWordEqueation.docx")
#' # For Markdown file the extenssion of the file should be assigned as \strong{md}.
#' writemath(y~x+z+asym(v+w)+asymL(q+a)+asymS(m),"~/Downloads/myWordEqueation.md")
#' # All equations are able to be saved in a PDF file.
#' writemath(y~x+z+asym(v+w)+asymL(q+a)+asymS(m),"~/Downloads/myWordEqueation.pdf")
  writemath<-function(formula_,output=1){
    if(inherits(formula_, "kardl")){
      vars<- formula_$inputs
    }else{
      if(inherits(formula_, "formula")){
        vars<- list(DifferentAsymLag=T)
        deterministic<-parseFormula(formula_,"deterministic()",T)
        deterministic_<-unique(trimws(deterministic$detected));
        vars[["deterministic"]] <- deterministic_[nzchar(deterministic_)]
        trend<-parseFormula(deterministic$theRest,"trend",T)
        vars[["trend"]] <-ifelse(trend$detected=="",F,T)
        Allvars_<-unique(trimws(c(all.vars(trend$theRest))));
        vars[["Allvars"]] <-Allvars_[nzchar(Allvars_)]
        Asym<-parseFormula(trend$theRest,"asym()",T)
        AsymL<-parseFormula(Asym$theRest,"AsymL()",T)
        asymS<-parseFormula(AsymL$theRest,"asymS()",T)
        ALvars_<-unique(trimws(c(Asym$detected,AsymL$detected)))
        vars[["ALvars"]] <-ALvars_[nzchar(ALvars_)]
        ASvars_ <-unique(trimws(c(Asym$detected,asymS$detected)))
        vars[["ASvars"]]<-ASvars_[nzchar(ASvars_)]
      }else{
        stop("Please check inputs!")
      }
    }
  fileExtension <-sub(pattern = "^(.*\\.|[^.]+)(?=[^.]*)", replacement = "", output, perl = TRUE)
  if(fileExtension==""){
    type<- output # type of print
    saveToFile<-F
  }else{
    file_name_no_ext <- sub("\\.[^.]+$", "", output)
   type<- switch (tolower(fileExtension) ,
            "tex"=1,
            "md"=2,
            "pdf"=4, # Here 3 dropped because of libreoffice option in previous condition.
            "docx" = 5,
            1
    )
    saveToFile<-T
  }
    formulas<-makeEquationComponents(vars,type)
  if(type==5){
     saveMsOffice(formulas,output)
  }else{
    theLastOutput<- switch (type,
      printLatex(formulas),  # if type == 1
      printMd(formulas),  # if type == 2
      printOpenOffice(formulas),  # if type == 3
      printMd(formulas)  # if type == 4 pdf
    )

    if(type==4){
      if(!requireNamespace("rmarkdown" , quietly = TRUE)) {
        stop("Please install the rmarkdown package to use PDF output.")
      }

      temp_rmd <- tempfile(fileext = ".Rmd")
      sink(temp_rmd)
      print(theLastOutput)
      sink()
      rmarkdown::render(temp_rmd, output_format = "pdf_document", output_file = output)
      unlink(temp_rmd)
    }else{
      if(isTRUE(saveToFile)){
          sink(output)
          print(theLastOutput)
          sink()
      }else{
        theLastOutput
      }
   }
  }

}


# makeEquationComponents
#
# make Equation Components using inputs. Output may be in OpenOffice or latex format
# @param OptModel
#
# @return list
#
makeEquationComponents<-function(settings,Fformat){

  if(Fformat==3){
    hspace<-"~"
    identifier<-"%"
    delta<-"%DELTA"
    sumation<-"sum"
    sumation_from<-" from"
    sumation_to<-" to"
    newLine<-" {}newline{} "
    longMinus<-"`-`"
    frac1<-""
    frac2<-" over "
    plusPower<-'size 12 {+" "}'
    minusPower<-'size 12 {-" "}'
    infinite<-"infinite"
    limitFrom<-"lim from"
    limitTo<-" toward "
    partial<-"partial"

  }else{
    hspace<-"\\:"
    identifier<-"\\"
    delta<-"\\Delta"
    sumation<-"\\sum"
    sumation_from<-"_"
    sumation_to<-"^"
    newLine<-switch (Fformat,
                     " \\\\ ",
                     " $$$$ ",
                     " $$$$ ",
                     "",
                     " \n\n"
    )
    longMinus<-"-"
    frac1<-"\\frac"
    frac2<-""
    plusPower<-'\\text{+ }'
    minusPower<-'\\text{- }'
    infinite<-"infty"
    limitFrom<-"\\lim_"
    limitTo<-" \\to "
    partial<-"\\partial"
  }

  independentVars <- settings$Allvars[-1] # all variables except the first one
  AllAsymVars<-unique(c(settings$ALvars,settings$ASvars))

  gostergeler<-c("p","q","m","n","v","w","b","h","g","d","s","r","o")
  n_var<-length(settings$Allvars)

  Libre_ex<-asVar<-Libre_SSum_InDep <-LibreO_AsyShort<-LibreO_AsyLong<-Libre_SSum_InDep2<-LibreO_ulaAsyTxtS1<-Libre_SSum_InDep1<-NCoefs_modif_eq_Formula<-NCoefs_longRun_eq_Formula<-Coefs_modif_eq_Formula<-Coefs_longRun_eq_Formula<-""

  DependVar<-settings$Allvars[1]
  LDependVar<-paste0('{',DependVar,'}')

  Libre_Dependent <-paste0(" ",delta," ",LDependVar,"_t = ")
  Libre_Long_Dependent <-paste0("  ",LDependVar,"_t = ") #Uzun Dönem
  Coefs_modif_eq_FormulaOratk<-paste0("  ",identifier,"eta _0  =",identifier,"theta")
  Coefs_longRun_eq_FormulaOrtak <-paste0(" ",hspace," ",identifier,"theta  = ",identifier,"eta _0  ")
  if(length(AllAsymVars)  > 0) {
    Decompos_eq_Formula<-""
    DynMul_eq_Formula<-""
    for(aslar in 1:length(AllAsymVars)){
      asVar<-paste0('{',AllAsymVars[aslar],'}')
      if (aslar>1) { SonrakiSatir<-newLine} else {SonrakiSatir<-"" }
      Decompos_eq_Formula<- paste0(Decompos_eq_Formula,SonrakiSatir,asVar,'^{',plusPower,' }_t = ',sumation,sumation_from,'{i=1}',sumation_to,'{t} { ',delta,' ',asVar,'^{',plusPower,' }_i } =  ',sumation,sumation_from,'{i=1}',sumation_to,'{t} {max(',delta,' ',asVar,"_i ,0) } ",hspace,";",hspace,asVar,'^{',minusPower,' }_t = ',sumation,sumation_from,'{i=1}',sumation_to,'{t} { ',delta,' ',asVar,'^{',minusPower,' }_i }=  ',sumation,sumation_from,'{i=1}',sumation_to,'{t} {min(',delta,' ',asVar,"_i ,0) } ")
      DynMul_eq_Formula<-paste0(DynMul_eq_Formula,SonrakiSatir,'m^{',plusPower,' } _h =  ',sumation,sumation_from,'{i=0}',sumation_to,'{h}{{',frac1,'{ ',partial,'  ',LDependVar," _{t+i} }",frac2,' {',partial,'  ',asVar,'^{',plusPower,' } _t}  }} ',hspace,";",hspace ,' m^{',minusPower,' } _h =  ',sumation,sumation_from,'{i=0}',sumation_to,'{h}  { ',frac1,'{ ',partial,'  ',LDependVar," _{t+i} }",frac2,' {',partial,'  ',asVar,'^{',minusPower,' } _t}  } ',newLine,' ',limitFrom,' { h ',limitTo,'   ',identifier,infinite,'  } m^{',plusPower,' } _h  =  ',identifier,'alpha^{',plusPower,' } _1 ',hspace,",",hspace  ,limitFrom,' { h ',limitTo,'   ',identifier,infinite,'  } m^{',minusPower,' } _h  =  ',identifier,'alpha^{',minusPower,' } _1   ')
    }
  }

  Libre_C <-paste0(" ",identifier,"beta_0 +  ")
  Libre_C1 <-paste0(" ",identifier,"alpha_0    ")
  Libre_C2 <-paste0(" ",identifier,"psi  +  ")
  Coefs_modif_eq_FormulaOratk<- paste0("",identifier,"psi = ",identifier,"beta_0 -  ",identifier,"theta  ",identifier,"alpha_0  ",hspace,",",hspace,Coefs_modif_eq_FormulaOratk 	   )


  ekGosterde<-0
  Libre_SSum_Dep <-paste0("",sumation,sumation_from,"{j=1}",sumation_to,"{",gostergeler[1],"} { ",identifier,"beta_{1j}  ",delta," ",LDependVar,"_{t-j} }")
  LibreO_ulaTxtLOrtak <-paste0("",identifier,"eta _0   ", LDependVar,"_{t-1} ")
  ARDL_def_eq_Formula<-paste0("ARDL(",gostergeler[1])

  if(.kardl_env$DifferentAsymLag &  length(settings$ASvars) >0 )
  {
    eklenecekSayi=1
  }else{
    eklenecekSayi=0
  }

  for (v in 2:n_var){
    buDeg= settings$Allvars[v]
    LbuDeg<-paste0('{',buDeg,'}')
    Libre_SSum_InDep <-paste0(Libre_SSum_InDep ,"+ ",sumation,sumation_from,"{j=0}",sumation_to,"{",gostergeler[v+ekGosterde],"} { ",identifier,"beta_{",v,"j}   ",delta," ",LbuDeg,"_{t-j} }")

    ARDL_def_eq_Formula<-paste0(ARDL_def_eq_Formula,",",gostergeler[(v+ekGosterde)])
    Libre_SSum_InDep2 <-paste0(Libre_SSum_InDep2 ," + ",identifier,"eta _",(v-1),"   ",LbuDeg,"_{t-1} ")
    Libre_SSum_InDep1 <-paste0(Libre_SSum_InDep1 ," + ",identifier,"alpha _",(v-1),"   ",LbuDeg,"_{t } ")
    if (length(AllAsymVars)  > 0 ){

      if (buDeg %in% settings$ASvars){
        LibreO_AsyShort<-paste0(LibreO_AsyShort,"+ ",sumation,sumation_from,"{j=0}",sumation_to,"{",gostergeler[v+ekGosterde],'}{',identifier,'beta^{',plusPower,' }_{', v,"j} ",delta," ",LbuDeg,'^{',plusPower,' }_{t-j}}+')
        ekGosterde= ekGosterde+eklenecekSayi
        LibreO_AsyShort<-paste0(LibreO_AsyShort,"",sumation,sumation_from,"{j=0}",sumation_to,"{",gostergeler[v+ekGosterde],'} {',identifier,'beta^{',minusPower,' }_{',v,"j}   ",delta," ",LbuDeg,'^{',minusPower,' }_{t-j}}')
      }else{
        LibreO_AsyShort<-paste0(LibreO_AsyShort,"+ ",sumation,sumation_from,"{j=0}",sumation_to,"{",gostergeler[v+ekGosterde],"} { ",identifier,"beta_{",v,"j}   ",delta," ",LbuDeg,"_{t-j} }")
      }
      if (buDeg %in% settings$ALvars){
        LibreO_AsyLong<-paste0(LibreO_AsyLong,' + ',identifier,'eta^{',plusPower,' } _',(v-1),"   ",LbuDeg,'^{',plusPower,' }_{t-1} +  ',identifier,'eta^{',minusPower,' } _',(v-1),"   ",LbuDeg,'^{',minusPower,' }_{t-1} ')
        LibreO_ulaAsyTxtS1 <-paste0(LibreO_ulaAsyTxtS1 ,' + ',identifier,'alpha^{',plusPower,' } _',(v-1),"   ",LbuDeg,'^{',plusPower,' }_{t } +   ',identifier,'alpha^{',minusPower,' } _',(v-1),"   ",LbuDeg,'^{',minusPower,' }_{t }')
        NCoefs_modif_eq_Formula<-paste0(NCoefs_modif_eq_Formula,' ',hspace,",",hspace, ' ',identifier,'eta^{',plusPower,' } _',(v-1),' =  ',longMinus,'{',identifier,'theta  ',identifier,'alpha^{',plusPower,' } _',(v-1),' } ' ,hspace,",",hspace, ' ',identifier,'eta^{',minusPower,' } _',(v-1),' =   ',longMinus,'{',identifier,'theta  ',identifier,'alpha^{',minusPower,' } _',(v-1)," } ")
        NCoefs_longRun_eq_Formula<-paste0(NCoefs_longRun_eq_Formula,' ',hspace,",",hspace, ' ',identifier,'alpha^{',plusPower,' } _',(v-1),'  =',longMinus,' { ',frac1,' { ',identifier,'eta^{',plusPower,' } _',(v-1),' }',frac2,'  {',identifier,'theta }} ' ,hspace,",",hspace, ' ',identifier,'alpha^{',minusPower,' } _',(v-1),'   =  ',longMinus,'{ ',frac1,'{ ',identifier,'eta^{',minusPower,' } _',(v-1),' }',frac2,'  {',identifier,'theta }}  ')
      }else{
        LibreO_AsyLong<-paste0(LibreO_AsyLong," + ",identifier,"eta _",(v-1),"   ",LbuDeg,"_{t-1} ")
        LibreO_ulaAsyTxtS1 <-paste0(LibreO_ulaAsyTxtS1 ," + ",identifier,"alpha _",(v-1),"   ",LbuDeg,"_{t } ")
        NCoefs_modif_eq_Formula<-paste0(NCoefs_modif_eq_Formula,' ',hspace,",",hspace, ' ',identifier,"eta _",(v-1)," = ",longMinus,"{",identifier,"theta  ",identifier,"alpha _",(v-1)," } ")
        NCoefs_longRun_eq_Formula<-paste0(NCoefs_longRun_eq_Formula,' ',hspace,",",hspace, ' ',identifier,"alpha _",(v-1)," =  ",longMinus,"{",frac1,"{ ",identifier,"eta _",(v-1)," }",frac2,"  {",identifier,"theta }}  ")
      }
    }
    Coefs_modif_eq_Formula<-paste0(Coefs_modif_eq_Formula,' ',hspace,",",hspace, ' ',identifier,"eta _",(v-1)," = ",longMinus,"{",identifier,"theta  ",identifier,"alpha _",(v-1)," } ")
    Coefs_longRun_eq_Formula<-paste0(Coefs_longRun_eq_Formula,' ',hspace,",",hspace, " { ",identifier,"alpha _",(v-1)," =",longMinus," ",frac1," { ",identifier,"eta _",(v-1)," }",frac2,"  {",identifier,"theta }}  ")

  }
  ARDL_def_eq_Formula<-paste0(ARDL_def_eq_Formula,")")
  xx<-trimws(settings$deterministic)
  # lapply(xx, function(z){ z[!is.na(z) & z != ""]})
  e_var<-length(xx[xx!="" & !is.na(xx)])

  if (e_var >0){
    for (v in 1:e_var) {
      if(length(v)>0){
        Libre_ex<-paste0(Libre_ex,"+ ",identifier,"gamma_",v,' {',settings$deterministic[v],'}_{t}  ')
      }

    }
  }




  ECMEqFormula <-paste0(Libre_Dependent ,Libre_C,Libre_SSum_Dep,Libre_SSum_InDep,Libre_ex,"+ ",identifier,"theta  ",identifier,"epsilon _{t-1} + e_t" )
   if( settings$trend) {
    Libre_ex<-paste0(Libre_ex," + ",identifier,"varphi t  ")
    Libre_SSum_InDep1<-paste0(Libre_SSum_InDep1," + ",identifier,"vartheta t  ")
    LibreO_ulaAsyTxtS1<-paste0(LibreO_ulaAsyTxtS1," + ",identifier,"vartheta t  ")
  }


  longRunEqFormula <-paste0(Libre_Long_Dependent, Libre_C1 ,Libre_SSum_InDep1," + ",identifier,"epsilon _t")
  ARDL_eq_Formula <-paste0(Libre_Dependent ,Libre_C2,LibreO_ulaTxtLOrtak,Libre_SSum_InDep2,"+",Libre_SSum_Dep,Libre_SSum_InDep,Libre_ex,"+ e_t" )
  NARDL_eq_Formula<-paste0(Libre_Dependent ,Libre_C2,LibreO_ulaTxtLOrtak,LibreO_AsyLong,"+",Libre_SSum_Dep,LibreO_AsyShort,Libre_ex,"+ e_t" )
  NARDL_eq_FormulaShort<-paste0(Libre_Dependent ,Libre_C2,LibreO_ulaTxtLOrtak,Libre_SSum_InDep2,"+",Libre_SSum_Dep,LibreO_AsyShort,Libre_ex,"+ e_t" )
  NARDL_eq_FormulaLong<-paste0(Libre_Dependent ,Libre_C2,LibreO_ulaTxtLOrtak,LibreO_AsyLong,"+",Libre_SSum_Dep,Libre_SSum_InDep,Libre_ex,"+ e_t" )
  NlongRunEqFormula<-paste0(Libre_Long_Dependent,Libre_C1 ,LibreO_ulaAsyTxtS1," + ",identifier,"epsilon _t")

  kardlOutput<-list(
    AllAsymVars=AllAsymVars,
    ASvars=settings$ASvars,
    ALvars=settings$ALvars,
    longRunEqFormula=longRunEqFormula,
    ECMEqFormula = ECMEqFormula,
    ARDL_eq_Formula = ARDL_eq_Formula,
    ARDL_def_eq_Formula = ARDL_def_eq_Formula,
    Coefs_modif_eq_Formula = paste0(Coefs_modif_eq_FormulaOratk,Coefs_modif_eq_Formula),
    Coefs_longRun_eq_Formula = paste0(Coefs_longRun_eq_FormulaOrtak,Coefs_longRun_eq_Formula),
    NlongRunEqFormula = NlongRunEqFormula,
    NARDL_eq_Formula = NARDL_eq_Formula,
    NCoefs_modif_eq_Formula = paste0(Coefs_modif_eq_FormulaOratk,NCoefs_modif_eq_Formula),
    NCoefs_longRun_eq_Formula = paste0(Coefs_longRun_eq_FormulaOrtak,NCoefs_longRun_eq_Formula),
    NARDL_eq_FormulaShort = NARDL_eq_FormulaShort,
    NARDL_eq_FormulaLong = NARDL_eq_FormulaLong,
    type=Fformat)
  if(length(AllAsymVars)  > 0) {
    kardlOutput<-lmerge(kardlOutput, list(

      DynMul_eq_Formula = DynMul_eq_Formula,
      Decompos_eq_Formula = Decompos_eq_Formula
    ))
  }

  kardlOutput<-lapply(kardlOutput, function(x) {
    class(x) <- "writemath"
    x
  })

 class(kardlOutput)<-"writemath"
 kardlOutput
}



# Write MS Word
#
# @param x contains all formul's eqautions
# @param fileName The word file's name
#
# @return file

saveMsOffice<-function(x,fileName){
  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("Please install the 'officer' package to use this function.")
  }
  if (!requireNamespace("flextable", quietly = TRUE)) {
    stop("Please install the 'flextable' package to use this function.")
  }
  if (!requireNamespace("equatags", quietly = TRUE)) {
    stop("Please install the 'equatags' package to use this function.")
  }
  My_word <-officer::read_docx()
  addEq2Word<- function(newFormula){
    df <- data.frame(formula = newFormula)
    ft <- flextable::flextable(df)
    ft <- flextable::compose(x = ft, j = "formula",value = flextable::as_paragraph(flextable::as_equation(formula, width = 6)))
    ft <- flextable::width(ft,   width = 6.2)
    ft <- flextable::fontsize(ft, size = 14, part = "all")
    ft<-flextable::border_remove(ft)
    ft<-flextable::delete_part(x = ft, part = "header")
    My_word <- officer::body_add_par(My_word,"")
    My_word <- flextable::body_add_flextable(My_word ,ft,topcaption=F)
    My_word
  }
  descOfModel<-commonText()
  bold_text <- officer::fp_text(bold = TRUE)
  regular_text <- officer::fp_text(bold = FALSE)
  My_word <-officer::body_add_par(My_word,"")
  My_par <-function(varName){
    officer::body_add_fpar(My_word, officer::fpar(
      officer::run_linebreak(),
      officer::ftext(descOfModel[[varName]]$title, prop = bold_text),
      officer::run_linebreak(),
      officer::ftext(descOfModel[[varName]]$desc, prop = regular_text),
      officer::run_linebreak(),
      officer::ftext(descOfModel[[varName]]$end, prop = regular_text)
          )
      )
  }

  My_word <-My_par("longRunEqFormula")
  My_word <- addEq2Word(x$longRunEqFormula)
  My_word <-My_par("ECMEqFormula")
   My_word <- addEq2Word(x$ECMEqFormula)
  My_word <-My_par("ARDL_eq_Formula")
  My_word <- addEq2Word(x$ARDL_eq_Formula)
  My_word <-My_par("ARDL_def_eq_Formula")
  My_word <- addEq2Word(x$ARDL_def_eq_Formula)
  My_word <-My_par("Coefs_modif_eq_Formula")
  My_word <- addEq2Word(paste0(x$Coefs_modif_eq_Formula ))
  My_word <-My_par("Coefs_longRun_eq_Formula")
  My_word <- addEq2Word(paste0(x$Coefs_longRun_eq_Formula))

  if(length(x$AllAsymVars)  > 0) {
    My_word <-My_par("Decompos_eq_Formula")
    split_text <- strsplit(x$Decompos_eq_Formula, "\n\n")
    for (variable in split_text) {
      My_word <- addEq2Word(variable)
    }
    My_word <-My_par("NlongRunEqFormula")
    My_word <- addEq2Word(x$NlongRunEqFormula)
    My_word <-My_par("NARDL_eq_Formula")
    My_word <- addEq2Word(x$NARDL_eq_Formula)
    My_word <-My_par("NCoefs_modif_eq_Formula")
    My_word <- addEq2Word(paste0(x$NCoefs_modif_eq_Formula))
    My_word <-My_par("NCoefs_longRun_eq_Formula")
    My_word <- addEq2Word(paste0(x$NCoefs_longRun_eq_Formula ))
    if(length(x$ASvars)>0){
      My_word <-My_par("NARDL_eq_FormulaShort")
      My_word <- addEq2Word(x$NARDL_eq_FormulaShort)
    }
    if(length(x$ALvars)>0){
      My_word <-My_par("NARDL_eq_FormulaLong")
      My_word <- addEq2Word(x$NARDL_eq_FormulaLong)
    }
    My_word <-My_par("DynMul_eq_Formula")
    split_text <- strsplit(x$DynMul_eq_Formula, "\n\n")
    for (variable in split_text) {
      My_word <- addEq2Word(variable)
    }
  }
  print(My_word,fileName)
}



commonText<-function(){
  list(
  longRunEqFormula=list(title="Long-Run Model",desc="A long-run model in econometrics estimates the relationship between variables when they reach a stable, long-term equilibrium. It is often used to analyze how dependent variables (e.g., output, income) respond to changes in independent variables (e.g., investment, technology) over time.",end="The long-run model is defined as follow:"),
  ECMEqFormula = list(title="Error Correction Model (ECM)",desc="The ECM is a type of econometric model used to estimate how quickly variables return to equilibrium after a short-term shock. It incorporates both short-term adjustments and long-term equilibrium relationships, allowing for more accurate modeling of time series data when variables are cointegrated.",end="The error correction model can be defined as:"),
  ARDL_eq_Formula = list(title="Integration of Long-Run Model into the ECM",desc="By embedding the long-run equilibrium relationship from the long-run model into the ECM, the model can account for both immediate changes and the gradual return to equilibrium, capturing both short-term fluctuations and the overarching long-run trend.",end="By using the long-run model into the ECM model we can have:"),
  ARDL_def_eq_Formula = list(title="ARDL Model Definition",desc="The ARDL (Auto-Regressive Distributed Lag) model is a regression model that includes lags of both the dependent and independent variables. It is particularly useful in analyzing relationships when variables have different orders of integration, as it can handle both I(0) (stationary) and I(1) (non-stationary) series. It provides a framework to explore both long-term relationships and short-term dynamics.",end="We have ARDL model with following definiation:"),
  Coefs_modif_eq_Formula = list(title="Parameters of the ARDL Model",desc="By embedding the residuals from the long-run model into the ECM, new parameters can be introduced.",end="We used following modifications to obtain the ARDL model:"),
  Coefs_longRun_eq_Formula = list(title="Long-Run Coefficients Calculation",desc="To obtain estimated values for the long-run model from the ARDL model estimation, additional calculations are required.",end="Then, for reobtaining the long-run coefficients...:"),
  Decompos_eq_Formula = list(title="Decomposition of the non-linear variable",desc="Asymmetric models capture different responses to positive and negative changes in independent variables. For example, a variable like oil prices might affect economic output differently during increases versus decreases. Asymmetric models allow for this kind of differential impact, which standard models may not capture.",end="The decomposition formula is as follow:"),
  NlongRunEqFormula = list(title="Asymmetric Long-Run Model",desc="The asymmetric long-run model extends the concept of asymmetry to the long-term relationship, estimating how positive and negative changes in independent variables differently impact the dependent variable over the long run.",end="The long-run model containing asymmetric variables is defined as follow:"),
  NARDL_eq_Formula = list(title="Asymmetric Model",desc="In this context, an asymmetric model explicitly models how variables respond differently depending on the direction of the change, applicable in both short and long-run analyses.",end="Non-linear ARDL model is as follow:"),
  NCoefs_modif_eq_Formula = list(title="Parameters of the NARDL Model",desc="By embedding the residuals from the nonlinear long-run model into the ECM, which includes nonlinear independent variables, new parameters can be introduced.",end="We used following modifications to obtain the NARDL model:"),
  NCoefs_longRun_eq_Formula = list(title="Long-Run Coefficients Calculation in the case of non-linearity",desc="To get estiamted values for the long-run NARDL model's values some calculations should be performed.",end="Then, for reobtaining the NARDL long-run coefficients...:"),
  NARDL_eq_FormulaShort = list(title="Asymmetric Short-Run Model",desc="The asymmetric short-run model analyzes the differing effects of positive and negative changes in independent variables in the short term. While nonlinearity exists in the short run, linearity is maintained in the long run.",end="The NARDL model in the case of linearity in the long-run is as follow:"),
  NARDL_eq_FormulaLong = list(title="Asymmetric Long-Run Model",desc="This variant of the asymmetric model considers long-term effects, focusing on how variables might have different impacts on the dependent variable in the long run, based on the direction of changes.",end="The NARDL model in the case of linearity in short-run"),
  DynMul_eq_Formula = list(title="Dynamic multipliers",desc="Asymmetric dynamics describe the process by which variables adjust over time, capturing differing speeds or magnitudes of response to positive versus negative shocks. These dynamics are critical in accurately modeling the evolution of economic or financial systems under asymmetric influences.",end="The dynamic multipliers formulas are as follow:")
  )
}


printOpenOffice<-function(x){
  descOfModel<-commonText()
  My_par <-function(varName){
    paste0(output,descOfModel[[varName]]$title,"\n\n",descOfModel[[varName]]$desc,"\n\n",descOfModel[[varName]]$end,"\n\n",x[[varName]],"\n\n")
  }
  output<- ""
  output<- My_par("longRunEqFormula")
  output<- My_par("ECMEqFormula")
  output<- My_par("ARDL_eq_Formula")
  output<- My_par("ARDL_def_eq_Formula")
  output<- My_par("Coefs_modif_eq_Formula")
  output<- My_par("Coefs_longRun_eq_Formula")
  if(length(x$AllAsymVars)  > 0) {
    output<- My_par("Decompos_eq_Formula")
    output<- My_par("NlongRunEqFormula")
    output<- My_par("NARDL_eq_Formula")
    output<- My_par("NCoefs_modif_eq_Formula")
    output<- My_par("NCoefs_longRun_eq_Formula")
    if(length(x$ASvars)>0){
      output<- My_par("NARDL_eq_FormulaShort")
    }
    if(length(x$ALvars)>0){
      output<- My_par("NARDL_eq_FormulaLong")
    }
    output<- My_par("DynMul_eq_Formula")
  }
  printObj<-list()
  printObj<-list(text= output , formulas=x,desc=descOfModel,type="OpenOffice")
  class(printObj)<-"kardlPrint"
  printObj

}

printMd<-function(x){
  descOfModel<-commonText()
  My_par <-function(varName){
    paste0(output,"## ",descOfModel[[varName]]$title,"\n\n",descOfModel[[varName]]$desc,"\n\n",descOfModel[[varName]]$end,"\n\n$$\n",x[[varName]],"\n$$\n\n")
  }
  output<- ""
  output<- My_par("longRunEqFormula")
  output<- My_par("ECMEqFormula")
  output<- My_par("ARDL_eq_Formula")
  output<- My_par("ARDL_def_eq_Formula")
  output<- My_par("Coefs_modif_eq_Formula")
  output<- My_par("Coefs_longRun_eq_Formula")
  if(length(x$AllAsymVars)  > 0) {
    output<- My_par("Decompos_eq_Formula")
    output<- My_par("NlongRunEqFormula")
    output<- My_par("NARDL_eq_Formula")
    output<- My_par("NCoefs_modif_eq_Formula")
    output<- My_par("NCoefs_longRun_eq_Formula")
    if(length(x$ASvars)>0){
      output<- My_par("NARDL_eq_FormulaShort")
    }
    if(length(x$ALvars)>0){
      output<- My_par("NARDL_eq_FormulaLong")
    }
    output<- My_par("DynMul_eq_Formula")
  }
  printObj<-list()
  printObj<-list(text= output , formulas=x,desc=descOfModel,type="Markdown")
  class(printObj)<-"kardlPrint"
  printObj
}


printLatex<-function(x){
  descOfModel<-commonText()
  My_par <-function(varName){
    paste0(output,"\\textbf{",descOfModel[[varName]]$title,"}\n\n",descOfModel[[varName]]$desc,"\n\n",descOfModel[[varName]]$end,"\n\n\\begin{equation*}\n\\begin{split}\n",x[[varName]],"\n\\end{split}\n\\end{equation*}\n\n")
  }
  output<- "\\documentclass{article} \n \\usepackage{amsmath}\n\\begin{document} \n"
  output<- My_par("longRunEqFormula")
  output<- My_par("ECMEqFormula")
  output<- My_par("ARDL_eq_Formula")
  output<- My_par("ARDL_def_eq_Formula")
  output<- My_par("Coefs_modif_eq_Formula")
  output<- My_par("Coefs_longRun_eq_Formula")
  if(length(x$AllAsymVars)  > 0) {
    output<- My_par("Decompos_eq_Formula")
    output<- My_par("NlongRunEqFormula")
    output<- My_par("NARDL_eq_Formula")
    output<- My_par("NCoefs_modif_eq_Formula")
    output<- My_par("NCoefs_longRun_eq_Formula")
    if(length(x$ASvars)>0){
      output<- My_par("NARDL_eq_FormulaShort")
    }
    if(length(x$ALvars)>0){
      output<- My_par("NARDL_eq_FormulaLong")
    }
    output<- My_par("DynMul_eq_Formula")
  }
  output<- paste0(output,"\n\\end{document}")
  printObj<-list()
  printObj<-list(text= output , formulas=x,desc=descOfModel,type="Latex")
  class(printObj)<-"kardlPrint"
  printObj
}


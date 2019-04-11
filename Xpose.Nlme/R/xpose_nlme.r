
library(xpose)
library(vpc)
library("utils")
library("gridExtra")
library(dplyr)
library(tibble)

promptForACase <-function(prompt,names){
  print("Options are:")
  print(names)
  notDone=TRUE
  indx=-1
  while ( notDone ) {
    c = readline(prompt)
    indx=as.integer(c)
    if ( c > 0 && c< length(names) ) {
      notDone=FALSE
    }
  }
  return(indx)
}

#'
#' lookupMappedColumn looks at column mapping file and maps a column
#'
#'@export
#'
#'
lookupMappedColumn <-function(columnNames,mapping,colName){
  ret=-1
#  indx=grep(paste0(colName,"("),mapping,fixed=TRUE)
  indx=grep(colName,mapping,fixed=TRUE)
  if ( length(indx) >1 )
    indx=indx[[1]]
  if ( length(indx) == 1 ) {
    line=mapping[[indx]]
    mappedName=unlist(strsplit(mapping[[indx]],"\""))[[2]]
    ret=grep(mappedName,columnNames,fixed=TRUE)
    ret=ret[[1]]
  } else
    ret=-1
  return(ret)

}

#'
#' getSimData : Opens and returns simulation data from an NLME VPC run
#'
#' @param input observation data frame
#' @param stratifyColumns comma separted list of covariates to stratify on
#' @param mapFile column mapping file name
#' @param simFile Filename containing simulation data
#'
#'
#' @export
#'
#' @examples
#'
#' simdata = getSimData(input,stratifyColumns="sex")
#'
#'
#'
getSimData <-function(input,stratifyColumns="",mapFile="cols1.txt",simFile="out.txt"){

 inames=colnames(input)
 simData = utils::read.csv(simFile)
 mapping=readLines(mapFile)
 snames= colnames(simData)
 #
 # Change IVAR to TIME and ID5 to ID
 #
 tIndex = grep("IVAR",snames)
 iIndex = grep("ID5",snames)
 snames[[tIndex]] = "TIME"
 snames[[iIndex]] = "ID"
 #
 # Match simulation file columns to input column names so vpc does not barf!
 #
 sTokens = unlist(strsplit(stratifyColumns,","))
 if ( length(sTokens) > 0 ) {
   indx = 1
   for ( cname in sTokens ) {
     mi =lookupMappedColumn(inames,mapping,cname)
     if ( mi == -1 )
       mi =lookupMappedColumn(inames,mapping,tolower(cname))
     name = inames[[mi]]
     snames[[indx]]=name
     indx = indx + 1
   }
 }
 colnames(simData) = snames

 simData
}

#'
#' getObsData : Updates column names for ID, TIME and DV so input can be passed on to vpc package
#'
#' @param input observation data frame
#' @param mapFile column mapping file name
#'
#'
#' @export
#'
#' @examples
#'
#' input = getObsData(input)
#'
#'
#'
getObsData <-function(input,mapFile="cols1.txt"){

  inames=colnames(input)

  mapping=readLines(mapFile)
  indx =lookupMappedColumn(inames,mapping,"time")
  inames[[indx]] = "TIME"
  indx =lookupMappedColumn(inames,mapping,"CObs")
  if ( indx != -1 ){
    inames[[indx]] = "DV"
    input[[indx]] = as.numeric(input[[indx]])
  }
  indx =lookupMappedColumn(inames,mapping,"id")
  if ( indx != -1 )
    inames[[indx]] = "ID"

  indx =lookupMappedColumn(inames,mapping,"dose")
  if ( indx != -1 )
    inames[[indx]] = "Dose"

  colnames(input) = inames
  # PHX ignores those observations in the rows where both dose and observation of zero (or NA) appear together
  # (see the rules given in Page 110 in PHX NLME User Guide).
  input = input[!( !is.na(input$Dose) & (is.na(input$DV) | input$DV == 0 )),]
  input
}

#'
#' mapCovariate : maps a covariate name to input file column name
#'
#' @param input observation data frame
#' @param covarNames comma separated covariate names
#' @param mapFile column mapping file name
#'
#'
#' @export
#'
#' @examples
#'
#' names = mapCovariate(input,"sex,age")
#'
#'
#'
mapCovariate <- function(input,covarNames,mapFile="cols1.txt"){

  ret=c()
  inames=colnames(input)

  mapping=readLines(mapFile)
  cTokens = unlist(strsplit(covarNames,","))
  if ( length(cTokens) > 0 ){
    for ( cn in cTokens ) {
      indx =lookupMappedColumn(inames,mapping,cn)
      if ( indx != -1 )
        ret = c(ret,inames[[indx]])
    }

  }
  ret
}


getNames <-function(lines,categoricalFlag){

    linNos=grep("covr(",lines,fixed=TRUE)
    names=c()
    for ( l in linNos ){
      covarName=""
      isCategorical=FALSE
      line=dmp.txt$cols1.txt[l]
      tokens=unlist(strsplit(line,split="(",fixed=TRUE))
      if ( length(tokens) >= 2 ){
          covarName=unlist(strsplit(tokens[2],split="\"",fixed=TRUE))[2]
          if ( length(tokens) > 2 )
              isCategorical = TRUE
      }
      if ( isCategorical == categoricalFlag )
          names=c(names,covarName)
    }
    return(names)
}



getCatcovNames <-function(lines ) {
    return(getNames(lines,TRUE))
}

getContcovNames <-function(lines ) {
    return(getNames(lines,FALSE))
}


#'
#' xposeNlme : Imports results of an NLME run into xpose database
#'
#' @param dir path to Nlme Run directory
#' @param dmpFile optional Nlme generated S/R file
#' @param dataFile optional Input file for Nlme Run
#' @param logFile optional engine log file
#' @param mapFile column mapping file name
#'
#' @export
#'
#' @examples
#'        xp = xposeNlme("./")
#'        xp = xposeNlme(dmpFile="dmp.txt",dataFile="data1.txt",
#'                       logFile="nlme7engine.log")
#'
#'        dv_vs_pred(xp)
#'        v_vs_ipred(xp)
#'        res_vs_pred(xp,res="IWRES")
#'        ind_plots(xp, res="IWRES")
#'        res_vs_idv(xp,res="WRES")
#'
xposeNlme <-function(dir="",
                     modelName="",
                     dmpFile="dmp.txt",
                     dataFile="data1.txt",
                     logFile="nlme7engine.log",
                     mapFile="cols1.txt"){


    if ( nchar(dir) == 0 )
        dir = "./"
    input=utils::read.csv(paste(dir,dataFile,sep="/"))
    names=colnames(input)

  names=gsub("X..","",names)
  colnames(input)=names


    mapping=readLines(mapFile)
  indx=grep("id",mapping)
  line=mapping[[indx[[1]]]]

  line=gsub("[\"|id(|)]","",line)
  idTokens=unlist(strsplit(line,split=","))
  if ( length(idTokens) > 1 ) {
    idColumns=c()
    for ( t in idTokens) {
      idColumns=c(idColumns,t)
    }
    input$ID = do.call(paste,c(input[idColumns], sep="_"))
  } else {
    # Rename ID column to make our lives easier
    iIndex = lookupMappedColumn(names,mapping,"id")
    if ( iIndex != -1 )
      names[[iIndex]]="ID"
  }

    tIndex=lookupMappedColumn(names,mapping,"time")
    if ( tIndex != -1 )
       names[[tIndex]] = "TIME"

    colnames(input)=names
    input$ID=as.character(input$ID)


    source(paste(dir,dmpFile,sep="/"))

  if ( length(idTokens) > 1 ) {
    if ( length(idTokens) == 2 ) {
      ids=paste(dmp.txt$residual$ID4,dmp.txt$residual$ID5,sep="_")
    }
    if ( length(idTokens) == 3) {
      ids=paste(dmp.txt$residual$ID3,dmp.txt$residual$ID4,
                     dmp.txt$residual$ID5,sep="_")
    }
    if ( length(idTokens) == 4 ) {
      ids=paste(dmp.txt$residual$ID2,dmp.txt$residual$ID3,
             dmp.txt$residual$ID4,dmp.txt$residual$ID5,sep="_")
    }
    if ( length(idTokens) == 5 ) {
      ids=paste(dmp.txt$residual$ID1,dmp.txt$residual$ID2,dmp.txt$residual$ID3,
             dmp.txt$residual$ID4, dmp.txt$residual$ID5,sep="_")
    }
  } else {
    ids=dmp.txt$residuals$ID5
  }


    IVAR=dmp.txt$residuals$IVAR
    dv=dmp.txt$residuals$DV
    PRED=dmp.txt$residuals$PRED
    IPRED=dmp.txt$residuals$IPRED
    IRES=dmp.txt$residuals$IRES
    WRES=dmp.txt$residuals$WRES
    IWRES=dmp.txt$residuals$IWRES
    CWRES=dmp.txt$residuals$CWRES
    Weight=dmp.txt$residuals$Weight
    ObsName=dmp.txt$residuals$ObsName
    TAD=dmp.txt$residuals$TAD

    d=as.data.frame(PRED)
    d$IPRED=IPRED
    d$IVAR=IVAR

    d$TIME=IVAR
    d$ID=as.character(ids)
    orderedIDsDmp = unique(d$ID)

    d$IRES=IRES
    d$Weight=Weight
    d$IPRED=IPRED
    if ( "DV" %in% toupper(colnames(input)) == FALSE ){
        d$DV=dv
    }
    d$WRES=WRES
    d$IWRES=IWRES
    d$CWRES=CWRES
    d$TAD=TAD
    d$ObsName=ObsName
    d$CdfDV=dmp.txt$residuals$CdfDV
    d$CdfPCWRES=dmp.txt$residuals$CdfPCWRES
    d$WhichDose=dmp.txt$residuals$WhichDose
    d$WhichReset=dmp.txt$residuals$WhichReset
    d$PREDSE=dmp.txt$residuals$PREDSE

    fixed=dmp.txt$coefficients$fixed
    fnames=names(fixed)
    indx=1
    for ( indx in 1:length(fixed) ){
      d[[fnames[indx]]]  = fixed[[indx]]
    }

    random=dmp.txt$coefficients$random$Subject
    rnames=dimnames(random)[[2]]
    rids=orderedIDsDmp

    nrandom=cbind.data.frame(random,ID=rids,stringsAsFactors =FALSE)

    d = merge(d,nrandom,by="ID")

    catcovNames = getCatcovNames(dmp.txt$cols1.txt)
    contcovNames = getContcovNames(dmp.txt$cols1.txt)


    d1 = dplyr::inner_join(input,d, by=c("ID","TIME"))

    data_ind <- d1 %>%
      colnames() %>%
      dplyr::tibble(table = 'nlme',
                col   = .,
                type  = NA_character_,
                label = NA_character_,
                units = NA_character_) %>%
      dplyr::mutate(type = dplyr::case_when(
        .$col == 'ID' ~ 'id',
        .$col == 'DV' ~ 'dv',
        .$col == 'IVAR' ~ 'idv',
#        grepl('time',.$col, ignore.case = TRUE ) ~ 'idv',
#        .$col == 'TAD' ~'idv',

        .$col == 'OCC' ~ 'occ',
        .$col == 'DVID' ~ 'dvid',
        .$col == 'AMT' ~ 'amt',
        .$col == 'Amout' ~ 'amt',
        .$col == 'MDV' ~ 'mdv',
        .$col == 'EVID' ~ 'evid',
        .$col == 'IPRED' ~ 'ipred',
        .$col == 'PRED' ~ 'pred',
        .$col %in% c('RES', 'WRES', 'CWRES', 'IWRES', 'EWRES', 'NPDE',
'IRES','CRES') ~ 'res',
        .$col %in% unlist(fnames) ~ 'param',
        .$col %in% unlist(catcovNames) ~ 'catcov',
        .$col %in% unlist(contcovNames) ~ 'contcov',
        .$col %in% unlist(rnames) ~ 'eta'))
#
# Other categories to identify
# evid
# occ
# amt
# catcov
# contcov
#
    data_ind$type[is.na(data_ind$type)] <- 'na'


    data <- list()
    data <- dplyr::tibble(problem = 1,
                      simtab = F,
                      index = list(data_ind),
                      data = list(d1),
                      modified = F)

    gg_theme    = xpose::theme_readable()
    xp_theme    = xpose::theme_xp_default()

    epsShrinkage = etaShrinkage = 'na'

    outLines=readLines(paste(dir,logFile,sep="/"))
    shrinkageLine=outLines[grep("eps-shrinkage",outLines)]
    epsShrinkage=unlist(strsplit(shrinkageLine,split ="="))[[2]]
    etaShrinkage=c()

    shrinkageLines=outLines[grep("eta-shrinkage",outLines)]
    for ( l in shrinkageLines ){
      s=unlist(strsplit(l,split ="="))[[2]]
      etaShrinkage = c(etaShrinkage,s)
    }


    summary = dplyr::bind_rows(
      dplyr::tibble(problem=0,subprob = 0,label = 'software', value='nlme'),
      dplyr::tibble(problem=0,subprob = 0,label = 'version' , value= '8.1') ,
      dplyr::tibble(problem=1,subprob = 0,label = 'ofv',
                    value = as.character(dmp.txt$logLik)),
      dplyr::tibble(problem=1,subprob = 0,label = 'epsshk',
                    value = as.character(epsShrinkage)),
      dplyr::tibble(problem=1,subprob = 0,label = 'etashk',
                    value = as.character(paste(etaShrinkage,collapse=","))),
      dplyr::tibble(problem=1,subprob = 0,label = 'run', value = modelName ),
      dplyr::tibble(problem=1,subprob = 0,label = 'nind',
                    value = as.character(dmp.txt$nSubj)),
      dplyr::tibble(problem=1,subprob = 0,label = 'nobs',
                    value =as.character(dmp.txt$nObs)),
      dplyr::tibble(problem=1,subprob = 0,label = 'dir', value = getwd())
  )

    xp = list(code = dmp.txt$".\\test.mdl", summary = summary ,
                    data = data, software='phx/nlme',
          gg_theme=gg_theme, xp_theme = xp_theme,
          options = list(dir = NULL, quiet = T )) %>%
      structure(class = c('xpose_data', 'uneval'))
        xp
}

.Last <-function(){
}

.First <-function(){
}

.onAttach <-function(libname,pkgname){
# Check and give warning if required packages are not there
# vpc
# xpose
#
    for ( lin in c("vpc","xpose") ){
        stat = require(lib,character.only=TRUE,quietly=TRUE)
        if ( stat == FALSE ) {
            packageStartupMessage(paste0("WARNNING : Library : ",lib," is required for using Xpose.Nlme"))
        }
    }
}

.onAttach <-function(libname,pkgname){
}



#'
#' Create a grid with 4 common diagnostics plots 
#'
#' @param xp xpose database or NULL to create a new database
#' @param modelName  Model name
#'
#'@export
#'

doxpose <-function(xp=NULL,modelName=""){


  if ( is.null(xp ) ) {
  xp=xposeNlme(dir="./",modelName=modelName)
  xpose::list_vars(xp)
  }


  xp1 <- xpose::dv_vs_pred(xp, title = "DV vs PRED",
                    subtitle = NULL, caption = NULL)
  xp2 <- xpose::dv_vs_ipred(xp, title = "DV vs IPRED",
                     subtitle = NULL, caption = NULL)
  xp3 <- xpose::res_vs_idv(xp, res = "IWRES", title = "IWRES vs time",
                    subtitle = NULL, caption = NULL)

  xp4 <- xpose::res_vs_pred(xp, res = "CWRES", title = "CWRES vs PRED",type="ps",
                     subtitle = NULL, caption = NULL)

  gridExtra::grid.arrange(xp1, xp2, xp3, xp4, nrow=2)
}



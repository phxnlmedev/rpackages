
#' @title NLME dataset object
#'
#' Class represents an NLME dataset object
#'
#' @param dataFile            subject datafile
#' @param colDefFile          datafile to model mapping file
#' @param modelFile           PML model file
#' @param estimatesDefFile    Initial estimates mapping file
#' @param estimatesDataFile   Initial estimates values
#' @param doseDefFile         dose column definition file
#' @param doseDataFile        dose datafile
#' @param ranEffectDefFile    random effects column definition file
#' @param ranEffectDataFile   random effects data file
#' @param outputFilename      Name of output file
#' @param phoenixSourceDir    Directory containing phoenix generated files
#' @export NlmeDataset
#' @examples 
#' dataset = NlmeDataset()
#'
NlmeDataset = setClass("NlmeDataset",representation(
                                      dataFile="character",
                                      colDefFile="character",
                                      modelFile="character",
                                      estimatesDefFile="character",
                                      estimatesDataFile="character",
                                      doseDefFile="character",
                                      doseDataFile="character",
                                      ranEffectDefFile="character",
                                      ranEffectDataFile="character",
                                      outputFilename="character",
                                      phoenixSourceDir="character",
                                      engineParamsFile="character"))

assign("NlmeDataset",NlmeDataset,env=.GlobalEnv)

setMethod("initialize","NlmeDataset",
    function(.Object,
              dataFile="data1.txt",
              colDefFile="cols1.txt",
              modelFile="test.mdl",
              estimatesDefFile="", # cols3.txt
              estimatesDataFile="",
              doseDefFile="", #cols2.txt
              doseDataFile="",
              ranEffectDefFile="", #cols4.txt
              ranEffectDataFile="",
              outputFilename="out.txt",
              phoenixSourceDir="",
              engineParamsFile="nlmeargs.txt"){
        .Object@dataFile=dataFile
        .Object@colDefFile=colDefFile
        .Object@modelFile=modelFile
        .Object@estimatesDefFile=estimatesDefFile
        .Object@estimatesDataFile=estimatesDataFile
        .Object@doseDefFile=doseDefFile
        .Object@doseDataFile=doseDataFile
        .Object@ranEffectDefFile=ranEffectDefFile
        .Object@ranEffectDataFile=ranEffectDataFile
        .Object@outputFilename=outputFilename
        .Object@phoenixSourceDir=phoenixSourceDir
        .Object@engineParamsFile=engineParamsFile
        if ( phoenixSourceDir != "" ) {
            if ( dir.exists(phoenixSourceDir) ) {
                if ( doseDefFile == "" && doseDataFile == "" ) {
                    if ( file.exists(paste0(phoenixSourceDir,"/cols2.txt"))&&
                         file.exists(paste0(phoenixSourceDir,"/data2.txt"))){
                        .Object@doseDefFile="cols2.txt"
                        .Object@doseDataFile="data2.txt"
                    }
                }
                if ( estimatesDefFile == "" && estimatesDataFile == "" ) {
                    if ( file.exists(paste0(phoenixSourceDir,"/cols3.txt"))&&
                         file.exists(paste0(phoenixSourceDir,"/data3.txt"))){
                        .Object@estimatesDefFile="cols3.txt"
                        .Object@estimatesDataFile="data3.txt"
                    }
                }
                if ( ranEffectDefFile == "" && ranEffectDataFile == "" ) {
                    if ( file.exists(paste0(phoenixSourceDir,"/cols4.txt"))&&
                         file.exists(paste0(phoenixSourceDir,"/data4.txt"))){
                        .Object@ranEffectDefFile="cols4.txt"
                        .Object@ranEffectDataFile="data4.txt"
                    }
                }
            }
        }
        .Object
    })


#'
#' @export 
#'
NlmeDatasetToString <-function(dataset)
{
    ret=""
    if (attr(dataset,"estimatesDefFile") != "" )
        ret = paste(ret,"/d3",attr(dataset,"estimatesDefFile"), 
                            attr(dataset,"estimatesDataFile") ,sep= " " ) 
    if (attr(dataset,"doseDefFile") != "" )
        ret = paste(ret,"/d2",attr(dataset,"doseDefFile"), 
                            attr(dataset,"doseDataFile") ,sep= " " ) 
    if (attr(dataset,"ranEffectDefFile") != "" )
        ret = paste(ret,"/d4",attr(dataset,"ranEffectDefFile"), 
                            attr(dataset,"ranEffectDataFile") ,sep= " " ) 
    ret = paste(ret,  attr(dataset,"colDefFile"), 
                            attr(dataset,"dataFile"), 
                            attr(dataset,"outputFilename"), 
                            sep=" ")

    return(ret)
}

#'
#' @export 
#'
DatasetGetObserveParams <-function(dataset)
{
    ret=c()
    observeKeywords=c("multi\\(","observe\\(","LL\\(","count\\(",
                      "ordinal\\(", "event\\(")
    lines = readLines(attr(dataset,"modelFile"))
    for ( l in lines ) {
        exist = any(sapply(observeKeywords, grepl, l))
        if ( exist == TRUE ) {
            ret=c(ret,l)
        }
    }
    return(ret)
}
assign("DatasetGetObserveParams",DatasetGetObserveParams,env=.GlobalEnv)

#'
#' @export 
#'
columnNames <-function(dataset)
{
    ret=c()
    lines=readLines(attr(dataset,"dataFile")) 
    header=gsub("^#*","",lines[1])
    names=unlist(strsplit(header, split=","))
    for ( i in 1:length(names) ) {
        names[i] =  gsub("^ *","", names[i])
    }
    return(names)
}

assign("columnNames",columnNames,env=.GlobalEnv)

#'
#' @export 
#'
NlmeDatasetFromScsInput <-function(filename) {

    scsInput = readmodel(filename)
    writeOutScsFiles(scsInput)
    dataset=NlmeDataset()
    return(dataset)
}



assign("NlmeDatasetFromScsInput",NlmeDatasetFromScsInput,env=.GlobalEnv)


writeOutScsFiles <-function(modstr){

  write(modstr$modcode,paste(".","test.mdl",sep="/"))

  if(is.null(modstr$filter)){
    file.copy(modstr$datafl,paste(".","data1.txt",sep="/"))
  }else{
    dataset<-read.csv(modstr$datafl,as.is=TRUE,na=c(".","NA"))
    names(dataset)<-sub("X..","",names(dataset),fixed=TRUE)
    datnms<-names(dataset)
    datfilt<-modstr$filter

    datfilt<-tolower(datfilt)
    for(i in 1:length(datnms)){
      datfilt<-gsub(paste("\\<",tolower(datnms[i]),"\\>",sep=""),
             paste("dataset",datnms[i],sep="$"),datfilt)
      datfilt<-sub("\"dataset$","\"",datfilt,fixed=TRUE)
    }
    datfilt<-paste("dataset<-dataset[",datfilt,",]",sep="")
    eval(parse(text=datfilt))
    names(dataset)[1]<-paste("##",names(dataset)[1],sep="")
    write.csv(dataset,paste(".","data1.txt",sep="/"),
             quote=FALSE,row.names=FALSE,na=c(".","NA"))
  }

  write(modstr$colmap,paste(".","cols1.txt",sep="/"))

}




"readmodel"<-function(model=NULL,flext="mdl",warn=TRUE,...){

#  if(!chkmodel(model,warn=FALSE,...)){
#    if(warn) write("Error: Wrong or missing source model.","")
#    return(NULL)
#  }

  ctlfl<-paste(model,flext,sep=".")

  modeltxt<-readLines(ctlfl)
  modeltxt<-modeltxt[gsub(" ","",modeltxt)!=""]

  dsrec<-which(regexpr("descr:",modeltxt,fixed=TRUE)>0)
  atrec<-which(regexpr("auth:",modeltxt,fixed=TRUE)>0)
  dtrec<-which(regexpr("date:",modeltxt,fixed=TRUE)>0)
  bsrec<-which(regexpr("base:",modeltxt,fixed=TRUE)>0)

  dfrec<-which(regexpr("datafl:",modeltxt,fixed=TRUE)>0)
  frrec<-which(regexpr("filter:",modeltxt,fixed=TRUE)>0)
  cmrec<-which(regexpr("colmap:",modeltxt,fixed=TRUE)>0)
  mdrec<-which(regexpr("model:",modeltxt,fixed=TRUE)>0)

### Model Header ###

  descr<-modeltxt[dsrec:(atrec-1)]
  descr<-sub("descr:","",descr,fixed=TRUE)
  descr<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$","",descr,perl=TRUE)
  test<-gsub(" ","",descr)
  indx<-which(test!="")
  if(length(indx)==0) descr<-"None"
  if(length(indx)>0){
    descr<-descr[indx]
    if(length(descr)>1) descr<-paste(descr,collapse="\n")
  }

  auth<-modeltxt[atrec:(dtrec-1)]
  auth<-sub("auth:","",auth,fixed=TRUE)
  auth<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$","",auth,perl=TRUE)
  test<-gsub(" ","",auth)
  test<-test[test!=""]
  if(length(test)==0) auth<-"None"

  date<-modeltxt[dtrec:(bsrec-1)]
  date<-sub("date:","",date,fixed=TRUE)
  date<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$","",date,perl=TRUE)
  test<-gsub(" ","",date)
  test<-auth[test!=""]
  if(length(test)==0) date<-"NA"

  base<-modeltxt[bsrec:(dfrec-1)]
  base<-sub("base:","",base,fixed=TRUE)
  base<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$","",base,perl=TRUE)
  test<-gsub(" ","",base)
  test<-test[test!=""]
  if(length(test)==0) base<-"NA"

### Model Sections ###

  if(length(frrec)>0){
    filter<-modeltxt[frrec:(cmrec-1)]
    filter<-sub("filter:","",filter,fixed=TRUE)
    filter<-gsub(" ","",filter)
    filter<-gsub("&"," & ",filter,fixed=TRUE)
    filter<-gsub("|"," | ",filter,fixed=TRUE)
    filter<-filter[filter!=""]
    if(length(filter)==0){
      filter<-NULL
    }
  }else{
    filter<-NULL
    frrec<-cmrec
  }

  datafl<-modeltxt[dfrec:(frrec-1)]
  datafl<-sub("datafl:","",datafl,fixed=TRUE)
  datafl<-gsub(" ","",datafl)
  datafl<-datafl[datafl!=""]

  colmap<-modeltxt[cmrec:(mdrec-1)]
  colmap<-sub("colmap:","",colmap,fixed=TRUE)
  colmap<-gsub(" ","",colmap)
  colmap<-colmap[colmap!=""]
  colmap<-paste(colmap,collapse="\n")

  modcode<-modeltxt[mdrec:length(modeltxt)]
  modcode<-sub("model:","",modcode,fixed=TRUE)
  modcode<-modcode[gsub(" ","",modcode)!=""]
  modcode<-paste(modcode,collapse="\n")

  return(list(descr=descr,auth=auth,date=date,base=base,
              datafl=datafl,filter=filter,colmap=colmap,modcode=modcode))
}




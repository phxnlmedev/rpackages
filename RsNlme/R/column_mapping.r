#'
#' @importFrom utils write.table
NULL
#'



#' NlmeColumnMap
#'
#' Class represents map between a model variable and a dataset column
#'
#' @param variableName   Model variable name
#' @param columnName     Corresponding column name in the dataset
#'
#' @examples
#'    mapping = NlmeColumnMap(variableName,columnName)
#'
#' @export NlmeColumnMap
#'
NlmeColumnMap= setClass("NlmeColumnMap",representation(
                                      variableName="character",
                                      columnName="character"))


setMethod("initialize","NlmeColumnMap",
    function(.Object,
              variableName="",
              columnName=""){

        .Object@variableName=variableName
        .Object@columnName=columnName
        .Object
    })
assign("NlmeColumnMap",NlmeColumnMap,envir=.GlobalEnv)
#' print.NlmeColumnMap
#'
#' Prints column mapping
#'
#' @param obj   Model object
#'
#' @examples
#'    print.NlmeColumnMap(NlmeColumnMapping(mapping))
#'
#' @export
#'
print.NlmeColumnMap <-function(x, ...)
{
    type = attr(x,"type")
    print( paste0(x@variableName , " => ", x@columnName))
}


#' NlmeColumnMapping
#'
#' Class represents mapping list between model variables and dataset columns
#'
#' @examples
#'    mapping = NlmeColumnMapping("list")
#'
#' @export NlmeColumnMapping
#'
NlmeColumnMapping= setClass("NlmeColumnMapping",representation(
                                      mapping = "list"))


setMethod("initialize","NlmeColumnMapping",
    function(.Object,
              model,
              inputData){

        .Object@mapping = createInitialMapping(model,inputData)
        .Object
    })
assign("NlmeColumnMapping",NlmeColumnMapping,envir=.GlobalEnv)

#' createInitialMapping
#'
#' Creates a mapping between model variable and dataset columns
#'
#' @param model      A PK/PD model
#' @param InputData  Column name
#'
#' @examples
#'    createInitialMapping(model,InputData)
#'
#' @export createInitialMapping
#'
createInitialMapping <-function(model,inputData){
    map=list()
    ids=c()
    if ( model@isPopulation == TRUE )
#        ids=c("sort","id")
        ids=c("id")
    if ( attr(model,"isTimeBased") == TRUE )
        ids=c(ids,"time")

    on = observationNames(model)
    en = observationExtraNames(model)
    cn = covariateNames(model)
    dn = doseNames(model)
    edn = extraDoseNames(model)
    names=c(ids,dn,edn,on,en,cn)
    if ( model@hasResetInfo )
      names=c(names,"Reset")
    colNames=colnames(inputData)

    for ( n in names ) {
        # Check for exact match
        for ( c in colNames ) {
            if ( toupper(c) == toupper(n) ) {
                m = NlmeColumnMap(n,c)
                map[[n]] = m
            }
        }
    }
    for ( n in names ) {
        # Check for partial match
        if ( length( map[[n]]) == 0 ) {
            for ( c in colNames ) {
                if ( length(grep(toupper(n),toupper(c))) != 0 ) {
                    m = NlmeColumnMap(n,c)
                    map[[n]] = m
                }
            }
        }
    }
    for ( n in names ) {
        # Check for partial match
        if ( length( map[[n]]) == 0 ) {
            m = NlmeColumnMap(n,"?")
            map[[n]] = m
        }
    }
    return(map)
}




#' NlmeDoseMapping
#'
#' Class represents mapping list between model variables and dose columns
#'
#' @examples
#'    mapping = NlmeDoseMapping("list")
#'
#' @export NlmeDoseMapping
#'
NlmeDoseMapping= setClass("NlmeDoseMapping",representation(
                                      mapping = "list"))


setMethod("initialize","NlmeDoseMapping",
    function(.Object,
              model,
              doseInput){

        .Object@mapping = createInitialDoseMapping(model,doseInput)
        .Object
    })
assign("NlmeDoseMapping",NlmeDoseMapping,envir=.GlobalEnv)

#' createInitialDoseMapping
#'
#' Creates a mapping between model variable and dose columns
#'
#' @param model  A PK/PD model
#' @param doseInput   Column names in dose file
#'
#' @examples
#'    createInitialDoseMapping(model,doseInput)
#'
#' @export createInitialDoseMapping
#'
createInitialDoseMapping <-function(model,doseInput){
    map=list()
    ids=c()
    if ( model@isPopulation == TRUE )
#        ids=c("sort","id")
        ids=c("id")
    if ( attr(model,"isTimeBased") == TRUE )
        ids=c(ids,"time")

    dn = doseNames(model)
    edn = extraDoseNames(model)
    names=c(ids,dn,edn)
    if ( model@hasResetInfo )
      names=c(names,"Reset")
    colNames=colnames(doseInput)

    for ( n in names ) {
        # Check for exact match
        for ( c in colNames ) {
            if ( toupper(c) == toupper(n) ) {
                m = NlmeColumnMap(n,c)
                map[[n]] = m
            }
        }
    }
    for ( n in names ) {
        # Check for partial match
        if ( length( map[[n]]) == 0 ) {
            for ( c in colNames ) {
                if ( length(grep(toupper(n),toupper(c))) != 0 ) {
                    m = NlmeColumnMap(n,c)
                    map[[n]] = m
                }
            }
        }
    }
    for ( n in names ) {
        #  Mark as unassigned
        if ( length( map[[n]]) == 0 ) {
            m = NlmeColumnMap(n,"?")
            map[[n]] = m
        }
    }
    return(map)
}



#' NlmeParamsMapping
#'
#' Class represents mapping list between model variables and data columns
#'
#' @examples
#'    mapping = NlmeParamsMapping("list")
#'
#' @export NlmeParamsMapping
#'
NlmeParamsMapping= setClass("NlmeParamsMapping",representation(
                                      mapping = "list"))

assign("NlmeParamsMapping",NlmeParamsMapping,envir=.GlobalEnv)

setMethod("initialize","NlmeParamsMapping",
    function(.Object,
              model,
              paramsInput){

        .Object@mapping = createInitialParamsMapping(model,paramsInput)
        .Object
    })

#' createInitialParamsMapping
#'
#' Creates a mapping between model variable and parameter columns
#'
#' @param model        A PK/PD model
#' @param paramsInput  Parameter column names
#'
#' @examples
#'    createInitialParamsMapping(model,doseInput)
#'
#' @export createInitialParamsMapping
#'
createInitialParamsMapping <-function(model,paramsInput){
  map=list()
  ids=c("param","init","high","low")
  pn = fixedParameterNames(model)
  names=ids
  for ( p in pn )
    names=c(names,p)
  colNames=colnames(paramsInput)
  for ( c in paramsInput$Parameter )
    colNames=c(colNames,c)

  for ( n in names ) {
    # Check for exact match
    for ( c in colNames ) {
      if ( toupper(c) == toupper(n) ) {
        m = NlmeColumnMap(n,c)
        map[[n]] = m
      }
    }
  }
  for ( n in names ) {
    # Check for partial match
    if ( length( map[[n]]) == 0 ) {
      for ( c in colNames ) {
        if ( length(grep(toupper(n),toupper(c))) != 0 ) {
          m = NlmeColumnMap(n,c)
          map[[n]] = m
        }
      }
    }
  }
  for ( n in names ) {
    #  Mark as unassigned
    if ( length( map[[n]]) == 0 ) {
      if ( n == "param" )
        m = NlmeColumnMap(n,"Parameter")
      else if ( n == "init" )
        m = NlmeColumnMap(n,"Initial")
      else if ( n == "low" )
        m = NlmeColumnMap(n,"Lower")
      else if ( n == "high" )
        m = NlmeColumnMap(n,"Upper")
      else
        m = NlmeColumnMap(n,"?")
      map[[n]] = m
    }
  }
  return(map)
}


#' NlmeRandParamsMapping
#'
#' Class represents mapping list between model variables and Random Effect
#'
#' @examples
#'    mapping = NlmeRandParamsMapping("list")
#'
#' @export NlmeRandParamsMapping
#'
NlmeRandParamsMapping= setClass("NlmeRandParamsMapping",representation(
                                      mapping = "list"))

assign("NlmeRandParamsMapping",NlmeRandParamsMapping,envir=.GlobalEnv)

setMethod("initialize","NlmeRandParamsMapping",
    function(.Object,
              model,
              randEffectInput){

        .Object@mapping = createInitialRandEffectMapping(model,randEffectInput)
        .Object
    })

#' createInitialRandEffectMapping
#'
#' Creates a mapping between model variables and random effects columns
#'
#' @param model            A PK/PD model
#' @param randEffectInput  Random effect data
#'
#' @examples
#'     createInitialRandEffectMapping(model,randEffectInput)
#'
#' @export createInitialRandEffectMapping
#'
createInitialRandEffectMapping <-function(model,randEffectInput){

  map=list()
  pn = randParameterNames(model)
  names=c()
  for ( p in pn )
    names=c(names,p)

  colNames=colnames(randEffectInput)

  for ( n in names ) {
    # Check for exact match
    for ( c in colNames ) {
      if ( toupper(c) == toupper(n) ) {
        m = NlmeColumnMap(n,c)
        map[[n]] = m
      }
    }
  }
  for ( n in names ) {
    # Check for partial match
    if ( length( map[[n]]) == 0 ) {
      for ( c in colNames ) {
        if ( length(grep(toupper(n),toupper(c))) != 0 ) {
          m = NlmeColumnMap(n,c)
          map[[n]] = m
        }
      }
    }
  }
  for ( n in names ) {
    #  Mark as unassigned
    if ( length( map[[n]]) == 0 ) {
      m = NlmeColumnMap(n,"?")
      map[[n]] = m
    }
  }
  return(map)

}



#' modelVariableNames
#'
#' Return model variable names
#'
#' @param model A PK/PD model
#'
#' @examples
#'    modelVariableNames(model)
#'
#' @export modelVariableNames
#'
modelVariableNames <-function(model){

    ids=c()
    if ( model@isPopulation == TRUE )
#        ids=c("sort","id")
        ids=c("id")
    if ( attr(model,"isTimeBased") == TRUE )
        ids=c(ids,"time")

    on = observationNames(model)
    en = observationExtraNames(model)
    cn = covariateNames(model)
    dn = doseNames(model)
    edn= extraDoseNames(model)
    names=c(ids,dn,on,en,cn,edn)
    if ( model@hasResetInfo )
      names=c(names,"Reset")
    return(names)
}
assign("modelVariableNames",modelVariableNames,envir=.GlobalEnv)

#' lookupColname
#'
#' Return column names
#'
#' @param mapping  Mapping list
#' @param vname    Variable name
#'
#' @examples
#'    lookupColname(mapping,vname)
#'
#' @export lookupColname
#'
lookupColname <-function(mapping,vname){
    for ( m in mapping ) {
        varName=attr(m,"variableName")
        colName=attr(m,"columnName")
        if ( vname ==  varName ) {
            return(colName)
        }
    }
    return("")
}

#' writeParamsMapping
#'
#' Write the mapping between model variables and parameter column
#' names to a file
#'
#' @param model    A PK/PD model
#' @param dataset  Source of parameter data
#'
#' @examples
#'    writeParamsMapping(model,dataset)
#'
#' @export writeParamsMapping
#'
writeParamsMapping <-function(model,dataset){

    colMap = model@paramsMapping@mapping
    data = model@fixedParamData

    lines=c()
    modelType = attr(attr(model,"modelType"),"modelType" )

    cols = c("param","init","high","low")
    for ( c in cols ) {
       name=lookupColname(colMap,c)
       if ( name != "" )  {
           line =  paste0(c,"(\"",name,"\")")
           lines=c(lines,line)
       }
    }
    for ( m in colMap ) {
        var = m@variableName
        name = m@columnName
        if ( ( var %in% cols ) == FALSE ) {
            if ( name != "?" && name != "" ) {
                 line=paste0("map(\"",var,"\" <- \"",name,"\")")
                 lines=c(lines,line)
            }
        }
    }

    workingDir = model@modelInfo@workingDir
    filename=paste0(workingDir,"/",dataset@estimatesDefFile)
    dataFilename=paste0(workingDir,"/",dataset@estimatesDataFile)
    appendFlag=FALSE
    for (  l in  lines ) {
        cat(l,file=filename , sep ="\n" ,append=appendFlag)
        appendFlag=TRUE
    }
    header=paste0("##",paste0(cols, collapse=","))

    data[is.na(data)]="."
    cat(header,file=dataFilename,sep="\n",append=FALSE)
    write.table(data,dataFilename,row.names=FALSE,col.names=FALSE,sep=",",
                quote=FALSE,append=TRUE)

}
assign("writeParamsMapping",writeParamsMapping,envir=.GlobalEnv)


#' writeRandParamsMapping
#'
#' Write the mapping between model variables and random effect column
#' names to a file
#'
#' @param model    A PK/PD model
#' @param dataset  Source of random effect data
#'
#' @examples
#'    writeRandParamsMapping(model,dataset)
#'
#' @export writeRandParamsMapping
#'
writeRandParamsMapping <-function(model,dataset){

    colMap = model@randParamsMapping@mapping
    data = model@randParamData

    subjectId=model@columnMapping@mapping$id@columnName

    lines=c()
    line = paste0("id(\"",subjectId,"\")")
    lines=c(lines,line)

    for ( c in colMap ) {
        varName=attr(c,"variableName")
        colName=attr(c,"columnName")
        if ( colName != "?" && colName != "" )  {
            line=paste0("covr(",varName,"<-\"",colName,"\")")
            lines=c(lines,line)
        }
    }

    workingDir = model@modelInfo@workingDir
    filename=paste0(workingDir,"/",dataset@ranEffectDefFile)
    dataFilename=paste0(workingDir,"/",dataset@ranEffectDataFile)
    appendFlag=FALSE
    for (  l in  lines ) {
        cat(l,file=filename , sep ="\n" ,append=appendFlag)
        appendFlag=TRUE
    }

    cols=colnames(data)
    header=paste0("##",paste0(cols, collapse=","))

    cat(header,file=dataFilename,sep="\n",append=FALSE)
    write.table(data,dataFilename,row.names=FALSE,col.names=FALSE,sep=",",
                quote=FALSE,append=TRUE)

}
assign("writeRandParamsMapping",writeRandParamsMapping,envir=.GlobalEnv)

#' writeDoseMapping
#'
#' Write the mapping between model variables and dose column names to a file
#'
#' @param model    A PK/PD model
#' @param dataset  Source of dose data
#'
#' @examples
#'    writeDoseMapping(model,dataset)
#'
#' @export writeDoseMapping
#'
writeDoseMapping <-function(model,dataset){


    lines=c()
    mapping = attr(model,"doseMapping")
    colMap = attr(mapping,"mapping")
    data = model@doseData
    if ( model@isPopulation == FALSE )
        cbind(zzzDummyId = 0, data)

    workingDir = model@modelInfo@workingDir
    filename=paste0(workingDir,"/",dataset@doseDefFile)
    dataFilename=paste0(workingDir,"/",dataset@doseDataFile)

    if ( model@isPopulation == FALSE )
        lines = c(lines,paste0("id(\"zzzDummyId\")"))
    for ( c in colMap ) {
        varName = c@variableName
        colName = c@columnName
        if ( colName != "?" && colName != "" )  {
            if ( varName == "id" || varName == "time" ) {
                line = paste0(varName,"(\"",colName,"\")")
            }
            else
                line = paste0("dose(",varName,"<-\"",colName,"\")")
            lines = c(lines, line)
        }
    }
    append=FALSE
    for ( l in lines ) {
        cat(l,file=filename,sep="\n",append=append)
        append=TRUE
    }

    cols=colnames(data)
    header=paste0("##",paste0(cols, collapse=","))

    cat(header,file=dataFilename,sep="\n",append=FALSE)
    write.table(data,dataFilename,row.names=FALSE,col.names=FALSE,sep=",",
                quote=FALSE,append=TRUE)

}


assign("writeDoseMapping",writeDoseMapping,envir=.GlobalEnv)

#' writeColumnMapping
#'
#' Write the mapping between model variables and column names to a file
#'
#' @param model      A PK/PD model
#' @param filename   Name of the file
#'
#' @examples
#'    writeColumnMapping(model,filename)
#'
#' @export writeColumnMapping
#'
writeColumnMapping <-function(model,filename){

    workingDir = model@modelInfo@workingDir
    fullPath = paste0(workingDir,"/",filename)

    lines=c()
    mapping = attr(model,"columnMapping")
    colMap = attr(mapping,"mapping")
    modelType = attr(attr(model,"modelType"),"modelType" )
    on = observationNames(model)
    en = observationExtraNames(model)
    cn = covariateNames(model)
    dn = doseNames(model)
    eDoseLines=extraDoseLines(model)
    vars=c()
    if ( model@isPopulation == TRUE )
        vars=c("id")
    if ( attr(model,"isTimeBased") == TRUE )
        vars=c(vars,"time")

    if ( model@isPopulation == FALSE )
        lines=c(lines,paste0("id(\"zzzDummyId\")"))
    for ( v in vars ) {
        colName=lookupColname(colMap,v)
#
# Handle multiple ID mapping
#
        tokens = unlist(strsplit(colName,","))
        line= paste0(v,"(")
        for ( t in 1:length(tokens ) ){
            tokens[[t]]=trimws(tokens[[t]],"both")
        }
#        lines=c(lines,paste0(v,"(\"",colName,"\")"))
        cols=""
        for ( t in 1:length(tokens ) ){
            if ( t == 1 ) 
                cols=paste0(cols,"\"",tokens[[t]],"\"")
            else
                cols=paste0(cols,",\"",tokens[[t]],"\"")
        }
        line=paste0(line,cols, ")")
        lines=c(lines,line)
    }

    for ( d in dn ) {
        colName=lookupColname(colMap,d)
        if ( modelType == PARAM_EMAX || modelType == PARAM_LINEAR )
        lines=c(lines,paste0("covr(",d,"<-\"",colName,"\")"))
        else {
            extraDoseInfo = ""
            if ( model@pkModelAttrs@infusionAllowed ) {
                if ( model@pkModelAttrs@isDuration ) {
                    infusionName=paste0(d,"_Duration")
                    dcolName=lookupColname(colMap,infusionName)
                    extraDoseInfo=paste0(", duration=\"",dcolName,"\"")
                } else { 
                    infusionName=paste0(d,"_Rate")
                    dcolName=lookupColname(colMap,infusionName)
                    extraDoseInfo=paste0(", \"",dcolName,"\"")
                }
            }
            lines=c(lines,paste0("dose(",d,"<-\"",colName,"\"",
                               extraDoseInfo,")"))
        }
    }

    lines=c(lines,eDoseLines)

    if ( model@hasResetInfo  ){
      colName = lookupColname(colMap,"Reset")
      lines=c(lines,paste0("reset(\"",colName,"\" , c(",
                           model@resetInfo@low,", ",model@resetInfo@hi,"))"))
    }

    for ( c in cn ) {
        colName=lookupColname(colMap,c)
        lines=c(lines,paste0("covr(",c,"<-\"",colName,"\"",
                covariatePartsString(getCovariate(model,c)),")"))
    }
    for ( o in on ) {
        colName=lookupColname(colMap,o)
        if ( colName != "?" ) {
            bqlName=paste0(o,"BQL")
            if ( length(en) > 0 && any(sapply(en,grepl,bqlName)) == TRUE )
                    s=sprintf("obs(%s<-\"%s\", bql <- \"%s\") ",
                             o,colName,bqlName)
                else
                    s=sprintf("obs(%s<-\"%s\")",o,colName)
            lines = c(lines, s ) 
        }
    }
    first = TRUE
    for ( v in structuralParameterNames(model) ) {
        if ( first == TRUE ) {
            table=sprintf("table(file=\"posthoc.csv\",time(0),")
            first = FALSE
        }
        else
            table=paste0(table,",")
        table=paste0(table,v)
    }
    table=paste0(table,")")
    lines=c(lines,table)

    if ( length(model@userDefinedExtraDefs) != 0 ) {
        for ( l in model@userDefinedExtraDefs ) {
            lines=c(lines,l)
        }
    }

    append=FALSE
    for ( l in lines ) {
        cat(l,file=fullPath,sep="\n",append=append)
        append=TRUE
    }
    return(lines)

}


assign("writeColumnMapping",writeColumnMapping,envir=.GlobalEnv)

#' writeInputData
#'
#' Write the input data for the model to a file
#'
#' @param model         A PK/PD model
#' @param datafileName  Input data file
#'
#' @examples
#'    writeDoseMapping(model,datafileName)
#'
#' @export writeInputData
#'
writeInputData <-function(model,datafileName){

    inputData = model@inputData
    if ( model@isPopulation == FALSE )
        inputData = cbind(zzzDummyId = 0, inputData)
    workingDir = model@modelInfo@workingDir
    if ( !dir.exists(workingDir ) ) {
        dir.create(workingDir)
    }
###    setwd(workingDir)

    fullPath = paste0(workingDir,"/",datafileName)
    colnames=colnames(inputData)
    header=paste0("##",paste(colnames,collapse=","))
    cat(header,file=fullPath,append=FALSE,sep="\n")
    write.table(inputData,fullPath,row.names=FALSE, col.names=FALSE,sep=",",
            quote=FALSE,append=TRUE)
}

assign("writeInputData",writeInputData,envir=.GlobalEnv)


#' addTablesToColumnMapping
#'
#' Add simulation tables to the mapping
#'
#' @param model       A PK/PD model
#' @param simParams   Simulation parameters
#' @param filename    Name of simulation table file
#'
#' @examples
#'        addTablesToColumnMapping(model,vpcParam,filename)
#'
#' @export addTablesToColumnMapping
#'
addTablesToColumnMapping <-function(model,simParams,filename){

    workingDir = model@modelInfo@workingDir
    filename=paste0(workingDir,"/",filename)
    lines=c()
    tables = attr(simParams,"simulationTables")
    on = observationNames(model)
    en = observationExtraNames(model)
    cn = covariateNames(model)
    dn = doseNames(model)

    for ( t in tables ) {
        tabName=attr(t,"name")
        tString=attr(t,"timesList")
        vString=attr(t,"variablesList")
        isTad=attr(t,"timeAfterDose")
        line=paste0("simtbl(file=\"",tabName,"\",")
        if ( tString != "" ) {
            line=paste0(line,"time(",tString,"),")
        }
        else
            line=paste0(line,"time(0),")
        if ( vString != "" ) {
            line=paste0(line,vString)
        }
        if ( isTad == TRUE ) {
            line=paste0(line,",specvar(TAD)")
        }
        line=paste0(line,")")
        lines=c(lines,line)
    }


    for ( l in lines ) {
        cat(l,file=filename,sep="\n",append=TRUE)
    }
    return(lines)

}


assign("addTablesToColumnMapping",addTablesToColumnMapping,envir=.GlobalEnv)



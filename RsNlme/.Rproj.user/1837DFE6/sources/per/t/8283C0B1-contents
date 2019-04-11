#' @export
Diagonal =  1
#' @export
Block =  2

#' NlmeRandomEffectBlock
#'
#' Random effect block definition
#'
#' @param type          Diagonal or Block
#' @param effectNames   Names of random effects
#' @param frozen        Are random effects frozen?
#'
#' @examples
#'       NlmeRandomEffectBlock(Diagonal,list("Cl","V"),FALSE)
#'
#' @export 
#'
NlmeRandomEffectBlock= setClass("NlmeRandomEffectBlock",representation(
                                      type="numeric",
                                      effectNames="list",
                                      frozen="logical"))


setMethod("initialize","NlmeRandomEffectBlock",
    function(.Object,
              type,
              effectNames,
              frozen=FALSE){

        .Object@type=type
        .Object@effectNames=effectNames
        .Object@frozen=frozen
     
        .Object
    })
assign("NlmeRandomEffectBlock",NlmeRandomEffectBlock,envir=.GlobalEnv)


#' NlmeRandomEffectValues 
#'
#' Random effect values matrix
#'
#' @param numEffects    Number of random effects
#' @param effectNames   Names of random effects
#' @param values        Matrix of random effect values
#'
#' @examples
#'       NlmeRandomEffectValues(2,c("Cl","V"),c(0.1,0,0,0,0,0.1))
#'
#' @export 
#'
NlmeRandomEffectValues= setClass("NlmeRandomEffectValues",representation(
                                      effectNames="list",
                                      effectValues="list",
                                      values="matrix"))

assign("NlmeRandomEffectValues",NlmeRandomEffectValues,envir=.GlobalEnv)

setMethod("initialize","NlmeRandomEffectValues",
    function(.Object,
              effectNames,
              effectValues=list(),
              values=NULL){

        numEffects = length(effectNames)
        .Object@effectNames=effectNames
        if ( is.null(values) ) {
            values = matrix(data=1,nrow=numEffects,ncol=numEffects,
                            dimnames=list(effectNames,effectNames))
            for ( r in 1:numEffects ) {
                for ( c in 1:numEffects ) {
                    if ( c != r )  {
                        values[r,c]=0
                    } else {
                        if ( length(effectValues) != 0 ) 
                            values[r,c] = effectValues[[c]]
                    }
                }

            }
        }
        .Object@values=values
     
        .Object
    })


#'
#' @export
#'
getRandomEffectNames <- function(model){

    names=c()

    sps = model@structuralParams

    for ( sp in sps ) {
        if ( sp@hasRandomEffect == TRUE ) {
            names=c(names,sp@randomEffName ) 
        }
    }

    names
}

    



#' initRandomEffects
#'
#' Initializes the random effect statement. It should be used after all model components have been created.
#'
#' @param model    PK/PD model
#' @param values   Random effect values specified as a PML string OR one or
#'                 more sets of quadruplet values
#'
#' @examples
#'      initRandomEffects(model)=c(PMLStringForRanef)
#'
#'      initRandomEffects(model)=c(BlockOrDiagonal,
#'                                 isFrozen,
#'                                 listOfVariables,
#'                                 listOfValues)
#'
#'      initRandomEffects(model)=c(Block,
#'                                 FALSE,
#'                                 "nV,nCl,nKa,nV2",
#'                                 "0.2, 0, 0.2, 0, 0, 0.2, 0, 0, 0, 0.1")
#'
#'      initRandomEffects(model)=c(Diagonal, FALSE,"nV,nCl","0.1, 0.02")
#'
#'      initRandomEffects(model)=c(Diagonal, FALSE,"nV,nCl","0.1, 0.02",
#'                                 Block, TRUE, "nCl2,nV2","0.2, 0, 0.2")
#'
#'      initRandomEffects(model)= c("block(nCl, nV)(freeze) = 
#'                                c(0.2, 0, 0.2), diag(nKa)(freeze) = 
#'                                c(0.2), diag(nV2) = 
#'                                c(0.1), diag(nV3, nCl2, nCl3) = 
#'                                c(1, 1, 1)")
#'
#' @export
#'
setGeneric(name="initRandomEffects",
           def=function(.Object)
           {
               standardGeneric("initRandomEffects")
           })
setMethod(f="initRandomEffects",
    signature="NlmePmlModel",
    definition=function(.Object){

    if ( .Object@isTextual )  {
        pos = grep("ranef\\(",.Object@statements)
        if ( length(pos) != 0 ) 
            statement = .Object@statements[[pos[[1]]]]
    } else {
        statement = randomBlockStatement(.Object)
    }
    statement
})
assign("initRandomEffects",initRandomEffects,envir=.GlobalEnv)

#'
#' @export
#'
setGeneric(name="initRandomEffects<-",
           def=function(.Object,values)
           {
               standardGeneric("initRandomEffects<-")
           })

setMethod(f="initRandomEffects<-",
    signature="NlmePmlModel",
    definition=function(.Object,values){
    effectsParams=attr(.Object,"effectsParams")
    randomEffectsStatements=attr(.Object,"randomEffectsStatements")
        sps = attr(.Object,"structuralParams")
    if ( .Object@isTextual )  {
        pos = grep("ranef\\(",.Object@statements)
        stopifnot( length(pos) > 0 )

        line=.Object@statements[[pos[[1]]]]
        line = unlist(strsplit(line,"ranef\\("))[[2]]
        line = gsub("\\s+","",line)
        tokens=unlist(strsplit(line,"\\)\\(|\\)=c\\(|\\(|\\)=c|\\),|\\)=|="))
        indx=1
        blocks=c()
        while ( indx < length(tokens) ) {
            typ=tokens[[indx]]
            if ( typ == "diag" )
                typ = Diagonal 
            else
                typ = Block 
            indx = indx + 1
            names=tokens[[indx]]
            indx = indx + 1
            nxt=tokens[[indx]]
            indx = indx + 1
            freeze = FALSE
            if ( nxt == "freeze" )  {
                vals=tokens[[indx]]
                freeze = TRUE
                indx = indx + 1
            }
            block = NlmeRandomEffectBlock(typ,
                                          as.list(unlist(strsplit(names,","))),
                                          freeze)
            blocks=c(blocks,block)
        }
#browser()
         .Object@randomBlocks = as.list(blocks)
         rv=NlmeRandomEffectValues(as.list(dimnames(values)[[1]]),
                                   values=values)
         .Object@randomValues = rv
         ranStatement = randomBlockStatement(.Object)
         .Object@statements[[pos[[1]]]] = ranStatement

    } else {
      if ( length(values) == 1 ) {
          .Object@randomValues@values = values 
      } else  {
        .Object@randomValuesInitialized = FALSE
        .Object=initializeRandomEffectsBlock(.Object)
        randomEffectNames = getRandomEffectNames(.Object)
        len = length(values)
        usedVars=c()
        if ( len %% 4 != 0 ) {
            print("Usage : initRandomEffects() wrong number of arguments")
            return(.Object)
        } else {
            .Object@randomBlocks=list()
            str=""
            num=len/4
            for ( indx in 1:num ) {
                if ( indx > 1 )
                   str=paste0(str,", ")
                else
                   str=""
                what = values[[(indx-1)*4+1]]
                frozen = as.logical(values[[(indx-1)*4+2]])
                vars = values[[(indx-1)*4+3]]
                names=unlist(strsplit(vars,split=","))
                for ( n in names ) {
                    pos = grep(paste0("^",trimws(n,"both"),"$"),randomEffectNames)
                    if (length(pos) == 0 ) {
                        stop(paste0(n ," : Is not a valid random effect name"))
                    }
                }
                vals = values[[(indx-1)*4+4]]
                typ=Diagonal
                if ( what == Block )
                    typ=Block
                else
                    typ=Diagonal
                block = NlmeRandomEffectBlock(typ,
                                              as.list(names),
                                              frozen)
                .Object@randomBlocks[[indx]]=block
                vals=unlist(strsplit(vals,split=","))
                usedVars = c(usedVars,names)
                if ( typ == Diagonal ) {
                    for ( i in 1:length(vals))
                        .Object@randomValues=updateValue(.Object@randomValues,
                                                     names[[i]],
                                                     vals[[i]])
                } else { 
                    indx=1
                    for ( i in 1:length(names))
                      for ( j in 1:i ) {
                        .Object@randomValues=updateValue2(.Object@randomValues,
                                                     names[[i]],
                                                     names[[j]],
                                                     vals[[indx]])
                        indx= indx + 1
                      }
                }

            }
            usedVars = unique(usedVars)
        }
        if ( length(randomEffectNames ) != length(usedVars ) )  {
            extraVars=c()
            for ( r in randomEffectNames ) {
                pos = grep(paste0("^",trimws(r,"both"),"$"),usedVars)
                if ( length(pos) == 0 ) {
                    extraVars = c(extraVars,r)
                } else { 
                    if ( length(extraVars ) != 0 ) {
                        block = NlmeRandomEffectBlock(Diagonal,
                                              as.list(extraVars),
                                              TRUE)
                      .Object@randomBlocks[[length(.Object@randomBlocks)+1]]=block
                       extraVars=c()
                    }
                }
            }
            if ( length(extraVars ) != 0 ) {
                block = NlmeRandomEffectBlock(Diagonal,
                                              as.list(extraVars),
                                              TRUE)
                .Object@randomBlocks[[length(.Object@randomBlocks)+1]]=block
            }
        }
        .Object@randomEffectsStatements=as.list(str)
      }
    }
    .Object=generatePML(.Object)
    return(.Object)
})




#' initializeRandomEffectsBlock
#'
#' Initializes random effects structure from structural parameters
#'
#' @param model   PK/PD model
#'
#' @export
#'
setGeneric(name="initializeRandomEffectsBlock",
           def=function(.Object)
           {
             standardGeneric("initializeRandomEffectsBlock")
           })
setMethod(f="initializeRandomEffectsBlock",
          signature="NlmePmlModel",
          definition=function(.Object){
            if ( .Object@randomValuesInitialized == FALSE ) {
            structuralParams=attr(.Object,"structuralParams")
            names=c()
            estimates=c()
            if ( length(structuralParams ) > 0 )
                for ( i in 1:length(structuralParams)) {
                  stp=structuralParams[[i]]
                  name = attr(stp,"name")
                  randomEffName = attr(stp,"randomEffName")
                  ranEffInitValue = attr(stp,"ranEffInitValue")
                  if ( randomEffName != "" ) {
                    estimates=c(estimates,ranEffInitValue)
                    names=c(names,randomEffName)
                  }
                }
                if ( length(names) > 0 ) {
                    values = NlmeRandomEffectValues(as.list(names),effectValues=as.list(estimates))
                    .Object@randomValues = values
                    .Object@randomBlocks = c(NlmeRandomEffectBlock(
                                     Diagonal,as.list(names)))
                    .Object@randomValuesInitialized = TRUE
                }
            }
            .Object
          })

assign("initializeRandomEffectsBlock",initializeRandomEffectsBlock,envir=.GlobalEnv)



lookupValue <-function (randomValues,effName){
   
    pos = grep(paste0("^",trimws(effName,"both"),"$"),randomValues@effectNames)
    val = randomValues@values[pos,pos]
    val
}

lookupValue2 <-function (randomValues,effName,effName2){
   
    pos = grep(paste0("^",trimws(effName,"both"),"$"),randomValues@effectNames)
    pos2 = grep(paste0("^",trimws(effName2,"both"),"$"),randomValues@effectNames)
    val = randomValues@values[pos,pos2]
    val
}

updateValue <-function (randomValues,effName,value){
   
    pos = grep(paste0("^",trimws(effName,"both"),"$"),randomValues@effectNames)
    randomValues@values[pos,pos] = value
    randomValues
}

updateValue2 <-function (randomValues,effName,effName2,value){
   
    pos = grep(paste0("^",trimws(effName,"both"),"$"),randomValues@effectNames)
    pos2 = grep(paste0("^",trimws(effName2,"both"),"$"),randomValues@effectNames)
    randomValues@values[pos,pos2] = value
    randomValues
}


#' randomBlockStatement
#'
#' Returns random block statement
#'
#' @param model   PK/PD model
#'
#' @export
#'
setGeneric(name="randomBlockStatement",
           def=function(.Object)
           {
             standardGeneric("randomBlockStatement")
           })

setMethod(f="randomBlockStatement",
          signature="NlmePmlModel",
          definition=function(.Object){
            structuralParams=attr(.Object,"structuralParams")
            names=c()
            estimates=c()
            statement=""
            if ( length(structuralParams ) > 0 )
                for ( i in 1:length(structuralParams)) {
                  stp=structuralParams[[i]]
                  name = attr(stp,"name")
                  randomEffName = attr(stp,"randomEffName")
                  ranEffInitValue = attr(stp,"ranEffInitValue")
                  if ( randomEffName != "" ) {
                    estimates=c(estimates,ranEffInitValue)
                    names=c(names,randomEffName)
                  }
                }
          if ( length(.Object@randomBlocks) > 0 ) {
              statement = ""
              firstBlock=TRUE
              for ( b in .Object@randomBlocks ) {
                  if ( b@type == Diagonal )
                      s = "diag("
                  else
                      s = "block("
                  first=TRUE
                  for ( v in b@effectNames ) {
                      if ( first == FALSE ) 
                          s = paste0(s,",")
                      s = paste0(s,v)
                      first=FALSE
                  }
                  s = paste0(s,")")
                  if ( b@frozen == TRUE )
                    s=paste0(s," (freeze) ")
                  s = paste0(s," = c(")
                  first=TRUE
                  if( b@type == Diagonal ) {
                    for ( i in 1:length(b@effectNames) ) {
                      v = b@effectNames[[i]]
                      val = lookupValue(.Object@randomValues,v)
                      if ( first == FALSE ) 
                          s = paste0(s,",")
                      s = paste0(s,val)
                      first=FALSE
                    }
                    s = paste0(s,")") 
                  }
                  else {
                    for ( i in 1:length(b@effectNames) ) {
                       for ( j in  1:i ) {
                           if ( i == j ) {
                              v = b@effectNames[[i]]
                              val = lookupValue(.Object@randomValues,v)
                           } else {
                              v = b@effectNames[[i]]
                              v2 = b@effectNames[[j]]
                              val = lookupValue2(.Object@randomValues,v,v2)
                           }
                          if ( first == FALSE ) 
                              s = paste0(s,",")
                          s = paste0(s,val)
                          first=FALSE
                       }
                    }
                    s = paste0(s,")") 
                  }
                  if ( firstBlock == TRUE )
                      statement=paste0(statement,s)
                  else
                      statement=paste0(statement,",",s)
                  firstBlock=FALSE
              }
              statement=paste0("    ranef(",statement,")")
          }
          statement
          })

assign("randomBlockStatement",randomBlockStatement,envir=.GlobalEnv)





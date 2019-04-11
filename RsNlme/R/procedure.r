
#' NlmeProcedure
#'
#' Class represents a procedure block
#'
#' @param structuralParams  List of structural parameters to define
#' @param codeLines         Lines of PML code to define continuous
#'                          function, including differential equations 
#'                          and logical statement
#'
#' @examples
#'      Creates a procedure block and adds it to the object model
#'
#'      proc = NlmeProcedure(structuralParams=list("Cl","C"),
#'                           codeLines=list("deriv(A1=Ka*Aa -Cl*C" ))
#'      addCompartment(model) = proc
#'
#' @export NlmeProcedure
#'
NlmeProcedure = setClass("NlmeProcedure",representation(
                               structuralParams="list",
                               isFrozen="list",
                               hasRandomEffect="list",
                               codeLines="list",
                               inPortNames="list",
                               outPortNames="list",
                               intInPortNames="list"),
                               contains = "StructuralBlock")

assign("NlmeProcedure",NlmeProcedure,envir=.GlobalEnv)

setMethod("initialize","NlmeProcedure",
    function(.Object,
             ...,
             structuralParams=list(),
             isFrozen=list(),
             hasRandomEffect=list(),
             codeLines=list()) {

        .Object@structuralParams         = as.list(structuralParams)
        .Object@codeLines                = as.list(codeLines)
        .Object@isFrozen              = as.list(isFrozen)
        .Object@hasRandomEffect           = as.list(hasRandomEffect)
        # Inport names are all structural parameters
        .Object@inPortNames=as.list(structuralParams)
        .Object@intInPortNames=as.list(structuralParams)

        # Output names are LHS of expressions
        .Object@outPortNames = grabPortNames(codeLines)

        callNextMethod(.Object,
                       ...,
                       structuralParams=structuralParams,
                       codeLines=codeLines)
    })

grabPortNames <- function(lines){

  names=c()
  #Look for =
  if ( length(lines ) > 0 ) {
    for ( line in lines ){

      tokens=unlist(strsplit(line,"="))

      if ( length(tokens) >= 2 ) {
        tokens=unlist(strsplit(tokens[[1]], "[( ]+" ))
        name=tokens[[length(tokens)]]

        names=c(names,name)
      }
    }

  }

  as.list(names)

}


#' print.NlmeProcedure
#'
#' Prints procedure block information
#'
#' @param obj    Model compartment
#'
#' @examples
#'       print.NlmeProcedure(obj="proc")
#'
#' @export print.NlmeProcedure
#'
print.NlmeProcedure <-function(x, ...)
{
  print(paste0("-- Procedure --"))
  print(paste0("ID             : ",x@compKey))
  print(paste0("name           : ",x@blockName))
  if ( length(x@structuralParams) > 0 ) {
    for ( s in x@structuralParams ) {
      print(paste0("sp             : ",s))
    }
  }
  if ( length(x@codeLines )> 0 ) {
    line=1
    for ( s in x@codeLines ) {
      print(paste0("Code Line",line,"    : ",s))
      line=line+1
    }
  }
  if ( length(x@inPortNames )> 0 ) {

    for ( indx in 1:length(x@inPortNames) ) {
      name=x@inPortNames[[indx]]
      if ( x@intInPortNames[[indx]] != "" )
        name = x@intInPortNames[[indx]]
      print(paste0("Input Port",indx,"   : ", name))
      line=line+1
    }
  }
  if ( length(x@outPortNames )> 0 ) {

    for ( indx in 1:length(x@outPortNames) ) {
      name=x@outPortNames[[indx]]

      print(paste0("Output Port",indx,"  : ", name))
      line=line+1
    }
  }

}

setGeneric(name="initObjFixedEffects",
           def=function(.Object,values)
           {
             standardGeneric("initObjFixedEffects")
           })

setMethod("initObjFixedEffects",
          signature="NlmeProcedure",
          definition = function(.Object,values){
          codeLines = .Object@codeLines
#browser()
#          pos=grep("fixef\\(",codeLines)
#          if ( length(pos) > 0 ) {
#              for ( p in pos) {
#                  line = codeLines[[p]]
#                  newLine = updateFixedEffectStr(codeLines[[p]],values)
#                  codeLines[[p]]=newLine
#              }
#          }
#
#          .Object@codeLines = codeLines
            .Object
          })

assign("initObjFixedEffects",initObjFixedEffects,envir=.GlobalEnv)


setMethod("clearInPortNames",
          signature="NlmeProcedure",
          definition = function(.Object){
            if ( length(.Object@intInPortNames) > 0 ) {
              for ( indx in 1:length(.Object@intInPortNames)){
                .Object@intInPortNames[[indx]]=""
              }
            }
            .Object
          })
setMethod("getNumInPorts",
          signature="NlmeProcedure",
          definition = function(.Object){
            numInports = length(.Object@inPortNames)

            numInports

          })



setMethod("getNumOutPorts",
          signature="NlmeProcedure",
          definition = function(.Object){
            numOutports = length(.Object@outPortNames)

            numOutports

          })

setMethod("getInPortName",
          signature="NlmeProcedure",
          definition = function(.Object,indx){
            #         print("NlmeCompartment getInPortName()")

            if ( .Object@intInPortNames[[indx]] == "" )
              name = .Object@inPortNames[[indx]]
            else
              name = .Object@intInPortNames[[indx]]

              name
          })

setMethod("getOutPortName",
          signature="NlmeProcedure",
          definition = function(.Object,indx){
            .Object@outPortNames[[indx]]
          })


setMethod("setInPortName",
          signature="NlmeProcedure",
          definition = function(.Object,indx,name){

            .Object@intInPortNames[[indx]] = name

            .Object
          })


setMethod("genStructuralParameters",
          signature="NlmeProcedure",
          definition = function(.Object,hasRandomEffect){
            sps=c()

            if ( length(.Object@inPortNames) > 0 ) {

              for ( indx in 1:length(.Object@structuralParams ) ) {
                s = .Object@inPortNames[[indx]]
                if ( .Object@intInPortNames[[indx]] == "" ){
                  isFrozen = FALSE
                  if ( length(.Object@isFrozen) > 0 ) 
                      isFrozen = .Object@isFrozen[[indx]]
                  hasRanEffect = hasRandomEffect
                  if ( length(.Object@hasRandomEffect) > 0 ) 
                      hasRanEffect = .Object@hasRandomEffect[[indx]]
                  sp = NlmeStructuralParameter(name=s,
                                                hasRandomEffect = hasRanEffect ,
                                                hasCovariateEffect = FALSE,
                                                isFrozen = isFrozen,
                                                 isSequential  = .Object@isSequential)
                  sps = c(sps,sp)
                }
              }
            }
            sps
          })

setMethod("genStatements",
          signature="NlmeProcedure",
          definition = function(.Object){
            statements=c()

            if ( length(.Object@codeLines ) > 0 ) {
              for ( line in .Object@codeLines ) {
                statements=c(statements,line)
              }
            }
            if ( length(.Object@inPortNames ) > 0 ) {

              for ( indx in 1:length(.Object@inPortNames ) ) {
                s = .Object@intInPortNames[[indx]]
                v = .Object@inPortNames[[indx]]
                if ( s != "" ) {
                  statement = paste0(v, " = ", s)
                  statements = c(statements, statement)
                }

              }
            }
            statements
          })
setMethod("genDoseStatements",
          signature="NlmeProcedure",
          definition = function(.Object){
            statements=c()



            statements
          })

setMethod("genFlowRateODE",
          signature="NlmeProcedure",
          definition = function(.Object,key,model){
            statement = ""


            statement
          })



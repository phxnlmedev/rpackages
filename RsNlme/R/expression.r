
#' NlmeExpression
#'
#' Defines a PML Expression block with optional structural parameters
#'
#' @param  blockName         Name of the expression block and LHS of assignment
#' @param  structuralParams  List of structural parameters to define
#' @param  codeLine          PML statement for expression
#'
#' @examples
#'
#' expr1 = NlmeExpression(blockName = "C2",
#'                        structuralParams=c("Vp"),
#'                        codeLine=c("Ap / Vp"))
#'
#' addCompartment(model1) = expr1
#'
#' @export NlmeExpression
#'
NlmeExpression = setClass("NlmeExpression",representation(
                               structuralParams="ANY",
                               codeLine="character",
                               inPortNames="list",
                               outPortName="character",
                               intInPortNames="list",
                               isFrozen="list",
                               hasRandomEffect ="list"),
                               contains = "StructuralBlock")


setMethod("initialize","NlmeExpression",
    function(.Object,
             ...,
             blockName,
             structuralParams=NULL,
             codeLine="" ,
             isFrozen = list(),
             hasRandomEffect  = list() ) {

        .Object@structuralParams        = structuralParams
        .Object@codeLine                = codeLine
        .Object@isFrozen             = isFrozen
        .Object@hasRandomEffect           = hasRandomEffect 
        # Inport names are all structural parameters
        if ( is.null(structuralParams) )  {
          .Object@inPortNames=list()
          .Object@intInPortNames=list()
        } else {
          .Object@inPortNames=as.list(structuralParams)
          .Object@intInPortNames=list()

          for ( indx in 1:length(.Object@inPortNames)){
            .Object@intInPortNames[[indx]]=""
          }

        }

        # Output name is name of the block
        .Object@outPortName = blockName

        callNextMethod(.Object,
                       ...,
                       structuralParams=structuralParams,
                       codeLine=codeLine,
                       isFrozen=isFrozen,
                       hasRandomEffect =hasRandomEffect ,
                       blockName=blockName)
    })

assign("NlmeExpression",NlmeExpression,envir=.GlobalEnv)

#' print.NlmeExpression
#'
#' Prints Expression block information
#'
#' @param obj      Expression block name
#'
#' @examples
#'       print.NlmeExpression(obj="C2")
#'
#' @export print.NlmeExpression
#'
print.NlmeExpression <-function(x, ...)
{
  print(paste0("-- Expression --"))
  print(paste0("ID             : ",x@compKey))
  print(paste0("name           : ",x@blockName))
  if ( length(x@structuralParams) > 0 ) {
    for ( s in x@structuralParams ) {
      print(paste0("sp             : ",s))
    }
  }
  if ( length(x@codeLine )> 0 ) {
    line=1
    for ( s in x@codeLine ) {
      print(paste0("Expression : ",x@blockName," = ",s))
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
   {

    for ( indx in 1:length(x@outPortName) ) {
      name=x@outPortName

      print(paste0("Output Port",indx,"  : ", name))
      line=line+1
    }
  }

}

setMethod("clearInPortNames",
          signature="NlmeExpression",
          definition = function(.Object){
            if ( length(.Object@intInPortNames) > 0 ) {
              for ( indx in 1:length(.Object@intInPortNames)){
                .Object@intInPortNames[[indx]]=""
              }
            }
            .Object
          })


setMethod("getNumInPorts",
          signature="NlmeExpression",
          definition = function(.Object){
            numInports = length(.Object@inPortNames)

            numInports

          })



setMethod("getNumOutPorts",
          signature="NlmeExpression",
          definition = function(.Object){
            numOutports = 1

            numOutports

          })

setMethod("getInPortName",
          signature="NlmeExpression",
          definition = function(.Object,indx){
            #         print("NlmeCompartment getInPortName()")

            if ( .Object@intInPortNames[[indx]] == "" )
              name = .Object@inPortNames[[indx]]
            else
              name = .Object@intInPortNames[[indx]]

              name
          })

setMethod("getOutPortName",
          signature="NlmeExpression",
          definition = function(.Object,indx){
            .Object@outPortName
          })


setMethod("setInPortName",
          signature="NlmeExpression",
          definition = function(.Object,indx,name){

            .Object@intInPortNames[[indx]] = name

            .Object
          })


setMethod("genStructuralParameters",
          signature="NlmeExpression",
          definition = function(.Object,hasRandomEffect){
            sps=c()

            if ( length(.Object@inPortNames) > 0 ) {

              for ( indx in 1:length(.Object@structuralParams ) ) {
                s = .Object@inPortNames[[indx]]
                if ( .Object@intInPortNames[[indx]] == "" ){
                  if ( length(.Object@isFrozen) == 0 ) 
                      isFrozen = FALSE
                  else
                      isFrozen = .Object@isFrozen[[indx]]
                  if ( length(.Object@hasRandomEffect ) == 0 ) 
                      hasRanEffect = FALSE
                  else
                      hasRanEffect = .Object@hasRandomEffect [[indx]]
                  sp = NlmeStructuralParameter(name=s,
                                                 isFrozen = isFrozen,
                                                 hasRandomEffect = hasRanEffect,
                                                 hasCovariateEffect = FALSE,
                                                 isSequential  = .Object@isSequential)
                  sps = c(sps,sp)
                }
              }
            }
            sps
          })

setMethod("genStatements",
          signature="NlmeExpression",
          definition = function(.Object){
            statements=c()

            if ( length(.Object@codeLine ) > 0 ) {
              for ( line in .Object@codeLine ) {
                statement = paste0( .Object@blockName," = ",line)
                statements=c(statements,statement)
              }
            }
            if ( length(.Object@inPortNames) > 0 ) {

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
          signature="NlmeExpression",
          definition = function(.Object){
            statements=c()



            statements
          })

setMethod("genFlowRateODE",
          signature="NlmeExpression",
          definition = function(.Object,key,model){
            statement = ""


            statement
          })



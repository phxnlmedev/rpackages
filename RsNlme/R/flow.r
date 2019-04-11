
#' @export
FpMicro=1
#' @export
FpClearance=2
#' @export
FpSaturating=3

FlowTypeNames = c("Micro","Clearance","Saturating")

#' PkFlow
#'
#' Class represents PK flow
#'
#' @param c1Name             Concentration variable name
#' @param flowName           Name of flow
#' @param k12Name            Name for the backward PK flow
#' @param k21Name            Name for the forward flow
#' @param kmName             Name for the fractional metabolic rate
#' @param vMaxName           Name for the maximum  metabolic rate
#' @param flowType           1=FpMicro, 2=FpClearance, 3=FpSaturating
#' @param fromPort           Identifier of the source object's port
#' @param toPort             Identifier of the target object's port
#' @param derivedVascularClearance  Name for the clearance variable
#' @param twoWay             Is the flow two-way (TRUE) or one-way (FALSE)
#' @param isVascular         Is the flow vascular
#' @param fromReceptorPort
#'
#' @examples
#'       
#' @export PkFlow
#'
PkFlow = setClass("PkFlow",representation(
                               clName="character",
                               flowName="character",
                               k12Name="character",
                               k21Name="character",
                               kmName="character",
                               vMaxName="character",
                               iclName="character",
                               ik12Name="character",
                               ik21Name="character",
                               ikmName="character",
                               ivMaxName="character",
                               flowType="numeric",
                               fromPort="numeric",
                               toPort="numeric",
                               derivedVascularClearance="character",
                               twoWay ="logical",
                               isVascular="logical",
                               fromReceptorPort="numeric"),
                               contains = "StructuralBlock")

setMethod("initialize", "PkFlow", 
    function(.Object, ...) {
      .Object <- callNextMethod()
      .Object
    })

setMethod("initialize","PkFlow",
    function(.Object,
             ...,
             flowType=FpClearance,
             clName="",
             flowName="",
             k12Name="",
             k21Name="",
             kmName="",
             vMaxName="",
             fromPort=-1,
             fromReceptorPort=-1,
             twoWay=FALSE,
             isVascular=FALSE,
             toPort=-1,
             blockName="") {

        .Object@flowType         = flowType
        .Object@clName           = clName
        .Object@flowName         = flowName
        .Object@k12Name          = k12Name
        .Object@k21Name          = k21Name
        .Object@kmName           = kmName
        .Object@vMaxName         = vMaxName
        .Object@iclName           = ""
        .Object@ik12Name          = ""
        .Object@ik21Name          = ""
        .Object@ikmName           = ""
        .Object@ivMaxName         = ""
        .Object@fromPort         = fromPort
        .Object@toPort           = toPort
        .Object@fromReceptorPort = fromReceptorPort
        .Object@twoWay = twoWay
        .Object@isVascular = isVascular
        .Object@derivedVascularClearance = clName
        if ( blockName == "" )
          blockName = clName

        callNextMethod(.Object,
                       ...,
                       flowType=flowType,
                       clName=clName,
                       flowName=flowName,
                       k12Name=k12Name,
                       k21Name=k21Name,
                       kmName=kmName,
                       vMaxName=vMaxName,
                       fromPort=fromPort,
                       toPort=toPort,
                       twoWay=twoWay,
                       isVascular = isVascular,
                       fromReceptorPort=fromReceptorPort,
                       blockName=blockName)
    })

assign("PkFlow",PkFlow,envir=.GlobalEnv)

#' print.PkFlow
#'
#' Prints PK Flow information
#'
#' @param obj      Model flow
#'
#' @examples
#'       print.PkFlow(obj="flowA")
#'
#' @export print.PkFlow
#'
print.PkFlow <-function(x, ...)
{
  print(paste0("----  PkFlow ----"))
  print(paste0("ID             : ",x@compKey))
  print(paste0("Name           : ",x@blockName))
  print(paste0("flowType       : ",FlowTypeNames[x@flowType]))
  print(paste0("Names          : ",x@clName," ",x@k12Name," ",x@k21Name," ",
                                      x@kmName," " , x@vMaxName))
  print(paste0("from           : ",x@fromPort))
  print(paste0("to             : ",x@toPort))

}

setMethod("clearInPortNames",
          signature="PkFlow",
          definition = function(.Object){

            .Object@iclName=""
            .Object@ik12Name=""
            .Object@ik21Name=""
            .Object@ikmName=""
            .Object@ivMaxName=""
            .Object
          })

setMethod("getNumInPorts",
          signature="PkFlow",
          definition = function(.Object){
            numInPorts = 1
            if ( .Object@flowType == FpClearance ) {
              numInPorts = 1
            }
            if (.Object@flowType == FpMicro ){
              numInPorts = 1
              if ( .Object@twoWay ) 
                  numInPorts = 2
            }
            if (.Object@flowType == FpMicro||.Object@flowType == FpSaturating) {
              numInPorts = 2
            }
            numInPorts

          })



setMethod("getNumOutPorts",
          signature="PkFlow",
          definition = function(.Object){
            numOutPorts = 0

            numOutPorts

          })

setMethod("getInPortName",
          signature="PkFlow",
          definition = function(.Object,indx){
            name = ""
            if ( .Object@flowType == FpClearance ) {
            if ( .Object@iclName == "" )
              name = .Object@clName
            else
              name = .Object@iclName

            }
            if (.Object@flowType == FpMicro ) {
              if ( indx == 1 ) {
                if ( .Object@ik12Name == "" )
                  name = .Object@k12Name
                else
                  name = .Object@ik12Name
 
              } else {
                if ( .Object@ik21Name == "" )
                  name = .Object@k21Name
                else
                  name = .Object@ik21Name
              }
            }
            if ( .Object@flowType == FpSaturating) {
              if ( indx == 1 ) {
                if ( .Object@ikmName == "" )
                  name = .Object@kmName
                else
                  name = .Object@ikmName
 
              } else {
                if ( .Object@ivMaxName == "" )
                  name = .Object@vMaxName
                else
                  name = .Object@ivMaxName
              }
            }
            name
          })

setMethod("setInPortName",
          signature="PkFlow",
          definition = function(.Object,indx,name){
            if ( .Object@flowType == FpClearance ) {
              .Object@iclName = name
            }
            if ( .Object@flowType == FpMicro ) {
              .Object@ik12Name = name
            }
              .Object
          })

setMethod("genStructuralParameters",
          signature="PkFlow",
          definition = function(.Object,hasRandomEffect){
            sps=c()

            if ( .Object@flowType == FpClearance ) {
                if ( .Object@isVascular && .Object@derivedVascularClearance != "" ) {

                }
                else {
                  if ( .Object@iclName == "" ) {
                    sp = NlmeStructuralParameter(name=.Object@clName,
                                             hasRandomEffect = hasRandomEffect ,
                                             hasCovariateEffect = FALSE,
                                             isSequential  = .Object@isSequential)
                    sps=c(sps,sp)
                  }
                }
            } else if ( .Object@flowType == FpMicro ) {
                if ( .Object@ik12Name == "" ) {
                  sp = NlmeStructuralParameter(name=.Object@k12Name,
                                           hasRandomEffect = hasRandomEffect ,
                                           hasCovariateEffect = FALSE,
                                           isSequential  = .Object@isSequential)
                  sps=c(sps,sp)
                }
                if ( .Object@twoWay ) {
                  if ( .Object@ik21Name == "" ){
                    sp = NlmeStructuralParameter(.Object@k21Name,
                                             hasRandomEffect = hasRandomEffect ,
                                             hasCovariateEffect = FALSE,
                                             isSequential  = .Object@isSequential)
                    sps=c(sps,sp)
                  }
              }

            } else if ( .Object@flowType == FpSaturating){
                if ( .Object@ivMaxName == "" ){
                  sp = NlmeStructuralParameter(.Object@vMaxName,
                                           hasRandomEffect = hasRandomEffect ,
                                           hasCovariateEffect = FALSE,
                                           isSequential  = .Object@isSequential)
                  sps=c(sps,sp)
                }
                if ( .Object@ikmName == "" ) {
                  sp = NlmeStructuralParameter(.Object@kmName,
                                           hasRandomEffect = hasRandomEffect ,
                                           hasCovariateEffect = FALSE,
                                           isSequential  = .Object@isSequential)
                  sps=c(sps,sp)
                }
            }

            sps
          })

setMethod("genStatements",
          signature="PkFlow",
          definition = function(.Object){
            statements=c()

            statements
          })

#' Wire
#'
#' Class represents flow connection between compartments
#'
#' @param wireName     Name of the wire connection
#' @param fromObject   Identifier of the source object
#' @param fromPort     Identifier of the source object's port
#' @param toObject     Identifier of the target object 
#' @param toPort       Identifier of the target object's port
#' @param contains
#'
#' @examples
#'       
#' @export Wire
#'
Wire = setClass("Wire",representation(
  wireName="character",
  fromObject="numeric",
  fromPort="numeric",
  toObject="numeric",
  toPort="numeric"),
  contains = "StructuralBlock")


#' @export
setMethod("initialize","Wire",
          function(.Object,
                   ...,
                   wireName="",
                   fromObject=-1,
                   fromPort=-1,
                   toObject=-1,
                   toPort=-1,
                   blockName="") {

            .Object@wireName         = wireName
            .Object@fromObject       = fromObject
            .Object@fromPort         = fromPort
            .Object@toObject         = toObject
            .Object@toPort           = toPort
            if ( blockName == "" )
              blockName=wireName

            callNextMethod(.Object,
                           ...,
                           wireName=wireName,
                           fromObject = fromObject,
                           fromPort= fromPort,
                           toObject = toObject,
                           toPort=toPort,
                           blockName=blockName)
          })


assign("Wire",Wire,envir=.GlobalEnv)

setMethod("clearInPortNames",
          signature="Wire",
          definition = function(.Object){

            .Object
          })

#' print.Wire
#'
#' Prints Wire information
#'
#' @param obj      Model wire
#'
#' @examples
#'       print.Wire(obj="WireA1")
#'
#' @export print.Wire
#'
print.Wire <-function(x, ...)
{
  print(paste0("----   Flow  ----"))
  print(paste0("ID               : ",x@compKey))
  print(paste0("Name             : ", x@blockName))
  print(paste0("from             : ",x@fromObject,",",x@fromPort))
  print(paste0("to               : ",x@toObject,",",x@toPort))

}



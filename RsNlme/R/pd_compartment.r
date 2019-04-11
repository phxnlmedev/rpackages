#' PDCompartment
#'
#' Class represents an NLME PD compartment
#'
#' @param name    Name of the PD compartment
#'
#' @examples
#'
#' @export PDCompartment
#'

PDCompartment= setClass("PDCompartment",representation(name="character"),
                                contains = "StructuralBlock" )



setMethod("initialize","PDCompartment",
          function(.Object,
                   ...,
                   name=""){


            callNextMethod(.Object,...)


          })
assign("PDCompartment",PDCompartment,envir=.GlobalEnv)




#' EmaxCompartment
#'
#' Class represents an NLME Emax compartment
#'
#' @param cName           Concentration variable name
#' @param c50Name         IC50 name
#' @param firstName       First emax parameter name
#' @param gammaName       Gamma variable name
#' @param maxName         Imax variable name
#' @param checkBaseline   Does model have a baseline response
#' @param checkFractional Is the model fractional
#' @param checkInhibitory Is the model inhibitory
#' @param checkSigmoid    Is the model sigmoidal
#'
#' @examples
#'      emaxComp = EmaxCompartment(checkBaseline=TRUE,
#'                                 checkFractional = TRUE,
#'                                 checkInhibitory = TRUE,
#'                                 checkSigmoid = TRUE,
#'                                 blockName = "E")
#'
#' @export EmaxCompartment
#'
EmaxCompartment= setClass("EmaxCompartment",representation(
  cName="character",
  c50Name="character",
  firstName="character",
  gammaName="character",
  maxName="character",
  icName="character",
  ic50Name="character",
  ifirstName="character",
  igammaName="character",
  imaxName="character",
  checkBaseline="logical",
  checkFractional="logical",
  checkInhibitory="logical",
  checkSigmoid="logical"),
  contain = "PDCompartment")



setMethod("initialize","EmaxCompartment",
          function(.Object,
                   ...,
                   cName="C",
                   c50Name="EC50",
                   firstName="E0",
                   gammaName="Gam",
                   maxName="EMax",
                   checkBaseline=FALSE,
                   checkFractional=FALSE,
                   checkInhibitory=FALSE,
                   checkSigmoid=FALSE){

            .Object@cName = cName
            .Object@c50Name = c50Name
            .Object@firstName = firstName
            .Object@gammaName = gammaName
            .Object@maxName = maxName
            .Object@icName = ""
            .Object@ic50Name = ""
            .Object@ifirstName = ""
            .Object@igammaName = ""
            .Object@imaxName = ""
            .Object@checkBaseline = checkBaseline
            .Object@checkFractional = checkFractional
            .Object@checkInhibitory = checkInhibitory
            .Object@checkSigmoid = checkSigmoid

            callNextMethod(.Object,...)

          })

assign("EmaxCompartment",EmaxCompartment,envir=.GlobalEnv)

#' print.EmaxCompartment
#'
#' Prints Emax compartment information
#'
#' @param obj    Model compartment
#'
#' @examples
#'       print.EmaxCompartment(obj="linear")
#'
#' @export print.EmaxCompartment
#'
print.EmaxCompartment <-function(x, ...)
{
  print(paste("Block Name     : ",attr(x,"blockName")))
  print(paste("Name           : ",attr(x,"cName")))
  print(paste("Variable Name  : ",x@c50Name))
  print(paste("Variable Name  : ",x@firstName))
  print(paste("Variable Name  : ",x@gammaName))
  print(paste("Variable Name  : ",x@maxName))
}



setMethod("clearInPortNames",
          signature="EmaxCompartment",
          definition = function(.Object){

            .Object@icName = ""
            .Object@ic50Name = ""
            .Object@ifirstName = ""
            .Object@igammaName = ""
            .Object@imaxName = ""
            .Object
          })


setMethod("getNumInPorts",
          signature="EmaxCompartment",
          definition = function(.Object){
            numInPorts = 5
            numInPorts

          })



setMethod("getNumOutPorts",
          signature="EmaxCompartment",
          definition = function(.Object){
            numOutPorts = 1

            numOutPorts

          })

setMethod("getInPortName",
          signature="EmaxCompartment",
          definition = function(.Object,indx){
            name = ""
            if ( indx == 0 )
                name = .Object@cName
            else if ( indx == 1 )
                name = .Object@c50Name
            else if ( indx == 2 )
                name = .Object@maxName
            else if ( indx == 3 )
                name = .Object@firstName
            else if ( indx == 4 )
                name = .Object@gammaName
            name
          })

setMethod("getOutPortName",
          signature="EmaxCompartment",
          definition = function(.Object,indx){
            name = .Object@name
            name
          })

setMethod("setInPortName",
          signature="EmaxCompartment",
          definition = function(.Object,indx,name){

            if ( indx == 0 )
                .Object@icName  = name
            else if ( indx == 1 )
                .Object@ic50Name  = name
            else if ( indx == 2 )
                .Object@imaxName  = name
            else if ( indx == 3 )
                .Object@ifirstName  = name
            else if ( indx == 4 )
                .Object@igammaName  = name
            .Object
          })

addAnEmaxStructuralParameter <-function(sps,spName,isSequential,hasRandomEffect=TRUE){
  sp = NlmeStructuralParameter(name=spName,
                               hasRandomEffect = hasRandomEffect ,
                               hasCovariateEffect = FALSE,
                               isSequential  = isSequential)
  sps=c(sps,sp)
  sps
}
setMethod("genStructuralParameters",
          signature="EmaxCompartment",
          definition = function(.Object,hasRandomEffect){
            sps=c()
            if ( .Object@ic50Name == "" ) {
              sps = addAnEmaxStructuralParameter(sps,.Object@c50Name,.Object@isSequential,hasRandomEffect)

            }
            if ( !.Object@checkBaseline ) {
              if ( !.Object@checkInhibitory ) {
                if ( .Object@imaxName == "" ){
                  sps = addAnEmaxStructuralParameter(sps,.Object@maxName,.Object@isSequential,hasRandomEffect)
                }

              } else{
                if ( .Object@ifirstName == "" ){
                  sps = addAnEmaxStructuralParameter(sps,.Object@firstName,.Object@isSequential,hasRandomEffect)
                }
              }

            } else {
              if ( .Object@ifirstName == "" )
                sps = addAnEmaxStructuralParameter(sps,.Object@firstName,.Object@isSequential,hasRandomEffect)
              if ( .Object@imaxName == "" )
                sps = addAnEmaxStructuralParameter(sps,.Object@maxName,.Object@isSequential,hasRandomEffect)
            }
            if ( .Object@checkSigmoid ) {
              if ( .Object@igammaName == "" )
                sps = addAnEmaxStructuralParameter(sps,.Object@gammaName,.Object@isSequential,hasRandomEffect)
            }
            sps
          })

setMethod("genStatements",
          signature="EmaxCompartment",
          definition = function(.Object){
            statements=c()
            if ( .Object@icName == "" )
                c = .Object@cName
            else
              c = .Object@icName
            if ( .Object@ic50Name == "" )
              c50 = .Object@c50Name
            else
              c50 = .Object@ic50Name
            if ( .Object@igammaName == "" )
              gammaName = .Object@gammaName
            else
              gammaName = .Object@igammaName
            if ( .Object@imaxName == "" )
              maxName = .Object@maxName
            else
              maxName = .Object@imaxName
            if ( .Object@ifirstName == "" )
              firstName = .Object@firstName
            else
              firstName = .Object@ifirstName

            if ( .Object@checkSigmoid ) {
              c = paste0(c,"^",gammaName)
              c50 = paste0(c50,"^",gammaName)
            }
            fractional = paste0(c, " / (", c50, " + " , c , ")" )
            s = ""
            s0 = firstName
            if ( !.Object@checkBaseline ) {
              if( !.Object@checkInhibitory ) {
                s = paste0(maxName, " * (", fractional , " )" )
              } else {
                s = paste0(s0, " *(1 - ",fractional, ")")
              }
            } else {
              if ( !.Object@checkFractional ) {
                  if ( !.Object@checkInhibitory ) {
                     s = paste0(s0, " + " , maxName, " * " , fractional)
                  } else {
                    s = paste0(s0, " - " , maxName, " * " , fractional)
                  }

               } else {
                 if ( !.Object@checkInhibitory ) {
                   s = paste0(s0, " *(1+ ",maxName, " * " , fractional,")")
                 } else {
                   s = paste0(s0, " *(1- ",maxName, " * " , fractional,")")
                 }
              }
            }

            statement = paste0( "    ", .Object@blockName , " = ", s)

            statements = c(statements,statement)

            statements
          })
setMethod("genDoseStatements",
          signature="EmaxCompartment",
          definition = function(.Object){
            statements=c()

            statements
          })

setMethod("genFlowRateODE",
          signature="EmaxCompartment",
          definition = function(.Object,objIndx,objects){
            sps=""
            #XXXXXXXXXXXXXXX
            sps
          })



#' EffectsCompartment
#'
#' Class represents an NLME Effects compartment
#'
#' @param flowInName 
#' @param ceName       Input variable name for port1
#' @param ke0Name      Input variable name for port2
#'
#' @examples
#'
#' @export EffectsCompartment
#'
EffectsCompartment= setClass("EffectsCompartment",representation(
  flowInName="character",
  ceName="character",
  ke0Name="character",
  iceName="character",
  ike0Name="character"),
  contain = "PDCompartment")



setMethod("initialize","EffectsCompartment",
          function(.Object,
                   ...,
                   flowInName=flowInName,
                   ceName="Ce",
                   ke0Name="Ke0"){

            .Object@flowInName = flowInName
            .Object@ceName = ceName
            .Object@ke0Name = ke0Name
            .Object@iceName = ""
            .Object@ike0Name = ""

            callNextMethod(.Object,...)

          })
assign("EffectsCompartment",EffectsCompartment,envir=.GlobalEnv)

#' 
#'
#' Prints Effects compartment information
#'
#' @param obj    Model compartment
#'
#' @examples
#'       print.EffectsCompartment(obj="linear")
#'
#' @export print.EffectsCompartment
#'
print.EffectsCompartment <-function(x, ...)
{
  print(paste("Name           : ",attr(x,"ceName")))
  print(paste("Variable Name  : ",x@ke0Name))
}



setMethod("clearInPortNames",
          signature="EffectsCompartment",
          definition = function(.Object){
            print("EffectsCompartment : clearInPortNames()")
            .Object@iceName = ""
            .Object@ike0Name = ""
            .Object
          })


setMethod("getNumInPorts",
          signature="EffectsCompartment",
          definition = function(.Object){
            numInPorts = 2
            numInPorts

          })



setMethod("getNumOutPorts",
          signature="EffectsCompartment",
          definition = function(.Object){
            numOutPorts = 1

            numOutPorts

          })

setMethod("getInPortName",
          signature="EffectsCompartment",
          definition = function(.Object,indx){
            name = ""
            if ( indx == 0 )
                name = .Object@ceName
            else if ( indx == 1 )
                name = .Object@ke0Name
            name
          })

setMethod("getOutPortName",
          signature="EffectsCompartment",
          definition = function(.Object,indx){

            name = .Object@ceName
            name
          })

setMethod("setInPortName",
          signature="EffectsCompartment",
          definition = function(.Object,indx,name){

            if ( indx == 0 )
                .Object@iceName  = name
            else if ( indx == 1 )
                .Object@ike0Name  = name
            .Object
          })

setMethod("genStructuralParameters",
          signature="EffectsCompartment",
          definition = function(.Object,hasRandomEffect){
            sps=c()
            if ( .Object@ike0Name == "" ) {
              sp = NlmeStructuralParameter(name=.Object@ke0Name,
                               hasRandomEffect = hasRandomEffect ,
                               hasCovariateEffect = FALSE,
                               isSequential  = .Object@isSequential)
              sps = c(sps,sp)
            }
            sps
          })

setMethod("genStatements",
          signature="EffectsCompartment",
          definition = function(.Object){
            statements=c()

            statements
          })
setMethod("genDoseStatements",
          signature="EffectsCompartment",
          definition = function(.Object){
            statements=c()

            statements
          })

setMethod("genFlowRateODE",
          signature="EffectsCompartment",
          definition = function(.Object,objIndx,objects){
            sps=""

            sps=paste0("    deriv(",.Object@ceName," = ",.Object@ke0Name, " * (",.Object@iceName,  " - " , .Object@ceName ,"))" )
            sps
          })

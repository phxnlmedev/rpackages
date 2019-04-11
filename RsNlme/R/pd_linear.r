
#' LinearCompartment
#'
#' Class represents an NLME Linear compartment
#'
#' @param cName       Concentration variable name
#' @param alphaName   Alpha variable name
#' @param betaName    Beta variable name
#' @param gammaName   Gamma variable name
#' @param type        LinearAlpha|LinearBeta|LinearGamma
#'
#' @examples
#'      linearComp = LinearCompartment(
#'                           type = LinearAlpha,
#'                           blockName = "E")
#'
#' @export LinearCompartment
#' 
LinearCompartment= setClass("LinearCompartment",representation(
  cName="character",
  alphaName="character",
  betaName="character",
  gammaName="character",
  icName="character",
  ialphaName="character",
  ibetaName="character",
  igammaName="character",
  type="numeric"),
  contain = "PDCompartment")



setMethod("initialize","LinearCompartment",
          function(.Object,
                    ...,
                   cName="C",
                   alphaName="Alpha",
                   betaName="Beta",
                   gammaName="Gam",
                   type=LinearAlpha ){

            .Object@cName = cName
            .Object@alphaName = alphaName
            .Object@betaName = betaName
            .Object@gammaName = gammaName
            .Object@type = type
            .Object@icName = ""
            .Object@ialphaName = ""
            .Object@ibetaName = ""
            .Object@igammaName = ""

            callNextMethod(.Object,...)

          })
assign("LinearCompartment",LinearCompartment,envir=.GlobalEnv)

#' print.LinearCompartment
#'
#' Prints linear compartment information
#'
#' @param obj    Model compartment
#'
#' @examples
#'       print.LinearCompartment(obj="linear")
#'
#' @export print.LinearCompartment
#'
print.LinearCompartment <-function(x, ...)
{
  print(paste("ID             : ",x@compKey))
  print(paste("Block Name     : ",attr(x,"blockName")))
  print(paste("Name           : ",attr(x,"cName")))
  print(paste("Type           : ",attr(x,"type")))
}



setMethod("clearInPortNames",
          signature="LinearCompartment",
          definition = function(.Object){

            .Object@icName = ""
            .Object@ialphaName = ""
            .Object@ibetaName = ""
            .Object@igammaName = ""
            .Object
          })


setMethod("getNumInPorts",
          signature="LinearCompartment",
          definition = function(.Object){
            numInPorts = 4
            numInPorts

          })



setMethod("getNumOutPorts",
          signature="LinearCompartment",
          definition = function(.Object){
            numOutPorts = 1

            numOutPorts

          })

setMethod("getInPortName",
          signature="LinearCompartment",
          definition = function(.Object,indx){
            name = ""
            if ( indx == 0 )
                name = .Object@cName
            else if ( indx == 1 )
                name = .Object@alphaName
            else if ( indx == 2 )
                name = .Object@betaName
            else if ( indx == 3 )
                name = .Object@gammaName
            name
          })

setMethod("getOutPortName",
          signature="LinearCompartment",
          definition = function(.Object,indx){
            name = .Object@blockName
            name
          })

setMethod("setInPortName",
          signature="LinearCompartment",
          definition = function(.Object,indx,name){

            if ( indx == 0 )
                 .Object@icName = name
            else if ( indx == 1 )
                 .Object@ialphaName = name
            else if ( indx == 2 )
                 .Object@ibetaName = name
            else if ( indx == 3 )
                 .Object@igammaName = name
            .Object
          })

addAnLinearStructuralParameter <-function(sps,spName,isSequential){
  sp = NlmeStructuralParameter(name=spName,
                               hasRandomEffect = TRUE ,
                               hasCovariateEffect = FALSE,
                               isSequential  = isSequential)
  sps=c(sps,sp)
  sps
}


setMethod("genStructuralParameters",
          signature="LinearCompartment",
          definition = function(.Object){
            sps=c()
            type = .Object@type
            if ( .Object@icName == "" )
              sps = addAnLinearStructuralParameter(sps,.Object@cName,.Object@isSequential)

            if ( .Object@ialphaName == "" && type >= LinearAlpha )
              sps = addAnLinearStructuralParameter(sps,.Object@alphaName,.Object@isSequential)

            if ( .Object@ibetaName == "" && type >= LinearBeta )
              sps = addAnLinearStructuralParameter(sps,.Object@betaName,.Object@isSequential)

            if ( .Object@igammaName == "" && type >= LinearGamma )
              sps = addAnLinearStructuralParameter(sps,.Object@gammaName,.Object@isSequential)

            sps
          })



setMethod("genStatements",
          signature="LinearCompartment",
          definition = function(.Object){
            statements=c()

            name = .Object@blockName

            if ( .Object@icName == "" )
                c = .Object@cName
            else
              c = .Object@icName

            statement = ""
            if ( .Object@ialphaName  == "" )
                alphaName = .Object@alphaName
            else
                alphaName = .Object@ialphaName
            if ( .Object@ibetaName  == "" )
                betaName = .Object@betaName
            else
                betaName = .Object@ibetaName
            if ( .Object@igammaName  == "" )
                gammaName = .Object@gammaName
            else
                gammaName = .Object@igammaName
            if ( .Object@icName == "" )  {
                statement=paste0("    covariate(",c,")")
                statements=c(statements,statement)
            }
            type = .Object@type
            statement = ""

            if ( type >= LinearAlpha )
                statement= paste0("    ",name," = ",alphaName)
            if ( type >= LinearBeta )
                statement= paste0(statement, " + (",betaName, " * ",c, ")")
            if ( type >= LinearGamma )
                statement= paste0(statement, " + (",gammaName, " * ",c, ," * ",c,")")
            statements=c(statements,statement)


            statements
          })


setMethod("genDoseStatements",
          signature="LinearCompartment",
          definition = function(.Object){
            statements=c()

            statements
          })

setMethod("genFlowRateODE",
          signature="LinearCompartment",
          definition = function(.Object,objIndx,objects){
            sps=""
            sps
          })




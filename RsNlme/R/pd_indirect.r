
#' IndirectCompartment
#'
#' Class represents an NLME Indirect compartment
#'
#' @param cName           Concentration variable name
#' @param emaxName        emax variable name
#' @param ec50Name        ec50 variable name
#' @param kinName         kin variable name
#' @param koutName        kout variable name
#' @param gammaName       gamma variable name
#' @param sName           
#' @param isExponent      Is there an exponent Gamma added to the effect statement
#' @param isBuildup       Is the response formation (TRUE) or degradation (FALSE) 
#'                            affected by increased concentrations 
#' @param type            LimitedStimulation|InfiniteStimulation|
#'                        LimitedInhibition|InverseInhibition|
#'                        LinearStimulation|LogLinearStimulation
#'
#' @examples
#'
#' @export IndirectCompartment
#'
IndirectCompartment= setClass("IndirectCompartment",representation(
  cName="character",
  emaxName="character",
  ec50Name="character",
  kinName="character",
  koutName="character",
  gammaName="character",
  sName="character",
  icName="character",
  iemaxName="character",
  iec50Name="character",
  ikinName="character",
  ikoutName="character",
  igammaName="character",
  isName="character",
  isExponent="logical",
  isBuildup="logical",
  type="numeric"),
  contain = "PDCompartment")



setMethod("initialize","IndirectCompartment",
          function(.Object,
                   ...,
                   cName="C",
                   emaxName="Emax",
                   ec50Name="EC50",
                   kinName="Kin",
                   koutName="Kout",
                   gammaName="gam",
                   sName="S",
                   isExponent=FALSE,
                   isBuildup=FALSE,
                   type=LimitedStimulation){

            .Object@cName = cName
            .Object@emaxName = emaxName
            .Object@ec50Name = ec50Name
            .Object@gammaName = gammaName
            .Object@kinName = kinName
            .Object@koutName = koutName
            .Object@sName = sName
            .Object@isExponent = isExponent
            .Object@isBuildup = isBuildup
            .Object@icName = ""
            .Object@iec50Name = ""
            .Object@iemaxName = ""
            .Object@igammaName = ""
            .Object@ikinName = ""
            .Object@ikoutName = ""
            .Object@isName = ""
            .Object@type = type

            callNextMethod(.Object,...)

          })
assign("IndirectCompartment",IndirectCompartment,envir=.GlobalEnv)

#' print.IndirectCompartment
#'
#' Prints indirect compartment information
#'
#' @param obj    Model compartment
#'
#' @examples
#'       print.IndirectCompartment(obj="linear")
#'
#' @export print.IndirectCompartment
#'
print.IndirectCompartment <-function(x, ...)
{
  print(paste("ID             : ",x@compKey))
  print(paste("Block Name     : ",attr(x,"blockName")))
  print(paste("Name           : ",attr(x,"cName")))
  print(paste("Variable Name  : ",x@emaxName))
  print(paste("Variable Name  : ",x@ec50Name))
  print(paste("Variable Name  : ",x@gammaName))
  print(paste("Variable Name  : ",x@kinName))
  print(paste("Variable Name  : ",x@koutName))
}



setMethod("clearInPortNames",
          signature="IndirectCompartment",
          definition = function(.Object){

            .Object@icName = ""
            .Object@iec50Name = ""
            .Object@iemaxName = ""
            .Object@igammaName = ""
            .Object@ikinName = ""
            .Object@ikoutName = ""
            .Object@isName = ""
            .Object
          })


setMethod("getNumInPorts",
          signature="IndirectCompartment",
          definition = function(.Object){
            numInPorts = 7
            numInPorts

          })



setMethod("getNumOutPorts",
          signature="IndirectCompartment",
          definition = function(.Object){
            numOutPorts = 1

            numOutPorts

          })

setMethod("getInPortName",
          signature="IndirectCompartment",
          definition = function(.Object,indx){
            name = ""
            if ( indx == 0 )
                name = .Object@cName
            else if ( indx == 1 )
                name = .Object@emaxName
            else if ( indx == 2 )
                name = .Object@ec50Name
            else if ( indx == 3 )
                name = .Object@kinName
            else if ( indx == 4 )
                name = .Object@koutName
            else if ( indx == 5 )
                name = .Object@gammaName
            else if ( indx == 6 )
                name = .Object@sName
            name
          })

setMethod("getOutPortName",
          signature="IndirectCompartment",
          definition = function(.Object,indx){
            name = .Object@name
            name
          })

setMethod("setInPortName",
          signature="IndirectCompartment",
          definition = function(.Object,indx,name){

            if ( indx == 0 )
                 .Object@icName = name
            else if ( indx == 1 )
                 .Object@iemaxName = name
            else if ( indx == 2 )
                 .Object@iec50Name = name
            else if ( indx == 3 )
                 .Object@ikinName = name
            else if ( indx == 4 )
                 .Object@ikoutName = name
            else if ( indx == 5 )
                 .Object@igammaName = name
            else if ( indx == 6 )
                 .Object@isName = name
            .Object
          })

addAnIndirectStructuralParameter <-function(sps,spName,isSequential,hasRandomEffect){
  sp = NlmeStructuralParameter(name=spName,
                               hasRandomEffect = hasRandomEffect ,
                               hasCovariateEffect = FALSE,
                               isSequential  = isSequential)
  sps=c(sps,sp)
  sps
}


setMethod("genStructuralParameters",
          signature="IndirectCompartment",
          definition = function(.Object,hasRandomEffect){
            sps=c()
            if ( .Object@iemaxName == "" )
              sps = addAnIndirectStructuralParameter(sps,.Object@emaxName,.Object@isSequential,hasRandomEffect)

            if ( .Object@iec50Name == "" )
              sps = addAnIndirectStructuralParameter(sps,.Object@ec50Name,.Object@isSequential,hasRandomEffect)

            if ( .Object@ikinName == "" )
              sps = addAnIndirectStructuralParameter(sps,.Object@kinName,.Object@isSequential,hasRandomEffect)

            if ( .Object@ikoutName == "" )
              sps = addAnIndirectStructuralParameter(sps,.Object@koutName,.Object@isSequential,hasRandomEffect)

            if ( .Object@igammaName == "" && .Object@isExponent )
              sps = addAnIndirectStructuralParameter(sps,.Object@gammaName,.Object@isSequential,hasRandomEffect)

            if ( .Object@isName == "" &&
                ( .Object@type == LinearStimulation ||
                  .Object@type == LogLinearStimulation )  )
              sps = addAnIndirectStructuralParameter(sps,.Object@sName,.Object@isSequential,hasRandomEffect)

            sps
          })



setMethod("genStatements",
          signature="IndirectCompartment",
          definition = function(.Object){
            statements=c()

            name = .Object@blockName

            if ( .Object@icName == "" )
                c = .Object@cName
            else
              c = .Object@icName
            statement = ""
            if ( .Object@iemaxName  == "" )
                emaxName = .Object@emaxName
            else
                emaxName = .Object@iemaxName
            if ( .Object@iec50Name  == "" )
                ec50Name = .Object@ec50Name
            else
                ec50Name = .Object@iec50Name
            if ( .Object@igammaName  == "" )
                gammaName = .Object@gammaName
            else
                gammaName = .Object@igammaName
            if ( .Object@ikinName  == "" )
                kinName = .Object@kinName
            else
                kinName = .Object@ikinName
            if ( .Object@ikoutName  == "" )
                koutName = .Object@koutName
            else
                koutName = .Object@ikoutName

            if ( .Object@type == LimitedStimulation ) {
              emax = emaxName
              ec50 = ec50Name
              if ( .Object@isExponent ) {
                ec50 = paste0(ec50 , "^",gammaName )
                c = paste0(c , "^",gammaName )
              }
              fractional = paste0( c, " / (", c, " + ",ec50,")")
              statement = paste0(emax, " * ", fractional )
              statement = paste0( "(1 + ",statement, ")")
            }
            else if ( .Object@type == InfiniteStimulation ) {
              ec50 = ec50Name
              fractional = paste0("(", c, " / ",ec50, ")")
              if ( .Object@isExponent ) {
                fractional = paste0(fractional, " ^ ",gammaName)
              }
              statement = fractional
              statement = paste0( "(1 + ",statement, ")")
            }
            else if ( .Object@type == LimitedInhibition ) {
              emax = emaxName
              ec50 = ec50Name
              if ( .Object@isExponent ) {
                ec50 = paste0(ec50 , "^",gammaName )
                c = paste0(c , "^",gammaName )
              }
              fractional = paste0( c, " / (", c, " + ",ec50,")")
              statement = paste0(emax, " * ", fractional )
              statement = paste0( "(1 - ",s, ")")
            }
            else if ( .Object@type == InverseInhibition ) {
              emax = emaxName
              ec50 = ec50Name
              if ( .Object@isExponent ) {
                ec50 = paste0(ec50 , "^",gammaName )
                c = paste0(c , "^",gammaName )
              }
              fractional = paste0( c, " / (", c, " + ",ec50,")")
              statement = paste0(emax, " * ", fractional )
              statement = paste0( "1 / (1 + ",s, ")")
            }
            else if ( .Object@type == LinearStimulation ) {
              var="s"
              if ( .Object@isExponent ) {
                c = paste0(c, " ^ ",gammaName)
              }
              statement = paste0( "(1 + ",var, " * " , c, ")")
            }
            else if ( .Object@type == LogLinearStimulation ) {
              var="s"
              if ( .Object@isExponent ) {
                c = paste0(c, " ^ ",gammaName)
              }
              statement = paste0( "(1 + log(1 + ",var, " * " , c, "))")
            }

            inEffects = ""
            outEffects = ""
            if ( .Object@isBuildup ) {
              inEffects = paste0( kinName, " * " , statement )
              outEffects = koutName
            } else {
              inEffects = kinName
              outEffects = paste0( koutName, " * " , statement )
            }

            statement = paste0("    deriv(",name, " = ",inEffects , " - ", outEffects, " * ", name, ")")
            statements = c(statements,statement)

            statement = paste0("    sequence{", name, " = (",kinName, ") / (",koutName,")}")
            statements = c(statements,statement)


            statements
          })


setMethod("genDoseStatements",
          signature="IndirectCompartment",
          definition = function(.Object){
            statements=c()

            statements
          })

setMethod("genFlowRateODE",
          signature="IndirectCompartment",
          definition = function(.Object,objIndx,objects){
            sps=""
            sps
          })




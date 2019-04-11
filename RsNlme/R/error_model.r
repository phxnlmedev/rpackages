
#' @export
ERR_ADDITIVE=1
#' @export
Additive=1
#' @export
ERR_LOG_ADDITIVE=2
#' @export
LogAdditive=2
#' @export
ERR_MULTIPLICATIVE=3
#' @export
Multiplicative=3
#' @export
ERR_ADD_MULT=4
#' @export
AdditiveMultiplicative=4
#' @export
ERR_MIX_RATIO=5
#' @export
MixRatio=5
#' @export
ERR_POWER=6
#' @export
Power=6
#' @export
ERR_CUSTOM=7
#' @export
Custom=7

ErrorTypeNames = c("Additive","LogAdditive","Multiplicative",
                   "AdditiveMultiplicative","MixRatio","Power","Custom")

#' NlmeResidualEffect
#'
#' Class represents an NLME/PML residual error model
#'
#' @param effectName   Name of the observed variable
#' @param observeName  Name to use for observation
#' @param epsilonName  Name to use for Epsilon
#' @param errorType    Additive|LogAdditive|Multiplicative
#'                     |AdditiveMultiplicative|MixRatio|Power|Custom
#' @param frozen       Is the standard deviation frozen?(default:FALSE)
#' @param SD           Standard deviation value
#' @param definition   Definition of a custom type or Power value
#' @param isBQL        Are there BQL values?(default:FALSE)
#' @param bqlStatic    Value LLOQ value
#' @param dobefore     Code to execute before the observation
#' @param doafter      Code to execute after the observation
#'
#' @examples
#'
#'  effect = NlmeResidualEffect(Additive,"C")
#'
#'  effect = NlmeResidualEffect(AdditiveMultiplicative,"E",isBQL=TRUE)
#'
#' @export NlmeResidualEffect
#'
NlmeResidualEffect= setClass("NlmeResidualEffect",representation(
                                      effectName="character",
                                      observeName="character",
                                      epsilonName="character",
                                      errorType="numeric",
                                      frozen="logical",
                                      SD="numeric",
                                      definition="character",
                                      isBQL="logical",
                                      bqlStaticValue="character",
                                      dobefore="character",
                                      doafter="character"))


setMethod("initialize","NlmeResidualEffect",
    function(.Object,
              errorType=Additive,
              effectName="",
              observeName="",
              epsilonName="",
              frozen=FALSE,
              SD=1,
              definition="",
              isBQL=FALSE,
              bqlStaticValue="",
              dobefore = "",
              doafter = ""){
         if( errorType<ERR_ADDITIVE || errorType>ERR_CUSTOM ){
             print(paste("errorType",errorType," is not supported!"))
             errorType = ERR_ADDITIVE
         }
         if ( effectName == "" )
             effectName="C"
         if ( observeName == "" )
             observeName=paste0(effectName,"Obs")
         if ( epsilonName == "" )
             epsilonName=paste0(effectName,"Eps")
         if ( epsilonName == "" )
             epsilonName=paste0(effectName,"Eps")
         if (( errorType == ERR_ADD_MULT ) && ( definition==""))
             definition=paste0(effectName,"MultStdev")

        .Object@dobefore = dobefore
        .Object@doafter = doafter

        .Object@errorType=errorType
        .Object@effectName=effectName
        .Object@observeName=observeName
        .Object@epsilonName=epsilonName
        .Object@frozen=frozen
        .Object@SD=SD
        .Object@definition=definition
        .Object@isBQL=isBQL
        .Object@bqlStaticValue=bqlStaticValue

        .Object
    })

assign("NlmeResidualEffect",NlmeResidualEffect,envir=.GlobalEnv)
#' print.NlmeResidualEffect
#'
#' Prints Residual effect information
#'
#' @param obj      Residual effect name
#'
#' @examples
#'       print.NlmeResidualEffect(obj="C2")
#'
#' @export print.NlmeResidualEffect
#'
print.NlmeResidualEffect <-function(x, ...){
    print(paste("Name             : ",x@effectName))
    print(paste("Type             : ",ErrorTypeNames[x@errorType]))
    print(paste("Observation Name : ",x@observeName))
    print(paste("Is Frozen        : ",x@frozen))
}


#' NlmeErrorModel
#'
#' Class represents an NLME/PML error model
#'
#' @param effectsList      List of residual effects to include in the error model
#' @param numberOfEffects  Number of effects being included in the error model
#'
#' @examples
#' resEff1=NlmeResidualEffect(ERR_ADDITIVE,"C")
#' resEff2=NlmeResidualEffect(ERR_SUM_EXP,"E")
#' errorModel=NlmeErrorModel(c(resEff1,resEff2))
#'
#' @export NlmeErrorModel
#'
NlmeErrorModel= setClass("NlmeErrorModel",representation(
                                      effectsList="list",
                                      numberOfEffects="numeric"))


setMethod("initialize","NlmeErrorModel",
    function(.Object,
 #             effectsList=c(NlmeResidualEffect())){
        effectsList=list()){
        .Object@effectsList=effectsList
        .Object@numberOfEffects=length(effectsList)
        .Object
    })
assign("NlmeErrorModel",NlmeErrorModel,envir=.GlobalEnv)

#' addToErrorModel
#'
#' Add to the NLME Error model
#'
#' @param model
#' @param effectsList
#'
#' @examples
#'
#' @export addToErrorModel
#'
#'
addToErrorModel <-function(model,effectsList){
    errorModel=model@errorModel
    origList=errorModel@effectsList
    len=length(origList)
    for ( i in 1:length(effectsList) ){
        origList[[len+i]] = effectsList[[i]]
    }
    errorModel@effectsList = origList
    errorModel@numberOfEffects = length(origList)
    model@errorModel=errorModel
    model
}
assign("addToErrorModel",addToErrorModel,envir=.GlobalEnv)



#' print.NlmeErrorModel
#'
#' Prints Error model information
#'
#' @param obj      Error model name
#'
#' @examples
#'       print.NlmeErrorModel(obj="C2")
#'
#' @export print.NlmeErrorModel
#'
print.NlmeErrorModel <-function(x, ...)
{
    print("Error Model")
    numEffects=x@numberOfEffects
    effectsList=x@effectsList
    for ( e in effectsList ) {
        print(e)
    }
}

ObservationCompartment= setClass("ObservationCompartment",representation(name="character",
                                                                         customCodeLines="list"),
                                                           contains = "StructuralBlock" )



setMethod("initialize","ObservationCompartment",
          function(.Object,
                   ...,
                   name="",
                   customCodeLines=c()){

            .Object@name=name
            .Object@customCodeLines= as.list(customCodeLines)
            callNextMethod(.Object,...)


          })
assign("ObservationCompartment",ObservationCompartment,envir=.GlobalEnv)




#' ObservationContinuous
#'
#' Class represents an NLME continuous observation
#'
#' @param name               Observation name
#' @param residualEffect     Defined by NlmeResidualEffect
#' @param customCodeLines    Lines of PML code to insert before LL() statement
#'
#' @examples
#'
#' @export ObservationContinuous
#'
ObservationContinuous= setClass("ObservationContinuous",representation(

  residualEffect="NlmeResidualEffect",
  varName="character",
  iVarName="character"),
  contain = "ObservationCompartment")



setMethod("initialize","ObservationContinuous",
          function(.Object,
                   ...,
                   blockName="",
                   residualEffect,
                   varName=""
                   ){

            .Object@residualEffect = residualEffect
            if ( blockName == "" )
              .Object@blockName = .Object@residualEffect@observeName
            else
              .Object@blockName= blockName
            .Object@varName=""
            .Object@iVarName=""

            callNextMethod(.Object,...,blockName=.Object@blockName)

          })
assign("ObservationContinuous",ObservationContinuous,envir=.GlobalEnv)

#' print.ObservationContinuous
#'
#' Prints Continuous observation information
#'
#' @param obj      Continuous observation name
#'
#' @examples
#'       print.ObservationContinuous(obj="Ex_Amt")
#'
#' @export print.NlmeExpression
#'
print.ObservationContinuous <-function(x, ...)
{
  print(paste0("-Continuous Obs-"))
  print(paste0("ID             : ",x@compKey))
  print(paste0("Name           : ",attr(x,"blockName")))
  print(paste0("Variable Name  : ",x@residualEffect@observeName))
  print(paste0("style          : ",ErrorTypeNames[x@residualEffect@errorType]))
}



setMethod("clearInPortNames",
          signature="ObservationContinuous",
          definition = function(.Object){

            .Object@iVarName = ""
            .Object
          })


setMethod("getNumInPorts",
          signature="ObservationContinuous",
          definition = function(.Object){
            numInPorts = 1
            numInPorts

          })



setMethod("getNumOutPorts",
          signature="ObservationContinuous",
          definition = function(.Object){
            numOutPorts = 0

            numOutPorts

          })

setMethod("getInPortName",
          signature="ObservationContinuous",
          definition = function(.Object,indx){
            name = ""
            if ( .Object@iVarName == "" )
              name = .Object@varName
            else
              name = .Object@iVarName
            name
          })
setMethod("getOutPortName",
          signature="ObservationContinuous",
          definition = function(.Object,indx){
            name = ""
            name
          })
setMethod("setInPortName",
          signature="ObservationContinuous",
          definition = function(.Object,indx,name){
            .Object@varName = name
            .Object
          })

setMethod("genStructuralParameters",
          signature="ObservationContinuous",
          definition = function(.Object,hasRandomEffect){
            sps=c()
            type=.Object@residualEffect@errorType
            if ( type == MixRatio || type == AdditiveMultiplicative) {
              sp = NlmeStructuralParameter(name=.Object@residualEffect@definition,
                                           hasRandomEffect = FALSE ,
                                           hasCovariateEffect = FALSE,
                                           isSequential  = .Object@isSequential)
              sps=c(sps,sp)
            }
            sps
          })

setMethod("genObserveError",
          signature="ObservationContinuous",
          definition = function(.Object){
            statements=c()
            if ( length(.Object@customCodeLines) > 0 ) {
              for ( l in .Object@customCodeLines){
                statements = c(statements,l)
              }
            }
            statements = generateObserveErrorStatements(c(.Object@residualEffect))
            statements
          })


#'
#' @export
Logit=1
#'
#' @export
Probit=2
#'
#' @export
LogLog=3
#'
#' @export
Cloglog=4
#'
#' @export
Custom=5

LinkTypeNames=c("Inverse Sigmoid","Inverse Cumulative Distribution","Logarithmic","Complementary Logarithmic","Custom")

LinkFuncs=c("ilogit","iprobit","iloglog","icloglog")

#' ObservationCategorical
#'
#' Class represents an NLME Categorical observation
#'
#' @param observationName    Observation name
#' @param name
#' @param linkFunction       Logit|Probit|LogLog|Cloglog|InvCustom
#' @param isInhibitory       Is the array of offset expressions in ascending order
#' @param slope              Slope expression
#' @param numOutcomes        Number of outcomes
#' @param offsetArray        Offset expressions
#' @param dobefore           Code to execute before effect is observed
#' @param doafter            Code to execute after effect is observed
#' @param customCodeLines    Lines of PML code to insert before LL() statement
#'
#'
#' @examples
#'
#' @export ObservationCategorical
#'
ObservationCategorical= setClass("ObservationCategorical",representation(
  observationName="character",
  name="character",
  iName="character",
  linkFunction="numeric",
  isInhibitory ="logical",
  slope="character",
  numOutcomes="numeric",
  offsetArray="list",
  dobefore="character",
  doafter="character"),
  contains = "ObservationCompartment")


setMethod("initialize","ObservationCategorical",

          function(.Object,
                   ...,
                   linkFunction = Logit,
                   observationName="",
                   name="",
                   slope="",
                   isInhibitory=FALSE,
                   numOutcomes=0,
                   offsetArray=c(),
                   dobefore="",
                   doafter="",
                   blockName=""){

            if ( blockName == "" )
              blockName = observationName
            .Object@blockName = blockName
            if ( name =="" )
              name  = "C"
            .Object@iName = ""
            .Object@linkFunction = linkFunction
            .Object@observationName = observationName
            .Object@name = name
            .Object@isInhibitory = isInhibitory
            .Object@slope = slope
            .Object@numOutcomes = numOutcomes
            if ( length(offsetArray) == 0  )
              .Object@offsetArray = list()
            else
              .Object@offsetArray = as.list(offsetArray)
            .Object@dobefore=dobefore
            .Object@doafter=doafter

            callNextMethod(.Object, ...,blockName=.Object@blockName,name=name)



          })
assign("ObservationCategorical",ObservationCategorical,envir=.GlobalEnv)
#' print.ObservationCategorical
#'
#' Prints Categorical observation information
#'
#' @param obj      Categorical observation name
#'
#' @examples
#'       print.ObservationCategorical(obj="pain")
#'
#' @export print.ObservationCategorical
#'
print.ObservationCategorical <-function(x, ...)
{
  print(paste0("Categorical Obs"))
  print(paste0("ID             :",x@compKey))
  print(paste0("Name           : ",x@blockName))
  print(paste0("Obs. Name      : ",x@observationName))
  print(paste0("Port Name      : ",x@name))
  print(paste0("style          : ",LinkTypeNames[x@linkFunction]))
}



setMethod("clearInPortNames",
          signature="ObservationCategorical",
          definition = function(.Object){

            .Object@iName = ""
            .Object
          })


setMethod("getNumInPorts",
          signature="ObservationCategorical",
          definition = function(.Object){
            numInPorts = 1
            numInPorts

          })



setMethod("getNumOutPorts",
          signature="ObservationCategorical",
          definition = function(.Object){
            numOutPorts = 0

            numOutPorts

          })

setMethod("getInPortName",
          signature="ObservationCategorical",
          definition = function(.Object,indx){
            if ( .Object@iName == "" )
              name = .Object@name
            else
              name = .Object@iName
            name
          })
setMethod("getOutPortName",
          signature="ObservationCategorical",
          definition = function(.Object,indx){
            name = ""
            name
          })
setMethod("setInPortName",
          signature="ObservationCategorical",
          definition = function(.Object,indx,name){
            .Object@iName = name
            .Object
          })

setMethod("genObserveError",
          signature="ObservationCategorical",
          definition = function(.Object){
            statements=c()
            if ( .Object@iName == "" )
              name = .Object@name
            else
              name = .Object@iName
            if ( length(.Object@customCodeLines) > 0 ) {
              for ( l in .Object@customCodeLines){
                statements = c(statements,l)
              }
            }
            statement = paste0("    multi(",.Object@observationName, ", ",
                               LinkFuncs[.Object@linkFunction])

            offsetstring = ""
            if ( .Object@isInhibitory )
              sign = "-"
            else
              sign = "+"
             for ( offset in .Object@offsetArray ) {
               offsetstring = paste0(offsetstring,", ", name, " * ", .Object@slope ,
                                     " ",sign,"  (",offset,")")

             }
            statement = paste0(statement, offsetstring)
            if ( .Object@dobefore != "" )  {
                statement = paste0(statement, ", dobefore =  ",.Object@dobefore )
            }
            if ( .Object@doafter != "" )  {
              statement = paste0(statement, ", doafter =  ",.Object@doafter )
            }
            statement = paste0(statement,")")
            statements = c(statements, statement)
            statements
          })




#' ObservationEventCounts
#'
#' Class represents an NLME Event/Count observation
#'
#' @param isEvent            Is it an event observation
#' @param observationName    Observation name
#' @param name
#' @param slopeExpression    Hazard slope expression
#' @param expression         Base hazard expression
#' @param dobefore           Code to execute before effect is observed
#' @param doafter            Code to execute after effect is observed
#' @param customCodeLines    Lines of PML code to insert before LL() statement
#'
#'
#' @examples
#'
#' @export ObservationEventCount
#'
ObservationEventCount= setClass("ObservationEventCount",representation(
  isEvent = "logical",
  observationName="character",
  name="character",
  iName="character",
  slopeExpression="character",
  expression="character",
  dobefore="character",
  doafter="character"),
  contains = "ObservationCompartment")


setMethod("initialize","ObservationEventCount",

          function(.Object,
                   ...,
                   isEvent=TRUE,
                   name="",
                   observationName="EvntObs",
                   slopeExpression="",
                   expression="",
                   dobefore="",
                   doafter="",
                   blockName=""){

            if ( blockName == "" )
              blockName = observationName
            .Object@blockName = blockName
            if ( name =="" )
              name  = "C"
            .Object@iName = ""
            .Object@isEvent = isEvent
            .Object@observationName = observationName
            .Object@name = name
            .Object@expression = expression
            .Object@slopeExpression = slopeExpression
            .Object@dobefore=dobefore
            .Object@doafter=doafter

            callNextMethod(.Object, ...,blockName=.Object@blockName,
                           expression=.Object@expression)



          })
assign("ObservationEventCount",ObservationEventCount,envir=.GlobalEnv)
#' print.ObservationEventCount
#'
#' Prints Event/Count observation information
#'
#' @param obj      Event/Count observation name
#'
#' @examples
#'       print.ObservationEventCount(obj="seizure")
#'
#' @export print.ObservationEventCount
#'
print.ObservationEventCount <-function(x, ...)
{
  if ( x@isEvent )
    print(paste0("Event Obs"))
  else
    print(paste0("Count Obs"))
  print(paste0("ID              : ",x@compKey))
  print(paste0("Name            : ",x@blockName))
  print(paste0("Obs. Name       : ",x@observationName))
  print(paste0("Port Name       : ",x@name))
  print(paste0("Slope Expression: ",x@slopeExpression))
  print(paste0("Expression      : ",x@expression))

}



setMethod("clearInPortNames",
          signature="ObservationEventCount",
          definition = function(.Object){

            .Object@iName = ""
            .Object
          })


setMethod("getNumInPorts",
          signature="ObservationEventCount",
          definition = function(.Object){
            numInPorts = 1
            numInPorts

          })



setMethod("getNumOutPorts",
          signature="ObservationEventCount",
          definition = function(.Object){
            numOutPorts = 0

            numOutPorts

          })

setMethod("getInPortName",
          signature="ObservationEventCount",
          definition = function(.Object,indx){
            if ( .Object@iName == "" )
              name = .Object@name
            else
              name = .Object@iName
            name
          })
setMethod("getOutPortName",
          signature="ObservationEventCount",
          definition = function(.Object,indx){
            name = ""
            name
          })
setMethod("setInPortName",
          signature="ObservationEventCount",
          definition = function(.Object,indx,name){
            .Object@iName = name
            .Object
          })

setMethod("genObserveError",
          signature="ObservationEventCount",
          definition = function(.Object){

            statements=c()
            if ( .Object@iName == "" )
              name = .Object@name
            else
              name = .Object@iName
            if ( .Object@isEvent )
              directive="event"
            else
              directive="count"
            if ( length(.Object@customCodeLines) > 0 ) {
              for ( l in .Object@customCodeLines){
                statements = c(statements,l)
              }
            }
            statement=paste0("    ",directive,"(",.Object@observationName,",")
            if ( .Object@slopeExpression == "" )
              statement = paste0(statement, .Object@expression)
            else
              statement = paste0(statement, name, "*", .Object@slopeExpression,
                                 " + (" , .Object@expression,")")

            if ( .Object@dobefore != "" )  {
              statement = paste0(statement, ", dobefore =  ",.Object@dobefore )
            }
            if ( .Object@doafter != "" )  {
              statement = paste0(statement, ", doafter =  ",.Object@doafter )
            }
            statement = paste0(statement,")")
            statements = c(statements, statement)
            statements
          })


#' ObservationLL
#'
#' Class represents an NLME LL observation
#'
#' @param observationName    Observation name
#' @param name
#' @param slopeExpression    Hazard slope expression
#' @param expression         Base hazard expression
#' @param dobefore           Code to execute before effect is observed
#' @param doafter            Code to execute after effect is observed
#' @param simulationCode     Lines of simulation code
#' @param customCodeLines    Lines of PML code to insert before LL() statement
#'
#' @examples
#'
#' @export ObservationLL
#'
ObservationLL= setClass("ObservationLL",representation(
  observationName="character",
  name="character",
  iName="character",
  slopeExpression="character",
  expression="character",
  dobefore="character",
  doafter="character",
  simulationCode="list"),
  contains = "ObservationEventCount")


setMethod("initialize","ObservationLL",

          function(.Object,
                   ...,
                   name="",
                   observationName="LLObs",
                   slopeExpression="",
                   expression="",
                   dobefore="",
                   doafter="",
                   blockName="",
                   simulationCode=c()){

            if ( blockName == "" )
              blockName = observationName
            .Object@blockName = blockName
            if ( name =="" )
              name  = "C"
            .Object@iName = ""
            .Object@observationName = observationName
            .Object@name = name
            .Object@expression = expression
            .Object@slopeExpression = slopeExpression
            .Object@dobefore=dobefore
            .Object@doafter=doafter
            .Object@simulationCode=as.list(simulationCode)


            callNextMethod(.Object, ...,blockName=.Object@blockName,
                           expression=.Object@expression,
                           observationName=.Object@observationName,
                           dobefore=.Object@dobefore,
                           doafter=.Object@doafter,
                           name=.Object@name,
                           slopeExpression=.Object@slopeExpression)


          })
assign("ObservationLL",ObservationLL,envir=.GlobalEnv)
#' print.ObservationLL
#'
#' Prints LL observation information
#'
#' @param obj      LL observation name
#'
#' @examples
#'       print.ObservationLL(obj="LL_Obs")
#'
#' @export print.ObservationLL
#'
print.ObservationLL <-function(x, ...)
{
  print(paste0("LL Obs"))
  print(paste0("ID              : ",x@compKey))
  print(paste0("Name            : ",x@blockName))
  print(paste0("Obs. Name       : ",x@observationName))
  print(paste0("Port Name       : ",x@name))
  print(paste0("Slope Expression: ",x@slopeExpression))
  print(paste0("Expression      : ",x@expression))
  print(paste0("Simulation Code : "))
  if ( length(x@simulationCode) > 0 )
    for ( s in x@simulationCode )
      print(s)
  if ( length(x@customCodeLines) > 0 )
    for ( s in x@customCodeLines )
      print(s)


}

setMethod("genObserveError",
          signature="ObservationLL",
          definition = function(.Object){

            statements=c()
            if ( .Object@iName == "" )
              name = .Object@name
            else
              name = .Object@iName

              directive="LL"

            if ( length(.Object@customCodeLines) > 0 ) {
              for ( l in .Object@customCodeLines){
                statements = c(statements,l)
              }
            }

            statement=paste0("    ",directive,"(",.Object@observationName,",")
            if ( .Object@slopeExpression == "" )
              statement = paste0(statement, .Object@expression)
            else
              statement = paste0(statement, name, "*", .Object@slopeExpression,
                                 " + (" , .Object@expression,")")

            if ( .Object@dobefore != "" )  {
              statement = paste0(statement, ", dobefore =  ",.Object@dobefore )
            }
            if ( .Object@doafter != "" )  {
              statement = paste0(statement, ", doafter =  ",.Object@doafter )
            }

            if ( length(.Object@simulationCode) != 0 ) {
              statements = c(statements, statement)
              statement=paste0("    , simulate = {\n")
              statements = c(statements, statement)
              for ( s in .Object@simulationCode ) {
                statements = c(statements,paste0("        ", s))
              }
              statements=c(statements,"    }")
              statements=c(statements,"    )")
            }
            else {
              statement = paste0(statement,")")
              statements = c(statements, statement)
            }
            statements
          })





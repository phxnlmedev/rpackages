
#' @export
STP_PRODUCT=1
#' @export
LogNormal=1
#' @export
STP_SUM_ETA=2
#' @export
Normal=2
#' @export
STP_SUM_EXP=3
#' @export
Combination=3
#' @export
STP_EXP_SUM=4
#' @export
Log=4
#' @export
STP_ILOGIT=5
#' @export
Logit=5
#' @export
STP_CUSTOM=6
#' @export
Custom=6

#'
#' Block random effects
#'
#' @export
#'
Block=1

#'
#' Diagonal random effects
#'
#' @export
#'
Diagonal=2


StructParamTypeNames = c("LogNormal","Normal","Combination","Log","Logit","Custom")
#' NlmeStrucParamStyle
#'
#' Class represents style of structural parameter
#'
#' @param style    Parameter style: 1=LogNormal, 2=Normal, 
#'                                  3=Combination, 4=Log, 
#'                                  5=Logit, 6=Custom
#'
#' @examples
#'        NlmeStrucParamStyle(style="1")
#'
#' @export NlmeStrucParamStyle
#'
NlmeStrucParamStyle= setClass("NlmeStrucParamStyle",representation(
                               style="numeric"))


setMethod("initialize","NlmeStrucParamStyle",
    function(.Object,
              style=STP_PRODUCT){
        pStyle = NlmeStrucParamStyle(style)
        .Object@style=pStyle
    })
assign("NlmeStrucParamStyle",NlmeStrucParamStyle,envir=.GlobalEnv)



#' NlmeStructuralParameter
#'
#' Class represents an NLME structural parameter
#'
#' @param name             Name of the structural parameter
#' @param fixedEffName     Name to use for fixed effects
#' @param randomEffName    Name to use for random effects
#' @param hasRandomEffect  Does the parameter have a random effect
#' @param style            Parameter style:
#'                           LogNormal, Normal, Combination, Log, Logit, Custom
#' @param initialValue     Initial value for the parameter
#' @param lowerBound       Lower limit for the parameter value
#' @param upperBound       Upper limit for the parameter value
#' @param units            Unit of measurement for the parameter
#' @param isFrozen         Is the parameter frozen
#' @param isSequential     Estimate the parameter sequentially
#' @param ranEffInitValue  Initial value for the random effect
#' @param code             For Custom style, PML code to override the definition
#' @param extraCode        Extra lines of code that relates to this parameter
#'
#' @examples
#'      NlmeStructuralParameter(STP_SUM_ETA,"EC50")
#'      NlmeStructuralParameter(STP_SUM_EXP,"Imax")
#'
#' @export NlmeStructuralParameter
#'
NlmeStructuralParameter= setClass("NlmeStructuralParameter",representation(
                                      name="character",
                                      fixedEffName="character",
                                      randomEffName="character",
                                      hasRandomEffect="logical",
                                      hasCovariateEffect="logical",
                                      style="numeric",
                                      initialValue="character",
                                      lowerBound="character",
                                      upperBound="character",
                                      units="character",
                                      isFrozen="logical",
                                      isSequential="logical",
                                      ranEffInitValue="character",
                                      code="ANY",
                                      extraCode="list"),
                                  contains = "StructuralBlock")


setMethod("initialize","NlmeStructuralParameter",
    function(.Object,
              name="",
              fixedEffName="",
              randomEffName="",
              hasRandomEffect=TRUE,
              hasCovariateEffect=TRUE,
              style=STP_PRODUCT,
              initialValue="1",
              lowerBound="",
              upperBound="",
              units="",
              isFrozen=FALSE,
              isSequential=FALSE,
              ranEffInitValue="1",
              code=list(),
              extraCode=list()){

         if ( fixedEffName == "" )
            fixedEffName=paste0("tv",name)
         if ( randomEffName == "" )
            randomEffName=paste0("n",name)
         if ( hasRandomEffect == FALSE )
            randomEffName=""
        .Object@name=name
        .Object@fixedEffName=fixedEffName
        .Object@randomEffName=randomEffName
        .Object@hasRandomEffect=hasRandomEffect
        .Object@hasCovariateEffect=hasCovariateEffect
        .Object@style=style
        .Object@initialValue=initialValue
        .Object@lowerBound=lowerBound
        .Object@upperBound=upperBound
        .Object@units=units
        .Object@isFrozen=isFrozen
        .Object@ranEffInitValue=ranEffInitValue
        .Object@isSequential=isSequential
        .Object@code=code
        .Object@extraCode=extraCode
        .Object
    })

assign("NlmeStructuralParameter",NlmeStructuralParameter,envir=.GlobalEnv)

#'
#' print.NlmeStructuralParameter
#'
#' Prints structural parameter information
#'
#' @param obj    Structural parameter
#'
#' @examples
#'       print.NlmeStructuralParameter(obj="EC50")
#'
#' @export print.NlmeStructuralParameter
#'
print.NlmeStructuralParameter <-function(x, ...)
{
    print(paste("Name           : ",attr(x,"name")))
    print(paste("Frozen         :", attr(x,"isFrozen")))
    print(paste("isSequential   :", attr(x,"isSequential")))
    print(paste("hasRandomEffect:", attr(x,"hasRandomEffect")))
#    print(paste("hasCovarEffect :", attr(x,"hasCovariateEffect")))
    print(paste("Type           :", StructParamTypeNames[
                                          attr(x,"style")]))
    print(paste("Initial Value  :", attr(x,"initialValue")))
    print(paste("Lower Bound    :", attr(x,"lowerBound")))
    print(paste("Upper Bound    :", attr(x,"upperBound")))
    print(paste("Units          :", attr(x,"units")))
    print(paste("code           :", paste(attr(x,"code"))))
    print(paste("extraCode      :", paste(attr(x,"extraCode"))))
}

setMethod("genStructuralParameters",
          signature="NlmeStructuralParameter",
          definition = function(.Object,hasRandomEffect){

            sps=c(.Object)

            sps
          })



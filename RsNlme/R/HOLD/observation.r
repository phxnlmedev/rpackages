
#'
#' @export
InvLogit=1
#'
#' @export
InvProbit=2
#'
#' @export
InvLogLog=3
#'
#' @export
InvClogClog=4
#'
#' @export
InvCustom=5

#' ObservationCategorical
#'
#' Class represents an NLME Categorical observation
#'
#' @param name               Observation name
#' @param linkFunction       InvLogit|InvProbit|InvLogLog|InvClogClog|InvCustom
#' @param isInhibitory       Is categorical model inhibitory
#' @param slope              Slope expression
#' @param numOutcomes        Number of outcomes
#' @param interceptArray     Intercept expressions
#'
#' @examples
#'
#' @export ObservationCategorical
#'
ObservationCategorical= setClass("ObservationCategorical",representation(
  name="character",
  linkFunction="numeric",
  isInhibitory ="logical",
  slope="character",
  numOutcomes="numeric",
  interceptArray="character"))

assign("ObservationCategorical",ObservationCategorical,env=.GlobalEnv)


setMethod("initialize","ObservationCategorical",
          function(.Object,
                   linkFunction = Additive,
                   name="",
                   slope="",
                   isInhibitory=FALSE,
                   numOutcomes=0,
                   interceptArray=""){
            
            .Object@linkFunction = linkFunction
            .Object@name = name
            .Object@isInhibitory = isInhibitory
            .Object@slope = slope
            .Object@numOutcomes = numOutcomes
            .Object@interceptArray = interceptArray
            .Object
            
            
          })
#' print.ObservationCategorical
#'
#' Prints categorical observation information
#'
#' @param obj      Model compartment
#'
#' @examples
#'       print.ObservationCategorical(obj="linear")
#'
#' @export print.ObservationCategorical
#'
print.ObservationCategorical <-function(obj)
{
  print(paste("Name           : ",attr(obj,"name")))
  print(paste("style           : ",LinkTypeNames[attr(obj,"linkFunction")]))
}




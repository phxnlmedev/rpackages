#' SecondaryParameter
#'
#' Class represents an NLME secondary parameter
#'
#' @param name          Name of the secondary parameter
#' @param definition    Definition of secondary variable
#' @param unit          Optional units 
#'
#' @examples
#'      param = SecondaryParameter("Spc_Param","log(2)/tvKe")
#'
#'      param = SecondaryParameter("Tmax",
#'                                 "CalcTMax(tvA,tvAlpha,tvB,tvBeta,C,Gamma)")
#'
#' @export SecondaryParameter
#'
SecondaryParameter= setClass("SecondaryParameter",representation(
                                      name="character",
                                      definition="character",
                                      unit="character"))


setMethod("initialize","SecondaryParameter",
    function(.Object,
              name,
              definition,
              unit=""){

        .Object@name=name
        .Object@definition=definition
        .Object@unit=unit
        .Object
    })

assign("SecondaryParameter",SecondaryParameter,envir=.GlobalEnv)
#' print.SecondaryParameter
#'
#' Prints secondary parameter information
#'
#' @param obj    Secondary parameter
#'
#' @examples
#'       print.SecondaryParameter(obj)
#'
#' @export print.SecondaryParameter
#'
print.SecondaryParameter <-function(x, ...)
{
    print(paste("Name           : ",attr(x,"name")))
    print(paste("Definition     :", attr(x,"definition")))
    print(paste("Unit           :", attr(x,"unit")))
}


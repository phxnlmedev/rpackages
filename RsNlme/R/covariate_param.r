
#' @export
COVAR_CONTINUOUS=1
#' @export
Continuous=1
#' @export
COVAR_CATEGORY=2
#' @export
Category=2
#' @export
COVAR_OCCASION=3
#' @export
Occasion=3
#' @export
CovarTypeNames=c("Continuous","Category","Occasion")

#' @export
COVAR_NUMBER=1
#' @export
CovarNumber=1
#' @export
COVAR_MEAN=2
#' @export
CovarMean=2
#' @export
COVAR_MEDIAN=3
#' @export
CovarMedian=3
#' @export
CovarEvalNames=c("Number","Mean","Median")

#' @export
COVAR_BACKWARD=1
#' @export
Backward=1
#' @export
COVAR_INTERPOLATE=2
#' @export
Interpolate=2
#' @export
COVAR_FORWARD=3
#' @export
Forward=3
#' @export
CovarDirectionNames=c("Backward","Interpolate","Forward")

#' @export
COVAR_EFF_YES=1
#' @export
EnableEffect=1
#' @export
COVAR_EFF_NO=2
#' @export
DisableEffect=2
#' @export
COVAR_EFF_PLUS_ONE=3
#' @export
PlusOneEffect=3
#' @export
CovarEffNames=c("Yes","No","PlusOne")


#' NlmeCovarItem
#'
#' Class represents an NLME Covariate parameter
#'
#' @param name      Name of covariate
#' @param value     Covariate value
#'
#' @examples
#'       NlmeCovarItem(name,value)
#'
#' @export NlmeCovarItem
#'
NlmeCovarItem= setClass("NlmeCovarItem",representation(
                               name="character",
                               value="numeric"))


setMethod("initialize","NlmeCovarItem",
    function(.Object,
              name="",
              value=0){
        .Object@name=name
        .Object@value=as.integer(value)
        .Object
    })



assign("NlmeCovarItem",NlmeCovarItem,envir=.GlobalEnv)
#' NlmeCovariateParameter
#'
#' Class represents an NLME Covariate parameter
#'
#' @param name             Name of covariate parameter
#' @param type             Type of covariate parameter: Continuous|Category|Occasion
#' @param direction        Curve fitting method: Forward|Interpolate|Backward
#' @param isDiagonal       Is the structure diagonal (TRUE)| or box (FALSE)
#' @param centerValue      Covariate centering value
#' @param isPositive       Are the covariate values all positive
#' @param continuousType   Type of value to use for the centering value: 
#'                              CovarNumber|CovarMean|CovarMedian
#' @param covarEffList
#' @param covarItems
#'
#' @examples
#'       weight=NlmeCovariateParameter(name="weight",
#'                          type=Continuous,
#'                          continuousType=CovarNumber
#'                          centerValue="70",
#'                          direction=Forward)
#'
#' @export NlmeCovariateParameter
#'
NlmeCovariateParameter= setClass("NlmeCovariateParameter",representation(
                                      name="character",
                                      type="numeric",
                                      direction="numeric",
                                      isDiagonal="logical",
                                      centerValue="character",
                                      isPositive="logical",
                                      continuousType="numeric",
                                      covarEffList="list",
                                      catEffInitValues="list",
                                      covarItems="list"))


setMethod("initialize","NlmeCovariateParameter",
    function(.Object,
              name="",
              type=Continuous,
              direction=Forward,
              centerValue="",
              isDiagonal=TRUE,
              isPositive=TRUE,
              continuousType=CovarNumber){

        .Object@name=name
        .Object@isDiagonal=isDiagonal
        .Object@type=type
        .Object@direction=direction
        .Object@centerValue=centerValue
        .Object@isPositive=isPositive
        .Object@continuousType=continuousType
        .Object@catEffInitValues=list()
        .Object
    })
assign("NlmeCovariateParameter",NlmeCovariateParameter,envir=.GlobalEnv)

#' print.NlmeCovariateParameter
#'
#' Prints covariate parameter information
#'
#' @param obj      Model covariate
#'
#' @examples
#'       print.NlmeCovariateParameter("weight”)
#'
#' @export print.NlmeCovariateParameter
#'
print.NlmeCovariateParameter <-function(x, ...)
{
    type = attr(x,"type")
    if ( type == COVAR_CONTINUOUS ) {
        print(paste(attr(x,"name"), " : Continuous"))
        print(paste("Direction : ",CovarDirectionNames[attr(x,"direction")]))
        print(paste("Center    : ",attr(x,"centerValue")))
        print(paste("Type      : ",CovarEvalNames[attr(x,"continuousType")]))
        print(paste("Positive? : ",attr(x,"isPositive")))
    } else if ( type == Category ) {
        print(paste(attr(x,"name"), " : Category"))
        print(paste("Direction : ",CovarDirectionNames[attr(x,"direction")]))
        l=attr(x,"covarItems")
        for ( c in l )
            print(paste0(attr(c,"name"), " " , attr(c,"value")))
    } else if ( type == COVAR_OCCASION ) {
        print(paste(attr(x,"name"), " : Occasion"))
        print(paste("Direction : ",CovarDirectionNames[attr(x,"direction")]))
        l=attr(x,"covarItems")
        for ( c in l )
            print(paste0(attr(c,"name"), " " , attr(c,"value")))
    }
    print(attr(x,"covarEffList"))
    print("")
}

#' covariateToString
#’
#’ Creates a string of all attributes for a covariate
#’
#’ @param obj       Model covariate
#’
#’ @examples
#’       covariateToString(weight)
#’
#' @export covariateToString
#'
covariateToString <-function(obj)
{
    str=""
    type = attr(obj,"type")
    if ( type == COVAR_CONTINUOUS ) {
        str=sprintf("%s",attr(obj,"name"))
    } else if ( type == Category ) {
        str=sprintf("%s(",attr(obj,"name"))
        l=attr(obj,"covarItems")
        sep=""
        for ( c in l ) {
            str=paste(str, sprintf("%s\"%s\"=%d",sep,attr(c,"name"),
                                                attr(c,"value")))
            sep=","
        }
        str=paste(str,")")
    } else if ( type == COVAR_OCCASION ) {
        str=sprintf("%s(",attr(obj,"name"))
        l=attr(obj,"covarItems")
        sep=""
        for ( c in l )  {
            str=paste(str, sprintf("%s\"%s\"=%d",attr(c,"name"),
                      attr(c,"value")))
            sep=";"
        }
    }
    return(str)
}

#' covariatePartsString
#’
#’ Creates string from each covariate attribute
#’
#’ @param obj       Model covariate
#’
#’ @examples
#’       covariatePartsString(weight)
#’
#' @export covariatePartsString
#'
covariatePartsString <-function(obj)
{
    str=""
    type = attr(obj,"type")
    if ( type == COVAR_CONTINUOUS ) {
        str=""
    } else if ( type == Category ) {
        str="("
        l=attr(obj,"covarItems")
        sep=""
        for ( c in l ) {
            str=paste0(str, sprintf("%s\"%s\"=%d",sep,attr(c,"name"),
                                                attr(c,"value")))
            sep=" ,"
        }
        str=paste0(str,")")
    } else if ( type == COVAR_OCCASION ) {
        str="("
        l=attr(obj,"covarItems")
        sep=""
        for ( c in l )  {
            str=paste(str, sprintf("%s\"%s\"=%d",sep,attr(c,"name"),
                      attr(c,"value")))
            sep=","
        }
        str=paste0(str,")")
    }
    return(str)
}

#' getCovariate
#'
#' Retrieve a covariate from a list
#'
#' @param model       Name of model
#' @param varname     Name of the covariate variable
#'
#' @examples
#'
#' @export getCovariate
#'
getCovariate <-function(model,varname){
    covarList = attr(model,"covariateList")
    for ( indx in 1:length(covarList) ){
        covar=covarList[[indx]]
        name=attr(covar,"name")
        if ( name == varname )
            return(covar)
    }
}

#'
#' continuousCovariate
#’
#’ Convenience method to create a continuous covariate
#’
#' @param name              Name of the covariate
#' @param centerValue       Covariate centering value
#' @param isPositive        Are the covariate values all positive
#' @param direction         Curve fitting method: Forward|Interpolate|Backward
#' @param continuousType    Type of value to use for the centering value:
#'                               CovarNumber|CovarMean|CovarMedian
#’
#’ @examples
#’       continuousCovariate(name=”weight”,
#’                           centerValue=0,
#’                           isPositive=FALSE,
#’                           direction=Backward,
#’                           continuousType=CovarNumber)
#’
#' @export continuousCovariate
#'
continuousCovariate <-function(name="",
                               centerValue=0,
                               isPositive=FALSE,
                                direction=Backward,
                               continuousType=CovarNumber) {
    covar=NlmeCovariateParameter(name,type=Continuous,
                                 isPositive=isPositive,
                                 centerValue=centerValue,
                                 continuousType=continuousType,
                                 )
    return(covar)
}

#' categoricalCovariate
#'
#' Convenience method to create a categorical covariate
#'
#' @param name           Name of the covariate
#' @param categories     Numerical values for each category
#' @param categorNames   Names of each category
#' @param direction      Curve fitting method: Forward|Backward|Interpolate
#'
#' @examples
#'       sex=categoricalCovariate("sex",
#'                        c(1,2),
#'                        c("female","male"),
#'                        direction=Forward)
#'
#' @export categoricalCovariate
#'
categoricalCovariate <-function(name="",
                                categories=c(),
                                categoryNames=c(),
                                direction=Backward) {
    covar=NlmeCovariateParameter(name,type=Category)
    l = attr(covar,"covarItems")
    for ( i in 1:length(categories) ) {
        value = categories[[i]]
        if ( length(categoryNames) >= i )
            name = categoryNames[[i]]
        else
            name = ""
        catItem=NlmeCovarItem(name,value)
        l=c(l,catItem)
    }
     attr(covar,"covarItems") = l
    return(covar)
}

#' occasionCovariate
#'
#' Convenience method to create an occasion covariate
#'
#' @param name           Name of the covariate
#' @param occasions      Numerical values for each occasion
#' @param occasionNames  Names of each occasion
#' @param direction      Curve fitting method: Forward|Backward|Interpolate
#' @param isDiagonal     Is the structure diagonal (TRUE) or box (FALSE)
#'
#' @examples
#'       occ=occasionCovariate("OCC",
#'                         c(1,2),
#'                         c("OCC1","OCC2"),
#'                         direction=Forward)
#'
#' @export occasionCovariate
#'
occasionCovariate <-function(name="",
                                occasions=c(),
                                occasionNames=c(),
                                direction=Backward,
                                isDiagonal=TRUE) {
    covar=NlmeCovariateParameter(name,type=Occasion,isDiagonal=isDiagonal)
    l = attr(covar,"covarItems")
    for ( i in 1:length(occasions) ) {
        value = occasions[[i]]
        if ( length(occasionNames) >= i )
            name = occasionNames[[i]]
        else
            name = ""
        catItem=NlmeCovarItem(name,value)
        l=c(l,catItem)
    }
     attr(covar,"covarItems") = l
    return(covar)
}


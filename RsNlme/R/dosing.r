
#' Steady State Dose
#' @export
SteadyStateDose=1

#' Addl Dose
#' @export
AddlDose=2

DoseTypeNames=c("Steady State","ADDL")

#' Bolus Dose
#' @export
BolusDose=1
#' Infusion Dose
#' @export
InfusionDose=2

DoseItemTypeNames=c("Bolus","Infusion")

#'
#' @export
ValueType=1
#'
#' @export
ColumnType=2


#' ExtraDoseDataType
#'
#' @param type       ValueType|ColumnType
#' @param value      Value of dosing parameter
#' @param column     Name of column containing dose information
#'
#' @examples
#'
#' @export ExtraDoseDataType
#'
ExtraDoseDataType= setClass("ExtraDoseDataType",representation(
  type="numeric",
  value="numeric",
  column="character"))



setMethod("initialize","ExtraDoseDataType",
          function(.Object,
                   type=ValueType,
                   value=0,
                   column=""){
            .Object@type=type
            .Object@value=value
            .Object@column=column
            .Object
          })
assign("ExtraDoseDataType",ExtraDoseDataType,envir=.GlobalEnv)

#' print.ExtraDoseDataType
#'
#' @export
#'
print.ExtraDoseDataType <-function(x, ...)
{
  if ( x@type == ValueType )
    print(x@value)
  else
    print(x@column)
}

#' ExtraDoseItem
#'
#' @param name              Dose point name
#' @param type              BolusDose|InfusionDose
#' @param amount            Optional dose amount
#' @param rate              Optional dose rate
#' @param deltaTime         Optional delta time
#' @param amountColumn      Optional column name containing dose amount data
#' @param rateColumn        Optional column name containing dose rate data
#' @param deltaTimeColumn   Optional column name containing dose delta time
#' @param isSecondDose      Use second dose point on compartment
#'
#' @examples
#'
#' dose = ExtraDoseItem(type=BolusDose,
#'                      amountColumn="Aa",
#'                      deltaTimeColumn="ii")
#'
#' dose1 = ExtraDoseItem(type=InfusionDose,
#'                      amount=12,
#'                      rate=4,
#'                      deltaTime=1,
#'                      isSecondDose=TRUE)
#'
#' @export ExtraDoseItem
#'
ExtraDoseItem= setClass("ExtraDoseItem",representation(
  name="character",
  type="numeric",
  amount="ExtraDoseDataType",
  rate="ExtraDoseDataType",
  deltaTime="ExtraDoseDataType",
  isSecondDose="logical"))



setMethod("initialize","ExtraDoseItem",
          function(.Object,
                   name="",
                   type=BolusDose,
                   amount=0.0,
                   rate=0.0,
                   deltaTime=0.0,
                   amountColumn="",
                   rateColumn="",
                   deltaTimeColumn="",
                   isSecondDose=FALSE){

            .Object@type=type
            .Object@name=name
            .Object@isSecondDose=isSecondDose
            if ( amountColumn != "" ){
                .Object@amount=ExtraDoseDataType(type=ColumnType,column=amountColumn)
            } else {
                .Object@amount=ExtraDoseDataType(type=ValueType,value=amount)
            }
            if ( rateColumn != "" ){
              .Object@rate=ExtraDoseDataType(type=ColumnType,column=rateColumn)
            } else {
              .Object@rate=ExtraDoseDataType(type=ValueType,value=rate)
            }
            if ( deltaTimeColumn != "" ){
              .Object@deltaTime=ExtraDoseDataType(type=ColumnType,column=deltaTimeColumn)
            } else {
              .Object@deltaTime=ExtraDoseDataType(type=ValueType,value=deltaTime)
            }
            .Object@isSecondDose=isSecondDose
            .Object
          })
assign("ExtraDoseItem",ExtraDoseItem,envir=.GlobalEnv)

#' print.ExtraDoseItem
#'
#' @export
#'
print.ExtraDoseItem <-function(x, ...)
{
  name = attr(x,"name")
  type = DoseItemTypeNames[attr(x,"type")]
  print( paste0(name ))
  print(paste0("Type       : ", type))

  print("Amount     : ")
  print(x@amount)
  if ( type == InfusionDose){
    print("Rate       : ")
    print(x@rate)
  }
  print("Delta Time : ")
  print(x@deltaTime)

}
#' ExtraDoseOption
#'
#' Additional dosing information
#'
#' @param name       Dose name
#' @param doseType   SteadyStateDose|AddlDose
#' @param doses      List of treatment doses
#'
#' @examples
#'
#' dose = ExtraDoseItem(type=BolusDose,
#'                      amountColumn="Aa",
#'                      deltaTimeColumn="ii")
#'
#' dose1 = ExtraDoseItem(type=InfusionDose,
#'                      amount=12,
#'                      rate=4,
#'                      deltaTime=1,
#'                      isSecondDose=TRUE)
#'
#' dosePoint = ExtraDoseOption(name="SteadyState",
#'                           doseType=SteadyStateDose,
#'                           doses=c(dose,dose1) )
#'
#' model = addExtraDose(model,dosePoint)
#'
#'
#' @export ExtraDoseOption
#'
ExtraDoseOption= setClass("ExtraDoseOption",representation(
  name="character",
  doseType="numeric",
  doses="ANY"))


setMethod("initialize","ExtraDoseOption",
          function(.Object,
                   name="",
                   doseType=SteadyStateDose,
                   doses=NULL){

            .Object@name=name
            .Object@doseType=doseType
            .Object@doses=doses
            .Object
          })
assign("ExtraDoseOption",ExtraDoseOption,envir=.GlobalEnv)

#' print.ExtraDoseOption
#'
#' @param obj
#'
#' @export
#'
print.ExtraDoseOption <-function(x, ...)
{
  name = attr(x,"name")
  type = DoseTypeNames[attr(x,"doseType")]
  print( paste0(name , "  ", type))
  for ( d in x@doses ) {
    print(d)
  }
}



#' ResetColumnInfo
#'
#' @param low    Lower value of reset range
#' @param hi     Upper value of reset range
#'
#' @examples
#'
#' @export ResetColumnInfo
#'
ResetColumnInfo= setClass("ResetColumnInfo",representation(
  low="numeric",
  hi="numeric"))


assign("ResetColumnInfo",ResetColumnInfo,envir=.GlobalEnv)

setMethod("initialize","ResetColumnInfo",
          function(.Object,
                   low,
                   hi){
            .Object@low=low
            .Object@hi=hi
            .Object
          })

#' print.ExtraDoseDataType
#'
#' @export
#'
print.ExtraDoseDataType <-function(x, ...)
{
  if ( x@type == ValueType )
    print(x@value)
  else
    print(x@column)
}






#'
#' @export
No=1
#' Rate Dose
#' @export
#'
RateDose=2
#' Duration Dose
#' @export
DurationDose=3

DosePointNames = c("No", "RateDose", "DurationDose" )
#' DosePoint
#'
#' Defines a dosepoint for a compartment
#'
#' @param isZeroOrderAbsorption   One of No|RateDose|DurationDose
#' @param isBioavail              Does dose point has bioavailability
#' @param bioavailExpression      Bioavailability expression
#' @param isTlag                  Does dose have time lag
#' @param tlagExpression          Time lag expression
#' @param durationExpression
#' @param rateExpression
#'        Rate or Duration Expression has the form
#            "StructuralParameter;Formula"
#'        StructuralParameter is created if new to the model.
#'        Optional Formula defines how to express dose in PML
#' @param dobefore          Code to execute before dose is administrated
#' @param doafter           Code to execute after dose is administrated
#'
#' @examples
#'
#' dosePoint= DosePoint(isZeroOrderAbsorption = DurationDose,
#'                      durationExpression = "DUR",
#'                      isTlag = FALSE,
#'                      isBioavail = TRUE,
#'                      bioavailExpression = "logitF1;1-ilogit(logitF1)")
#'
#' absDosePoint= DosePoint(isZeroOrderAbsorption = NoDose,
#'                         isBioavail = TRUE,
#'                         bioavailExpression = "logitF1;ilogit(logitF1)")
#'
#' @export DosePoint
#'
DosePoint= setClass("DosePoint",representation(
  isZeroOrderAbsorption="numeric",
  isBioavail="logical",
  bioavailExpression="character",
  isTlag="logical",
  tlagExpression="character",
  isInfusion="logical",
  isDuration="logical",
  durationExpression="character",
  rateExpression="character",
  dobefore="character",
  doafter="character"))

assign("DosePoint",DosePoint,envir=.GlobalEnv)

setMethod("initialize","DosePoint",
          function(.Object,
                   isZeroOrderAbsorption=NoDose,
                   isBioavail=FALSE,
                   bioavailExpression="",
                   isTlag=FALSE,
                   tlagExpression="",
                   isInfusion=FALSE,
                   isDuration=FALSE,
                   durationExpression="",
                   rateExpression="",
                   dobefore = "",
                   doafter = ""){
            .Object@isZeroOrderAbsorption=isZeroOrderAbsorption
            .Object@isBioavail=isBioavail
            .Object@bioavailExpression=bioavailExpression
            .Object@isTlag=isTlag
            .Object@tlagExpression=tlagExpression
            .Object@durationExpression=durationExpression
            .Object@rateExpression=rateExpression
            .Object@dobefore = dobefore
            .Object@doafter = doafter
            .Object

          })




#'@export
DurationLine=1
#'@export
RateLine=2
#'@export
BioavailLine=3
#'@export
TlagLine=4



setGeneric(name="getDoseSP",
           def=function(.Object,what)
           {
             standardGeneric("getDoseSP")
           })

setMethod("getDoseSP",
          signature="DosePoint",
          definition = function(.Object,what){
            if ( what == DurationLine )
              line = .Object@durationExpression
            if ( what == RateLine )
              line = .Object@rateExpression
            if ( what == BioavailLine )
              line = .Object@bioavailExpression
            if ( what == TlagLine )
              line = .Object@tlagExpression
            tokens=unlist(strsplit(line,";"))
            if ( length(tokens) > 0 )
              return(tokens[[1]])
            else
              return("")

          })
assign("getDoseSP",getDoseSP,envir=.GlobalEnv)

setGeneric(name="getDoseExpr",
           def=function(.Object,what)
           {
             standardGeneric("getDoseExpr")
           })

setMethod("getDoseExpr",
          signature="DosePoint",
          definition = function(.Object,what){
            if ( what == DurationLine )
              line = .Object@durationExpression
            if ( what == RateLine )
              line = .Object@rateExpression
            if ( what == BioavailLine )
              line = .Object@bioavailExpression
            if ( what == TlagLine )
              line = .Object@tlagExpression
            tokens=unlist(strsplit(line,";"))
            if ( length(tokens) > 0 )
              return(tokens[[length(tokens)]])
            else
              return("")

          })
assign("getDoseExpr",getDoseExpr,envir=.GlobalEnv)


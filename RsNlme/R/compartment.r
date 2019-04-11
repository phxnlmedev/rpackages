#' @export
Central=1
#' @export
Absorption=2
#' @export
Peripheral=3
#' @export
Elimination=4


CompartmentTypeNames = c("Central","Absorption","Peripheral","Elimination")

#' CompartmentType
#'
#' Class represents type of compartment
#'
#' @param style    Compartment style: 1=Central, 2=Absorption, 3=Peripheral, 4=Elimination 
#'
#' @examples
#'       CompartmentType(styl="1")
#'
#' @export CompartmentType
#'
CompartmentType= setClass("CompartmentType",representation(
                               styl="numeric"))

setMethod("initialize","CompartmentType",
    function(.Object,
              styl=Central){
        .Object@stye=styl
    })
assign("CompartmentType",CompartmentType,envir=.GlobalEnv)


#' @export
NoDose=1
#' @export
OneDose=2
#' @export
TwoDoses=3
#' @export
SplitDoses=4


CompartmentDoseTypeNames = c("NoDose","OneDose","TwoDoses","SplitDoses")

#' CompartmentDoseType
#'
#' Class represents type of dose
#'
#' @param dosetype    Dose type: 1=NoDose, 2=OneDose, 3=TwoDoses, 4=SplitDoses 
#'
#' @examples
#'        CompartmentDoseType(doseType="1")
#'
#' @export 
#'
CompartmentDoseType= setClass("CompartmentDoseType",representation(
                               doseType="numeric"))


#
# initializes  a CompartmentDoseType class
#
# @param .Object  Dosetype object
# @param doseType Type of dose
#
# @alias initialize,CompartmentDoseType-method
#
setMethod("initialize","CompartmentDoseType",
    function(.Object,
              doseType=NoDose){
        .Object@doseType=doseType

    })

assign("CompartmentDoseType",CompartmentDoseType,envir=.GlobalEnv)

#' StructuralBlock
#'
#' Class represents structural blocks
#'
#' @param blockName     Name of the structural block
#' @param compKey       Identifier for the block
#' @param isSequential  Is structural block in a sequential model 
#'
#' @examples
#'       StructuralBlock(blockName="Abs1",compKey="-1",isSequential=FALSE)
#'
#' @export StructuralBlock
#'
StructuralBlock= setClass("StructuralBlock",representation(
  blockName="character",
  compKey = "numeric",
  isSequential="logical"),
  prototype=list(
           blockName="",
           compKey = -1 ,
           isSequential=FALSE))


setMethod("initialize","StructuralBlock",
          function(.Object,
                   ...,
                   blockName = "",
                   compKey = -1 ,
                   isSequential=FALSE){
            .Object@blockName=blockName
            .Object@isSequential = isSequential
            callNextMethod(.Object,
                           ...,
                           blockName=blockName,
                           isSequential=isSequential)
          })
assign("StructuralBlock",StructuralBlock,envir=.GlobalEnv)

setGeneric(name="initObjFixedEffects",
           def=function(.Object,values)
           {
             standardGeneric("initObjFixedEffects")
           })

setMethod("initObjFixedEffects",
          signature="StructuralBlock",
          definition = function(.Object,values){
            .Object
          })

assign("initObjFixedEffects",initObjFixedEffects,envir=.GlobalEnv)

setGeneric(name="clearInPortNames",
           def=function(.Object)
           {
             standardGeneric("clearInPortNames")
           })

setMethod("clearInPortNames",
          signature="StructuralBlock",
          definition = function(.Object){
 #           print("StructuralBlock clearInPortNames()")
            .Object
          })
assign("clearInPortNames",clearInPortNames,envir=.GlobalEnv)

setGeneric(name="getInPortInUse",
           def=function(.Object,indx)
           {
             standardGeneric("getInPortInUse")
           })

setMethod("getInPortInUse",
          signature="StructuralBlock",
          definition = function(.Object,indx){
   #         print("StructuralBlock getInPortInUse()")
          })
assign("getInPortInUse",getInPortInUse,envir=.GlobalEnv)

setGeneric(name="getInPortName",
           def=function(.Object,indx)
           {
             standardGeneric("getInPortName")
           })

setMethod("getInPortName",
          signature="StructuralBlock",
          definition = function(.Object,indx){
#            print("StructuralBlock getInPortName()")
          })
assign("getInPortName",getInPortName,envir=.GlobalEnv)


setGeneric(name="getNumInPorts",
           def=function(.Object,indx)
           {
             standardGeneric("getNumInPorts")
           })

setMethod("getNumInPorts",
          signature="StructuralBlock",
          definition = function(.Object,indx){
  #          print("StructuralBlock getNumInPorts()")
          })
assign("getNumInPorts",getNumInPorts,envir=.GlobalEnv)


setGeneric(name="getNumOutPorts",
           def=function(.Object)
           {
             standardGeneric("getNumOutPorts")
           })

setMethod("getNumOutPorts",
          signature="StructuralBlock",
          definition = function(.Object){
  #          print("StructuralBlock getNumOutPorts()")
          })
assign("getNumOutPorts",getNumOutPorts,envir=.GlobalEnv)

setGeneric(name="getOutPortName",
           def=function(.Object,indx)
           {
             standardGeneric("getOutPortName")
           })

setMethod("getOutPortName",
          signature="StructuralBlock",
          definition = function(.Object,indx){
  #          print("StructuralBlock getOutPortName()")
          })
assign("getOutPortName",getOutPortName,envir=.GlobalEnv)

setGeneric(name="setInPortName",
           def=function(.Object,indx,name)
           {
             standardGeneric("setInPortName")
           })

setMethod("setInPortName",
          signature="StructuralBlock",
          definition = function(.Object,indx,name){
  #          print("StructuralBlock setInPortName()")
          })
assign("setInPortName",setInPortName,envir=.GlobalEnv)


setGeneric(name="genStructuralParameters",
           def=function(.Object,hasRandomEffect)
           {
             standardGeneric("genStructuralParameters")
           })

setMethod("genStructuralParameters",
          signature="StructuralBlock",
          definition = function(.Object,hasRandomEffect){

            sps=list()
            sps
          })
assign("genStructuralParameters",genStructuralParameters,envir=.GlobalEnv)

setGeneric(name="genStatements",
           def=function(.Object)
           {
             standardGeneric("genStatements")
           })

setMethod("genStatements",
          signature="StructuralBlock",
          definition = function(.Object){

            sps=list()
            sps
          })
assign("genStatements",genStatements,envir=.GlobalEnv)

setGeneric(name="genObserveError",
           def=function(.Object)
           {
             standardGeneric("genObserveError")
           })

setMethod("genObserveError",
          signature="StructuralBlock",
          definition = function(.Object){

            sps=list()
            sps
          })
assign("genObserveError",genObserveError,envir=.GlobalEnv)

setGeneric(name="genDoseStatements",
           def=function(.Object)
           {
             standardGeneric("genDoseStatements")
           })

setMethod("genDoseStatements",
          signature="StructuralBlock",
          definition = function(.Object){
  #          print("StructuralBlock genDoseStatements()")
            sps=list()
            sps
          })
assign("genDoseStatements",genDoseStatements,envir=.GlobalEnv)



setGeneric(name="genFlowRateODE",
           def=function(.Object,...)
           {
             standardGeneric("genFlowRateODE")
           })

setMethod("genFlowRateODE",
          signature="StructuralBlock",
          definition = function(.Object,key,objects){
  #          print("StructuralBlock genFlowRateODE()")
            sps=""
            sps
          })
assign("genFlowRateODE",genFlowRateODE,envir=.GlobalEnv)

#' NlmeCompartment
#'
#' Class represents an NLME compartment
#'
#' @param aName               Absorption variable name
#' @param cName               Observation variable name
#' @param vName               Volume variable name
#' @param feName              Fraction Excreted variable name
#' @param inputName           Compartment name
#' @param hasVolume           Does compartment have volume variable
#' @param isConcentration     Is compartment a concentration compartment
#' @param isFractionExcreted  Does compartment include a fraction excreted parameter
#' @param isSequential        Is compartment part of a sequential model
#' @param type                Central|Elimination|Peripheral
#'
#' @examples
#'     centralCompartment=NlmeCompartment(inputVariableName="A1",
#'                          aName="",
#'                          vName="V",
#'                          cName="C",
#'                          type=Central)
#'
#' @export NlmeCompartment
#'
NlmeCompartment= setClass("NlmeCompartment",representation(
                                      aName="character",
                                      cName="character",
                                      vName="character",
                                      feName="character",
                                      iaName="character",
                                      icName="character",
                                      ivName="character",
                                      ifeName="character",
                                      inputName="character",
                                      hasVolume ="logical",
                                      isConcentration="logical",
                                      isFractionExcreted="logical",
                                      numInPorts="numeric",
                                      inPorts="list",
                                      numOutPorts="numeric",
                                      outPorts="list",
                                      doses="list",
                                      type="numeric",
                                      numDosePoints="numeric",
#                                      effectsInfo="NlmeResidualEffect"),
                                      effectsInfo="ANY"),
                          contains = "StructuralBlock")


setMethod("initialize","NlmeCompartment",
    function(.Object,
             ...,
              type = Central,

              aName="A1",
              cName="C",
              vName="V",
              feName="",
              inputName="",
              isConcentration=TRUE,
              isFractionExcreted = FALSE,
#              effectsInfo=NlmeResidualEffect(),
              effectsInfo=NULL,
              numDosePoints=numDosePoints){

      # Check the names and make sure they are unique

        .Object@type = type
        .Object@aName=aName
        .Object@cName=cName
        .Object@vName=vName
        .Object@feName = feName
        .Object@iaName=""
        .Object@icName=""
        .Object@ivName=""
        .Object@ifeName = ""
        .Object@inputName = inputName
        .Object@isFractionExcreted = isFractionExcreted
        .Object@isConcentration=isConcentration
        .Object@numDosePoints=numDosePoints
        .Object@effectsInfo=effectsInfo

        # objects have a maximum of two dose points
        dose1 = DosePoint()
        dose2 = DosePoint()
        doses=list()
        doses[[1]] = dose1
        doses[[2]] = dose2
        .Object@doses=doses
        callNextMethod(.Object, ...)

    })
assign("NlmeCompartment",NlmeCompartment,envir=.GlobalEnv)
#' print.NlmeCompartment
#'
#' Prints compartment information
#'
#' @param obj    Model compartment
#'
#' @examples
#'       print.NlmeCompartment(obj="Abs1")
#'
#' @export print.NlmeCompartment
#'
print.NlmeCompartment <-function(x, ...)
{

    print(paste0("-- Compartment --"))
    print(paste0("ID              : ",x@compKey))
    print(paste0("Name            : ",attr(x,"blockName")))
    print(paste0("aName           : ",attr(x,"aName")))
    print(paste0("cName           : ",attr(x,"cName")))
    print(paste0("vName           : ",attr(x,"vName")))
    print(paste0("Type            : ",CompartmentTypeNames[attr(x,"type")]))
    print(paste0("Num Doses       : ",x@numDosePoints ))
}



setMethod("clearInPortNames",
          signature="NlmeCompartment",
          definition = function(.Object){
            .Object@ivName=""
            .Object@ifeName=""
            .Object
          })


#'
#' @export
#'
setMethod("getNumInPorts",
          signature="NlmeCompartment",
          definition = function(.Object){
            numInports = 0
            type = .Object@type
            if ( ( type == Central || type == Peripheral) &&   .Object@isConcentration )
                numInports = numInports + 1
            if ( type == Elimination && .Object@isFractionExcreted )
              numInports = numInports + 1
            numInports

          })



#'
#' @export
#'
setMethod("getNumOutPorts",
          signature="NlmeCompartment",
          definition = function(.Object){
            numOutports = 0
            type = .Object@type
            if ( ( type == Central || type == Peripheral) &&   .Object@isConcentration )
              numOutports = numOutports + 1

            numOutports

          })

#'
#' @export
#'
setMethod("getInPortName",
          signature="NlmeCompartment",
          definition = function(.Object,indx){
          name=""
#          browser()

            for ( n in 1:getNumInPorts(.Object) ) {
            if ( .Object@type == Central || .Object@type == Peripheral ){
              if ( indx == 1 && .Object@isConcentration ){
                if ( indx == n )
                  name = .Object@vName
              }
            }
            if ( .Object@type == Elimination)
              if ( indx == n )
                name = .Object@feName
            }
            name
          })

#'
#' @export
#'
setMethod("getOutPortName",
          signature="NlmeCompartment",
          definition = function(.Object,indx){
   #         print("NlmeCompartment getOutPortName()")
            name = .Object@aName
            if ( .Object@type == Central || .Object@type == Peripheral ){
              if ( indx == 1 && .Object@isConcentration )
                name = .Object@cName
            }
            name
          })


#'
#' @export
#'
setMethod("setInPortName",
          signature="NlmeCompartment",
          definition = function(.Object,indx,name){
   #         print("NlmeCompartment setInPortName()")
            n=1
            if ( .Object@type == Central || .Object@type == Peripheral ){
              if ( indx == 1 && .Object@isConcentration ){
                if ( indx == n )
                  .Object@ivName = name
                n = n + 1
              }
            }
            if ( .Object@type == Elimination)
              if ( indx == n )
                .Object@ifeName = name
            .Object
          })

#'
#' @export
#'
setMethod("genStructuralParameters",
          signature="NlmeCompartment",
          definition = function(.Object,hasRandomEffect){
            sps=c()

  #          print("NlmeCompartment genStructuralParameters()")
            if ( .Object@type == Central || .Object@type == Peripheral ){
              if ( .Object@isConcentration && .Object@ivName == "" ){
                  sp = NlmeStructuralParameter(name=.Object@vName,
                                             hasRandomEffect = hasRandomEffect ,
                                             hasCovariateEffect = FALSE,
                                             isSequential  = .Object@isSequential)
                  sps = c(sps,sp)
              }
            }
            if ( .Object@type == Elimination && .Object@isFractionExcreted ){
              if ( .Object@ifeName == "" ){
                sp = NlmeStructuralParameter(name=.Object@feName,
                                            hasRandomEffect = hasRandomEffect ,
                                            hasCovariateEffect = FALSE,
                                            isSequential  = .Object@isSequential)
                sps = c(sps,sp)
              }
            }
            if ( .Object@numDosePoints != 0 )
            for ( d in 1:.Object@numDosePoints ){
              doseInfo=.Object@doses[[d]]
              for ( l in 1:4 ) {

                sp=getDoseSP(doseInfo,l)
                if ( sp != "" ) {
                 sp = NlmeStructuralParameter(name=sp,
                                             hasRandomEffect = hasRandomEffect ,
                                             hasCovariateEffect = FALSE,
                                             isSequential  = .Object@isSequential)
                 sps = c(sps,sp)

                }
             }
            }

            sps
          })

#'
#' @export
#'
setMethod("genStatements",
          signature="NlmeCompartment",
          definition = function(.Object){
            statements=c()

            if ( .Object@isConcentration) {
              statement = paste0("    ",.Object@cName , " = ", .Object@aName, " / " , .Object@vName)
              statements = c(statements, statement)
            }
            if ( .Object@numDosePoints > 0 ) {
              for ( indx in 1:.Object@numDosePoints ) {
                aName = .Object@aName
              }
            }
            statements
          })

#'
#' @export
#'
setMethod("genDoseStatements",
          signature="NlmeCompartment",
          definition = function(.Object){
            statements=c()
            aName = .Object@aName
            if ( .Object@numDosePoints > 0  ){
              for ( indx in 1:.Object@numDosePoints ){
                d = .Object@doses[[indx]]
                if ( indx == 1 )
                  dose="    dosepoint("
                else
                  dose = "    dosepoint2("
                dose=paste0(dose,aName,",idosevar = ",aName,"Dose")
                if ( d@isTlag ) {
                  dose=paste0(dose,",tlag = (",getDoseExpr(d,TlagLine),")")
                }
                if ( d@isBioavail ) {
                  dose=paste0(dose,",bioavail = (",getDoseExpr(d,BioavailLine),")")
                }
                if ( d@isZeroOrderAbsorption == RateDose ) {
                  dose=paste0(dose,",rate = (",getDoseExpr(d,RateLine),")")
                }
                if ( d@isZeroOrderAbsorption == DurationDose ) {
                  dose=paste0(dose,",duration = (",getDoseExpr(d,DurationLine),")")
                }
                if ( indx == 1 )
                  dose=paste0(dose,",infdosevar = ",aName,"InfDose,infratevar = ",aName,"InfRate",actionString(d@dobefore,d@doafter),")")
                else
                  dose=paste0(dose,getDoseExpr(d@actionCode),")")

                statements=c(statements,dose)
              }
            }


            statements
          })

#'
#' @export
#'
setMethod("genFlowRateODE",
          signature="NlmeCompartment",
          definition = function(.Object,key,model){
            statement = ""
            totalFlowRate = ""

            for (indx in 1 : length(model@objects ))  {

              if ( class(model@objects[[indx]]) != "PkFlow" )
                next
              flow=model@objects[[indx]]
              if ( ( flow@fromPort < 0 ) || (flow@toPort < 0 ))
                next

              if ( flow@fromReceptorPort >= 0 ) {
                if ( (flow@fromPort == key) ||
                     (flow@toPort == key ) ||
                     ( flow@fromReceptorPort == key ))
                  next

                flowRate=""
                fromComp = getCompartment(model,flow@fromPort)
                toComp = getCompartment(model,flow@toPort)
                receptorComp = getCompartment(model,flow@fromReceptorPort)
                # flow out of cpt2 = koff * A2
                  flowOutOf2 = paste0(flow@k21Name , "*", toComp@aName)
                  # flow into cpt2 = kon * C0 * Cl
                  if ( fromComp@isConcentration )
                    c0 = fromComp@cName
                  else
                    c0 = fromComp@aName
                  if ( receptorComp@isConcentration )
                    c1 = receptorComp@cName
                  else
                    c1 = receptorComp@aName
                  flowInto2 = paste0(flow@k12Name , " * ", c0, " * " ,c1)

                  flowRate = paste0( flowInto2 , " - ",flowOutOf2)
                  if ( flow@fromPort == key  || flow@fromReceptorPort == key ) {
                    totalFlowRate = paste0( totalFlowRate , "- (",flowRate, ")")
                  }
                  if ( flow@toPort == key ){
                    if ( totalFlowRate != "" )
                      totalFlowRate = paste0(totalFlowRate, " + " )
                    totalFlowRate = paste0(totalFlowRate , "(", flowRate, ")")
                  }
              }
              else {
                flowRate=""
                fromCmpt = getCompartment(model,flow@fromPort)
                toCmpt = getCompartment(model,flow@toPort)

                if ( flow@flowType == FpMicro ){
                  flowRate = paste0( fromCmpt@aName , " * " , flow@k12Name )
                  if ( flow@twoWay )
                    flowRate = paste0(flowRate, "- ", toCmpt@aName, " * " , flow@k21Name)

                }
                if ( flow@flowType == FpClearance ) {
                  if ( flow@isVascular || !flow@twoWay ) {
                    flowRate = paste0( flowRate, flow@clName, " * ", fromCmpt@cName )
                  }
                  else if ( toCmpt@isConcentration) {
                    flowRate = paste0(flowRate, flow@derivedVascularClearance , " * (",
                                      fromCmpt@cName," - ", toCmpt@cName, ")" )
                  } else {
                    flowRate = paste0( flowRate, flow@clName, " * ", fromCmpt@cName )
                  }
                }
                if ( flow@flowType == FpSaturating ) {

                  flowRate = flow@vMaxName
                  if ( fromCmpt@isConcentration )
                    cName = fromCmpt@cName
                  else
                    cName = fromCmpt@aName
                  flowRate = paste0( flowRate , " * " , cName, " / (",cName, " + ", flow@kmName,")")
                }
                if ( flow@fromPort == key  ) {
                  totalFlowRate = paste0( totalFlowRate , "- (",flowRate, ")")
                }
                if ( flow@toPort == key ){
                  if ( totalFlowRate != "" )
                    totalFlowRate = paste0(totalFlowRate, " + " )
                  totalFlowRate = paste0(totalFlowRate , "(", flowRate, ")")
                }

              }

            }
            if ( totalFlowRate == "" )
              totalFlowRate = "0"
            if ( .Object@type == Elimination )
              statement = "    urinecpt"
            else
              statement = "    deriv"
            statement = paste0(statement, "(", .Object@aName, " = ", totalFlowRate)
            if ( .Object@type == Elimination && .Object@isFractionExcreted )
              statement = paste0(statement,", fe=",.Object@feName )
            statement = paste0(statement,")")

            statement
          })


#' eliminationCompartment 
#'
#' Creates an elimination compartment
#'
#' @param aName               Absorption variable name
#' @param vName               Volume variable name
#' @param cName               Observation variable name
#' @param feName              Fraction Excreted variable name
#' @param isSequential        Is compartment part of a sequential model
#' @param isFractionExcreted  Does compartment include a fraction excreted parameter
#' @param numDosePoints       Number of dose points
#' @param blockName           Name of the structural block
#'
#' @examples
#'     elimComp = eliminationCompartment(aName="Abs0",
#'                            vName="Vol0",
#'                            cName="Conc0",
#'                            feName="FrEx",
#'                            isSequential=FALSE,
#'                            isFractionExcreted=TRUE,
#'                            numDosePoints="2",
#'                            blockName="Elim1")
#'
#' @export eliminationCompartment
#'
eliminationCompartment <- function(aName="",
                                   vName="",
                                   cName="",
                                   feName="",
                                   isSequential,
                                   isFractionExcreted,
                                   numDosePoints=0,
                                   blockName=""){

  type=Elimination
  if( aName == "" || vName == "" || cName == "" ) {
    aName="A0"
    vName="V0"
    cName="C0"
    feName="Fe"
  }
  if ( blockName == "" )
    blockName=aName
  isConcentration=FALSE

  comp = NlmeCompartment(aName=aName,
                         vName=vName,
                         cName=cName,
                         feName=feName,
                         isConcentration=isConcentration,
                         isFractionExcreted = isFractionExcreted,
                         type=type,
                         numDosePoints = numDosePoints,
                         isSequential = isSequential,
                         blockName=blockName)

  comp
}

#' centralCompartment 
#'
#' Creates a central compartment
#'
#' @param aName             Absorption variable name
#' @param vName             Volume variable name
#' @param cName             Observation variable name
#' @param isConcentration   Are there volume and concentration parameters
#' @param isSequential      Is compartment part of a sequential model
#' @param numDosePoints     Number of dose points
#' @param blockName         Name of the structural block
#'
#' @examples
#'     cenComp = centralCompartment(aName="Abs1",
#'                        vName="Vol",
#'                        cName="Conc",
#'                        isConcentration=TRUE,
#'                        isSequential=FALSE,
#'                        numDosePoints="2",
#'                        blockName="Cent1")
#'
#' @export centralCompartment
#'
centralCompartment <- function(aName="",
                               vName="",
                               cName="",
                               isConcentration=TRUE,
                               isSequential=FALSE,
                               numDosePoints=0,
                               blockName=""){

  type=Central
  feName=""
  if( aName == "" || vName == "" || cName == "" ) {
    aName="A1"
    vName="V"
    cName="C"

  }
  if ( blockName == "" )
    blockName=aName
  isConcentration=TRUE

  comp = NlmeCompartment(
                         aName=aName,
                         vName=vName,
                         cName=cName,
                         isConcentration=isConcentration,
                         type=type,
                         numDosePoints = numDosePoints,
                         isSequential = isSequential,
                         blockName=blockName)

  comp
}

#' peripheralCompartment
#'
#' Creates a peripheral compartment
#'
#' @param aName             Absorption variable name
#' @param vName             Volume variable name
#' @param cName             Observation variable name
#' @param isConcentration   Are there volume and concentration parameters
#' @param isSequential      Is compartment part of a sequential model
#' @param numDosePoints     Number of dose points
#' @param compIndex         Number to append to variable names identifier
#' @param blockName         Name of the structural block
#'
#' @examples
#'     periComp = peripheralCompartment(aName="Abs1",
#'                        vName="Vol",
#'                        cName="Conc",
#'                        isConcentration=TRUE,
#'                        isSequential=FALSE,
#'                        numDosePoints="2",
#'                        blockName="Peri1")
#'
#' @export peripheralCompartment
#'

peripheralCompartment <- function(
                                  ...,
                                  aName="",
                                  vName="",
                                  cName="",
                                  isConcentration=FALSE,
                                  isSequential=FALSE,
                                  numDosePoints=0,
                                  compIndex=1,
                                  blockName=""){

  type = Peripheral
  feName=""
  if( aName == "" || vName == "" || cName == "" ) {
      aName=paste0("A",compIndex)
      vName=paste0("V",compIndex)
      cName=paste0("C",compIndex)
   }
  if ( blockName == "" )
    blockName=aName
  comp = NlmeCompartment(
                         ...,
                         aName=aName,
                         vName=vName,
                         cName=cName,
                         isConcentration=isConcentration,
                         type=type,
                         numDosePoints = numDosePoints,
                         isSequential = isSequential,
                         blockName=blockName)

  comp
}

#' absorptionCompartment
#'
#' Creates an absorption compartment
#'
#' @param aName             Absorption variable name
#' @param isConcentration   Are there volume and concentration parameters
#' @param effectsInfo       
#' @param isSequential      Is compartment part of a sequential model
#' @param numDosePoints     Number of dose points
#' @param blockName         Name of the structural block
#'
#' @examples
#'       abs = absorptionCompartment(aName="Abs", isConcentration=FALSE, 
#'                              effectsInfo=NULL,
#’                              isSequential=FALSE, 
#'                              numDosePoints="1",
#’                              blockName="Abs")
#'
#' @export absorptionCompartment
#'
absorptionCompartment <- function(aName="",
                                  isConcentration=FALSE,
                                  effectsInfo=NULL,
                                  isSequential=FALSE,
                                  numDosePoints=1,
                                  blockName=""){

  type = Absorption
  feName=""
  if( aName == ""  ) {
    aName="Aa"
  }
  if ( blockName == "" )
    blockName=aName
  isConcentration=FALSE
  comp = NlmeCompartment(aName=aName,
                         isConcentration=isConcentration,
                         type=type,
                         numDosePoints = numDosePoints,
                         effectsInfo = effectsInfo,
                         isSequential = isSequential,
                         blockName=blockName)

  comp
}












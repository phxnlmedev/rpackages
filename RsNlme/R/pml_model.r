

require(Certara.NLME8)

#' XimportFrom utils View write.table
#NULL
#' XimportFrom Certara.NLME8 NlmeDataset NlmeEngineExtraParams NlmeParallelHost
#NULL

#' @import Certara.NLME8 

#' @import methods

NlmeRandomEffectBlock= setClass("NlmeRandomEffectBlock",representation(
  type="numeric",
  effectNames="list"))
NlmeRandomEffectValues= setClass("NlmeRandomEffectValues")


#' @export
PARAM_PK_LINEAR=1
#' @export
PkLinear=1
#' @export
PARAM_PK=2
#' @export
Pk=2
#' @export
PARAM_EMAX=3
#' @export
Emax=3
#' @export
PARAM_PK_EMAX=4
#' @export
PkEmax=4
#' @export
PARAM_PK_INDIRECT=5
#' @export
PkIndirect=5
#' @export
PARAM_LINEAR=6
#' @export
Linear=6
#' @export
Blank=7
#' @export
PhoenixModel=8

ModelTypeNames=c("PK_LINEAR","PK","EMAX","PK_EMAX","PK_INDIRECT","LINEAR","BLANK","PHOENIX")


#' @export
LIMITED_STIM=1
#' @export
LimitedStimulation=1
#' @export
INFINITE_STIM=2
#' @export
InfiniteStimulation=2
#' @export
LIMITED_INHIB=3
#' @export
LimitedInhibition=3
#' @export
INVERSE_INHIB=4
#' @export
InverseInhibition=4
#' @export
LINEAR_STIM=5
#' @export
LinearStimulation=5
#' @export
LOG_LINEAR_STIM=6
#' @export
LogLinearStimulation=6


#' @export
LinearAlpha=1
#' @export
LinearBeta=2
#' @export
LinearGamma=3


#' NlmeModelType
#'
#' Class represents an NLME/PML model type
#'
#' @param modelType
#'
#' @examples
#'       NlmeModelType(PK)
#'
#' @export NlmeModelType
#'
NlmeModelType = setClass("NlmeModelType",representation(
                                      modelType="numeric"))


setMethod("initialize","NlmeModelType",
    function(.Object,
              modelType=PARAM_PK){
         if ( modelType < PARAM_PK_LINEAR || modelType > PhoenixModel ){
             print(paste("modelType",modelType," is not supported!"))
             modelType = PARAM_PK
         }
        .Object@modelType=modelType
        .Object
    })
assign("NlmeModelType",NlmeModelType,envir=.GlobalEnv)

#' @export
PARAM_MICRO=1
#' @export
Micro=1
#' @export
PARAM_CLEARANCE=2
#' @export
Clearance=2
#' @export
PARAM_MACRO=3
#' @export
Macro=3
#' @export
PARAM_MACRO1=4
#' @export
Macro1=4

ModelParametrizationNames=c("Micro","Clearance","Macro","Macro1")

#' NlmeModelParameterization
#'
#' Class represents an NLME/PML model parameterization
#'
#' @param paramType    One of Micro|Clearance|Macro|Macro1
#'
#' @examples
#'       NlmeModelParameterization(PARAM_CLEARANCE)
#'
#' @export NlmeModelParameterization
#'
NlmeModelParameterization= setClass("NlmeModelParameterization",representation(
                                      paramType="numeric"))


setMethod("initialize","NlmeModelParameterization",
    function(.Object,
              paramType=PARAM_CLEARANCE){
         if ( paramType < PARAM_MICRO || paramType > PARAM_MACRO1 ){
             print(paste("paramType",paramType," is not supported!"))
             paramType = PARAM_CLEARANCE
         }
        .Object@paramType=paramType
        .Object
    })
assign("NlmeModelParameterization",NlmeModelParameterization,envir=.GlobalEnv)

#' @export
PARAM_INTRAVENOUS=1
#' @export
PARAM_EXTRAVASCULAR=2
#' @export
Intravenous=1
#' @export
Extravascular=2

ModelAbsorptionNames=c("Intravenous", "Extravascular")

#' NlmeModelAbsorption
#'
#' Class represents an NLME/PML model absorption
#'
#' @param absorpType   Intravenous|Extravascular
#'
#' @examples
#'       NlmeModelAbsorption(PARAM_EXTRAVASCULAR)
#'
#' @export NlmeModelAbsorption
#'
NlmeModelAbsorption= setClass("NlmeModelAbsorption",representation(
                                      absorpType="numeric"))


setMethod("initialize","NlmeModelAbsorption",
    function(.Object,
              absorpType=PARAM_EXTRAVASCULAR){
         if(absorpType<PARAM_INTRAVENOUS||absorpType>PARAM_EXTRAVASCULAR ){
             print(paste("absorpType",absorpType," is not supported!"))
             absorpType = PARAM_EXTRAVASCULAR
         }
        .Object@absorpType=absorpType
        .Object
    })
assign("NlmeModelAbsorption",NlmeModelAbsorption,envir=.GlobalEnv)

#'
#' @export
#'
covariateNames <-function(model)
{
    names=c()
    covariateList=attr(model,"covariateList")
    for ( c in covariateList ) {
        name=attr(c,"name")
        names=c(names,name)
    }
    return(names)
}
assign("covariateNames",covariateNames,envir=.GlobalEnv)


#'
#' @export
#'
observationNames <-function(model)
{
    names=c()
    if ( model@isObjectModel ) {
      for ( o in model@objects ) {
        if ( is( o , "ObservationCompartment") ) {
          names=c(names,o@blockName)
#          if ( o@residualEffect@isBQL )
#              names = c(names,paste0(o@blockName,"BQL"))
        }
      }

    }else {
        if ( model@isTextual == FALSE ) {
        errorModel=attr(model,"errorModel")
        effectsList=attr(errorModel,"effectsList")
        for ( c in effectsList ) {
            name=attr(c,"observeName")
            names=c(names,name)
#            if ( c@isBQL )
#              names = c(names,paste0(c@observeName,"BQL"))
        }
        } else {
          statements=model@statements
          names=c()
          observeKeywords=c("multi\\(","observe\\(","LL\\(","count\\(",
                            "ordinal\\(", "event\\(")

          for ( l in statements ) {
            exist = any(sapply(observeKeywords, grepl, l))
            if ( exist == TRUE ) {
              name=unlist(strsplit(l,split="[(,=,,]"))[2]
              name=substr(name,1,1)
              names=c(names,name)
            }
          }

        }
    }
    return(names)
}
assign("observationNames",observationNames,envir=.GlobalEnv)

#'
#' @export
#'
observationExtraNames <-function(model)
{
    names=c()
    if ( model@isObjectModel ) {
      for ( o in model@objects ) {
#        if ( is( o , "ObservationCompartment") ) {
#          if ( o@residualEffect@isBQL )
#              names = c(names,paste0(o@blockName,"BQL"))
#        }
      }

    }else {
        errorModel=attr(model,"errorModel")
        effectsList=attr(errorModel,"effectsList")
        for ( c in effectsList ) {
            if ( c@isBQL )
              names = c(names,paste0(c@observeName,"BQL"))
        }
    }
    return(names)
}
assign("observationExtraNames",observationExtraNames,envir=.GlobalEnv)

#'
#' @export
#'
doseNames <-function(model)
{
    names=c()
    if ( model@isObjectModel ) {
      for ( o in model@objects ) {
        if ( is( o , "NlmeCompartment") ) {
          if ( o@numDosePoints > 0 )
          for ( indx in 1:o@numDosePoints ) {
            names=c(names,o@aName)
          }

        }
      }

    }else {
        doseList=attr(model,"dosePoints")
        if ( length(doseList) == 0 )
            names=c("C")
        else {
            for ( c in doseList ) {
                name=c
                names=c(names,name)
            }
        }
    }
    return(names)
}
assign("doseNames",doseNames,envir=.GlobalEnv)

#' extraDoseNames
#'
#' @export
#'
extraDoseNames <-function(model)
{
    names=c()
    doseList=attr(model,"extraDoses")
    if ( length(doseList) > 0 ) {
        for ( c in doseList ) {
          if ( c@doseType == SteadyStateDose )
            name="SteadyState"
          else
            name="ADDL"
 #           name=c@name
          names=c(names,name)
        }
    }
    if ( model@pkModelAttrs@infusionAllowed ) {
        dl=attr(model,"dosePoints")
        for ( c in dl ) {
            if ( model@pkModelAttrs@isDuration )
                name=paste0(c,"_Duration")
            else
                name=paste0(c,"_Rate")
            names = c(names,name)
        }
    }
    return(names)
}
assign("extraDoseNames",extraDoseNames,envir=.GlobalEnv)

#' extraDoseLines
#'
#' @export
#'
extraDoseLines <-function(model)
{
    lines=c()
    doseList=attr(model,"extraDoses")
    colMap=model@columnMapping@mapping
    if ( length(doseList) > 0 ) {
        for ( c in doseList ) {
            name=c@name
            doseType=c@doseType
            if ( doseType == SteadyStateDose ) {
                cmd = "ss"
                colName=lookupColname(colMap,"SteadyState")
            } else {
                cmd = "addl"
                colName=lookupColname(colMap,"ADDL")
            }

            line = paste0(cmd,"(\"",colName,"\", ")

            for ( d in c@doses ) {
                if ( d@amount@type == ColumnType ){
                    amount = paste0("\"",d@amount@column,"\"")
                } else{
                    amount=d@amount@value
                }
                if ( d@rate@type == ColumnType ){
                    rate = paste0("\"",d@rate@column,"\"")
                }else{
                    rate=d@rate@value
                }
                if ( d@deltaTime@type == ColumnType ){
                  deltaTime = paste0("\"",d@deltaTime@column,"\"")
                } else{
                  deltaTime=d@deltaTime@value
                }
                if ( d@isSecondDose )
                  second="2"
                else
                  second=""
                if ( d@type == BolusDose ) {
                  if ( doseType == SteadyStateDose )
                    line= paste0(line, amount, " bolus",second,"(",name ,") ", deltaTime, " dt ")
                  else
                    line= paste0(line, deltaTime, " dt ", amount," bolus",second,"(",name ,") ")
                } else {
                  if ( doseType == SteadyStateDose )
                    line= paste0(line, amount, " ", rate," inf",second,"(", name,") ", deltaTime, " dt")
                  else
                    line= paste0(line, amount, " ", rate," inf",second,"(", name,") ", deltaTime, " dt")
                    line= paste0(line, deltaTime, " dt ", amount, " ", rate," inf",second,"(",name ,") ")
                }
            }
            line=paste0(line,")")
            lines=c(lines,line)
        }
    }
    return(lines)
}
assign("extraDoseLines",extraDoseLines,envir=.GlobalEnv)

#'
#' @export
#'
structuralParameterNames <-function(model)
{
    sps = attr(model,"structuralParams")
    names=c()
    for ( s in sps ) {
        name=attr(s,"name")
        names=c(names,name)
    }
    return(names)
}
assign("structuralParameterNames",structuralParameterNames,envir=.GlobalEnv)


#'
#' @export
#'
fixedParameterNames <-function(model)
{
    sps = attr(model,"structuralParams")
    names=c()
    for ( s in sps ) {
        name=attr(s,"name")
        names=c(names,paste0("tv",name))
    }
    return(names)
}
assign("fixedParameterNames",fixedParameterNames,envir=.GlobalEnv)


#'
#' @export
#'
randParameterNames <-function(model)
{
    sps = attr(model,"structuralParams")
    names=c()
    for ( s in sps ) {
        name=attr(s,"name")
        names=c(names,paste0("n",name))
    }
    return(names)
}
assign("randParameterNames",randParameterNames,envir=.GlobalEnv)



#'
#' @export
#'
residualEffectNames <-function(model)
{
    names=c()
    errorModel=attr(model,"errorModel")
    numEffects=attr(errorModel,"numberOfEffects")
    effects=attr(errorModel,"effectsList")
    for ( e in effects ) {
        name=attr(e,"effectName")
        names=c(names,name)
    }
    return(names)
}

assign("covariateNames",covariateNames,envir=.GlobalEnv)

#' NlmePkParameters
#'
#' Class represents an NLME/PML PK model parameters
#'
#' @param parameterization    Taken from NlmeModelParameterization
#' @param absorption          Taken from NlmeModelAbsorption
#' @param numCompartments     Number of compartments
#' @param isTlag              Does dose have a time lag
#' @param hasEliminationComp  Is there data available for an elimination compartment
#' @param isFractionExcreted  Does compartment include a fraction excreted parameter
#' @param isMichaelisMenten   Is this a Michaelis-Menten model
#' @param isSaturating        Is the elimination rate equal to the absorption rate
#' @param infusionAllowed     Is infusion allowed
#' @param isDuration          Is duration of infusion measured (TRUE) or rate (FALSE)
#' @param isSequential        Is model part of a PK/PD model that is being fitted sequentially
#' @param isClosedForm        Is model closed-form algebraic (TRUE) or differential equation (FALSE)
#'
#' @examples
#'       NlmePkParameters(ERR_ADDITIVE)
#'
#' @export NlmePkParameters
#'
NlmePkParameters= setClass("NlmePkParameters",representation(
                              parameterization="NlmeModelParameterization",
                              absorption="NlmeModelAbsorption",
                              numCompartments="numeric",
                              isTlag="logical",
                              hasEliminationComp="logical",
                              isFractionExcreted="logical",
                              isMichaelisMenten="logical",
                              isSaturating="logical",
                              infusionAllowed="logical",
                              isDuration="logical",
                              isSequential="logical",
                              isPkFrozen="logical",
                              isClosedForm="logical"))


setMethod("initialize","NlmePkParameters",
    function(.Object,
              parameterization=NlmeModelParameterization(),
              absorption=NlmeModelAbsorption(),
              numCompartments=1,
              isTlag=FALSE,
              hasEliminationComp=FALSE,
              isFractionExcreted=FALSE,
              isMichaelisMenten=FALSE,
              isSaturating=FALSE,
              infusionAllowed=FALSE,
              isDuration=FALSE,
              isClosedForm=TRUE,
              isSequential=FALSE,
              isPkFrozen=FALSE){

        .Object@parameterization=parameterization
        .Object@absorption=absorption
        .Object@numCompartments=numCompartments
        .Object@hasEliminationComp=hasEliminationComp
        .Object@isFractionExcreted=isFractionExcreted
        .Object@isSaturating=isSaturating
        .Object@infusionAllowed=infusionAllowed
        .Object@isDuration=isDuration
        .Object@isSequential=isSequential
        .Object@isPkFrozen=isPkFrozen
        .Object@isTlag=isTlag
        .Object@isClosedForm=isClosedForm
        .Object@isMichaelisMenten=isMichaelisMenten
        if( .Object@isSaturating )
          .Object@isClosedForm=FALSE
        .Object
    })
assign("NlmePkParameters",NlmePkParameters,envir=.GlobalEnv)

#' NlmeEmaxParameters
#'
#' Class represents an NLME/PML Emax model parameters
#'
#' @param checkBaseline    Model has a baseline response
#' @param checkFractional  Model is fractional
#' @param checkInhibitory  Model is inhibitory
#' @param checkSigmoid     Model is sigmoidal
#'
#' @examples
#'      NlmeEmaxParameters(checkBaseline=TRUE)
#'
#' @export NlmeEmaxParameters
#'
NlmeEmaxParameters= setClass("NlmeEmaxParameters",representation(
                                      checkBaseline="logical",
                                      checkFractional="logical",
                                      checkInhibitory="logical",
                                      checkSigmoid="logical",
                                      frozen="logical"))


setMethod("initialize","NlmeEmaxParameters",
    function(.Object,
              checkBaseline=FALSE,
              checkFractional=FALSE,
              checkInhibitory=FALSE,
              checkSigmoid=FALSE,
             frozen=FALSE){

        .Object@checkBaseline=checkBaseline
        .Object@checkFractional=checkFractional
        .Object@checkInhibitory=checkInhibitory
        .Object@checkSigmoid=checkSigmoid
        .Object@frozen=frozen
        .Object
    })

assign("NlmeEmaxParameters",NlmeEmaxParameters,envir=.GlobalEnv)

#' @export
LINEAR_ALPHA_TYPE=1
#' @export
LINEAR_BETA_TYPE=2
#' @export
LINEAR_GAMMA_TYPE=3

ModelLinearNames=c("E = Alpha ","E = Alpha + Beta*C ","E = Alpha + Beta*C +Gam*C^2")
#'
#' NlmePmlModelInfo Model information
#'
#' @export
NlmePmlModelInfo= setClass("NlmePmlModelInfo",representation(
                           modelName = "character",
                           workingDir = "character"))


setMethod("initialize","NlmePmlModelInfo",
    function(.Object,
             modelName="",
             workingDir ="" ){

    if ( workingDir == "" ) {
        workingDir = getwd()
        if ( modelName == "" ) {
            modelName=basename(tempfile(pattern="Model",tmpdir=workingDir))
            workingDir=paste(workingDir,modelName,sep="/")
        } else {
            workingDir=paste(workingDir,modelName,sep="/")
        }
    }
    if ( !dir.exists(workingDir ) ) {
#        dir.create(workingDir)
    }
    if ( modelName == "" )
        modelName=basename(tempfile(pattern="Model",tmpdir=workingDir))
    print(workingDir)
    print(modelName)
    .Object@workingDir=workingDir
    .Object@modelName=modelName
    .Object
})
assign("NlmePmlModelInfo",NlmePmlModelInfo,envir=.GlobalEnv)
#' NlmeIndirectParameters
#'
#' Class represents an NLME/PML Indirect PD model parameters
#'
#' @param type                   Indirect model type
#' @param hasEffectsCompartment  Is there data available for an effects compartment
#' @param isBuildup              Is the response formation (TRUE) or
#'                                    degradation (FALSE) concentration dependent
#' @param isExponent             Is there an exponent in the effect statement
#' @param frozen                 Freeze standard deviation to prevent estimation
#'                                    of the PK part of the model
#'
#' @examples
#'      NlmeIndirectParameters(ERR_ADDITIVE)
#'
#' @export NlmeIndirectParameters
#'
NlmeIndirectParameters= setClass("NlmeIndirectParameters",representation(
                              type="numeric",
                              hasEffectsCompartment="logical",
                              isBuildup="logical",
                              isExponent="logical",
                              frozen="logical"))


setMethod("initialize","NlmeIndirectParameters",
    function(.Object,
              type=LIMITED_STIM,
             hasEffectsCompartment = FALSE,
              isBuildup=TRUE,
              isExponent=FALSE,
              frozen=FALSE){
        .Object@type=type
        .Object@hasEffectsCompartment= hasEffectsCompartment
        .Object@isBuildup=isBuildup
        .Object@isExponent=isExponent
        .Object@frozen=frozen
        .Object
    })
assign("NlmeIndirectParameters",NlmeIndirectParameters,envir=.GlobalEnv)





#' NlmePmlModel
#'
#' Class represents an NLME/PML model
#'
#’ @param isPopulation              Is this a population model (TRUE) or
#'                                       individual (FALSE)
#’ @param modelType                 Taken from NlmeModelType
#’ @param isTimeBased               Is model time-based
#’ @param linearModelType           Type of linear model
#' @param isLinearFrozen            Is linear model frozen
#’ @param pkModelAttrs              Taken from NlmePkParameters
#’ @param indirectModelAttrs        Taken from NlmeIndirectParameters
#’ @param emaxModelAttrs            Taken from NlmeEmaxParameters
#’ @param hasEffectsCompartment     Is there data available for an effects compartment
#’ @param errorModel                Taken from NlmeErrorModel
#’ @param structuralParams          List of structural parameters
#’ @param outputParams              List of output parameters
#’ @param diffEquations             List of differential equations
#’ @param statements                List of PML statements
#’ @param dosePoints                List of dosepoints
#’ @param covariateList             List of covariates
#’ @param columnMapping             Taken from NlmeColumnMapping
#’ @param doseMapping               Taken from NlmeDoseMapping
#’ @param paramsMapping             Taken from NlmeParamsMapping
#’ @param randParamsMapping         Taken from NlmeRandParamsMapping
#’ @param inputData                 Input data source
#’ @param doseData                  Dose data source
#’ @param fixedParamData            Fixed effect parameter data source
#’ @param randParamData             Random effect parameter data source
#’ @param isTextual                 Is model textual (TRUE) or graphical (FALSE)
#’ @param pmloutput                 List of PML output to generate
#’ @param modelInfo                 Taken from NlmePmlModelInfo
#’ @param objects                   Model objects
#’ @param objectsNeedRegenerating   Regenerate objects
#’ @param randomEffectsStatements   Custom random effects statements
#' @param userDefinedExtraDefs      Custom definition for extra column
#'                                       and table generation
#'
#' @examples
#'       NlmePmlModel()
#'
#' @export NlmePmlModel
#'
NlmePmlModel= setClass("NlmePmlModel",representation(
                                      isPopulation="logical",
                                      modelType="NlmeModelType",
                                      isTimeBased="logical",
                                      linearModelType="numeric",
                                      isLinearFrozen="logical",
                                      pkModelAttrs="NlmePkParameters",
                                      indirectModelAttrs="NlmeIndirectParameters",
                                      emaxModelAttrs="NlmeEmaxParameters",
                                      hasEffectsCompartment="logical",
                                      errorModel="NlmeErrorModel",
                                      structuralParams="list",
                                      outputParams="list",
                                      diffEquations="list",
                                      statements="list",
                                      dosePoints="list",
                                      covariateList="list",
                                      columnMapping="NlmeColumnMapping",
                                      doseMapping="NlmeDoseMapping",
                                      paramsMapping="NlmeParamsMapping",
                                      randParamsMapping="NlmeRandParamsMapping",
                                      inputData="ANY",
                                      doseData="ANY",
                                      extraDoses="list",
                                      resetInfo="ANY",
                                      hasResetInfo="logical",
                                      fixedParamData="ANY",
                                      randParamData="ANY",
                                      isTextual="logical",
                                      pmloutput="list",
                                      randomEffectsStatements="list",
                                      modelInfo="NlmePmlModelInfo",
                                      objects="ANY",
                                      objectsNeedRegenerating="logical",
                                      nextKey="numeric",
                                      isObjectModel="logical",
                                      userDefinedExtraDefs="list",
                                      secondaryParameters="list",
                                      dataset="NlmeDataset",
                                      engine="NlmeEngineExtraParams",
                                      host="NlmeParallelHost",
                                      randomValues="NlmeRandomEffectValues",
                                      randomBlocks="list",
                                      randomValuesInitialized="logical"))


setMethod("initialize","NlmePmlModel",
    function(.Object,
              isPopulation=TRUE,
              isTextual=FALSE,
              modelType=PARAM_PK,
              isTimeBased=TRUE,
              linearModelType=LINEAR_ALPHA_TYPE,
              isLinearFrozen=FALSE,

              pkModelAttrs=NlmePkParameters(),
              indirectModelAttrs=NlmeIndirectParameters(),
              emaxModelAttrs=NlmeEmaxParameters(),
              hasEffectsCompartment=FALSE,
              errorModel=NlmeErrorModel() ,
              modelInfo=NlmePmlModelInfo(),
              dataset = NULL,
              engine = NlmeEngineExtraParams(),
              host = NlmeParallelHost(),
              randomValues= NULL,
              randomBlocks = NULL
              ){

        .Object@isPopulation=isPopulation
        .Object@isTextual=isTextual
        mType = NlmeModelType(modelType)
        .Object@modelType=mType
        .Object@isTimeBased=isTimeBased
        .Object@linearModelType=linearModelType
        .Object@isLinearFrozen = isLinearFrozen
        .Object@pkModelAttrs=pkModelAttrs
        .Object@indirectModelAttrs=indirectModelAttrs
        .Object@emaxModelAttrs=emaxModelAttrs
        .Object@hasEffectsCompartment = hasEffectsCompartment
        .Object@errorModel=errorModel
        .Object@modelInfo=modelInfo
        .Object@hasResetInfo=FALSE
        .Object@objects=list()
        .Object@extraDoses=list()
        .Object@randomEffectsStatements=list()
        .Object@objectsNeedRegenerating=TRUE
        .Object@nextKey = 1
        .Object@isObjectModel=FALSE
        .Object@userDefinedExtraDefs=list()
        .Object@secondaryParameters=list()
        if ( is.null(dataset) )
            dataset=NlmeDataset(.Object@modelInfo@workingDir)
        .Object@dataset = dataset
        .Object@engine = engine
        .Object@host = host
        if ( ! is.null(randomValues ))
            .Object@randomValues = randomValues
        if ( ! is.null(randomBlocks ))
          .Object@randomBlocks = randomBlocks
        .Object@randomValuesInitialized=FALSE
        .Object
    })
assign("NlmePmlModel",NlmePmlModel,envir=.GlobalEnv)

#'
#' @export
#'
print.NlmePmlModel <-function(x, ...)
{
    modelType=attr(attr(x,"modelType"),"modelType")
    modelTypeName=ModelTypeNames[modelType]
    print(paste("Is population     : ",attr(x,"isPopulation")))
    print(paste("Model Type        : ",modelTypeName))
    if ( x@isObjectModel == FALSE ) {
    if (length(grep( "PK", modelTypeName  ))  ){
        paramType=x@pkModelAttrs@parameterization@paramType
        absorpType=x@pkModelAttrs@absorption@absorpType
        paramName=ModelParametrizationNames[paramType]
        absorpName=ModelAbsorptionNames[absorpType]
        print(paste("Parametrization   : ",paramName))
        print(paste("Absorption        : ", absorpName))
        print(paste("Num Compartments  : ",x@pkModelAttrs@numCompartments))
        print(paste("Dose Tlag?        : ",attr(attr(x,"pkModelAttrs"),"isTlag")))
        flag=attr(attr(x,"pkModelAttrs"),"hasEliminationComp")
        print(paste("Elimination Comp ?: ",flag))
        if ( flag == TRUE )
            print(paste("Fraction Excreted : ",attr(attr(x,"pkModelAttrs"),"isFractionExcreted")))
        flag=attr(attr(x,"pkModelAttrs"),"infusionAllowed")
        print(paste("Infusion Allowed ?: ",flag))
        if ( flag == TRUE )
            print(paste("Duration          : ",attr(attr(x,"pkModelAttrs"),"duration")))
        print(paste("Sequential        : ",attr(attr(x,"pkModelAttrs"),"isSequential")))
        print("---------------------------------------")
    } 
    if (length(grep( "EMAX", modelTypeName  ))  ){
        print(paste("Check Baseline    : ",attr(attr(x,"emaxModelAttrs"),"checkBaseline")))
        print(paste("Check Inhibitory  : ",attr(attr(x,"emaxModelAttrs"),"checkInhibitory")))
        print(paste("Check Sigmoid     : ",attr(attr(x,"emaxModelAttrs"),"checkSigmoid")))
        print("---------------------------------------")
    } 
    if (length(grep( "LINEAR", modelTypeName  ))  ){
        name=ModelLinearNames[attr(x,"linearModelType")]
        print(paste("Linear Model Type  : ",name))
        print("---------------------------------------")
    }
    }
    else
        print(ModelTypeName)

    statements=attr(x,"statements")
    structuralParams=attr(x,"structuralParams")
    dosePoints=attr(x,"dosePoints")
    outputParams=attr(x,"outputParams")
    covariates=attr(x,"covariateList")
    listModelStatements(x)
    print("---------------------------------------")

    structuralParameterNames(x)
    print("---------------------------------------")

    if ( x@isObjectModel == FALSE ) {
    errorModel=attr(x,"errorModel")
    print(errorModel)
    print("---------------------------------------")
    }
}
#'
#' @export
#'
setGeneric(name="createIndirectStructuralParameters",
           def=function(.Object)
           {
               standardGeneric("createIndirectStructuralParameters")
           })

setMethod(f="createIndirectStructuralParameters",
    signature="NlmePmlModel",
    definition=function(.Object){
        structuralParams=attr(.Object,"structuralParams")
        dosePoints=attr(.Object,"dosePoints")
        dosePoints=list()
        outputParams=attr(.Object,"outputParams")
        diffEquations=attr(.Object,"diffEquations")
        attrs=attr(.Object,"indirectModelAttrs")

        type   = attrs@type
        hasEffectsCompartment   = .Object@hasEffectsCompartment
        isBuildup   = attrs@isBuildup
        isExponent   = attrs@isExponent
        frozen   = attrs@frozen



        sCOutput = "C"
        bFreeze = FALSE
        if ( frozen )
          bFreeze = TRUE
        if ( .Object@isPopulation == FALSE )
            hasRandomEffect = FALSE
        else
            hasRandomEffect = !bFreeze
        if ( hasEffectsCompartment ) {

            structuralParams=addStructuralParameter(structuralParams,"Ke0",
                                            hasRandomEffect=hasRandomEffect ,
                                            isFrozen = bFreeze)
        }
        structuralParams=addStructuralParameter(structuralParams,"Kin",
                                            hasRandomEffect=hasRandomEffect ,
                                            isFrozen = bFreeze)
        structuralParams=addStructuralParameter(structuralParams,"Kout",
                                            hasRandomEffect=hasRandomEffect ,
                                            isFrozen = bFreeze)

        sMax = ""
        s50 = ""

        if ( type == LIMITED_STIM  ) {
            sMax = "Emax"
            s50 = "EC50"
        }
        if ( type == INFINITE_STIM  ) {
            sMax = ""
            s50 = "EC50"
        }
        if ( type == LIMITED_INHIB  ) {
            sMax = "Imax"
            s50 = "IC50"
        }
        if ( type == INVERSE_INHIB  ) {
            sMax = "Imax"
            s50 = "IC50"
        }
        if ( type == LINEAR_STIM  ) {
            sMax = ""
            s50 = "s"
        }
        if ( type == LOG_LINEAR_STIM  ) {
            sMax = ""
            s50 = "s"
        }
        structuralParams=addStructuralParameter(structuralParams,sMax,
                                            hasRandomEffect=hasRandomEffect ,
                                            isFrozen = bFreeze)
        structuralParams=addStructuralParameter(structuralParams,s50,
                                            hasRandomEffect=hasRandomEffect ,
                                            isFrozen = bFreeze)
        if ( isExponent )
            structuralParams=addStructuralParameter(structuralParams,"gam",
                                            hasRandomEffect=hasRandomEffect ,
                                            isFrozen = bFreeze)

        if ( bFreeze == FALSE )
            outputParams=c(outputParams,"E")
        .Object@structuralParams=structuralParams
        .Object@outputParams=outputParams
        return(.Object)
    })

assign("createIndirectStructuralParameters",createIndirectStructuralParameters,envir=.GlobalEnv)


#'
#' @export
#'
setGeneric(name="modelColumnMapping",
           def=function(.Object)
           {
               standardGeneric("modelColumnMapping")
           })

#'
#' Lists mapping between model variables and data columns
#'
#' @examples
#'       modelColumnMapping(model)=c(id="Subject", CObs="Conc",A1="Amount")j
#'
#' @export
#'
setMethod(f="modelColumnMapping",
    signature="NlmePmlModel",
    definition=function(.Object){
        map=attr(.Object,"columnMapping")
        return(map)
    })
assign("modelColumnMapping",modelColumnMapping,envir=.GlobalEnv)

#'
#' Sets mapping between model variables and data columns
#'
#' @param   List of variable/column pairs
#'
#' @examples
#'       modelColumnMapping(model)=c(id="Subject", CObs="Conc",A1="Amount")j
#'
#' @export
#'
setGeneric(name="modelColumnMapping<-",
           def=function(.Object,value)
           {
               standardGeneric("modelColumnMapping<-")
           })

setMethod(f="modelColumnMapping<-",
    signature="NlmePmlModel",
    definition=function(.Object,value){
    names=names(value)
    for ( i in 1:length(names)){
        n=names[i]
        v=value[[n]]
        mappedColumn(.Object,n)=v
    }
    return(.Object)
    })


#'
#' @export
#'
setGeneric(name="modelDoseMapping",
           def=function(.Object)
           {
             standardGeneric("modelDoseMapping")
           })

#'
#' Lists mapping between model variables and data columns
#'
#' @examples
#'       modelColumnMapping(model)=c(id="Subject", CObs="Conc",A1="Amount")j
#'
#' @export
#'
setMethod(f="modelDoseMapping",
          signature="NlmePmlModel",
          definition=function(.Object){
            map=attr(.Object,"doseMapping")
            return(map)
          })
assign("modelDoseMapping",modelDoseMapping,envir=.GlobalEnv)

#'
#' Sets mapping between model variables and data columns
#'
#' @param      List of variable/column pairs
#'
#' @examples
#'       modelColumnMapping(model)=c(id="Subject", CObs="Conc",A1="Amount")j
#'
#' @export
#'
setGeneric(name="modelDoseMapping<-",
           def=function(.Object,value)
           {
             standardGeneric("modelDoseMapping<-")
           })

setMethod(f="modelDoseMapping<-",
          signature="NlmePmlModel",
          definition=function(.Object,value){
            names=names(value)
            for ( i in 1:length(names)){
              n=names[i]
              v=value[[n]]
              mappedDose(.Object,n)=v
            }
            return(.Object)
          })


#'
#' modelParamsMapping
#'
#' Lists mapping between model fixed effects and input columns
#'
#' @examples
#'       modelParamsMapping(model)
#'
#' @export
#'
setGeneric(name="modelParamsMapping",
           def=function(.Object)
           {
             standardGeneric("modelParamsMapping")
           })


#'
#' @export
#'
setMethod(f="modelParamsMapping",
          signature="NlmePmlModel",
          definition=function(.Object){
            map=attr(.Object,"paramsMapping")
            return(map)
          })
assign("modelParamsMapping",modelParamsMapping,envir=.GlobalEnv)


#'
#' modelRandParamsMapping
#'
#' Lists mapping between model random effects and input columns
#'
#' @examples
#'       modelRandParamsMapping(model)
#'
#' @export
#'
setGeneric(name="modelRandParamsMapping",
           def=function(.Object)
           {
             standardGeneric("modelRandParamsMapping")
           })

#'
#' @export
#'
setMethod(f="modelRandParamsMapping",
          signature="NlmePmlModel",
          definition=function(.Object){
            map=attr(.Object,"randParamsMapping")
            return(map)
          })
assign("modelRandParamsMapping",modelRandParamsMapping,envir=.GlobalEnv)


#'
#' @export
#'
setGeneric(name="modelRandParamsMapping<-",
           def=function(.Object,value)
           {
             standardGeneric("modelRandParamsMapping<-")
           })

setMethod(f="modelRandParamsMapping<-",
          signature="NlmePmlModel",
          definition=function(.Object,value){
            names=names(value)
            for ( i in 1:length(names)){
              n=names[i]
              v=value[[n]]
              mappedParams(.Object,n)=v
            }
            return(.Object)
          })



setGeneric(name="initColMapping<-",
           def=function(.Object,value)
           {
               standardGeneric("initColMapping<-")
           })

#' initColMapping
#'
#' Tries to map the input dataset columns to model variables
#'
#' @param model    A PK/PD model with the variables to be mapping
#' @param dataset  Input dataset
#'
#' @method         Creates a mapping between input columns and model
#'                 variables, tries to do string matching between the two
#'
#' @export
#'
setMethod(f="initColMapping<-",
    signature="NlmePmlModel",
    definition=function(.Object,value){
        map=NlmeColumnMapping(.Object,value)
        attr(.Object,"columnMapping")=map
        attr(.Object,"inputData")=value
        return(.Object)
    })

#'
#' initDoseColMapping
#'
#' Maps columns in the dosing spreadsheet to model dose variables by string matching
#'
#' It is used when dose values are supplied in a separate input file
#'
#' @param model             PK/PD model
#' @param doseSpreadsheet   Data frame containing the dose information
#'
#' @examples
#'     doseInput=read.csv("PKPD_Data.csv")
#'     initDoseColMapping(pkpdmodel)=doseInput
#'
#'     print(modelDoseMapping(pkpdmodel))
#'     print(doseNames(pkpdmodel))
#'     modelDoseMapping(pkpdmodel)=c(Aa="Dose")
#'
#'     Dose  values and  mappings are supplied in cols2.txt and
#'     data2.txt files to the engine
#'
#' @export
#'
setGeneric(name="initDoseColMapping<-",
           def=function(.Object,value)
           {
             standardGeneric("initDoseColMapping<-")
           })

#'
#' @export
#'
setMethod(f="initDoseColMapping<-",
          signature="NlmePmlModel",
          definition=function(.Object,value){
            map=NlmeDoseMapping(.Object,value)
            attr(.Object,"doseMapping")=map
            attr(.Object,"doseData")=value
            return(.Object)
          })



#'
#' initParamsMapping
#'
#' Maps columns in the fixed effects dataframe  to model  variables
#' by string matching
#'
#' It is used when initial values for fixed effects are supplied from
#' a previous fitting run.
#'
#' @param model           PK/PD model
#' @param paramsInput     Data frame containing fixed effects information
#'
#' @examples
#'     paramsInput=read.csv("params.csv")
#'     head(paramsInput)
#'       Parameter Initial Lower Upper
#'       tvKa        3       1     18
#'       tvV         4      -9     44
#'       tvCl       NA      NA     NA
#'
#'     initParamsMapping(model)=paramsInput
#'
#'     modelParamsMapping(model)
#'     dataset = updateDatasetParams(model,dataset)
#'
#'     Fixed effect values and column mappings are supplied in cols3.txt and
#'     data3.txt files to the engine
#'
#' @export
#'
setGeneric(name="initParamsMapping<-",
           def=function(.Object,value)
           {
             standardGeneric("initParamsMapping<-")
           })

#'
#' @export
#'
setMethod(f="initParamsMapping<-",
          signature="NlmePmlModel",
          definition=function(.Object,value){
            map=NlmeParamsMapping(.Object,value)
            attr(.Object,"paramsMapping")=map
            attr(.Object,"fixedParamData")=value
            return(.Object)
          })

#'
#' initRandParamsMapping
#'
#' Maps columns in the random effects dataframe  to model  variables
#' by string matching
#'
#' It is used when initial values for random effects are supplied from
#' a previous fitting run.
#'
#' @param model         PK/PD model
#' @param paramsInput   Data frame containing random effects information
#'
#' @examples
#'     paramsInput=read.csv("ranparams.csv")
#'     OR
#'     source("dmp.txt")
#'     paramsInput=dmp.txt$coefficients$random[[1]]
#'
#'     head(paramsInput)
#'
#'              nV        nCl
#'           -0.617345 -0.3034520
#'            0.263520  0.5110880
#'           -0.439680 -0.3502870
#'
#'     initRandParamsMapping(model)=paramsInput
#'
#'     modelRandParamsMapping(model)
#'     dataset = updateDatasetRandParams(model,dataset)
#'
#'     Fixed effect values and column mappings are supplied in cols4.txt and
#'     data4.txt files to the engine
#'
#' @export
#'
setGeneric(name="initRandParamsMapping<-",
           def=function(.Object,value)
           {
             standardGeneric("initRandParamsMapping<-")
           })

#'
#' @export
#'
setMethod(f="initRandParamsMapping<-",
          signature="NlmePmlModel",
          definition=function(.Object,value){
            map=NlmeRandParamsMapping(.Object,value)
            attr(.Object,"randParamsMapping")=map
            attr(.Object,"randParamData")=value
            return(.Object)
          })

#'
#' @export
#'
setGeneric(name="columnMappings<-",
           def=function(.Object,value)
           {
               standardGeneric("columnMappings<-")
           })

setMethod(f="columnMappings<-",
    signature="NlmePmlModel",
    definition=function(.Object,value){

        columnMapping=attr(attr(.Object,"columnMapping"),"columnMapping")
        map=attr(columnMapping,"mapping")

        if ( length(map ) > 0 )  {
            for ( i in 1:length(map)) {
                m=map[[i]]
                name = attr(m,"variableName")
                if( ! is.na(value[name]) ) {
                    attr(m,"columnName") = name
                }
                map[[i]]=m
             }
         }
        attr(columnMapping,"mapping") = map
        attr(.Object,"columnMapping") = columnMapping
        return(.Object)
    })





##'
##' @export
##'
##setGeneric(name="modelColumnMapping<-",
##           def=function(.Object,value)
##           {
##               standardGeneric("modelColumnMapping<-")
##           })
##
###'
###' @export
###'
#setMethod(f="modelColumnMapping<-",
##    signature="NlmePmlModel",
##    definition=function(.Object,value){
##        .Object@columnMapping = value
##        return(.Object)
##    })
##


#'
#' @export
#'
setGeneric(name="mappedColumn",
    def=function(.Object,varName)
    {
               standardGeneric("mappedColumn")
    })

#'
#' @export
#'
setMethod(f="mappedColumn",
    signature="NlmePmlModel",
    definition=function(.Object,varName){
        map=attr(.Object,"columnMapping")
        cm=attr(map,"mapping")
         if ( length(cm[[varName]]) != 0 )
                return(attr(cm[[varName]],"columnName"))
    })
assign("mappedColumn",mappedColumn,envir=.GlobalEnv)

#'
#' @export
#'
setGeneric(name="mappedColumn<-",
           def=function(.Object,varName,value)
           {
               standardGeneric("mappedColumn<-")
           })

#'
#' @export
#'
setMethod(f="mappedColumn<-",
    signature="NlmePmlModel",
    definition=function(.Object,varName,value){
        names=modelVariableNames(.Object)
         if ( ! is.na(match(varName,names) ) ) {
            map=attr(.Object,"columnMapping")
            cm=attr(map,"mapping")
            n = NlmeColumnMap(varName,value)
            cm[[varName]]=n
            map@mapping=cm
            .Object@columnMapping = map
        }
        return(.Object)
    })

#'
#' @export
#'
setGeneric(name="mappedDose",
           def=function(.Object,varName)
           {
             standardGeneric("mappedDose")
           })

#'
#' @export
#'
setMethod(f="mappedDose",
          signature="NlmePmlModel",
          definition=function(.Object,varName){
            map=attr(.Object,"doseMapping")
            cm=attr(map,"mapping")
            if ( length(cm[[varName]]) != 0 )
              return(attr(cm[[varName]],"columnName"))
          })
assign("mappedDose",mappedDose,envir=.GlobalEnv)

#'
#' @export
#'
setGeneric(name="mappedDose<-",
           def=function(.Object,varName,value)
           {
             standardGeneric("mappedDose<-")
           })

#'
#' @export
#'
setMethod(f="mappedDose<-",
          signature="NlmePmlModel",
          definition=function(.Object,varName,value){
            names=doseNames(.Object)
            if ( ! is.na(match(varName,names) ) ) {
              map=attr(.Object,"doseMapping")
              cm=attr(map,"mapping")
              n = NlmeColumnMap(varName,value)
              cm[[varName]]=n
              map@mapping=cm
              .Object@doseMapping = map
            }
            return(.Object)
          })
#'
#' @export
#'
setGeneric(name="mappedParams",
           def=function(.Object,varName)
           {
             standardGeneric("mappedParams")
           })

#'
#' @export
#'
setMethod(f="mappedParams",
          signature="NlmePmlModel",
          definition=function(.Object,varName){
            map=attr(.Object,"paramsMapping")
            cm=attr(map,"mapping")
            if ( length(cm[[varName]]) != 0 )
              return(attr(cm[[varName]],"columnName"))
          })
assign("mappedParams",mappedParams,envir=.GlobalEnv)

#'
#' @export
#'
setGeneric(name="mappedParams<-",
           def=function(.Object,varName,value)
           {
             standardGeneric("mappedParams<-")
           })

#'
#' @export
#'
setMethod(f="mappedParams<-",
          signature="NlmePmlModel",
          definition=function(.Object,varName,value){
            names=fixedParameterNames(.Object)
            if ( ! is.na(match(varName,names) ) ) {
              map=attr(.Object,"paramsMapping")
              cm=attr(map,"mapping")
              n = NlmeColumnMap(varName,value)
              cm[[varName]]=n
              map@mapping=cm
              .Object@paramsMapping = map
            }
            return(.Object)
          })

#' residualEffect
#'
#' Method for getting/setting residual effect attributes
#'
#' @param model        Model
#' @param effectName   Effect to lookup/set attributes for
#' @param              List of attributes to set
#'
#' @examples
#'     residualEffect(model,"C")
#'
#'     residualEffect(model,"C") = c(errorType=LogAdditive)
#'
#'     residualEffect(model,"E") = c(errorType=LogAdditive,
#'                                   isBQL=TRUE,
#'                                   frozen=TRUE)
#'
#'     residualEffect(model,"C") = c(errorType=Multiplicative,
#'                                   SD="0.094345")
#'
#'     residualEffect(model,"C") = c(errorType=Power,
#'                                   SD="0.094345",
#'                                   definition="3")
#'
#'     See ?NlmeResidualEffect help for all attributes for this object
#'
#'
#' @export residualEffect
#'
setGeneric(name="residualEffect",
           def=function(.Object,effectName)
           {
               standardGeneric("residualEffect")
           })

#'
#' @export
#'
setMethod(f="residualEffect",
    signature="NlmePmlModel",
    definition=function(.Object,effectName){

      effectsList=c()

      if ( is.null(.Object@objects) || length(.Object@objects )  == 0  ) {
        errorModel=attr(.Object,"errorModel")
        effectsList=attr(errorModel,"effectsList")
      } else {
        effectsList=c()
        for ( indx in 1:length(.Object@objects) ) {
          comp=.Object@objects[[indx]]
          if ( class(comp) == "ObservationContinuous" ) {
            if ( comp@residualEffect@effectName == effectName ) {
              obsCompIndx = indx
              effectsList=c(effectsList,comp@residualEffect)

            }
          }
        }

      }
        effect=NULL
        for ( e in effectsList ) {
            name=attr(e,"effectName")
            if ( name == effectName )
                effect= e
        }
        if ( ! is.null(effect) )
            print(effect)
    })
assign("residualEffect",residualEffect,envir=.GlobalEnv)

#'
#' @export
#'
setGeneric(name="residualEffect<-",
           def=function(.Object,effectName,value)
           {
               standardGeneric("residualEffect<-")
           })

#'
#' @export
#'
setMethod(f="residualEffect<-",
    signature="NlmePmlModel",
    definition=function(.Object,effectName,value){
        obsCompIndx= -1

        if ( is.null(.Object@objects) || length(.Object@objects )  == 0  ) {
            errorModel=attr(.Object,"errorModel")
            effectsList=attr(errorModel,"effectsList")
        } else {
          effectsList=c()
          for ( indx in 1:length(.Object@objects) ) {
            comp=.Object@objects[[indx]]
            if ( class(comp) == "ObservationContinuous" ) {
              if ( comp@residualEffect@effectName == effectName ) {
                obsCompIndx = indx
                effectsList=c(effectsList,comp@residualEffect)

              }
            }
          }

        }
        if ( length(effectsList ) > 0 )
        for ( indx in 1:length(effectsList) ) {
            effect = effectsList[[indx]]
            name=attr(effect,"effectName")
            if ( name == effectName )  {
                if ( ! is.na(value["effectName"]) )
                    effect@effectName= value[["effectName"]]
                if ( ! is.na(value["observeName"]) )
                    effect@observeName= value[["observeName"]]
                if ( ! is.na(value["epsilonName"]) )
                    effect@epsilonName= value[["epsilonName"]]
                if ( ! is.na(value["errorType"]) ) {
                    effect@errorType= as.integer(value[["errorType"]])
                    if ( effect@errorType == ERR_ADD_MULT &&
                         effect@definition == "" )
                        effect@definition=paste0(effect@effectName,"MultStdev")
                    if ( effect@errorType == ERR_MIX_RATIO &&
                         effect@definition == "" )
                        effect@definition=paste0(effect@effectName,"MixRatio")
                    if ( effect@errorType == ERR_POWER &&
                         effect@definition == "" )
                        effect@definition=paste0("0")
                }
                if ( ! is.na(value["frozen"]) ) {
                    effect@frozen= as.logical(value[["frozen"]])
                }
                if ( ! is.na(value["SD"]) )
                    effect@SD= as.numeric(value[["SD"]])
                if ( ! is.na(value["definition"]) )
                    effect@definition= value[["definition"]]
                if ( ! is.na(value["isBQL"]) )
                    effect@isBQL= as.logical(value[["isBQL"]])
                if ( ! is.na(value["bqlStaticValue"]) )
                    effect@bqlStaticValue= value[["bqlStaticValue"]]
                if ( ! is.na(value["dobefore"]) )
                  effect@dobefore= value[["dobefore"]]
                if ( ! is.na(value["doafter"]) )
                  effect@doafter= value[["doafter"]]
                effectsList[[indx]] = effect
            }
        }
        if ( is.null(.Object@objects) || length(.Object@objects )  == 0  ) {
          errorModel@effectsList = effectsList
          .Object@errorModel = errorModel
          .Object = updateErrorModel(.Object)
        } else {
          if (obsCompIndx != -1 ) {
            .Object@objects[[obsCompIndx]]@residualEffect = effectsList[[1]]
          }
        }
        .Object = generatePML(.Object)

        return(.Object)
    })

#' covariateEffect
#'
#' Lists style for a covariate/variable
#'
#' @param model            A PK/PD model
#' @param covariateName    Name of the covariate
#' @param variableName     Name of the model variable
#'
#' @return effect style    One of: EnableEffect|DisableEffect|PlusOneEffect
#'
#' @examples
#'     covariateEffect(pkpdmodel,"age","Cl")
#'
#'     covariateEffect(pkpdmodel,"age","Cl") = PlusOneEffect
#'
#' @export
#'
setGeneric(name="covariateEffect",
           def=function(.Object,covariateName,parameterName)
           {
               standardGeneric("covariateEffect")
           })

#'
#' @export
#'
setMethod(f="covariateEffect",
    signature="NlmePmlModel",
    definition=function(.Object,covariateName,parameterName){
        covariateList=attr(.Object,"covariateList")
        type=""
        for ( c in covariateList ) {
            name=attr(c,"name")
            if ( name == covariateName ) {
                effectsList=attr(c,"covarEffList")
                type= effectsList[[parameterName]]
            }
        }
        print(type)
    })
assign("covariateEffect",covariateEffect,envir=.GlobalEnv)

#'
#' @export
#'
setGeneric(name="covariateEffect<-",
           def=function(.Object,covariateName,parameterName,value)
           {
               standardGeneric("covariateEffect<-")
           })

#'
#' @export
#'
setMethod(f="covariateEffect<-",
    signature="NlmePmlModel",
    definition=function(.Object,covariateName,parameterName,value){
        covariateList=attr(.Object,"covariateList")
        indx=1
        if ( length(covariateList) > 0 )
        for ( indx in 1:length(covariateList) ) {
            c=covariateList[[indx]]
            name=attr(c,"name")
            if ( name == covariateName ) {
                effectsList=attr(c,"covarEffList")
                effectsList[[parameterName]]=value
                attr(c,"covarEffList")=effectsList
                covariateList[[indx]]=c
            }
        }
        attr(.Object,"covariateList")=covariateList
        return(.Object)
    })


#' initOccasionRandomEffect
#'
#' Lists/sets initial values for an inter-occasion variability
#'
#' @param model            A PK/PD model
#' @param covariateName    Name of the occasion covariate
#'
#' @examples
#'     initOccasionRandomEffect(pkpdmodel,"Occasion")
#'
#'     initOccasionRandomEffect(pkpdmodel,"Occasion") = c(0.1,0.02,0.1)
#'
#' @export
#'
setGeneric(name="initOccasionRandomEffect",
           def=function(.Object,covariateName)
           {
               standardGeneric("initOccasionRandomEffect")
           })

#'
#' @export
#'
setMethod(f="initOccasionRandomEffect",
    signature="NlmePmlModel",
    definition=function(.Object,covariateName){
        covariateList=attr(.Object,"covariateList")
        for ( c in covariateList ) {
            name=attr(c,"name")
            type=c@type
            if ( name == covariateName && type == Occasion ) {
                items = c@covarItems
                effectsList=attr(c,"covarEffList")
                valuesList=attr(c,"catEffInitValues")
#print(effectsList)
print(valuesList)
            }
        }
    })
assign("initOccasionRandomEffect",initOccasionRandomEffect,envir=.GlobalEnv)

#'
#' @export
#'
setGeneric(name="initOccasionRandomEffect<-",
           def=function(.Object,covariateName,parameterName,value)
           {
               standardGeneric("initOccasionRandomEffect<-")
           })

#'
#' @export
#'
setMethod(f="initOccasionRandomEffect<-",
    signature="NlmePmlModel",
    definition=function(.Object,covariateName,parameterName,value){
        covariateList=attr(.Object,"covariateList")
        for ( indx in 1:length(covariateList) ) {
            c=covariateList[[indx]]
            name=attr(c,"name")
            type=c@type
            if ( name == covariateName && type == Occasion ) {
                c@catEffInitValues=as.list(value)
                covariateList[[indx]]=c
            }
        }
        attr(.Object,"covariateList")=covariateList
        .Object = generatePML(.Object)
        return(.Object)
    })




#' userDefinedExtraDefinitions
#'
#' Lists/sets user defined extra column/table definition
#'
#' @param model            A PK/PD model
#' @param userDefinedList  Lines of extra column/table definition
#'
#' @examples
#'
#'     userDefinedExtraDefinitions(model)
#'
#'     userDefinedExtraDefinitions(model) = c("addlcol(ADDL)",
#'            " iicol(II)", "table(file=\"res.csv\",time(0),Ka,V,Cl,Tlg)")
#'
#' @export
#'
setGeneric(name="userDefinedExtraDefinitions",
           def=function(.Object,covariateName)
           {
             standardGeneric("userDefinedExtraDefinitions")
           })

#'
#' @export
#'
setMethod(f="userDefinedExtraDefinitions",
          signature="NlmePmlModel",
          definition=function(.Object,covariateName){
            userDefinedExtraDefs=attr(.Object,"userDefinedExtraDefs")

            userDefinedExtraDefs

          })
assign("userDefinedExtraDefinitions",userDefinedExtraDefinitions,envir=.GlobalEnv)

#'
#' @export
#'
setGeneric(name="userDefinedExtraDefinitions<-",
           def=function(.Object,covariateName,value)
           {
             standardGeneric("userDefinedExtraDefinitions<-")
           })

#'
#' @export
#'
setMethod(f="userDefinedExtraDefinitions<-",
          signature="NlmePmlModel",
          definition=function(.Object,covariateName,value){

            .Object@userDefinedExtraDefs = as.list(value)

            return(.Object)
          })





#' addSecondary
#'
#' Adds a secondary parameter to model definition
#'
#' @param model         PK/PD model
#' @param name          Name of the secondary parameter
#' @param definition    Definition of secondary variable
#' @param unit          Optional units
#'
#' @examples
#'
#'     model = addSecondary(model,"Spc_Param","log(2)/tvKe")
#'     model = addSecondary(model,"Tmax",
#'                          "CalcTMax(tvA,tvAlpha,tvB,tvBeta,C,Gamma)")
#'
#' @export
#'
setGeneric(name="addSecondary",
           def=function(.Object,name,definition,unit="")
           {
             standardGeneric("addSecondary")
           })

#'
#' @export
#'
setMethod(f="addSecondary",
          signature="NlmePmlModel",
          definition=function(.Object,name,definition,unit=""){

            secondParams = .Object@secondaryParameters
            found=FALSE
            for ( s in secondParams ) {
              if ( s@name == name ) {
                found=TRUE
                break
              }
            }
            if ( found == TRUE ) {
              print(paste0("Duplicate name ", name,  "is not allowed"))
            } else {
              param = SecondaryParameter(name,definition,unit)
              secondParams[[length(secondParams)+1]] = param

            .Object@secondaryParameters=secondParams
            .Object=generatePML(.Object)
          }
            .Object
          })
assign("addSecondary",addSecondary,envir=.GlobalEnv)

#' listSecondary
#'
#' Lists secondary parameter definitions for the model
#'
#' @param model PK/PD model
#'
#' @export
#'
setGeneric(name="listSecondary",
           def=function(.Object)
           {
             standardGeneric("listSecondary")
           })

#'
#' @export
#'
setMethod(f="listSecondary",
          signature="NlmePmlModel",
          definition=function(.Object){
            secondParams = .Object@secondaryParameters
            found=FALSE
            for ( s in secondParams ) {
              print(s)

            }

          })
assign("listSecondary",listSecondary,envir=.GlobalEnv)


#' deleteSecondary
#'
#' Deletes a secondary parameter from the model
#'
#' @param model     PK/PD model
#' @param name      Name of the secondary parameter
#'
#' @export
#'
setGeneric(name="deleteSecondary",
           def=function(.Object,name)
           {
             standardGeneric("deleteSecondary")
           })

#'
#' @export
#'
setMethod(f="deleteSecondary",
          signature="NlmePmlModel",
          definition=function(.Object,name){

            secondParams = .Object@secondaryParameters
            found=FALSE

            for ( indx in 1:length(secondParams) ) {
              s = secondParams[[indx]]
              if ( s@name == name ) {
                secondParams[indx] = NULL
                found = TRUE
                break
              }
            }
            if ( found == TRUE ) {
                .Object@secondaryParameters = secondParams
                .Object=generatePML(.Object)
            }else {
             print("Secondary parameter ",name," Not found")
           }
            .Object

          })
assign("deleteSecondary",deleteSecondary,envir=.GlobalEnv)


#'
#' Method to list attributes for a structural parameter
#'
#' @param model   Model with the parameter
#' @param name    Parameter name
#'
#' @export structuralParam
#'
#' @examples
#'       structuralParam(model,"Cl")
#'
setGeneric(name="structuralParam",
    def=function(.Object,parameterName)
    {
        standardGeneric("structuralParam")
    })

#'
#' @export
#'
setMethod(f="structuralParam",
    signature="NlmePmlModel",
    definition=function(.Object,parameterName){
        sps=attr(.Object,"structuralParams")
        for ( s in sps ) {
            name=attr(s,"name")
            if ( name == parameterName ) {
                return(s)
            }
        }
    })
assign("structuralParam",structuralParam,envir=.GlobalEnv)

#'
#' Method to set structural parameter attributes
#'
#' @param model   Model with the parameter
#' @param name    Name of the parameter
#'
#' @param fixedEffName     Name of fixed effect
#' @param randomEffName    Name of random effect
#' @param style            One of LogNormal|Normal|Combination|Log|Logit|Custom
#' @param initialValue     Initial value of the fixed effect
#' @param lowerBound       Lower limit value of fixed effect
#' @param upperBound       Upper limit value of fixed effect
#' @param units            Unit of measurement of fixed effect data
#' @param isFrozen         Freeze fixed effect?( default FALSE )
#' @param hasRandomEffect  Has random effect? (default TRUE)
#' @param code             Line of code that defines stparm()
#' @param extraCode        Extra lines of code that relates to this parameter
#'
#' @examples
#'      structuralParam(model,"Cl") = c(style=LogNormal,initialValue="0.75")
#'
#'      structuralParam(model,"Cl2") = c(style=Custom, code="stparm(V=10^(tvlog10V + nlog10V))")
#'
#' @export structuralParam
#'
setGeneric(name="structuralParam<-",
           def=function(.Object,parameterNames,value)
           {
               standardGeneric("structuralParam<-")
           })

#'
#' @export
#'
setMethod(f="structuralParam<-",
    signature="NlmePmlModel",
    definition=function(.Object,parameterNames,value){
        sps=attr(.Object,"structuralParams")
        indx=1
        for ( indx in 1:length(sps) ) {
            sp=sps[[indx]]
            name=attr(sp,"name")
            for ( parameterName in parameterNames ) {
            if ( name == parameterName ) {
                if( ! is.na(value["name"]) )
                    sp@name = value[["name"]]
                if( ! is.na(value["fixedEffName"]) )
                    sp@fixedEffName = value[["fixedEffName"]]
                if( ! is.na(value["randomEffName"]) )
                    sp@randomEffName = value[["randomEffName"]]
                if( ! is.na(value["hasRandomEffect"]) )
                    sp@hasRandomEffect = as.logical(value[["hasRandomEffect"]])
                if( ! is.na(value["hasCovariateEffect"]) )
                    sp@hasCovariateEffect =
                            as.logical(value[["hasCovariateEffect"]])
                if( ! is.na(value["style"]) )
                    sp@style = as.integer(value[["style"]])
                if( ! is.na(value["initialValue"]) )
                    sp@initialValue = value[["initialValue"]]
                if( ! is.na(value["lowerBound"]) )
                    sp@lowerBound = value[["lowerBound"]]
                if( ! is.na(value["upperBound"]) )
                    sp@upperBound = value[["upperBound"]]
                if( ! is.na(value["units"]) )
                    sp@units = value[["units"]]
                if( ! is.na(value["isFrozen"]) )
                    sp@isFrozen = value[["isFrozen"]]
                if( ! is.na(value["isSequential"]) )
                    sp@isSequential = value[["isSequential"]]
                if( ! is.na(value["code"]) )
                    sp@code = value[["code"]]
                extraCode=c()
                for( i in 1:10 ) {
                    key=paste0("extraCode",i)
                    if( ! is.na(value[key]) ) {
                        extraCode=c(extraCode,value[[key]])
                    }
                }
                if ( length(extraCode) != 0 )  {
                    sp@extraCode = as.list(extraCode)
                }
                sps[[indx]]=sp
            }
            }
        }
        attr(.Object,"structuralParams")=sps
        .Object=generatePMLModel(.Object)
        return(.Object)
    })

#'
#' @export
#'
spExists <-function(structuralParams,paramName ){

  found= FALSE
  for ( s in structuralParams ) {
    if ( s@name == paramName )
      found = TRUE
  }
  return(found)
}

#'
#' @export
#'
addStructuralParameter <-function(structuralParams,paramName ,
                                 hasRandomEffect=TRUE,
                                 hasCovariateEffect=FALSE,
                                 isFrozen= FALSE,
                                 isSequential=FALSE) {
   sp = NlmeStructuralParameter(paramName,
                                hasRandomEffect=hasRandomEffect,
                                hasCovariateEffect=hasCovariateEffect,
                                isFrozen = isFrozen,
                                isSequential=isSequential)
   indx=length(structuralParams)+1
   structuralParams[[indx]]= sp
   return(structuralParams)
}


#'
#' @export
#'
removeStructuralParameter <-function(structuralParams,paramName ){

    for ( indx in 1:length(structuralParams )) {
        sp = structuralParams[[indx]]

        if ( attr(sp,"name") == paramName ) {

            structuralParams[indx]=NULL
        }
    }
   return(structuralParams)
}





#'
#' @export
#'
setGeneric(name="modelStatements",
    def=function(.Object)
    {
               standardGeneric("modelStatements")
    })
setGeneric(name="modelStatements<-",
    def=function(.Object,value)
    {
               standardGeneric("modelStatements<-")
    })

#'
#' @export
#'
setMethod(f="modelStatements",
    signature="NlmePmlModel",
    definition=function(.Object){
        statements=attr(.Object,"statements")
        return(statements)
    })

#'
#' @export
#'
setMethod(f="modelStatements<-",
    signature="NlmePmlModel",
    definition=function(.Object,value){
        attr(.Object,"statements")=value
        return(.Object)
    })
assign("modelStatements",modelStatements,envir=.GlobalEnv)

#'
#' @export
#'
listModelStatements <-function(model){
    statements=attr(model,"statements")
    for ( s in statements )
        print(s)
}



#'
#' @export
#'
setGeneric(name="errorModel",
    def=function(.Object)
    {
               standardGeneric("errorModel")
    })
setGeneric(name="errorModel<-",
    def=function(.Object,value)
    {
               standardGeneric("errorModel<-")
    })

#'
#' @export
#'
setMethod(f="errorModel",
    signature="NlmePmlModel",
    definition=function(.Object){
        errorModel=attr(.Object,"errorModel")
        return(errorModel)
    })

#'
#' @export
#'
setMethod(f="errorModel<-",
    signature="NlmePmlModel",
    definition=function(.Object,value){
        attr(.Object,"errorModel")=value
        return(.Object)
    })
assign("errorModel",errorModel,envir=.GlobalEnv)


setGeneric(name="updateErrorModel",
    def=function(.Object)
    {
               standardGeneric("updateErrorModel")
    })

#' updateErrorModel
#'
#' Updates error model, recreating some structural parameters
#' that depend on how residual effects are used
#'
#' @export updateErrorModel
#'
setMethod(f="updateErrorModel",
    signature="NlmePmlModel",
    definition=function(.Object){
        errorModel=attr(.Object,"errorModel")
        effectsList=attr(errorModel,"effectsList")
        structuralParams=attr(.Object,"structuralParams")
        for ( indx in 1:length(effectsList) ) {
            effect=effectsList[[indx]]
            definition = attr(effect,"definition")
            errorType = attr(effect,"errorType")
            if ( errorType == ERR_ADD_MULT || errorType == ERR_MIX_RATIO )  {
                if ( definition!="" ){
                    if ( spExists(structuralParams,definition ) == FALSE )
                        structuralParams=addStructuralParameter(structuralParams,
                                                   definition,
                                                   hasRandomEffect=FALSE,
                                                   hasCovariateEffect=FALSE)
                    }
                } else {
                    if ( definition != "" ) {
                        structuralParams=removeStructuralParameter(
                                                   structuralParams,
                                                   definition)
                }
            }
        }
        if ( length(structuralParams) != 0 )
            .Object@structuralParams=structuralParams
        return(.Object)
    })

assign("updateErrorModel",updateErrorModel,envir=.GlobalEnv)


#'
#' @export
#'
setGeneric(name="createEmaxStructuralParameters",
           def=function(.Object)
           {
               standardGeneric("createEmaxStructuralParameters")
           })

setMethod(f="createEmaxStructuralParameters",
    signature="NlmePmlModel",
    definition=function(.Object){
        structuralParams=attr(.Object,"structuralParams")
        dosePoints=attr(.Object,"dosePoints")
        outputParams=attr(.Object,"outputParams")
        diffEquations=attr(.Object,"diffEquations")
        emaxModelAttrs=attr(.Object,"emaxModelAttrs")
        checkInhibitory = attr(emaxModelAttrs,"checkInhibitory")
        checkBaseline = attr(emaxModelAttrs,"checkBaseline")
        checkSigmoid = attr(emaxModelAttrs,"checkSigmoid")
        checkFractional = attr(emaxModelAttrs,"checkFractional")

        hasRandomEffect = .Object@isPopulation 

# Add E to the output
        outputParams=c(outputParams,"E")
        errorModel=attr(.Object,"errorModel")
        effectsList=attr(errorModel,"effectsList")
# Figure out equation for Emax model based on other params
        s0 = "E0"
        sMax = ""
        s50 = ""
        if ( checkInhibitory == FALSE ) {
            sMax = "Emax"
            s50  = "EC50"
        }
        else {
            sMax = "Imax"
            s50  = "IC50"
        }
    structuralParams=addStructuralParameter(structuralParams,s50,
                                            hasRandomEffect=hasRandomEffect )
    if ( checkSigmoid == TRUE )
        structuralParams=addStructuralParameter(structuralParams,"Gam" ,
                                           hasRandomEffect=hasRandomEffect)
    if ( checkBaseline == FALSE ) {
        if ( checkInhibitory == FALSE ) {
            structuralParams=addStructuralParameter(structuralParams,sMax ,
                                               hasRandomEffect=hasRandomEffect)
        }
        else {
            structuralParams=addStructuralParameter(structuralParams,s0,
                                              hasRandomEffect=hasRandomEffect )
        }
    }
    else {
       structuralParams=addStructuralParameter(structuralParams,s0 ,
                                              hasRandomEffect=hasRandomEffect )
       structuralParams=addStructuralParameter(structuralParams,sMax ,
                                              hasRandomEffect=hasRandomEffect )
    }

# Add standard deviation name to structural parameters

    if ( length(effectsList) > 0  )  {
        definition=attr(effectsList[[1]],"definition")
        errorType=attr(effectsList[[1]],"errorType")
        if ( errorType == ERR_ADD_MULT || errorType == ERR_MIX_RATIO )
            structuralParams=addStructuralParameter(structuralParams,
                                                   definition,
                                                   hasRandomEffect=FALSE,
                                                   hasCovariateEffect=FALSE)
    }
    .Object@outputParams=outputParams
    .Object@structuralParams=structuralParams
    return(.Object)
    })

assign("createEmaxStructuralParameters",createEmaxStructuralParameters,envir=.GlobalEnv)


#'
#' @export
#'
setGeneric(name="createLinearStructuralParameters",
           def=function(.Object)
           {
               standardGeneric("createLinearStructuralParameters")
           })

setMethod(f="createLinearStructuralParameters",
    signature="NlmePmlModel",
    definition=function(.Object){
        structuralParams=attr(.Object,"structuralParams")
        dosePoints=attr(.Object,"dosePoints")
        outputParams=attr(.Object,"outputParams")
        diffEquations=attr(.Object,"diffEquations")
        linearModelType  = .Object@linearModelType

        cInput = "C"

        hasRandomEffect = .Object@isPopulation
# Add E to the output
        outputParams=c(outputParams,"E")
        errorModel=attr(.Object,"errorModel")
        effectsList=attr(errorModel,"effectsList")

# Figure out equation for Linear model based on other params

        structuralParams=addStructuralParameter(structuralParams,"Alpha",
                                            hasRandomEffect=hasRandomEffect )
        s = "E = Alpha"
        if ( linearModelType >= LinearBeta ) {
            s = paste0(s, " + Beta*", cInput)
            structuralParams=addStructuralParameter(structuralParams,"Beta",
                                            hasRandomEffect=hasRandomEffect )
        }
        if ( linearModelType >= LinearGamma ) {
            s = paste0(s, " + Gam*", cInput,"^2")
            structuralParams=addStructuralParameter(structuralParams,"Gam",
                                            hasRandomEffect=hasRandomEffect )
        }

# Add standard deviation name to structural parameters

    if ( length(effectsList) > 0  )  {
        definition=attr(effectsList[[1]],"definition")
        errorType=attr(effectsList[[1]],"errorType")
        if ( errorType == ERR_ADD_MULT || errorType == ERR_MIX_RATIO )
            structuralParams=addStructuralParameter(structuralParams,
                                                   definition,
                                                   hasRandomEffect=hasRandomEffect,
                                                   hasCovariateEffect=FALSE)
    }
    .Object@outputParams=outputParams
    .Object@structuralParams=structuralParams
    return(.Object)
    })

assign("createLinearStructuralParameters",createLinearStructuralParameters,envir=.GlobalEnv)


#'
#' @export
#'
setGeneric(name="createPkStructuralParameters",
           def=function(.Object)
           {
               standardGeneric("createPkStructuralParameters")
           })

setMethod(f="createPkStructuralParameters",
    signature="NlmePmlModel",
    definition=function(.Object){
        structuralParams=attr(.Object,"structuralParams")
        dosePoints=attr(.Object,"dosePoints")
        dosePoints=list()
        outputParams=attr(.Object,"outputParams")
        diffEquations=attr(.Object,"diffEquations")
        pkModelAttrs=attr(.Object,"pkModelAttrs")

        parameterization   = attr(pkModelAttrs,"parameterization")
        paramType          = attr(parameterization,"paramType")
        absorption         = attr(pkModelAttrs,"absorption")
        absorpType         = attr(absorption,"absorpType")
        numCompartments      = attr(pkModelAttrs,"numCompartments")
        isTlag             = attr(pkModelAttrs,"isTlag")
        hasEliminationComp = attr(pkModelAttrs,"hasEliminationComp")
        isFractionExcreted = attr(pkModelAttrs,"isFractionExcreted")
        isSaturating = attr(pkModelAttrs,"isSaturating")
        infusionAllowed    = attr(pkModelAttrs,"infusionAllowed")
        isDuration         = attr(pkModelAttrs,"isDuration")
        isSequential       = attr(pkModelAttrs,"isSequential")
        isPkFrozen       = attr(pkModelAttrs,"isPkFrozen")
        isClosedForm       = attr(pkModelAttrs,"isClosedForm")

        sKe = ""
        sKa = "Ka"
        sDose =""

# Ka parameter
        sKe = "Ke"

        if (  paramType  == PARAM_MICRO )
            sKe = "Ke"
# FRED       sKe = "Ka"

        hasRandomEffect = .Object@isPopulation 
        if ( absorpType == PARAM_EXTRAVASCULAR ) {
            structuralParams=addStructuralParameter(structuralParams,"Ka",
                                            hasRandomEffect=hasRandomEffect ,
                                            isFrozen = isPkFrozen,
                                            isSequential = isSequential)
        } else {
#            sDosepoint = "A1"
        }

# Check to see if you need to add C.
# logic is
#  if ! ((bPKPD &&( bSequential || bFrozen ) ))
#
        modelType = attr(attr(.Object,"modelType"),"modelType" )
        if ( modelType != PARAM_PK )  {
            sCOutput = "C"
            outputParams=c(outputParams,sCOutput)
        }
        structuralParams=addStructuralParameter(structuralParams,"V",
                                            hasRandomEffect=hasRandomEffect ,
                                            isFrozen = isPkFrozen,
                                            isSequential = isSequential)
        if ( paramType == PARAM_MICRO ) {
            structuralParams=addStructuralParameter(structuralParams,sKe,
                                            hasRandomEffect=hasRandomEffect ,
                                            isFrozen = isPkFrozen,
                                            isSequential = isSequential)
        }
        if ( paramType == PARAM_CLEARANCE ) {
            if ( isSaturating == FALSE )
                structuralParams=addStructuralParameter(structuralParams,"Cl",
                                            hasRandomEffect=hasRandomEffect ,
                                            isFrozen = isPkFrozen,
                                            isSequential = isSequential)
            else {
                structuralParams=addStructuralParameter(structuralParams,"Km",
                                            hasRandomEffect=hasRandomEffect ,
                                            isFrozen = isPkFrozen,
                                            isSequential = isSequential)
                structuralParams=addStructuralParameter(structuralParams,"Vmax",
                                            hasRandomEffect=hasRandomEffect ,
                                            isFrozen = isPkFrozen,
                                            isSequential = isSequential)
            }
        }
        if ( hasEliminationComp == TRUE && isFractionExcreted == TRUE ){
            structuralParams=addStructuralParameter(structuralParams,"Fe",
                                            hasRandomEffect=hasRandomEffect ,
                                            isFrozen = isPkFrozen,
                                            isSequential = isSequential)
        }
        if ( numCompartments >= 2 )
        for ( c in 2:numCompartments ) {
            if ( paramType == PARAM_MICRO ) {
            structuralParams=addStructuralParameter(structuralParams,
                                            sprintf("K1%0d",c),
                                            hasRandomEffect=hasRandomEffect ,
                                            isFrozen = isPkFrozen,
                                            isSequential = isSequential)
            structuralParams=addStructuralParameter(structuralParams,
                                            sprintf("K%0d1",c),
                                            hasRandomEffect=hasRandomEffect,
                                            isFrozen = isPkFrozen,
                                            isSequential = isSequential)
            }
            if ( paramType == PARAM_CLEARANCE ) {
                structuralParams=addStructuralParameter(structuralParams,
                                            sprintf("V%0d",c),
                                            hasRandomEffect=hasRandomEffect ,
                                            isFrozen = isPkFrozen,
                                            isSequential = isSequential)
                structuralParams=addStructuralParameter(structuralParams,
                                            sprintf("Cl%0d",c),
                                            hasRandomEffect=hasRandomEffect,
                                            isFrozen = isPkFrozen,
                                            isSequential = isSequential)
            }
        }
        sDose = paste0(sDose,")")

        if ( isTlag )
            structuralParams=addStructuralParameter(structuralParams,
                                            "Tlag",
                                            hasRandomEffect=hasRandomEffect,
                                            isFrozen = isPkFrozen,
                                            isSequential = isSequential)

        errorModel=attr(.Object,"errorModel")
        effectsList=attr(errorModel,"effectsList")

# Add standard deviation name to structural parameters

    if ( length(effectsList) > 0  )  {
        definition=attr(effectsList[[1]],"definition")
        errorType=attr(effectsList[[1]],"errorType")
        if ( errorType == ERR_ADD_MULT || errorType == ERR_MIX_RATIO )
            structuralParams=addStructuralParameter(structuralParams,
                                                   definition,
                                                   hasRandomEffect=hasRandomEffect,
                                                   hasCovariateEffect=FALSE,
                                            isFrozen = isPkFrozen,
                                            isSequential = isSequential)
    }
    .Object@structuralParams=structuralParams
    .Object@outputParams=outputParams
    return(.Object)
    })

assign("createPkStructuralParameters",createPkStructuralParameters,envir=.GlobalEnv)





setGeneric(name="generateLinearModel",
           def=function(.Object,scInput)
           {
               standardGeneric("generateLinearModel")
           })

setMethod(f="generateLinearModel",
    signature="NlmePmlModel",
    definition=function(.Object,scInput){
        statements=attr(.Object,"statements")
        if ( length(statements) == 0 )
            statements=c(statements, "test(){" )
        structuralParams=attr(.Object,"structuralParams")
        dosePoints=attr(.Object,"dosePoints")
        outputParams=attr(.Object,"outputParams")
        diffEquations=attr(.Object,"diffEquations")
        linearModelType  = .Object@linearModelType

# Add a C covariate if there is no input from a prior model
        if ( length(dosePoints) == 0 ) {
#FRED
            statement=sprintf("    covariate(%s)",scInput)
            statements = c( statements,  statement )

        }
# Add E to the output
        outputParams=c(outputParams,"E")

    statement = "E = Alpha"
    if ( linearModelType >= LinearBeta )
        statement = paste0(statement, " + Beta*", scInput)
    if ( linearModelType >= LinearGamma )
        statement = paste0(statement, " + Gam*", scInput,"^2")
    statements = c( statements,  statement )

    .Object@statements=statements
    .Object@dosePoints=dosePoints
    .Object@outputParams=outputParams
    return(.Object)
    })


setGeneric(name="generateEffectsModel",
           def=function(.Object,scInput,frozen)
           {
             standardGeneric("generateEffectsModel")
           })

setMethod(f="generateEffectsModel",
          signature="NlmePmlModel",
          definition=function(.Object,scInput,frozen){
            statements=attr(.Object,"statements")

            structuralParams=attr(.Object,"structuralParams")

            diffEquations=attr(.Object,"diffEquations")
            if ( spExists(structuralParams,"Ke0") == FALSE ){
                structuralParams=addStructuralParameter(structuralParams,"Ke0",
                                                    hasRandomEffect=!frozen ,
                                                    hasCovariateEffect = !frozen,
                                                    isFrozen = frozen)
            }
            statement = paste0("    deriv(Ce = Ke0*(",scInput, " - Ce))")
            statements = c(statements, statement)

            .Object@structuralParams = structuralParams
            .Object@statements = statements

            .Object
})

setGeneric(name="generateEmaxModel",
           def=function(.Object,scInput)
           {
               standardGeneric("generateEmaxModel")
           })

setMethod(f="generateEmaxModel",
    signature="NlmePmlModel",
    definition=function(.Object,scInput){
        statements=attr(.Object,"statements")
        if ( length(statements) == 0 )
            statements=c(statements, "test(){" )
        structuralParams=attr(.Object,"structuralParams")
        dosePoints=attr(.Object,"dosePoints")
        outputParams=attr(.Object,"outputParams")
        diffEquations=attr(.Object,"diffEquations")
        emaxModelAttrs=attr(.Object,"emaxModelAttrs")
        checkInhibitory = attr(emaxModelAttrs,"checkInhibitory")
        checkBaseline = attr(emaxModelAttrs,"checkBaseline")
        checkSigmoid = attr(emaxModelAttrs,"checkSigmoid")
        checkFractional = attr(emaxModelAttrs,"checkFractional")
        frozen = emaxModelAttrs@frozen

# Add a C covariate if there is no input from a prior model
        if ( length(dosePoints) == 0 ) {
#FRED
            statement=sprintf("    covariate(%s)",scInput)
            statements = c( statements,  statement )

        }
# Add E to the output
        outputParams=c(outputParams,"E")

# Figure out equation for Emax model based on other params
        s0 = "E0"
        sMax = ""
        s50 = ""
        if ( checkInhibitory == FALSE ) {
            sMax = "Emax"
            s50  = "EC50"
        }
        else {
            sMax = "Imax"
            s50  = "IC50"
        }

    sC   = ""
    sC50 = ""
    if ( checkSigmoid == FALSE ) {
        sC   = scInput
        sC50 = s50
    }
    else {
        sC   = paste0(scInput,"^Gam")
        sC50 = paste0(s50,"^Gam")
    }
    sFrac = paste0(sC , " / (",sC50," + ",sC,")")
    if ( checkBaseline == FALSE ) {
        if ( checkInhibitory == FALSE ) {
            s = paste0(sMax , " * ", sFrac )
        }
        else {
            s = paste0(s0  , " + ", sMax, " * ",sFrac  )
        }
    }
    else  if ( checkFractional == FALSE && checkInhibitory == FALSE ) {
       s = paste0( s0, " + ", sMax, " * " , sFrac );
    } else if ( checkFractional == FALSE && checkInhibitory == TRUE ) {
       s = paste0( s0, " - ", sMax, " * " , sFrac );
    } else if ( checkFractional == TRUE && checkInhibitory == FALSE ) {
       s = paste0( s0, " * (1  +  ", sMax, " * " , sFrac ,")");
    } else if ( checkFractional == TRUE && checkInhibitory == TRUE ) {
       s = paste0( s0, " * (1  -  ", sMax, " * " , sFrac ,")");
    }
    statement=sprintf("    E = %s",s)
    statements = c( statements,  statement )

    .Object@statements=statements
    .Object@dosePoints=dosePoints
    .Object@outputParams=outputParams
    return(.Object)
    })

assign("generateEmaxModel",generateEmaxModel,envir=.GlobalEnv)


setGeneric(name="generatePkModel",
           def=function(.Object)
           {
               standardGeneric("generatePkModel")
           })

setMethod(f="generatePkModel",
    signature="NlmePmlModel",
    definition=function(.Object){

        statements=attr(.Object,"statements")
        if ( length(statements) == 0 )
            statements=c(statements, "test(){" )
        structuralParams=attr(.Object,"structuralParams")
        dosePoints=attr(.Object,"dosePoints")
        outputParams=attr(.Object,"outputParams")
#        diffEquations=attr(.Object,"diffEquations")
        diffEquations=list()
        pkModelAttrs=attr(.Object,"pkModelAttrs")

        parameterization   = attr(pkModelAttrs,"parameterization")
        paramType          = attr(parameterization,"paramType")
        absorption         = attr(pkModelAttrs,"absorption")
        absorpType         = attr(absorption,"absorpType")
        numCompartments    = attr(pkModelAttrs,"numCompartments")
        isTlag             = attr(pkModelAttrs,"isTlag")
        hasEliminationComp = attr(pkModelAttrs,"hasEliminationComp")
        isFractionExcreted = attr(pkModelAttrs,"isFractionExcreted")
        isSaturating        = attr(pkModelAttrs,"isSaturating")
        infusionAllowed    = attr(pkModelAttrs,"infusionAllowed")
        isDuration         = attr(pkModelAttrs,"isDuration")
        isSequential       = attr(pkModelAttrs,"isSequential")
        isClosedForm       = attr(pkModelAttrs,"isClosedForm")
        isMichaelisMenten       = attr(pkModelAttrs,"isMichaelisMenten")
        isMichaelisMenten = isSaturating
        sDose = ""
        sAa = ""
        sA1 = ""
        sA2 = ""
        sA3 = ""
        sA0 = ""
        sFe = "Fe"
        sA12 = ""
        sA13 = ""
        sA21 = ""
        sA31 = ""
        sAa1 = ""
        sA10 = ""
        sKe = "Ke"
        sKa = "Ka"
        sMicro = ""
        sMacro = ""
        sDoseStatment = ""
        sDosepoint = ""

#if (iParm==parmMicro && iAbs==absExtravascular && bKaEqKe) sKe = "Ka";
#XXXXXXXXXXX

        attr(.Object,"isTimeBased") = TRUE

        if (  paramType  == PARAM_MICRO )
            sKe = "Ke"

        if ( absorpType == PARAM_EXTRAVASCULAR )
            sDosepoint = "Aa"
        else
            sDosepoint = "A1"
        doseStatments=c()

        sDoseStatment = paste0("    dosepoint(",sDosepoint)

        sCOutput = "C"

# if (bPKPD && (bSequential || bFrozen)) : Do not add C to output

        outputParams=c(outputParams,sCOutput)

        if ( isTlag ) {
            sDoseStatment = paste0(sDoseStatment,", tlag = Tlag")
        }
#if (DoesWNLBuiltinModelAcceptIDoseCovariate(ms.iWNLModelPK)){
##				sDose += ", infdosevar = "+sDosepoint+"InfDose";
#			}
#			if (DoesWNLBuiltinModelAcceptRateCovariate(ms.iWNLModelPK)){
#				sDose += ", infratevar = "+sDosepoint+"InfRate";


        sDoseStatment =  paste0(sDoseStatment, ")")

        dosePoints = c( dosePoints, sDosepoint  )
        doseStatments = c( doseStatments, sDoseStatment  )

        # Get absorption flow
#        ???
          if (  paramType  == PARAM_MICRO &&
                absorpType == PARAM_EXTRAVASCULAR )
            sAa1 = paste0(sKa , " * Aa")
        else {
            if ( absorpType == PARAM_EXTRAVASCULAR )
              sAa1 = "Ka * Aa"
        }
 #       if (iParm==parmMicro && iAbs==absExtravascular && bKaEqKe){
 #           sAa1 = sKe + " * Aa";
 #           ms.AddSParm(sKe, bFrozen, true, bSequential);
 #         } else if (iAbs==absExtravascular){
 #           sAa1 = "Ka * Aa";
 #           ms.AddSParm("Ka", bFrozen, true, bSequential);
 #         }
        # Concentration statement
        doseStatments  = c(  doseStatments, "    C = A1 / V")

        # Is it closed form
        if ( isClosedForm ) {
            if ( paramType == PARAM_MICRO )
                sMicro = paste0("cfMicro(A1, ",sKe)
            if ( paramType == PARAM_CLEARANCE )
                sMicro = paste0("cfMicro(A1,Cl/V")
        }
        else {
        }
        if ( paramType == PARAM_MACRO )
            sMacro = paste0("cfMacro(",sDosepoint,",Cl,",sDosepoint,"Dose,A,Alpha")
        if ( paramType == PARAM_MACRO1 )
            sMacro = paste0("cfMacro1(A,Alpha")

        macros=c("B,Beta","C,Gamma")
        if ( numCompartments >= 2 )
        for ( c in 2:numCompartments ) {
            if (  isClosedForm ) {
                if (  paramType == PARAM_MICRO )
                    sMicro=paste0(sMicro,sprintf(", K1%0d, K%0d1",c,c))
                else  if ( paramType == PARAM_CLEARANCE )
                    sMicro = paste0(sMicro,sprintf(", Cl%0d/V, Cl%0d/V%0d",c,c,c))
            } else {
                if ( paramType == PARAM_CLEARANCE )
                    statements = c( statements, sprintf("    C%0d = A%0d/V%0d",c,c,c))
            }
            if ( paramType == PARAM_MACRO || paramType == PARAM_MACRO1 )
                sMacro = paste0(sMacro, macros[c-1])
        }
        if ( sMicro != "" ) {
            if ( absorpType == PARAM_EXTRAVASCULAR )
                sMicro = paste0(sMicro,", first = (Aa = Ka)")
            sMicro = paste0(sMicro,")")
            statements = c( statements, sMicro)
        }

        if ( sMacro != "" ) {
            if ( PARAM_MACRO ) {
                if ( absorpType == PARAM_EXTRAVASCULAR )
                    sMacro = paste0(sMacro,",Ka")
                sMacro = paste0(sMacro,",strip=A1strip")
                statements = c( statements, "covariate(A1Strip)")
            } else {
                if ( absorpType == PARAM_EXTRAVASCULAR )
                    sMacro = paste0(sMacro,",first = (Aa = Ka)")
            }
            sMacro = paste0(sMacro,")")
            statements = c( statements, sMacro)
        }
        for ( d in doseStatments )
            statements = c( statements,  d )

        # Get elimination flow
        if ( paramType == PARAM_CLEARANCE ) {
            if ( isMichaelisMenten ) {
                sA10 = "Vmax * C / (Km +C)"
            } else {
                sA10 = "Cl * C"
            }
        }
        if ( paramType == PARAM_MICRO ) {
            sA10 = paste0(sKe, " * A1")
        }
        # Compartment 2 elimination
        if ( numCompartments >= 2 ){
            if ( paramType == PARAM_MICRO ) {
                sA12 = "K12 * A1"
                sA21 = "K21 * A2"
            } else if ( paramType == PARAM_CLEARANCE ) {
                sA12 = "Cl2 * (C - C2)"
            }
        }
        # Compartment 3 elimination
        if ( numCompartments >= 3 ){
            if ( paramType == PARAM_MICRO ) {
                sA13 = "K13 * A1"
                sA31 = "K31 * A3"
            } else if ( paramType == PARAM_CLEARANCE ) {
                sA13 = "Cl3 * (C - C3)"
            }
        }

        # Get differential equation
        # Handle absorption
        if ( sAa1 != "" ) {
            if ( sAa != "" )
                sAa = paste0(sAa," ")
            sAa = paste(sAa,"- ",sAa1)

            if ( sA1 != "" )
                sA1 = paste0(sA1," + ")
            sA1 = paste(sA1, sAa1)
        }
        # Handle elimination
        if ( sA10 != "" ) {
            if ( sA1 != "" )
                sA1 = paste0(sA1," ")
            sA1 = paste(sA1,"- ",sA10)
            if ( sA0 != "" )
                sA0 = paste0(sA0," + ")
            sA0 = paste(sA0, sA10)
        }
        # Handle compartment 2 flow
        if ( sA12 != "" ) {
            if ( sA1 != "" )
                sA1 = paste0(sA1," ")
            sA1 = paste(sA1,"- ",sA12)
            if ( sA2 != "" )
                sA2 = paste0(sA2," + ")
            sA2 = paste(sA2, sA12)
        }
        if ( sA21 != "" ) {
            if ( sA2 != "" )
                sA2 = paste0(sA2," ")
            sA2 = paste(sA2,"- ",sA21)
            if ( sA1 != "" )
                sA1 = paste0(sA1," + ")
            sA1 = paste(sA1, sA21)
        }
        # Handle compartment 3 flow
        if ( sA13 != "" ) {
            if ( sA1 != "" )
                sA1 = paste0(sA1," ")
            sA1 = paste(sA1,"- ",sA13)
            if ( sA3 != "" )
                sA3 = paste0(sA3," + ")
            sA3 = paste(sA3, sA13)
        }
        if ( sA31 != "" ) {
            if ( sA3 != "" )
                sA3 = paste0(sA3," ")
            sA3 = paste(sA3,"- ",sA31)
            if ( sA1 != "" )
                sA1 = paste0(sA1," + ")
            sA1 = paste(sA1, sA31)
        }

        # Generate differential equations
        if ( sAa != "" ) {
            ds = paste0("    deriv(Aa = ",sAa,")")
            diffEquations = c(diffEquations, ds)
        }
        if ( sA1 != "" ) {
            ds = paste0("    deriv(A1 = ",sA1,")")
            diffEquations = c(diffEquations, ds)
        }
        if ( sA2 != "" ) {
            ds = paste0("    deriv(A2 = ",sA2,")")
            diffEquations = c(diffEquations, ds)
        }
        if ( sA3 != "" ) {
            ds = paste0("    deriv(A3 = ",sA3,")")
            diffEquations = c(diffEquations, ds)
        }
        if ( sA0 != "" && hasEliminationComp ){
            s = paste0("    urinecpt(A0 = ",sA0)
#FIX_ME            if ( bElimCptFe && sFe != "" )
            if ( sFe != "" && isFractionExcreted == TRUE )
                s = paste0(s,", fe=",sFe)
            s = paste0(s,")")
            ds = s
            outputParams=c(outputParams,"A0")
            diffEquations = c(diffEquations, ds)
        }
if ( isClosedForm == FALSE ) {
     if ( length(diffEquations) > 0 )
     for ( de in diffEquations ) {
         statements = c(statements,de)
     }
}


    .Object@statements=statements
    .Object@dosePoints=dosePoints
    .Object@outputParams=outputParams
    .Object@diffEquations=diffEquations
    return(.Object)
    })


assign("generatePkModel",generatePkModel,envir=.GlobalEnv)


#' modelObservationVariables
#'
#' Returns observation variables defined for this model
#'
#' @param model    PK/PD model
#'
#' @examples
#'
#'       obsvars = modelObservationVariables(pkmodel)
#'
#' @export
#'
modelObservationVariables <-function(model){
   statements=model@statements
   obs=c()
   observeKeywords=c("multi\\(","observe\\(","LL\\(","count\\(",
                     "ordinal\\(", "event\\(")

   for ( l in statements ) {
     exist = any(sapply(observeKeywords, grepl, l))
     if ( exist == TRUE ) {
       obs=c(obs,l)
     }
   }
   return(GetObservationVariables(modelLines=obs))
}

assign("modelObservationVariables",modelObservationVariables,envir=.GlobalEnv)

#'
#' @export
#'
getContinuousEffectsString <-function(usageType,name,effName,effType,isPositive,
                            centerValue,style){

    if ( isPositive == TRUE && style == LogNormal ) {
        operation="/"
        operation2="^"
    }
    else {
        operation="-"
        operation2="*"
    }
    if ( usageType == COVAR_EFF_PLUS_ONE ){
        operation="-"
        operation2="*"
    }

    if ( effType == COVAR_NUMBER ) {
       if ( centerValue != "" )
            name=paste0("(",name,operation,centerValue,")")
       if ( usageType == COVAR_EFF_YES ) {
            name=paste0("(",name,operation2,effName,")")
        } else if ( usageType == COVAR_EFF_PLUS_ONE ) {
                name=paste0("(1+",name,operation2,effName,")")
        }
    }else if ( effType == COVAR_MEDIAN ) {

        name=paste0("(",name,operation,"median(",name,"))")
       if ( usageType == COVAR_EFF_YES ) {
            name=paste0("(",name,operation2,effName,")")
        } else if ( usageType == COVAR_EFF_PLUS_ONE ) {
                name=paste0("(1+",name,operation2,effName,")")
        }

    }else if ( effType == COVAR_MEAN ) {
        name=paste0("(",name,operation,"mean(",name,"))")
       if ( usageType == COVAR_EFF_YES ) {
            name=paste0("(",name,operation2,effName,")")
        } else if ( usageType == COVAR_EFF_PLUS_ONE ) {
                name=paste0("(1+",name,operation2,effName,")")
        }
    }
    return(name)
}


#'
#' @export
#'
getCovariateEffNames <-function(model){
#print("------------ getCovariateEffNames()-----------")
    names=c()
    structuralParams=attr(model,"structuralParams")
    covariateList=attr(model,"covariateList")

    if ( length(structuralParams ) > 0 )
    for ( i in 1:length(structuralParams)) {
        stp = structuralParams[[i]]
        name = attr(stp,"name")
        style = stp@style
        fixedEffName = attr(stp,"fixedEffName")
        if ( fixedEffName != "" && stp@isFrozen == FALSE ) {
            if ( length(covariateList) > 0 )
            for ( indx in 1:length(covariateList) ){
                strArray=generateCovariateNames(name,
                                                  covariateList[[indx]],
                                                  style )
                if ( length(strArray) != 0 ) {
                    for ( s in unlist(strArray) )
                        names= c(names,s)
                }
            }
        }
    }
    return(names)
}

#'
#' @export
#'
getCovariateEffDirection <-function(model){
#print("------------ getCovariateEffDirection()-----------")
    direction=c()
    kSubmodel=0
    structuralParams=attr(model,"structuralParams")
    covariateList=attr(model,"covariateList")
    if ( length(structuralParams ) > 0 )
    for ( i in 1:length(structuralParams)) {
        stp = structuralParams[[i]]
        name = attr(stp,"name")
        style = stp@style
        fixedEffName = attr(stp,"fixedEffName")
        if ( fixedEffName != "" ) {
            if ( length(covariateList) > 0 )
            for ( indx in 1:length(covariateList) ){
#                dir = attr(covariateList[[indx]],"direction")
                # check for frozen covariate effects
                strArray=generateCovariateNames(name,
                                                  covariateList[[indx]],
                                                  style )
                if ( length(strArray) != 0 ) {
                    for ( s in unlist(strArray) ) {
                        dir=kSubmodel
                        kSubmodel = kSubmodel + 1
                        direction= c(direction,dir)
                    }
                }
            }
        }
    }
    return(direction)
}




#'
#' @export
#'
getCategoryEffectString <-function(usageType,stpName,covarName,indx){

    effectName=paste0("d",stpName,"d",covarName,indx)
    if ( usageType == COVAR_EFF_YES ) {
        name=paste0(paste0(effectName,"*(",covarName,"==",indx,")"))
##        name=paste0(paste0("exp(",effectName,"*(",covarName,"==",indx,"))"))
    } else if ( usageType == COVAR_EFF_PLUS_ONE ) {
        name=paste0(paste0("(1+",effectName,"*(",covarName,"==",indx,"))"))
    }
#    return(paste0(effectName,"*(",covarName,"==",indx,")"))
    return(name)
}


#'
#' @export
#'
getOccasionEffectString <-function(usageType,stpName,covarName,indx){

    effectName=paste0("n",stpName,"x",indx)
    if ( usageType == COVAR_EFF_YES ) {
        name=paste0(paste0(effectName,"*(",covarName,"==",indx,")"))
#        name=paste0(paste0("exp(",effectName,"*(",covarName,"==",indx,"))"))
    } else if ( usageType == COVAR_EFF_PLUS_ONE ) {
        name=paste0(paste0("(1+",effectName,"*(",covarName,"==",indx,"))"))
    }
#    return(paste0(effectName,"*(",covarName,"==",indx,")"))
    return(name)
}



#'
#' @export
#'
generateCovariateEffects <-function(stpName,covariate,style){

    ret=c()
        c=covariate
        covarEffList=attr(c,"covarEffList")
        usageType=covarEffList[[stpName]]

        sList=list()
        if ( length(usageType ) == 0 )
            usageType = COVAR_EFF_NO
        if ( usageType != COVAR_EFF_NO ) {
            covarName=attr(c,"name")
            type=attr(c,"type")
            centerValue=attr(c,"centerValue")
            effType=attr(c,"continuousType")
            isPositive=attr(c,"isPositive")
            effName=paste0("d",stpName,"d",covarName)
            if ( type == COVAR_CONTINUOUS ){
                s=getContinuousEffectsString(usageType,covarName,effName,
                                            effType,isPositive,centerValue,style)
                ret=c(ret,s)
            } else if ( type == Category ) {
                sList=list()
                items=attr(c,"covarItems")
                for ( i in 2:length(items) ) {
                    item=items[[i]]
                    val=attr(item,"value")
                    s=getCategoryEffectString(usageType,stpName,covarName,val)
                    ret=c(ret,s)
                }
            } else if ( type == COVAR_OCCASION ) {
                sList=list()
                items=attr(c,"covarItems")
                for ( i in 1:length(items) ) {
                    item=items[[i]]
                    val=attr(item,"value")
                    s=getOccasionEffectString(usageType,stpName,covarName,val)
                    ret=c(ret,s)
                }
            }
        }
     return(ret)
}

#'
#' @export
#'
generateCovariateNames <-function(stpName,covariate,style){

    ret=c()
        c=covariate
        covarEffList=attr(c,"covarEffList")
        usageType=covarEffList[[stpName]]

        sList=list()
        if ( length(usageType ) == 0 )
            usageType = COVAR_EFF_NO
        if ( usageType != COVAR_EFF_NO ) {
            covarName=attr(c,"name")
            type=attr(c,"type")
            effName=paste0("d",stpName,"d",covarName)
            if ( type == COVAR_CONTINUOUS ){
                effName=paste0("d",stpName,"d",covarName)
                ret=c(ret,effName)
            } else if ( type == Category ) {
                sList=list()
                items=attr(c,"covarItems")
                for ( i in 2:length(items) ) {
                    item=items[[i]]
                    val=attr(item,"value")
                    s=getCategoryEffectString(usageType,stpName,covarName,val)
                    effName=paste0("d",stpName,"d",covarName,val)
                    ret=c(ret,effName)
                }
            } else if ( type == COVAR_OCCASION ) {
            }
        }
     return(ret)
}
#'
#' @export
#'
setGeneric(name="generateCovariateStatement",
           def=function(.Object)
           {
               standardGeneric("generateCovariateStatement")
           })
#'
#' @export
#'
setMethod(f="generateCovariateStatement",
    signature="NlmePmlModel",
    definition=function(.Object){
        statements=attr(.Object,"statements")
        covariates=attr(.Object,"covariateList")

        if ( length(covariates) != 0 )
        for ( i in 1:length(covariates)) {
            covar=covariates[[i]]
            name = attr(covar,"name")
            type = attr(covar,"type")
            direction=attr(covar,"direction")

            if ( direction == COVAR_BACKWARD )
                statement =  paste0("    covariate(", name)
            if ( direction == COVAR_FORWARD )
                statement =  paste0("    fcovariate(", name)
            if ( direction == COVAR_INTERPOLATE )
                statement =  paste0("    interpolate(", name)
            if ( type != COVAR_CONTINUOUS )
                statement =  paste0(statement,"()")
            statement =  paste0(statement,")")
            statements=c(statements,statement)
         }
         .Object@statements = statements
         return(.Object)
    })

#' resetCovariateEffects
#'
#' Returns a new model with  all covariate effects cleared.
#'
#' @param model    PK/PD model
#'
#' @examples
#'       newModel = resetCovariateEffects(pkmodel)
#'
#'       covariateEffect(newModel,"wt","Cl")=COVAR_EFF_YES
#'
#' @export resetCovariateEffects
#'
setGeneric(name="resetCovariateEffects",
           def=function(.Object)
           {
               standardGeneric("resetCovariateEffects")
           })
#'
#' @export
#'
setMethod(f="resetCovariateEffects",
    signature="NlmePmlModel",
    definition=function(.Object){
        statements=attr(.Object,"statements")
        covariates=attr(.Object,"covariateList")
        if ( length(covariates) != 0 )
        for ( i in 1:length(covariates)) {
            covar=covariates[[i]]
            name = attr(covar,"name")
            type = attr(covar,"type")
            direction=attr(covar,"direction")
            covar@covarEffList = list()
            covariates[[i]] = covar
         }
         .Object@covariateList = covariates
         return(.Object)
    })



#' @export
setGeneric(name="generateStparmSatement",
           def=function(.Object)
           {
               standardGeneric("generateStparmSatement")
           })
#'
#' @export
#'
setMethod(f="generateStparmSatement",
    signature="NlmePmlModel",
    definition=function(.Object){
        statements=attr(.Object,"statements")
        structuralParams=attr(.Object,"structuralParams")
        covariateList=attr(.Object,"covariateList")
        if ( length(structuralParams ) > 0 )
        for ( i in 1:length(structuralParams)) {
            stp=structuralParams[[i]]
            name = attr(stp,"name")
            fixedEffName = attr(stp,"fixedEffName")
            randomEffName = attr(stp,"randomEffName")
            hasRandomEffect = attr(stp,"hasRandomEffect")
            hasCovariateEffect = attr(stp,"hasCovariateEffect")
            style = attr(stp,"style")
            closeParanthesis = FALSE
            closeOuterParanthesis = FALSE
            if ( fixedEffName != "" ) {
                if ( style == LogNormal ) {
                    statement =  paste0( name," = ", fixedEffName)
                } else if ( style == Normal ) {
                    statement =  paste0( name," = ", fixedEffName)
                } else if ( style == Combination ) {
                    statement =  paste0( name," = (", fixedEffName)
                    closeParanthesis = TRUE
                } else if ( style == Log ) {
                    statement =  paste0( name," = exp(", fixedEffName)
                    closeOuterParanthesis = TRUE
                } else if ( style == Logit ) {
                    statement =  paste0( name," = ilogit(", fixedEffName)
                    closeOuterParanthesis = TRUE
                } else if ( style == STP_CUSTOM ) {
                }
            }

# Generate covariate effects

            randomEffectUsed = FALSE
            if ( stp@isFrozen == FALSE && length(covariateList) > 0 )
            for ( indx in 1:length(covariateList) ){
                strArray=generateCovariateEffects(name,covariateList[[indx]],style)
                if ( length(strArray) != 0 ) {
                  covarEffList=attr(covariateList[[indx]],"covarEffList")
                  usageType=covarEffList[[name]]
                  type = attr(covariateList[[indx]],"type")
                  if ( type == Occasion ) {
if (  hasRandomEffect == TRUE )  {
                    randomEffectUsed = TRUE
                    if ( style == LogNormal || style == Combination ) {
                      statement = paste0(statement, " * exp(", randomEffName )
                      closeParanthesis = TRUE
                    } else
                      statement = paste0(statement, " + ", randomEffName )
}
                  }
                  for ( s in strArray ) {
                    if ( s != "" ) {
                        if ( type == Occasion ) {
                             statement= paste0( statement," + ", s )
                        } else {
                        if ( style == LogNormal ) {
                            if ( usageType ==COVAR_EFF_PLUS_ONE )
                                statement= paste0( statement," * ( ", s,") ")
                            else {

                                if ( type == Category  )
                                  statement= paste0( statement," * exp(", s,")")
                                else
                                  statement= paste0( statement," * ", s, "  ")
                            }
                        } else if ( style == Normal ) {
                            statement =  paste0( statement," + ", s)
                        } else if ( style == Combination ) {
                            statement =  paste0( statement," + ", s)
                        } else if ( style == Log ) {
                            statement =  paste0( statement," + ", s)
                        } else if ( style == Logit ) {
                            statement =  paste0( statement," + ", s)
                        } else if ( style == Custom ) {
                        }
                      }
                    }
                  }
                if ( type == Occasion  && closeParanthesis == TRUE ) {
                    statement = paste0(statement, " )"  )
                    closeParanthesis = FALSE
                }
              }
            }
            if ( style == Combination ) {
                    statement = paste0(statement, " )"  )
            }

# Generate random effect

            if (  hasRandomEffect == TRUE )  {
              if ( randomEffectUsed == FALSE ) {
                ranEffName = attr(stp,"randomEffName")
                if ( ranEffName != "" && stp@isFrozen == FALSE ) {
                  style = attr(stp,"style")
                  if ( style == LogNormal ) {
                      statement = paste0( statement , " * exp(", ranEffName,")")
                  } else if ( style == Normal ) {
                      statement =  paste0( statement , " + ", ranEffName)
                  } else if ( style == Combination ) {
                      statement= paste0( statement , "  * exp(", ranEffName,")")
                  } else if ( style == Log ) {
                      statement =  paste0( statement , " + ", ranEffName,")")
                  } else if ( style == Logit ) {
                      statement =  paste0( statement , " + ", ranEffName,")")
                  } else if ( style == Custom ) {
                  }
                }
              } else {
                if ( closeOuterParanthesis == TRUE  ) {
                    statement = paste0(statement, " )"  )
                    closeOuterParanthesis = FALSE
                }
              }
            } else {
                if ( closeOuterParanthesis == TRUE  ) {
                    statement = paste0(statement, " )"  )
                    closeOuterParanthesis = FALSE
               }
            }

            if ( style == STP_CUSTOM ) {
                for ( c in stp@code ) {
                    statement=c
                    statements=c(statements,statement)
                }
            } else {
              statement=paste0("    stparm(",statement,")")
              structuralParams[[i]]@code=statement
              statements=c(statements,statement)
            }
         }
         .Object@structuralParams = structuralParams
         .Object@statements = statements
         return(.Object)
    })
#'
#' @export
#'
setGeneric(name="generateStparmExtraCode",
           def=function(.Object)
           {
               standardGeneric("generateStparmExtraCode")
           })
#'
#' @export
#'
setMethod(f="generateStparmExtraCode",
    signature="NlmePmlModel",
    definition=function(.Object){
        statements=attr(.Object,"statements")
        structuralParams=attr(.Object,"structuralParams")
        if ( length(structuralParams ) > 0 )
        for ( i in 1:length(structuralParams)) {
            stp=structuralParams[[i]]
            name = attr(stp,"name")
            extraCode = attr(stp,"extraCode")
            if ( length(extraCode) > 0 ) {
                for ( l in extraCode )
                    statements = c(statements, l )
            }
         }
         .Object@statements = statements
         return(.Object)
    })


actionString <-function(dobefore,doafter){
  ret=""
  if ( dobefore != "" )
    ret=paste0(", dobefore = ",dobefore)
  if ( doafter != "" )
    ret=paste0(ret,", doafter = ",doafter)

  ret
}

assign("actionString",actionString,envir=.GlobalEnv)
#'
#' @export
#'
generateObserveErrorStatements <-function(effectsList){
    statements=c()
        if ( length(effectsList ) > 0 )
        for ( i in 1:length(effectsList)) {
            effect=effectsList[[i]]
            effectName = attr(effect,"effectName")
            observeName = attr(effect,"observeName")
            epsilonName = attr(effect,"epsilonName")
            errorType = attr(effect,"errorType")
            frozen = attr(effect,"frozen")
            SD = attr(effect,"SD")
            definition = attr(effect,"definition")
            isBQL = attr(effect,"isBQL")
            bqlStaticValue = attr(effect,"bqlStaticValue")
            dobefore = attr(effect,"dobefore")
            doafter = attr(effect,"doafter")
            if ( isBQL == TRUE ) {
                bql = ",bql"
                if ( bqlStaticValue != "" )
                    bql=paste0(bql,"=",bqlStaticValue)
            }
            else
                bql = ""
            if ( frozen == TRUE ) {
                freeze="(freeze)"

            }
            else
                freeze=""


            observe=""
            error=paste0("    error(",epsilonName,freeze,"=",SD,")")
            bqlStaticValue = attr(effect,"bqlStaticValue")

            if ( errorType == ERR_ADDITIVE ) {
                observe=paste0("    observe(",observeName,"=",effectName," + ",
                               epsilonName,bql,actionString(dobefore,doafter),")")
            }
            if ( errorType == ERR_LOG_ADDITIVE ) {
                observe=paste0("    observe(",observeName,"=",effectName," * exp( ",
                               epsilonName,")",bql,actionString(dobefore,doafter),")")
            }
            if ( errorType == ERR_MULTIPLICATIVE ) {
                observe=paste0("    observe(",observeName,"=",effectName," * ( 1 + ",
                               epsilonName,")",bql,actionString(dobefore,doafter),")")
            }
            if ( errorType == ERR_ADD_MULT ) {
                observe=paste0("    observe(",observeName,"=",
                                effectName," + ",
                                epsilonName, " * sqrt(1 + ",
                                effectName, "^2 * (",
                                definition,"/sigma())^2)",bql,actionString(dobefore,doafter),")")
            }
            if ( errorType == ERR_POWER ) {
                observe=paste0("    observe(",observeName,"=",
                                observeName," + ",
                                observeName, " ^(",
                                definition, ")*",
                                effectName,")",bql,actionString(dobefore,doafter),")")
            }
            if ( errorType == ERR_MIX_RATIO ) {
                if ( definition == "" )
                   definition = paste0(effectName,"MixRatio")
                observe=paste0("    observe(",observeName,"=",
                                effectName," + ",
                                epsilonName, " * (1 + ",
                                effectName, " * ",
                                definition, " )",bql,actionString(dobefore,doafter),")")
            }
            if ( errorType == ERR_CUSTOM ) {
                observe=paste0("    observe(",observeName,"=",
                                definition,bql,actionString(dobefore,doafter),")")
            }

            statements=c(statements,error)
            statements=c(statements,observe)
        }
    statements
}

#'
#' @export
#'
setGeneric(name="generateErrorStatment",
           def=function(.Object)
           {
               standardGeneric("generateErrorStatment")
           })
#'
#' @export
#'
setMethod(f="generateErrorStatment",
    signature="NlmePmlModel",
    definition=function(.Object){
        statements=attr(.Object,"statements")
        errorModel=attr(.Object,"errorModel")
        effectsList=attr(errorModel,"effectsList")

        errStatments = generateObserveErrorStatements(effectsList)

        .Object@statements = c(statements,errStatments)
         return(.Object)
    })

setGeneric(name="generateEffectsVariables",
           def=function(.Object)
           {
               standardGeneric("generateEffectsVariables")
           })
#'
#' @export
#'
setMethod(f="generateEffectsVariables",
    signature="NlmePmlModel",
    definition=function(.Object){

         oldEffects=attr(.Object,"effectsParams")
         effectsParams=c()

         names = getCovariateEffNames(.Object)
         if ( length(names) > 0 )
         for ( indx in 1:length(names) ) {
             fixedEffName = names[indx]
             hasRandomEffect= FALSE
             style = STP_PRODUCT
             initialValue="0"
             name = names[indx]
             for ( e in oldEffects ) {
                 if ( e@name == name ) {
                     fixedEffName = e@fixedEffName
                     hasRandomEffect = e@hasRandomEffect
                     initialValue = e@initialValue
                     style = e@style
                     break
                 }
             }
             param =  NlmeStructuralParameter(
                          name = name,
                          fixedEffName = name,
                          hasRandomEffect=  hasRandomEffect,
                          style = style,
                          initialValue=as.character(initialValue))


             effectsParams=c(effectsParams,param)
         }
        attr(.Object,"effectsParams")=effectsParams
        return(.Object)
})

#' listCovariateEffectNames
#'
#' Lists covariate effect names in the model
#'
#' @param model   PK/PD model
#'
#' @examples
#'    listCovariateEffectNames(model)
#'        "dVdweight"  "dVdage"     "dCldsex2"   "dCldweight"
#'
#'    scenario=NlmeScenario("scenario1", "1,3")
#'        defines a scenario for covariate effect dVdweight and dCldsex2
#'
#' @export
#'
setGeneric(name="listCovariateEffectNames",
           def=function(.Object)
           {
               standardGeneric("listCovariateEffectNames")
           })
#'
#' @export
#'
setMethod(f="listCovariateEffectNames",
    signature="NlmePmlModel",
    definition=function(.Object){
    names=c()
    for ( e in .Object@effectsParams ) {
      names=c(names,e@name)
    }
    names
})



#'
#' @export
#'
setGeneric(name="generateFixedEffStatment",
           def=function(.Object)
           {
               standardGeneric("generateFixedEffStatment")
           })
#'
#' @export
#'
setMethod(f="generateFixedEffStatment",
    signature="NlmePmlModel",
    definition=function(.Object){
#print("-------- generateFixedEffStatment()-----------")
        statements=attr(.Object,"statements")
        structuralParams=attr(.Object,"structuralParams")
        effectsParams=attr(.Object,"effectsParams")
        frozenList = c()
        if ( length(structuralParams ) > 0 )
        for ( i in 1:length(structuralParams)) {
            stp=structuralParams[[i]]
            name = attr(stp,"name")
            fixedEffName = attr(stp,"fixedEffName")
            randomEffName= attr(stp,"randomEffName")
            initialValue = attr(stp,"initialValue")
            lowerBound = attr(stp,"lowerBound")
            upperBound = attr(stp,"upperBound")
            isFrozen = attr(stp,"isFrozen")
            isSequential = attr(stp,"isSequential")
            if ( fixedEffName != "" ) {
                if ( isFrozen || isSequential){
                  freezeStatement="(freeze)"
                  frozenList = c(frozenList,randomEffName)
                }
                else
                  freezeStatement = ""
                statement =  paste0("    fixef( ",fixedEffName,
                              freezeStatement,
                              " = c(", lowerBound,",",initialValue,",",upperBound,"))")
            }
            statements=c(statements,statement)
         }
        directions = getCovariateEffDirection(.Object)
        if ( length(effectsParams ) > 0 )
        for ( i in 1:length(effectsParams)) {
            stp=effectsParams[[i]]
            name = attr(stp,"name")
            fixedEffName = attr(stp,"fixedEffName")
            initialValue = attr(stp,"initialValue")
            lowerBound = attr(stp,"lowerBound")
            upperBound = attr(stp,"upperBound")
            if ( fixedEffName != "" ) {
                statement =  paste0("    fixef( ",
                                    fixedEffName,
                                    "(enable=c(",
                                    directions[i],
                                    ")) = c(",
                                    lowerBound,
                                    ",",
                                    initialValue,
                                    ",",
                                    upperBound,
                                    "))")
            }
            statements=c(statements,statement)
        }
  #      if (length(frozenList) > 0 ) {
          if ( FALSE  ) {
          frozenStr = paste(("covariate("),paste(as.character(frozenList),collapse=", "),")")


          statements=c(statements,frozenStr)
        }
         .Object@statements = statements
         return(.Object)
    })

#'
#' @export
#'
setGeneric(name="generateSecondaryStatement",
           def=function(.Object)
           {
             standardGeneric("generateSecondaryStatement")
           })
#'
#' @export
#'
setMethod(f="generateSecondaryStatement",
          signature="NlmePmlModel",
          definition=function(.Object){
            statements=attr(.Object,"statements")
            secondary=attr(.Object,"secondaryParameters")
            for ( s in secondary ) {
              line = paste0("    secondary(",s@name,"=", s@definition, ")")
              statements = c( statements, line )
            }

            .Object@statements = statements
            return(.Object)
          })


#'
#' @export
#'
setGeneric(name="generateRanEffStatment",
           def=function(.Object)
           {
               standardGeneric("generateRanEffStatment")
           })
#'
#' @export
#'
setMethod(f="generateRanEffStatment",
    signature="NlmePmlModel",
    definition=function(.Object){
        statements=attr(.Object,"statements")
        structuralParams=attr(.Object,"structuralParams")
        randomEffectsStatements = .Object@randomEffectsStatements
        covariateStatement=""

        if ( .Object@randomValuesInitialized == FALSE ) {
          .Object = initializeRandomEffectsBlock(.Object)
        }
        if ( TRUE ) {
           statements = c(statements,randomBlockStatement(.Object))
        } else {
          if ( length(structuralParams ) > 0 )  {
            first=TRUE
            firstCovariate=TRUE
            effectStatement = ""
            effectInitialValues = ""
            for ( i in 1:length(structuralParams)) {
                stp=structuralParams[[i]]
                name = attr(stp,"name")
                fixedEffName = attr(stp,"fixedEffName")
                randomEffName = attr(stp,"randomEffName")
                initialValue = attr(stp,"initialValue")
                ranEffInitValue = attr(stp,"ranEffInitValue")
                lowerBound = attr(stp,"lowerBound")
                upperBound = attr(stp,"upperBound")
                hasRandomEffect = attr(stp,"hasRandomEffect")
                isFrozen = attr(stp,"isFrozen")
                isSequential = attr(stp,"isSequential")
                if ( randomEffName != "" && hasRandomEffect &&(isFrozen == FALSE) && ( isSequential == FALSE ) ){
                    if ( first == TRUE ) {
                        effectStatement = "    ranef(diag("
                        effectInitialValues = " c("
                        first=FALSE
                    }
                    else{
                        effectStatement=paste0(effectStatement,",")
                        effectInitialValues=paste0(effectInitialValues,",")
                    }
                    effectStatement=paste0(effectStatement,randomEffName)
                    effectInitialValues=paste0(effectInitialValues,ranEffInitValue)
#                    effectInitialValues=paste0(effectInitialValues,"1")
                }
                if ( randomEffName != "" && hasRandomEffect &&(isFrozen == FALSE) && ( isSequential == TRUE ) ){
                    if ( firstCovariate == TRUE ) {
                        covariateStatement =randomEffName
                        firstCovariate=FALSE
                    }
                    else{
                        covariateStatement=paste0(covariateStatement,",",randomEffName)
                    }
                }
             }
            if ( covariateStatement != "" ) {
                covariateStatement=paste0("    covariate(",covariateStatement,")")
                statements=c(statements,covariateStatement)
            }
            if ( effectStatement != "" ) {
                statement=paste0(effectStatement,") = ",effectInitialValues,"))")
                statements=c(statements,statement)
            }
          }
        }

        # Generate categorical covariate effect's random effect
        covariates = .Object@covariateList
        variables=c()
        for ( c in covariates ) {
            if ( c@type == Occasion ) {
                items = c@covarItems
                effects = c@covarEffList
                values = c@catEffInitValues
                names=names(effects)

                for ( indx in 1:length(names) ) {
                    value=values[[indx]]
                    name=paste0("n",names[[indx]],"x",items[[1]]@value)
                    variables=c(variables,name)

                }
                statement = paste0("    ranef(diag(",paste(as.character(variables),collapse=","),") = c(",paste(as.character(values),collapse=","),")")
                if ( length(items) > 1 ) {
                    for ( i in 2:length(items)){
                      variables=c()
                      for ( indx in 1:length(names) ) {
                        name=paste0("n",names[[indx]],"x",items[[i]]@value)
                        variables=c(variables,name)
                      }
                      statement = paste0(statement,", same(",paste(as.character(variables),collapse=","),")")
                   }

                }
                statement=paste0(statement,")")
                statements=c(statements,statement)
            }
        }

        .Object@statements = statements
        return(.Object)
    })


#' generatePMLModel
#'
#' Generates PML statements based on the current model
#'

setGeneric(name="generatePMLModel",
           def=function(.Object)
           {
               standardGeneric("generatePMLModel")
           })
#'
#' @export
#'
setMethod(f="generatePMLModel",
    signature="NlmePmlModel",
    definition=function(.Object){
        modelStatements(.Object)=list()
        modelType = attr(attr(.Object,"modelType"),"modelType" )


        frozen=FALSE
        .Object@dosePoints=list()
        pdInput = "C"
        if ( .Object@hasEffectsCompartment )
          pdInput = "Ce"
        if ( modelType == PARAM_PK ||
             modelType == PARAM_PK_EMAX ||
             modelType == PARAM_PK_INDIRECT ||
             modelType == PARAM_PK_LINEAR) {
            .Object=generatePkModel(.Object)
        }
        if ( modelType == PARAM_EMAX ||
             modelType == PARAM_PK_EMAX ){
            .Object=generateEmaxModel(.Object,pdInput)
            frozen = .Object@emaxModelAttrs@frozen
        }
        if ( modelType == PARAM_LINEAR ||
             modelType == PARAM_PK_LINEAR ){
            .Object=generateLinearModel(.Object,pdInput)
            frozen = .Object@isLinearFrozen
        }
        if ( modelType == PkIndirect ) {

            .Object=generateIndirectModel(.Object,pdInput)
            frozen = .Object@indirectModelAttrs@frozen
        }
        if ( .Object@hasEffectsCompartment ) {

          .Object = generateEffectsModel(.Object,"C",frozen)

        }
        .Object = generateEffectsVariables(.Object)
        .Object = generateErrorStatment(.Object)
        .Object = generateStparmSatement(.Object)
        .Object = generateCovariateStatement(.Object)
        .Object = generateStparmExtraCode(.Object)
        .Object = generateFixedEffStatment(.Object)
        .Object = generateRanEffStatment(.Object)
        .Object = generateSecondaryStatement(.Object)
        statements=attr(.Object,"statements")
        if ( length(statements) == 0 ) {
          statements = c(statements,"test(){")
        }
        statements=c(statements,"}")
        .Object@statements=statements
        return(.Object)
    })


setGeneric(name="generateIndirectModel",
           def=function(.Object,scInput)
           {
               standardGeneric("generateIndirectModel")
           })

setMethod(f="generateIndirectModel",
    signature="NlmePmlModel",
    definition=function(.Object,scInput){
        statements=attr(.Object,"statements")
        if ( length(statements) == 0 )
            statements=c(statements, "test(){" )

        structuralParams=attr(.Object,"structuralParams")
        dosePoints=attr(.Object,"dosePoints")
        outputParams=attr(.Object,"outputParams")
        diffEquations=attr(.Object,"diffEquations")
        attrs=attr(.Object,"indirectModelAttrs")

        type   = attrs@type
        hasEffectsCompartment   = attrs@hasEffectsCompartment
        isBuildup   = attrs@isBuildup
        isExponent   = attrs@isExponent
        frozen   = attrs@frozen


        sC = scInput
        sMax = ""
        s50 = ""

        if ( type == LIMITED_STIM  ) {
            sMax = "Emax"
            s50 = "EC50"
            if ( isExponent )  {
                s50 = paste0(s50," ^gam ")
                sC = paste0(sC," ^gam ")
            }
            sFrac = paste0( sC, " / (", sC, " + ",s50,")")
            statement = paste0(sMax, " * ", sFrac )
            statement = paste0( "(1 + ",statement, ")")


        }
        if ( type == INFINITE_STIM  ) {
            sMax = ""
            s50 = "EC50"
            sFrac = paste0("(" , sC , " / " , s50 , ")")
            if ( isExponent )  {
                sFrac = paste0(sFrac," ^ gam")
            }
            statement = sFrac
            statement = paste0("(1 + ",statement, ")")
        }
        if ( type == LIMITED_INHIB  ) {
            sMax = "Imax"
            s50 = "IC50"
            if ( isExponent )  {
                s50 = paste0(s50," ^ gam")
                sC = paste0(sC," ^ gam")
            }
            sFrac = paste0(sC , " / (" , sC , " + " , s50 , ")")
            statement = paste0(sMax , " * " , sFrac)
            statement = paste0("(1 - " , statement , ")")
        }
        if ( type == INVERSE_INHIB  ) {
            sMax = "Imax"
            s50 = "IC50"
            if ( isExponent )  {
                s50 = paste0(s50," ^ gam")
                sC = paste0(sC," ^ gam")
            }
            sFrac = paste0(sC , " / (" , sC , " + " , s50 , ")")
            statement = paste0(sMax , " * " , sFrac)
            statement = paste0("1 / (1 + " , statement , ")")
        }
        if ( type == LINEAR_STIM  ) {
            sS = "s"
            if ( isExponent )
                sC = paste0(sC," ^ gam")
            statement = paste0("(1 +  " , sS," * ",sC,")")
        }
        if ( type == LOG_LINEAR_STIM  ) {
            sS = "s"
            if ( isExponent )
                sC = paste0(sC," ^ gam")
            statement = paste0("(1 + log(1 + " , sS," * ",sC,"))")
        }
    if ( isBuildup == TRUE  ) {
        sIn = paste0("Kin * ",statement )
        sOut = "Kout"
    } else {
        sIn = "Kin"
        sOut = paste0("Kout * ",statement )
    }
    deriv = paste0("    deriv(E = ", sIn, " - ",sOut , " * E)")
    diffEquations=c(diffEquations,deriv)
    statements=c(statements, deriv)
    statement="    sequence{ E= Kin / Kout}"
    statements = c( statements,  statement )

    .Object@statements=statements
    .Object@diffEquations=diffEquations
    .Object@outputParams=outputParams
    return(.Object)
    })

assign("generateIndirectModel",generateIndirectModel,envir=.GlobalEnv)

#'
#' @export
#'
setGeneric(name="associateCovarsWithParams",
    def=function(model,covariates)
    {
        standardGeneric("associateCovarsWithParams")
    })
#'
#' @export
#'
setMethod(f="associateCovarsWithParams",
    signature="NlmePmlModel",
    definition= function(model,covariates){
    structuralParams=model@structuralParams

    if ( length(covariates) != 0 )
    for ( indx in 1:length(covariates) ) {
        c=covariates[[indx]]
        covEffList=attr(c,"covarEffList")
        covariateType=attr(c,"type")
        names=c()
        for ( s in structuralParams ) {
            if ( attr(s,"hasCovariateEffect" ) == TRUE && s@isFrozen == FALSE ) {
            name=attr(s,"name")
            names=c(names,name)
            if ( covariateType == COVAR_CONTINUOUS )
                covEffList = c(covEffList,name=COVAR_EFF_PLUS_ONE)
            else
                covEffList = c(covEffList,name=COVAR_EFF_YES)
            }
        }
        names(covEffList)=names
        attr(c,"covarEffList")=covEffList
        covariates[[indx]] = c
    }
    cov = attr(model,"covariateList")
    covariates = c(cov,covariates)
    attr(model,"covariateList") = covariates
    return(model)
})

#' addCovariates
#'
#' Adds covariates to existing model
#'
#' @param model       PK/PD model
#' @param covariates  List of covariates variables defined already
#' @param effects     Covariate/parameter list
#'
#' @examples
#'       model=addCovariates(model,
#'                           c(sex,weight,age),
#'                           c("EC0"="weight,age",
#'                           "IC50"= "age,sex"))
#'
#' @export addCovariates
#'
setGeneric(name="addCovariates",
    def=function(model,covariates,effects)
    {
        standardGeneric("addCovariates")
    })

#'
#' @export
#'
setMethod(f="addCovariates",
    signature="NlmePmlModel",
    definition= function(model,covariates,effects){
    model=associateCovarsWithParams(model,covariates)

    if ( length(effects) != 0 ) {
        names=names(effects)
        for ( n in names ) {
            covars=effects[n]
            for ( c in unlist(strsplit(covars,split=","))){
                c=trimws(c,"both")
                isPositive=FALSE
                type=CovarNumber
                for ( covar in covariates ) {
                  if ( covar@name == c ) {
                    isPositive = covar@isPositive
                    type = covar@type
                  }
                }
                if ( type == Continuous && isPositive == FALSE )
                  covariateEffect(model,c,n)=COVAR_EFF_PLUS_ONE
                else {
                  covariateEffect(model,c,n)=COVAR_EFF_YES
               }
            }
        }
        covariateList=model@covariateList
        for ( indx in 1:length(covariateList) ) {
            c=covariateList[[indx]]
            if ( c@type == Occasion ) {
                num=length(c@covarEffList)
               c@catEffInitValues=as.list(rep(1,num))
                covariateList[[indx]] = c
            }
        }
        model@covariateList=covariateList
    }
    model=generatePMLModel(model)
    return(model)
})

#' emaxmodel
#'
#' Method to create an Emax PML model
#'
#' @param isPopulation       Population vs Individual models
#' @param checkBaseline      Does EMAX have a baseline
#' @param checkFractional    Is EMAX fractional
#' @param checkInhibitory    Is EMAX inhibitory
#' @param checkSigmoid       Is EMAX sigmoid
#' @param covariates         List of covariates defined for the model
#' @param modelName          Visual name for the model
#' @param workingDir         Working directory to run the model
#'
#' @examples
#'        model=emaxmodel()
#'
#'        OR
#'
#'        model=emaxmodel(checkBaseline=TRUE,
#'                            checkFractional=TRUE,
#'                            checkInhibitory=TRUE))
#'
#'        print(attr(model,"statements"))
#'        "test(){"
#'        "    covariate(C)"
#'        "    E = E0 * (1  -  Imax * C / (IC50 + C))"
#'        "    error(EEps=1)"
#'        "    observe(EObs=E + EEps)"
#'        "    stparm(IC50 = tvIC50 * exp(nIC50))"
#'        "    stparm(E0 = tvE0 * exp(nE0))"
#'        "    stparm(Imax = tvImax * exp(nImax))"
#'        "    fixef( tvIC50 = c(,1,))"
#'        "    fixef( tvE0 = c(,1,))"
#'        "    fixef( tvImax = c(,1,))"
#'        "    ranef(diag(nIC50,nE0,nImax) =  c(1,1,1))"
#'        "}"
#'
#' @export emaxmodel
#'


emaxmodel <-function(
                           isPopulation=TRUE,
                           checkBaseline=FALSE,
                           checkFractional=FALSE,
                           checkInhibitory=FALSE,
                           checkSigmoid=FALSE,
                           covariates=c(),
                           modelName="",
                           workingDir="") {
    emaxParams=NlmeEmaxParameters(
                       checkBaseline = checkBaseline,
                       checkFractional = checkFractional,
                       checkInhibitory = checkInhibitory,
                       checkSigmoid = checkSigmoid,
                       frozen = FALSE )

    residual = NlmeResidualEffect(errorType=ERR_ADDITIVE,
                                  effectName="E")
#    residual = NlmeResidualEffect(errorType=ERR_ADD_MULT,
#                                  effectName="E")
    errorModel=NlmeErrorModel(c(residual))

    model=NlmePmlModel(modelType=PARAM_EMAX,
                       isPopulation=isPopulation,
                       emaxModelAttrs=emaxParams,
                       errorModel=errorModel,
                       modelInfo=NlmePmlModelInfo(modelName,workingDir))


    model=createEmaxStructuralParameters(model)

    structuralParams=model@structuralParams

    model=associateCovarsWithParams(model,covariates)

    model=generatePMLModel(model)
    return(model)
}
assign("emaxmodel",emaxmodel,envir=.GlobalEnv)


#' pkmodel
#'
#' Method to create a PK model
#'
#' @param isPopulation        Population vs Individual models
#' @param parameterization    Options are Clearance|Micro|Macro|Macro1
#' @param absorption          Options are Extravascular|Intravenous
#' @param numCompartments     1|2|3
#' @param isTlag              Is there a dose time lag
#' @param hasEliminationComp  Is there an elimination compartment
#' @param isFractionExcreted  Is elimination compartment fraction excreted
#' @param isSaturating        Use Michaelis-Menten kinetics
#' @param infusionAllowed     Are infusions allowed
#' @param isDuration          Do infusions use duration instead of rate
#' @param isTimeBased         Is model time-based
#' @param isSequential        Does PK model receive parameters from another model
#' @param covariates          Covariates defined for the model
#' @param modelName           Visual name for the model
#' @param workingDir          Working directory to run the model
#'
#' @examples
#'       model = pkmodel(numCompartments=2)
#'
#'       OR
#'
#'       sex=categoricalCovariate("sex",c(0,1),c("male","female"))
#'       weight=NlmeCovariateParameter("weight",centerValue=75,
#'             continuousType=COVAR_MEDIAN)
#'
#'       model=pkmodel( isPopulation=TRUE,
#'                            parameterization=Clearance,
#'                            absorption= Intravenous,
#'                            numCompartments=1,
#'                            isClosedForm=TRUE,
#'                            isTlag=FALSE,
#'                            isTimeBased=TRUE,
#'                            hasEliminationComp=FALSE,
#'                            isFractionExcreted=FALSE,
#'                            isSaturating=FALSE,
#'                            infusionAllowed=FALSE,
#'                            isDuration=FALSE,
#'                            isSequential=FALSE,
#'                            covariates=c(sex,weight))
#'
#'       print(attr(model,"statements"))
#'       test(){
#'           cfMicro(A1, Cl / V, first = (Aa = Ka))
#'           dosepoint(Aa)
#'           C = A1 / V
#'           error(CEps = 0.0942729)
#'           observe(CObs = C * (1 + CEps))
#'           stparm(Ka = tvKa * exp(nKa))
#'           stparm(V = tvV * (wt/75)^dVdwt * exp(dVdsex1*(sex==1)) * exp(nV))
#'           stparm(Cl = tvCl * exp(nCl))
#'           covariate(sex())
#'           covariate(wt)
#'           fixef(tvKa = c(, 1, ))
#'           fixef(tvV = c(, 9.6764, ))
#'           fixef(tvCl = c(, 1, ))
#'           fixef(dVdwt(enable=c(0)) = c(, 0, ))
#'           fixef(dVdsex1(enable=c(1)) = c(, 0.101632, ))
#'           ranef(diag(nV, nCl, nKa) = c(0.10608772, 0.082715079, 1))
#'       }
#'
#' @export pkmodel
#'

pkmodel <-function( isPopulation=TRUE,
                          parameterization=Clearance,
                          absorption=Intravenous,
                          numCompartments=1,
                          isClosedForm=TRUE,
                          isTlag=FALSE,
                          isTimeBased=TRUE,
                          hasEliminationComp=FALSE,
                          isFractionExcreted=FALSE,
                          infusionAllowed=FALSE,
                          isSaturating=FALSE,
                          isDuration=FALSE,
                          isSequential=FALSE,
                          covariates=c(),
                          modelName="",
                          workingDir="") {
    if ( hasEliminationComp )
      isClosedForm = FALSE
    pkParams=NlmePkParameters(
                parameterization=NlmeModelParameterization(parameterization),
                absorption=NlmeModelAbsorption(absorption),
                numCompartments=numCompartments,
                isTlag=isTlag,
                hasEliminationComp=hasEliminationComp,
                isFractionExcreted=isFractionExcreted,
                isSaturating=isSaturating,
                infusionAllowed=infusionAllowed,
                isDuration=isDuration,
                isClosedForm=isClosedForm,
                isSequential=isSequential)


    residual = NlmeResidualEffect(errorType=ERR_ADDITIVE,
                                  effectName="C",
                                  frozen = isSequential )
    if ( hasEliminationComp ) {
        a0residual = NlmeResidualEffect(errorType=ERR_ADDITIVE,
                                  effectName="A0",
                                  frozen = isSequential )
        errorModel=NlmeErrorModel(c(residual,a0residual))
    }
    else
        errorModel=NlmeErrorModel(c(residual))

    model=NlmePmlModel(modelType=PARAM_PK,
                       isPopulation=isPopulation,
                       pkModelAttrs=pkParams,
                       errorModel=errorModel,
                       modelInfo=NlmePmlModelInfo(modelName,workingDir))


    model=createPkStructuralParameters(model)

    structuralParams=model@structuralParams

    model=associateCovarsWithParams(model,covariates)

    model=generatePMLModel(model)
    return(model)
}
assign("pkmodel",pkmodel,envir=.GlobalEnv)


#' pkemaxmodel
#'
#' Method to create a PK/PD model
#'
#' @param isPopulation            Population vs Individual models
#' @param parameterization        Options are Clearance|Micro|Macro|Macro1
#' @param absorption              Options are Extravascular|Intravenous
#' @param numCompartments         1|2|3
#' @param isTlag                  Is there a dose time lag
#' @param hasEliminationComp      Is there an elimination compartment
#' @param isFractionExcreted      Is elimination compartment fraction excreted
#' @param isSaturating            Use Michaelis-Menten kinetics
#' @param infusionAllowed         Are infusions allowed
#' @param isDuration              Do infusions use duration instead of rate
#' @param isTimeBased             Is model time-based
#' @param isSequential            Does PK model receive parameters from another model
#' @param isPkFrozen              Is PK model frozen
#' @param checkBaseline           Does EMAX have a baseline
#' @param hasEffectsCompartment   Model has effect compartment
#' @param checkFractional         Is EMAX fractional
#' @param checkInhibitory         Is EMAX inhibitory
#' @param checkSigmoid            Is EMAX sigmoid
#' @param isEmaxFrozen            Is Emax model frozen
#' @param covariates              Covariates defined for the model
#' @param modelName               Visual name for the model
#' @param workingDir              Working directory to run the model
#'
#' @examples
#'       model=pkemaxmodel()
#'
#'        OR
#'
#'       model=pkemaxmodel( isPopulation=TRUE,
#'                            parameterization=Clearance,
#'                            absorption= ExtraVascular,
#'                            numCompartments=1,
#'                            isTlag=TRUE,
#'                            hasEliminationComp=TRUE,
#'                            hasEffectsComp=TRUE,
#'                            checkBaseline=TRUE))
#'
#'       print(attr(model,"statements"))
#'       test(){
#'             cfMicro(A1,Cl/V, first = (Aa = Ka))
#'             dosepoint(Aa, tlag = Tlag)
#'             C = A1 / V
#'             E = E0 + Emax * C / (EC50 + C)
#'             error(CEps=1)
#'             observe(CObs=C + CEps)
#'             error(EEps=1)
#'             observe(EObs=E + EEps)
#'             stparm(Ka = tvKa * exp(nKa))
#'             stparm(V = tvV * exp(nV))
#'             stparm(Cl = tvCl * exp(nCl))
#'             stparm(Tlag = tvTlag * exp(nTlag))
#'             stparm(EC50 = tvEC50 * exp(nEC50))
#'             stparm(E0 = tvE0 * exp(nE0))
#'             stparm(Emax = tvEmax * exp(nEmax))
#'             fixef( tvKa = c(,1,))
#'             fixef( tvV = c(,1,))
#'             fixef( tvCl = c(,1,))
#'             fixef( tvTlag = c(,1,))
#'             fixef( tvEC50 = c(,1,))
#'             fixef( tvE0 = c(,1,))
#'             fixef( tvEmax = c(,1,))
#'             ranef(diag(nKa,nV,nCl,nTlag,nEC50,nE0,nEmax) =  c(1,1,1,1,1,1,1))
#'        }
#'
#' @export pkemaxmodel
#'

pkemaxmodel <-function( isPopulation=TRUE,
                          parameterization=Clearance,
                          absorption=Intravenous,
                          numCompartments=1,
                          isClosedForm=TRUE,
                          isTlag=FALSE,
                          isTimeBased=TRUE,
                          hasEliminationComp=FALSE,
                          isFractionExcreted=FALSE,
                          infusionAllowed=FALSE,
                          isSaturating=FALSE,
                          isDuration=FALSE,
                          isSequential=FALSE,
                          isPkFrozen=FALSE,
                        hasEffectsCompartment=FALSE,
                          checkBaseline=FALSE,
                          checkFractional=FALSE,
                          checkInhibitory=FALSE,
                          checkSigmoid=FALSE,
                          isEmaxFrozen=FALSE,
                          covariates=c(),
                          modelName="",
                          workingDir="") {

    if ( hasEliminationComp )
      isClosedForm = FALSE


    emaxParams=NlmeEmaxParameters(
                       checkBaseline = checkBaseline,
                       checkFractional = checkFractional,
                       checkInhibitory = checkInhibitory,
                       checkSigmoid = checkSigmoid,
                       frozen = isEmaxFrozen )

    pkParams=NlmePkParameters(
                parameterization=NlmeModelParameterization(parameterization),
                absorption=NlmeModelAbsorption(absorption),
                numCompartments=numCompartments,
                isTlag=isTlag,
                hasEliminationComp=hasEliminationComp,
                isFractionExcreted=isFractionExcreted,
                isSaturating=isSaturating,
                infusionAllowed=infusionAllowed,
                isDuration=isDuration,
                isClosedForm=isClosedForm,
                isSequential=isSequential,
                isPkFrozen = isPkFrozen)

    pdresidual = NlmeResidualEffect(errorType=ERR_ADDITIVE,
                                  effectName="E")

    pkresidual = NlmeResidualEffect(errorType=ERR_ADDITIVE,
                                  effectName="C",
                                  frozen = isSequential || isPkFrozen)
    if ( hasEliminationComp ) {
        a0residual = NlmeResidualEffect(errorType=ERR_ADDITIVE,
                                  effectName="A0",
                                  frozen = isSequential )
        pkresidual=c(pkresidual,a0residual)
    }
     if ( isEmaxFrozen )
       errorModel = NlmeErrorModel(c(pkresidual))
    else
      errorModel=NlmeErrorModel(c(pkresidual,pdresidual))

    model=NlmePmlModel(modelType=PkEmax,
                       isPopulation=isPopulation,
                       emaxModelAttrs=emaxParams,
                       pkModelAttrs=pkParams,
                       hasEffectsCompartment=hasEffectsCompartment,
                       errorModel=errorModel,
                       modelInfo=NlmePmlModelInfo(modelName,workingDir))

    model=createPkStructuralParameters(model)
    model=createEmaxStructuralParameters(model)

    structuralParams=model@structuralParams

    model=associateCovarsWithParams(model,covariates)



    model=generatePMLModel(model)
    return(model)

}

assign("pkemaxmodel",pkemaxmodel,envir=.GlobalEnv)


#' pkindirectmodel
#'
#' Method to create a PK/Indirect PD model
#'
#' @param isPopulation           Population vs Individual models
#' @param parameterization       Options are Clearance|Micro|Macro|Macro1
#' @param absorption             Options are Extravascular|Intravenous
#' @param numCompartments        1|2|3
#' @param isTlag                 Is there a dose time lag
#' @param hasEliminationComp     Is there an elimination compartment
#' @param isFractionExcreted     Is elimination compartment fraction excreted
#' @param isSaturating           Use Michaelis-Menten kinetics
#' @param infusionAllowed        Are infusions allowed
#' @param isDuration             Do infusions use duration instead of rate
#' @param isTimeBased            Is model time-based
#' @param isSequential           Does PK model receive parameters from another model
#' @param isPkFrozen             Is PK model frozen
#' @param indirectType           Type of model: LimitedStimulation|
#'                               InfiniteStimulation|LimitedInhibition|
#'                               InverseInhibition|LinearStimulation|
#'                               LogLinearStimulation
#' @param isBuildup              Do concentration changes cause a Buildup (TRUE)
#'                               or Loss (FALSE)
#' @param hasEffectsCompartment  Model has effect compartment
#' @param isExponent             Is an exponent required
#' @param indirectFrozen         Is Indirect model frozen
#' @param covariates             List of covariates defined for the model
#' @param modelName              Visual name for the model
#' @param workingDir             Working directory to run the model
#'
#' @examples
#'       pkpdmodel = pkindirectmodel(numComp=2,
#'                            absorption = Intravenous,
#'                            isClosedForm = FALSE,
#'                            isSequential = FALSE  ,
#'                            hasEliminationComp = FALSE,
#'                            indirectType=LimitedStimulation,
#'                            isBuildup=TRUE,
#'                            hasEffectsCompartment = TRUE,
#'                            modelName="PkIndirectModel")
#'
#'       print(attr(model,"statements"))
#'       test(){
#'           deriv(A1 = - Cl * C - Cl2 * (C - C2))
#'           deriv(A2 = Cl2 * (C - C2))
#'           deriv(Ce = Ke0*(C - Ce))
#'           deriv(E = Kin * (1 + Emax * Ce / (Ce + EC50)) - Kout * E)
#'           dosepoint(A1)
#'           C = A1 / V
#'           C2 = A2 / V2
#'           sequence{E = Kin / Kout}
#'           error(CEps = 1)
#'           observe(CObs = C + CEps)
#'           error(EEps = 1)
#'           observe(EObs = E + EEps)
#'           stparm(V = tvV * exp(nV))
#'           stparm(V2 = tvV2 * exp(nV2))
#'           stparm(Cl = tvCl * exp(nCl))
#'           stparm(Cl2 = tvCl2 * exp(nCl2))
#'           stparm(Ke0 = tvKe0 * exp(nKe0))
#'           stparm(Kin = tvKin * exp(nKin))
#'           stparm(Kout = tvKout * exp(nKout))
#'           stparm(Emax = tvEmax * exp(nEmax))
#'           stparm(EC50 = tvEC50 * exp(nEC50))
#'           fcovariate(Age)
#'           fcovariate(BodyWeight)
#'           fcovariate(Gendercode())
#'           fixef(tvV = c(, 15.3918, ))
#'           fixef(tvV2 = c(, 41.2013, ))
#'           fixef(tvCl = c(, 6.60942, ))
#'           fixef(tvCl2 = c(, 14.038, ))
#'           fixef(tvKe0 = c(, 1, ))
#'           fixef(tvKin = c(, 1, ))
#'           fixef(tvKout = c(, 1, ))
#'           fixef(tvEmax = c(, 1, ))
#'           fixef(tvEC50 = c(, 1, ))
#'           ranef(diag(nV, nCl, nV2, nCl2, nKin, nKout,
#'                      nEmax, nEC50, nKe0) = c(0.06931719,
#'                      0.18120092, 3.0838168E-09,
#'                      0.042620177, 1, 1, 1, 1, 1))
#'           }
#'
#' @export pkindirectmodel
#'

pkindirectmodel <-function( isPopulation=TRUE,
                          parameterization=Clearance,
                          absorption=Intravenous,
                          numCompartments=1,
                          isClosedForm=TRUE,
                          isTlag=FALSE,
                          isTimeBased=TRUE,
                          hasEliminationComp=FALSE,
                          isFractionExcreted=FALSE,
                          infusionAllowed=FALSE,
                          isSaturating=FALSE,
                          isDuration=FALSE,
                          isSequential=FALSE,
                          isPkFrozen=FALSE,
                          indirectType=LIMITED_STIM,
                          hasEffectsCompartment = FALSE,
                          isBuildup=TRUE,
                          isExponent=FALSE,
                          indirectFrozen=FALSE,
                          covariates=c(),
                          modelName="",
                          workingDir="") {

    if ( hasEliminationComp )
      isClosedForm = FALSE
    indirectParams=NlmeIndirectParameters(
                       type = indirectType,
                       hasEffectsCompartment = hasEffectsCompartment,
                       isBuildup = isBuildup,
                       isExponent = isExponent,
                       frozen = indirectFrozen)
    pkParams=NlmePkParameters(
                parameterization=NlmeModelParameterization(parameterization),
                absorption=NlmeModelAbsorption(absorption),
                numCompartments=numCompartments,
                isTlag=isTlag,
                hasEliminationComp=hasEliminationComp,
                isFractionExcreted=isFractionExcreted,
                isSaturating=isSaturating,
                infusionAllowed=infusionAllowed,
                isDuration=isDuration,
                isClosedForm=isClosedForm,
                isSequential=isSequential,
                isPkFrozen=isPkFrozen)

    pdresidual = NlmeResidualEffect(errorType=ERR_ADDITIVE,
                                  effectName="E")

    pkresidual = NlmeResidualEffect(errorType=ERR_ADDITIVE,
                                  effectName="C",
                                  frozen = isSequential || isPkFrozen)
    if ( hasEliminationComp ) {
        a0residual = NlmeResidualEffect(errorType=ERR_ADDITIVE,
                                  effectName="A0",
                                  frozen = isSequential )
        pkresidual=c(pkresidual,a0residual)
    }
    if ( indirectFrozen )
      errorModel = NlmeErrorModel(c(pkresidual))
    else
      errorModel=NlmeErrorModel(c(pkresidual,pdresidual))
    model=NlmePmlModel(modelType=PkIndirect,
                       isPopulation=isPopulation,
                       indirectModelAttrs=indirectParams,
                       pkModelAttrs=pkParams,
                       hasEffectsCompartment=hasEffectsCompartment,
                       errorModel=errorModel,
                       modelInfo=NlmePmlModelInfo(modelName,workingDir))

    model=createPkStructuralParameters(model)
    model=createIndirectStructuralParameters(model)

    structuralParams=model@structuralParams

    model=associateCovarsWithParams(model,covariates)



    model=generatePMLModel(model)
    return(model)

}

assign("pkindirectmodel",pkindirectmodel,envir=.GlobalEnv)



#' pklinearmodel
#'
#' Method to create a PK/PD model
#'
#' @param isPopulation         Population vs Individual models
#' @param parameterization     Options are Clearance|Micro|Macro|Macro1
#' @param absorption           Options are Extravascular|Intravenous
#' @param numCompartments      1|2|3
#' @param isTlag               Is there a dose time lag
#' @param hasEliminationComp   Is there an elimination compartment
#' @param isFractionExcreted   Is elimination compartment fraction excreted
#' @param isSaturating         Use Michaelis-Menten kinetics
#' @param infusionAllowed      Are infusions allowed
#' @param isDuration           Do infusions use duration instead of rate
#' @param isTimeBased          Is model time-based
#' @param isSequential         PK model receive parameters from another model
#' @param isPkFrozen           Is PK model frozen
#' @param linearType           One of LinearAlpha|LinearBeta|LinearGamma
#' @param isLinearFrozen       Is Linear Model frozen
#' @param covariates           List of covariates defined for the model
#' @param modelName            Visual name for the model
#' @param workingDir           Working directory to run the model
#'
#' @examples
#'       model=pklinearmodel()
#'
#'        OR
#'
#'       model=pklinearmodel( isPopulation=TRUE,
#'                            parameterization=Clearance,
#'                            absorption= ExtraVascular,
#'                            numCompartments=1,
#'                            isTlag=TRUE,
#'                            hasEliminationComp=TRUE,
#'                            linearModelType=LinearBeta))
#'
#'       print(attr(model,"statements"))
#'       test(){
#'      	deriv(Aa = - Ka * Aa)
#'      	deriv(A1 = Ka * Aa - Cl * C)
#'      	urinecpt(A0 = Cl * C)
#'      	dosepoint(Aa, tlag = Tlag)
#'      	C = A1 / V
#'      	E = Alpha + Beta*C
#'      	error(EEps = 1)
#'      	observe(EObs = E + EEps)
#'      	error(CEps = 1)
#'      	observe(CObs = C + CEps)
#'      	error(A0Eps = 1)
#'      	observe(A0Obs = A0 + A0Eps)
#'      	stparm(Ka = tvKa * exp(nKa))
#'      	stparm(V = tvV * exp(nV))
#'      	stparm(Cl = tvCl * exp(nCl))
#'      	stparm(Tlag = tvTlag * exp(nTlag))
#'      	stparm(Alpha = tvAlpha * exp(nAlpha))
#'      	stparm(Beta = tvBeta * exp(nBeta))
#'      	fixef(tvKa = c(, 1, ))
#'      	fixef(tvV = c(, 1, ))
#'      	fixef(tvCl = c(, 1, ))
#'      	fixef(tvTlag = c(, 1, ))
#'      	fixef(tvAlpha = c(, 1, ))
#'      	fixef(tvBeta = c(, 1, ))
#'      	ranef(diag(nAlpha, nBeta, nV, nCl, nTlag, nKa) = c(1, 1, 1, 1, 1, 1))
#'        }
#'
#' @export pklinearmodel
#'

pklinearmodel <-function( isPopulation=TRUE,
                          parameterization=Clearance,
                          absorption=Intravenous,
                          numCompartments=1,
                          isClosedForm=TRUE,
                          isTlag=FALSE,
                          isTimeBased=TRUE,
                          hasEliminationComp=FALSE,
                          isFractionExcreted=FALSE,
                          infusionAllowed=FALSE,
                          isSaturating=FALSE,
                          isDuration=FALSE,
                          isSequential=FALSE,
                          isPkFrozen=FALSE,
                          linearType=LinearAlpha,
                          isLinearFrozen=FALSE,
                          hasEffectsCompartment=FALSE,
                          covariates=c(),
                          modelName="",
                          workingDir="") {


    if ( hasEliminationComp )
      isClosedForm = FALSE
    pkParams=NlmePkParameters(
                parameterization=NlmeModelParameterization(parameterization),
                absorption=NlmeModelAbsorption(absorption),
                numCompartments=numCompartments,
                isTlag=isTlag,
                hasEliminationComp=hasEliminationComp,
                isFractionExcreted=isFractionExcreted,
                isSaturating=isSaturating,
                infusionAllowed=infusionAllowed,
                isDuration=isDuration,
                isClosedForm=isClosedForm,
                isSequential=isSequential,
                isPkFrozen = isPkFrozen)

    pdresidual = NlmeResidualEffect(errorType=ERR_ADDITIVE,
                                  effectName="E")

    pkresidual = NlmeResidualEffect(errorType=ERR_ADDITIVE,
                                  effectName="C",
                                  frozen = isSequential || isPkFrozen)
    if ( hasEliminationComp ) {
        a0residual = NlmeResidualEffect(errorType=ERR_ADDITIVE,
                                  effectName="A0",
                                  frozen = isSequential )
        pkresidual=c(pkresidual,a0residual)
    }
    if ( isLinearFrozen )
      errorModel = NlmeErrorModel(c(pkresidual))
    else
      errorModel=NlmeErrorModel(c(pkresidual,pdresidual))

    model=NlmePmlModel(modelType=PkLinear,
                       isPopulation=isPopulation,
                       linearModelType=linearType,
                       isLinearFrozen = isLinearFrozen,
                       pkModelAttrs=pkParams,
                       hasEffectsCompartment=hasEffectsCompartment,
                       errorModel=errorModel,
                       modelInfo=NlmePmlModelInfo(modelName,workingDir))

    model=createPkStructuralParameters(model)
    model=createLinearStructuralParameters(model)

    structuralParams=model@structuralParams

    model=associateCovarsWithParams(model,covariates)



    model=generatePMLModel(model)
    return(model)

}

assign("pklinearmodel",pklinearmodel,envir=.GlobalEnv)



#' linearModel
#'
#' Method to create a Linear PML model
#'
#' @param isPopulation   Population vs Individual models
#' @param type           Type of Linear model
#'                       LinearAlpha|LinearBeta|LinearGamma
#' @param covariates     List of covariates defined for the model
#' @param modelName      Visual name for the model
#' @param workingDir     Working directory to run the model
#'
#' @examples
#'        model=linearmodel()
#'
#'        OR
#'
#'        model=linearmodel(type=LinearBeta,
#'                          modelName="Linear",
#'                          workingDire="./"))
#'
#'        print(attr(model,"statements"))
#'
#'        "test(){"
#'        "	covariate(C)"
#'        "	E = Alpha + Beta*C"
#'        "	error(EEps = 1)"
#'        "	observe(EObs(C) = E + EEps)"
#'        "	stparm(Alpha = tvAlpha * exp(nAlpha))"
#'        "	stparm(Beta = tvBeta * exp(nBeta))"
#'        "	fixef(tvAlpha = c(, 1, ))"
#'        "	fixef(tvBeta = c(, 1, ))"
#'        "	ranef(diag(nAlpha, nBeta) = c(1, 1))"
#'        "}"
#'
#' @export linearmodel
#'


linearmodel <-function(
  isPopulation=TRUE,
  type=LinearAlpha,
  covariates=c(),
  modelName="",
  workingDir="") {


    residual = NlmeResidualEffect(errorType=ERR_ADDITIVE,
                                  effectName="E")
    errorModel=NlmeErrorModel(c(residual))

    model=NlmePmlModel(modelType=PARAM_LINEAR,
                       isPopulation=isPopulation,
                       linearModelType = type,
                       errorModel=errorModel,
                       isTimeBased = FALSE,
                       modelInfo=NlmePmlModelInfo(modelName,workingDir))


    model=createLinearStructuralParameters(model)

    structuralParams=model@structuralParams

    model=associateCovarsWithParams(model,covariates)

    model=generatePMLModel(model)
    return(model)
}
assign("linearmodel",linearmodel,envir=.GlobalEnv)




#'
#' @export
#
addEmaxModel <-function(model,
                           emaxParams = NlmeEmaxParameters() ,
                           isPkFrozen = TRUE ) {

    covariates=attr(model,"covariateList")
    attr(model,"emaxModelAttrs")= emaxParams


    attr(model,"modelType") = NlmeModelType(PARAM_PK_EMAX)

    model=createEmaxStructuralParameters(model)

    structuralParams=model@structuralParams

    resEffect = NlmeResidualEffect(errorType = ERR_ADDITIVE,
                                   effectName = "E")
    model = addToErrorModel(model,c(resEffect))

#    model=associateCovarsWithParams(model,covariates)

    model=generatePMLModel(model)

    return(model)
}

assign("addEmaxModel",addEmaxModel,envir=.GlobalEnv)



#'
#' Method to create an empty model
#'
#' @param  modelName    Visual name for the model
#' @param  workingDir   working directory to run the model
#'
#' @examples
#'
#'       model = model()
#'
#' @export
#'

blankmodel <-function( modelName="",
                  workingDir="",
                  isPopulation=TRUE) {


  isClosedForm = FALSE

  model=NlmePmlModel(modelType=Blank,
                     modelInfo=NlmePmlModelInfo(modelName,workingDir))

  model@isPopulation=isPopulation
  model@isObjectModel= TRUE
  model@objects=list()
  model=generatePMLModel(model)
  return(model)
}

assign("blankmodel",blankmodel,envir=.GlobalEnv)

#'
#' Method to create a model from existing files generated by phoenix
#'
#' @param  modelName    Visual name for the model
#' @param  dataset      Dataset representing phoenix generated model files
#'
#' @examples
#'
#'       dataset = NlmeDataset(phoenixSourceDir="C:/PhoenixFiles/PkModel")
#'       model = phxmodel(modelName="Test",
#'                        dataset = dataset )
#'       job = fitmodel(host,dataset)
#'
#' @export
#'

phxmodel <-function( modelName="",
                     dataset) {


  isClosedForm = FALSE

  model=NlmePmlModel(modelType=PhoenixModel,
                     modelInfo=NlmePmlModelInfo(modelName,""))

  model@isTextual = TRUE

  model = copyPhoenixFiles(model,dataset)

  return(model)
}

assign("blankmodel",blankmodel,envir=.GlobalEnv)


#'
#' Method to create column mapping file
#'
#' @param model       Model to be mapped
#' @param fileName    Name of file containing column mapping
#'
#' @examples
#'
#' @export generateColumnMappingFile
#'

generateColumnMappingFile<-function(model,fileName){


    on = observationNames(model)
    en = observationExtraNames(model)
    cn = covariateNames(model)
    dn = doseNames(model)
    colmap = modelColumnMapping(model)
    map= attr(colmap,"mapping")
    append=FALSE
    for ( cm in map ) {
        varName=cm@variableName
        colName=cm@columnName
        if ( colName != "?" ) {
            if ( any(sapply(dn,grepl,varName)) == TRUE ) {
                s=sprintf("covr(%s<-\"%s\")",varName,colName)
            }
            else if ( any(sapply(on,grepl,varName)) == TRUE ) {
                bqlName=paste0(varName,"BQL")
                if ( length(en) > 0 && any(sapply(en,grepl,bqlName)) == TRUE )
                    s=sprintf("obs(%s<-\"%s\", bql <- \"%s\") ",varName,colName,bqlName)
                else
                    s=sprintf("obs(%s<-\"%s\")",varName,colName)
            }
            else if ( any(sapply(cn,grepl,varName)) == TRUE ) {
                s=sprintf("covr(%s<-\"%s\")",varName,
                    covariateToString(getCovariate(model,varName)))
            }
            else {
                s=sprintf("%s(\"%s\")",varName,colName)
            }
            cat(s,file=fileName,sep="\n",append=append)
            append=TRUE
        }
    }
}

assign("generateColumnMappingFile",generateColumnMappingFile,envir=.GlobalEnv)




#'
#' @export
#'
setGeneric(name="writeModelStatements",
    def=function(.Object,filename)
    {
               standardGeneric("writeModelStatements")
    })
#'
#' @export
#'
setMethod(f="writeModelStatements",
    signature="NlmePmlModel",
    definition=function(.Object,filename){
        statements=attr(.Object,"statements")
        append=FALSE
        workingDir = .Object@modelInfo@workingDir
        for ( s in statements ) {
            cat(s,file=paste0(workingDir,"/",filename),sep="\n",append=append)
            append=TRUE
        }
    })




#'
#' @export
#'
setGeneric(name="covariateModel",
           def=function(.Object)
           {
               standardGeneric("covariateModel")
           })

#'
#' @export
#'
setMethod(f="covariateModel",
    signature="NlmePmlModel",
    definition=function(.Object){
        cvEffects=c()
        df=c()
        scenarios=c()
        isDefaultList=c()
        covariateList=attr(.Object,"covariateList")
        for ( c in covariateList ) {
            name=attr(c,"name")
            type= attr(c,"type")
            degreesOfFreedom = 1
            if ( type == COVAR_CONTINUOUS ) {
                degreesOfFreedom = 1
            }
            if ( type == Category ) {
                degreesOfFreedom = length(attr(c,"covarItems")) - 1
            }

            effectsList=attr(c,"covarEffList")
            if ( length(effectsList) > 0 ) {
                indx=1
                for ( n in names(effectsList ) ) {
                    if ( effectsList[[indx]] != COVAR_EFF_NO ) {
                    str=paste0(n,"-",name)
                    cvEffects=c(cvEffects,str)
                    scenarios=c(scenarios,"S")
                    isDefaultList=c(isDefaultList,"1")
                    df=c(df,degreesOfFreedom)
                    }
                    indx = indx+1
                }
            }
        }
        cm = NlmeCovariateEffectModel(paste(cvEffects,collapse=","),
                                      paste(scenarios,collapse=","),
                                      paste(isDefaultList,collapse=","),
                                      paste(df,collapse=","))
        return(cm)
    })
assign("covariateModel",covariateModel,envir=.GlobalEnv)


#' getThetas
#'
#' Gets names and values of Thetas by parsing PML model for fixef()
#'
#' @param model    PK/PD model
#'
#' @examples
#'       getThetas(pkpdmodel)
#'             tvKa    tvV   tvCl tvEC50   tvE0 tvEmax
#'              "1"    "1"    "1"  "1"     "1"    "1"
#'
#' @export
#'
setGeneric(name="getThetas",
           def=function(.Object)
           {
             standardGeneric("getThetas")
           })
setMethod(f="getThetas",
          signature="NlmePmlModel",
          definition=function(.Object){
            statements=attr(.Object,"statements")
            thetas=list()
            lineIndexes=grep("fixef\\(",.Object@statements)
            for ( indx in lineIndexes ) {
              line=.Object@statements[[indx]]
              tokens=unlist(strsplit(line,"fixef(",fixed=TRUE))
              name=unlist(strsplit(tokens[[2]],"=",fixed=TRUE))[[1]]
              name=unlist(strsplit(name,"(",fixed=TRUE))[[1]]
              nam=trimws(name,"both")
              val=unlist(strsplit(tokens[[2]],",",fixed=TRUE))[[2]]
              thetas[[name]]=val
            }

            return(thetas)
          })
assign("getThetas",getThetas,envir=.GlobalEnv)


#'
#' @export
#'
updateFixedEffectStr <-function (line, values){

    newLine = line
    tokens = unlist(strsplit(line,split="\\("))
    fixEffName = trimws(unlist(strsplit(tokens[[2]],split="="))[[1]],"both")
    if ( ! is.na(values[fixEffName]) ) {
        value = as.numeric(values[fixEffName])
        newValue = sub("[^,]+[^,]+",value,trimws(tokens[[3]],"left"))
        tokens[[3]] = newValue
        newLine = paste(tokens,collapse="(")
    }
    newLine
}

assign("updateFixedEffectStr",updateFixedEffectStr,envir=.GlobalEnv)

#'
#' @export
#'
getFixedEffects <-function(lines){
    pos=grep("fixef\\(",lines)
    estimates=c()
    if ( length(pos) > 0 )
    for (p in pos ) {
        line = lines[[p]]
        tokens = unlist(strsplit(line,split="\\("))
        fixEffName = trimws(unlist(strsplit(tokens[[2]],split="="))[[1]],"both")
        value = trimws(unlist(strsplit(tokens[[3]],split=","))[[2]])
         estimates[fixEffName] = value
    }
    estimates
}


assign("getFixedEffects",getFixedEffects,envir=.GlobalEnv)





#' initFixedEffects
#'
#' Prints/sets the initial values for fixed effects.
#'
#' @param .Object    PK/PD model
#'
#' @examples
#'       initFixedEffects(pkpdmodel)
#'             tvKa    tvV   tvCl tvEC50   tvE0 tvEmax
#'              "1"    "1"    "1"  "1"     "1"    "1"
#'
#'       initFixedEffects(model) = c(tvIC50=0.987,tvE0=322,tvImax=0)
#'
#' @export initFixedEffects
#'
setGeneric(name="initFixedEffects",
           def=function(.Object)
           {
               standardGeneric("initFixedEffects")
           })
setMethod(f="initFixedEffects",
    signature="NlmePmlModel",
    definition=function(.Object){
    structuralParams=attr(.Object,"structuralParams")
    effectsParams=attr(.Object,"effectsParams")
    estimates=c()
    names=c()
    if ( .Object@isTextual )  {
        statements = .Object@statements
        estimates = getFixedEffects(statements)
    } else {
      if ( length(structuralParams ) > 0 )
        for ( i in 1:length(structuralParams)) {
            stp=structuralParams[[i]]
            name = attr(stp,"name")
            fixedEffName = attr(stp,"fixedEffName")
            initialValue = attr(stp,"initialValue")
            lowerBound = attr(stp,"lowerBound")
            upperBound = attr(stp,"upperBound")
            if ( fixedEffName != "" ) {
                estimates=c(estimates,initialValue)
                names=c(names,fixedEffName)
            }
         }
        if ( length(effectsParams ) > 0 )
        for ( i in 1:length(effectsParams)) {
            stp=effectsParams[[i]]
            name = attr(stp,"name")
            fixedEffName = attr(stp,"fixedEffName")
            initialValue = attr(stp,"initialValue")
            lowerBound = attr(stp,"lowerBound")
            upperBound = attr(stp,"upperBound")
            if ( fixedEffName != "" ) {
                estimates=c(estimates,initialValue)
                names=c(names,fixedEffName)
            }
         }
        names(estimates)=names
    }
    return(estimates)
})
assign("initFixedEffects",initFixedEffects,envir=.GlobalEnv)



#'
#' initFixedEffects
#'
#' Assigns the initial values for fixed effects
#' It should be used after all model components have been created.
#'
#' @param model   PK/PD model
#'
#' @examples
#'       initFixedEffects(model) = c(tvIC50=0.987,tvE0=322,tvImax=0)
#'
#' @export
#'
setGeneric(name="initFixedEffects<-",
           def=function(.Object,value)
           {
               standardGeneric("initFixedEffects<-")
           })

setMethod(f="initFixedEffects<-",
    signature="NlmePmlModel",
    definition=function(.Object,value){
    effectsParams=attr(.Object,"effectsParams")
        sps = attr(.Object,"structuralParams")
    if ( .Object@isTextual )  {
#        stop("Model is textual, view/edit manually")
        statements = .Object@statements
        pos=grep("fixef\\(",statements)
        if ( length(pos) != 0 ) {
            for ( p in pos ) {
                ret = updateFixedEffectStr(statements[[p]],
                                                  value)
                statements[[p]] = ret
            }
            .Object@statements = statements
        }
    }
    else {
      if ( length(sps ) > 0 )  {
        for ( i in 1:length(sps)) {
            sp=sps[[i]]
            name = attr(sp,"name")
            fixedEffName = attr(sp,"fixedEffName")
            if( ! is.na(value[fixedEffName]) ) {
                attr(sp,"initialValue") = as.character(value[fixedEffName])
            }
            extraCode = sp@extraCode
            if ( length(extraCode) != 0  ) {
                pos=grep("fixef\\(",extraCode)
                if ( length(pos) != 0 ) {
                    for ( indx in 1:length(pos) ) {
                       ret = updateFixedEffectStr(extraCode[[pos[[indx]]]],
                                                  value)
                       extraCode[[indx]] = ret
                    }
                }
                sp@extraCode = extraCode
            }
            sps[[i]]=sp
         }
     }
    if ( .Object@isObjectModel ) {
        for ( indx in 1:length(.Object@objects ) ) {
           obj=.Object@objects[[indx]]
#           newObj=initObjFixedEffects(obj,value)
#           .Object@objects[[indx]]= obj
        }
    }
    if ( length(effectsParams ) > 0 )  {
        for ( i in 1:length(effectsParams)) {
            sp=effectsParams[[i]]
            name = attr(sp,"name")
            fixedEffName = attr(sp,"fixedEffName")
            if( ! is.na(value[fixedEffName]) ) {
            attr(sp,"initialValue") = as.character(value[fixedEffName])
            }
            effectsParams[[i]]=sp
         }
     }
     attr(.Object,"structuralParams") = sps
     attr(.Object,"effectsParams") = effectsParams
    }
     .Object=generatePML(.Object)
     return(.Object)
})




#' copyPhoenixFiles
#'
#' Copy Phoenix model/data files into the model directory
#'
#' @param model     PK/PD model
#' @param dataset   Dataset to run simulation/fitting on
#'
#' @export
#'
copyPhoenixFiles <-function(model,dataset){

  if ( !dir.exists(model@modelInfo@workingDir ) ) {
    dir.create(model@modelInfo@workingDir)
  }
  setwd(model@modelInfo@workingDir)
  isTextual=attr(model,"isTextual")
  phoenixSourceDir=dataset@phoenixSourceDir
  if ( isTextual == TRUE &&
       model@modelType@modelType == PhoenixModel &&
       phoenixSourceDir !="") {
    mf=attr(dataset,"modelFile")
    cf=attr(dataset,"colDefFile")
    df=attr(dataset,"dataFile")
    pf=attr(dataset,"engineParamsFile")
    files = c(mf,cf,df,pf)
    estF = dataset@estimatesDataFile
    estDef = dataset@estimatesDefFile
    if ( estF != "" ) {
      files=c(files,estF,estDef)
    }
    doseF = dataset@doseDataFile
    doseDef = dataset@doseDefFile
    if ( doseF != "" )
      files=c(files,doseF,doseDef)
    ranF = dataset@ranEffectDataFile
    ranDef = dataset@ranEffectDefFile
    if ( ranF != "" )
      files=c(files,ranF,ranDef)

    for ( f in files ) {
      from=paste0(phoenixSourceDir,"/",f)
      to = paste0(model@modelInfo@workingDir)
      file.copy(from,to,overwrite = TRUE)
    }
    statements=readLines(mf)
    model@statements = as.list(statements)

  }
  model
}
assign("copyPhoenixFiles",copyPhoenixFiles,envir=.GlobalEnv)



#' writeDefaultFiles
#'
#' Writes out data/column mapping and engine parameter files for the engine
#'
#' @param model     PK/PD model
#' @param dataset   Dataset to run simulation/fitting on
#' @param simParams Simulation Parameters
#'
#' @export
#'
writeDefaultFiles <-function(model,dataset,simParams=NULL){

  isTextual=attr(model,"isTextual")
  if ( isTextual == FALSE ) {
    mf=attr(dataset,"modelFile")
    cf=attr(dataset,"colDefFile")
    df=attr(dataset,"dataFile")
    writeInputData(model,df)
    writeColumnMapping(model,cf)
    writeModelStatements(model,mf)
    if ( ! is.null(simParams) ) {
      addTablesToColumnMapping(model,simParams,cf)
    }
    if ( length(model@doseMapping@mapping) != 0 )
        writeDoseMapping(model,dataset)
    if ( length(model@paramsMapping@mapping) != 0 )
        writeParamsMapping(model,dataset)
    if ( length(model@randParamsMapping@mapping) != 0 )
        writeRandParamsMapping(model,dataset)
  }
}
assign("writeDefaultFiles",writeDefaultFiles,envir=.GlobalEnv)


#' defaultDataset
#'
#' Formats column names of a data table and writes it out
#' to given file name
#'
#' @param model     A PK/PD model
#' @param input     Data frame with subject/observation data
#'
#' @return dataset  NLME dataset class that links model to input
#'
#' @examples
#'   dataset = defaultDataset(model,input)
#'
#' @export defaultDataset
#'
defaultDataset <-function(model,inputFile){


library("Certara.NLME8")
    dataset=NlmeDataset(model@modelInfo@workingDir)
    dataset
}

assign("defaultDataset",defaultDataset,envir=.GlobalEnv)




#'
#' Formats column names of a data table and writes it out
#' to given file name
#'
#' @export
#'
updateDatasetDosing <-function(model,dataset){

    dataset@doseDefFile = "cols2.txt"
    dataset@doseDataFile = "data2.txt"
    dataset
}

assign("updateDatasetDosing",updateDatasetDosing,envir=.GlobalEnv)

#'
#' Formats column names of a data table and writes it out
#' to given file name
#'
#' @export
#'
updateDatasetParams <-function(model,dataset){

    dataset@estimatesDefFile = "cols3.txt"
    dataset@estimatesDataFile = "data3.txt"
    dataset
}

assign("updateDatasetParams",updateDatasetParams,envir=.GlobalEnv)

#'
#' Formats column names of a data table and writes it out
#' to given file name
#'
#' @export
#'
updateDatasetRandParams <-function(model,dataset){

    dataset@ranEffectDefFile = "cols4.txt"
    dataset@ranEffectDataFile = "data4.txt"
    dataset
}

assign("updateDatasetRandParams",updateDatasetRandParams,envir=.GlobalEnv)




#'
#' Copies a model into a new object, new working directory is created
#' and all files are copied into it
#'
#' @export
#'
copyModel <-function(model,modelName="",workingDir=""){

    oldWorkingDir = model@modelInfo@workingDir
    oldModelName = model@modelInfo@modelName
#    if ( file.exists("dmp.txt") ) {
#        source("dmp.txt")
#    }
    if ( workingDir == "" ) {
        setwd(dirname(oldWorkingDir))
    }
    modelInfo = NlmePmlModelInfo(modelName,workingDir)
    modelName=modelInfo@modelName
    workingDir=modelInfo@workingDir
    model@dataset@workingDir = workingDir

    if ( !dir.exists(workingDir ) ) {
        dir.create(workingDir)
    }
#    files = list.files(path=oldWorkingDir,full.names=FALSE)
    files = c("test.mdl","cols1.txt","data1.txt","cols2.txt","data2.txt",
             "cols3.txt","data3.txt","cols4.txt","data4.txt")
    for ( f in files ) {
        file.copy(paste(oldWorkingDir,f,sep="/"),
                  paste(workingDir,f,sep="/"),overwrite=TRUE)
    }
    if ( exists(paste0(workingDir,"/dmp.txt" )) ) {
        model=generatePML(model)
    }
    newModel = model
    newModel@modelInfo=modelInfo
    newModel
}

assign("copyModel",copyModel,envir=.GlobalEnv)

#' setModel
#'
#' Changes the current working directory to model's directory
#'
#' @param model    PK/PD model
#'
#' @examples
#'       setModel(pkmodel)
#'
#' @export
#'
setModel <-function(model){
    setwd(model@modelInfo@workingDir)
    if ( file.exists("dmp.txt" ) )
        source("dmp.txt")
}

assign("setModel",setModel,envir=.GlobalEnv)

#' editModel
#'
#' Allows user to edit PML text and column definition.  It will return a new textual model
#' containing the edited model.
#'
#' @param model     A PK/PD model
#' @param dataset   Dataset
#'
#' @examples
#'
#'       newModel = editModel(model,dataset)
#'
#' @export
#'
editModel <-function(model,dataset){
  mf=attr(dataset,"modelFile")
  cf=attr(dataset,"colDefFile")
  utils::file.edit(name=mf,editor="internal")
  utils::file.edit(name=cf,editor="internal")
  model@isTextual = TRUE
#  statements=readLines(dataset@modelFile)
#  model@statements = statements
  return(model)
}
assign("editModel",editModel,envir=.GlobalEnv)

#'
#' @export
#'
refreshModel <-function(model,dataset){
  mf=attr(dataset,"modelFile")
  cf=attr(dataset,"colDefFile")
  statements = readLines(mf)
  model@statements = as.list(statements)
  return(model)
}
assign("refreshModel",refreshModel,envir=.GlobalEnv)


#' saveModel
#'
#' Saves the model object in model's working directory
#'
#' @param model    PK/PD model
#' @param dataset  Optional dataset to save with model
#' @param engine   Optional engine parameters to save with model
#' @param host     Optional host to save with model
#'
#' @examples
#'       saveModel(pkmodel)
#'
#'       model=loadModel(directoryPath)
#'
#' @export
#'
saveModel <-function(model,dataset=NULL,engine=NULL,host=NULL){

  cwd=getwd()
  if ( ! is.null(dataset ) )
    model@dataset = dataset
  if ( ! is.null(engine ))
    model@engine = engine
  if ( ! is.null(host ) )
    model@host = host
  setwd(model@modelInfo@workingDir)
  saveRDS(model,"model.RDS")
  setwd(cwd)

}

assign("saveModel",saveModel,envir=.GlobalEnv)

#' loadModel
#'
#' Loads a previously saved model from disk and sets cwd to Model's working directory
#'
#' @param directory    Directory where the model was saved
#'
#' @examples
#'       model=loadModel(directory)
#'
#' @export
#'
loadModel <-function(directory){
  model=readRDS(paste0(directory,"/","model.RDS"))
#  setwd(model@modelInfo@workingDir)
  model

}

assign("loadModel",loadModel,envir=.GlobalEnv)



#' workspace
#'
#' Sets the given directory as root of where models get created OR
#' switches cwd to the previously set workspace
#'
#' @param directory   Workspace root directory
#'
#' @examples
#'       workspace(directory)
#'
#'       workspace()
#'
#' @export
#'
workspace <-function(directory=""){
  if ( directory != "" ) {
      NlmeWorkspace=directory
      setwd(NlmeWorkspace)
  } else {
    if ( NlmeWorkspace != "" ){
      setwd(NlmeWorkspace)
    } else {
      NlmeWorkspace = getwd()
    }
  }
  assign("NlmeWorkspace",NlmeWorkspace, env = .GlobalEnv)
  print(NlmeWorkspace)

}

assign("workspace",workspace,envir=.GlobalEnv)







#' generateObjectModel
#'
#' Generates a connection table of model objects that represents
#' the built-in model
#'
#' @export
#'

setGeneric(name="generateObjectModel",
           def=function(obj)
           {
             standardGeneric("generateObjectModel")
           })

#' @export

setMethod(f="generateObjectModel",
          signature="NlmePmlModel",
          definition=function(obj){

  modelType=attr(attr(obj,"modelType"),"modelType")
  modelTypeName=ModelTypeNames[modelType]
  centralIndex = -1
  # FRED
  # Reset previously generated info

  obj@diffEquations = list()
  obj@statements=list()
  obj@dosePoints=list()
  obj@outputParams=list()

  # Process all PK elements
  if (length(grep( "PK", modelTypeName  ))  ){
      attrs = obj@pkModelAttrs
      paramType = attrs@parameterization@paramType
      absorpType = attrs@absorption@absorpType
      numComp = attrs@numCompartments
      isTlag = attrs@isTlag
      hasElimComp = attrs@hasEliminationComp
      isFractionExcreted = attrs@isFractionExcreted
      infusionAllowed = attrs@infusionAllowed
      isDuration = attrs@isDuration
      isFractionExcreted = attrs@isFractionExcreted

      isClearance = paramType == Clearance
      isSequential = obj@pkModelAttrs@isSequential
      isSaturating = attrs@isSaturating

      hasRandomEffect = obj@isPopulation 

      # TODO var bSaturating = bClearance && ms.pk.bMM;
      if ( isClearance && isSaturating ) {
        isSaturating = TRUE
      } else
        isSaturating = FALSE

      if ( absorpType == Extravascular )
        numDosePoints = 0
      else
        numDosePoints = 1

      # Central compartment
      cp1 = centralCompartment(isSequential = isSequential,numDosePoints = numDosePoints,
                               blockName="C" )

      if ( numDosePoints > 0 ) {

        if ( infusionAllowed ) {
          cp1@doses[[1]]@isInfusion = TRUE
        }
        if ( isDuration ) {
          cp1@doses[[1]]@isDuration = TRUE
        }
        if ( isTlag ) {
          cp1@doses[[1]]@isTlag = TRUE
          cp1@doses[[1]]@tlagExpression = "Tlag"
          tlag = NlmeStructuralParameter(name="Tlag",
                                hasRandomEffect=hasRandomEffect)
          addCompartment(obj)=tlag
        }
      }
      addCompartment(obj)=cp1
      centralIndex = length(obj@objects)
      centralPort = 1
      fromObject = centralIndex
      fromPort = 1 # C port
      eliminationType = FpClearance
      peripheralType  = FpClearance
      if ( isClearance == FALSE  ){
        eliminationType = FpMicro
        peripheralType  = FpMicro
      }
      if ( isSaturating ) {
        eliminationType = FpSaturating
        peripheralType  = FpSaturating
      }
 #     if ( hasElimComp) {
      if ( TRUE ) {
          # Elimination compartment
          el1 = eliminationCompartment(isSequential = isSequential,
                                   isFractionExcreted = isFractionExcreted,
                                   blockName="A0")
          addCompartment(obj)=el1
          eliminIndx = length(obj@objects)
          eliminPort = 0


          # Connect central compartment to elimination comp.
          flow= PkFlow(fromPort=centralIndex,
                   toPort = eliminIndx,
                   clName="Cl",
                   k12Name="Ke",
                   k21Name="",
                   kmName="Km",
                   vMaxName="VMax",
                   flowType = eliminationType,
                   isSequential=isSequential)

          addCompartment(obj)=flow
          flowIndx = length(obj@objects)
      }
      # Add absorption compartment
      if ( absorpType == Extravascular) {
        ac= absorptionCompartment(isSequential = isSequential, numDosePoints  = 1)

        if ( infusionAllowed ) {
          ac@doses[[1]]@isInfusion = TRUE
        }
        if ( isDuration ) {
          ac@doses[[1]]@isDuration = TRUE
        }
        if ( isTlag ) {
          ac@doses[[1]]@isTlag = TRUE
          ac@doses[[1]]@tlagExpression = "Tlag"
          tlag = NlmeStructuralParameter(name="Tlag",
                                         hasRandomEffect=hasRandomEffect)
          addCompartment(obj)=tlag
        }
        addCompartment(obj) = ac
        acIndx = length(obj@objects)

        absFlow = PkFlow(fromPort=acIndx,
                     toPort = centralIndex,
                     clName="",
                     k12Name="Ka",
                     k21Name="",
                     kmName="",
                     vMaxName="",
                     flowType = FpMicro,
                     isSequential=isSequential)
        addCompartment(obj) = absFlow
        absFlowIndx = length(obj@objects)

      }
      # Add PK compartment 2
      if ( numComp > 1 ) {
         cp2 = peripheralCompartment(isSequential = isSequential,
                                     isConcentration = isClearance  ,
                                     compIndex = 2 )
         addCompartment(obj) = cp2
         cp2Indx = length(obj@objects)
         abs2Flow = PkFlow(fromPort=centralIndex,
                          toPort = cp2Indx,
                          clName="Cl2",
                          k12Name="K12",
                          k21Name="K21",
                          kmName="",
                          vMaxName="",
                          twoWay=TRUE,
                          flowType = peripheralType,
                          isSequential=isSequential)
         addCompartment(obj) = abs2Flow
         abs2FlowIndx = length(obj@objects)
      }
      # Add PK compartment 3
      if ( numComp > 2 ) {
        cp3 = peripheralCompartment(isSequential = isSequential,
                                    isConcentration = isClearance  ,
                                    compIndex = 3 )
        addCompartment(obj)=  cp3
        cp3Indx = length(obj@objects)
        abs3Flow = PkFlow(fromPort=centralIndex,
                          toPort = cp3Indx,
                          clName="Cl3",
                          k12Name="K13",
                          k21Name="K31",
                          kmName="",
                          vMaxName="",
                          twoWay=TRUE,
                          flowType = peripheralType,
                          isSequential=isSequential)
        addCompartment(obj) = abs3Flow
        abs3FlowIndx = length(obj@objects)
      }
      # Error model
      errorModel = model@errorModel
      if ( length(errorModel@effectsList ) > 0 ) {
        for ( e in errorModel@effectsList ) {
          if ( e@effectName == "C" || e@effectName == "A0" ) {
          obs = ObservationContinuous(residualEffect=e)
#                                      name = e@observeName,
#                                      varName = e@effectName,
#                                      style = e@errorType,
#                                      definition = e@definition,
#                                      epsName = e@epsilonName,
#                                      isFrozen = e@frozen,
#                                      isBql = e@isBQL,
#                                      mixRatioName = e@definition,
#                                      multSdName= e@definition,
#                                      power = e@power,
#                                      stdev = e@SD )
          addCompartment(obj) = obs
          obsIndx = length(obj@objects)
          if ( e@effectName == "C")
              wire = Wire(wireName="",fromObject=centralIndex, fromPort=centralPort, toObject=obsIndx,toPort=0)
          else
              wire = Wire(wireName="",fromObject=eliminIndx,fromPort= eliminPort, toObject = obsIndx,toPort = 0)

          addCompartment(obj) = wire
          wireIndx = length(obj@objects)
          }
        }
      }


  }
  # Setup effect compartment
  if ( modelType == PkEmax ||modelType == PkIndirect || modelType == PkLinear ) {
    if ( obj@hasEffectsCompartment ){
       pkOutputName = "C"
       effComp = EffectsCompartment(flowInName=pkOutputName)
       addCompartment(obj) = effComp
       effIndx = length(obj@objects)

       if ( centralIndex >= 0 ) {
         wire = Wire(wireName="",
                     fromObject=centralIndex,
                     fromPort=centralPort,
                     toObject =effIndx,
                     toPort=0)
         addCompartment(obj) = wire
       }
       # override these guys
       centralIndex = effIndx
       centralPort = 0

    }

  }
  pdIndx = -1
  pdPort = 0
  # Setup Emax part of the model
  if ( modelType == PkEmax || modelType == Emax ) {
    emaxParam = obj@emaxModelAttrs
    emaxComp = EmaxCompartment(checkBaseline=emaxParam@checkBaseline,
                               checkFractional = emaxParam@checkFractional,
                               checkInhibitory = emaxParam@checkInhibitory,
                               checkSigmoid = emaxParam@checkSigmoid,
                               blockName = "E")

    addCompartment(obj) = emaxComp
    pdIndx = length(obj@objects)
    pdPort = 0
    if ( centralIndex >= 0 ) {
       wire = Wire(wireName="",
                fromObject=centralIndex,
                fromPort=centralPort,
                toObject =pdIndx,
                toPort=pdPort)
        addCompartment(obj) = wire
    }
  }

  # Setup Indirect part of the model
  if ( modelType == PkIndirect ) {
    indirectParam = obj@indirectModelAttrs
    indirectComp = IndirectCompartment(isExponent=indirectParam@isExponent,
                               isBuildup = indirectParam@isBuildup,
                               type = indirectParam@type,
                               blockName = "E")

    addCompartment(obj) = indirectComp
    pdIndx = length(obj@objects)
    pdPort = 0
    if ( centralIndex >= 0 ) {
      wire = Wire(wireName="",
                  fromObject=centralIndex,
                  fromPort=centralPort,
                  toObject =pdIndx,
                  toPort=pdPort)
      addCompartment(obj) = wire
    }
  }
  # Setup Linear part of the model
  if ( modelType == Linear || modelType == PkLinear ) {
    linearComp = LinearCompartment(
                                       type = obj@linearModelType,
                                       blockName = "E")

    addCompartment(obj) = linearComp
    pdIndx = length(obj@objects)
    pdPort = 0
    if ( centralIndex >= 0 ) {
      wire = Wire(wireName="",
                  fromObject=centralIndex,
                  fromPort=centralPort,
                  toObject =pdIndx,
                  toPort=pdPort)
      addCompartment(obj) = wire
    }
  }

  # Setup E observation
  if ( pdIndx >= 0 ) {
    errorModel = model@errorModel
    if ( length(errorModel@effectsList ) > 0 ) {
      for ( e in errorModel@effectsList ) {
        if ( e@effectName == "E" ) {
          obs = ObservationContinuous(residualEffect=e)

          addCompartment(obj) = obs
          obsIndx = length(obj@objects)

          wire = Wire(wireName="",
                      fromObject=pdIndx,
                      fromPort=pdPort,
                      toObject=obsIndx,
                      toPort=0)

          addCompartment(obj) = wire
          wireIndx = length(obj@objects)
        }
      }
    }

  }
  obj@errorModel=NlmeErrorModel(list())
  obj@isObjectModel = TRUE

   return(obj)
})




setGeneric(name="addCompartment<-",
           def=function(model,value)
           {
             standardGeneric("addCompartment<-")
           })
#'
#' @export
#'
setMethod(f="addCompartment<-",
          signature="NlmePmlModel",
          definition=function(model,value){

  indx = length(model@objects)+1
  value@compKey = model@nextKey
  model@objects[[indx]] = value
  model@nextKey = model@nextKey + 1
  model@objectsNeedRegenerating = TRUE
  return(model)
})

setGeneric(name="removeCompartment",
           def=function(model,key)
           {
             standardGeneric("removeCompartment")
           })
#'
#' @export
#'
setMethod(f="removeCompartment",
          signature="NlmePmlModel",
          definition=function(model,key){
            indx = compartmentIndx(model,key)
            model@objects[[indx]]=NULL
            model

          })

setGeneric(name="compartmentIndx",
           def=function(model,key)
           {
             standardGeneric("compartmentIndx")
           })
#'
#' @export
#'
setMethod(f="compartmentIndx",
          signature="NlmePmlModel",
          definition=function(model,key){
            for ( indx in 1:length(model@objects) ) {
              if ( model@objects[[indx]]@compKey == key )
                return(indx )
            }
            return(-1)

          })

setGeneric(name="compartmentKey",
           def=function(model,name)
           {
             standardGeneric("compartmentKey")
           })
#'
#' @export
#'
setMethod(f="compartmentKey",
          signature="NlmePmlModel",
          definition=function(model,name){
            for ( indx in 1:length(model@objects) ) {
              if ( model@objects[[indx]]@blockName == name )
                return(model@objects[[indx]]@compKey )
            }
            return(-1)

          })




setGeneric(name="getCompartment",
           def=function(model,key)
           {
             standardGeneric("getCompartment")
           })
#'
#' @export
#'
setMethod(f="getCompartment",
          signature="NlmePmlModel",
          definition=function(model,key){
            for ( comp in model@objects ) {
              if ( comp@compKey == key )
                return(comp )
            }
            return(NULL)

          })


setGeneric(name="setCompartment<-",
           def=function(model,key,value)
           {
             standardGeneric("setCompartment<-")
           })
#'
#' @export
#'
setMethod(f="setCompartment<-",
          signature="NlmePmlModel",
          definition=function(model,key,value){

            for ( indx in 1:length(model@objects) ) {
              if ( model@objects[[indx]]@compKey == key )
                model@objects[[indx]]=value
            }

            return(model)
          })




setGeneric(name="regenerateObjectModelPML",
           def=function(model,value)
           {
             standardGeneric("regenerateObjectModelPML")
           })
#'
#' @export
#'
setMethod(f="regenerateObjectModelPML",
          signature="NlmePmlModel",
          definition=function(model){

# Clear InputNames for all compartments

      indx=1
      for ( indx in 1:length(model@objects) ) {

        obj = model@objects[[indx]]

        obj1=clearInPortNames(obj)

        model@objects[[indx]] = obj1
      }

# Generate Procedure block codes
# Generate Expression block codes
# Update names of downstream Wires

      for ( indx in 1:length(model@objects) ) {
        obj = model@objects[[indx]]
        if ( is(obj,"Wire")){
          if (( obj@fromObject >= 0 ) &&
              ( obj@toObject >= 0 ) ){

            from=getCompartment(model,obj@fromObject)
            to = getCompartment(model,obj@toObject)

            fromName = getOutPortName(from,obj@fromPort)
            newTo = setInPortName(to, obj@toPort, fromName)
            setCompartment(model,to@compKey) = newTo
          }

        }
      }

# Create structural parameters for all compartments

      structuralParameters = model@structuralParams
      structuralParameters = list()
      hasRandomEffect= model@isPopulation

      for ( indx in 1:length(model@objects) ) {
        obj = model@objects[[indx]]

        sps = genStructuralParameters(obj,hasRandomEffect)

        if ( length(sps ) > 0 )
        for ( sp in sps ){
            found=FALSE

            if ( length(structuralParameters)  > 0 )
            for ( i in 1:length(structuralParameters) ) {
              if ( structuralParameters[[i]]@name == sp@name )
                found = TRUE
              if ( found )
                break
            }
            if ( ! found )
                structuralParameters[[length(structuralParameters)+1]] = sp
        }
      }
      model@structuralParams = structuralParameters

      model@randomValuesInitialized = FALSE
      initializeRandomEffectsBlock(model)
      statements = list()
      statements=c(statements, "test(){" )
      model@statements  = statements

      # Generate ODE statements
      odeStatements=c()

      for ( indx in 1:length(model@objects) ) {
        obj = model@objects[[indx]]

        st = genFlowRateODE(obj,obj@compKey,model)

        if ( st  != ""  )
          odeStatements=c(odeStatements,st)
      }
      model@statements = c(model@statements, odeStatements)

      # Now dose statements
      doseStatements=c()

      for ( indx in 1:length(model@objects) ) {
        obj = model@objects[[indx]]

        sts = genDoseStatements(obj)

        if ( length(sts ) > 0 )
          for ( s in sts )
            doseStatements=c(doseStatements,s)
      }
      model@statements = c(model@statements, doseStatements)


      for ( indx in 1:length(model@objects) ) {
        obj = model@objects[[indx]]

        sts = genStatements(obj)

        if ( length(sts ) > 0 )
          for ( s in sts )
            model@statements=c(model@statements,s)
      }
      # Generate observe/error statements

      for ( indx in 1:length(model@objects) ) {
        obj = model@objects[[indx]]

        sts = genObserveError(obj)

        if ( length(sts ) > 0 )
          for ( s in sts )
            model@statements=c(model@statements,s)
      }




      model = generateEffectsVariables(model)
      model = generateErrorStatment(model)
      model = generateStparmSatement(model)
      model = generateCovariateStatement(model)
      model = generateStparmExtraCode(model)
      model = generateFixedEffStatment(model)
      model = generateRanEffStatment(model)
      statements=attr(model,"statements")
      statements=c(statements,"}")
      model@statements=statements
      model@objectsNeedRegenerating = FALSE
      model
})

assign("regenerateObjectModelPML",regenerateObjectModelPML,envir=.GlobalEnv)

setGeneric(name="generateObjectModelPML",
           def=function(model,value)
           {
             standardGeneric("generateObjectModelPML")
           })
#'
#' @export
#'
setMethod(f="generateObjectModelPML",
          signature="NlmePmlModel",
          definition=function(model){

      statements = list()
      statements=c(statements, "test(){" )
      model@statements  = statements

      # Generate ODE statements
      odeStatements=c()

      for ( indx in 1:length(model@objects) ) {
        obj = model@objects[[indx]]

        st = genFlowRateODE(obj,obj@compKey,model)

        if ( st  != ""  )
          odeStatements=c(odeStatements,st)
      }
      model@statements = c(model@statements, odeStatements)

      # Now dose statements
      doseStatements=c()

      for ( indx in 1:length(model@objects) ) {
        obj = model@objects[[indx]]

        sts = genDoseStatements(obj)

        if ( length(sts ) > 0 )
          for ( s in sts )
            doseStatements=c(doseStatements,s)
      }
      model@statements = c(model@statements, doseStatements)


      for ( indx in 1:length(model@objects) ) {
        obj = model@objects[[indx]]

        sts = genStatements(obj)

        if ( length(sts ) > 0 )
          for ( s in sts )
            model@statements=c(model@statements,s)
      }
      # Generate observe/error statements

      for ( indx in 1:length(model@objects) ) {
        obj = model@objects[[indx]]

        sts = genObserveError(obj)

        if ( length(sts ) > 0 )
          for ( s in sts )
            model@statements=c(model@statements,s)
      }




      model = generateEffectsVariables(model)
      model = generateErrorStatment(model)
      model = generateStparmSatement(model)
      model = generateCovariateStatement(model)
      model = generateStparmExtraCode(model)
      model = generateFixedEffStatment(model)
      model = generateRanEffStatment(model)
      model = generateSecondaryStatement(model)
      statements=attr(model,"statements")
      statements=c(statements,"}")
      model@statements=statements

      model
})

assign("generateObjectModelPML",generateObjectModelPML,envir=.GlobalEnv)




#' generatePML
#'
#' Generates PML statements based on the current model
#'
#' @export
#'

setGeneric(name="generatePML",
           def=function(.Object)
           {
             standardGeneric("generatePML")
           })
#'
#' @export
#'
setMethod(f="generatePML",
          signature="NlmePmlModel",
          definition=function(.Object){

    if ( .Object@isTextual == FALSE ) {
            if ( .Object@isObjectModel == FALSE  )
              .Object = generatePMLModel(.Object)
            else {
              if ( .Object@objectsNeedRegenerating == TRUE ) {
                .Object = regenerateObjectModelPML(.Object)
              }
              else {

                .Object = generateObjectModelPML(.Object)
              }
            }
            if ( .Object@randomValuesInitialized == FALSE ){
              .Object=initializeRandomEffectsBlock(.Object)
            }
    }
            return(.Object)
          })

assign("generatePML",generatePML,envir=.GlobalEnv)



#' zeroOrderAbsorption
#'
#' Creates a zeroth order absorption dosing regimen for the model
#'
#' @param model      PK/PD model to apply absorption model to
#' @param doseInfo   Dose information for central compartment
#'
#' @examples
#'       dosePoint= DosePoint(isZeroOrderAbsorption = DurationDose,
#'                            durationExpression = "DUR",
#'                            isTlag = TRUE,
#'                            tlagExpression = "Tlag")
#'
#'       model2=zeroOrderAbsorption(model,dosePoint)
#'       print(model2)
#'
#' @export
#'

setGeneric(name="zeroOrderAbsorption",
           def=function(model,doseInfo)
           {
             standardGeneric("zeroOrderAbsorption")
           })
#'
#' @export
#'
setMethod(f="zeroOrderAbsorption",
          signature="NlmePmlModel",
          definition=function(model,doseInfo){
            if ( length(model@objects) == 0 )
              model = generateObjectModel(model)
            # Find first central compartment
            done=FALSE
            for ( indx in 1:length(model@objects)){
              obj = model@objects[[indx]]
              if ( class(obj) == "NlmeCompartment" && obj@type == Central) {
                obj@numDosePoints = 1
                obj@doses[[1]] = doseInfo

                done=TRUE
                model@objects[[indx]] = obj


              }
              if ( done )
                break

            }

            model=generatePML(model)
            return(model)
          })

assign("zeroOrderAbsorption",zeroOrderAbsorption,envir=.GlobalEnv)




#'
#' @export
#'
setGeneric(name="mixedFirstOrderZeroOrderAbsorption",
           def=function(model,doseInfo,absDoseInfo)
           {
             standardGeneric("mixedFirstOrderZeroOrderAbsorption")
           })
#' mixedFirstOrderZeroOrderAbsorption
#'
#' Creates a mixed 1st, zeroth order absorption
#' dosing regimen for a given model
#'
#' @param model        PK/PD model to apply absorption model to
#' @param doseInfo     Dose information for central compartment
#' @param absDoseInfo  Dose information for absorption compartment
#'
#' @examples
#'       dosePoint= DosePoint(isZeroOrderAbsorption = DurationDose,
#'                            durationExpression = "DUR",
#'                            isTlag = FALSE,
#'                            isBioavail = TRUE,
#'                            bioavailExpression = "logitF1;1-ilogit(logitF1)")
#'
#'       absDosePoint= DosePoint(isZeroOrderAbsorption = NoDose,
#'                               isBioavail = TRUE,
#'                               bioavailExpression = "logitF1;ilogit(logitF1)")
#'
#'       model2=mixedFirstOrderZeroOrderAbsorption(model,dosePoint,absDosePoint)
#'       print(model2)
#'
#' @export
#'

setMethod(f="mixedFirstOrderZeroOrderAbsorption",
          signature="NlmePmlModel",
          definition=function(model,doseInfo,absDoseInfo){
            if ( length(model@objects) == 0 )
              model = generateObjectModel(model)
            # Find first central compartment
            done=FALSE
            centralIndex = -1
            for ( indx in 1:length(model@objects)){
              obj = model@objects[[indx]]
              if ( class(obj) == "NlmeCompartment" && obj@type == Central) {
                obj@numDosePoints = 1
                obj@doses[[1]] = doseInfo
                centralIndx = indx
                done=TRUE
                model@objects[[indx]] = obj


              }
              if ( done )
                break

            }
            if ( done == TRUE ){
              isSequential = model@objects[[centralIndx]]@isSequential
              done = FALSE
              for ( indx in 1:length(model@objects)){
                obj = model@objects[[indx]]
                if ( class(obj) != "PkFlow" )
                  next
                if ( obj@toPort != centralIndx )
                  next
                absorpIndx = obj@fromPort
                obj = model@objects[[absorpIndx]]
                if ( class(obj) == "NlmeCompartment" && obj@type == Absorption ) {
                  obj@numDosePoints = 1
                  obj@doses[[1]] = absDoseInfo

                  done=TRUE
                  model@objects[[absorpIndx]] = obj


                }
                if ( done )
                  break
              }
              if ( done == FALSE ) {
                ac= absorptionCompartment(aName="Aa1",
                                          isSequential = isSequential)
                ac@numDosePoints = 1
                ac@doses[[1]] = absDoseInfo
                addCompartment(model) = ac
                absorpIndx = length(model@objects)

                absFlow = PkFlow(fromPort=absorpIndx,
                                 toPort = centralIndx,
                                 clName="",
                                 k12Name="Ka1",
                                 k21Name="",
                                 kmName="",
                                 vMaxName="",
                                 flowType = FpMicro,
                                 isSequential=isSequential)
                addCompartment(model) = absFlow
              }
            }

            model=generatePML(model)
            return(model)
          })

assign("mixedFirstOrderZeroOrderAbsorption",mixedFirstOrderZeroOrderAbsorption,envir=.GlobalEnv)


#' parallelFirstOrderAbsorption
#'
#' Creates a parallel first order absorption dosing regimen
#' for the given model per dosing instructions
#'
#' @param model        PK/PD model to apply absorption model to
#' @param doseInfo     Dose information for central compartment
#' @param absDoseInfo  Dose information for absorption compartment
#'
#' @examples
#'       dosePoint1 = DosePoint(isZeroOrderAbsorption = NoDose,
#'                              isTlag = TRUE,
#'                              tlagExpression="Tlag",
#'                              isBioavail = TRUE,
#'                              bioavailExpression = "logitF1;1-ilogit(logitF1)")
#'
#'       dosePoint2 = DosePoint(isZeroOrderAbsorption = NoDose,
#'                              isBioavail = TRUE,
#'                              bioavailExpression = "logitF1;ilogit(logitF1)")
#'
#'       model2=parallelFirstOrderAbsorption(model,dosePoint1,dosePoint2)
#'       print(model2)
#'
#' @export
#'

setGeneric(name="parallelFirstOrderAbsorption",
           def=function(model,doseInfo1,doseInfo2)
           {
             standardGeneric("parallelFirstOrderAbsorption")
           })

#' @export

setMethod(f="parallelFirstOrderAbsorption",
          signature="NlmePmlModel",
          definition=function(model,doseInfo1,doseInfo2){
            if ( length(model@objects) == 0 )
              model = generateObjectModel(model)
            # Find first central compartment
            done=FALSE
            centralIndex = -1
            for ( indx in 1:length(model@objects)){
              obj = model@objects[[indx]]
              if ( class(obj) == "NlmeCompartment" && obj@type == Central) {
                obj@numDosePoints = 0
                centralIndx = indx
                done=TRUE
                model@objects[[indx]] = obj
              }
              if ( done )
                break

            }

            fromAbsorpIndexes=c()
            if ( done == TRUE ){
              isSequential = model@objects[[centralIndx]]@isSequential
              done = FALSE
              for ( indx in 1:length(model@objects)){
                obj = model@objects[[indx]]
                if ( class(obj) != "PkFlow" )
                  next
                if ( obj@toPort != centralIndx )
                  next
                absorpIndx = obj@fromPort
                aObj=model@objects[[absorpIndx]]
                if ( class(aObj) == "NlmeCompartment" && aObj@type == Absorption )
                    fromAbsorpIndexes=c(fromAbsorpIndexes, absorpIndx)
              }

              stopifnot( length(fromAbsorpIndexes) <= 2 )


              for ( a in 1:2 ) {
                if ( length(fromAbsorpIndexes) < a ){
                  ac= absorptionCompartment(aName=paste0("Aa",a),
                                            isSequential = isSequential)
                  addCompartment(model) = ac
                  absorpIndx = length(model@objects)
                  fromAbsorpIndexes=c(fromAbsorpIndexes,absorpIndx)

                  absFlow = PkFlow(fromPort=absorpIndx,
                                   toPort = centralIndx,
                                   clName="",
                                   k12Name=paste0("Ka",a),
                                   k21Name="",
                                   kmName="",
                                   vMaxName="",
                                   flowType = FpMicro,
                                   isSequential=isSequential)
                  addCompartment(model) = absFlow
                  absFlowIndx = length(model@objects)
                } else
                    absorpIndx = fromAbsorpIndexes[[a]]
                obj = model@objects[[absorpIndx]]
                obj@numDosePoints = 1
                if ( a == 1 )
                  obj@doses[[1]] = doseInfo1
                else
                  obj@doses[[1]] = doseInfo2
                model@objects[[absorpIndx]] = obj
              }

            }

            model=generatePML(model)
            return(model)
          })

assign("parallelFirstOrderAbsorption",parallelFirstOrderAbsorption,envir=.GlobalEnv)





#' addContinuousObservation
#'
#' Adds a continuous observation to the model
#'
#' @param observationName      Observation name
#' @param effect	       Specification of residual error model
#' @param structuralParameters Optional array of structural parameters to define
#' @param isFrozen             Optional array to freeze fixed effects. default = FALSE
#' @param hasRandomEffect      Optional array to enable random effects. default = TRUE
#'
#' @examples
#'       effect = NlmeResidualEffect(ERR_ADD_MULT,"E",isBQL=TRUE)
#'
#'       model2 = addContinuousObservation(model,
#'                observationName="ContinuousObs", effect = effect,
#'                hasRandomEffect=TRUE)
#'
#' @export
#'
setGeneric(name="addContinuousObservation",
           def = function(model,...)
           {
             standardGeneric("addContinuousObservation")
           })


#' addContinuousObservation
#'
#' @export
#'

setMethod(f="addContinuousObservation",
          signature="NlmePmlModel",
          definition=function(model,
                             observationName="CatObs",
                             effect,
                             structuralParameters=c(),
                             hasRandomEffect=c(),
                             isFrozen=c(),
                             customCodeLines=c()){

            # Generate an object model if needed
            if ( length(model@objects) == 0 )
              model = generateObjectModel(model)

            # Add all optional structural parameters
            if ( length(structuralParameters) != 0  )
#              for ( s in structuralParameters ) {
              for ( indx in 1:length(structuralParameters) ) {
                s = structuralParameters[[indx]]
                frozen = FALSE
                randomEffect = TRUE
                if ( length(isFrozen) > 0 )
                    frozen = isFrozen[[indx]]
                if ( length(hasRandomEffect) > 0 )
                    randomEffect = hasRandomEffect[[indx]]
                sp = NlmeStructuralParameter(name=s,
                                             hasRandomEffect = randomEffect ,
                                             isFrozen = frozen ,
                                             hasCovariateEffect = FALSE,
                                             isSequential  = FALSE)
                addCompartment(model) = sp
              }
            # Create and add continuous observation compartment

            obs = ObservationContinuous(
                                        blockName = observationName,
                                        residualEffect=effect,
                                        customCodeLines=customCodeLines)
            addCompartment(model) = obs

            model=generatePML(model)
            return(model)
          })

assign("addContinuousObservation",addContinuousObservation,envir=.GlobalEnv)



#' addCategoricalObservation
#'
#' Adds a categorical observation to the model
#'
#' @param observationName       Observation name
#' @param linkFunction          Logit|Probit|LogLog|Cloglog|InvCustom
#' @param isInhibitory          Is the array of offset expressions in
#'                                   ascending order? (default = FALSE)
#' @param slope                 Slope expression
#' @param offsetArray           Array of offset  expressions
#' @param dobefore              Code to execute before the observation
#' @param doafter               Code to execute after the observation
#' @param structuralParameters  Array of structural parameters to define
#' @param isFrozen              Optional array to freeze fixed effects. default = FALSE
#' @param hasRandomEffect       Optional array to enable random effects. default = TRUE
#'
#' @examples
#'       model2 = addCategoricalObservation(model,
#'                observationName="CategoricalObs",
#'                offsetArray=c("E0 + Emax*C/(C+EC50) + Cat1Constant",
#'                              "E0 + Emax*C/(C+EC50)"),
#'                structuralParameters=c("E0","Emax","EC50","Cat1Constant"))
#'
#' @export
#'
setGeneric(name="addCategoricalObservation",
           def = function(model,...)
           {
             standardGeneric("addCategoricalObservation")
           })

#' addCategoricalObservation
#'
#' Adds a categorical observation to the model
#'
#' @param observationName       Observation name
#' @param linkFunction          InvLogit|InvProbit|InvLogLog|InvClogClog|InvCustom
#' @param isInhibitory          Is categorical model inhibitory
#' @param slope                 Slope expression
#' @param offsetArray           Array of offset  expressions
#' @param dobefore              Code to execute before the observation
#' @param doafter               Code to execute after the observation
#' @param structuralParameters  Array of structural parameters to define
#' @param isFrozen              Optional array to freeze fixed effects. default = FALSE
#' @param hasRandomEffect       Optional array to enable random effects. default = TRUE
#'
#' @examples
#'       model2 = addCategoricalObservation(model,
#'                observationName="CategoricalObs",
#'                offsetArray=c("E0 + Emax*C/(C+EC50) + Cat1Constant",
#'                              "E0 + Emax*C/(C+EC50)"),
#'                structuralParameters=c("E0","Emax","EC50","Cat1Constant"))
#'
#' @export
#'

setMethod(f="addCategoricalObservation",
          signature="NlmePmlModel",
          definition=function(model,
                             name="C",
                             observationName="CatObs",
                             linkFunction=InvLogit,
                             isInhibitory=FALSE,
                             slope="0",
                             offsetArray=c(),
                             dobefore="",
                             doafter="",
                             structuralParameters=c(),
                             hasRandomEffect=c(),
                             isFrozen=c(),
                             customCodeLines=c()){

            # Generate an object model if needed
            if ( length(model@objects) == 0 )
              model = generateObjectModel(model)

            # Add all optional structural parameters
            if ( length(structuralParameters) != 0  )
              for ( indx in 1:length(structuralParameters) ) {
                s = structuralParameters[[indx]]
                frozen = FALSE
                randomEffect = TRUE
                if ( length(isFrozen) > 0 )
                    frozen = isFrozen[[indx]]
                if ( length(hasRandomEffect) > 0 )
                    randomEffect = hasRandomEffect[[indx]]
                sp = NlmeStructuralParameter(name=s,
                                             hasRandomEffect = randomEffect ,
                                             isFrozen = frozen ,
                                             hasCovariateEffect = FALSE,
                                             isSequential  = FALSE)
                addCompartment(model) = sp
              }
            # Create and add categorical observation compartment

            obs = ObservationCategorical(
                                        name=name,
                                        observationName = observationName,
                                        blockName = observationName,
                                        linkFunction=linkFunction,
                                        isInhibitory=isInhibitory,
                                        slope=slope,
                                        offsetArray=offsetArray,
                                        dobefore=dobefore,
                                        doafter=doafter,
                                        customCodeLines=customCodeLines)
            addCompartment(model) = obs

            model=generatePML(model)
            return(model)
          })

assign("addCategoricalObservation",addCategoricalObservation,envir=.GlobalEnv)



#' addEventObservation
#'
#' Adds an event observation to the model
#'
#' @param observationName        Observation name
#' @param slopeExpression        Hazard slope expression
#' @param expression             Base hazard expression
#' @param dobefore               Code to execute before the observation
#' @param doafter                Code to execute after the observation
#' @param structuralParameters   Array of structural parameters to define
#' @param isFrozen               Optional array to freeze fixed effects. default = FALSE
#' @param hasRandomEffect        Optional array to enable random effects. default = TRUE
#'
#' @examples
#'       model2 = addEventObservation(model,
#'                observationName="EventObs",
#'                expression="1 - C/(C+ IC50)",
#'                structuralParameters=c("IC50"))
#'
#' @export
#'
setGeneric(name="addEventObservation",
           def = function(model,...)
           {
             standardGeneric("addEventObservation")
           })

#'
#' @export
#'

setMethod(f="addEventObservation",
          signature="NlmePmlModel",
          definition=function(model,
                              name="C",
                              observationName="CatObs",
                              slopeExpression="",
                              expression="",
                              dobefore="",
                              doafter="",
                              structuralParameters=c(),
                              hasRandomEffect=c(),
                              isFrozen=c(),
                              customCodeLines=c()){

            # Generate an object model if needed
            if ( length(model@objects) == 0 )
              model = generateObjectModel(model)

            # Add all optional structural parameters
            if ( length(structuralParameters) != 0  )
              for ( indx in 1:length(structuralParameters) ) {
                s = structuralParameters[[indx]]
                frozen = FALSE
                randomEffect = TRUE
                if ( length(isFrozen) > 0 )
                    frozen = isFrozen[[indx]]
                if ( length(hasRandomEffect) > 0 )
                    randomEffect = hasRandomEffect[[indx]]
                sp = NlmeStructuralParameter(name=s,
                                             hasRandomEffect = randomEffect ,
                                             isFrozen = frozen ,
                                             hasCovariateEffect = FALSE,
                                             isSequential  = FALSE)
                addCompartment(model) = sp
              }
            # Create and add  observation compartment
            obs = ObservationEventCount(
              name=name,
              isEvent = TRUE,
              observationName = observationName,
              slopeExpression = slopeExpression,
              expression =expression,
              dobefore=dobefore,
              doafter=doafter,
              customCodeLines=customCodeLines)
            addCompartment(model) = obs

            model=generatePML(model)
            return(model)
          })

assign("addEventObservation",addEventObservation,envir=.GlobalEnv)



#' addCountObservation
#'
#' Adds a count observation to the model
#'
#' @param observationName       Observation name
#' @param slopeExpression       Hazard slope expression
#' @param expression            Base hazard expression
#' @param dobefore              Code to execute before the observation
#' @param doafter               Code to execute after the observation
#' @param structuralParameters  Array of structural parameters to define
#' @param isFrozen              Optional array to freeze fixed effects. default = FALSE
#' @param hasRandomEffect       Optional array to enable random effects. default = TRUE
#'
#' @examples
#'       model2 = addCountObservation(model,
#'                observationName="CountObs",
#'                expression="Emax * C / (EC50 + C)",
#'                structuralParameters=c("Emax","EC50"))
#'
#' @export
#'
setGeneric(name="addCountObservation",
           def = function(model,...)
           {
             standardGeneric("addCountObservation")
           })

#'
#' @export
#'

setMethod(f="addCountObservation",
          signature="NlmePmlModel",
          definition=function(model,
                              name="C",
                              observationName="CatObs",
                              slopeExpression="",
                              expression="",
                              dobefore="",
                              doafter="",
                              structuralParameters=c(),
                              hasRandomEffect=c(),
                              isFrozen=c(),
                              customCodeLines=c()){

            # Generate an object model if needed
            if ( length(model@objects) == 0 )
              model = generateObjectModel(model)

            # Add all optional structural parameters
            if ( length(structuralParameters) != 0  )
              for ( indx in 1:length(structuralParameters) ) {
                s = structuralParameters[[indx]]
                frozen = FALSE
                randomEffect = TRUE
                if ( length(isFrozen) > 0 )
                    frozen = isFrozen[[indx]]
                if ( length(hasRandomEffect) > 0 )
                    randomEffect = hasRandomEffect[[indx]]
                sp = NlmeStructuralParameter(name=s,
                                             hasRandomEffect = randomEffect ,
                                             isFrozen = frozen ,
                                             hasCovariateEffect = FALSE,
                                             isSequential  = FALSE)
                addCompartment(model) = sp
              }
            # Create and add observation compartment
            obs = ObservationEventCount(
              name=name,
              isEvent = FALSE,
              observationName = observationName,
              slopeExpression = slopeExpression,
              expression =expression,
              dobefore=dobefore,
              doafter=doafter,
              customCodeLines=customCodeLines)

            addCompartment(model) = obs

            model=generatePML(model)
            return(model)
          })

assign("addCountObservation",addCountObservation,envir=.GlobalEnv)


#' addLLObservation
#'
#' Adds an LL observation to the model
#'
#' @param observationName       Observation name
#' @param expression            Logarithm of conditional likelihood
#' @param dobefore              Code to execute before the observation
#' @param doafter               Code to execute after the observation
#' @param structuralParameters  Array of structural parameters to define
#' @param isFrozen              Optional array to freeze fixed effects. default = FALSE
#' @param hasRandomEffect       Optional array to enable random effects. default = TRUE
#' @param simulationCode        Array of simulation code lines
#'
#' @examples
#'       model2 = addLLObservation(model,
#'                observationName="iObs",
#'                expression="-E +iObs*log(E) - lgamm(iObs+1)",
#'                customCodeLines=c("double(prob,i,u)"),
#'                simulationCode=c("u = unif()",
#'                                 "prob = 0",
#'                                 "i = 0",
#'                                 "prob = prob + exp(-E + i*log(E) -lgamm(i+1))",
#'                                 "while(u >= prob){",
#'                                 "    prob = prob + exp(-E + i * log(E) -lgamm(i+1))",
#'                                 "    i = i + 1",
#'                                 " }",
#'                                 " iObs = i"))
#'
#' @export
#'
setGeneric(name="addLLObservation",
           def = function(model,...)
           {
             standardGeneric("addLLObservation")
           })

#'
#' @export
#'

setMethod(f="addLLObservation",
          signature="NlmePmlModel",
          definition=function(model,
                              name="C",
                              observationName="CatObs",
                              slopeExpression="",
                              expression="",
                              dobefore="",
                              doafter="",
                              structuralParameters=c(),
                              hasRandomEffect=c(),
                              isFrozen=c(),
                              simulationCode=c(),
                              customCodeLines=c()){

            # Generate an object model if needed
            if ( model@isObjectModel == TRUE )
              model = generateObjectModel(model)

            # Add all optional structural parameters
            if ( length(structuralParameters) != 0  )
              for ( indx in 1:length(structuralParameters) ) {
                s = structuralParameters[[indx]]
                frozen = FALSE
                randomEffect = TRUE
                if ( length(isFrozen) > 0 )
                    frozen = isFrozen[[indx]]
                if ( length(hasRandomEffect) > 0 )
                    randomEffect = hasRandomEffect[[indx]]
                sp = NlmeStructuralParameter(name=s,
                                             hasRandomEffect = randomEffect ,
                                             isFrozen = frozen ,
                                             hasCovariateEffect = FALSE,
                                             isSequential  = FALSE)
                addCompartment(model) = sp
              }
            # Create and add observation compartment

            obs = ObservationLL(
              name=name,
              isEvent = FALSE,
              observationName = observationName,
              slopeExpression = slopeExpression,
              expression =expression,
              dobefore=dobefore,
              doafter=doafter,
              simulationCode= simulationCode ,
              customCodeLines=customCodeLines)
            addCompartment(model) = obs

            model=generatePML(model)
            return(model)
          })

assign("addLLObservation",addLLObservation,envir=.GlobalEnv)



#' addExtraDose
#'
#' Adds an extra dose definition to the model
#'
#' @param model     PK/PD model
#' @param name      Name of the compartment to which the dose is administered
#' @param doseType  SteadyStateDose|AddlDose
#' @param doses     List of treatment doses
#'
#' @examples
#'      dose = ExtraDoseItem(type=BolusDose,
#'                           amountColumn="Aa",
#'                           deltaTimeColumn="ii")
#'
#'      dose1 = ExtraDoseItem(type=InfusionDose,
#'                           amount=12,
#'                           rate=4,
#'                           deltaTime=1,
#'                           isSecondDose=TRUE)
#'
#'      model = addExtraDose(model,name="Aa",
#'                           doseType=SteadyStateDose,
#'                           doses=c(dose,dose1) )
#'
#' @export
#'
setGeneric(name="addExtraDose",
           def = function(model,name,doseType,doses)
           {
             standardGeneric("addExtraDose")
           })

#'
#' @export
#'

setMethod(f="addExtraDose",
          signature="NlmePmlModel",
          definition=function(model,
                              name,
                              doseType,
                              doses){
            dose = ExtraDoseOption(name=name,doseType=doseType,doses=doses)
            model@extraDoses = c(model@extraDoses, dose)

            model=generatePML(model)
            return(model)
          })

assign("addExtraDose",addExtraDose,envir=.GlobalEnv)

#' addReset
#'
#' Adds reset dose instructions to the model
#'
#' @param model     PK/PD model
#' @param low       Lower value of reset range
#' @param hi        Upper value of reset range
#'
#' @examples
#'
#' @export
#'
setGeneric(name="addReset",
           def = function(model,low,hi)
           {
             standardGeneric("addReset")
           })

#'
#' @export
#'

setMethod(f="addReset",
          signature="NlmePmlModel",
          definition=function(model,
                              low,hi){

            model@resetInfo = ResetColumnInfo(low,hi)
            model@hasResetInfo = TRUE

            model=generatePML(model)
            return(model)
          })

assign("addReset",addReset,envir=.GlobalEnv)





#' addExpression
#'
#' Adds an expression to the model and optionally override the
#' definition of an structural parameter.
#'
#' @param model             PK/PD model
#' @param blockName         Expression variable name
#' @param codeLine          Line of PML  defining the expression
#' @param structuralParams  Optional array of structural parameters to define
#' @param isFrozen          Optional array to freeze fixed effects. default = FALSE
#' @param hasRandomEffect   Optional array to enable random effects. default = TRUE for population model
#' @param override          Override existing definition.(FALSE)
#'
#' @examples
#'      model = addExpression(model,
#'                       blockName="Cl",
#'                       structuralParams = c("baseCl","Imax","T50","Gam"),
#'                       codeLine=" baseCl * (1 - Imax * t^Gam /(t^Gam + T50^Gam))",
#'                       isFrozen=c(FALSE,TRUE,TRUE,TRUE),
#'                       override= TRUE)
#'
#' @export
#'
setGeneric(name="addExpression",
           def = function(model,...)
           {
             standardGeneric("addExpression")
           })

#'
#' @export
#'

setMethod(f="addExpression",
          signature="NlmePmlModel",
          definition=function(model,
                              blockName,
                              codeLine,
                              structuralParams=c(),
                              isFrozen = c(),
                              hasRandomEffect = c(),
                              override=FALSE){

            # Generate an object model if needed
            if ( model@isObjectModel != TRUE )
              model = generateObjectModel(model)

            if ( model@isPopulation == FALSE ) {
                if ( length(structuralParams) > 0 ) 
                    hasRandomEffect=as.list(rep(FALSE,length(structuralParams)))
            }
            # Create and add expression
            expr =NlmeExpression(blockName=blockName,
                                   structuralParams = as.list(structuralParams),
                                 isFrozen = as.list(isFrozen),
                                 hasRandomEffect = as.list(hasRandomEffect),
                                   codeLine= codeLine)
            nextKey = model@nextKey
            addCompartment(model) = expr

            # Modify existing param
            if ( override  ) {
              for ( o in model@objects ) {
                numInPorts = getNumInPorts(o)
                if ( ! is.null(numInPorts) &&  ( numInPorts > 0 )) {
                   for ( i in 1:numInPorts ) {
                     name = getInPortName(o,i)
                     if ( !is.null(name ) && ( name == blockName ))  {
                       wire = Wire(wireName="", fromObject=nextKey,fromPort=0,toObject=o@compKey,toPort=i)
                       addCompartment(model) = wire
                       break
                     }
                   }
                }

              }
            }

            model=generatePML(model)
            return(model)
          })

assign("addExpression",addExpression,envir=.GlobalEnv)


#' addParameter
#'
#' Adds a structural parameter to the model and optionally
#' override the definition of an structural parameter.
#'
#' @param model            PK/PD model
#' @param name             Name of the structural parameter
#' @param fixedEffName     Name to use for fixed effects
#' @param randomEffName    Name to use for random effects
#' @param hasRandomEffect  Does the parameter have a random effect? (default:TRUE)
#' @param style            Parameter style:
#'                            LogNormal, Normal, Combination, Log, Logit, Custom
#' @param initialValue     Initial value for the fixed effect
#' @param lowerBound       Lower limit for the fixed effect
#' @param upperBound       Upper limit for the fixed effect
#' @param isFrozen         Freeze fixed effect? (default : FALSE)
#' @param ranEffInitValue  Initial value for the random effect
#' @param code             For Custom style, PML code to override the definition
#' @param extraCode        Extra lines of code that relates to this parameter
#'
#' @examples
#'      model = addParameter(model,
#'                           name="baseCl",
#'                           style=LogNormal)
#'
#' @export
#'
setGeneric(name="addParameter",
           def = function(model,...)
           {
             standardGeneric("addParameter")
           })

#'
#' @export
#'

setMethod(f="addParameter",
          signature="NlmePmlModel",
          definition=function(model,
                              name,
                              fixedEffName="",
                              randomEffName="",
                              hasRandomEffect=TRUE,
                              hasCovariateEffect=FALSE,
                              style=LogNormal,
                              initialValue="1",
                              lowerBound="",
                              upperBound="",
                              units="",
                              isFrozen=FALSE,
                              isSequential=FALSE,
                              ranEffInitValue="1",
                              code=list(),
                              extraCode=list()){

            # Generate an object model if needed
            if ( model@isObjectModel != TRUE )
              model = generateObjectModel(model)

              if ( model@isPopulation == FALSE )
                  hasRandomEffect = FALSE 
              sp = NlmeStructuralParameter(name=name,
                                        fixedEffName = fixedEffName,
                                        hasRandomEffect = hasRandomEffect ,
                                        randomEffName = randomEffName,
                                        hasCovariateEffect = hasCovariateEffect,
                                        style = style,
                                        initialValue = initialValue,
                                        lowerBound = lowerBound,
                                        units = units,
                                        isFrozen = isFrozen,
                                        isSequential = isSequential,
                                        ranEffInitValue = ranEffInitValue,
                                        code  = as.list(code),
                                        extraCode = as.list(extraCode))
              addCompartment(model) = sp
            model=generatePML(model)
            return(model)
          })

assign("addParameter",addParameter,envir=.GlobalEnv)

#'
#' addProcedure
#'
#' Adds a procedure block to the model and optionally override the definition of
#' structural parameters.
#'
#' @param model               PK/PD model
#' @param blockName           Procedure block name
#' @param codeLines           Line of PML code to insert into model
#' @param structuralParams    Optional array of structural parameters to define
#' @param isFrozen            Optional array to freeze fixed effects. default = FALSE
#' @param hasRandomEffect     Optional array to enable random effects. default = TRUE for population model
#' @param override            Override existing definition.(FALSE)
#'
#' @examples
#'      model = addProcedure(model,
#'                           blockName="Del",
#'                           structuralParams = c("Del"),
#'                           codeLines=c("Ka=Ke+Del"),
#'                           isFrozen=c(FALSE),
#'                           override= TRUE)
#'
#' @export
#'
setGeneric(name="addProcedure",
           def = function(model,...)
           {
             standardGeneric("addProcedure")
           })

#'
#' @export
#'

setMethod(f="addProcedure",
          signature="NlmePmlModel",
          definition=function(model,
                              blockName,
                              codeLines,
                              structuralParams=c(),
                              isFrozen = c(),
                              hasRandomEffect = c(),
                              override=FALSE){
            # Generate an object model if needed
            if ( model@isObjectModel != TRUE )
              model = generateObjectModel(model)

            if ( model@isPopulation == FALSE ) {
                if ( length(structuralParams) > 0 ) 
                    hasRandomEffect=as.list(rep(FALSE,length(structuralParams)))
            }
            # Create and add Procedure
            proc =NlmeProcedure(blockName=blockName,
                                 structuralParams = as.list(structuralParams),
                                 isFrozen = as.list(isFrozen),
                                 hasRandomEffect = as.list(hasRandomEffect),
                                 codeLines= as.list(codeLines))
            nextKey = model@nextKey
            addCompartment(model) = proc

            outPortNames=c()
            numOutPorts = getNumOutPorts(proc)
            if ( numOutPorts > 0 ) {
              for ( i in 1:numOutPorts ) {
                outPortNames=c(outPortNames,getOutPortName(proc,i))
              }
            }


            # Modify existing param
            if ( override  ) {
              for ( o in model@objects ) {
                numInPorts = getNumInPorts(o)
                if ( ! is.null(numInPorts) &&  ( numInPorts > 0 )) {
                  for ( i in 1:numInPorts ) {
                    name = getInPortName(o,i)
                    if ( !is.null(name ) && length(outPortNames) > 0  )   {
                      for ( indx in 1:length(outPortNames) ) {
                        portName=outPortNames[[indx]]
                        if ( name == portName ) {
                          wire = Wire(wireName="", fromObject=nextKey,
                             fromPort=indx,toObject=o@compKey,toPort=i)
                          addCompartment(model) = wire
                          break
                        }
                      }
                    }
                  }
                }
              }
            }

            model=generatePML(model)
            return(model)
          })

assign("addProcedure",addProcedure,envir=.GlobalEnv)


#' acceptAllEffects
#'
#' Accepts all estimates for fixed effects, Sigma and random effects
#'
#' @param model    PK/PD model
#'
#' @examples
#'      model = acceptAllEffects(model)
#'
#' @export
#'


acceptAllEffects <-function(model){

#  if ( model@isTextual )
#      stop("Textual models should be editted manually")

  source(paste0(model@modelInfo@workingDir,"/dmp.txt"))

  fixedEffects = dmp.txt$coefficients$fixed

  omegas = dmp.txt$omega

  sigmas = dmp.txt$sigma

  initFixedEffects(model ) = fixedEffects

  if ( model@isTextual == TRUE ) {
      initRandomEffects(model ) = omegas
      pos = grep( "error\\(",model@statements )
      if ( length(pos) > 0 ) {
          for ( indx in 1:length(pos)) {
              line=model@statements[[pos[[indx]]]]
              tokens=unlist(strsplit(line,"\\(|="))
              epsilonName=trimws(tokens[[2]],"both")
              if ( !is.na(fixedEffects[epsilonName]) ) {
                  newLine=sub("=.*[^)]",paste0("=",fixedEffects[epsilonName]),
                             line)
                  model@statements[[pos[[indx]]]]=newLine
              }
          }
      }
  } else {
    model@randomValues@values = omegas

    if ( length(model@errorModel@effectsList) > 0 )
    for ( i in 1:length(model@errorModel@effectsList ) ) {
        epsilonName = model@errorModel@effectsList[[i]]@epsilonName
        if ( !is.na(fixedEffects[epsilonName]) )
            model@errorModel@effectsList[[i]]@SD = fixedEffects[epsilonName]
    }
  }
  model = generatePML(model)
  model

}



.Last <- function(){

}
.First <- function(){


}


.onAttach <- function(libname,pkgname){
  for ( lib in c("shiny","XML","reshape","xpose","ggplot2","ssh","vpc","Certara.NLME8","Xpose.Nlme")  ) {
    stat = require(lib,character.only=TRUE,quietly=TRUE)
    if ( stat == FALSE ) {
      packageStartupMessage(paste0("WARNNING : Library : ",lib," is required for using RsNlme package"))
   }
  }
  NlmeWorkspace=Sys.getenv("NLME_WORKSPACE")
  assign("NlmeWorkspace",NlmeWorkspace,envir=.GlobalEnv)
}


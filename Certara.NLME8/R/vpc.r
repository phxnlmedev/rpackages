
library(parallel)
library(XML)

#' @export
VPC_XAXIS_T=1
#' @export
VPC_XAXIS_TAD=2
#' @export
VPC_XAXIS_PRED=3
#' @export
VPC_XAXIS_OTHER=4


#' @export
XAxisNames=c("t","TAD","PRED","Other")

#' @export
VPC_BIN_NONE=1
#' @export
VPC_BIN_KMEANS=2
#' @export
VPC_BIN_EXP_CENTERS=3
#' @export
VPC_BIN_EXP_BOUNDARIES=4

#' @export
VPC_PRED_NONE=1
#' @export
VPC_PRED_PROPOTIONAL=2
#' @export
VPC_PRED_ADDITIVE=3

#' @export
VPC_OBSERVE_T=1
#' @export
VPC_MULTI_T=1
#' @export
VPC_LL_T=1
#' @export
VPC_COUNT_T=1
#' @export
VPC_ORDINAL_T=1
#' @export
VPC_EVENT_T=1

#' @export
ObserveTypeNames=c("observe","multi","LL","count","ordinal","event")


#'
#' NlmeSimTableDef : Parameters for VPC/Simulation runs
#'
#' @param  name Name of the generated simulation file
#' @param  timesList  List of time values
#' @param  variablesList List of variables
#' @param  timeAfterDose Time after dose flag
#'
#' @export NlmeSimTableDef
#'
NlmeSimTableDef = setClass("NlmeSimTableDef",representation(
                                      name="character",
                                      timesList="character",
                                      variablesList="character",
                                      timeAfterDose="logical"))


setMethod("initialize","NlmeSimTableDef",
    function(.Object,
             name="",
             timesList="",
             variablesList="",
             timeAfterDose=FALSE,...){
        .Object@name=name
        .Object@timesList=timesList
        .Object@variablesList=variablesList
        .Object@timeAfterDose=timeAfterDose
        .Object
    })

assign("NlmeSimTableDef",NlmeSimTableDef,env=.GlobalEnv)


#'
#' NlmeObservationVar : Describes an observation(observe,multi,...)
#'
#'
#' @param  name of observation variable
#' @param  type of observation
#' @param  xaxis One of:VPC_XAXIS_T,VPC_XAXIS_TAD,VPC_XAXIS_PRED,VPC_XAXIS_OTHER
#' @param  binningMethod VPC_BIN_NONE,VPC_BIN_KMEANS,VPC_BIN_EXP_CENTERS,VPC_BIN_EXP_BOUNDARIES
#' @param  binningOption comma separated list to specify centers or boundary values
#' @param  quantilesValues comma separated list
#' @param  quantilesSecondaryValues  comma separated list
#'
#' @export NlmeObservationVar
#'
#' @examples
#'
#' var = NlmeObservationVar(
#'                       name="Cobs",
#'                       type=VPC_OBSERVE_T,
#'                       xaxis=VPC_XAXIS_TAD,
#'                       binningMethod=VPC_BIN_NONE,
#'                       quantilesValues ="5,50,95")
#'

NlmeObservationVar = setClass("NlmeObservationVar",representation(
                                      name="character",
                                      type="numeric",
                                      xaxis="numeric",
                                      xaxisLabel="character",
                                      binningMethod="numeric",
                                      binningOption="character",
                                      timeToEvent="character",
                                      quantilesValues="character",
                                      isBql="logical",
                                      quantilesSecondaryValues="character"))

setMethod("initialize","NlmeObservationVar",
    function(.Object,
             name="",
             type=VPC_OBSERVE_T,
             xaxis=VPC_XAXIS_T,
             xaxisLabel="",
             binningMethod=VPC_BIN_NONE,
             binningOption="",
             timeToEvent="",
             quantilesValues="5,50,95",
             isBql=FALSE,
             quantilesSecondaryValues=""){
        .Object@name=name
        .Object@type=type
        .Object@xaxis=xaxis
        .Object@xaxisLabel=xaxisLabel
        .Object@binningMethod=binningMethod
        .Object@binningOption=binningOption
        .Object@timeToEvent=timeToEvent
        .Object@quantilesValues=quantilesValues
        .Object@isBql=isBql
        .Object@quantilesSecondaryValues=quantilesSecondaryValues
        .Object
    })


#'
#' @export
#'
GetObservationVariables <-function(dataset=NULL, modelLines=c())
{
    obsVars=c()
    if ( length(modelLines) == 0 )
        lines = DatasetGetObserveParams(dataset)
    else
        lines = modelLines
    for ( l in unlist(lines) ) {
#        l=gsub("\t","",l)
        type=which(sapply(ObserveTypeNames, grepl, l))[[1]]
        isBql= length(grep("bql",l)) != 0
        name=unlist(strsplit(l,split="[(,=,,]"))[2]
        obsVar=NlmeObservationVar(name=name,type=type,isBql=isBql)
        obsVars=c(obsVars,obsVar)
    }
    return(obsVars)
}

assign("GetObservationVariables",GetObservationVariables,env=.GlobalEnv)

setGeneric(name="observationParameters",
           def=function(.Object)
           {
               standardGeneric("observationParameters")
           })


#'
#' @export observationParameters
#'
setMethod(f="observationParameters",
    signature="NlmeObservationVar",
    definition=function(.Object){
        print(.Object)
    })
assign("observationParameters",observationParameters,env=.GlobalEnv)

#'
#' @export
#'
setGeneric(name="observationParameters<-",
           def=function(.Object,value)
           {
               standardGeneric("observationParameters<-")
           })


#'
#' @export observationParameters<-
#'
setMethod(f="observationParameters<-",
    signature="NlmeObservationVar",
    definition=function(.Object,value){
         if( ! is.na(value["name"]) )
             .Object@age = value["name"]
         if( ! is.na(value["xaxis"]) )
             .Object@xaxis = as.integer(value["xaxis"])
         if( ! is.na(value["xaxisLabel"]) )
             .Object@xaxisLabel = value["xaxisLabel"]
         if( ! is.na(value["binningMethod"]) )
             .Object@binningMethod = as.integer(value["binningMethod"])
         if( ! is.na(value["binningOption"]) )
             .Object@binningOption = value["binningOption"]
         if( ! is.na(value["timeToEvent"]) )
             .Object@timeToEvent = value["timeToEvent"]
         if( ! is.na(value["quantilesValues"]) )
             .Object@quantilesValues = value["quantilesValues"]
         if( ! is.na(value["quantilesSecondaryValues"]) )
             .Object@quantilesSecondaryValues =value["quantilesSecondaryValues"]
         if( ! is.na(value["isBql"]) )
             .Object@isBql = as.logical(value["isBql"])
	 return(.Object)
    })


#'
#' NlmeVpcParams : Parameters for VPC runs
#'
#' @param  numReplicates Number of replicates to simulate
#' @param  seed Random number generator seed
#' @param  predCorrection One of VPC_PRED_NONE,VPC_PRED_PROPOTIONAL,VPC_PRED_ADDITIVE
#' @param  predVarCorr flag to use Prediction Variance Correction
#' @param  stratifyColumns List of covariates for Stratified PC
#' @param  observactionVars (NlmeObservationVar)
#' @param  simulationTables Optional list of simulatio tables (NlmeSimTableDef)
#'
#' @export NlmeVpcParams
#'
#' @examples
#'
#' observe1 = NlmeObservationVar(name="Cobs",
#'                              type=VPC_OBSERVE_T,
#'                              xaxis=VPC_XAXIS_TAD,
#'                              binningMethod=VPC_BIN_NONE,
#'                              quantilesValues ="5,50,95")
#'
#' observe2 = NlmeObservationVar(name="Iobs",
#'                              type=VPC_MULTI_T,
#'                              xaxis=VPC_XAXIS_PRED,
#'                              quantilesValues ="5,50,95")
#'
#' observe3 = NlmeObservationVar(name="Eobs",
#'                              type=VPC_LL_T,
#'                              timeToEvent="seq(1,10)"
#'                              quantilesValues ="5,50,95")
#'
#'   table1=NlmeSimTableDef(name="simulate.csv",
#'                          timesList="0,2,4,12,24",
#'                          variablesList="V,Cl",
#'                          timeAfterDose=TRUE)
#'
#' vpc = NlmeVpcParams(numReplicates=10,
#'                       seed=29423,
#'                       predCorrection=VPC_PRED_PROPOTIONAL,
#'                       predVarCorr=TRUE,
#'                       stratifyColumns="sex,race,dosing",
#'                       observationVars=c(observe1,observe2,observe3),
#'                       simulationTables=c(table1))
#'
NlmeVpcParams = setClass("NlmeVpcParams",representation(
                                      numReplicates="numeric",
                                      seed="numeric",
                                      predCorrection="numeric",
                                      predVarCorr="logical",
                                      stratifyColumns="character",
                                      observationVars="list",
                                      simulationTables="list"))

assign("NlmeVpcParams",NlmeVpcParams,env=.GlobalEnv)

setMethod("initialize","NlmeVpcParams",
    function(.Object,
             numReplicates=2,
             seed=1234,
             predCorrection=VPC_PRED_NONE,
             predVarCorr=FALSE,
             stratifyColumns="",
#             observationVars=c(NlmeObservationVar()),
             observationVars=list(),
             simulationTables=list()){
        .Object@numReplicates=numReplicates
        .Object@seed=seed
        .Object@predCorrection=predCorrection
        .Object@predVarCorr=predVarCorr
        .Object@stratifyColumns=stratifyColumns
        .Object@observationVars=observationVars
        .Object@simulationTables=simulationTables
        .Object
    })

#'
#' NlmeSimulationParams : Parameters for simulation runs
#'
#' @param  numReplicates Number of replicates to simulate
#' @param  seed Random number generator seed
#' @param  simulationTables (NlmeSimTableDef)
#' @param  isPopulation  Simulating a population model(default=TRUE).  The rest of arguments applies to individual models only
#' @param  numPoints  Number of points in simulation
#' @param  maxXRange  Max value of independent variable
#' @param  yVariables comma separated list of Y variables
#' @param  simAtObs   Simulate values at observed values of ivar
#'
#' @export NlmeSimulationParams
#'
#' @examples
#'
#' table1=NlmeSimTableDef(name="simulate.csv",timesList="0,2,4,12,24",
#'                        variablesList="V,Cl",
#'                        timeAfterDose=TRUE)
#'
#' simParam = NlmeSimulationParams(numReplicates=10,
#'                       seed=29423,
#'                       simulationTables = c(table1))
#'
#' simParam = NlmeSimulationParams(isPopulation=FALSE,
#'                       numPoints=100,
#'                       maxXRange=50,
#'                       yVariables="C,A1",
#'                       simulationTables = c(table1))
#'
NlmeSimulationParams = setClass("NlmeSimulationParams",representation(
                                      numReplicates="numeric",
                                      seed="numeric",
                                      isPopulation="logical",
                                      numPoints="numeric",
                                      maxXRange="numeric",
                                      yVariables="character",
                                      simAtObs="logical",
                                      simulationTables="list"))

assign("NlmeSimulationParams",NlmeSimulationParams,env=.GlobalEnv)

setMethod("initialize","NlmeSimulationParams",
    function(.Object,
             numReplicates=2,
             seed=1234,
             isPopulation=TRUE,
             numPoints=100,
             maxXRange=50,
             yVariables="",
             simAtObs=FALSE,
             simulationTables=c(NlmeSimTableDef())){
        .Object@numReplicates=numReplicates
        .Object@seed=seed
        .Object@isPopulation=isPopulation
        .Object@numPoints=numPoints
        .Object@maxXRange=maxXRange
        .Object@yVariables=yVariables
        .Object@simAtObs=simAtObs
        .Object@simulationTables=simulationTables
        .Object
    })


#'
#' RunVpcSimulation() : Method to execute an NLME VPC simulation
#'
#' @param  hostPlatform How to execute the run(NlmeParallelHost)
#' @param  dataset Dataset and model information(NlmeDataset)
#' @param  params Engine parameters(NlmeEngineExtraParams)
#' @param  vpcParams VPC parameters(NlmeVpcParams)
#' @param  runInBackground TRUE will run in background and return prompt(Bool)
#'
#' @export RunVpcSimulation
#'
#' @examples
#'
#' dataset = NlmeDataset()
#'
#' vpcParams = NlmeVpcParams()
#'
#' param = NlmeEngineExtraParams(PARAMS_METHOD=METHOD_FOCE_LB,
#'                              PARAMS_NUM_ITERATIONS=1000)
#'
#' job = RunVpcSimulation(defaultHost,dataset,params,vpcParams,simParams)
#'
RunVpcSimulation <-function(
                              hostPlatform,
                              dataset,
                              params,
                              vpcParams=NULL,
                              simParams=NULL,
                              runInBackground=TRUE)
{

    workFlow="WorkFlow"
    cleanupFromPreviousRun()
    if ( attr(hostPlatform,"hostType")== "Windows" )
        runInBackground=FALSE
    argsFile=GenerateControlfile(dataset, params,workFlow,vpcOption=vpcParams,simOption=simParams)
    cwd = getwd()

    argsList=list()
    argsList=c(argsList,"GENERIC")
    argsList=c(argsList,attr(attr(hostPlatform,"parallelMethod"),"method"))
    argsList=c(argsList,attr(hostPlatform,"installationDirectory"))
    argsList=c(argsList,attr(hostPlatform,"sharedDirectory"))
    argsList=c(argsList,cwd)
    argsList=c(argsList,argsFile)

    argsList=c(argsList,attr(hostPlatform,"numCores"))
    argsList=c(argsList,workFlow)

   if ( is.null(vpcParams))
       jobName="Simulation"
   else
       jobName="VPC"
   job=SimpleNlmeJob(jobType=jobName,
               localDir=cwd,
               remoteDir=cwd,
               host=hostPlatform,
               argsList=argsList,
               argsFile=argsFile,
               workflow=workFlow,
               runInBackground=runInBackground)

   status=executeJob(job)

    return(job)
}

#'
#' simmodel
#'
#' Method to execute an NLME simulation
#'
#' @param  hostPlatform How to execute the run(NlmeParallelHost)
#' @param  dataset Dataset and model information(NlmeDataset)
#' @param  simParams Simulation parameters(NlmeSimulationParam)
#' @param  model  optional PK/PD model
#' @param  runInBackground TRUE will run in background and return prompt(Bool)
#'
#' @export
#'
#' @examples
#'
#'
#' SimTableObs = NlmeSimTableDef(name = "SimTableObs.csv",
#'                                  timesList = "0,1,2,4,4.9,55.1,56,57,59,60",
#'                                  variablesList = "C, CObs",
#'                                  timeAfterDose = FALSE)
#'
#' simParams = NlmeSimulationParams(numReplicates = 50,
#'                             seed = 3527,
#'                             simulationTables = c(SimTableObs))
#'
#' job = simmodel(defaultHost,dataset,simParams,model)
#'
simmodel <-function( hostPlatform,
                     dataset,
                     simParams= NULL,
                     model = NULL,
                     runInBackground=TRUE)
{
    params = NlmeEngineExtraParams(PARAMS_METHOD=METHOD_NAIVE_POOLED,
                                   PARAMS_NUM_ITERATIONS=0)
    if ( ! is.null(model) ) {
        writeDefaultFiles(model=model,dataset=dataset,simParams=simParams)
        simParams@isPopulation = model@isPopulation
    }
    return(RunVpcSimulation(hostPlatform=hostPlatform,params=params,dataset=dataset,simParams=simParams,runInBackground= runInBackground))
}

#'
#' vpcmodel
#'
#' Method to execute an NLME visual predictive check
#'
#' @param  hostPlatform How to execute the run(NlmeParallelHost)
#' @param  dataset Dataset and model information(NlmeDataset)
#' @param  vpcParams VPC parameters(NlmeVpcParam)
#' @param  model  optional PK/PD model
#' @param  runInBackground TRUE will run in background and return prompt(Bool)
#'
#' @export
#'
#' @examples
#'
#' obsVars = GetObservationVariables(dataset)
#'
#' observationParameters(obsVars[[1]])=c(xaxis=VPC_XAXIS_T,
#'                                       binningMethod=VPC_BIN_NONE,
#'                                       quantilesValues ="5,50,95")
#'
#' vpcParams = NlmeVpcParams(numReplicates=2,
#'                        seed=1234,
#'                        observationVars=obsVars)
#'
#'
#' job = vpcmodel(defaultHost,dataset,vpcParams,model)
#'
vpcmodel <-function( hostPlatform,
                     dataset,
                     vpcParams = NULL ,
                     model = NULL,
                     runInBackground=TRUE)
{
    params = NlmeEngineExtraParams(PARAMS_METHOD=METHOD_NAIVE_POOLED,
                                   PARAMS_NUM_ITERATIONS=0)
    if ( ! is.null(model) )
        writeDefaultFiles(model=model,dataset=dataset,simParams=vpcParams)
    return(RunVpcSimulation(hostPlatform=hostPlatform,params=params,dataset=dataset,vpcParams=vpcParams,runInBackground= runInBackground))
}


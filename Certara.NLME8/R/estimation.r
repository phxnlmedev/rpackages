

    



#'
#' @export
#'
generateFirstFewArguments <-function(jobType,argsFile)
{
    argList=c()
    argList=c(argList,"jobType")
    argList=c(argList,attr(hostPlatform,"parallelMethod"))
    argList=c(argList,attr(hostPlatform,"installationDirectory"))
    argList=c(argList,attr(hostPlatform,"sharedDirectory"))
    argList=c(argList,cwd)
    argList=c(argList,argsFile)

    return(argList)
}

#'
#'
#' fitmodel() : Method to execute an NLME simple estimation
#'
#' @param  hostPlatform How to execute the run(NlmeParallelHost)
#' @param  dataset Dataset and model information(NlmeDataset)
#' @param  params Engine parameters(NlmeEngineExtraParams)
#' @param  model  Optional PK/PD model
#' @param  runInBackground TRUE will run in background and return prompt(Bool)
#' 
#' @export fitmodel  
#' 
#' @examples 
#'
#' input=read.csv("PKPD_Data.csv")
#' 
#' model = pkmodel(numComp=1,
#'                absorption = Extravascular,
#'                modelName="PkModel")
#'
#' dataset = defaultDataset(model,input)
#'
#' initColMapping(model)= input
#'
#' modelColumnMapping(model)=c(id="ID", CObs="Conc",Aa="Dose")
#'
#' initFixedEffects(model)=c(tvV=16,tvCl=41,tvV2=7,tvCl2=14)
#'
#' host = NlmeParallelHost(sharedDirectory=Sys.getenv("NLME_ROOT_DIRECTORY"),
#'                        parallelMethod=NlmeParallelMethod("LOCAL_MPI"),
#'                        hostName="MPI",
#'                        numCores=4)
#'
#' params = NlmeEngineExtraParams(PARAMS_METHOD=METHOD_FOCE_LB,
#'                              PARAMS_NUM_ITERATIONS=1000)
#' 
#' job = fitmodel(host,dataset,params,model)
#'
fitmodel <-function( hostPlatform,
                     dataset,
                     params = NULL,
                     model = NULL,
                     runInBackground=TRUE)
{
    if ( ! is.null(model) )
        writeDefaultFiles(model=model,dataset=dataset)

    return(RunSimpleEstimation(hostPlatform,dataset,params,runInBackground))
}




#'
#'
#' RunSimpleEstimation() : Method to execute an NLME simple estimation
#'
#' @param  hostPlatform How to execute the run(NlmeParallelHost)
#' @param  dataset Dataset and model information(NlmeDataset)
#' @param  params Engine parameters(NlmeEngineExtraParams)
#' @param  runInBackground TRUE will run in background and return prompt(Bool)
#' 
#' @export RunSimpleEstimation  
#' 
#' @examples 
#' 
#' dataset = NlmeDataset()
#' 
#' param = NlmeEngineExtraParams(PARAMS_METHOD=METHOD_FOCE_LB,
#'                              PARAMS_NUM_ITERATIONS=1000)
#' 
#' job = RunSimpleEstimation(defaultHost,dataset,params)
#'
RunSimpleEstimation <-function(
                              hostPlatform,
                              dataset,
                              params,
                              runInBackground=TRUE)
{

    workFlow="WorkFlow"
 #   cleanupFromPreviousRun()
    if ( attr(hostPlatform,"hostType")== "Windows" )
        runInBackground=FALSE
   
    argsFile=GenerateControlfile(dataset, params,workFlow)

  
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
   
   
   
    job=SimpleNlmeJob(jobType="Estimation",
                localDir=cwd,
                remoteDir=cwd,
                host=hostPlatform,
                argsList=argsList,
                argsFile=argsFile,
                workflow=workFlow,
                runInBackground=runInBackground)
   
    status=executeJob(job)
  
#    library(parallel)
#
#    if ( runInBackground == TRUE ) 
#        mcparallel(performParallelNLMERun(argList))
#    else
#        performParallelNLMERun(argList,reportProgress=TRUE)
    
    return(job)
}



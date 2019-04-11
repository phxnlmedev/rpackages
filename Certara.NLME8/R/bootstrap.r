#'
#' NlmeBootstrapParams : Represents parameters for a bootstrap run
#'
#' @param numReplicates Number of bootstrap replicates to run
#' @param initialEstimates Run and initial estimation to get initial values(1|0)
#' @param numRetries Number of times to retry a failed replicated
#' @param randomNumSeed Seed for random number generator
#' @param confidenceLevel Confidence Level
#' @param stratifyColumns What columns to stratify on(comma separated)
#'
#' @export NlmeBootstrapParams
#'
#' @examples
#'
#' boot = NlmeBootstrapParams(numReplicates=50,
#'                            randomNumSeed=1234)
#'
NlmeBootstrapParams = setClass("NlmeBootstrapParams",representation(
                                      numReplicates="numeric",
                                      initialEstimates="numeric",
                                      numRetries="numeric",
                                      randomNumSeed="numeric",
                                      confidenceLevel="numeric",
                                      stratifyColumns="character"))
assign("NlmeBootstrapParams",NlmeBootstrapParams,env=.GlobalEnv)

setMethod("initialize","NlmeBootstrapParams",
    function(.Object,numReplicates=3,initialEstimates=1,numRetries=2,randomNumSeed=1234,confidenceLevel=95,stratifyColumns="",...){
        .Object@numReplicates=numReplicates
        .Object@initialEstimates=initialEstimates
        .Object@numRetries=numRetries
        .Object@randomNumSeed=randomNumSeed
        .Object@confidenceLevel=confidenceLevel
        .Object@stratifyColumns=stratifyColumns
        .Object
    })

#'
#' bootstrap() : Method to execute an NLME Bootstrap 
#'
#' @param  hostPlatform How to execute the run(NlmeParallelHost)
#' @param  dataset Dataset and model information(NlmeDataset)
#' @param  params Engine parameters(NlmeEngineExtraParams)
#' @param  bootParams Bootstrap parameters(NlmeBootstrapParams)
#' @param  model Optional PK/PD model
#' @param  runInBackground TRUE will run in background and return prompt(Bool)
#' 
#' @export 
#' 
#' @examples 
#' 
#' dataset = NlmeDataset()
#' 
#' params = NlmeEngineExtraParams(PARAMS_METHOD=METHOD_FOCE_LB,
#'                              PARAMS_NUM_ITERATIONS=1000)
#' bootParams = NlmeBootstrapParams(numReplicates=5, randomNumSeed=1234)
#' 
#' job = bootstrap(defaultHost,dataset,params,bootParams,model)
#'
#' while (!(NlmeJobStatus(job) == "Finished" || NlmeJobStatus(job) == "Failed") )
#' {
#'     print(NlmeJobStatus(job))
#'     print(job)
#'     Sys.sleep(5)
#' }
#'


bootstrap <-function(
                        hostPlatform,
                        dataset,
                        params,
                        bootParams,
                        model = NULL,
                        runInBackground=TRUE)
{
    if ( ! is.null(model) )
        writeDefaultFiles(model=model,dataset=dataset)

    return(RunBootstrap(
                        hostPlatform,
                        dataset,
                        params,
                        bootParams,
                        runInBackground))
}

#'
#' RunBootstrap() : Method to execute an NLME Bootstrap 
#'
#' @param  hostPlatform How to execute the run(NlmeParallelHost)
#' @param  dataset Dataset and model information(NlmeDataset)
#' @param  params Engine parameters(NlmeEngineExtraParams)
#' @param  bootParams Bootstrap parameters(NlmeBootstrapParams)
#' @param  runInBackground TRUE will run in background and return prompt(Bool)
#' 
#' @export RunBootstrap  
#' 
#' @examples 
#' 
#' dataset = NlmeDataset()
#' 
#' bootParams = NlmeBootstrapParams(numReplicates=50,randomNumSeed=1234)
#' 
#' param = NlmeEngineExtraParams(PARAMS_METHOD=METHOD_FOCE_LB,
#'                              PARAMS_NUM_ITERATIONS=1000)
#' bootParam = NlmeBootstrapParams(numReplicates=50, randomNumSeed=1234)
#' 
#' job = RunBootstrap(defaultHost,dataset,params,bootParams)
#'
#' while (!(NlmeJobStatus(job) == "Finished" || NlmeJobStatus(job) == "Failed") )
#' {
#'     print(NlmeJobStatus(job))
#'     print(job)
#'     Sys.sleep(5)
#' }
#'


RunBootstrap <-function(
                        hostPlatform,
                        dataset,
                        params,
                        bootParams,
                        runInBackground=TRUE)
{

    workFlow="WorkFlow"
    #cleanupFromPreviousRun()
    if ( attr(hostPlatform,"hostType")== "Windows" )
        runInBackground=FALSE
    argsFile=GenerateControlfile(dataset, params,workFlow,
                                 bootStratify=attr(bootParams,"stratifyColumns"))
    cwd = getwd()
 
    argsList=list()
    argsList=c(argsList,attr(attr(hostPlatform,"parallelMethod"),"method"))
    argsList=c(argsList,attr(hostPlatform,"installationDirectory"))
    argsList=c(argsList,attr(hostPlatform,"sharedDirectory"))
    argsList=c(argsList,attr(hostPlatform,"numCores"))
    argsList=c(argsList,cwd)
    argsList=c(argsList,argsFile)

    argsList=c(argsList,attr(bootParams,"numReplicates"))
    argsList=c(argsList,attr(bootParams,"initialEstimates"))
    argsList=c(argsList,attr(bootParams,"numRetries"))
    argsList=c(argsList,attr(bootParams,"randomNumSeed"))
    argsList=c(argsList,attr(bootParams,"confidenceLevel"))
    argsList=c(argsList,workFlow)
    


    job=BootNlmeJob(jobType="Bootstrap",
                      localDir=cwd,
                      remoteDir=cwd,
                      host=hostPlatform,
                      argsList=argsList,
                      argsFile=argsFile,
                      workflow=workFlow,
                      runInBackground=runInBackground,
                      boot = bootParams)
    
    status=executeJob(job)
    
    return(job)

}

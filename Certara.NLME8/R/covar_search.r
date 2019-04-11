#' NlmeStepwiseParams : NLME stepwise parameters
#'
#' Class represents an NLME Stepwise search parameters
#'
#' @param addPValue     Threshold for adding a covariate effect
#' @param removePValue  Threshold for removing a covariate effect
#' @param method        -2LL|AIC|BIC
#' @export NlmeStepwiseParams
#' @examples
#'  NlmeStepwiseParams(addPValue,removePValue,method)
#'
NlmeStepwiseParams = setClass("NlmeStepwiseParams",representation(
                                      addPValue="numeric",
                                      removePValue="numeric",
                                      method="character"))
assign("NlmeStepwiseParams",NlmeStepwiseParams,env=.GlobalEnv)

setMethod("initialize","NlmeStepwiseParams",
    function(.Object,
              addPValue=0.01,
              removePValue=0.001,
              method="-2LL"){
        .Object@addPValue=addPValue
        .Object@removePValue=removePValue
        .Object@method=method
        .Object
    })

#'
#' @export
#'
NlmeStepwiseParamsString <-function(covarModel,
                         stepwiseParams)
{
    return(sprintf("%s:%s",
                    attr(stepwiseParams,"method"),
                    attr(covarModel,"degreesOfFreedom")))
}



#' NlmeCovariateEffectModel : NLME covariate effects model object class
#'
#' Class represents an NLME covariate effects model
#'
#' @param numCovariates     Number of covariate effects
#' @param covariateList     Comma separted list of covariate effects names
#' @param scenarioNames     Comma separted list of scenario names
#' @param isDefault         Comma separted list of flags
#' @param degreesOfFreedom  Comma separted list of degress of freedome
#' @keywords NLME, NlmeCovariateEffectModel
#' @export NlmeCovariateEffectModel
#' @examples
#' NlmeCovariateEffectModel(...)
#'
NlmeCovariateEffectModel=setClass("NlmeCovariateEffectModel",representation(
                                      numCovariates="numeric",
                                      covariateList="character",
                                      scenarioNames="character",
                                      isDefault="character",
                                      degreesOfFreedom="character"))

assign("NlmeCovariateEffectModel",NlmeCovariateEffectModel,env=.GlobalEnv)

setMethod("initialize","NlmeCovariateEffectModel",
    function(.Object,covariateList,scenarioNames,isDefault,degreesOfFreedom,...){
        .Object@covariateList=covariateList
        .Object@numCovariates=length(unlist(strsplit(.Object@covariateList,",")))
        .Object@scenarioNames=scenarioNames
        .Object@isDefault=isDefault
        .Object@degreesOfFreedom=degreesOfFreedom
        .Object
    })


#'
#' @export
#'
ReadNlmeCovariateEffectModel <-function(covariateEffectFile)
{
    lines=readLines(covariateEffectFile)
    #covarList=paste(lines,collapse=',')
    covars=c()
    degrees=c()
    scenarioNames=c()
    isDefault=c()
    for ( l in lines )
    {
        tokens=unlist(strsplit(l,","))
        if ( length(tokens) == 1 )
        {
            covars = c(covars,tokens[[1]])
            degrees= c(degrees,1)
            scenarioNames= c(scenarioNames,"")
        }
        else if ( length(tokens) == 2 )
        {
            covars = c(covars,tokens[[1]])
            degrees= c(degrees,tokens[[2]])
            scenarioNames= c(scenarioNames,"")
            isDefault= c(isDefault,"")
        }
        else if ( length(tokens) == 3 )
        {
            covars = c(covars,tokens[[1]])
            degrees= c(degrees,tokens[[2]])
            scenarioNames= c(scenarioNames,tokens[[3]])
            isDefault= c(isDefault,"")
        }
        else
        {
            covars = c(covars,tokens[[1]])
            degrees= c(degrees,tokens[[2]])
            scenarioNames= c(scenarioNames,tokens[[3]])
            isDefault= c(isDefault,tokens[[4]])
        }
    }
    covarList=paste(covars,collapse=',')
    degreesList=paste(degrees,collapse=',')
    scenariosList=paste(scenarioNames,collapse=',')
    isDefaultList=paste(isDefault,collapse=',')
    cm = NlmeCovariateEffectModel(covarList,scenariosList,isDefaultList,degreesList)
    return(cm)
}

#'
#' shotgunSearch()
#'
#' Method to execute an NLME shotgun covariate search
#'
#' @param  hostPlatform How to execute the run(NlmeParallelHost)
#' @param  dataset Dataset and model information(NlmeDataset)
#' @param  params Engine parameters(NlmeEngineExtraParams)
#' @param  covariateModel Covariate Effects Model(NlmeCovariateEffectModel)
#' @param  shotgunParam shotgun parameters(NlmeStepwiseParams)
#' @param  dataset Optional PK/PD model
#' @param  runInBackground TRUE will run in background and return prompt(Bool)
#'
#' @examples
#'
#' dataset = NlmeDataset()
#'
#' param = NlmeEngineExtraParams(PARAMS_METHOD=METHOD_FOCE_LB,
#'                              PARAMS_NUM_ITERATIONS=1000)
#'
#' job = shotgunSearch(defaultHost,
#'                     dataset,
#'                     params,
#'                     covariateModel(model),
#'                     model)
#'
#' @export
#'
shotgunSearch <-function( hostPlatform,
                            dataset,
                            params= NULL ,
                            covariateModel,
                            model = NULL,
                            runInBackground=TRUE)
{
    if ( ! is.null(model) )
        writeDefaultFiles(model=model,dataset=dataset)

    return(RunShotgunSearch(hostPlatform,
                            dataset,
                            params,
                            covariateModel,
                            runInBackground))
}


#'
#' RunShotgunSearch : Method to execute an NLME shotgun covariate search
#'
#' @param  hostPlatform How to execute the run(NlmeParallelHost)
#' @param  dataset Dataset and model information(NlmeDataset)
#' @param  params Engine parameters(NlmeEngineExtraParams)
#' @param  covariateModel Covariate Effects Model(NlmeCovariateEffectModel)
#' @param  runInBackground TRUE will run in background and return prompt(Bool)
#'
#' @examples
#'
#' dataset = NlmeDataset()
#'
#' param = NlmeEngineExtraParams(PARAMS_METHOD=METHOD_FOCE_LB,
#'                              PARAMS_NUM_ITERATIONS=1000)
#' covariateModel = ReadNlmeCovariateEffectModel("covariates.txt")
#'
#' job = RunShotgunSearch(defaultHost,dataset,params,covariateModel)
#'
#' @export RunShotgunSearch
#'
RunShotgunSearch <-function(
                            hostPlatform,
                            dataset,
                            params,
                            covariateModel,
                            runInBackground=TRUE)
{

    workFlow="WorkFlow"
    cleanupFromPreviousRun()
    if ( attr(hostPlatform,"hostType")== "Windows" )
        runInBackground=FALSE
    argsFile=GenerateControlfile(dataset, params,workFlow)
    stuff=parseControlFile(argsFile)
    modelFile=stuff[1]
    extraArgsFile=stuff[4]
    filesToCopy=stuff[6]

    nlmeControlFile="newNlmeControlFile.txt"
    nlmeArgsFile="newNlmeargsCombined.txt"

    filesToCopy=paste0(filesToCopy," ", nlmeControlFile," ",  nlmeArgsFile)

    generateCovarSearchArgsFile(nlmeControlFile,
                                nlmeArgsFile,
                                attr(dataset,"modelFile"),
                                extraArgsFile,
                                filesToCopy,
                                attr(covariateModel,"numCovariates"),
                                gsub(","," ",
                                     attr(covariateModel,"covariateList"),
                                     fixed=TRUE))
    cwd = getwd()

    argList=list()
    argList=c(argList,"COVAR_SEARCH")
    argList=c(argList,attr(attr(hostPlatform,"parallelMethod"),"method"))
    argList=c(argList,attr(hostPlatform,"installationDirectory"))
    argList=c(argList,attr(hostPlatform,"sharedDirectory"))
    argList=c(argList,cwd)
    argList=c(argList,nlmeControlFile)
 #   argList=c(argList,attr(covariateModel,"numCovariates"))
 #   argList=c(argList,gsub(","," ",
 #                          attr(covariateModel,"covariateList"),fixed=TRUE))
    argList=c(argList,attr(hostPlatform,"numCores"))
    argList=c(argList,workFlow)


    job=ShotgunNlmeJob(jobType="Shotgun",
               localDir=cwd,
               remoteDir=cwd,
               host=hostPlatform,
               argsList=argList,
               argsFile=argsFile,
               workflow=workFlow,
               runInBackground=runInBackground)

    status=executeJob(job)
    return(job)

#    job=NlmeJob("Shotgun Covar Search",
#                cwd,
#                cwd,
#                hostPlatform)
#    library(parallel)
#    if ( runInBackground == TRUE )
#        mcparallel(performParallelNLMERun(argList))
#    else
#        performParallelNLMERun(argList,reportProgress=TRUE)
#    return(job)

}

#'
#' stepwiseSearch()
#'
#' Method to execute an NLME stepwise covariate search
#'
#' @param  hostPlatform How to execute the run(NlmeParallelHost)
#' @param  dataset Dataset and model information(NlmeDataset)
#' @param  params Engine parameters(NlmeEngineExtraParams)
#' @param  covariateModel Covariate Effects Model(NlmeCovariateEffectModel)
#' @param  stepwiseParam stepwise parameters(NlmeStepwiseParams)
#' @param  model Optional PK/PD model
#' @param  runInBackground TRUE will run in background and return prompt(Bool)
#'
#' @examples
#'
#' dataset = NlmeDataset()
#'
#' param = NlmeEngineExtraParams(PARAMS_METHOD=METHOD_FOCE_LB,
#'                              PARAMS_NUM_ITERATIONS=1000)
#'
#' sp = NlmeStepwiseParams(0.01, 0.001, "-2LL")
#' 
#' job=stepwiseSearch(host,
#'                    dataset,
#'                    params,
#'                    covariateModel(covarModel),
#'                    sp,
#'                    model)
#'
#' @export
#'
stepwiseSearch <-function(
                            hostPlatform,
                            dataset,
                            params= NULL,
                            covariateModel,
                            stepwiseParams,
                            model = NULL,
                            runInBackground=TRUE)
{
    if ( ! is.null(model) )
        writeDefaultFiles(model=model,dataset=dataset)
    return(RunStepwiseSearch(hostPlatform,
                            dataset,
                            params,
                            covariateModel,
                            stepwiseParams,
                            runInBackground))
}



#'
#' RunStepwiseSearch() : Method to execute an NLME stepwise covariate search
#'
#' @param  hostPlatform How to execute the run(NlmeParallelHost)
#' @param  dataset Dataset and model information(NlmeDataset)
#' @param  params Engine parameters(NlmeEngineExtraParams)
#' @param  covariateModel Covariate Effects Model(NlmeCovariateEffectModel)
#' @param  stepwiseParam stepwise parameters(NlmeStepwiseParams)
#' @param  runInBackground TRUE will run in background and return prompt(Bool)
#'
#' @examples
#'
#' dataset = NlmeDataset()
#'
#' param = NlmeEngineExtraParams(PARAMS_METHOD=METHOD_FOCE_LB,
#'                              PARAMS_NUM_ITERATIONS=1000)
#' covariateModel = ReadNlmeCovariateEffectModel("covariates.txt")
#'
#' stepwiseParam = NlmeStepwiseParams(0,01, 0.001, "-2LL")
#'
#' job = RunStepwiseSearch(defaultHost,dataset,params,covariateModel,stepwiseParam)
#'
#' @export RunStepwiseSearch
#'
RunStepwiseSearch <-function(
                            hostPlatform,
                            dataset,
                            params,
                            covariateModel,
                            stepwiseParams,
                            runInBackground=TRUE)
{

    workFlow="WorkFlow"
    cleanupFromPreviousRun()
    if ( attr(hostPlatform,"hostType")== "Windows" )
        runInBackground=FALSE
    argsFile=GenerateControlfile(dataset, params,workFlow)
    stuff=parseControlFile(argsFile)
    extraArgsFile=stuff[4]
    filesToCopy=stuff[6]

    cwd = getwd()

    argsList=list()
    argsList=c(argsList,attr(attr(hostPlatform,"parallelMethod"),"method"))
    argsList=c(argsList,attr(hostPlatform,"installationDirectory"))
    argsList=c(argsList,attr(hostPlatform,"sharedDirectory"))
    argsList=c(argsList,cwd)
    argsList=c(argsList,attr(dataset,"modelFile"))
    argsList=c(argsList,extraArgsFile)
    argsList=c(argsList,filesToCopy)
    argsList=c(argsList,attr(covariateModel,"numCovariates"))
    argsList=c(argsList,gsub(","," ",
                           attr(covariateModel,"covariateList"),fixed=TRUE))
    argsList=c(argsList,NlmeStepwiseParamsString(covariateModel,stepwiseParams))
    argsList=c(argsList,attr(stepwiseParams,"addPValue"))
    argsList=c(argsList,attr(stepwiseParams,"removePValue"))
    argsList=c(argsList,attr(hostPlatform,"numCores"))
    argsList=c(argsList,workFlow)
    print(argsList)

   job=StepwiseNlmeJob(jobType="Stepwise",
               localDir=cwd,
               remoteDir=cwd,
               host=hostPlatform,
               argsList=argsList,
               argsFile=argsFile,
               workflow=workFlow,
               runInBackground=runInBackground,
               stepParam=stepwiseParams)

   status=executeJob(job)

#    job=NlmeJob("Stepwise Covar Search",
#                cwd,
#                cwd,
#                hostPlatform)
#    library(parallel)
#
#    if ( runInBackground == TRUE )
#        mcparallel(performStepwiseCovarSearch(argList))
#    else
#        performStepwiseCovarSearch(argList,reportProgress=TRUE)
    return(job)
}


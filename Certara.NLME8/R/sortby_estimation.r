

#' NLME scenario object
#'
#' Class represents an NLME Scenario
#'
#' @param scenarioName   Name of the scenario
#' @param covariatesList Comma separated Indexs of covariates effects to use for this scenario
#'
#' @export NlmeScenario
#' @examples
#'
#' scenario1=NlmeScenario("SC0001","1")
#'     Use covariate effect index 1
#' scenario2=NlmeScenario("SC0002","")
#'     Do not use any covariate effects
#' scenario3=NlmeScenario("SC0002","2")
#'     Use second covariate effect in the model
#' scenario4=NlmeScenario("SC0002","1,2")
#'    Use 1st and 2nd covariate effect
#'
#'

NlmeScenario= setClass("NlmeScenario",slots=c(
                                      scenarioName="character",
                                      covariatesList="character",
                                      annotation="character"))

assign("NlmeScenario",NlmeScenario,env=.GlobalEnv)

setMethod("initialize","NlmeScenario",
    function(.Object,scenarioName,covariatesList,annotation=""){
        .Object@scenarioName=scenarioName
        .Object@covariatesList=covariatesList
        .Object@annotation=annotation
        .Object
    })


#' @export
getScenarioNames <-function(scenarios){
    names=""
    for ( s in scenarios ) {
        names=paste(names,attr(s,"scenarioName"))
    }
    return(names)
}


#' NLME Sort column object
#'
#' Class represents an NLME Sort columns
#'
#' @param sortColumnList     Comma separted list of covariate effects names
#' @export NlmeSortColumns
#' @examples
#' NlmeSortColumns("Country,City")
#'
NlmeSortColumns=setClass("NlmeSortColumns",representation(
                                      numSortColumns="numeric",
                                      sortColumnList="character"))

assign("NlmeSortColumns",NlmeSortColumns,env=.GlobalEnv)

setMethod("initialize","NlmeSortColumns",
    function(.Object,commaSeparatedListOfColumns,...){
        .Object@sortColumnList=commaSeparatedListOfColumns
        .Object@numSortColumns=length(unlist(strsplit(.Object@sortColumnList,",")))
        .Object
    })



#'
#' scenariofit() : Method to execute an NLME simple estimation on a set of scenario
#'
#' @param  hostPlatform How to execute the run(NlmeParallelHost)
#' @param  dataset Dataset and model information(NlmeDataset)
#' @param  params Engine parameters(NlmeEngineExtraParams)
#' @param  scenariosList list of scenario names(c())
#' @param  model Optional PK/PD model
#' @param  runInBackground TRUE will run in background and return prompt(Bool)
#'
#'
#' @examples
#'
#' dataset = NlmeDataset()
#'
#' param = NlmeEngineExtraParams(PARAMS_METHOD=METHOD_FOCE_LB,
#'                              PARAMS_NUM_ITERATIONS=1000)
#'
#' listCovariateEffectNames(model)
#'
#' scenario1=NlmeScenario("SC0001","1")
#' scenario2=NlmeScenario("SC0002","")
#' scenario3=NlmeScenario("SC0002","2")
#' scenario4=NlmeScenario("SC0002","1,2")
#' scenarios = c(scenario1,scenario2,scenario3,scenario4)
#'
#' job = scenariofit(defaultHost,dataset,params,scenarios,model)
#'
#' @export
#'
scenariofit <-function( hostPlatform,
                            dataset,
                            params,
                            scenarios,
                            model = NULL,
                            runInBackground=TRUE)
{
    sortColumns=NlmeSortColumns("")
    if ( ! is.null(model) )
        writeDefaultFiles(model=model,dataset=dataset)
    return(RunSortByEstimation(hostPlatform,dataset,params,
                              sortColumns,scenarios, runInBackground))


  }



#'
#' sortfit() : Method to execute an NLME simple estimation w sort keys
#'
#' @param  hostPlatform How to execute the run(NlmeParallelHost)
#' @param  dataset Dataset and model information(NlmeDataset)
#' @param  params Engine parameters(NlmeEngineExtraParams)
#' @param  sortList list of sort columns (NlmeSortColumns)
#' @param  scenariosList list of scenario names(c())
#' @param  model Optional PK/PD model
#' @param  runInBackground TRUE will run in background and return prompt(Bool)
#'
#'
#' @examples
#'
#' dataset = NlmeDataset()
#'
#' param = NlmeEngineExtraParams(PARAMS_METHOD=METHOD_FOCE_LB,
#'                              PARAMS_NUM_ITERATIONS=1000)
#' listCovariateEffectNames(model)
#'
#' scenario1=NlmeScenario("SC0001","1")
#' scenario2=NlmeScenario("SC0002","")
#' scenario3=NlmeScenario("SC0002","2")
#' scenario4=NlmeScenario("SC0002","1,2")
#' scenarios = c(scenario1,scenario2,scenario3,scenario4)
#'
#' sortColumns=NlmeSortColumns("group,sex")
#'
#' job = sortfit(defaultHost,dataset,params,sortColumns,scenarios,model)
#'
#' @export
#'
sortfit<-function(
                            hostPlatform,
                            dataset,
                            params = NULL ,
                            sortColumns,
                            scenarios=list(),
                            model=NULL,
                            runInBackground=TRUE)
{
    if ( ! is.null(model) )
        writeDefaultFiles(model=model,dataset=dataset)

    return(RunSortByEstimation(hostPlatform,
                               dataset,
                               params,
                               sortColumns,
                               scenarios, 
                               runInBackground))


  }





#'
#' RunSortByEstimation() : Method to execute multiple NLME estimation w sort key
#'
#' @param  hostPlatform How to execute the run(NlmeParallelHost)
#' @param  dataset Dataset and model information(NlmeDataset)
#' @param  params Engine parameters(NlmeEngineExtraParams)
#' @param  sortList list of sort columns (NlmeSortColumns)
#' @param  scenariosList list of scenario names(c())
#' @param  runInBackground TRUE will run in background and return prompt(Bool)
#'
#' @examples
#'
#' dataset = NlmeDataset()
#'
#' param = NlmeEngineExtraParams(PARAMS_METHOD=METHOD_FOCE_LB,
#'                              PARAMS_NUM_ITERATIONS=1000)
#' listCovariateEffectNames(model)
#'
#' scenario1=NlmeScenario("SC0001","1")
#' scenario2=NlmeScenario("SC0002","")
#' scenario3=NlmeScenario("SC0002","2")
#' scenario4=NlmeScenario("SC0002","1,2")
#' sortColumns=NlmeSortColumns("group,sex")
#' scenariosList = c(scenario1,scenario2,scenario3,secnario4)
#'
#' job = RunSortByEstimation(defaultHost,dataset,params,sortColumns,scenariosList,runInBackground)
#' @export RunSortByEstimation
#'
RunSortByEstimation <-function(
                            hostPlatform,
                            dataset,
                            params,
                            sortColumns,
                            scenarios=list(),
                            runInBackground=TRUE)
{
    workFlow="WorkFlow"
#    cleanupFromPreviousRun()
    if ( attr(hostPlatform,"hostType")== "Windows" )
        runInBackground=FALSE

    argsFile=GenerateControlfile(dataset, params,workFlow,scenarios=scenarios)

    cwd = getwd()

    argsList=list()
    argsList=c(argsList,attr(attr(hostPlatform,"parallelMethod"),"method"))
    argsList=c(argsList,attr(hostPlatform,"installationDirectory"))
    argsList=c(argsList,attr(hostPlatform,"sharedDirectory"))
    argsList=c(argsList,cwd)
    argsList=c(argsList,argsFile)

    argsList=c(argsList,attr(sortColumns,"numSortColumns"))
    argsList=c(argsList,gsub(","," ",attr(sortColumns,"sortColumnList")))
    argsList=c(argsList,attr(hostPlatform,"numCores"))

    if ( length(scenarios) == 0 )
        argsList=c(argsList,workFlow)
    else
        argsList=c(argsList,getScenarioNames(scenarios))

#    browser()


    job=SortByNlmeJob(jobType="Sort_By_Column",
                      localDir=cwd,
                      remoteDir=cwd,
                      host=hostPlatform,
                      argsList=argsList,
                      argsFile=argsFile,
                      sortColumns=sortColumns,
                      scenarios=scenarios,
                      workflow=workFlow,
                      runInBackground=runInBackground)


    status=executeJob(job)

    return(job)

}


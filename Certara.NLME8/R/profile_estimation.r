

#' NLME Profile variable
#'
#' Class represents an NLME profile pertubation variable
#'
#' @param fixed effect name
#' @param initial value
#' @param list of values to pertubate by(either delta or percentage)
#' @export NlmeProfileVar
#'
#' @examples
#' NlmeProfileVar("tvV",9.95,"-2,-1,0,1,2")
#'

NlmeProfileVar= setClass("NlmeProfileVar",slots=c(
                                      effectName="character",
                                      initialValue="numeric",
                                      pertubateValues="character"))

assign("NlmeProfileVar",NlmeProfileVar,env=.GlobalEnv)

setMethod("initialize","NlmeProfileVar",
    function(.Object,effectName,initialValue,pertubateValues){
        .Object@effectName=effectName
        .Object@initialValue=initialValue
        .Object@pertubateValues=pertubateValues
        .Object
    })

#'
#'
#' @export
#'
print.NlmeProfileVar <-function(obj)
{
    print(paste(attr(obj,"effectName"),",",
                attr(obj,"initialValue"),",",
                attr(obj,"pertubateValues"),sep=""))
}

#' NLME Profile variable
#'
#' Class represents an NLME profile pertubation variable
#'
#' @param How to apply profile variables. USE_DELTA|USE_PERCENTAGE
#' @param list of profile variables
#' @export NlmeProfileParameters
#'
#' @examples
#' profile1 = NlmeProfileVar("tvV","9.548","-2,-1,0,1,2")
#' profile2 = NlmeProfileVar("tvCl","3.219","-1,0,1")
#' profiles= NlmeProfileParameters("USE_DELTA",c(profile1,profile2))
#'

NlmeProfileParameters= setClass("NlmeProfileParameters",slots=c(
                                      howToPertubate="character",
                                      profileVars="list"))

assign("NlmeProfileParameters",NlmeProfileParameters,env=.GlobalEnv)

setMethod("initialize","NlmeProfileParameters",
    function(.Object,howToPertubate,
                     profileVars){
        .Object@howToPertubate=howToPertubate
        .Object@profileVars=profileVars
        .Object
    })

#'
#' @export
#'
getProfilesString <-function(profiles)
{
    strList=""
    for ( p in attr(profiles,"profileVars") ) {
        if ( strList == "" ) 
            strList = paste( attr(p,"effectName"),",",
                attr(p,"initialValue"),",",
                attr(p,"pertubateValues"),sep="")
        else 
            strList = paste(strList,paste( attr(p,"effectName"),",",
                attr(p,"initialValue"),",",
                attr(p,"pertubateValues"),sep=""),sep=" ")
    }
    return(strList)
}

#'
#'
#' profilePertubate() : Method to execute an NLME profile pertubation
#'
#' @param  hostPlatform How to execute the run(NlmeParallelHost)
#' @param  params Engine parameters(NlmeEngineExtraParams)
#' @param  profiles profiles to pertubate(NlmeProfileParameters)
#' @param  model PK/PD model
#' @param  sortColumns Optional list of columns to sort and fit(NlmeSortColumns)
#' @param  scenarios Optional list of scenarios to fit(NlmeScenario)
#' @param  runInBackground TRUE will run in background and return prompt(Bool)
#' 
#' @export 
#' 
#' @examples 
#' 
#' 
#' param = NlmeEngineExtraParams(PARAMS_METHOD=METHOD_FOCE_LB,
#'                              PARAMS_NUM_ITERATIONS=1000)
#' 
#' profile1 = NlmeProfileVar("tvV",9.548,"-2,-1,0,1,2")
#' profile2 = NlmeProfileVar("tvCl",0.919,"-0.5,0,1.5")
#' profiles= NlmeProfileParameters("USE_DELTA",c(profile1,profile2))
#' 
#' job = profilePertubate(defaultHost,params,profiles,model)
#'


profilePertubate<-function(
                              hostPlatform,
                              params= NULL,
                              profiles,
                              model = NULL,
                              sortColumns=NlmeSortColumns(""),
                              scenarios=list(),
                              runInBackground=TRUE)
{
    if ( ! is.null(model) ) {
        writeDefaultFiles(model=model,dataset=model@dataset)
        workingDir = model@modelInfo@workingDir
    }
    else 
        workingDir = getwd()
    return(RunProfilePertubation(hostPlatform,model@dataset,params,profiles,sortColumns,scenarios,runInBackground,workingDir=workingDir))
}


#'
#'
#' RunProfilePertubation() : Method to execute an NLME profile pertubation
#'
#' @param  hostPlatform How to execute the run(NlmeParallelHost)
#' @param  dataset Dataset and model information(NlmeDataset)
#' @param  params Engine parameters(NlmeEngineExtraParams)
#' @param  profiles profiles to pertubate(NlmeProfileParameters)
#' @param  sortColumns Optional list of columns to sort and fit(NlmeSortColumns)
#' @param  scenarios Optional list of scenarios to fit(NlmeScenario)
#' @param  runInBackground TRUE will run in background and return prompt(Bool)
#' @param  workingDir where to run the job
#' 
#' @export RunProfilePertubation  
#' 
#' @examples 
#' 
#' dataset = NlmeDataset()
#' 
#' param = NlmeEngineExtraParams(PARAMS_METHOD=METHOD_FOCE_LB,
#'                              PARAMS_NUM_ITERATIONS=1000)
#' 
#' sortColumns=NlmeSortColumns("")
#' profile1 = NlmeProfileVar("tvV",9.548,"-2,-1,0,1,2")
#' profile2 = NlmeProfileVar("tvCl",0.919,"-0.5,0,1.5")
#' profiles= NlmeProfileParameters("USE_DELTA",c(profile1,profile2))
#' 
#' job = RunProfilePertubation(defaultHost,dataset,params,profiles,sortColumns,scenarios)
#'
RunProfilePertubation<-function(
                              hostPlatform,
                              dataset,
                              params,
                              profiles,
                              sortColumns,
                              scenarios=list(),
                              runInBackground=TRUE,
                              workingDir=NULL)
{

    workFlow="WorkFlow"
    cleanupFromPreviousRun()
    if ( attr(hostPlatform,"hostType")== "Windows" )
        runInBackground=FALSE
    if (  is.null(workingDir ))
        cwd = getwd()
    else
        cwd = workingDir

    argsFile=GenerateControlfile(dataset, params,workFlow,scenarios=scenarios,
                                 workingDir = cwd)



    argsList=list()
    argsList=c(argsList,attr(attr(hostPlatform,"parallelMethod"),"method"))
    argsList=c(argsList,attr(hostPlatform,"installationDirectory"))
    argsList=c(argsList,attr(hostPlatform,"sharedDirectory"))
    argsList=c(argsList,cwd)
    argsList=c(argsList,argsFile)

    argsList=c(argsList,attr(sortColumns,"numSortColumns"))
    argsList=c(argsList,gsub(","," ",attr(sortColumns,"sortColumnList")))
    argsList=c(argsList,getProfilesString(profiles))
    argsList=c(argsList,attr(profiles,"howToPertubate"))
    argsList=c(argsList,attr(hostPlatform,"numCores"))
    if ( length(scenarios) == 0 )
        argsList=c(argsList,workFlow)
    else
        argsList=c(argsList,getScenarioNames(scenarios))


    job=ProfileNlmeJob(jobType="Profile_Pertubation",
                      localDir=cwd,
                      remoteDir=cwd,
                      host=hostPlatform,
                      argsList=argsList,
                      argsFile=argsFile,
                      profiles=profiles,
                      sortColumns=sortColumns,
                      scenarios=scenarios,
                      workflow=workFlow,
                      runInBackground=runInBackground)


    status=executeJob(job)

    return(job)
}

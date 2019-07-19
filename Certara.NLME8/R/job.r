

#library(ssh)

setClass("NlmeSortColumns")
setClass("NlmeProfileParameters")



#' @export getFilesToTransfer
getFilesToTransfer <- function(dirName,argsFile){
  if ( dirname(argsFile) == "." ) 
      lines=readLines(paste0(dirName,"/",argsFile))
  else
      lines=readLines(argsFile)
  filesLine=lines[2]
  files=unlist(strsplit(filesLine,split=" "))
  if ( dirName != "." ) {
      for ( indx in 1:length(files) ) {
          dn = dirname(files[indx])
          if ( dn == "." ) 
              files[indx] = paste0(dirName,"/",files[indx])
      }
  }
  files
}

#' @export getResultsList
getResultsList <- function(localDir,argsFile){

 if ( dirname(argsFile) == "." )
     lines=readLines(paste0(localDir,"/",argsFile))
 else
     lines=readLines(argsFile)
  filesLine=lines[3]
  files=unlist(strsplit(filesLine,split=" "))
  files
}

#' @export
copyTemplateFiles <-function(host,localDir){
    defaultRootDirectory=Sys.getenv("NLME_ROOT_DIRECTORY")
    if (  defaultRootDirectory != "" ) {
        installDirectory=paste0(defaultRootDirectory,"/InstallDirNLME")
        templateFile=""
        parallelModeString=attr(attr(host,"parallelMethod"),"method")
        if ( length(grep("TORQUE",parallelModeString)) ==1)
            templateFile="batchtools.torque.tmpl"
        if ( length(grep("SGE",parallelModeString)) ==1)
            templateFile="batchtools.sge.tmpl"
        if (length(grep("LSF",parallelModeString)) ==1 )
            templateFile="batchtools.lsf.tmpl"
        if ( templateFile != "" ) {
            copyFrom=paste0(installDirectory,"/",templateFile)
            copyTo=paste0(localDir,"/",templateFile)
            if ( !file.exists(copyTo) ) {
                file.copy(copyFrom,copyTo)
            }
        }
    }
}

#'
#'
#'
#' @export SimpleNlmeJob
#'
SimpleNlmeJob=setClass("SimpleNlmeJob",representation(
  jobType="character",
  localDir="character",
  remoteDir="character",
  host="NlmeParallelHost",
  argsList="list",
  argsFile="character",
  scriptFile="character",
  workflow="character",
  runInBackground="logical"))

 # contains=c("logical","character","list","NlmeParallelHost"))


setMethod("initialize","SimpleNlmeJob",
          function(.Object,...,
                   jobType="",
                   localDir="",
                   remoteDir="",
                   host=NlmeParallelHost(),
                   argsList=list(),
                   argsFile="",
                   workflow="",
                   runInBackground= FALSE){
              cat("NLME Job\n")
            if ( host@isLocal ) {
                copyTemplateFiles(host,localDir)
                .Object@localDir = localDir
                .Object@remoteDir = localDir
                .Object@host = host
                .Object@argsFile = argsFile
                .Object@argsList = argsList
                .Object@workflow = workflow
                .Object@scriptFile=generateScript(.Object)
            }
            else {
            
                .Object@localDir = localDir
                tmpdir=mktempdir(host@remoteExecutor)
                .Object@remoteDir = tmpdir
                .Object@host = host
                .Object@argsFile = argsFile
                .Object@argsList = argsList
                .Object@workflow = workflow
                .Object@scriptFile=generateScript(.Object)
                          
              }
            
              callNextMethod(.Object, ..., 
                             jobType=jobType,
                             remoteDir=.Object@remoteDir,
                             localDir=localDir,
                             host=host,
                             argsList=argsList,
                             argsFile=argsFile,
                             workflow=workflow,
                             runInBackground=runInBackground
                             )
              
            })

removeCommandFile <-function(){
    if ( file.exists("nlme.cmd") ) 
        file.remove("nlme.cmd") 
}
setGeneric(name="executeJob",
           def=function(.Object)
           {
             standardGeneric("executeJob")
           })


setMethod("executeJob",
          signature="SimpleNlmeJob",
          definition = function(.Object){
            runInBackground=.Object@runInBackground
            if ( attr(.Object@host,"hostType")== "Windows" )
              runInBackground=FALSE

            removeCommandFile() 
            if ( .Object@host@isLocal == TRUE ) {
              library(parallel)
              
              if ( runInBackground == TRUE )
                mcparallel(performParallelNLMERun(as.character(.Object@argsList)))
              else {
                performParallelNLMERun(as.character(.Object@argsList),reportProgress=TRUE)
                print(.Object)
              }
              
            } else {
              
              files=getFilesToTransfer(.Object@localDir,.Object@argsFile)
              all=c(files,.Object@scriptFile,.Object@argsFile)
              stat=uploadFiles(.Object@host@remoteExecutor,.Object@remoteDir,all)
              fullPath=paste(.Object@remoteDir,basename(.Object@scriptFile),sep="/")
              
              cmds=c(paste0("chmod 777 ",fullPath),
                     paste0("dos2unix ",fullPath),
                     paste0("cd ",.Object@remoteDir," ; env > env.log ; nohup ",fullPath, " > NlmeRemote.LOG 2>&1 &"))
              
              for ( cmd in cmds ) {
                print(cmd)
                stat=execCommand(.Object@host@remoteExecutor,cmd)
              }
              
              if ( .Object@runInBackground == FALSE ) {
                waitForJobToFinish(.Object,TRUE)
                retrieveJobResults(.Object)
                cleanupRemoteDirectory(.Object)
                print(.Object)
              } else
                print(.Object)
            }
          })



setGeneric(name="cancelJob",
           def=function(.Object)
           {
             standardGeneric("cancelJob")
           })


#'
#' @export
#'
setMethod("cancelJob",
          signature="SimpleNlmeJob",
          definition = function(.Object){
            runInBackground=.Object@runInBackground

            if ( attr(.Object@host,"hostType")== "Windows" )
              runInBackground=FALSE
          
            if ( .Object@host@isLocal == TRUE ||
                 runInBackground == FALSE ) {
                 print("Error : Cannot cancel local jobs")
                 return()
            } 

            stat = NlmeJobStatus(job)

            if ( stat == "InProgress" ) {
             
              cat("STOP",file="nlme.cmd",append=FALSE)
              files=c("nlme.cmd")
              stat=uploadFiles(.Object@host@remoteExecutor,.Object@remoteDir,
                               files)
            }
           print(.Object)
          })


setGeneric(name="waitForJobToFinish",
           def=function(.Object,verbose)
           {
             standardGeneric("waitForJobToFinish")
           })
#'
#' @export waitForJobToFinish
#' 
setMethod("waitForJobToFinish",
          signature="SimpleNlmeJob",
          definition = function(.Object,verbose=TRUE){
            
            stat=NlmeJobStatus(.Object)
            print("----------WaitForJobToFinish-----------")
            print(stat)
            while ( stat == "InProgress" || stat == "" ) {
              tryCatch({
			      Sys.sleep(2)
                  if ( verbose == TRUE )
                    print(.Object)
                  Sys.sleep(2)
                  print("Get NlmeJobStatus")             
                  stat=NlmeJobStatus(.Object)
                  print(stat)
              },
              error=function(ex){
                  stat = ""
              })
            }
            print("Finished with wait")
            return(stat)
          })

setGeneric(name="retrieveJobResults",
           def=function(.Object)
           {
             standardGeneric("retrieveJobResults")
           })

#' 
#' @export retrieveJobResults
#' 
setMethod("retrieveJobResults",
          signature="SimpleNlmeJob",
          definition = function(.Object){
            files=getResultsList(.Object@localDir,.Object@argsFile)
            for ( f in files ) {
              print(f)
              
              stat = downloadFiles(.Object@host@remoteExecutor,
                                   .Object@localDir,
                                   c(paste(.Object@remoteDir,f,sep="/")))
            }
            
          })
#' 
#' @export cleanupRemoteDirectory
#' 
setGeneric(name="cleanupRemoteDirectory",
           def=function(.Object)
           {
             standardGeneric("cleanupRemoteDirectory")
           })


setMethod("cleanupRemoteDirectory",
          signature="SimpleNlmeJob",
          definition = function(.Object){
            
            
          })


setGeneric(name="generateScript",
           def=function(.Object)
           {
             standardGeneric("generateScript")
           })


setMethod("generateScript",
          signature="SimpleNlmeJob",
          definition = function(.Object){
            
            {
              if ( .Object@host@hostType == "Windows" )  {
                scriptName=paste0(.Object@remoteDir,"/cmd.bat")

                appendFlag=FALSE
                hash= Sys.getenv("NLME_HASH")
                if (  hash != "" ) {
                    cat(paste0("set NLME_HASH=",hash),
                        file=scriptName,append=appendFlag,sep="\n")
                    appendFlag=TRUE
                }

                cat(gsub("/","\\",paste0("set INSTALLDIR=",.Object@host@installationDirectory),fixed=TRUE),
                    file=scriptName,append=appendFlag,sep="\n")
                appendFlag=TRUE
                cat(gsub("/","\\",paste0("set shared_directory=",.Object@host@sharedDirectory),fixed=TRUE),
                file=scriptName,append=appendFlag,sep="\n")
                cmd = paste0(.Object@host@installationDirectory,
                            "/generic_run.bat ",
                           .Object@host@parallelMethod@method, " ",
                           "%INSTALLDIR% ",
                           "%shared_directory% ",
                           .Object@remoteDir," ",
                           .Object@argsFile, " ",
                           .Object@host@numCores, " ",
                           .Object@workflow)
                    cmd = gsub("/","\\",cmd,fixed=TRUE)
                    cat(cmd, file=scriptName,append=appendFlag,sep="\n")

              } else {
                scriptName=paste0(.Object@localDir,"/cmd.sh")
                
                appendFlag=FALSE
                hash= Sys.getenv("NLME_HASH")
                if (  hash != "" ) {
                  cat(paste0("export NLME_HASH=",hash),
                    file=scriptName,append=appendFlag,sep="\n")
                appendFlag=TRUE
                }

                if ( .Object@host@scriptPath != "" ) {
                  cat(paste0("if [ -e ",.Object@host@scriptPath," ]"),
                      file=scriptName,append=appendFlag,sep="\n")
                  appendFlag=TRUE
                  cat(paste0("then"),
                      file=scriptName,append=appendFlag,sep="\n")
                  cat(paste0(". ",.Object@host@scriptPath),
                      file=scriptName,append=appendFlag,sep="\n")
                  cat(paste0("fi"),
                      file=scriptName,append=appendFlag,sep="\n")
                }
                if ( .Object@host@rLocation != "" ) {
                  cat(paste0("export PATH=",.Object@host@rLocation,":$PATH"),
                      file=scriptName,append=appendFlag,sep="\n")
                  appendFlag=TRUE
                } else {
                  cat(paste0("export PATH=/usr/bin:$PATH"),
                      file=scriptName,append=appendFlag,sep="\n")
                  appendFlag=TRUE
                }
                cat(paste0("export INSTALLDIR=",.Object@host@installationDirectory),
                    file=scriptName,append=appendFlag,sep="\n")
                appendFlag=TRUE
                cat(paste0("export shared_directory=",.Object@host@sharedDirectory),
                    file=scriptName,append=appendFlag,sep="\n")
                cat(paste0(.Object@host@installationDirectory,"/generic_run.sh ",
                           .Object@host@parallelMethod@method, " ",
                           "${INSTALLDIR} ",
                           "${shared_directory} ",
                           .Object@remoteDir," ",
                           basename(.Object@argsFile), " ",
                           .Object@host@numCores, " ",
                           .Object@workflow),
                    #                           " > NlmeRemote.LOG 2>&1"),
                    file=scriptName,append=appendFlag,sep="\n")
                
                
              }
            }
            return(scriptName)
          })
#'
#' print : reads progress file and prints out its contents
#'
#' @param  job handle to an NLME job
#'
#' @export
#' @examples
#'     print(jobHandle)
#'
print.SimpleNlmeJob <-function(obj)
{
  
  tryCatch ( {
  localDir=attr(obj,"localDir")

  xmlFile=file.path(localDir,"progress.xml")  
  if ( obj@host@isLocal == FALSE ) {
    #stat = downloadFiles(obj@host@remoteExecutor,
    #                     obj@localDir,
    #                     c(paste(obj@remoteDir,"progress.xml",sep="/")))
	stat = downloadFileWithCat(obj, "progress.xml")
	con = file(xmlFile, "w")
	writeLines(stat, con)
	close(con)
  }
  if ( file.exists(xmlFile) )
  {
    stuff=xmlToList(xmlParse(xmlFile))
    print("------------------------------------------------")
    print(paste("Job Type          :",attr(obj,"jobType")))
    print(paste("Local Directory   :",attr(obj,"localDir")))
    print(paste("Parallel Protocol :",stuff$ParallelProtocol))
    print(paste("Status            :",stuff$Status))
    print(paste("Num Replicates    :",stuff$NumOfSamples))
    print(paste("Num Completed     :",stuff$NumOfSamplesCompleted))
    print(paste("Num Failed        :",stuff$NumOfSamplesFailed))
    line1=stuff$DetailInfoLine1
    line2=stuff$DetailInfoLine2
    line3=stuff$DetailInfoLine3
    if ( ! is.null(line1) && line1 != "" )
        print(line1)
    if ( ! is.null(line2) && line2 != "" )
        print(line2)
    if ( ! is.null(line3) && line3 != "" )
        print(line3)
    print("------------------------------------------------")
  } 
 }, 
 error=function(ex){
    return("")    

  })
}

#' downloadFileWithCat : reads progress file and returns its content as stdout of a 'cat' command
#'
#' @param  obj		job handle to an NLME job
#'
#' @param  filename	file name related to remote directory
#'
downloadFileWithCat <-function(obj, filename)
{
#     print("download file with 'cat' command")
	 ret = execCommand(obj@host@remoteExecutor, c(paste("cat",paste(obj@remoteDir,filename,sep="/"))))
	 stat = rawToChar(ret$stdout)
	 return(stat)
}

#' NlmeJobStatus : reads progress file and returns the status of a job
#'
#' @param  job handle to an NLME job
#'
#' @export
#' @examples
#'     status=NlmeJobStatus(jobHandle)
#'
NlmeJobStatus <-function(obj)
{
  localDir=attr(obj,"localDir")
  
  tryCatch ( {
    print("NlmeJobStatus()")
    xmlFile=file.path(localDir,"progress.xml")
    if ( obj@host@isLocal == FALSE ) {
     if ( file.exists("progress.xml") )
		file.remove("progress.xml")
		
#     print("download file")
     #stat = downloadFiles(obj@host@remoteExecutor,
     #                     obj@localDir,
     #                     c(paste(obj@remoteDir,"progress.xml",sep="/")))
	 stat = downloadFileWithCat(obj, "progress.xml")
	 con = file(xmlFile, "w")
	 writeLines(stat, con)
	 close(con)
    }

    if ( file.exists(xmlFile) )
    {
#       print("read progress.xml")
       stuff=xmlToList(xmlParse(xmlFile))
       return(stuff$Status)
    }
    else
       return("")
 }, 
 error=function(ex){
    print("errorFunction")
    print(ex)
    print("------------")
    return("")    

  })
}


BootNlmeJob <- setClass("BootNlmeJob", representation(boot="NlmeBootstrapParams"), contains="SimpleNlmeJob" )


  setMethod("initialize", "BootNlmeJob", function(.Object, ..., boot="") {
    cat("Boot Job\n")
    callNextMethod(.Object, ..., boot=boot)
  })




setMethod("generateScript",
          signature="BootNlmeJob",
          definition = function(.Object){
            
            
              if ( .Object@host@hostType == "Windows" )  {
                scriptName=paste0(.Object@remoteDir,"/cmd.bat")
                appendFlag=FALSE
                hash= Sys.getenv("NLME_HASH")
                if (  hash != "" ) {
                    cat(paste0("set NLME_HASH=",hash),
                        file=scriptName,append=appendFlag,sep="\n")
                    appendFlag=TRUE
                }


                cat(gsub("/","\\",paste0("set INSTALLDIR=",.Object@host@installationDirectory),fixed=TRUE),
                    file=scriptName,append=appendFlag,sep="\n")
                appendFlag=TRUE
                cat(gsub("/","\\",paste0("set shared_directory=",.Object@host@sharedDirectory),fixed=TRUE),

                    file=scriptName,append=appendFlag,sep="\n")

#                cat(gsub("/","\\",paste0(.Object@host@installationDirectory,"/generic_run.bat ",fixed=TRUE),
#                           .Object@host@parallelMethod@method, " ",
#                           "%INSTALLDIR% ",
#                           "%shared_directory% ",
#                           .Object@remoteDir," ",
#                           .Object@argsFile, " ",
#                           .Object@host@numCores, " ",
#                           .Object@workflow),
#                    file=scriptName,append=appendFlag,sep="\n")

                controlFile=.Object@argsFile
                stuff=parseControlFile(.Object@localDir,controlFile)
                modelFile=stuff[1]
                colDefFile=stuff[2]
                dataFile=stuff[3]
                extraArgsFile=stuff[4]
                outputFile=stuff[5]
                filesToCopy=stuff[6]
                engine=stuff[7]
                numIterations=stuff[8]
                numSamples=.Object@argsList[[7]]
                initialEstimates=.Object@argsList[[8]]
                maxRetries=.Object@argsList[[9]]
                startSeed=.Object@argsList[[10]]
                numProc=.Object@argsList[[4]]
                confidenceLevel=.Object@argsList[[11]]
                workflow_name = .Object@argsList[[12]]
                cmd = paste0(.Object@host@installationDirectory,"/phx_bootstrap.bat ",
                           .Object@host@parallelMethod@method, " ",
                           "%INSTALLDIR% ",
                           "%shared_directory% ",
                           .Object@remoteDir," ",
                           engine, " ",
                           numIterations, " " ,
                           numSamples," ",
                           maxRetries, " " ,
                           modelFile, " " ,
                           colDefFile," " ,
                           dataFile, " " ,
                           startSeed, " " ,
                           extraArgsFile," " ,
                           "\"",filesToCopy, "\" ",
                           .Object@host@numCores, " ",
                           confidenceLevel, "  ",
                           .Object@workflow)
                    cmd=gsub("/","\\",cmd,fixed=TRUE)
                    cat(cmd, file=scriptName,append=appendFlag,sep="\n")
              } else {
                scriptName=paste0(.Object@localDir,"/cmd.sh")
                
                appendFlag=FALSE
                hash= Sys.getenv("NLME_HASH")
                if (  hash != "" ) {
                  cat(paste0("export NLME_HASH=",hash),
                    file=scriptName,append=appendFlag,sep="\n")
                appendFlag=TRUE
                }
                if ( .Object@host@scriptPath != "" ) {
                  cat(paste0("if [ -e ",.Object@host@scriptPath," ]"),
                      file=scriptName,append=appendFlag,sep="\n")
                  appendFlag=TRUE
                  cat(paste0("then"),
                      file=scriptName,append=appendFlag,sep="\n")
                  cat(paste0(". ",.Object@host@scriptPath),
                      file=scriptName,append=appendFlag,sep="\n")
                  cat(paste0("fi"),
                      file=scriptName,append=appendFlag,sep="\n")
                }
                if ( .Object@host@rLocation != "" ) {
                  cat(paste0("export PATH=",.Object@host@rLocation,":$PATH"),
                      file=scriptName,append=appendFlag,sep="\n")
                  appendFlag=TRUE
                } else {
                  cat(paste0("export PATH=/usr/bin:$PATH"),
                      file=scriptName,append=appendFlag,sep="\n")
                  appendFlag=TRUE
                }
                cat(paste0("export INSTALLDIR=",.Object@host@installationDirectory),
                    file=scriptName,append=appendFlag,sep="\n")
                appendFlag=TRUE
                cat(paste0("export shared_directory=",.Object@host@sharedDirectory),
                    file=scriptName,append=appendFlag,sep="\n")
                
                controlFile=.Object@argsFile
                stuff=parseControlFile(.Object@localDir,controlFile)
                modelFile=stuff[1]
                colDefFile=stuff[2]
                dataFile=stuff[3]
                extraArgsFile=stuff[4]
                outputFile=stuff[5]
                filesToCopy=stuff[6]
                engine=stuff[7]
                numIterations=stuff[8]
                numSamples=.Object@argsList[[7]]
                initialEstimates=.Object@argsList[[8]]
                maxRetries=.Object@argsList[[9]]
                startSeed=.Object@argsList[[10]]
                numProc=.Object@argsList[[4]]
                confidenceLevel=.Object@argsList[[11]]
                workflow_name = .Object@argsList[[12]]
                cat(paste0(.Object@host@installationDirectory,"/phx_bootstrap.sh ",
                           .Object@host@parallelMethod@method, " ",
                           "${INSTALLDIR} ",
                           "${shared_directory} ",
                           .Object@remoteDir," ",
                           engine, " ",
                           numIterations, " " ,
                           numSamples," ",
                           maxRetries, " " ,
                           modelFile, " " ,
                           colDefFile," " ,
                           dataFile, " " ,
                           startSeed, " " ,
                           extraArgsFile," " ,
                           "\"",filesToCopy, "\" ",
                           .Object@host@numCores, " ",
                           confidenceLevel, "  ",
                           .Object@workflow),
                    #                           " > NlmeRemote.LOG 2>&1"),
                    file=scriptName,append=appendFlag,sep="\n")
                
                
              }
            return(scriptName)
              
          })

setMethod("executeJob",
          signature="BootNlmeJob",
          definition = function(.Object){
            runInBackground=.Object@runInBackground
            if ( attr(.Object@host,"hostType")== "Windows" )
              runInBackground=FALSE
          
            removeCommandFile() 
            if ( .Object@host@isLocal == TRUE ) {
              library(parallel)
              
              if ( runInBackground == TRUE )
                mcparallel(performBootstrap2(as.character(.Object@argsList)))
              else {
                performBootstrap2(as.character(.Object@argsList),reportProgress=TRUE)
                print(.Object)
               }
            } else {
              
              files=getFilesToTransfer(.Object@localDir,.Object@argsFile)
              all=c(files,.Object@scriptFile,.Object@argsFile)
              stat=uploadFiles(.Object@host@remoteExecutor,.Object@remoteDir,all)
              fullPath=paste(.Object@remoteDir,basename(.Object@scriptFile),sep="/")
              
              
              cmds=c(paste0("chmod 777 ",fullPath),
                     paste0("dos2unix ",fullPath),
                     paste0("cd ",.Object@remoteDir," ; env > env.log ; nohup ",fullPath, " > NlmeRemote.LOG 2>&1 &"))
              
              for ( cmd in cmds ) {
                print(cmd)
                stat=execCommand(.Object@host@remoteExecutor,cmd)
              }
              
              if ( .Object@runInBackground == FALSE ) {
                waitForJobToFinish(.Object,TRUE)
                retrieveJobResults(.Object)
                cleanupRemoteDirectory(.Object)
              }
            }
          })





#'
#' @export StepwiseNlmeJob
#'
StepwiseNlmeJob <- setClass("StepwiseNlmeJob", representation(stepParam="NlmeStepwiseParams"), contains="SimpleNlmeJob" )


  setMethod("initialize", "StepwiseNlmeJob", function(.Object, ..., stepParam="") {
    cat("Stepwise Job\n")
    callNextMethod(.Object, ..., stepParam=stepParam)
  })




setMethod("generateScript",
          signature="StepwiseNlmeJob",
          definition = function(.Object){
            
            
              if ( .Object@host@hostType == "Windows" )  {
                scriptName=paste0(.Object@remoteDir,"/cmd.bat")
                appendFlag=FALSE
                hash= Sys.getenv("NLME_HASH")
                if (  hash != "" ) {
                    cat(paste0("set NLME_HASH=",hash),
                        file=scriptName,append=appendFlag,sep="\n")
                    appendFlag=TRUE
                }


                cat(gsub("/","\\",paste0("set INSTALLDIR=",.Object@host@installationDirectory),fixed=TRUE),
                    file=scriptName,append=appendFlag,sep="\n")
                appendFlag=TRUE
                cat(gsub("/","\\",paste0("set shared_directory=",.Object@host@sharedDirectory),fixed=TRUE),

                    file=scriptName,append=appendFlag,sep="\n")

                controlFile=.Object@argsFile
                stuff=parseControlFile(.Object@localDir,controlFile)
                modelFile=stuff[1]
                colDefFile=stuff[2]
                dataFile=stuff[3]
                extraArgsFile=stuff[4]
                outputFile=stuff[5]
                filesToCopy=stuff[6]
                engine=stuff[7]
                numIterations=stuff[8]

                numCovariates = .Object@argsList[[8]]
                covariateList = .Object@argsList[[9]]
                stepwiseMethod= .Object@argsList[[10]]

                cmd = paste0(.Object@host@installationDirectory,"/phx_stepwise_covarsrch.bat ",
                           .Object@host@parallelMethod@method, " ",
                           "%INSTALLDIR% ",
                           "%shared_directory% ",
                           .Object@remoteDir," ",
                           modelFile, " " ,
                           extraArgsFile," " ,
                           "\"",filesToCopy, "\" ",
                           numCovariates, " ",
                           "\"", covariateList, "\" ",
                           "\"", stepwiseMethod, "\" ",
                           .Object@stepParam@addPValue, " ",
                           .Object@stepParam@removePValue, " " ,
                           .Object@host@numCores, " ",
                           .Object@workflow)

                    cat(cmd, file=scriptName,append=appendFlag,sep="\n")
              } else {
                scriptName=paste0(.Object@localDir,"/cmd.sh")
                
                appendFlag=FALSE
                hash= Sys.getenv("NLME_HASH")
                if (  hash != "" ) {
                  cat(paste0("export NLME_HASH=",hash),
                    file=scriptName,append=appendFlag,sep="\n")
                appendFlag=TRUE
                }
                if ( .Object@host@scriptPath != "" ) {
                  cat(paste0("if [ -e ",.Object@host@scriptPath," ]"),
                      file=scriptName,append=appendFlag,sep="\n")
                  appendFlag=TRUE
                  cat(paste0("then"),
                      file=scriptName,append=appendFlag,sep="\n")
                  cat(paste0(". ",.Object@host@scriptPath),
                      file=scriptName,append=appendFlag,sep="\n")
                  cat(paste0("fi"),
                      file=scriptName,append=appendFlag,sep="\n")
                }
                if ( .Object@host@rLocation != "" ) {
                  cat(paste0("export PATH=",.Object@host@rLocation,":$PATH"),
                      file=scriptName,append=appendFlag,sep="\n")
                  appendFlag=TRUE
                } else {
                  cat(paste0("export PATH=/usr/bin:$PATH"),
                      file=scriptName,append=appendFlag,sep="\n")
                  appendFlag=TRUE
                }
                cat(paste0("export INSTALLDIR=",.Object@host@installationDirectory),
                    file=scriptName,append=appendFlag,sep="\n")
                appendFlag=TRUE
                cat(paste0("export shared_directory=",.Object@host@sharedDirectory),
                    file=scriptName,append=appendFlag,sep="\n")

                controlFile=.Object@argsFile
                stuff=parseControlFile(.Object@localDir,controlFile)
                modelFile=stuff[1]
                colDefFile=stuff[2]
                dataFile=stuff[3]
                extraArgsFile=stuff[4]
                outputFile=stuff[5]
                filesToCopy=stuff[6]
                engine=stuff[7]
                numIterations=stuff[8]

                numCovariates = .Object@argsList[[8]]
                covariateList = .Object@argsList[[9]]
                stepwiseMethod= .Object@argsList[[10]]

                cat(paste0(.Object@host@installationDirectory,"/phx_stepwise_covarsrch.sh ",
                           .Object@host@parallelMethod@method, " ",
                           "${INSTALLDIR} ",
                           "${shared_directory} ",
                           .Object@remoteDir," ",
                           modelFile, " " ,
                           extraArgsFile," " ,
                           "\"",filesToCopy, "\" ",
                           numCovariates, " ",
                           "\"", covariateList, "\" ",
                           stepwiseMethod, " ",
                           .Object@stepParam@addPValue, " ",
                           .Object@stepParam@removePValue, " " ,
                           .Object@host@numCores, " ",
                           .Object@workflow),
                    #                           " > NlmeRemote.LOG 2>&1"),
                    file=scriptName,append=appendFlag,sep="\n")
                
                
              }
            return(scriptName)
              
          })

setMethod("executeJob",
          signature="StepwiseNlmeJob",
          definition = function(.Object){
            runInBackground=.Object@runInBackground
            if ( attr(.Object@host,"hostType")== "Windows" )
              runInBackground=FALSE
          
            removeCommandFile() 
            if ( .Object@host@isLocal == TRUE ) {
              library(parallel)
              
              if ( runInBackground == TRUE )
                mcparallel(performStepwiseCovarSearch(
                                              as.character(.Object@argsList)))
              else {
                performStepwiseCovarSearch(
                                              as.character(.Object@argsList),
                                              reportProgress=TRUE)
                print(.Object)
              }

            } else {

              files=getFilesToTransfer(.Object@localDir,.Object@argsFile)
              all=c(files,.Object@scriptFile,.Object@argsFile)
              stat=uploadFiles(.Object@host@remoteExecutor,.Object@remoteDir,all)
              fullPath=paste(.Object@remoteDir,basename(.Object@scriptFile),sep="/")


              cmds=c(paste0("chmod 777 ",fullPath),
                     paste0("dos2unix ",fullPath),
                     paste0("cd ",.Object@remoteDir," ; env > env.log ; nohup ",fullPath, " > NlmeRemote.LOG 2>&1 &"))

              for ( cmd in cmds ) {
                print(cmd)
                stat=execCommand(.Object@host@remoteExecutor,cmd)
              }

              if ( .Object@runInBackground == FALSE ) {
                waitForJobToFinish(.Object,TRUE)
                retrieveJobResults(.Object)
                cleanupRemoteDirectory(.Object)
              }
            }
          })





#'
#' @export ShotgunNlmeJob
#'
#'
#' @export ShotgunNlmeJob
#'
ShotgunNlmeJob <- setClass("ShotgunNlmeJob", representation(), contains="SimpleNlmeJob" )


setMethod("initialize", "ShotgunNlmeJob", function(.Object, ...) {
  cat("Shotgun Job\n")
  callNextMethod(.Object, ...)
})




setMethod("generateScript",
          signature="ShotgunNlmeJob",
          definition = function(.Object){

            print(" generateScript for ShotgunNlmeJob()")

            if ( .Object@host@hostType == "Windows" )  {
                scriptName=paste0(.Object@remoteDir,"/cmd.bat")
                appendFlag=FALSE
                hash= Sys.getenv("NLME_HASH")
                if (  hash != "" ) {
                  cat(paste0("set NLME_HASH=",hash),
                      file=scriptName,append=appendFlag,sep="\n")
                  appendFlag=TRUE
                }


                cat(gsub("/","\\",paste0("set INSTALLDIR=",.Object@host@installationDirectory),fixed=TRUE),
                    file=scriptName,append=appendFlag,sep="\n")
                appendFlag=TRUE
                cat(gsub("/","\\",paste0("set shared_directory=",.Object@host@sharedDirectory),fixed=TRUE),
                    file=scriptName,append=appendFlag,sep="\n")

              controlFile=.Object@argsFile
              stuff=parseControlFile(.Object@localDir,controlFile)
              modelFile=stuff[1]
              colDefFile=stuff[2]
              dataFile=stuff[3]
              extraArgsFile=stuff[4]
              outputFile=stuff[5]
              filesToCopy=stuff[6]
              engine=stuff[7]
              numIterations=stuff[8]

              cat(paste0(.Object@host@installationDirectory,"/phx_covar_search.bat ",
                         .Object@host@parallelMethod@method, " ",
                         "%INSTALLDIR% ",
                         "%shared_directory% ",
                         .Object@remoteDir," ",
                         paste0(.Object@localDir,"/",controlFile)," " ,
                         .Object@host@numCores, " ",
                         .Object@workflow),
                  file=scriptName,append=appendFlag,sep="\n")


            } else {
                scriptName=paste0(.Object@localDir,"/cmd.sh")

                appendFlag=FALSE
                hash= Sys.getenv("NLME_HASH")
                if (  hash != "" ) {
                  cat(paste0("export NLME_HASH=",hash),
                      file=scriptName,append=appendFlag,sep="\n")
                  appendFlag=TRUE
                }

              appendFlag=TRUE
              if ( .Object@host@scriptPath != "" ) {
                cat(paste0("if [ -e ",.Object@host@scriptPath," ]"),
                    file=scriptName,append=appendFlag,sep="\n")
                appendFlag=TRUE
                cat(paste0("then"),
                    file=scriptName,append=appendFlag,sep="\n")
                cat(paste0(". ",.Object@host@scriptPath),
                    file=scriptName,append=appendFlag,sep="\n")
                cat(paste0("fi"),
                    file=scriptName,append=appendFlag,sep="\n")
              }
              if ( .Object@host@rLocation != "" ) {
                cat(paste0("export PATH=",.Object@host@rLocation,":$PATH"),
                    file=scriptName,append=appendFlag,sep="\n")
                appendFlag=TRUE
              } else {
                cat(paste0("export PATH=/usr/bin:$PATH"),
                    file=scriptName,append=appendFlag,sep="\n")
                appendFlag=TRUE
              }
              cat(paste0("export INSTALLDIR=",.Object@host@installationDirectory),
                  file=scriptName,append=appendFlag,sep="\n")
              appendFlag=TRUE
              cat(paste0("export shared_directory=",.Object@host@sharedDirectory),
                  file=scriptName,append=appendFlag,sep="\n")

              controlFile=.Object@argsFile
              stuff=parseControlFile(.Object@localDir,controlFile)
              modelFile=stuff[1]
              colDefFile=stuff[2]
              dataFile=stuff[3]
              extraArgsFile=stuff[4]
              outputFile=stuff[5]
              filesToCopy=stuff[6]
              engine=stuff[7]
              numIterations=stuff[8]


              cat(paste0(.Object@host@installationDirectory,"/phx_covar_search.sh ",
                         .Object@host@parallelMethod@method, " ",
                         "${INSTALLDIR} ",
                         "${shared_directory} ",
                         .Object@remoteDir," ",
                         basename(controlFile)," " ,
                         .Object@host@numCores, " ",
                         .Object@workflow),
                  #                           " > NlmeRemote.LOG 2>&1"),
                  file=scriptName,append=appendFlag,sep="\n")


            }
            return(scriptName)

          })

setMethod("executeJob",
          signature="ShotgunNlmeJob",
          definition = function(.Object){
            runInBackground=.Object@runInBackground
            if ( attr(.Object@host,"hostType")== "Windows" )
              runInBackground=FALSE

            removeCommandFile() 
            if ( .Object@host@isLocal == TRUE ) {
              library(parallel)
              if ( runInBackground == TRUE )
                mcparallel(performParallelNLMERun(
                  as.character(.Object@argsList)))
              else {
                performParallelNLMERun(
                  as.character(.Object@argsList),
                  reportProgress=TRUE)
                print(.Object)
              }

            } else {

              files=getFilesToTransfer(.Object@localDir,.Object@argsFile)
              all=c(files,.Object@scriptFile,paste0(.Object@localDir,"/",.Object@argsFile))
              stat=uploadFiles(.Object@host@remoteExecutor,.Object@remoteDir,all)
              fullPath=paste(.Object@remoteDir,basename(.Object@scriptFile),sep="/")


              cmds=c(paste0("chmod 777 ",fullPath),
                     paste0("dos2unix ",fullPath),
                     paste0("cd ",.Object@remoteDir," ; env > env.log ; nohup ",fullPath, " > NlmeRemote.LOG 2>&1 &"))

              for ( cmd in cmds ) {
                print(cmd)
                stat=execCommand(.Object@host@remoteExecutor,cmd)
              }

              if ( .Object@runInBackground == FALSE ) {
                waitForJobToFinish(.Object,TRUE)
                retrieveJobResults(.Object)
                cleanupRemoteDirectory(.Object)
              }
            }
          })




#'
#' @export SortByNlmeJob
#'
SortByNlmeJob <- setClass("SortByNlmeJob", representation(sortColumns="NlmeSortColumns",scenarios="list"), contains="SimpleNlmeJob" )


setMethod("initialize", "SortByNlmeJob", function(.Object, ...,
                                         sortColumns,
                                         scenarios=list()) {
  cat("SortByNlmeJob Job\n")
  .Object@sortColumns=sortColumns
  .Object@scenarios=scenarios
  callNextMethod(.Object, ...,sortColumns=sortColumns,scenarios=scenarios)
})


setMethod("executeJob",
          signature="SortByNlmeJob",
          definition = function(.Object){
            runInBackground=.Object@runInBackground
            if ( attr(.Object@host,"hostType")== "Windows" )
              runInBackground=FALSE

            removeCommandFile() 
            if ( .Object@host@isLocal == TRUE ) {
              library(parallel)

              if ( runInBackground == TRUE )
                mcparallel(performEstimationOnSortColumns(as.character(.Object@argsList)))
              else {
                performEstimationOnSortColumns(as.character(.Object@argsList),reportProgress=TRUE)
                print(.Object)
              }
              
            } else {
              
              files=getFilesToTransfer(.Object@localDir,.Object@argsFile)
              all=c(files,.Object@scriptFile,.Object@argsFile)
              stat=uploadFiles(.Object@host@remoteExecutor,.Object@remoteDir,all)
              fullPath=paste(.Object@remoteDir,basename(.Object@scriptFile),sep="/")
              
              cmds=c(paste0("chmod 777 ",fullPath),
                     paste0("dos2unix ",fullPath),
                     paste0("cd ",.Object@remoteDir," ; env > env.log ; nohup ",fullPath, " > NlmeRemote.LOG 2>&1 &"))

              for ( cmd in cmds ) {
                print(cmd)
                stat=execCommand(.Object@host@remoteExecutor,cmd)
              }

              if ( .Object@runInBackground == FALSE ) {
                waitForJobToFinish(.Object,TRUE)
                retrieveJobResults(.Object)
                cleanupRemoteDirectory(.Object)
                print(.Object)
              }
            }
          })


setMethod("generateScript",
          signature="SortByNlmeJob",
          definition = function(.Object){

            {
              if ( .Object@host@hostType == "Windows" )  {
                scriptName=paste0(.Object@remoteDir,"/cmd.bat")

                appendFlag=FALSE
                hash= Sys.getenv("NLME_HASH")
                if (  hash != "" ) {
                  cat(paste0("set NLME_HASH=",hash),
                      file=scriptName,append=appendFlag,sep="\n")
                  appendFlag=TRUE
                }


                cat(gsub("/","\\",paste0("set INSTALLDIR=",.Object@host@installationDirectory),fixed=TRUE),
                    file=scriptName,append=appendFlag,sep="\n")
                appendFlag=TRUE
                cat(gsub("/","\\",paste0("set shared_directory=",.Object@host@sharedDirectory),fixed=TRUE),
                    file=scriptName,append=appendFlag,sep="\n")
                cmd = paste0(.Object@host@installationDirectory,
                             "/phx_sortcol_estimation.bat ",
                             .Object@host@parallelMethod@method, " ",
                             "%INSTALLDIR% ",
                             "%shared_directory% ",
                             .Object@remoteDir," ",
                             .Object@argsFile, " ",
                             .Object@sortColumns@numSortColumns, " ",
                             "\"",gsub(","," ",attr(sortColumns,"sortColumnList")),"\" ",
                             .Object@host@numCores, " ",
                             .Object@workflow)
                cmd = gsub("/","\\",cmd,fixed=TRUE)
                cat(cmd, file=scriptName,append=appendFlag,sep="\n")

              } else {
                scriptName=paste0(.Object@localDir,"/cmd.sh")

                appendFlag=FALSE
                hash= Sys.getenv("NLME_HASH")
                if (  hash != "" ) {
                  cat(paste0("export NLME_HASH=",hash),
                      file=scriptName,append=appendFlag,sep="\n")
                  appendFlag=TRUE
                }

                if ( .Object@host@scriptPath != "" ) {
                  cat(paste0("if [ -e ",.Object@host@scriptPath," ]"),
                      file=scriptName,append=appendFlag,sep="\n")
                  appendFlag=TRUE
                  cat(paste0("then"),
                      file=scriptName,append=appendFlag,sep="\n")
                  cat(paste0(". ",.Object@host@scriptPath),
                      file=scriptName,append=appendFlag,sep="\n")
                  cat(paste0("fi"),
                      file=scriptName,append=appendFlag,sep="\n")
                }
                if ( .Object@host@rLocation != "" ) {
                  cat(paste0("export PATH=",.Object@host@rLocation,":$PATH"),
                      file=scriptName,append=appendFlag,sep="\n")
                  appendFlag=TRUE
                } else {
                  cat(paste0("export PATH=/usr/bin:$PATH"),
                      file=scriptName,append=appendFlag,sep="\n")
                  appendFlag=TRUE
                }
                cat(paste0("export INSTALLDIR=",.Object@host@installationDirectory),
                    file=scriptName,append=appendFlag,sep="\n")
                appendFlag=TRUE
                cat(paste0("export shared_directory=",.Object@host@sharedDirectory),
                    file=scriptName,append=appendFlag,sep="\n")
                cat(paste0(.Object@host@installationDirectory,"/phx_sortcol_estimation.sh ",
                           .Object@host@parallelMethod@method, " ",
                           "${INSTALLDIR} ",
                           "${shared_directory} ",
                           .Object@remoteDir," ",
                           basename(.Object@argsFile), " ",
                             .Object@sortColumns@numSortColumns, " ",
                             "\"",gsub(","," ",attr(sortColumns,"sortColumnList")),"\" ",
                           .Object@host@numCores, " ",
                           .Object@workflow),
                    #                           " > NlmeRemote.LOG 2>&1"),
                    file=scriptName,append=appendFlag,sep="\n")


              }
            }
            return(scriptName)
          })



#'
#' @export ProfileNlmeJob
#'
ProfileNlmeJob <- setClass("ProfileNlmeJob", representation(profiles="NlmeProfileParameters",sortColumns="NlmeSortColumns",scenarios="list"), contains="SimpleNlmeJob" )


setMethod("initialize", "ProfileNlmeJob", function(.Object, ...,
                                         profiles,
                                         sortColumns,
                                         scenarios=list()) {
  cat("ProfileNlmeJob Job\n")
  .Object@profiles=profiles
  .Object@sortColumns=sortColumns
  .Object@scenarios=scenarios
  callNextMethod(.Object, ...)
})


setMethod("executeJob",
          signature="ProfileNlmeJob",
          definition = function(.Object){
            runInBackground=.Object@runInBackground
            if ( attr(.Object@host,"hostType")== "Windows" )
              runInBackground=FALSE

            removeCommandFile() 
            if ( .Object@host@isLocal == TRUE ) {
              library(parallel)

              if ( runInBackground == TRUE )
                mcparallel(performProfileEstimation(as.character(.Object@argsList)))
              else {
                performProfileEstimation(as.character(.Object@argsList),reportProgress=TRUE)
                print(.Object)
              }

            } else {

              files=getFilesToTransfer(.Object@localDir,.Object@argsFile)
              all=c(files,.Object@scriptFile,.Object@argsFile)
              stat=uploadFiles(.Object@host@remoteExecutor,.Object@remoteDir,all)
              fullPath=paste(.Object@remoteDir,basename(.Object@scriptFile),sep="/")
              
              cmds=c(paste0("chmod 777 ",fullPath),
                     paste0("dos2unix ",fullPath),
                     paste0("cd ",.Object@remoteDir," ; env > env.log ; nohup ",fullPath, " > NlmeRemote.LOG 2>&1 &"))
              
              for ( cmd in cmds ) {
                print(cmd)
                stat=execCommand(.Object@host@remoteExecutor,cmd)
              }
              
              if ( .Object@runInBackground == FALSE ) {
                waitForJobToFinish(.Object,TRUE)
                retrieveJobResults(.Object)
                cleanupRemoteDirectory(.Object)
                print(.Object)
              }
            }
          })


setMethod("generateScript",
          signature="ProfileNlmeJob",
          definition = function(.Object){

            {
              if ( .Object@host@hostType == "Windows" )  {
                scriptName=paste0(.Object@remoteDir,"/cmd.bat")

                appendFlag=FALSE
                hash= Sys.getenv("NLME_HASH")
                if (  hash != "" ) {
                  cat(paste0("set NLME_HASH=",hash),
                      file=scriptName,append=appendFlag,sep="\n")
                  appendFlag=TRUE
                }


                cat(gsub("/","\\",paste0("set INSTALLDIR=",.Object@host@installationDirectory),fixed=TRUE),
                    file=scriptName,append=appendFlag,sep="\n")
                appendFlag=TRUE
                cat(gsub("/","\\",paste0("set shared_directory=",.Object@host@sharedDirectory),fixed=TRUE),
                    file=scriptName,append=appendFlag,sep="\n")
                cmd = paste0(.Object@host@installationDirectory,
                             "/phx_profile_estimation.bat ",
                             .Object@host@parallelMethod@method, " ",
                             "%INSTALLDIR% ",
                             "%shared_directory% ",
                             .Object@remoteDir," ",
                             .Object@argsFile, " ",
                             .Object@sortColumns@numSortColumns, " ",
                             "\"",gsub(","," ",attr(.Object@sortColumns,"sortColumnList")),"\" ",
                             "\"",getProfilesString(.Object@profiles),"\" ",
                             attr(.Object@profiles,"howToPertubate"), " ",
                             .Object@host@numCores, " ",
                             .Object@workflow)
                cmd = gsub("/","\\",cmd,fixed=TRUE)
                cat(cmd, file=scriptName,append=appendFlag,sep="\n")

              } else {
                scriptName=paste0(.Object@localDir,"/cmd.sh")

                appendFlag=FALSE
                hash= Sys.getenv("NLME_HASH")
                if (  hash != "" ) {
                  cat(paste0("export NLME_HASH=",hash),
                      file=scriptName,append=appendFlag,sep="\n")
                  appendFlag=TRUE
                }

                if ( .Object@host@scriptPath != "" ) {
                  cat(paste0("if [ -e ",.Object@host@scriptPath," ]"),
                      file=scriptName,append=appendFlag,sep="\n")
                  appendFlag=TRUE
                  cat(paste0("then"),
                      file=scriptName,append=appendFlag,sep="\n")
                  cat(paste0(". ",.Object@host@scriptPath),
                      file=scriptName,append=appendFlag,sep="\n")
                  cat(paste0("fi"),
                      file=scriptName,append=appendFlag,sep="\n")
                }
                if ( .Object@host@rLocation != "" ) {
                  cat(paste0("export PATH=",.Object@host@rLocation,":$PATH"),
                      file=scriptName,append=appendFlag,sep="\n")
                  appendFlag=TRUE
                } else {
                  cat(paste0("export PATH=/usr/bin:$PATH"),
                      file=scriptName,append=appendFlag,sep="\n")
                  appendFlag=TRUE
                }
                cat(paste0("export INSTALLDIR=",.Object@host@installationDirectory),
                    file=scriptName,append=appendFlag,sep="\n")
                appendFlag=TRUE
                cat(paste0("export shared_directory=",.Object@host@sharedDirectory),
                    file=scriptName,append=appendFlag,sep="\n")
                cat(paste0(.Object@host@installationDirectory,"/phx_profile_estimation.sh ",
                           .Object@host@parallelMethod@method, " ",
                           "${INSTALLDIR} ",
                           "${shared_directory} ",
                           .Object@remoteDir," ",
                           basename(.Object@argsFile), " ",
                             .Object@sortColumns@numSortColumns, " ",
                             "\"",gsub(","," ",attr(.Object@sortColumns,"sortColumnList")),"\" ",
                             "\"",getProfilesString(.Object@profiles),"\" ",
                             attr(.Object@profiles,"howToPertubate"), " ",
                           .Object@host@numCores, " ",
                           .Object@workflow),
                    #                           " > NlmeRemote.LOG 2>&1"),
                    file=scriptName,append=appendFlag,sep="\n")


              }
            }
            return(scriptName)
          })


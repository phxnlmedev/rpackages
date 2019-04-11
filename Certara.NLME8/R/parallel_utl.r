

nlmeEnv  = new.env()

loadBatchLibrary = function(){
#
# Load batch jobs package
#
if ( "batchtools" %in% installed.packages()[,"Package"] ) {
library("batchtools")
    UsingBatchTools=TRUE
} else {
    UsingBatchTools=FALSE
if ( "BatchJobs" %in% installed.packages()[,"Package"] ) {
library("BatchJobs")
}
}
    assign("UsingBatchTools",UsingBatchTools,envir=.GlobalEnv)
    assign("UsingBatchTools",UsingBatchTools,envir=nlmeEnv)
}

memory_usage <-function(envrionment){

    if ( FALSE ) {
        #Find the objects       
        MemoryObjects = ls(envir=envrionment)    
        #Create an array
        MemoryAssessmentTable=array(NA,dim=c(length(MemoryObjects),2))
        #Name the columns
        colnames(MemoryAssessmentTable)=c("object","bytes")
        #Define the first column as the objects
        MemoryAssessmentTable[,1]=MemoryObjects
        #Define a function to determine size        
        MemoryAssessmentFunction=function(x){object.size(get(x,envir=envrionment))}
        #Apply the function to the objects
        MemoryAssessmentTable[,2]=t(t(sapply(MemoryAssessmentTable[,1],MemoryAssessmentFunction)))
        #Produce a table with the largest objects first
        print(noquote(MemoryAssessmentTable[rev(order(as.numeric(MemoryAssessmentTable[,2]))),]))
        print(memory.profile())
    }
}


loadBatchLibrary()

DoInitialNlmeRun=TRUE
DebugProgress=FALSE
MpiExecutable="NLME7.exe"
MpiArgument="MPINO"
MpiNumCores=1
MpiLocal="NO"
GlobalSummaryLine1=""
GlobalSummaryLine2=""
GlobalSummaryLine3=""
IsMpiInitialized=FALSE
assign("IsMpiInitialized", IsMpiInitialized , envir=nlmeEnv)

getBatchDirectoryLocation = function(SharedWorkingDir ) {

    UsingBatchTools=get("UsingBatchTools",envir=nlmeEnv)
    parallelMethod=get("parallelMethod",envir=nlmeEnv)
    dir=SharedWorkingDir
    if ( UsingBatchTools == TRUE && !(tolower(parallelMethod) == "multicore" || tolower(parallelMethod) == "none" || tolower(parallelMethod) == "local_mpi") )
         dir=sprintf("%s/registry",SharedWorkingDir)
    return(dir)
}

shortModelName <-function(){
    if (exists("workflow_name", envir = nlmeEnv)) 
        workflow_name = get("workflow_name",envir=nlmeEnv)
    else
        workflow_name=""
    workflow_name=gsub("[^A-Za-z0-9_]","",workflow_name)
    
    return(strtrim(workflow_name,30))
}

reportCurrentStatus <-function(num_samples, numSuccessful, numFailed){
    print("---------------------------------------------")
    print(paste("Num Replicates : ",num_samples))
    print(paste("Num Finished   : ",numSuccessful))
    print(paste("Num Failed     : ",numFailed))
}

listToXML <-function(node,sublist){
    for(i in 1:length(sublist) ){
        child <- newXMLNode(names(sublist)[i], parent=node)
        if ( typeof(sublist[[i]]) == "list"){
            listToXML(child,sublist[[i]])
        } else {
            xmlValue(child) = sublist[[i]]
        }
    }
}

FirstTimeInitializeTracking=TRUE
TrackingHistoryFilename=""


TrackingJobHistoryInitialization <- function(){
DEBUG_MASTER("TrackingJobHistoryInitialization()")
    if ( FirstTimeInitializeTracking ) {
        userValue=Sys.getenv("NLME_JOB_TRACK_HISTORY")
DEBUG_MASTER(paste("userValue",userValue))
        if ( userValue != "" ) {
            localWorkingDir = get("localWorkingDir",envir=nlmeEnv)
            dir=dirname(localWorkingDir)
            fname=basename(localWorkingDir)
            progressFilename=sprintf("%s_history.xml",fname)
            TrackingHistoryFilename <<- file.path(dir,progressFilename)
        }
    }
    FirstTimeInitializeTracking=FALSE
DEBUG_MASTER(paste("TrackingHistoryFilename",TrackingHistoryFilename))
}

ReportProgress <-function(){

    ProgressStatus = get("ProgressStatus",envir=nlmeEnv)
    localWorkingDir = get("localWorkingDir",envir=nlmeEnv)
    ProgressStatus$ModelName= shortModelName()
    numTries <<- 0 
    numTimesToTry=5
    root = newXMLNode("progress")
    listToXML(root,ProgressStatus)
	done=0
	while ( ! done ) {
	    tryCatch ( 
    	{
            XML::saveXML(root,file=file.path(localWorkingDir,"progress.xml"))
            if ( TrackingHistoryFilename != "" ) {
                XML::saveXML(root,file=TrackingHistoryFilename)
            }
            done=1
    	},
    	error = function(ex) {
            numTries <<- numTries + 1 
            if ( numTries < numTimesToTry ) {
            Sys.sleep(1)
            } else {
                done = 1 
print(localWorkingDir)
print(paste0("WARNING: Tried ",numTries," times to write ",localWorkingDir,"/progress.xml"))
    numTries <<- 0 
			}
	    } )
	}
    rm(root)
    gc()
}

UpdateProgressMessages <-function(currentJobDirectory="",progressStage=""){

    ProgressStatus = get("ProgressStatus",envir=nlmeEnv)
    progress = ProgressStatus 
    if ( progressStage != "" ) {
        progress$ProgressStage= progressStage
    }
    else
        progress$ProgressStage= ""
    if ( currentJobDirectory == "" ) {
        GlobalSummaryLine1 = get("GlobalSummaryLine1",envir=nlmeEnv)
        GlobalSummaryLine2 = get("GlobalSummaryLine2",envir=nlmeEnv)
        GlobalSummaryLine3 = get("GlobalSummaryLine3",envir=nlmeEnv)
        progress$DetailInfoLine1= GlobalSummaryLine1
        progress$DetailInfoLine2= GlobalSummaryLine2
        progress$DetailInfoLine3= GlobalSummaryLine3
    }
    else {
        lines=readProgressUpdate(currentJobDirectory,2)
        if ( length(lines) >= 1 ) 
        progress$DetailInfoLine1= lines[1]
        if ( length(lines) >= 2 ) 
        progress$DetailInfoLine2= lines[2]
        if ( length(lines) >= 3 ) 
        progress$DetailInfoLine3= lines[3]
        rm(lines)
    }
    assign("ProgressStatus",progress,envir=nlmeEnv)
    ReportProgress()
    rm(GlobalSummaryLine1)
    rm(GlobalSummaryLine2)
    rm(GlobalSummaryLine3)
    rm(progress)
    gc()
}

updateInitialStatus <- function(runMode="",parallelMethod="",localWorkingDir=""){
    TrackingJobHistoryInitialization()
    progress=list(MachineName="LocalHost", ParallelProtocol="None",ModelName="",StartTime="", EndTime="", Status="InProgress", NumOfSamples=1, NumOfSamplesCompleted=0,NumOfSamplesFailed= 0 , NumOfSamplesExpired = 0 , NumOfSamplesErrored=0)
    progress$NumOfSamples = 1
    progress$Status = "InProgress"
    progress$ParallelProtocol = parallelMethod
    progress$StartTime= getLocalTimeUTC()
    assign("ProgressStatus",progress,envir=nlmeEnv)

    GlobalSummaryLine1=sprintf("Preparing files for %s run",runMode)
    assign("GlobalSummaryLine1", GlobalSummaryLine1, envir=nlmeEnv)
    GlobalSummaryLine2=""
    assign("GlobalSummaryLine2", GlobalSummaryLine2, envir=nlmeEnv)
    GlobalSummaryLine3=""
    assign("GlobalSummaryLine3", GlobalSummaryLine3, envir=nlmeEnv)
    assign("localWorkingDir", localWorkingDir, envir=nlmeEnv)
    UpdateProgressMessages()
}


UpdateProgress <-function(numSamples,numSuccessful, numFailed, numExpired, currentJobDirectory="",progressStage="",numErrors=0){

    ProgressStatus = get("ProgressStatus",envir=nlmeEnv)
    progress = ProgressStatus 
    progress$NumOfSamples= numSamples
    progress$NumOfSamplesCompleted= numSuccessful
    progress$NumOfSamplesFailed= numFailed
    progress$NumOfSamplesExpired= numExpired
    progress$NumOfSamplesErrored= numErrors
    if ( progressStage != "" ) {
        progress$ProgressStage= progressStage
    }
    else
        progress$ProgressStage= ""
    if ( currentJobDirectory == "" ) {
        GlobalSummaryLine1 = get("GlobalSummaryLine1",envir=nlmeEnv)
        GlobalSummaryLine2 = get("GlobalSummaryLine2",envir=nlmeEnv)
        GlobalSummaryLine3 = get("GlobalSummaryLine3",envir=nlmeEnv)
        progress$DetailInfoLine1= GlobalSummaryLine1
        progress$DetailInfoLine2= GlobalSummaryLine2
        progress$DetailInfoLine3= GlobalSummaryLine3
    }
    else {
        lines=readProgressUpdate(currentJobDirectory,2)
        if ( length(lines) >= 1 ) 
        progress$DetailInfoLine1= lines[1]
        if ( length(lines) >= 2 ) 
        progress$DetailInfoLine2= lines[2]
        if ( length(lines) >= 3 ) 
        progress$DetailInfoLine3= lines[3]
        rm(lines)
    }
    assign("ProgressStatus",progress,envir=nlmeEnv)
    ReportProgress()
    gc()
}

CompleteProgress <-function(){

    ProgressStatus = get("ProgressStatus",envir=nlmeEnv)
    progress = ProgressStatus 
    progress$Status = "Finished"
    progress$EndTime= getLocalTimeUTC()
    GlobalSummaryLine1 = "Finished, uploading results"
    assign("GlobalSummaryLine1",GlobalSummaryLine1,envir=nlmeEnv)
    GlobalSummaryLine2 = ""
    assign("GlobalSummaryLine2",GlobalSummaryLine2,envir=nlmeEnv)
    GlobalSummaryLine3 = ""
    assign("GlobalSummaryLine3",GlobalSummaryLine3,envir=nlmeEnv)
    assign("ProgressStatus",progress,envir=nlmeEnv)
    ReportProgress()
}

FailProgress <-function(){

    ProgressStatus = get("ProgressStatus",envir=nlmeEnv)
    progress = ProgressStatus 
    progress$Status = "Failed"
    progress$NumOfSamplesFailed = progress$NumOfSamples
    progress$EndTime= getLocalTimeUTC()
    assign("ProgressStatus",progress,envir=nlmeEnv)
    ReportProgress()
}

CancelProgress <-function(numSamples,numSuccessful, numFailed){
    ProgressStatus = get("ProgressStatus",envir=nlmeEnv)
    progress = ProgressStatus 
    progress$NumOfSamples= numSamples
    progress$NumOfSamplesCompleted= numSuccessful
    progress$NumOfSamplesFailed= numFailed
    progress$Status = "Canceled"
    progress$EndTime= getLocalTimeUTC()
    assign("ProgressStatus",progress,envir=nlmeEnv)
    ReportProgress()
}

CheckUserCommands <-function(){
tryCatch (
{
  localWorkingDir=get("localWorkingDir",envir=nlmeEnv)
  fileName = file.path(localWorkingDir,"nlme.cmd")
  if ( file.exists(fileName) ) {
      lines=scan(fileName,what=character(),sep="\n",quiet=TRUE)
      a=grep("STOP",lines)
      if ( length(a) != 0 ){
print("------------------- We are Canceled------------------")
      rm(a)
      rm(lines)
      rm(localWorkingDir)
      rm(fileName)
      gc()
          return("Canceled")
      }
      a=grep("RESULTS",lines)
      if ( length(a) != 0 ){
print("------------------- Generate Interim Results ------------------")
      rm(a)
      rm(lines)
      rm(localWorkingDir)
      rm(fileName)
      gc()
          return("ReturnResults")
      }
      a=grep("QUIT",lines)
      if ( length(a) != 0 ){
print("------------------- Early Termination ------------------")
      rm(a)
      rm(lines)
      rm(localWorkingDir)
      rm(fileName)
      gc()
          return("EarlyTermination")
      }
  }
  return("" )
},
error=function(ex){
return("" )
})
}



WriteResultsStatusFile<-function(msg){
    localWorkingDir=get("localWorkingDir",envir=nlmeEnv)
    fileName = file.path(localWorkingDir,"nlme_cmd.status")
    tryCatch (
    {
        cat(msg,file=fileName,sep="\n",append=FALSE)
    },
        error=function(ex){
    })
}


IsJobCanceled <-function(){
    stat = CheckUserCommands()
    if ( stat == "Canceled" ) {
        return( TRUE )
    } else {
        return( FALSE )
    }
}

InterimResultsRequested <-function(){
    stat = CheckUserCommands()
    if ( stat == "ReturnResults" ) {
        return( TRUE )
    } else {
        return( FALSE )
    }
}

IsEarlyTerminationRequested <-function(){
    stat = CheckUserCommands()
    if ( stat == "EarlyTermination" ) {
        return( TRUE )
    } else {
        return( FALSE )
    }
}

RemoveUserCommands <-function(){
  localWorkingDir=get("localWorkingDir",envir=nlmeEnv)
  fileName = file.path(localWorkingDir,"nlme.cmd")
  if ( file.exists(fileName) ) {
    file.remove(fileName)
  }
}


mkdir<-function(dirname){
    if ( dir.exists(dirname) == FALSE ) {
        dir.create(dirname,showWarnings = FALSE )
    }
}


#
# Write out globals and routines that are needed on the remote node
#
writeOutGlobals<-function(name){
DEBUG_MASTER("func --> writeOutGlobals() ")
DEBUG_MASTER(paste("name",name))

    UsingBatchTools=get("UsingBatchTools",envir=nlmeEnv)
    submissionPlatform=get("submissionPlatform",envir=nlmeEnv)
    exeFileExtension=get("exeFileExtension",envir=nlmeEnv)
    parallelMethod=get("parallelMethod",envir=nlmeEnv)
    jobType=get("jobType",envir=nlmeEnv)
    num_samples=as.integer(get("num_samples",envir=nlmeEnv))
    num_processes=as.integer(get("num_processes",envir=nlmeEnv))
    localWorkingDir=get("localWorkingDir",envir=nlmeEnv)
    SharedWorkingDir=get("SharedWorkingDir",envir=nlmeEnv)
    files_to_copy=get("files_to_copy",envir=nlmeEnv)
    model_file=get("model_file",envir=nlmeEnv)
    extra_args_file=get("extra_args_file",envir=nlmeEnv)
    MpiExecutable=get("MpiExecutable",envir=.GlobalEnv)
    MpiArgument=get("MpiArgument",envir=.GlobalEnv)
    MpiNumCores=get("MpiNumCores",envir=.GlobalEnv)
    MpiLocal=get("MpiLocal",envir=.GlobalEnv)
DEBUG_MASTER(paste("MpiExecutable",MpiExecutable))
    if ( jobType == "BOOTSTRAP" )  {
        engine=as.integer(get("engine",envir=nlmeEnv))
        num_iterations=as.integer(get("num_iterations",envir=nlmeEnv))
        max_tries=as.integer(get("max_tries",envir=nlmeEnv))
        start_seed=as.integer(get("start_seed",envir=nlmeEnv))
        column_def_file=get("column_def_file",envir=nlmeEnv)
        data_file=get("data_file",envir=nlmeEnv)
        confidence_level=get("confidence_level",envir=nlmeEnv)
    }
    else { 
        control_file=get("control_file",envir=nlmeEnv)
        control_lines=get("control_lines",envir=nlmeEnv)
    }

    cat(sprintf("UsingBatchTools=%s",UsingBatchTools),file=name,sep="\n",append=FALSE)
    cat(sprintf("submissionPlatform=\"%s\"",submissionPlatform),file=name,sep="\n",append=TRUE)
    cat(sprintf("jobType=\"%s\"",jobType),file=name,sep="\n",append=TRUE)
    cat(sprintf("num_samples=%d",num_samples),file=name,sep="\n",append=TRUE)
    cat(sprintf("exeFileExtension=\"%s\"",exeFileExtension),file=name,sep="\n",append=TRUE)
    cat(sprintf("parallelMethod=\"%s\"",parallelMethod),file=name,sep="\n",append=TRUE)
    cat(sprintf("num_processes=%d",num_processes),file=name,sep="\n",append=TRUE)
    cat(sprintf("localWorkingDir=\"%s\"",localWorkingDir),file=name,sep="\n",append=TRUE)
    cat(sprintf("SharedWorkingDir=\"%s\"",SharedWorkingDir),file=name,sep="\n",append=TRUE)
    cat(sprintf("files_to_copy=\"%s\"",files_to_copy),file=name,sep="\n",append=TRUE)
    cat(sprintf("model_file=\"%s\"",model_file),file=name,sep="\n",append=TRUE)
    cat(sprintf("extra_args_file=\"%s\"",extra_args_file),file=name,sep="\n",append=TRUE)

    cat(sprintf("MpiExecutable=\"%s\"",MpiExecutable),file=name,sep="\n",append=TRUE)
    cat(sprintf("MpiArgument=\"%s\"",MpiArgument),file=name,sep="\n",append=TRUE)
    cat(sprintf("MpiLocal=\"%s\"",MpiLocal),file=name,sep="\n",append=TRUE)
    cat(sprintf("MpiNumCores=\"%s\"",MpiNumCores),file=name,sep="\n",append=TRUE)
    cat(sprintf("assign(\"MpiExecutable\",MpiExecutable,envir=.GlobalEnv)"),file=name,sep="\n",append=TRUE)
    cat(sprintf("assign(\"MpiArgument\",MpiArgument,envir=.GlobalEnv)"),file=name,sep="\n",append=TRUE)
    cat(sprintf("assign(\"MpiLocal\",MpiLocal,envir=.GlobalEnv)"),file=name,sep="\n",append=TRUE)
    cat(sprintf("assign(\"MpiNumCores\",MpiNumCores,envir=.GlobalEnv)"),file=name,sep="\n",append=TRUE)

    if ( jobType == "BOOTSTRAP" )  {
        cat(sprintf("engine=%d",engine),file=name,sep="\n",append=TRUE)
        cat(sprintf("num_iterations=%d",num_iterations),file=name,sep="\n",append=TRUE)
        cat(sprintf("max_tries=%d",max_tries),file=name,sep="\n",append=TRUE)
        cat(sprintf("start_seed=%d",start_seed),file=name,sep="\n",append=TRUE)
        cat(sprintf("column_def_file=\"%s\"",column_def_file),file=name,sep="\n",append=TRUE)
        cat(sprintf("data_file=\"%s\"",data_file),file=name,sep="\n",append=TRUE)
        cat(sprintf("confidence_level=%f",confidence_level),file=name,sep="\n",append=TRUE)
    }
    else {
        cat(sprintf("control_file=\"%s\"",control_file),file=name,sep="\n",append=TRUE)
        dput(control_lines,"temp.txt")
        lines=readLines("temp.txt")
        tok=""
        for ( l in lines ) {
            tok=paste0(tok,l)
        }
        cat(sprintf("control_lines=%s",tok),file=name,sep="\n",append=TRUE)
    }

    cat(sprintf("mkdir<-function(dirname){"), file=name, sep="\n",append=TRUE)
    cat(sprintf("    if ( file.exists(dirname) == FALSE ) {"), file=name, sep="\n",append=TRUE)
    cat(sprintf("        dir.create(dirname)"), file=name, sep="\n",append=TRUE)
    cat(sprintf("    }"), file=name, sep="\n",append=TRUE)
    cat(sprintf("}"),file=name, sep="\n", append=TRUE)

#    dput(Certara.NLME:::runNLMESample, file= "temp1.r" )
    dput(runNLMESample, file= "temp2.r" ,control=c("useSource"))
    lines = readLines(sprintf("temp2.r"))
    cat(sprintf("runNLMESample<- function(indx,eArgsFile,ofn,extraArgs=\"\",seed=-1,max_tries=1,exePostfix = \"\")"),file=name, sep="\n", append=TRUE)
    cat(lines[3:length(lines)],file=name,sep="\n",append=TRUE)
#    dput(Certara.NLME:::"getExtraArgumentFilename", file= "temp1.r" )
    dput(getExtraArgumentFilename, file= "temp1.r" ,control=c("useSource"))
    lines = readLines(sprintf("temp1.r"))
    cat(sprintf("getExtraArgumentFilename<- function(line)"),file=name, sep="\n", append=TRUE)
    cat(lines[2:length(lines)],file=name,sep="\n",append=TRUE)
#    dput(Certara.NLME:::"getRunSuccessFilename", file= "temp1.r" )
    dput(getRunSuccessFilename, file= "temp1.r" ,control=c("useSource"))
    lines = readLines(sprintf("temp1.r"))
    cat(sprintf("getRunSuccessFilename<- function(line)"),file=name, sep="\n", append=TRUE)
    cat(lines[2:length(lines)],file=name,sep="\n",append=TRUE)

#    dput(Certara.NLME:::"getExtraArgumentFilenameIndex", file= "temp1.r" )
    dput(getExtraArgumentFilenameIndex, file= "temp1.r" ,control=c("useSource"))
    lines = readLines(sprintf("temp1.r"))
    cat(sprintf("getExtraArgumentFilenameIndex<- function(line)"),file=name, sep="\n", append=TRUE)
    cat(lines[2:length(lines)],file=name,sep="\n",append=TRUE)

    dput(DEBUG_MASTER, file= "temp1.r" ,control=c("useSource"))
    lines = readLines(sprintf("temp1.r"))
    cat(sprintf("DEBUG_MASTER<- function(str)"),file=name, sep="\n", append=TRUE)
    cat(lines[2:length(lines)],file=name,sep="\n",append=TRUE)

    dput(getExePostfix, file= "temp3.r" ,control=c("useSource"))
    lines = readLines(sprintf("temp3.r"))
    cat(sprintf("getExePostfix<- function(line)"),file=name, sep="\n", append=TRUE)
    cat(lines[2:length(lines)],file=name,sep="\n",append=TRUE)
}




runNLMESample<-function(indx,eArgsFile,ofn,extraArgs="",seed=-1,max_tries=1,exePostfix= "" ){

    source("myglobaldefs.r")
DEBUG_MASTER("func ----> runNLMESample() ")
DEBUG_MASTER(paste("exePostfix",exePostfix))
    
    lines=readLines("myglobaldefs.r")

    if ( jobType != "BOOTSTRAP" ){
        engine=1
        column_def_file=""
        data_file=""
    }
    if ( UsingBatchTools == TRUE && !(tolower(parallelMethod) == "multicore" || tolower(parallelMethod) == "none" || tolower(parallelMethod) == "local_mpi") )  {
        baseDirectory=getwd()
        baseJobDirectory=sprintf("%s/registry",baseDirectory)
    } else {
        baseJobDirectory=getwd()
        baseDirectory=baseJobDirectory
    }
    if ( indx == 0 ) {
        workingDir= paste0(baseJobDirectory,"/")
        statusFile=sprintf("%s/S_%03d.status",baseDirectory,indx)
        statusBackupFile=sprintf("%s/S_%03d.status.bak",baseDirectory,indx)
        logFile=paste(workingDir,".status",sep="")
        outFile=paste(workingDir,ofn,sep="")
        workPath= baseJobDirectory
        newFilePath=sprintf("%s/exNLME%s",baseJobDirectory,exeFileExtension)
    }
    else
    {    
    # Create working directory to run individual jobs in
        baseIndx=indx %% 100
        workingDir1=sprintf("%s%sjobs%s%02d%s",baseJobDirectory,.Platform$file.sep,.Platform$file.sep,baseIndx,.Platform$file.sep)
        workingDir=sprintf("%s%sjobs%s%02d%s%d%s",baseJobDirectory,.Platform$file.sep,.Platform$file.sep,baseIndx,.Platform$file.sep,indx,.Platform$file.sep)
#    statusFile=sprintf("%s/../../../S_%03d.status",workingDir,indx)
#    statusBackupFile=sprintf("%s/../../../S_%03d.status.bak",workingDir,indx)
        statusFile=sprintf("%s/S_%03d.status",baseDirectory,indx)
        statusBackupFile=sprintf("%s/S_%03d.status.bak",baseDirectory,indx)
        logFile=paste(workingDir,".status",sep="")
        outFile=paste(workingDir,ofn,sep="")
        mkdir(workingDir1)
        mkdir(workingDir)
        workPath=sprintf("%s/jobs/%02d/%d/",baseJobDirectory,baseIndx,indx)
        newFilePath=sprintf("%s/jobs/%02d/%d/exNLME%s",baseJobDirectory,baseIndx,indx,exeFileExtension)

        # Copy all the input files to the individual job directory
        for ( f in unlist(strsplit(files_to_copy, split=" ") )) {

    
            file.copy(sprintf("%s",f),sprintf("%s/%s",workingDir,f),overwrite=TRUE)
        }
        file.copy(sprintf("%s",eArgsFile),sprintf("%s/%s",workingDir,eArgsFile),overwrite=TRUE)
    }

    # nlmeargs.txt needs @ for commandline
    if( nchar(eArgsFile) > 0  )
        extraArgsFile=sprintf("@%s",eArgsFile)
    else
        extraArgsFile=eArgsFile

    MpiExecutable=get("MpiExecutable",envir=.GlobalEnv)
    MpiArgument=get("MpiArgument",envir=.GlobalEnv)
    MpiNumCores=get("MpiNumCores",envir=.GlobalEnv)
    MpiLocal=get("MpiLocal",envir=.GlobalEnv)
    MpiExecutable=gsub(".exe",paste0(exePostfix,".exe"),MpiExecutable,fixed=TRUE)

DEBUG_MASTER(paste("MpiNumCores",MpiNumCores))
    if ( submissionPlatform == "unix" ) {
        # There are two ways to specify the input files
        # As part of the commandline or inside of nlmeargs.txt
        if ( nchar(column_def_file) > 0 ) {
            commandString=sprintf("%s/execNLMECmd%s RUN %s/%s %s %s %s %s \"\" NLME_DIR \"\" \" %s %s %s %s\" \"\" %s", baseDirectory,exeFileExtension,baseDirectory,model_file ,workingDir,MpiArgument,MpiLocal,MpiNumCores,extraArgs,extraArgsFile,column_def_file,data_file,exePostfix)
        } else {
            commandString=sprintf("%s/execNLMECmd%s RUN %s/%s %s %s %s %s \"\" NLME_DIR \"\" \" %s %s \" \"\" %s", baseDirectory,exeFileExtension,baseDirectory,model_file ,workingDir,MpiArgument,MpiLocal,MpiNumCores,extraArgs,extraArgsFile,exePostfix)
        }
        # Write out the shellscript that we will run
        cat("#!/bin/ksh",file=newFilePath,sep="\n",append=FALSE)
        cat("###set -x",file=newFilePath,sep="\n",append=TRUE)
        cat("typeset -i numTries",file=newFilePath,sep="\n",append=TRUE)
        cat("typeset -i bootSeed",file=newFilePath,sep="\n",append=TRUE)

        cat(paste("cd ",workingDir),file=newFilePath,sep="\n", append=TRUE)
        cat(sprintf("ln -s %s/%s %s",baseDirectory,MpiExecutable,workPath),file=newFilePath,sep="\n", append=TRUE)
        cat(sprintf("chmod 777 %s ",MpiExecutable),file=newFilePath,sep="\n", append=TRUE)
        cat(paste("echo 'RUNNING' >" ,logFile),file=newFilePath,sep="\n",append=TRUE)
        cat(paste("echo 'RUNNING' >" ,statusFile),file=newFilePath,sep="\n",append=TRUE)
        cat(paste("echo '",format(Sys.time(),"%Y-%m-%d %H:%M:%S %Z"),"' >>" ,statusFile),file=newFilePath,sep="\n",append=TRUE)
#        cat(paste("echo  >" ,logFile),file=statusFile,sep="\n",append=TRUE)
        cat(sprintf("maxTries=%d",max_tries),file=newFilePath,sep="\n",append=TRUE)
        cat(sprintf("numTries=1"),file=newFilePath,sep="\n",append=TRUE)
        cat(sprintf("bootSeed=%d",seed),file=newFilePath,sep="\n",append=TRUE)
        cat("startTime=`date +\"%x %T\"`",file=newFilePath,sep="\n",append=TRUE)
        cat(sprintf("while [ ${numTries} -le ${maxTries} ]; do"),file=newFilePath,sep="\n",append=TRUE)
        cat(commandString,file=newFilePath,sep="\n",append=TRUE)
        cat("stopTime=`date +\"%x %T\"`",file=newFilePath,sep="\n",append=TRUE)
        cat(sprintf("((numTries++))"),file=newFilePath,sep="\n",append=TRUE)
        cat(sprintf("bootSeed=${bootSeed}+1"),file=newFilePath,sep="\n",append=TRUE)
        cat("sleep 1",file=newFilePath,sep="\n",append=TRUE)
        cat(paste("if [ -s ", outFile," ]"),file=newFilePath,sep="\n",append=TRUE)
        cat("then",file=newFilePath,sep="\n",append=TRUE)
        cat(paste("echo 'SUCCESS' >" ,logFile),file=newFilePath,sep="\n",append=TRUE)
        cat(paste("echo 'SUCCESS' >" ,statusFile),file=newFilePath,sep="\n",append=TRUE)
        cat(paste("echo ${startTime} >>" ,statusFile),file=newFilePath,sep="\n",append=TRUE)
        cat(paste("echo ${stopTime} >>" ,statusFile),file=newFilePath,sep="\n",append=TRUE)
        cat(paste("break"),file=newFilePath,sep="\n",append=TRUE)
        cat("else",file=newFilePath,sep="\n",append=TRUE)
        cat(paste("if [  ${numTries} -ge ${maxTries} ]"),file=newFilePath,sep="\n",append=TRUE)
        cat("then",file=newFilePath,sep="\n",append=TRUE)
        cat(paste("echo 'FAILED' >" ,logFile),file=newFilePath,sep="\n",append=TRUE)
        cat(paste("echo 'FAILED' >" ,statusFile),file=newFilePath,sep="\n",append=TRUE)
        cat("fi",file=newFilePath,sep="\n",append=TRUE)
        cat("fi",file=newFilePath,sep="\n",append=TRUE)
        cat(sprintf("done"),file=newFilePath,sep="\n",append=TRUE)

        cat(paste("if [ -s ", outFile," ]"),file=newFilePath,sep="\n",append=TRUE)
        cat("then",file=newFilePath,sep="\n",append=TRUE)
        cat(paste("echo 'SUCCESS' >" ,logFile),file=newFilePath,sep="\n",append=TRUE)
        cat(paste("echo 'SUCCESS' >" ,statusFile),file=newFilePath,sep="\n",append=TRUE)
        cat(paste("echo ${startTime} >>" ,statusFile),file=newFilePath,sep="\n",append=TRUE)
        cat(paste("echo ${stopTime} >>" ,statusFile),file=newFilePath,sep="\n",append=TRUE)
        cat("else",file=newFilePath,sep="\n",append=TRUE)
        cat(paste("echo 'FAILED' >" ,logFile),file=newFilePath,sep="\n",append=TRUE)
        cat(paste("echo 'FAILED' >" ,statusFile),file=newFilePath,sep="\n",append=TRUE)
        cat("fi",file=newFilePath,sep="\n",append=TRUE)

        Sys.chmod(newFilePath, mode="0777", use_umask = TRUE)
        if ( indx != 0 && ( toupper(parallelMethod) == "MULTICORE" || toupper(parallelMethod) == "NONE" )  )
            system(paste("nohup ",newFilePath, " > log 2>&1  &"))
        else
            system(paste0(newFilePath, "  > log 2>&1 ",indx," "))
    } else {
        if ( nchar(column_def_file) > 0 ) {
            commandString=sprintf("CALL %s\\execNLMECmd%s RUN %s\\%s %s %s %s %s \"\" NLME_DIR \"\" \" %s %s %s %s\" \"\" %s", gsub("/","\\",baseDirectory,fixed=TRUE),exeFileExtension,gsub("/","\\",baseDirectory,fixed=TRUE),model_file ,gsub("/","\\",workingDir,fixed=TRUE),MpiArgument,MpiLocal,MpiNumCores,extraArgs,extraArgsFile,column_def_file,data_file,exePostfix)
        } else {
            commandString=sprintf("CALL %s\\execNLMECmd%s RUN %s\\%s %s %s %s %s \"\" NLME_DIR \"\" \" %s %s \" \"\" %s", gsub("/","\\",baseDirectory,fixed=TRUE),exeFileExtension,gsub("/","\\",baseDirectory,fixed=TRUE),model_file ,gsub("/","\\",workingDir,fixed=TRUE),MpiArgument,MpiLocal,MpiNumCores,extraArgs,extraArgsFile,exePostfix)
        }
        cat("@echo off",file=newFilePath,sep="\n",append=FALSE)
        if ( indx != 0 ) 
            cat(sprintf("copy %s\\%s %s\\%s",gsub("/","\\",baseDirectory,fixed=TRUE),MpiExecutable,gsub("/","\\",workPath,fixed=TRUE),MpiExecutable),file=newFilePath,sep="\n", append=TRUE)
        cat(paste("@echo RUNNING>" ,gsub("/","\\",logFile,fixed=TRUE)),file=newFilePath,sep="\n",append=TRUE)
        cat(paste("@echo RUNNING>" ,gsub("/","\\",statusFile,fixed=TRUE)),file=newFilePath,sep="\n",append=TRUE)
        cat(paste("@echo '",format(Sys.time(),"%Y-%m-%d %H:%M:%S %Z"),"' >>" ,gsub("/","\\",statusFile,fixed=TRUE)),file=newFilePath,sep="\n",append=TRUE)
#        cat(paste("@echo  >" ,gsub("/","\\",logFile,fixed=TRUE)),file=statusFile,sep="\n",append=TRUE)
        cat(sprintf("set maxTries=%d",max_tries),file=newFilePath,sep="\n",append=TRUE)
        cat(sprintf("set statusRetries=1"),file=newFilePath,sep="\n",append=TRUE)
        cat(sprintf("set numTries=1"),file=newFilePath,sep="\n",append=TRUE)
        cat(sprintf("set bootSeed=%d",seed),file=newFilePath,sep="\n",append=TRUE)
        cat("set startTime=%DATE:~10,4%/%DATE:~4,4% %TIME%",file=newFilePath,sep="\n",append=TRUE)
        cat(sprintf(":DoItAgain"),file=newFilePath,sep="\n",append=TRUE)
        cat(commandString,file=newFilePath,sep="\n",append=TRUE)
        cat("set stopTime=%DATE:~10,4%/%DATE:~4,4% %TIME%",file=newFilePath,sep="\n",append=TRUE)
        cat(sprintf(":RetryStatus"),file=newFilePath,sep="\n",append=TRUE)
        cat("if %statusRetries% GTR 20 goto DONE",file=newFilePath,sep="\n",append=TRUE)
        cat("SET /A statusRetries=%statusRetries%+1",file=newFilePath,sep="\n",append=TRUE)
        cat(paste("if EXIST ", gsub("/","\\",outFile,fixed=TRUE)," goto SUCCESS"),file=newFilePath,sep="\n",append=TRUE)
        cat(paste("SET /A numTries=%numTries%+1"),file=newFilePath,sep="\n",append=TRUE)
        cat(paste("SET /A bootSeed=%bootSeed%+1"),file=newFilePath,sep="\n",append=TRUE)
        cat(paste("if %numTries% LEQ %maxTries% goto DoitAgain"),file=newFilePath,sep="\n",append=TRUE)
        cat(paste("@echo FAILED>" ,gsub("/","\\",logFile,fixed=TRUE)),file=newFilePath,sep="\n",append=TRUE)
        cat(paste("move ",gsub("/","\\",statusFile,fixed=TRUE),gsub("/","\\",statusBackupFile,fixed=TRUE)),file=newFilePath,sep="\n",append=TRUE)

        cat(paste("if NOT ERRORLEVEL 0 ("),file=newFilePath,sep="\n",append=TRUE)
        cat(paste("@echo %ERRORLEVEL%"),file=newFilePath,sep="\n",append=TRUE)
        cat(paste("@echo File is locked on move"),file=newFilePath,sep="\n",append=TRUE)
        cat(paste("timeout /t 1"),file=newFilePath,sep="\n",append=TRUE)
        cat(paste("goto RetryStatus"),file=newFilePath,sep="\n",append=TRUE)
        cat(paste(")"),file=newFilePath,sep="\n",append=TRUE)

        cat(paste("@echo FAILED>" ,gsub("/","\\",statusFile,fixed=TRUE)),file=newFilePath,sep="\n",append=TRUE)

        cat(paste("if NOT ERRORLEVEL 0 ("),file=newFilePath,sep="\n",append=TRUE)
        cat(paste("@echo %ERRORLEVEL%"),file=newFilePath,sep="\n",append=TRUE)
        cat(paste("@echo File is locked on write"),file=newFilePath,sep="\n",append=TRUE)
        cat(paste("goto RetryStatus"),file=newFilePath,sep="\n",append=TRUE)
        cat(paste(")"),file=newFilePath,sep="\n",append=TRUE)

        cat(paste("goto DONE"),file=newFilePath,sep="\n",append=TRUE)
        cat(paste(":SUCCESS"),file=newFilePath,sep="\n",append=TRUE)
        cat(paste("@echo SUCCESS>" ,gsub("/","\\",logFile,fixed=TRUE)),file=newFilePath,sep="\n",append=TRUE)
        cat(paste("move ",gsub("/","\\",statusFile,fixed=TRUE),gsub("/","\\",statusBackupFile,fixed=TRUE)),file=newFilePath,sep="\n",append=TRUE)

        cat(paste("if NOT ERRORLEVEL 0 ("),file=newFilePath,sep="\n",append=TRUE)
        cat(paste("@echo %ERRORLEVEL%"),file=newFilePath,sep="\n",append=TRUE)
        cat(paste("@echo File is locked on move"),file=newFilePath,sep="\n",append=TRUE)
        cat(paste("timeout /t 1"),file=newFilePath,sep="\n",append=TRUE)
        cat(paste("goto RetryStatus"),file=newFilePath,sep="\n",append=TRUE)
        cat(paste(")"),file=newFilePath,sep="\n",append=TRUE)

        cat(paste("@echo SUCCESS>" ,gsub("/","\\",statusFile,fixed=TRUE)),file=newFilePath,sep="\n",append=TRUE)

        cat(paste("if NOT ERRORLEVEL 0 ("),file=newFilePath,sep="\n",append=TRUE)
        cat(paste("@echo %ERRORLEVEL%"),file=newFilePath,sep="\n",append=TRUE)
        cat(paste("@echo File is locked on write"),file=newFilePath,sep="\n",append=TRUE)
        cat(paste("goto RetryStatus"),file=newFilePath,sep="\n",append=TRUE)
        cat(paste(")"),file=newFilePath,sep="\n",append=TRUE)

        cat(paste("@echo %startTime% >>" ,gsub("/","\\",statusFile,fixed=TRUE)),file=newFilePath,sep="\n",append=TRUE)

        cat(paste("if NOT ERRORLEVEL 0 ("),file=newFilePath,sep="\n",append=TRUE)
        cat(paste("@echo %ERRORLEVEL%"),file=newFilePath,sep="\n",append=TRUE)
        cat(paste("@echo File is locked on write"),file=newFilePath,sep="\n",append=TRUE)
        cat(paste("goto RetryStatus"),file=newFilePath,sep="\n",append=TRUE)
        cat(paste(")"),file=newFilePath,sep="\n",append=TRUE)

        cat(paste("@echo %stopTime% >>" ,gsub("/","\\",statusFile,fixed=TRUE)),file=newFilePath,sep="\n",append=TRUE)

        cat(paste("if NOT ERRORLEVEL 0 ("),file=newFilePath,sep="\n",append=TRUE)
        cat(paste("@echo %ERRORLEVEL%"),file=newFilePath,sep="\n",append=TRUE)
        cat(paste("@echo File is locked on write"),file=newFilePath,sep="\n",append=TRUE)
        cat(paste("goto RetryStatus"),file=newFilePath,sep="\n",append=TRUE)
        cat(paste(")"),file=newFilePath,sep="\n",append=TRUE)

        cat(paste(":DONE"),file=newFilePath,sep="\n",append=TRUE)
        cat(paste("EXIT /B"),file=newFilePath,sep="\n",append=TRUE)
        if ( ( indx != 0 ) && ( toupper(parallelMethod) == "MULTICORE" || toupper(parallelMethod) == "NONE" )  ) {

#print("---------------------------------------------")
#print(getwd())
#print(paste("shell(", paste("START /MIN CMD /C CALL ", newFilePath),", wait=FALSE,intern=TRUE)"))
##print(paste("shell(", paste("CALL CMD /C CALL ", newFilePath),", wait=FALSE)"))
#print("---------------------------------------------")
#            stat=shell(paste("START /B CMD /C CALL ", newFilePath),wait=FALSE)
            stat=shell(paste("START /MIN CMD /C CALL ", newFilePath),wait=FALSE,intern=TRUE)
#            stat=shell(paste( newFilePath),wait=FALSE,intern=TRUE)
#            stat=shell(paste( newFilePath),wait=TRUE,intern=TRUE)
#            stat=shell(paste( newFilePath))

        }
        else 
        {
            stat=shell( newFilePath)
        }
    }

    if ( submissionPlatform == "unix" ) {
    } else {
    }
}



runNLMEBootstrapSample<- function(indx,seed) { 

    source(sprintf("myglobaldefs.r"))
DEBUG_MASTER("func --> runNLMEBootstrapSample() ")

    if ( submissionPlatform == "unix" ) {
        bootstrapArgs=sprintf("/m %d /n %d /boot ${bootSeed} /bootsamp %d -boottry ${numTries}",engine,num_iterations,indx )
    }
    else {
        bootstrapArgs=sprintf("/m %d /n %d /boot %%bootSeed%% /bootsamp %d -boottry %%numTries%%",engine,num_iterations,indx )
    }

    runNLMESample(indx, extra_args_file,"out.txt",bootstrapArgs,seed,max_tries)

}


runNLMEInitialSample<- function(indx) { 
    source(sprintf("myglobaldefs.r"))
DEBUG_MASTER("func --> runNLMEInitialSample() ")

    runNLMESample(indx, extra_args_file,"out.txt")
DEBUG_MASTER("func --> runNLMEInitialSample() Finished")
}


runNLMEGenericSample<- function(jobIndx) { 

    source(sprintf("myglobaldefs.r"))
DEBUG_MASTER("func --> runNLMEGenericSample() ")
DEBUG_MASTER(Sys.info()["nodename"])
DEBUG_MASTER(paste("Index is ",jobIndx))
 
    extraArgsFile =getExtraArgumentFilename(control_lines[jobIndx])
    extraArgsFileIndx =getExtraArgumentFilenameIndex(control_lines[jobIndx])
DEBUG_MASTER(extraArgsFile)
DEBUG_MASTER(extraArgsFileIndx)
    if ( ! is.na(extraArgsFileIndx) ){
        lines=readLines(extraArgsFile)
        extraArgsFile=sprintf("%s.%s",extraArgsFile,extraArgsFileIndx)
        idx=as.integer(extraArgsFileIndx)
        numLines=length(lines)
# Due to these records not all being usied in one run, we need a different 
# method to find num lines
#  numLinesPerRecord=numLines/num_samples
        numLinesPerRecord=numLines/length(grep("-anagrad",lines))
        appendFlag=FALSE
DEBUG_MASTER(paste("numLinesPerRecord",numLinesPerRecord))
DEBUG_MASTER(paste("Index is",idx))
        for ( i in 1:numLinesPerRecord ) {
            cat(lines[(idx-1)*numLinesPerRecord + i ],file=extraArgsFile,sep="\n",append=appendFlag)
            appendFlag=TRUE
        }
    }
    outputFileName =getRunSuccessFilename(control_lines[jobIndx])
    ep =getExePostfix(control_lines[jobIndx])

DEBUG_MASTER(paste("line is ",control_lines[jobIndx]))
DEBUG_MASTER(paste0("exePostfix is .",ep,"."))
    runNLMESample(jobIndx, extraArgsFile,outputFileName,exePostfix=ep)

    if ( ! is.na(extraArgsFileIndx) ){
        file.remove(extraArgsFile)
    }
}



getFrozenFixedEffectKey <- function(fixEffName,frozenValue){
    key=sprintf("%s_%05.2f",fixEffName,frozenValue)
    return(key)
}

generateFrozenModelFile <-function(origModelFilename, 
                                   frozenModelFilename, 
                                   fixEffName, 
                                   frozenValue){

    lines=readLines(origModelFilename)
    cat(lines,file=frozenModelFilename,sep="\n",append=FALSE)
    modelName=unlist(strsplit(lines[1], split="(",fixed=TRUE))[1]
    cat(sprintf("override %s(){",modelName),file=frozenModelFilename,sep="\n",append=TRUE)
    cat(sprintf("fixef(%s(freeze) = c(,%s,))",fixEffName,frozenValue),file=frozenModelFilename,sep="\n",append=TRUE)
    cat(sprintf("}",modelName),file=frozenModelFilename,sep="\n",append=TRUE)

    
}

generateProfileModels <- function(){

    nxt=1
    if (exists("profileDescriptors", envir = nlmeEnv)) 
        profileDescriptors=get("profileDescriptors",envir=nlmeEnv)
    else 
        profileDescriptors = NULL
    profileModels=array(data=list(modelName="", exePostfix=""),dim=1)
    if ( length(profileDescriptors)  < 1  || profileDescriptors == ""  ) {
        modelSpec=list(modelName="test.mdl", exePostfix="",theta="",initialValue="",percentage="",delta="")
        profileModels[[1]]=modelSpec
#        profileModels[[2]]=NULL
    } else {
        profilePercentFlag=get("profilePercentFlag",envir=nlmeEnv)
        for ( prof in unlist(strsplit(profileDescriptors, split=" ")) ) {
            tokens=unlist(strsplit(prof, split=",")) 
            fixEffName=tokens[1]
            initialValue=as.double(tokens[2])
            for ( indx in  3:length(tokens) ) {
                if ( profilePercentFlag == "USE_PERCENTAGE" ) {
                    percent = as.double(tokens[indx])
                    delta = ""
                    frozenValue=initialValue * ( 1 + percent/100 )
                }
                else {
                    delta = as.double(tokens[indx])
                    percent = ""
                    frozenValue=initialValue + delta
                }
                frozenKey=getFrozenFixedEffectKey(fixEffName,frozenValue)
                newModelFilename=sprintf("test_%s.mdl",frozenKey)
                generateFrozenModelFile("test.mdl",newModelFilename,fixEffName,frozenValue)
                modelSpec=list(modelName=newModelFilename, exePostfix=frozenKey,theta=fixEffName,initialValue=frozenValue,percentage=percent,delta=delta)
                profileModels[[nxt]]=modelSpec
                nxt=nxt+1
            }
        }
    }

    assign("profileModels", profileModels, envir=nlmeEnv)
    return(profileModels)
}

getListOfExesNeeded <- function(){
DEBUG_MASTER("profileModels")
    if (exists("profileModels", envir = nlmeEnv))  {
        profileModels=get("profileModels",envir=nlmeEnv)
DEBUG_MASTER(paste(profileModels))
    }
    else 
        profileModels = NULL
    return(profileModels)
}


updateCompileStatus <- function(indx,tot){
    GlobalSummaryLine1=sprintf("Compiling %d of %d NLME models",indx,tot)
    assign("GlobalSummaryLine1", GlobalSummaryLine1, envir=nlmeEnv)
    GlobalSummaryLine2=""
    assign("GlobalSummaryLine2", GlobalSummaryLine2, envir=nlmeEnv)
    GlobalSummaryLine3=""
    assign("GlobalSummaryLine3", GlobalSummaryLine3, envir=nlmeEnv)
    rm(GlobalSummaryLine1)
    rm(GlobalSummaryLine2)
    rm(GlobalSummaryLine3)
    gc()
    UpdateProgressMessages()
}


#
# Compile the model and create NLME.exe
#
compileAndLinkNLME<- function() { 
DEBUG_MASTER("func --> compileAndLinkNLME()")

    listOfExesToBuild = getListOfExesNeeded()
    tot=length(listOfExesToBuild)
    if ( tot == 0 ) {
        ret = generateNLMEScriptAndRun("COMPILE")
        if ( ret == FALSE )
            return(FALSE);
    }
    current=1
    ret = TRUE
    for ( l in listOfExesToBuild ) {
        modelFilename=l$modelName
        exePostfix=l$exePostfix
        updateCompileStatus(current,tot)
        ret = generateNLMEScriptAndRun("COMPILE",modelFilename,exePostfix)
        if ( ret == FALSE )
            return(FALSE);
        current= current + 1
        rm(modelFilename)
        rm(exePostfix)
    }
    GlobalSummaryLine1=""
    assign("GlobalSummaryLine1", GlobalSummaryLine1, envir=nlmeEnv)
    UpdateProgressMessages()
    rm(listOfExesToBuild)
    rm(GlobalSummaryLine1)
    gc()
    return(ret)
}

#
# Return the factor used to determine mpi num cores
#
getNlmePopulationFactor <- function(){
    NLME_POPULATION_FACTOR = 3
    userValue=Sys.getenv("NLME_POPULATION_FACTOR")
    if ( userValue != "" )
        NLME_POPULATION_FACTOR = as.integer(userValue)
    if ( NLME_POPULATION_FACTOR < 1 )
        NLME_POPULATION_FACTOR = 3
    return(NLME_POPULATION_FACTOR)
}

mpiAutoNumCores <- function(num_samples, smallestPopulation){

    NLME_POPULATION_FACTOR = getNlmePopulationFactor()

    num_processes=get("num_processes",envir=nlmeEnv)
    extraCores = as.integer(num_processes / num_samples)
    if ( extraCores < 1 ) 
        extraCores = 1 

    userValue=Sys.getenv("NLME_POP_MPI_FACTOR")
    if ( userValue != "" ) {
        NLME_POP_MPI_FACTOR = as.integer(userValue)
        extraCoresAllowed = num_processes / NLME_POP_MPI_FACTOR
        extraCores = as.integer(num_processes / num_samples)
        if ( extraCores < 1 ) 
            extraCores = 1 
        if ( extraCoresAllowed > extraCores )
            extraCores = extraCoresAllowed 
    }

    if ( extraCores > 1 ) {
        guessAtBestNo = smallestPopulation/ NLME_POPULATION_FACTOR 
        if ( extraCores > guessAtBestNo )
            extraCores = guessAtBestNo
        extraCores=as.integer(extraCores)
        if ( extraCores <= 0 ) 
            extraCores = 1 
    }
DEBUG_MASTER(paste("****************", extraCores, "***************"))
DEBUG_MASTER(num_samples)
DEBUG_MASTER(num_processes)
DEBUG_MASTER(smallestPopulation)
    return(extraCores)
}


figureOutMpiNumCores <-function(num_samples){

    column_def_file=get("column_def_file",envir=nlmeEnv)
    data_file=get("data_file",envir=nlmeEnv)

    smallestPopulation=getNumSubjects(column_def_file, data_file )

    num_cores=mpiAutoNumCores(num_samples,smallestPopulation)
DEBUG_MASTER(paste("num_samples",num_samples,"smallestPopulation",smallestPopulation,"num_cores",num_cores))
    return(num_cores)
}

figureOutMpiNumCoresForPop <-function(num_samples,control_file){

    smallestPopulation=getMinimumNumSubjects(control_file)

    num_cores=mpiAutoNumCores(num_samples,smallestPopulation)

    return(num_cores)
}

requestStopEarly <-function(sharedWorkingDir,jobId){
    baseJobDirectory=getBatchDirectoryLocation(SharedWorkingDir)
    jobId = 1 
    baseIndx=jobId
    stopFile=sprintf("%s/jobs/%02d/%d/stop.txt",baseJobDirectory,baseIndx,jobId)
    if (file.exists(sprintf("%s/jobs/%02d/%d",baseJobDirectory,baseIndx,jobId)))
        cat("STOP",file=stopFile,sep="\n",append=FALSE)
}

ResetMPIFlags <-function(dir,numCores){
DEBUG_MASTER("ResetMPIFlags()")
DEBUG_MASTER(paste("numCores",numCores))
DEBUG_MASTER(paste("dir",dir))
    if ( numCores == 1 ) {
        MpiNumCores = 1
        MpiArgument = "MPINO"
        MpiExecutable = "NLME7.exe"
    } else {
        MpiNumCores = numCores
        MpiArgument = "MPIYES"
        MpiExecutable = "mpiNLME7.exe"
    }
    assign("MpiArgument", MpiArgument, envir=.GlobalEnv)
    assign("MpiExecutable", MpiExecutable, envir=.GlobalEnv)
    assign("MpiNumCores", MpiNumCores, envir=.GlobalEnv)
    writeOutGlobals(sprintf("%s/myglobaldefs.r",dir))
}

#
# Run an NLME job to get initial estimates used in bootstrap
# 
runNLMEInitialRun  <- function() { 
DEBUG_MASTER("func --> runNLMEInitialRun()")

files= c( "VarCoVar.csv","doses.csv", "err2.txt", "err1.txt", "IniCovr.txt", "MultCovr.txt", "progress.txt", "IdEta.txt","dmp.txt","EtaEta.txt","EtaCov.txt","StrCov.txt","EtaShrinkageBySubject","bluptable.dat","etameansnp.asc","nparsupport.asc")

    MpiNumCores = get("MpiNumCores",envir=.GlobalEnv)
    MpiArgument=get("MpiArgument",envir=.GlobalEnv)
    parallelMethod = get("parallelMethod",envir=nlmeEnv)
    localWorkingDir=get("localWorkingDir",envir=nlmeEnv)
    SharedWorkingDir=get("SharedWorkingDir",envir=nlmeEnv)

    OldMpiNumCores = MpiNumCores
    NewMpiNumCores = MpiNumCores
    if ( MpiArgument == "MPINO" ) 
        num = 1 
    else
        num = figureOutMpiNumCores(1)
    NewMpiNumCores = num
    MpiNumCores = NewMpiNumCores

#
# We need to account for the case where MPI is being used and run's 
# number of cores is one.  We do not want to use MPI for this case
# if we used MPI for initial estimates run in the above scenario, we need
# to regenerate the NLME7.exe and turn off MPI
#
    if ( OldMpiNumCores == 1 && MpiArgument != "MPINO" && NewMpiNumCores > 1){
        OldMpiExecutable = MpiExecutable
        OldMpiArgument = MpiArgument 
        MpiExecutable="NLME7.exe"
        MpiArgument="MPINO"
        assign("MpiExecutable", MpiExecutable, envir=.GlobalEnv)
        assign("MpiArgument", MpiArgument, envir=.GlobalEnv)
        copyFiles(SharedWorkingDir)
        generateNLMEScriptAndRun("COMPILE")
    }

    assign("MpiNumCores", MpiNumCores, envir=.GlobalEnv)

# 
# Switch and create a new registry
#
    OldSharedWorkingDir=SharedWorkingDir

    jobHome=get("jobHome",envir=nlmeEnv)
    UsingBatchTools=get("UsingBatchTools",envir=nlmeEnv)
    SharedWorkingDir=gsub("\\","/",tempfile(pattern="NLME",tmpdir=jobHome),fixed=TRUE)
#    dir.create(SharedWorkingDir)
    mkdir(SharedWorkingDir)
    assign("SharedWorkingDir", SharedWorkingDir, envir=nlmeEnv)
    setwd(SharedWorkingDir)

    ResetMPIFlags(SharedWorkingDir,NewMpiNumCores)

DEBUG_MASTER(sprintf("Writing out globals to %s/myglobaldefs.r",SharedWorkingDir))
    writeOutGlobals(sprintf("%s/myglobaldefs.r",SharedWorkingDir))
    if ( UsingBatchTools == TRUE )  {
         registryDir=getBatchDirectoryLocation(SharedWorkingDir)
        gridRegistry2 <- makeRegistry(file.dir=registryDir, work.dir=SharedWorkingDir)
        if ( length(grep("torque",tolower(parallelMethod))) > 0 )  
            if ( exists("makeClusterFunctionsTorque") )
                gridRegistry2$cluster.functions=makeClusterFunctionsTorque(template=sprintf("%s/batchtools.torque.tmpl",localWorkingDir))
            else 
                gridRegistry2$cluster.functions=makeClusterFunctionsTORQUE(template=sprintf("%s/batchtools.torque.tmpl",localWorkingDir))
         else {
            if ( length(grep("sge",tolower(parallelMethod))) > 0 )  
                gridRegistry2$cluster.functions=makeClusterFunctionsSGE(template=sprintf("%s/batchtools.sge.tmpl",localWorkingDir))
            else {
DEBUG_MASTER("makeClusterFunctionsLSF")
                gridRegistry2$cluster.functions=makeClusterFunctionsLSF(template=sprintf("%s/batchtools.lsf.tmpl",localWorkingDir))
            }
        }
    } else
        gridRegistry2 <- makeRegistry(file.dir=SharedWorkingDir,work.dir=SharedWorkingDir)

#
# Copy files and generate executable
#
    copyFiles(SharedWorkingDir)
    generateNLMEScriptAndRun("COMPILE")

    jobIds=rep(1,1)
    if ( UsingBatchTools == TRUE )   {
        bids  <- batchMap(reg=gridRegistry2, fun=runNLMEInitialSample, indx=jobIds )
        done <- submitJobs(reg=gridRegistry2, resources = list(nodes = NewMpiNumCores, walltime="18:00:00"), ids=bids)
    } else {
        bids  <- batchMap(gridRegistry2, runNLMEInitialSample, jobIds ,use.names=TRUE)
        done <- submitJobs(gridRegistry2, resources = list(nodes = NewMpiNumCores, walltime="18:00:00"))
        rm(done)
    }
    jobs = getJobTable(reg=gridRegistry2)
assign("gridJobs2", jobs$batch.id, envir=nlmeEnv)

    numSuccessful = 0 
    numFailed = 0 
    done = FALSE
    num_samples = 1
    numSuccessuful = 0
    numFailed = 0
    numExpired = 0
    while ( !done  ) {
memory_usage(.GlobalEnv)
memory_usage(nlmeEnv)
        jobInProgressPath=""
        if ( IsJobCanceled() ){
            killAGridJob(gridRegistry2)
            CancelProgress(num_samples, numSuccessful, numFailed )
            done = TRUE
            break
        }
        if ( IsEarlyTerminationRequested() ) {
            requestStopEarly(sharedWorkingDir,1)

        }
        tryCatch (
        {
            if ( UsingBatchTools == TRUE )   {
                running = findRunning(ids=NULL, reg=gridRegistry2)
                if ( nrow(running) > 0  )
                    running=running$job.id
                else
                    running=c()
            } else
                running = findRunning(gridRegistry2)
            if ( length(running) >= 1 ) {
                d=running[1]
                baseIndx=d %% 100
                jobInProgressPath=sprintf("%s/jobs/%02d/%d/",getBatchDirectoryLocation(SharedWorkingDir),baseIndx,d)
           }
           rm(running)
           {
                if ( UsingBatchTools == TRUE ) { 
                    doneJobs = findDone(ids=NULL, reg=gridRegistry2)
                    if ( nrow(doneJobs) > 0 ) 
                        doneJobs=doneJobs$job.id
                    else
                        doneJobs=c()
                } else
                    doneJobs = findDone(gridRegistry2)
                if ( length(doneJobs) > 0 ) {
                if ( UsingBatchTools == TRUE ) 
                    d=doneJobs[[1]]
                else
                    d=doneJobs[1]
                baseIndx=d %% 100
                statusFile=sprintf("%s/jobs/%02d/%d/.status",getBatchDirectoryLocation(SharedWorkingDir),baseIndx,d)
                if ( file.exists(statusFile)) {
                    stat = readChar(statusFile, file.info(statusFile)$size - 1 )
                    stat = gsub("\r","",stat, fixed=TRUE) 
                    done = TRUE
                    break
                }
               }
               rm(doneJobs)
           }
        },
        error=function(ex){
            jobInProgressPath=""
print(ex)
        })
        UpdateProgress(num_samples, numSuccessful, numFailed, numExpired , currentJobDirectory = jobInProgressPath,progressStage="Initial Estimates ")
gc()
memory_usage(.GlobalEnv)
memory_usage(nlmeEnv)
    }
#
# Copy results back to original registry
#
    wd=sprintf("%s/jobs/%02d/%d/",getBatchDirectoryLocation(SharedWorkingDir),1,1)
DEBUG_MASTER(paste("wd :",wd))
    MpiExecutable=get("MpiExecutable",envir=.GlobalEnv)
    status=TRUE
    if ( file.exists(sprintf("%s/out.txt",wd)) ){
        for ( f in files ){
            file.copy(sprintf("%s/%s",wd,f),sprintf("../%s",f),overwrite=TRUE)
        }
        if ( submissionPlatform == "unix" ) {
            system(sprintf("cp %s/out.txt ../out_initialEstimates.txt",wd))
            system(sprintf("cp %s/EtaShrinkageBySubject.txt ../EtaShrinkageBySubject.txt",wd))
            system(sprintf("cp %s/nlme7engine.log ../nlme7engine.log",wd))
            system(sprintf("cp %s/dmp.txt  %s/dmp.txt",wd,OldSharedWorkingDir))
            system(sprintf("cp %s/%s  %s/%s",wd,MpiExecutable,OldSharedWorkingDir,MpiExecutable))
        }
        else {
            wd = gsub("/","\\\\",wd,fixed=TRUE)
DEBUG_MASTER(sprintf("copy %s\\out.txt ..\\out_initialEstimates.txt",wd))
            shell(sprintf("copy %s\\out.txt ..\\out_initialEstimates.txt",wd))
DEBUG_MASTER(sprintf("copy %s\\EtaShrinkageBySubject.txt ..\\EtaShrinkageBySubject.txt",wd))
            shell(sprintf("copy %s\\EtaShrinkageBySubject.txt ..\\EtaShrinkageBySubject.txt",wd))
DEBUG_MASTER(sprintf("copy %s\\nlme7engine.log ..\\nlme7engine.log",wd))
            shell(sprintf("copy %s\\nlme7engine.log ..\\nlme7engine.log",wd))
DEBUG_MASTER(sprintf("copy %s\\dmp.txt  %s\\dmp.txt",wd,gsub("/","\\",OldSharedWorkingDir,fixed=TRUE)))
            shell(sprintf("copy %s\\dmp.txt  %s\\dmp.txt",wd,gsub("/","\\",OldSharedWorkingDir,fixed=TRUE)))
DEBUG_MASTER(sprintf("copy %s\\%s  %s\\%s",wd,MpiExecutable,gsub("/","\\",OldSharedWorkingDir,fixed=TRUE),MpiExecutable))
            shell(sprintf("copy %s\\%s  %s\\%s",wd,MpiExecutable,gsub("/","\\",OldSharedWorkingDir,fixed=TRUE),MpiExecutable))
        }

        status = TRUE
    } else {
        if ( file.exists(sprintf("%s/err2.txt",wd)) ){
            file.copy(sprintf("%s/err2.txt",wd),sprintf("../err2.txt"),overwrite=TRUE)
            lines=readLines(sprintf("%s/err2.txt",wd))
            print(lines)
        }
        ProgressStatus = get("ProgressStatus",envir=nlmeEnv)
        progress = ProgressStatus 
        numSamples = numFailed = progress$NumOfSamples
        numSuccessful = 0 
        UpdateProgress(numSamples,numSuccessful, numFailed, 0 )
        FailProgress()
        status = FALSE
    }
    flag=""
    flag=Sys.getenv("NLME_KEEP_GRID_RESULTS")
    DEBUG_MASTER(flag)

    for ( t in 1:1 ) {
        tryCatch (
        {
            if ( flag != TRUE ) {
            }
            break
        },
            error=function(ex){
    	    Sys.sleep(1)
            })
    }
    setwd(OldSharedWorkingDir)
    assign("gridRegistry2", gridRegistry2, envir=nlmeEnv)
    assign("SharedWorkingDir", OldSharedWorkingDir, envir=nlmeEnv)
DEBUG_MASTER("Done running initial estimates")

    ResetMPIFlags(OldSharedWorkingDir,OldMpiNumCores)

    assign("MpiNumCores", OldMpiNumCores, envir=.GlobalEnv)
    return(status)
}



#
# Grab files needed from initial estimates run(that we did not run!)
#
fakeNLMEInitialRun  <- function(jobId) { 

DEBUG_MASTER("func --> fakeNLMEInitialRun()")
files= c( "VarCoVar.csv","doses.csv", "err2.txt", "err1.txt", "IniCovr.txt", "MultCovr.txt", "progress.txt", "IdEta.txt","dmp.txt","EtaEta.txt","EtaCov.txt","StrCov.txt","EtaShrinkageBySubject","bluptable.dat","etameansnp.asc","nparsupport.asc")

    SharedWorkingDir=get("SharedWorkingDir",envir=nlmeEnv)

    setwd(SharedWorkingDir)
    baseDirectory =getBatchDirectoryLocation(SharedWorkingDir)
#
# Copy results back to original registry
#
    wd=sprintf("%s/jobs/%02d/%d/",baseDirectory,jobId,jobId)
   
    status=TRUE 
    if ( file.exists(sprintf("%s/out.txt",wd)) ){
        for ( f in files ){
            file.copy(sprintf("%s/%s",wd,f),sprintf("../%s",f),overwrite=TRUE)
        }
        if ( submissionPlatform == "unix" ) {
            system(sprintf("cp %s/out.txt ../out_initialEstimates.txt",wd))
            system(sprintf("cp %s/EtaShrinkageBySubject.txt ../EtaShrinkageBySubject.txt",wd))
            system(sprintf("cp %s/nlme7engine.log ../nlme7engine.log",wd))
        }
        else {
            shell(sprintf("copy %s\\out.txt ..\\out_initialEstimates.txt",wd))
            shell(sprintf("copy %s\\EtaShrinkageBySubject.txt ..\\EtaShrinkageBySubject.txt",wd))
            shell(sprintf("copy %s\\nlme7engine.log ..\\nlme7engine.log",wd))
        }

        status = TRUE
    } else {
        ProgressStatus = get("ProgressStatus",envir=nlmeEnv)
        progress = ProgressStatus 
        numSamples = numFailed = progress$NumOfSamples
        numSuccessful = 0 
        UpdateProgress(numSamples,numSuccessful, numFailed, 0 )
        FailProgress()
        status = FALSE
    }
    return(status)
}




OLDrunNLMEInitialRun  <- function() { 
files= c( "VarCoVar.csv","doses.csv", "err2.txt", "err1.txt", "IniCovr.txt", "MultCovr.txt", "progress.txt", "IdEta.txt","dmp.txt","EtaEta.txt","EtaCov.txt","StrCov.txt","EtaShrinkageBySubject","bluptable.dat","nparsupport.asc","etameansnp.asc")
    SharedWorkingDir=get("SharedWorkingDir",envir=nlmeEnv)
    generateNLMEScriptAndRun("COMPILE")
    runNLMEInitialSample(0)
    if ( file.exists("out.txt")) {
        for ( f in files ){
            file.copy(sprintf("%s",f),sprintf("../%s",f),overwrite=TRUE)
        }
        if ( submissionPlatform == "unix" ) {
            system("cp out.txt ../out_initialEstimates.txt")
            system("cp EtaShrinkageBySubject.txt ../EtaShrinkageBySubject.txt")
            system("cp nlme7engine.log ../nlme7engine.log")
        }
        else {
            shell("copy out.txt ..\\out_initialEstimates.txt")
            shell("copy EtaShrinkageBySubject.txt ..\\EtaShrinkageBySubject.txt")
            shell("copy nlme7engine.log ..\\nlme7engine.log")
        }
        return(TRUE)
    } else {
        ProgressStatus = get("ProgressStatus",envir=nlmeEnv)
        progress = ProgressStatus 
        numSamples = numFailed = progress$NumOfSamples
        numSuccessful = 0 
        UpdateProgress(numSamples,numSuccessful, numFailed, 0 )
        FailProgress()
        return(FALSE)
    }
}









generateNLMEScriptAndRun <-function(runType,customModel="",exePostfix=""){
    source(sprintf("myglobaldefs.r"))
    if ( customModel!= "" )
        model_file = customModel
DEBUG_MASTER("func --> generateNLMEScriptAndRun() ")
DEBUG_MASTER(paste("customModel",customModel))
DEBUG_MASTER(paste("exePostfix",exePostfix))
    MpiArgument = get("MpiArgument",envir=.GlobalEnv)
    MpiLocal = get("MpiLocal",envir=.GlobalEnv)
    MpiNumCores = get("MpiNumCores",envir=.GlobalEnv)
    workingDir=sprintf("%s",getwd())
    newFilePath=sprintf("%s%sexNLME%s",getwd(),.Platform$file.sep,exeFileExtension)
    if ( jobType != "BOOTSTRAP" ){
        engine=1
        column_def_file=""
        data_file=""
    }
    if( nchar(extra_args_file) > 0  )
        extra_args_file=sprintf("@%s",extra_args_file)

    if ( submissionPlatform == "unix" ) {
        commandString=sprintf("./execNLMECmd.sh %s %s %s %s %s %s \"\" NLME_DIR \"\" \"/m %d /n %d  %s %s %s\" \"\" %s", runType,model_file ,workingDir,MpiArgument,MpiLocal,MpiNumCores,engine, 1 ,extra_args_file,column_def_file,data_file,exePostfix)

        cat("#!/bin/ksh",file=newFilePath,sep="\n",append=FALSE)
        cat("##set -x",file=newFilePath,sep="\n",append=TRUE)
        cat(commandString,file=newFilePath,sep="\n",append=TRUE)

        Sys.chmod(newFilePath, mode="0777", use_umask = TRUE)
        ret = system(newFilePath)
        if ( ret != 0 ) 
            return(FALSE)

    } else {
        DEBUG_MASTER(sprintf("execNLMECmd%s %s %s %s %s %s %s \"\" NLME_DIR \"\" \"/m %d /n %d  %s %s %s\" \"\" %s", exeFileExtension,runType,model_file ,gsub("/","\\\\",workingDir,fixed=TRUE),MpiArgument,MpiLocal,MpiNumCores,engine, 1 ,extra_args_file,column_def_file,data_file,exePostfix))
        commandString=sprintf("execNLMECmd%s %s %s %s %s %s %s \"\" NLME_DIR \"\" \"/m %d /n %d  %s %s %s\" \"\" %s", exeFileExtension,runType,model_file ,gsub("/","\\\\",workingDir,fixed=TRUE),MpiArgument,MpiLocal,MpiNumCores,engine, 1 ,extra_args_file,column_def_file,data_file,exePostfix)
        cat(commandString,file=newFilePath,sep="\n",append=FALSE)
        Sys.chmod(newFilePath, mode="0777", use_umask = TRUE)
        shell(newFilePath)

        if ( runType == "COMPILE" ) {
            path=Sys.getenv("PATH")
            tokens=unlist(strsplit(path,split=";"))
            for ( t in tokens ) { 
                t = gsub("//","/",shortPathName(t),fixed=TRUE)
                fullPath= file.path(t,"SignTool.exe")
                if ( file.exists(fullPath) ) {
                    installDir=Sys.getenv("INSTALLDIR")
                    if ( MpiArgument == "MPIYES" ) 
                        exe=file.path(gsub("//","/",workingDir,fixed=TRUE),paste0("mpiNLME7",exePostfix,".exe"))
                    else
                        exe=file.path(gsub("//","/",workingDir,fixed=TRUE),paste0("NLME7",exePostfix,".exe"))
                    if ( file.exists(exe) == FALSE )  {
                        print("Compile/Link Failed!")
                        return(FALSE)
                    }
                    else {
                        cmd = paste0("\"", t, "\"/SignTool.exe  sign /f " , file.path(installDir,"certara.codesign.pfx")  , " /p C3rt@r@ " , exe )
                        system(cmd)
                    }
                }
            }
        }
    }
DEBUG_MASTER("End of generateNLMEScriptAndRun() ")
    return(TRUE)
}



#
# Copy NLME input files into remote-shared-directory
#
copyFiles<-function(dirToCopyTo){
DEBUG_MASTER("func --> copyFiles()")

    localWorkingDir=get("localWorkingDir",envir=nlmeEnv)
    exeFileExtension=get("exeFileExtension",envir=nlmeEnv)

    files_to_copy=get("files_to_copy",envir=nlmeEnv)
    jobType=get("jobType",envir=nlmeEnv)
    num_samples=get("num_samples",envir=nlmeEnv)

DEBUG_MASTER(localWorkingDir)
DEBUG_MASTER(dirToCopyTo)
DEBUG_MASTER(files_to_copy)

    if ( jobType != "BOOTSTRAP" )  {
        control_lines=get("control_lines",envir=nlmeEnv)
        if ( localWorkingDir != dirToCopyTo ) {
            for ( indx  in 1:num_samples ) {
                fileToCopy = getExtraArgumentFilename(control_lines[indx])
                fileIndex = getExtraArgumentFilenameIndex(control_lines[indx])
#                if ( indx == 1 || is.na(fileIndex)    )
                if ( is.na(fileIndex) || fileIndex=="1"  || num_samples == 1   ) {
                    file.copy(sprintf("%s/%s",localWorkingDir,fileToCopy),sprintf("%s/%s",dirToCopyTo,fileToCopy),overwrite=TRUE)
                }
            }
        }
    }

    if ( localWorkingDir != dirToCopyTo ) {
        for ( f in unlist(strsplit(files_to_copy, split=" ")) ) {
            file.copy(sprintf("%s/%s",localWorkingDir,f),sprintf("%s/%s",dirToCopyTo,f),overwrite=TRUE)
        }
    }
    writeOutGlobals(sprintf("%s/myglobaldefs.r",dirToCopyTo))

    file.copy(paste(Sys.getenv("INSTALLDIR"),.Platform$file.sep,"execNLMECmd",exeFileExtension,sep=""),dirToCopyTo,overwrite=TRUE)
}


#
# Copy results of an NLME run from grid-shared-directory back to user's 
# local run directory
#

copyResults<-function(dirToCopyTo, copyFilesFlag = FALSE ){
DEBUG_MASTER(paste("func --> copyResults() ",copyFilesFlag))

    jobsDirectoryRoot=SharedWorkingDir
    assign("jobsDirectoryRoot", jobsDirectoryRoot, envir=nlmeEnv)
DEBUG_MASTER(paste("jobsDirectoryRoot",jobsDirectoryRoot))
    if ( copyFilesFlag  ) {
        tryCatch (
        {
            SharedWorkingDir=get("SharedWorkingDir",envir=nlmeEnv)
            results_file_list=get("results_file_list",envir=nlmeEnv)
            for ( p in unlist(strsplit(results_file_list, split=" ")) ) {
                expFiles=list.files(paste0(SharedWorkingDir,"/.."), pattern = p,all.files=TRUE)
                for ( f in expFiles ) {
                    file.copy(sprintf("%s/../%s",SharedWorkingDir,f),sprintf("%s/%s",dirToCopyTo,f),overwrite=TRUE)

DEBUG_MASTER(paste("file.copy(",sprintf("%s/../%s",SharedWorkingDir,f),sprintf("%s/%s",dirToCopyTo,f),"),overwrite=TRUE)"))
                }
            }
            # Grab the status files so we can report timing
            num_samples=as.integer(get("num_samples",envir=nlmeEnv))
            for ( job in 1:num_samples ) {
                f=sprintf("S_%03d.status",job)

                file.copy(sprintf("%s/%s",SharedWorkingDir,f),sprintf("%s/%s",dirToCopyTo,f),overwrite=TRUE)


DEBUG_MASTER(paste("file.copy(",sprintf("%s/%s",SharedWorkingDir,f),sprintf("%s/%s",dirToCopyTo,f),",overwrite=TRUE)"))
            }
        },
        error=function(ex){
            DEBUG_MASTER("Failed to copy results")
            print(paste("ERROR is : ",ex))
        })
DEBUG_MASTER("DONE func --> copyResults() ")
    }
}




#
# This method reconnects to a job that is already been submitted to grid
#
reconnectGenericGridJob<-function(jobType){

    loadBatchLibrary()

    num_processes=as.integer(get("num_processes",envir=nlmeEnv))
    SharedWorkingDir=get("SharedWorkingDir",envir=nlmeEnv)
    num_samples=as.integer(get("num_samples",envir=nlmeEnv))
    gridRegistry=get("gridRegistry",envir=nlmeEnv)
    assign("jobType", jobType, envir=nlmeEnv)

    jobOrder=1:num_samples 
    status=vector(mode="character",num_samples)

    numberOfChunks =  num_processes

    done = findDone(gridRegistry)

    numSuccessful = 0 
    numFailed = 0 
    while ( TRUE  ) {
        if ( IsJobCanceled() ){
            CancelProgress(num_samples, numSuccessful, numFailed )
            break
        }
        tryCatch (
        {
            expired = findExpired(gridRegistry)
        },
        error=function(ex){
            expired=c()
        })
        for ( e in expired ) {
            status[e] = "EXPIRED"
        }
        for ( d in 1:length(status) ) {
            baseIndx=d %% 100
            if ( nchar(status[d]) == 0 )  {
                statusFile=sprintf("%s/jobs/%02d/%d/.status",SharedWorkingDir,baseIndx,d)


                if ( file.exists(statusFile)) {
                    tryCatch(
                    {
                        stat = readChar(statusFile, file.info(statusFile)$size - 1 )
                        stat = gsub("\r","",stat, fixed=TRUE) 
                        if ( substr(stat,1,7) != "RUNNING" ) 
                            status[d] = stat
                    },
                        error = function(ex){
                    } )


                }
            }
        }
        numSuccessful = 0 
        numFailed = 0 
        numExpired = 0 
        jobList=c()

        for ( jobId in 1:length(status) ) {
            s = status[jobId]
            if ( s == "SUCCESS" ) {
                numSuccessful = numSuccessful + 1 
                jobList=c(jobList,jobId)
            } 

            if ( s == "FAILED" ) {
                numFailed = numFailed + 1 
            }
            if ( s == "EXPIRED" ) {
                numExpired = numExpired + 1 
            }
        }
#
# Check to see if we are to return interim results
#
        if ( InterimResultsRequested() ) {
            RemoveUserCommands()
            if ( numSuccessful > 0  ) {
            if ( jobType == "BOOTSTRAP" ) {
                localDir=get("localWorkingDir",envir=nlmeEnv)
                confidence_level=get("confidence_level",envir=nlmeEnv)
                WriteResultsStatusFile("Generating Bootstrap Results...")
                cwd=getwd()
                generateBootstrapResults( localDir, jobList, confidence_level ) 
                setwd(cwd)
                WriteResultsStatusFile("Results Generated.")
            }
            else {
                localDir=get("localWorkingDir",envir=nlmeEnv)
                control_lines=get("control_lines",envir=nlmeEnv)
                WriteResultsStatusFile("Generating Results...")
                cwd=getwd()
                generateJobResults(localDir,jobType,jobList,control_lines,TRUE)
                setwd(cwd)
                WriteResultsStatusFile("Results Generated.")
            }
        } else
                WriteResultsStatusFile("Results Generated.")
        }


    cat(paste("num_samples",num_samples, "numSuccessful:",numSuccessful,"numFailed:",numFailed,"numExpired:",numExpired),file="status.log",sep="\n",append=FALSE)
        if (  num_samples ==( numSuccessful + numFailed + numExpired ) ) {
            UpdateProgress(num_samples, numSuccessful, numFailed , numExpired )
            break
        }
        UpdateProgress(num_samples, numSuccessful, numFailed, numExpired )
#
# TODO
# Lets check  on the running time of the jobs still out there
#
        flag=Sys.getenv("NLME_JOB_TIME_LIMIT")

        if ( flag != "" ) {
            cat(paste("NLME_JOB_TIME_LIMIT:",flag),file="status.log",sep="\n",append=TRUE)
            for ( jobId in 1:length(status) ) {
                s = status[jobId]
                if ( nchar(status[jobId]) == 0 )  {
                    statusFile=sprintf("%s/S_%03d.status",SharedWorkingDir,jobId)
                    if ( file.exists(statusFile)) {
                        tryCatch(
                        {
                            lines = readLines(statusFile)
                        },
                            error = function(ex){
                        } )
                        if ( length(lines) == 2 ) {
                            if ( lines[1] == "RUNNING" ) {
                                startTime = as.numeric(as.POSIXlt(lines[2]))
                                now = as.numeric(Sys.time())
                                lapse = now - startTime 
cat(paste("startTime",startTime,"now",now,"lapse",lapse),file="status.log",sep="\n",append=TRUE)
                                if ( lapse > ( 60 * as.numeric(flag) ) ){
#cat(paste("STOP job",jobId),file="status.log",sep="\n",append=TRUE)

baseIndx=jobId %% 100
stopFile=sprintf("%s/jobs/%02d/%d/stop.txt",SharedWorkingDir,baseIndx,jobId)

#cat(stopFile,file="status.log",sep="\n",append=TRUE)

#    if (file.exists(sprintf("%s/jobs/%02d/%d",SharedWorkingDir,baseIndx,jobId)))
#        cat("STOP",file=stopFile,sep="\n",append=FALSE)
                                }
                            }
                        }
                    }
                }
            }
        }

        Sys.sleep(1)
        tryCatch(
        {
            done = findDone(gridRegistry)
        },
        error = function(ex){
            Sys.sleep(1)
            done = findDone(gridRegistry)
        } )
        if ( IsJobCanceled() ){
#####            CancelProgress(num_samples, numSuccessful, numFailed )
            break
        }
    }
    return(TRUE)
}



killAGridJob <-function(gridRegistry){
DEBUG_MASTER("killAGridJob()")
tryCatch (
{
    if ( UsingBatchTools == TRUE )  {
        notDone = findNotDone(reg=gridRegistry)
        if ( nrow(notDone) > 0 ) { 
            notDone=notDone$job.id
            killJobs(reg=gridRegistry,ids=notDone)
        notDone = findNotDone(reg=gridRegistry)
        }
    }
    else {
        notDone = findNotDone(gridRegistry)
        killJobs(gridRegistry,notDone)
    }
},
error=function(ex){
return("" )
})
}


grabDoneReplicates <-function(status){
    done=c()
    for ( jobId in 1:length(status) ) {
        s = status[jobId]
        if ( s == "SUCCESS" || status == "FAILED" ) 
            done = c(done,jobId)
    }
    return(done)
}

startGenericGridJob<-function(jobType,allowIntermediateResults,progressStage="",reportProgress=FALSE){
DEBUG_MASTER("func --> startGenericGridJob()")

    FakeInitialEstimatesRun = FALSE
    InitialEstimatesRunFaked = FALSE

    loadBatchLibrary()

#
# Figure out working env
#
    num_processes=as.integer(get("num_processes",envir=nlmeEnv))
    MpiNumCores=get("MpiNumCores",envir=nlmeEnv)
    parallelMethod=get("parallelMethod",envir=nlmeEnv)
    SharedWorkingDir=get("SharedWorkingDir",envir=nlmeEnv)
    num_samples=as.integer(get("num_samples",envir=nlmeEnv))
    baseJobDirectory=SharedWorkingDir
    MpiNumCores=get("MpiNumCores",envir=.GlobalEnv)

#
# BatchJobs
#
    # create a registry
    if ( UsingBatchTools == TRUE )  {
        baseJobDirectory=getBatchDirectoryLocation(SharedWorkingDir)
        registryDir=getBatchDirectoryLocation(SharedWorkingDir)
        gridRegistry <- makeRegistry(file.dir=registryDir, work.dir=SharedWorkingDir)
        localWorkingDir=get("localWorkingDir",envir=nlmeEnv)
        if ( length(grep("torque",tolower(parallelMethod))) > 0 )  
            if ( exists("makeClusterFunctionsTorque") )
                gridRegistry$cluster.functions=makeClusterFunctionsTorque(template=sprintf("%s/batchtools.torque.tmpl",localWorkingDir))
            else
                gridRegistry$cluster.functions=makeClusterFunctionsTORQUE(template=sprintf("%s/batchtools.torque.tmpl",localWorkingDir))
        else {
            if ( length(grep("sge",tolower(parallelMethod))) > 0 )  
                gridRegistry$cluster.functions=makeClusterFunctionsSGE(template=sprintf("%s/batchtools.sge.tmpl",localWorkingDir))
            else {
DEBUG_MASTER("makeClusterFunctionsLSF")
                gridRegistry$cluster.functions=makeClusterFunctionsLSF(template=sprintf("%s/batchtools.lsf.tmpl",localWorkingDir))
            }
        }
    } else
        gridRegistry <- makeRegistry(file.dir=SharedWorkingDir,work.dir=SharedWorkingDir)
    assign("gridRegistry", gridRegistry, envir=nlmeEnv)
#
# Job information
#
    assign("jobType", jobType, envir=nlmeEnv)
    jobOrder=1:num_samples 
    status=vector(mode="character",num_samples)

#
# Copy files to the remote directory
#
    copyFiles(SharedWorkingDir)

    currWorkDir=getwd()
    setwd(SharedWorkingDir)

#
# Figure out initial estimates run
#
    if ( jobType == "BOOTSTRAP" ) {
        DoInitialNlmeRun=TRUE
        flag=Sys.getenv("NLME_SKIP_INITIAL_BOOTSTRAP_RUN")
        if ( flag == "TRUE" ) {
            DoInitialNlmeRun = FALSE
            FakeInitialEstimatesRun = TRUE
        }
        start_seed=as.integer(get("start_seed",envir=nlmeEnv))
        seeds=1:num_samples 
        for ( i in 1:num_samples ) {
            seeds[i] = start_seed + ( i - 1 ) * 100
        }
    }
    else {
        DoInitialNlmeRun=FALSE
        FakeInitialEstimatesRun = FALSE
    }
DEBUG_MASTER(paste("DoInitialNlmeRun:",DoInitialNlmeRun))
    if ( DoInitialNlmeRun ) {
memory_usage(.GlobalEnv)
memory_usage(nlmeEnv)
        stat=runNLMEInitialRun()
memory_usage(.GlobalEnv)
memory_usage(nlmeEnv)
        if ( stat == FALSE ) 
            return(list(stat=FALSE,done=c()))
        generateInitialEstimatesFiles()

        lines=readLines(extra_args_file)
        
        cat(" /d3 cols3.txt data3.txt",file=extra_args_file,sep="\n",append=FALSE)
        for ( l in lines ) {
            cat(l,file=extra_args_file,sep="\n",append=TRUE)
        }
        files_to_copy=get("files_to_copy",envir=nlmeEnv)
        files_to_copy=paste(files_to_copy, " cols3.txt data3.txt")

        assign("files_to_copy", files_to_copy , envir=nlmeEnv)
        writeOutGlobals(sprintf("%s/myglobaldefs.r",SharedWorkingDir))
    }
    else {
        ResetMPIFlags(SharedWorkingDir,MpiNumCores)
        ret = compileAndLinkNLME()
        if ( ret == FALSE ) 
            return(list(stat=FALSE,done=c()))
    }

    setwd(currWorkDir)
    if ( jobType == "BOOTSTRAP" ) {
        functionToRun="runNLMEBootstrapSample"
        if ( UsingBatchTools == TRUE )  {
            ids  <- batchMap(reg=gridRegistry, fun=runNLMEBootstrapSample, indx=jobOrder ,seed =seeds )
        } else
            ids  <- batchMap(gridRegistry, runNLMEBootstrapSample, jobOrder ,seeds,use.names=TRUE)
    } else {
        functionToRun="runNLMEGenericSample"
        if ( UsingBatchTools == TRUE )  {
            ids  <- batchMap(reg=gridRegistry, fun=runNLMEGenericSample, jobIndx=jobOrder )
        } else
            ids  <- batchMap(gridRegistry, runNLMEGenericSample, jobOrder ,use.names=TRUE)
    }


    ResetMPIFlags(SharedWorkingDir,MpiNumCores)

    num_processes = as.integer(num_processes / MpiNumCores)

    numberOfChunks =  num_processes
    if ( num_samples <= num_processes ) {
        if ( UsingBatchTools == TRUE )  
            done <- submitJobs(reg=gridRegistry, resources = list(nodes = MpiNumCores, walltime="18:00:00"))
        else
            done <- submitJobs(gridRegistry, resources = list(nodes = MpiNumCores, walltime="18:00:00"))
    }
    else {
        if ( UsingBatchTools == TRUE )   {
DEBUG_MASTER(paste("numberOfChunks:",numberOfChunks))
            chunked = chunkIds(reg=gridRegistry,ids=findJobs(reg=gridRegistry),n.chunks=numberOfChunks)
            done <- submitJobs(reg=gridRegistry, chunked, resources = list(nodes = MpiNumCores, walltime="18:00:00") )
        } else {
            chunked = chunk(getJobIds(gridRegistry),n.chunks=numberOfChunks,shuffle=TRUE)
            done <- submitJobs(gridRegistry, chunked, resources = list(nodes = MpiNumCores, walltime="18:00:00") )
        }

    }
    if ( UsingBatchTools == TRUE )    {
        done = findDone(ids=NULL, gridRegistry)
        errors = findErrors(ids=NULL, gridRegistry)
        if ( nrow(done) > 0 ) 
            done=done$job.id
        else
            done=c()
        if ( nrow(errors) > 0 ) 
            errors=errors$job.id
        else
            errors=c()
    } else {
        done = findDone(gridRegistry)
        errors = findErrors(gridRegistry)
    }
    numSuccessful = 0 
    numFailed = 0 
    numErrors = 0 

    allJobsAreDone = FALSE
    while ( allJobsAreDone == FALSE  ) {
        jobInProgressPath=""
        if ( IsJobCanceled() ){
            CancelProgress(num_samples, numSuccessful, numFailed )
            killAGridJob(gridRegistry)
            allJobsAreDone = TRUE
            break
        }
        if ( IsEarlyTerminationRequested() ) {
            if ( num_samples == 1 ) { # drop a file to force NLME to quit
                jobId = 1 
                requestStopEarly(sharedWorkingDir,jobId)
            }
            else { 
                done=grabDoneReplicates(status)
                break
            }
        }
        tryCatch (
        {
            baseJobsDirectory=SharedWorkingDir
            if ( UsingBatchTools == TRUE )   {
                baseJobsDirectory=getBatchDirectoryLocation(SharedWorkingDir)
                running = findRunning(ids=NULL, reg=gridRegistry)
                if ( nrow(running) > 0  )
                    running=running$job.id
                else
                    running=c()
                errors = findErrors(ids=NULL, reg=gridRegistry)
                if ( nrow(errors) > 0  )
                    errors=errors$job.id
                else
                    errors=c()
            } else {
                running = findRunning(gridRegistry)
                errors = findErrors(gridRegistry)
            }
            if ( length(running) >= 1 ) {
                d=running[1]
                baseIndx=d %% 100
                jobInProgressPath=sprintf("%s/registry/jobs/%02d/%d/",SharedWorkingDir,baseIndx,d)
           }
        },
        error=function(ex){
            jobInProgressPath=""
        })
#
# Ignore expired jobs
#
        expired=c()
#        tryCatch (
#        {
#            if ( UsingBatchTools == TRUE )    {
#                expired = findExpired(ids=NULL,reg=gridRegistry)
#                if ( nrow(expired) > 0  )
#                    expired=expired$job.id
#                else
#                    expired=c()
#            } else
#                expired = findExpired(gridRegistry)
#        },
#        error=function(ex){
#            expired=c()
#        })

        for ( e in expired ) {
            status[e] = "EXPIRED"
        }
        for ( e in errors ) {
            status[e] = "ERROR"
        }
        for ( d in 1:length(status) ) {
            baseIndx=d %% 100
            if ( nchar(status[d]) == 0 )  {
                statusFile=sprintf("%s/jobs/%02d/%d/.status",getBatchDirectoryLocation(SharedWorkingDir),baseIndx,d)
                if ( file.exists(statusFile)) {
                    tryCatch(
                    {
                        stat = readChar(statusFile, file.info(statusFile)$size - 1 )
                       stat = gsub("\r","",stat, fixed=TRUE) 
                       if ( substr(stat,1,7) != "RUNNING" ) {
                           status[d] = stat
                       }
                    },
                        error = function(ex){
                    } )
                }
            }
        }
        numSuccessful = 0 
        numFailed = 0 
        numExpired = 0 
        numErrors = 0 
        jobList=c()

        for ( jobId in 1:length(status) ) {
            s = status[jobId]
            if ( s == "SUCCESS" ) {
                numSuccessful = numSuccessful + 1 
                jobList=c(jobList,jobId)
                if ( FakeInitialEstimatesRun && !InitialEstimatesRunFaked ) {
                    fakeNLMEInitialRun(jobId)
                    InitialEstimatesRunFaked = TRUE
                }
            } 

            if ( s == "FAILED" ) {
                numFailed = numFailed + 1 
            }
            if ( s == "EXPIRED" ) {
                numExpired = numExpired + 1 
            }
            if ( s == "ERROR" ) {
                numErrors = numErrors + 1 
            }
        }
memory_usage(.GlobalEnv)
memory_usage(nlmeEnv)
gc()
memory_usage(.GlobalEnv)
memory_usage(nlmeEnv)

#
# Check to see if we are to return interim results
#
        if ( InterimResultsRequested() ) {
            RemoveUserCommands()
            if ( numSuccessful > 0  || jobType == "STEPWISE_SEARCH") {
            if ( allowIntermediateResults == TRUE ) 
            {
            if ( jobType == "BOOTSTRAP" ) {
                localDir=get("localWorkingDir",envir=nlmeEnv)
                confidence_level=get("confidence_level",envir=nlmeEnv)
                WriteResultsStatusFile("Generating Bootstrap Results...")
                cwd=getwd()
                generateBootstrapResults( localDir, jobList, confidence_level ) 
                setwd(cwd)
                WriteResultsStatusFile("Results Generated.")
            }
            else {
                localDir=get("localWorkingDir",envir=nlmeEnv)
                control_lines=get("control_lines",envir=nlmeEnv)
                WriteResultsStatusFile("Generating Results...")
                cwd=getwd()
                generateJobResults(localDir,jobType,jobList,control_lines,TRUE)
                setwd(cwd)
                WriteResultsStatusFile("Results Generated.")
            }
            } else 
                WriteResultsStatusFile("Results Generated.")
        } else
                WriteResultsStatusFile("Results Generated.")
        }

        cat(paste("num_samples",num_samples, "numSuccessful:",numSuccessful,"numFailed:",numFailed,"numExpired:",numExpired,"numErrors:",numErrors),file="status.log",sep="\n",append=FALSE)
        if (  num_samples ==( numSuccessful + numFailed + numExpired + numErrors ) ) {
            UpdateProgress(num_samples, numSuccessful, numFailed , numExpired ,progressStage=progressStage,numErrors=numErrors)
            allJobsAreDone = TRUE
            break
        }
        UpdateProgress(num_samples, numSuccessful, numFailed, numExpired , currentJobDirectory = jobInProgressPath,progressStage=progressStage,numErrors=numErrors)
#
# TODO
# Lets check  on the running time of the jobs still out there
#
        flag=Sys.getenv("NLME_JOB_TIME_LIMIT")

        if ( flag != "" ) {
            cat(paste("NLME_JOB_TIME_LIMIT:",flag),file="status.log",sep="\n",append=TRUE)
            for ( jobId in 1:length(status) ) {
                s = status[jobId]
                if ( nchar(status[jobId]) == 0 )  {
                    statusFile=sprintf("%s/S_%03d.status",SharedWorkingDir,jobId)
                    if ( file.exists(statusFile)) {
                        tryCatch(
                        {
                            lines = readLines(statusFile)
                        },
                            error = function(ex){
                        } )
                        if ( length(lines) == 2 ) {
                            if ( lines[1] == "RUNNING" ) {
                                startTime = as.numeric(as.POSIXlt(lines[2]))
                                now = as.numeric(Sys.time())
                                lapse = now - startTime 
cat(paste("startTime",startTime,"now",now,"lapse",lapse),file="status.log",sep="\n",append=TRUE)
                                if ( lapse > ( 60 * as.numeric(flag) ) ){
cat(paste("STOP job",jobId),file="status.log",sep="\n",append=TRUE)

                requestStopEarly(sharedWorkingDir,jobId)
                                }
                            }
                        }
                    }
                }
            }
        }

        Sys.sleep(1)
        tryCatch(
        {
            if ( UsingBatchTools == TRUE )    {
                done = findDone(ids=NULL, reg=gridRegistry)
                if ( nrow(done) > 0 ) 
                    done=done$job.id
                else
                    done=c()
                errors = findErrors(ids=NULL, gridRegistry)
                if ( nrow(errors) > 0 ) 
                    errors=errors$job.id
                else
                    errors=c()
            } else {
                done = findDone(gridRegistry)
                errors = findErrors(gridRegistry)
            }
        },
        error = function(ex){
            Sys.sleep(1)
            if ( UsingBatchTools == TRUE )    {
                done = findDone(ids=NULL, reg=gridRegistry)
                if ( nrow(done) > 0 ) 
                    done=done$job.id
                else
                    done=c()
                errors = findErrors(ids=NULL, gridRegistry)
                if ( nrow(errors) > 0 ) 
                    errors=errors$job.id
                else
                    errors=c()
            } else {
                done = findDone(gridRegistry)
                errors = findErrors(gridRegistry)
            }
        } )
        if ( IsJobCanceled() ){
            CancelProgress(num_samples, numSuccessful, numFailed )
            allJobsAreDone=TRUE
            break
        }
    if ( UsingBatchTools == TRUE )    {
        done = findDone(ids=NULL, gridRegistry)
        if ( nrow(done) > 0 ) 
            done=done$job.id
        else
            done=c()
    }
    else
        done = findDone(gridRegistry)
        if ( reportProgress == TRUE )  {
            reportCurrentStatus(num_samples, numSuccessful, numFailed)
        }
    }
DEBUG_MASTER(paste("Num_Samples ",num_samples,"numErrors",numErrors,"numFailed",numFailed))
    jobs = getJobTable(reg=gridRegistry)
    assign("gridJobs", jobs$batch.id, envir=nlmeEnv)
    if ( IsJobCanceled() )
        return(list(stat=FALSE,done=c()))
    if ( ( numErrors+ numFailed ) == num_samples ) 
        return(list(stat=FALSE,done=jobList))
    else
        return(list(stat=TRUE,done=jobList))
}




#
# Create the jobs directory
#
createJobsDirectory <- function(SharedWorkingDir,num_samples) {

#    dir.create(sprintf("%s/jobs",SharedWorkingDir))
    mkdir(sprintf("%s/jobs",SharedWorkingDir))
    for ( indx in 1:num_samples ) {
        baseIndx=indx %% 100
        workingDir1=sprintf("%s/jobs/%02d/",SharedWorkingDir,baseIndx)
        workingDir=sprintf("%s/jobs/%02d/%d/",SharedWorkingDir,baseIndx,indx)
#        dir.create(workingDir1)
#        dir.create(workingDir)
        mkdir(workingDir1)
        mkdir(workingDir)
    }

}

MPIRunOneNLMESample <- function() {

DEBUG_MASTER(sprintf("MPIRunOneNLMESample: %d",mpi.comm.rank()))
DEBUG_SLAVE(sprintf("MPIRunOneNLMESample: %d",mpi.comm.rank()))
DEBUG_SLAVE(paste("Slave's cwd is ",getwd()))
DEBUG_SLAVE(paste("Slave's cwd is ",getwd()))
    emptyTask <- 0 
    require(XML)
    done <- 0 
    while (done != 1) {
        # Signal being ready to receive a new task 
DEBUG_SLAVE("WAITING:")
        mpi.send.Robj(emptyTask,0,SLAVE_IS_READY_MSG ) 

        # Receive a task 
        task <- mpi.recv.Robj(mpi.any.source(),mpi.any.tag()) 
        task_info <- mpi.get.sourcetag() 
        tag <- task_info[2] 
DEBUG_SLAVE(paste("<<<<<<<<<<<< " , tag , " >>>>>>>>>>>"))
        if (tag == MASTER_REMOVE_TEMP_MSG ) {
DEBUG_SLAVE("Message : MASTER_REMOVE_TEMP_MSG")

        } else if (tag == MASTER_CREATE_TEMP_MSG ) {
		
DEBUG_SLAVE("Message : MASTER_CREATE_TEMP_MSG")
        } else if (tag == MASTER_PROCESS_JOB_MSG ) {
DEBUG_SLAVE("Message : MASTER_PROCESS_JOB_MSG")
            if ( mpi.comm.rank() == 1 ){
                # wait a couple of minutes and then signal ready again.
                Sys.sleep(HEARTBEAT_SLEEP_TIME)
                mpi.send.Robj(emptyTask,0,SLAVE_HEARTBEAT_MSG ) 
            } else {
                Job = task$Job
                JobType = task$JobType
DEBUG_SLAVE(paste("Message : jobType",jobType))
                Seed = task$Seed
DEBUG_SLAVE(paste("Message : Seed",Seed))
                SharedWorkingDir = task$SharedWorkingDir
DEBUG_SLAVE(paste("Message : SharedWorkingDir",SharedWorkingDir))
DEBUG_SLAVE("-----------------------------------------------")
                # 
                # Run the job and return the results
                #
                # 1. Need to make sure we are in the shared drive job directory
                # 2. Run th job
                # 3. Check the status file
                if ( jobType == "BOOTSTRAP" ) {
DEBUG_SLAVE("-----------------------------------------------")
DEBUG_SLAVE(getwd())
for ( i in 1:5 ) {
DEBUG_SLAVE(paste("Try : ",i))
flag=file.exists(SharedWorkingDir)
DEBUG_SLAVE(paste("Slave check working dir",flag))
if ( flag == TRUE )  {
    i = 9999
    break
}
Sys.sleep(HEARTBEAT_SLEEP_TIME)
}
                    setwd(SharedWorkingDir)
DEBUG_SLAVE("Slave after setwd()")
DEBUG_SLAVE(paste("Message : runNLMEBootstrapSample()",Job,Seed))
                    runNLMEBootstrapSample(Job,Seed)
                }
                else {
                    setwd(SharedWorkingDir)
DEBUG_SLAVE(paste("Message : runNLMEGenericSample()",Job))
                    runNLMEGenericSample(Job)
                }
DEBUG_SLAVE("Message : checkJobStatus()")
                status = checkJobStatus(SharedWorkingDir,Job)
DEBUG_SLAVE("AFTER : Message : checkJobStatus()")
#               status=TRUE
DEBUG_SLAVE(sprintf("job status %d",status))
                results <- list(Success=status,Job=Job)
                mpi.send.Robj(results,0,SLAVE_HAS_RESULTS_MSG  )
            }
        } else if (tag == MASTER_PROCESS_QUANTILES_JOB_MSG ) {
DEBUG_SLAVE("Message : MASTER_PROCESS_QUANTILES_JOB_MSG")
            if ( mpi.comm.rank() == 1 ){
                # wait a couple of minutes and then signal ready again.
                Sys.sleep(HEARTBEAT_SLEEP_TIME)
                mpi.send.Robj(emptyTask,0,SLAVE_HEARTBEAT_MSG ) 
            } else {
                results = 0 
                Sys.sleep(10)
                   mpi.send.Robj(results,0,SLAVE_HAS_RESULTS_MSG  )
            }
        } else if (tag == MASTER_CLOSE_MSG ) {
DEBUG_SLAVE("Message : MASTER_CLOSE_MSG")
                results = 0 
                mpi.send.Robj(results,0,SLAVE_CLOSING_MSG  )
        } else if (tag == MASTER_SHUTDOWN_MSG ) {
DEBUG_SLAVE(paste("MASTER_SHUTDOWN_MSG rank ",mpi.comm.rank()))
            # The server is done with us, we will break out of the infinite
            # loop and exit
            done <- 1
        } else if (tag == MASTER_PING_MSG ) {
DEBUG_SLAVE(paste("MASTER_PING_MSG ",mpi.comm.rank()))

        }
        # We'll just ignore any unknown messages
    }
DEBUG_SLAVE(paste("SLAVE END Rank d",mpi.comm.rank()))

}




startAndMonitorMPIJobs <- function( jobType, numCoresToUse, num_samples,  seeds, allowIntermediateResults,reportProgress=FALSE){ 

DEBUG_MASTER("startAndMonitorMPIJobs")
    library(Rmpi)

    statusArray=vector(mode="character",num_samples)

    machinesProcessed=c()
    mpi.bcast.Robj2slave(FailProgress)
    mpi.bcast.Robj2slave(MPIRunOneJob)
    mpi.bcast.Robj2slave(MPIRunOneNLMESample)
    mpi.bcast.Robj2slave(mkdir)
    mpi.bcast.Robj2slave(runNLMESample)
    mpi.bcast.Robj2slave(runNLMEGenericSample)
    mpi.bcast.Robj2slave(runNLMEBootstrapSample)
    mpi.bcast.Robj2slave(checkJobStatus)
    mpi.bcast.Robj2slave(getExtraArgumentFilename)
    mpi.bcast.Robj2slave(getRunSuccessFilename)
    mpi.bcast.Robj2slave(getExtraArgumentFilenameIndex)
    mpi.bcast.Robj2slave(WriteResultsStatusFile)
    mpi.bcast.Robj2slave(InterimResultsRequested)
    mpi.bcast.Robj2slave(IsEarlyTerminationRequested)
    mpi.bcast.Robj2slave(RemoveUserCommands)
    mpi.bcast.Robj2slave(generateBootstrapResults)
    mpi.bcast.Robj2slave(generateJobResults)
    mpi.bcast.Robj2slave(listJobErrors)
    mpi.bcast.Robj2slave(collectJobErrors)
    mpi.bcast.Robj2slave(collectJobErrorLogs)
    mpi.bcast.Robj2slave(getLocalTimeUTC)

    mpi.bcast.Robj2slave(DEBUG_SLAVE)
    mpi.bcast.Robj2slave(DEBUG_MASTER)
    mpi.bcast.Robj2slave(HEARTBEAT_SLEEP_TIME)
    mpi.bcast.Robj2slave(SLAVE_IS_READY_MSG)
    mpi.bcast.Robj2slave(SLAVE_HAS_RESULTS_MSG)
    mpi.bcast.Robj2slave(SLAVE_CLOSING_MSG)
    mpi.bcast.Robj2slave(SLAVE_HEARTBEAT_MSG)
    mpi.bcast.Robj2slave(MASTER_PROCESS_JOB_MSG)
    mpi.bcast.Robj2slave(MASTER_CLOSE_MSG)
    mpi.bcast.Robj2slave(MASTER_SHUTDOWN_MSG)
    mpi.bcast.Robj2slave(MASTER_CREATE_TEMP_MSG)
    mpi.bcast.Robj2slave(MASTER_REMOVE_TEMP_MSG)
    mpi.bcast.Robj2slave(MASTER_PROCESS_QUANTILES_JOB_MSG)
    mpi.bcast.Robj2slave(MASTER_PING_MSG)

    mpi.bcast.Robj2slave(SubmissionPlatform)
    mpi.bcast.Robj2slave(jobType)
    mpi.bcast.Robj2slave(num_samples)
    mpi.bcast.Robj2slave(parallelMethod)
    mpi.bcast.Robj2slave(num_processes)
    mpi.bcast.Robj2slave(localWorkingDir)
    mpi.bcast.Robj2slave(SharedWorkingDir)
    mpi.bcast.Robj2slave(files_to_copy)
    mpi.bcast.Robj2slave(model_file)
    mpi.bcast.Robj2slave(extra_args_file)
    mpi.bcast.Robj2slave(jobType)

    mpi.bcast.Robj2slave(MpiExecutable)
    mpi.bcast.Robj2slave(MpiArgument)
    mpi.bcast.Robj2slave(MpiNumCores)
    mpi.bcast.Robj2slave(MpiLocal)
    mpi.bcast.Robj2slave(GlobalSummaryLine1)
    mpi.bcast.Robj2slave(GlobalSummaryLine2)
    mpi.bcast.Robj2slave(GlobalSummaryLine3)

    if ( jobType == "BOOTSTRAP" )  {
        mpi.bcast.Robj2slave(engine)
        mpi.bcast.Robj2slave(num_iterations)
        mpi.bcast.Robj2slave(max_tries)
        mpi.bcast.Robj2slave(start_seed)
        mpi.bcast.Robj2slave(column_def_file)
        mpi.bcast.Robj2slave(data_file)
        mpi.bcast.Robj2slave(confidence_level)
        mpi.bcast.Robj2slave(seeds)

    }
    else { 
        mpi.bcast.Robj2slave(control_file)
        mpi.bcast.Robj2slave(control_lines)
    }
    emptyTask <- 0 
    closed_slaves = 0 
    CurrentJob <- 1
    numJobs = num_samples
    numJobsFinished = 0
    numJobsFailed = 0

    n_slaves <- mpi.comm.size()-1
    slaveCloseList=list(dim=n_slaves)
    for ( s in (1:n_slaves)){
        slaveCloseList[s] = FALSE
    }
DEBUG_MASTER("Start all processes")
    # Call the function in all the slaves to get them ready to
    # undertake task
    if ( TRUE ){
        mpi.bcast.cmd(MPIRunOneNLMESample())
    } else {
        for ( s in (1:n_slaves)){
                mpi.send.Robj(emptyTask , s , MASTER_PING_MSG  )
            }
    }

DEBUG_MASTER("Start MPI loop")

    while (closed_slaves < n_slaves) { 

DEBUG_MASTER(paste("n_slaves",n_slaves,"closed_slaves",closed_slaves))

        # While there are more slaves that we started alive, keep waiting
        # for a message from them.
        message <- mpi.recv.Robj(mpi.any.source(),mpi.any.tag()) 
        message_info <- mpi.get.sourcetag() 
        slave_id <- message_info[1] 
        tag <- message_info[2] 
DEBUG_MASTER(paste("Received message from slave ",slave_id,tag))
	if ( IsJobCanceled() || IsEarlyTerminationRequested() ) {
            for ( s in (1:n_slaves) ){
                if ( slaveCloseList[s] != TRUE ){
                    mpi.send.Robj(task , s, MASTER_CLOSE_MSG )
                    slaveCloseList[s] = TRUE
                }
            }
            break
#            res=list(Status='Canceled')
#            return(res)
        }
        if (tag == SLAVE_IS_READY_MSG ) { 
            # slave is ready for a task. Give it the next task, or tell it tasks
            # are done if there are none. 

            if ( CurrentJob <= numJobs ) {
                if ( slave_id == 1 ) {
                    # Reserve the 1st slave for the heartbeat
                    mpi.send.Robj(emptyTask , slave_id , MASTER_PROCESS_JOB_MSG)
                }
                else {
    
 
                if ( slave_id == -1 ){
                    machineName = "Local"
                } else {
                    machineName = AllHostNames[slave_id]
#                #
                task <- list(Job= CurrentJob, JobType=jobType, Seed = seeds[CurrentJob], SharedWorkingDir=SharedWorkingDir )
                
                CurrentJob = CurrentJob + 1 
                mpi.send.Robj(task , slave_id, MASTER_PROCESS_JOB_MSG )
                }
                }
            }
	    else {
                if ( slaveCloseList[slave_id] != TRUE ){
                    mpi.send.Robj(task , slave_id, MASTER_CLOSE_MSG )
                    slaveCloseList[slave_id] = TRUE
                }
            }
        } else if (tag == SLAVE_HAS_RESULTS_MSG ) { 
            #The message contains results. The slave finished with one replicate
            #and we will now stove away the results.
            # TODO
            # For now(for the sake of simplicity and performance),
            # I am keeping them all in memory but might want to flush to disk
            # when there are large number of replicates to hold
            #
            rep = message$Job
            if ( message$Success == TRUE ){         
                numJobsFinished = numJobsFinished + 1 
                statusArray[rep] = "SUCCESS"
            } else {
                numJobsFailed = numJobsFailed + 1 
                statusArray[rep] = "FAILED"
	   }
            UpdateProgress(num_samples, numJobsFinished, numJobsFailed , 0 )
        } 
        else if (tag == SLAVE_CLOSING_MSG ) { 
            # A slave has closed down. 
            closed_slaves <- closed_slaves + 1 
        } 
        else if (tag == SLAVE_HEARTBEAT_MSG ) { 
        # Heartbeat function
        # Look thru all the files that have been marked as started
        #   Look at the time stamp
        #   If it is greater than 10 X the longest running job
        #   then 
        #       Assume that it is a lost cause
        #   endif
        # 
        }
    }
    MPICloseSlaves()
    mpi.close.Rslaves()
    if ( IsJobCanceled() ){
        CancelProgress(num_samples, numSuccessful, numFailed )
        retStatus=FALSE
        done=c()
    }
    else {
        retStatus = TRUE
        done = grabDoneReplicates(statusArray)
    }
DEBUG_MASTER("DONE startAndMonitorMPIJobs")
    return(list(stat=retStatus,done=done))
}

  
startAndMonitorMulticoreJobs <- function( numCoresToUse, num_samples,  seeds, allowIntermediateResults,reportProgress=FALSE) { 
  
    statusArray=vector(mode="character",num_samples)
    JobId = 1
    NextJobToRun=1
    NumCoresToUse = min( c(numCoresToUse, num_samples ) )
    for ( job in (1: NumCoresToUse) ) {
        if ( jobType == "BOOTSTRAP" ) 
            runNLMEBootstrapSample(job,seeds[job]) 
        else 
            runNLMEGenericSample(job)
    }
    NextJobToRun = NextJobToRun + NumCoresToUse

    done = 0 
    numSuccessful = 0 
    numFailed = 0 
    while ( done != 1   ) {
        if ( IsJobCanceled() ){
            CancelProgress(num_samples, numSuccessful, numFailed )
            done=1
            break
        }
        if ( IsEarlyTerminationRequested() ){
            done=1
            break
        }
        if ( done == 1 ) 
            break
        jobId = 0 
        files=list.files(path = getwd(), pattern = "^S.*\\.status$",all.files=TRUE)
        currentRunningJobDir=""

        for ( f in files ) {
                jobId=as.integer(tail(unlist(strsplit(sub(".status","",f),"_")),1))
                if ( ! is.na(jobId) && ( jobId != 0 ) ) {
                baseIndx=jobId %% 100
                if ( nchar(statusArray[jobId]) == 0 )  {
    
                    for ( t in 1:10 ) {
                    tryCatch(
                    {
                    stat = readChar(f, file.info(f)$size - 1 )
                    if ( substr(stat,1,7) == "SUCCESS" || substr(stat,1,6) == "FAILED" ) 
                    {
                        statusFile=sprintf("%s/jobs/%02d/%d/.status",SharedWorkingDir,baseIndx,jobId)
                        stat = readChar(statusFile, file.info(statusFile)$size - 1 )
                        statusArray[jobId] = gsub("\r","",stat, fixed=TRUE) 
                        if ( NextJobToRun <= num_samples ) {
                            if ( jobType == "BOOTSTRAP" ) 
                                runNLMEBootstrapSample(NextJobToRun,seeds[NextJobToRun]) 
                            else  {

                                runNLMEGenericSample(NextJobToRun)
                            }
                            NextJobToRun = NextJobToRun + 1 
                        }
                    }
                    else 
                        if ( substr(stat,1,7) == "RUNNING")
                            currentRunningJobDir=sprintf("%s/jobs/%02d/%d/",SharedWorkingDir,baseIndx,jobId)
                    t=99
                    break
                    },
                    error = function(ex){
                   
                    statusArray[jobId] = ""
                        Sys.sleep(1)
                    } )
                }
              }
          }
        }
        numSuccessful = 0 
        numFailed = 0 
        jobList = c()
        for ( jobId in 1:length(statusArray) ) {
         
            s = statusArray[jobId] 
            if ( substr(s,1,7) == "SUCCESS" ) {
                numSuccessful = numSuccessful + 1 
                jobList = c(jobList ,jobId )
            } 

            if ( substr(s,1,6) == "FAILED" ) {
                numFailed = numFailed + 1 
            }
        }
#
# Check to see if we are to return interim results
#
        if ( InterimResultsRequested() ) {
            RemoveUserCommands()
            if ( numSuccessful > 0 ){
            if ( allowIntermediateResults == TRUE ) {
            if ( jobType == "BOOTSTRAP" ) {
                localDir=get("localWorkingDir",envir=nlmeEnv)
                confidence_level=get("confidence_level",envir=nlmeEnv)
                WriteResultsStatusFile("Generating Results...")
                cwd=getwd()
                generateBootstrapResults( localDir, jobList, confidence_level ) 
                setwd(cwd)
                WriteResultsStatusFile("Results Generated.")
            }
            else {
                localDir=get("localWorkingDir",envir=nlmeEnv)
                control_lines=get("control_lines",envir=nlmeEnv)
                WriteResultsStatusFile("Generating Results...")
                cwd=getwd()
                generateJobResults(localDir,jobType,jobList,control_lines,TRUE)
                setwd(cwd)
                WriteResultsStatusFile("Results Generated.")
            }
            } else 
                WriteResultsStatusFile("Results Generated.")
        } else
                WriteResultsStatusFile("Results Generated.")
        }
        UpdateProgress(num_samples, numSuccessful, numFailed , 0 , currentJobDirectory=currentRunningJobDir)
        if (  num_samples ==( numSuccessful + numFailed ) ) {
            UpdateProgress(num_samples, numSuccessful, numFailed , 0 )
            break
        }
        else {
        }
        if ( reportProgress == TRUE )  {
            reportCurrentStatus(num_samples, numSuccessful, numFailed)
        }
    }
    UpdateProgress(num_samples, numSuccessful, numFailed , 0 )
    Sys.sleep(1)
    if ( IsJobCanceled() ){
        CancelProgress(num_samples, numSuccessful, numFailed )
        retStatus=FALSE
        done=c()
    }
    else {
        retStatus = TRUE
        done = grabDoneReplicates(statusArray)
    }
    return(list(stat=retStatus,done=done))

}

multiCoreGeneric<-function(parallelMethod,jobType,numCoresToUse,allowIntermediateResults=TRUE,progressStage="",reportProgress=FALSE){

DEBUG_MASTER("multiCoreGeneric()")

    SharedWorkingDir=get("SharedWorkingDir",envir=nlmeEnv)
    num_samples=as.integer(get("num_samples",envir=nlmeEnv))
    assign("jobType", jobType, envir=nlmeEnv)
    seeds=1:num_samples 
    if ( jobType == "BOOTSTRAP" ) {
        start_seed=as.integer(get("start_seed",envir=nlmeEnv))
        DoInitialNlmeRun=TRUE
        for ( i in 1:num_samples ) {
            seeds[i] = start_seed + ( i - 1 ) * 100
        }
    }
    else {
        DoInitialNlmeRun=FALSE
    }


    currWorkDir=getwd()
    copyFiles(SharedWorkingDir)
    setwd(SharedWorkingDir)
    flag=Sys.getenv("NLME_SKIP_INITIAL_BOOTSTRAP_RUN")
    if ( flag == "TRUE" ) {
        DoInitialNlmeRun = FALSE
    }

    if ( DoInitialNlmeRun ) {
        stat=OLDrunNLMEInitialRun()
        if ( stat == FALSE ) 
            return(list(stat=FALSE,done=c()))
        generateInitialEstimatesFiles()

        lines=readLines(extra_args_file)
        
        cat(" /d3 cols3.txt data3.txt",file=extra_args_file,sep="\n",append=FALSE)
        for ( l in lines ) {
            cat(l,file=extra_args_file,sep="\n",append=TRUE)
        }
        files_to_copy=get("files_to_copy",envir=nlmeEnv)
        files_to_copy=paste(files_to_copy, " cols3.txt data3.txt")

        assign("files_to_copy", files_to_copy , envir=nlmeEnv)
        writeOutGlobals(sprintf("%s/myglobaldefs.r",SharedWorkingDir))
    }
    else {
        compileAndLinkNLME()
    }

    setwd(currWorkDir)

    setwd(currWorkDir)
    createJobsDirectory(SharedWorkingDir,num_samples)

    setwd(SharedWorkingDir)

DEBUG_MASTER(paste("Master working directory ", getwd()))

    if ( parallelMethod == "MPI" ) {
DEBUG_MASTER("startAndMonitorMPIJobs()")
        ret = startAndMonitorMPIJobs( jobType, numCoresToUse, num_samples,  seeds,allowIntermediateResults )
    }
    else {
        if ( parallelMethod == "LOCAL_MPI" ) 
            numCoresToUse = 1 
        ret = startAndMonitorMulticoreJobs( numCoresToUse, num_samples,  seeds ,allowIntermediateResults ,reportProgress)
    }
    return(ret)

}


shrinkDmpDotTxt <-function(fname){
DEBUG_MASTER("shrinkDmpDotTxt()")
    newFname=paste0(fname,".new")
    if ( !file.exists(newFname) ) {
        lines = readLines(fname)
        b=grep("\"residuals\" =",lines)
        if ( length(b) != 0 ) {
            e=grep("end of residuals",lines)
            n=grep(" names =",lines)
            lines[n] = gsub(", \"residuals\"","",lines[n],fixed=TRUE)
            cat(lines[1:(b-1)], file= newFname, sep = "\n", append=FALSE)
            cat(lines[(e+1):length(lines)], file= newFname,sep = "\n",  append=TRUE)
        }
        else
            cat(lines, file= newFname, sep = "\n", append=FALSE)
    }
    return(newFname)
}

reformatResidualsFile <-function(outFilename){

DEBUG_MASTER("reformatResidualsFile()")
DEBUG_MASTER(outFilename)
    residualFilename = "res.csv"
    {
        lines=readLines(outFilename)
        b = grep("residuals",lines)
        lines[b+1]= gsub("  ID1  ID2  ID3  ID4  ID5","ID1 \t ID2 \t ID3 \t ID4 \t ID5",lines[b+1],fixed=TRUE)
        cat(lines[(b+1):length(lines)], file= residualFilename ,sep = "\n",  append=FALSE)
    }
    rm(lines)
    gc()
    return(residualFilename)
}


regenerateModelFile<-function(){

DEBUG_MASTER("------------------------------------")
DEBUG_MASTER("func --> regenerateModelFile()")
    SharedWorkingDir=get("SharedWorkingDir",envir=nlmeEnv)
    fileToRead=sprintf("%s/%s",SharedWorkingDir,"dmp.txt")
    oldModel=sprintf("%s/test.mdl",SharedWorkingDir)
    newModel=sprintf("%s/test1.mdl",SharedWorkingDir)

    newFileToRead=shrinkDmpDotTxt(fileToRead)
    source(newFileToRead)
    fixedEffects=dmp.txt$coefficients$fixed
    fixedEffectNames=names(fixedEffects)

    lines = scan(oldModel,what="character",sep="\n")
    cat("",file=oldModel,sep="\n",append=FALSE)
    for ( i in 1:length(lines)){
        cat(lines[i],file=oldModel,sep="\n",append=TRUE)
        cat(lines[i],file=newModel,sep="\n",append=TRUE)
    }
    rm(lines)
    cat("override test(){",file=oldModel,sep="\n",append=TRUE)
    for ( i in 1:(length(fixedEffectNames)-1)) {
        cat(sprintf("fixef(%s = c(, %s, ))",fixedEffectNames[i],fixedEffects[[i]]),file=oldModel,sep="\n",append=TRUE)
    }
    cat("}",file=oldModel,sep="\n",append=TRUE)
    rm(dmp.txt)
    gc()

}


#
# Generates cols3.txt and data3.txt
#
generateInitialEstimatesFiles<-function(){

    SharedWorkingDir=get("SharedWorkingDir",envir=nlmeEnv)
    fileToRead=sprintf("%s/%s",SharedWorkingDir,"dmp.txt")

    newFileToRead=shrinkDmpDotTxt(fileToRead)
    source(newFileToRead)
    fixedEffects=dmp.txt$coefficients$fixed
    fixedEffectNames=names(fixedEffects)

    cols3File=sprintf("%s/cols3.txt",SharedWorkingDir)
    data3File=sprintf("%s/data3.txt",SharedWorkingDir)
DEBUG_MASTER("------------------------------------")
DEBUG_MASTER("generateInitialEstimatesFiles()")
DEBUG_MASTER(cols3File)
DEBUG_MASTER(data3File)
DEBUG_MASTER("------------------------------------")
    cat("##Parameter, Initial, Lower, Upper",file=data3File,sep="\n",append=FALSE)
    for ( i in 1:(length(fixedEffectNames)-1)) {
        cat(sprintf("%s ,%s, .,.",fixedEffectNames[i],fixedEffects[[i]]),file=data3File,sep="\n",append=TRUE)
    }

    cat("param(\"Parameter\")",file=cols3File,sep="\n",append=FALSE)
    cat("high(\"Upper\")",file=cols3File,sep="\n",append=TRUE)
    cat("init(\"Initial\")",file=cols3File,sep="\n",append=TRUE)
    cat("low(\"Lower\")",file=cols3File,sep="\n",append=TRUE)
    for ( i in 1:(length(fixedEffectNames)-1)) {
        cat(sprintf("map(\"%s\" <- \"%s\")" ,fixedEffectNames[i],fixedEffectNames[[i]]),file=cols3File,sep="\n",append=TRUE)
    }

}



#
# copy/concatenate/coallate Bootstrap NLME results file from all runs into one
#
collectJobResults<-function(done ){

textfiles= c("out.bin.txt")
csvfiles= c( "BootSubj.csv","out.csv" )
files= c("iniest.csv")

DEBUG_MASTER("------- collectJobResults ------------")
DEBUG_MASTER(paste(done))
    SharedWorkingDir=get("SharedWorkingDir",envir=nlmeEnv)
    UsingBatchTools=get("UsingBatchTools",envir=nlmeEnv)
    baseDirectory =getBatchDirectoryLocation(SharedWorkingDir)
    appFlag=FALSE
    for ( f in textfiles ) {
        outFileName = sprintf("%s/../%s",SharedWorkingDir,f)
        for ( job in done ) {
            baseIndx=job %% 100
            wd=sprintf("%s/jobs/%02d/%d/",baseDirectory,baseIndx,job)
            if ( file.exists(sprintf("%s/%s",wd,f)) ) {
                lines = readLines(sprintf("%s/%s",wd,f))
                cat(lines,file=outFileName,sep="\n",append=appFlag)
                appFlag=TRUE
            }
        }
    }
    for ( f in csvfiles ) {
        appFlag=FALSE
        first = TRUE
        outFileName = sprintf("%s/../%s",SharedWorkingDir,f)
        for ( job in done ) {
            baseIndx=job %% 100
            wd=sprintf("%s/jobs/%02d/%d/",baseDirectory,baseIndx,job)
            if ( file.exists(sprintf("%s/%s",wd,f)) ) {
                lines = readLines(sprintf("%s/%s",wd,f))
                if ( first ) {
                    cat(lines, file=outFileName,sep="\n",append=appFlag)
                }
                else {
                    cat(lines[2:length(lines)], file=outFileName,sep="\n",append=appFlag)
                }
                first = FALSE
                appFlag=TRUE
            }
        }
    }
    wd=sprintf("%s/jobs/%02d/%d/",baseDirectory,1,1)
    for ( f in files ){
        if ( file.exists(sprintf("%s/%s",wd,f)) ) {
            file.copy(sprintf("%s/%s",wd,f),sprintf("%s/../%s",SharedWorkingDir,f))
        }
    }


    file.copy(sprintf("%s/%s",SharedWorkingDir,"test.mdl"),sprintf("%s/../%s",SharedWorkingDir,"test_new.mdl"))
    file.copy(sprintf("%s/%s",SharedWorkingDir,"progress.xml"),sprintf("%s/../%s",SharedWorkingDir,"progress.xml"))

# CLSC-72
# Copy out.txt from the last job to the file we are returning to the GUI
    job=done[length(done)]
    baseIndx=job %% 100
    wd=sprintf("%s/jobs/%02d/%d/",baseDirectory,baseIndx,job)
    file.copy(file.path(wd,"out.txt"),sprintf("%s/../%s",SharedWorkingDir,"out.txt"),overwrite=TRUE)

}



#
# We are having a problem with nightly JUnit tests on the small grid.
# The problem is that we are signaled that the job is finished before it realy
# is!!  Do not yet know what might be causing it.  Lets collect some information
# about what is going on
# All jobs will create a S_00n.status file in shared directory.  File will contain any of :
#
# SUCCESS | FAILED | RUNNING 
# as the 1st line
#
#
hangAroundABitForTheStatusFile <-function(SharedWorkingDir,done){

numTimesChecked=0
maximumNumTimesToCheck = 100
    for ( job in done ) {
        while ( numTimesChecked < maximumNumTimesToCheck ) {
            statusFileIsPresent = FALSE
            tryCatch (
            {
                statusFile=sprintf("%s/S_%03d.status",SharedWorkingDir,job)
                if ( file.exists(statusFile) ) {
                    lines=readLines(statusFile)
                    if ( length(grep("FAILED",lines)) > 0 )  {
                        statusFileIsPresent = TRUE
                    }
                    if ( length(grep("SUCCESS",lines)) > 0 )  {
                        statusFileIsPresent = TRUE
                    }
                }
                if ( statusFileIsPresent ) {
                    numTimesChecked=9999
                    break
                }
             },
             error=function(ex){
    	         Sys.sleep(1)
                DEBUG_MASTER(paste("hangAroundABitForTheStatusFile()",statusFile,numTimesChecked))
             })
            numTimesChecked=numTimesChecked+1
    	    Sys.sleep(5)
        }
    }
}



collectJobStatusAndCoreFiles<-function(job,files,localDir){

DEBUG_MASTER(paste("    collectJobStatusAndCoreFiles()"))
    SharedWorkingDir=get("SharedWorkingDir",envir=nlmeEnv)
    #
    # Return files from individual runs.
    #
    baseIndx=job %% 100
    wd=sprintf("%s/jobs/%02d/%d/",getBatchDirectoryLocation(SharedWorkingDir),baseIndx,job)
    #
    # Files that get copied back
    #
    for ( p in unlist(strsplit(files,split=" ") )){
        p=sprintf("%s$",p)
        expFiles=list.files(wd, pattern = p,all.files=TRUE)
        for ( f in expFiles ) {
            toFile=gsub('.txt.*','.txt', f)
DEBUG_MASTER(paste("Copy From:",sprintf("%s/%s",wd,f)," To : ",sprintf("%s/%s",localDir,toFile)))
            file.copy(sprintf("%s/%s",wd,f),sprintf("%s/%s",localDir,toFile))
            if ( f == 'nlme7engine.log' && submissionPlatform == "unix" ) 
                system(sprintf("unix2dos %s/nlme7engine.log > /dev/null 2>&1",localDir))
            if ( length(grep("^out",f )  ) == 1 && submissionPlatform == "unix" ) 
                system(sprintf("unix2dos %s/%s > /dev/null 2>&1",localDir,toFile))
        }
    }
}

#
# Look for err2.txt in all jobs directories and list the errors
# so they are captured in the log file.
#
collectJobErrorLogs<-function(localDir,done, copyFilesFlag = FALSE ){

DEBUG_MASTER(paste("collectJobErrorLogs()"))

    files=c("err2.txt")
    SharedWorkingDir=get("SharedWorkingDir",envir=nlmeEnv)
    UsingBatchTools=get("UsingBatchTools",envir=nlmeEnv)
    baseDirectory = getBatchDirectoryLocation(SharedWorkingDir)
    hangAroundABitForTheStatusFile(SharedWorkingDir,done)

    #
    # Return files from individual runs.
    #
    for ( job in done ) {
        baseIndx=job %% 100
        wd=sprintf("%s/jobs/%02d/%d/",baseDirectory,baseIndx,job)

        #
        # These are files that get tagged with jobname
        for ( f in strsplit(files,split=" ") ){
            if ( copyFilesFlag ) {
                DEBUG_MASTER(paste("Copy From:",sprintf("%s/%s",wd,f)," To : ",sprintf("%s/%s.Job%0d",localDir,f,job)))
                file.copy(sprintf("%s/%s",wd,f),sprintf("%s/%s.Job%0d",localDir,f,job))
                lines=readLines(sprintf("%s/%s.Job%0d",localDir,f,job))
DEBUG_MASTER(paste(lines))
                print(lines)
            }
        }
    }

}


#
# copy/concatenate/coallate NLME results file from all runs into one
#
collectJobResultsGeneric<-function(done, copyFilesFlag = FALSE ){

DEBUG_MASTER(paste("collectJobResultsGeneric()"))

    SharedWorkingDir=get("SharedWorkingDir",envir=nlmeEnv)
    UsingBatchTools=get("UsingBatchTools",envir=nlmeEnv)
    baseDirectory = getBatchDirectoryLocation(SharedWorkingDir)
    hangAroundABitForTheStatusFile(SharedWorkingDir,done)
    #
    # Files to return from initial run or compile NLME7
    files_to_return=get("files_to_return",envir=nlmeEnv)
    for ( f in unlist(strsplit(files_to_return," ")) ){
        outFileName = sprintf("%s/../%s",SharedWorkingDir,f)
        file.copy(sprintf("%s/%s",SharedWorkingDir,f),sprintf("%s/../%s",SharedWorkingDir,f))
    }

    #
    # Return files from individual runs.
    #
    for ( job in done ) {
        baseIndx=job %% 100
        wd=sprintf("%s/jobs/%02d/%d/",baseDirectory,baseIndx,job)
        #
        # Files that get copied back
        #
        files=getOutputFilenames(control_lines[job])
        for ( p in unlist(strsplit(files,split=" ") )){
            p=sprintf("%s$",p)
            expFiles=list.files(wd, pattern = p,all.files=TRUE)

            for ( f in expFiles ) {
                if ( copyFilesFlag ) {
DEBUG_MASTER(paste("Copy From:",sprintf("%s/%s",wd,f)," To : ",sprintf("%s/../%s",SharedWorkingDir,f)))
                    file.copy(sprintf("%s/%s",wd,f),sprintf("%s/../%s",SharedWorkingDir,f))
                      if ( f == 'nlme7engine.log' && submissionPlatform == "unix" ) 
                          system(sprintf("unix2dos %s/../nlme7engine.log > /dev/null 2>&1",SharedWorkingDir))
# only supported afte R3.3  if ( startsWith(f,"out" )  )
                      if ( length(grep("^out",f )  ) == 1 && submissionPlatform == "unix" ) 
                          system(sprintf("unix2dos %s/../%s > /dev/null 2>&1",SharedWorkingDir,f))
                }
                else {
                  if ( f == 'nlme7engine.log' && submissionPlatform == "unix" ) 
                      system(sprintf("unix2dos %s/nlme7engine.log > /dev/null 2>&1",wd))
                  if ( length(grep("^out",f )  ) == 1 && submissionPlatform == "unix" ) 
                      system(sprintf("unix2dos %s/%s > /dev/null 2>&1",wd,f))
                }
            }
        }
        #
        # These are files that get tagged with jobname
        files=getOutputFilenames2(control_lines[job])
        for ( f in strsplit(files,split=" ") ){
            if ( copyFilesFlag ) {
                DEBUG_MASTER(paste("Copy From:",sprintf("%s/%s",wd,f)," To : ",sprintf("%s/../%s.Job%0d",SharedWorkingDir,f,job)))
                file.copy(sprintf("%s/%s",wd,f),sprintf("%s/../%s.Job%0d",SharedWorkingDir,f,job))
            }
        }
    }

    # Progress.xml file
    file.copy(sprintf("%s/%s",SharedWorkingDir,"progress.xml"),sprintf("%s/../%s",SharedWorkingDir,"progress.xml"))

}

#
# Get the list of files that is needed to be copied back from remote disk
#
getGenericResultsList <-function(control_lines){

    file_list= c("progress.xml")
    
    files_to_return=get("files_to_return",envir=nlmeEnv)

    for ( f in unlist(strsplit(files_to_return," "))){
        file_list=c(file_list,f)
    }
    for ( l in control_lines ) {
        files=unlist(strsplit(l, split=",") )[5]
        for ( f in unlist(strsplit(files," "))){
            file_list=c(file_list,f)
        }
    }
    job=1
    for ( l in control_lines ) {
        files=unlist(strsplit(l, split=",") )[6]
        for ( f in unlist(strsplit(files," "))){
            file_list=c(file_list,sprintf("%s.Job%0d",f,job))
        }
        job = job + 1 
    }
    return(file_list)
}

#
# Grab the extra argument file name ( i.e. nlmeargs.txt ) out of a line
#
getExtraArgumentFilename <-function(line){
    fileRec=unlist(strsplit(line, split=",") )[2]
    file=unlist(strsplit(fileRec, split=":") )[1]
    return(file)
}
getExtraArgumentFilenameIndex <-function(line){
    fileRec=unlist(strsplit(line, split=",") )[2]
    fileIndex=unlist(strsplit(fileRec, split=":") )[2]
    return(fileIndex)
}

getRunSuccessFilename <-function(line){
    file=unlist(strsplit(line, split=",") )[4]
    return(file)
}

getOutputFilenames <-function(line){
    file=unlist(strsplit(line, split=",") )[5]
    return(file)
}

getOutputFilenames2 <-function(line){
    file=unlist(strsplit(line, split=",") )[6]
    return(file)
}
getScenarioName <-function(line){
    name=unlist(strsplit(line, split=",") )[1]
    return(name)
}
getExePostfix <-function(line){
    exePostfix=gsub("^\\s+|\\s+$", "",(unlist(strsplit(line, split=",") )[7]))
    if ( is.na(exePostfix)) 
        exePostfix=""
    return(exePostfix)
}

checkJobStatus <- function(SharedWorkingDir,Job){

DEBUG_SLAVE(sprintf(".%s.",SharedWorkingDir))
    statusFile=sprintf("%s/S_%03d.status",SharedWorkingDir,Job)
DEBUG_SLAVE(sprintf("..Checking status in %s..",statusFile))
    statusFileLines=readLines(statusFile)
DEBUG_SLAVE(sprintf("After reading status file lines lenght = %d",length(statusFileLines)))
DEBUG_SLAVE(paste(statusFileLines,collalpse=''))
DEBUG_SLAVE("-------------------")
for ( l in statusFileLines ) {
DEBUG_SLAVE("......................")
DEBUG_SLAVE(l)
}
    if ( length(grep("FAILED",statusFileLines)) > 0 )  {
DEBUG_SLAVE("FALSE..........")
        return(FALSE)
    }
    else {
DEBUG_SLAVE("TRUE........")
        return(TRUE)
    }
}






#
# Reads bootstrap file and transposes the data
# It computes the following statistics per sample run:
# Mean, Stderr, CV, Median, Percentiles
#
# It will generate ascii files : BootTheta.txt and BootThetaStacked.txt
#
#' Reads bootstrap file and transposes the data
#' It computes the following statistics per sample run:
#' Mean, Stderr, CV, Median, Percentiles
#'
#' It will generate ascii files : BootTheta.txt and BootThetaStacked.txt
#' 
#' @param local working directory
#' @param Confidence level
#' @keywords summarizeBootstrap
#' @export
#' @examples
#' summarizeBootstrap(localWorkingDir,confidenceLevel)

summarizeBootstrap <-function(localWorkingDir,confidenceLevel){
separator="9875"
outputFilename="out.csv"
bootstrapFilename="out.bin.txt"
rDumpFile="dmp.txt"
bootSecondaryFilename="BootSecondary.csv"
bootThetaFilename="BootTheta.csv"
bootOverallFilename="BootOverall.csv"
bootThetaStackedFilename="BootThetaStacked.csv"
bootOmegaFilename="BootOmega.csv"
bootOmegaStackedFilename="BootOmegaStacked.csv"
bootOmegaStderrFilename="BootOmegaStderr.csv"
bootVarCoVarFilename="BootVarCoVar.csv"

DEBUG_MASTER("---summarizeBootstrap()---")
DEBUG_MASTER(localWorkingDir)
tryCatch (
{
    setwd(localWorkingDir)
    fn <- "dmp.txt.new"
    if (file.exists(fn)) file.remove(fn)
    fileToRead="dmp.txt"
    newFileToRead=shrinkDmpDotTxt(fileToRead)
    source(newFileToRead)
    fileToRead=bootstrapFilename
    if ( file.exists(fileToRead) ) {
        dat=readLines(fileToRead)
        fileToRead=outputFilename
        outputData=read.table(fileToRead,header=TRUE,sep=",")
        numSamples=0
        for ( d in dat ) {
            if ( d == separator ) {
                numSamples = numSamples + 1
            }
        }
        fixedEffectNames=names(dmp.txt$coefficients$fixed)
        secondaryEffectNames=names(dmp.txt$coefficients$secondary)
        randomEffectNames=dimnames(dmp.txt$omega)[1]

        numFixedEffects=length(fixedEffectNames)

        numParams = as.integer(dat[3])
        numObservations = as.integer(dat[4])
        numSubjects = as.integer(dat[5])
        numFixedEffects = as.integer(dat[6])
        numSecondaryEffects = as.integer(dat[7+numFixedEffects])
        numRandomEffects = as.integer(dat[8+numFixedEffects+numSecondaryEffects])
        stuff=list(dim=numSamples)
        row = 0 
        for ( d in dat ) {
           if ( d == separator ) {
               if ( row != 0 ) {
                   stuff[row] = currentList
               }
               row = row + 1 
               currentList = list()
           } 
           else {
               currentList=list(unlist(currentList),d)
           }
        }

        stuff[row] = currentList

        if ( numSecondaryEffects > 0 ) {
        secondaryEffectsMeanValues=vector()

# Generate BootSecondary.txt

        frac=(100.0 - confidenceLevel )/200
        cat(sprintf("Scenario,Parameter,Mean,Stderr,CV%%,Median,%3.1f%%,%4.1f%%",frac*100.0,100.0-(frac*100)),file=bootSecondaryFilename,sep="\n",append=FALSE)
        for ( c in  (6+numFixedEffects+1):(6 + numFixedEffects + numSecondaryEffects  )  ) {

            secondaryEffectsValues=vector()
            for ( r in 1: numSamples ) {
                secondaryEffectsValues=append(secondaryEffectsValues,as.double(stuff[[r]][[c]]))
           }

           secondaryEffectsMeanValues=append(secondaryEffectsMeanValues,mean(secondaryEffectsValues))
           frac=(100.0 - confidenceLevel )/200
           q=quantile(secondaryEffectsValues,c(frac,1.0-frac))
           sumsq=0.0
           for ( n in secondaryEffectsValues ) {
               sumsq= sumsq + n * n
           }
           myVar=sumsq/length(secondaryEffectsValues) - mean(secondaryEffectsValues) * mean(secondaryEffectsValues)
           stdError=sqrt(myVar)
           cvPercent=100 * stdError / mean(secondaryEffectsValues)
          
           name=secondaryEffectNames[c-(6+numFixedEffects)]
           cat(sprintf("(B),%s,%9.7f,%9.7f,%9.7f,%9.7f,%9.7f,%9.7f",name,mean(secondaryEffectsValues),stdError,cvPercent,median(secondaryEffectsValues),q[1],q[2]),file=bootSecondaryFilename,sep="\n",append=TRUE)
        }
        }




        fixedEffectsMeanValues=vector()

# Generate BootTheta.txt

        frac=(100.0 - confidenceLevel )/200
        cat(sprintf("Scenario,Parameter,Mean,Stderr,CV%%,Median,%3.1f%%,%4.1f%%",frac*100.0,100.0-(frac*100)),file=bootThetaFilename,sep="\n",append=FALSE)
        for ( c in  6:(6 + numFixedEffects -1 )  ) {

        fixedEffectsValues=vector()
        for ( r in 1: numSamples ) {
            fixedEffectsValues=append(fixedEffectsValues,as.double(stuff[[r]][[c]]))
       }


       fixedEffectsMeanValues=append(fixedEffectsMeanValues,mean(fixedEffectsValues))
       frac=(100.0 - confidenceLevel )/200
       q=quantile(fixedEffectsValues,c(frac,1.0-frac))
       sumsq=0.0
       for ( n in fixedEffectsValues ) {
           sumsq= sumsq + n * n
       }
       myVar=sumsq/length(fixedEffectsValues) - mean(fixedEffectsValues) * mean(fixedEffectsValues)
       stdError=sqrt(myVar)
       cvPercent=100 * stdError / mean(fixedEffectsValues)
       if ( c == 6 + numFixedEffects -1 ) {
           name="stdev0"
       }
       else {
           name=fixedEffectNames[c-5]
       }       
       cat(sprintf("(B),%s,%9.7f,%9.7f,%9.7f,%9.7f,%9.7f,%9.7f",name,mean(fixedEffectsValues),stdError,cvPercent,median(fixedEffectsValues),q[1],q[2]),file=bootThetaFilename,sep="\n",append=TRUE)
        }

#
# Write out BootVarCoVar.csv
#
    line="Scenario,Var Name"
    for ( c in 1: numFixedEffects ) {
       if ( c == numFixedEffects ) {
           name="stdev0"
       }
       else {
           name=fixedEffectNames[c]
       }       
        line=sprintf("%s,%s",line,name)
    }
    cat(line,file=bootVarCoVarFilename,sep="\n",append=FALSE)
    for ( f in 1 : (numFixedEffects ) ) {
       if ( f == numFixedEffects ) {
           name="stdev0"
       }
       else {
           name=fixedEffectNames[f]
       }       
        line=sprintf("(B),%s",name)

        for ( s in 1 : f ) {
            sum = 0.0 
            for ( r in 1:(numSamples  )) {
                sum = sum + (as.double(stuff[[r]][[5+s]]) - fixedEffectsMeanValues[s] ) * ( as.double(stuff[[r]][[5+f]]) - fixedEffectsMeanValues[f]  )
            }
            dvcv = sum / numSamples 
            line=sprintf("%s,%13.11f",line,dvcv)
        }
        while ( s < numFixedEffects ) {
            line=sprintf("%s,",line)
            s=s+1
        }
        cat(line,file=bootVarCoVarFilename,sep="\n",append=TRUE)
    }
#
# Generate BootThetaStacked.txt
#
    cat(sprintf("Scenario,Replicate,Theta,Value"),file=bootThetaStackedFilename,sep="\n",append=FALSE)
    appendFlag=TRUE
    for ( r in 1: numSamples ) {
        for ( c in  6:(6 + numFixedEffects -1 )  ) {
            cat(sprintf("(B),%d,%s,%9.7f",r,fixedEffectNames[c-5],as.double(stuff[[r]][[c]])),file=bootThetaStackedFilename,sep="\n",append=appendFlag)
            appendFlag=TRUE
        }
    }
#
# Generate BootOverall.txt
#
    cat(sprintf("Scenario,Replicate,ReturnCode,LL"),file=bootOverallFilename,sep="\n",append=FALSE)
    appendFlag=TRUE
    for ( r in 1: numSamples ) {
        cat(sprintf("(B),%d,%d,%9.7f",r,outputData[1][r,],outputData[2][r,]),file=bootOverallFilename,sep="\n",append=appendFlag)
    }

    outNames=names(outputData)
    numOmegas=( length(outNames) - 2 - length(fixedEffectNames)) 


#
# Generate BootOmegaStacked file
#
    cat(sprintf("Scenario,Replicate,Omega,Value"),file=bootOmegaStackedFilename,sep="\n",append=FALSE)
    for ( r in 1: numSamples ) {
        for ( o in 1: numOmegas ) {
            name=gsub("omega.","",outNames[numFixedEffects+2+o],fixed=TRUE)
            name=gsub(".","_",name,fixed=TRUE)
            name=substr(name,1,nchar(name)-1)
            cat(sprintf("(B),%d,%s,%13.11f",r,name,outputData[o+2+numFixedEffects][r,]),file=bootOmegaStackedFilename,sep="\n",append=TRUE)
        }
    }


#
# Grab mean and stderr for all omega values
#
    randomEffectsOmegaVector = vector()
    meanOmegaVector = vector()
    stderrOmegaVector = vector()
    for ( o in 1 : numOmegas ) {
        omegaValues=vector()
        for ( r in 1: numSamples ) {
            omegaValues=append(omegaValues,as.double(outputData[o+2+numFixedEffects][r,]))
        }
        sumsq=0.0
        for ( n in omegaValues ) {
           sumsq= sumsq + n * n
        }
        myVar=sumsq/length(omegaValues) - mean(omegaValues) * mean(omegaValues)
        stdError=sqrt(myVar)

        meanOmegaVector=append(meanOmegaVector,mean(omegaValues))
        stderrOmegaVector=append(stderrOmegaVector,stdError)
    }

    indx = 1 
    for ( r in 1 : numRandomEffects ) {
        for ( c in 1 : r ) {
            if ( r == c ) {
                if ( meanOmegaVector[indx]  > 0.0 ) {
                    val = sqrt(meanOmegaVector[indx])
                } else {
                    val = 1
                }
                randomEffectsOmegaVector=append(randomEffectsOmegaVector,val)
            }
            indx=indx+1
        }
    }

#
# Write out BootOmage file
#
    line="Scenario,Label"
    for ( r in 1: numRandomEffects ) {
        line=sprintf("%s,%s",line,randomEffectNames[[1]][r])
    }
    cat(line,file=bootOmegaFilename,sep="\n",append=FALSE)
    line="(B),Omega"
    for ( r in 1: numRandomEffects -1) {
        line=sprintf("%s,",line)
    }
    cat(line,file=bootOmegaFilename,sep="\n",append=TRUE)


    indx=1
    line="(B)"
    for ( r in 1: numRandomEffects ) {
        line=sprintf("%s,%s",line,randomEffectNames[[1]][r])
        for ( c in 1:r ) {
            if ( nchar(line) == 0 ) {
                line=sprintf("%9.7f",meanOmegaVector[indx])
            }
            else {
                line=sprintf("%s,%9.7f",line,meanOmegaVector[indx])
            }
            indx = indx + 1 
        }
        while ( r < numRandomEffects ) {
            line=sprintf("%s,",line)
            r=r+1
        }
        cat(line,file=bootOmegaFilename,sep="\n",append=TRUE)
        line="(B)"
    }
#
# Write out corellation
#
    line="(B),Correlation"
    s=1
    while ( s < ( numRandomEffects + 1 ) ) {
            line=sprintf("%s,",line)
            s=s+1
    }
    cat(line,file=bootOmegaFilename,sep="\n",append=TRUE)
    indx=1
    line="(B)"
    for ( r in 1: numRandomEffects ) {
        line=sprintf("%s,%s",line,randomEffectNames[[1]][r])
        for ( c in 1:r ) {
            dStdI=randomEffectsOmegaVector[r]
            dStdJ=randomEffectsOmegaVector[c]

            dOmega=meanOmegaVector[indx]
            val = dOmega / ( dStdI * dStdJ )
            line=sprintf("%s,%d",line,as.integer(round(val)))
            indx = indx + 1 
        }
        while ( r < numRandomEffects ) {
            line=sprintf("%s,",line)
            r=r+1
        }
        cat(line,file=bootOmegaFilename,sep="\n",append=TRUE)
        line="(B)"
    }
#
# Write out BootOmagaStderr file
#
    line="Scenario,Label"
    for ( r in 1: numRandomEffects ) {
        line=sprintf("%s,%s",line,randomEffectNames[[1]][r])
    }
    cat(line,file=bootOmegaStderrFilename,sep="\n",append=FALSE)

    indx=1
    line="(B)"
    for ( r in 1: numRandomEffects ) {
        line=sprintf("%s,%s",line,randomEffectNames[[1]][r])
        for ( c in 1:r ) {
            if ( nchar(line) == 0 ) {
                line=sprintf("%9.7f",stderrOmegaVector[indx])
            }
            else {
                line=sprintf("%s,%13.11f",line,stderrOmegaVector[indx])
            }
            indx = indx + 1 
        }
        while ( r < numRandomEffects ) {
            line=sprintf("%s,",line)
            r=r+1
        }
        cat(line,file=bootOmegaStderrFilename,sep="\n",append=TRUE)
        line="(B)"
    }
    }

},
         error=function(ex){
})

}


#
# Reads the requested number of lines out of progress.txt file while a 
# job is running.
#
# Returns : a vector of strings, the 1st line is the header information
#

readProgressUpdate <- function(jobDirectory,numIterationsToRead){

    results=c()
tryCatch (
{
    filename=file.path(jobDirectory,"progress.txt")
    if ( file.exists(filename) ) {
    lines=readLines(filename)
    if ( length(lines) > 2 ) {
        nSubj=gsub(")","",unlist(strsplit(lines[1]," = "))[2],fixed=TRUE)
        nObs=gsub(")","",unlist(strsplit(lines[2]," = "))[2],fixed=TRUE)
        lines=lines[3:length(lines)]
        iterLineNos=grep("Iteration",lines)
        numIterations=length(iterLineNos)
        numTokens=length(lines)/numIterations - 1
   
        firstTime=TRUE
        startIteration=(numIterations-numIterationsToRead+1)
        if ( startIteration < 1 )
            startIteration = 1 
        for ( it in numIterations:startIteration ) {
            firstLine=(it - 1 ) * (numTokens + 1 )  + 1
            names=list(numTokens)
            values=list(numTokens)
            headerLine=""
            detailLine=""
            for ( indx in 1:numTokens ) {
                l=lines[firstLine-1 + indx]
                values[indx]=gsub(")","",unlist(strsplit(l," = "))[2],fixed=TRUE) 
                names[indx]=unlist(strsplit(unlist(strsplit(l," = "))[1],"(",fixed=TRUE))[2]
                if ( names[indx] == "LL" ) {
                    names[indx] = "-2LL"
                    values[indx] = sprintf("%7.2f",as.numeric(values[indx]) * -2 ) 
                }
                detailLine=sprintf("%s \t%-10s",detailLine,values[[indx]])

                if ( firstTime == TRUE ) {
                    headerLine=sprintf("%s \t%-10s",headerLine,names[[indx]])
                }
            }
            if ( firstTime == TRUE ) {
                headerLine=sprintf("%s \t%-10s \t%-10s",headerLine,"nSubj","nObs")
                results=c(results,headerLine)
                firstTime= FALSE
            }
            detailLine=sprintf("%s \t%-10s \t%-10s",detailLine,nSubj,nObs)
            results=c(results,detailLine)
        }
        rm(lines)
    }
    } else {
        filename=file.path(jobDirectory,"err1.txt")
        if ( file.exists(filename) ) {
            lines=readLines(filename)
            iterLines=grep("iter=",lines)
            start=length(iterLines)-numIterationsToRead
            if ( start < 1 ) 
                start = 1
            for ( i in length(iterLines):start ){
                results=c(results,lines[iterLines[i]])
            }
            rm(lines)
            rm(iterLines)
        }
    }
},
         error=function(ex){
})
    gc()
    return(results)
}

#
# Summarizes the results from estimation runs sorted by particular columns
# generates Overall.csv and StatusWindows.txt
#
summarizeSortByIdEstimation <-function(localWorkingDir,jobList,control_lines){

require(reshape)

    workflow_name=get("workflow_name",envir=nlmeEnv)
    unique_sorted_values=get("unique_sorted_values",envir=nlmeEnv)
    jobsDirectoryRoot=get("jobsDirectoryRoot",envir=nlmeEnv)
    jobsBaseDirectory=getBatchDirectoryLocation(jobsDirectoryRoot)
    UsingBatchTools=get("UsingBatchTools",envir=nlmeEnv)
    setwd(localWorkingDir)

    GlobalSummaryLine1=sprintf("Summarizing sort column estimation %d runs",length(jobList))
    assign("GlobalSummaryLine1", GlobalSummaryLine1, envir=nlmeEnv)
    UpdateProgressMessages()
    GlobalSummaryLine2=""
    assign("GlobalSummaryLine2", GlobalSummaryLine2, envir=nlmeEnv)
    GlobalSummaryLine3=""
    assign("GlobalSummaryLine3", GlobalSummaryLine3, envir=nlmeEnv)

    fileNames=c()
    scenarioNames=c()
    for ( job in jobList) {
        fileName= getRunSuccessFilename(control_lines[job])
        # expand the filename to point to original run directory
        if ( jobsDirectoryRoot != "" ) {
            jobBaseIndx =job %% 100
            fileName=sprintf("%s/jobs/%02d/%d/%s",jobsBaseDirectory,jobBaseIndx,job,fileName)
        }
        else
            fileName=sprintf("%s.Job%d",fileName,job)
        fileNames=c(fileNames,fileName)
        sName = getScenarioName(control_lines[job])
        if ( ! sName %in% scenarioNames ) 
            scenarioNames=c(scenarioNames,sName)
    }

    GlobalSummaryLine1=sprintf("Summarizing sort column estimation %d runs",length(jobList))
    assign("GlobalSummaryLine1", GlobalSummaryLine1, envir=nlmeEnv)
    GlobalSummaryLine2=sprintf("Generating Overall.csv")
    assign("GlobalSummaryLine2", GlobalSummaryLine2, envir=nlmeEnv)
    GlobalSummaryLine3=""
    assign("GlobalSummaryLine3", GlobalSummaryLine3, envir=nlmeEnv)
DEBUG_MASTER("generateOverallSummary")
    generateOverallSummary(fileNames,scenarioNames,TRUE)
    generateOmegaStderr(fileNames,scenarioNames,TRUE)
    UpdateProgressMessages()

    filenames=c()
    progressfilenames=c()
    scenarioNames=c()
    for ( job in jobList ) {
        statusFile=sprintf("S_%03d.status",job)
        fileName="progress.txt"
        statusFile="progress.txt"
        if ( jobsDirectoryRoot != "" ) {
            jobBaseIndx =job %% 100
            progressFile=sprintf("%s/jobs/%02d/%d/%s",jobsBaseDirectory,jobBaseIndx,job,fileName)
            statusFile=sprintf("%s/S_%03d.status",jobsDirectoryRoot,as.integer(job))
        }
        else
            progressFile=sprintf("%s.Job%0d",fileName,job)
        filenames=c(filenames,statusFile)
        progressfilenames=c(progressfilenames,progressFile)
        sName = getScenarioName(control_lines[job])
        if ( ! sName %in% scenarioNames ) 
            scenarioNames=c(scenarioNames,sName)
    }
    GlobalSummaryLine1=sprintf("Summarizing sort column estimation %d runs",length(jobList))
    assign("GlobalSummaryLine1", GlobalSummaryLine1, envir=nlmeEnv)
    GlobalSummaryLine2=sprintf("Generating StatusWindow.txt")
    assign("GlobalSummaryLine2", GlobalSummaryLine2, envir=nlmeEnv)
    GlobalSummaryLine3=""
    assign("GlobalSummaryLine3", GlobalSummaryLine3, envir=nlmeEnv)
    UpdateProgressMessages()
DEBUG_MASTER("generateStatusWindow")
    generateStatusWindow(filenames,progressfilenames,scenarioNames)

    GlobalSummaryLine1=sprintf("Summarizing sort column estimation %d runs",length(jobList))
    assign("GlobalSummaryLine1", GlobalSummaryLine1, envir=nlmeEnv)
    GlobalSummaryLine2=sprintf("Generating EtaEta.csv")
    assign("GlobalSummaryLine2", GlobalSummaryLine2, envir=nlmeEnv)
    GlobalSummaryLine3=""
    assign("GlobalSummaryLine3", GlobalSummaryLine3, envir=nlmeEnv)
    UpdateProgressMessages()
DEBUG_MASTER("generateEtaEtaTable")
    generateEtaEtaTable(jobList,unique_sorted_values,scenarioNames)

DEBUG_MASTER("renameDmpDotTxtFiles")
    renameDmpDotTxtFiles(jobList,unique_sorted_values,scenarioNames)

    GlobalSummaryLine1=sprintf("Summarizing sort column estimation %d runs",length(jobList))
    assign("GlobalSummaryLine1", GlobalSummaryLine1, envir=nlmeEnv)
    GlobalSummaryLine2=sprintf("Generating EtaCov.csv")
    assign("GlobalSummaryLine2", GlobalSummaryLine2, envir=nlmeEnv)
    GlobalSummaryLine3=""
    assign("GlobalSummaryLine3", GlobalSummaryLine3, envir=nlmeEnv)
    UpdateProgressMessages()
DEBUG_MASTER("readEtaCovTxtFile")
    readEtaCovTxtFile(jobList,unique_sorted_values,scenarioNames)

    fileNames=c()
    for ( job in jobList) {
        fileName=getRunSuccessFilename(control_lines[job])
        # expand the filename to point to original run directory
        if ( jobsDirectoryRoot != "" ) {
            jobBaseIndx =job %% 100
            fileName=sprintf("%s/jobs/%02d/%d/%s",jobsBaseDirectory,jobBaseIndx,job,fileName)
        }
        fileNames=c(fileNames,fileName)
    }
    GlobalSummaryLine1=sprintf("Summarizing sort column estimation %d runs",length(jobList))
    assign("GlobalSummaryLine1", GlobalSummaryLine1, envir=nlmeEnv)
    GlobalSummaryLine2=sprintf("Generating Secondary.csv")
    assign("GlobalSummaryLine2", GlobalSummaryLine2, envir=nlmeEnv)
    GlobalSummaryLine3=""
    assign("GlobalSummaryLine3", GlobalSummaryLine3, envir=nlmeEnv)
    UpdateProgressMessages()
DEBUG_MASTER("generateSecondary")
    generateSecondary(fileNames,unique_sorted_values,scenarioNames)

    GlobalSummaryLine1=sprintf("Summarizing sort column estimation %d runs",length(jobList))
    assign("GlobalSummaryLine1", GlobalSummaryLine1, envir=nlmeEnv)
    GlobalSummaryLine2=sprintf("Generating StrCovariate.csv")
    assign("GlobalSummaryLine2", GlobalSummaryLine2, envir=nlmeEnv)
    GlobalSummaryLine3=""
    assign("GlobalSummaryLine3", GlobalSummaryLine3, envir=nlmeEnv)
    UpdateProgressMessages()
DEBUG_MASTER("readStrCovTxtFile")
    readStrCovTxtFile(jobList,unique_sorted_values,scenarioNames)


    GlobalSummaryLine1=sprintf("Summarizing sort column estimation %d runs",length(jobList))
    assign("GlobalSummaryLine1", GlobalSummaryLine1, envir=nlmeEnv)
    GlobalSummaryLine2=sprintf("Generating ConvergenceData.csv")
    assign("GlobalSummaryLine2", GlobalSummaryLine2, envir=nlmeEnv)
    GlobalSummaryLine3=""
    assign("GlobalSummaryLine3", GlobalSummaryLine3, envir=nlmeEnv)
    UpdateProgressMessages()
DEBUG_MASTER("generateConvergencFile")
    generateConvergencFile(jobList,unique_sorted_values,progressfilenames,scenarioNames)

    GlobalSummaryLine1=sprintf("Summarizing sort column estimation %d runs",length(jobList))
    assign("GlobalSummaryLine1", GlobalSummaryLine1, envir=nlmeEnv)
    GlobalSummaryLine2=sprintf("Generating initest.csv")
    assign("GlobalSummaryLine2", GlobalSummaryLine2, envir=nlmeEnv)
    GlobalSummaryLine3=""
    assign("GlobalSummaryLine3", GlobalSummaryLine3, envir=nlmeEnv)
    UpdateProgressMessages()
DEBUG_MASTER("concatenateInitEstimates")
    concatenateInitEstimates(jobList,unique_sorted_values,scenarioNames)

    GlobalSummaryLine1=sprintf("Summarizing sort column estimation %d runs",length(jobList))
    assign("GlobalSummaryLine1", GlobalSummaryLine1, envir=nlmeEnv)
    GlobalSummaryLine2=sprintf("Generating dose.csv")
    assign("GlobalSummaryLine2", GlobalSummaryLine2, envir=nlmeEnv)
    GlobalSummaryLine3=""
    assign("GlobalSummaryLine3", GlobalSummaryLine3, envir=nlmeEnv)
    UpdateProgressMessages()
DEBUG_MASTER("generateDoseTable")
    generateDoseTable(jobList,unique_sorted_values,scenarioNames)

    GlobalSummaryLine1=sprintf("Summarizing sort column estimation %d runs",length(jobList))
    assign("GlobalSummaryLine1", GlobalSummaryLine1, envir=nlmeEnv)
    GlobalSummaryLine2=sprintf("Generating omega.csv")
    assign("GlobalSummaryLine2", GlobalSummaryLine2, envir=nlmeEnv)
    GlobalSummaryLine3=""
    assign("GlobalSummaryLine3", GlobalSummaryLine3, envir=nlmeEnv)
    UpdateProgressMessages()
DEBUG_MASTER("generateOmegaEtas")
    generateOmegaEtas(jobList,unique_sorted_values,scenarioNames)

    GlobalSummaryLine1=sprintf("Summarizing sort column estimation %d runs",length(jobList))
    assign("GlobalSummaryLine1", GlobalSummaryLine1, envir=nlmeEnv)
    GlobalSummaryLine2=sprintf("Generating thetas.csv")
    assign("GlobalSummaryLine2", GlobalSummaryLine2, envir=nlmeEnv)
    GlobalSummaryLine3=""
    assign("GlobalSummaryLine3", GlobalSummaryLine3, envir=nlmeEnv)
    UpdateProgressMessages()
DEBUG_MASTER("generateThetas")
    generateThetas(jobList,unique_sorted_values,scenarioNames)

    GlobalSummaryLine1=sprintf("Summarizing sort column estimation %d runs",length(jobList))
    assign("GlobalSummaryLine1", GlobalSummaryLine1, envir=nlmeEnv)
    GlobalSummaryLine2=sprintf("Generating Eta.csv and EtaStacked.csv")
    assign("GlobalSummaryLine2", GlobalSummaryLine2, envir=nlmeEnv)
    GlobalSummaryLine3=""
    assign("GlobalSummaryLine3", GlobalSummaryLine3, envir=nlmeEnv)
    UpdateProgressMessages()
DEBUG_MASTER("generateEtaSpreadsheet")
    generateEtaSpreadsheet(jobList,unique_sorted_values,scenarioNames)


    GlobalSummaryLine1=sprintf("Summarizing sort column estimation %d runs",length(jobList))
    assign("GlobalSummaryLine1", GlobalSummaryLine1, envir=nlmeEnv)
    GlobalSummaryLine2=sprintf("Generating Residuals.csv and Resid2.csv ")
    assign("GlobalSummaryLine2", GlobalSummaryLine2, envir=nlmeEnv)
    GlobalSummaryLine3=""
    assign("GlobalSummaryLine3", GlobalSummaryLine3, envir=nlmeEnv)
    UpdateProgressMessages()
DEBUG_MASTER("generateResiduals")
    generateResiduals(jobList,unique_sorted_values,scenarioNames,jobsBaseDirectory)



    GlobalSummaryLine1=sprintf("Summarizing sort column estimation %d runs",length(jobList))
    assign("GlobalSummaryLine1", GlobalSummaryLine1, envir=nlmeEnv)
    GlobalSummaryLine2=sprintf("Generating posthoc.csv and table0n.csv")
    assign("GlobalSummaryLine2", GlobalSummaryLine2, envir=nlmeEnv)
    GlobalSummaryLine3=""
    assign("GlobalSummaryLine3", GlobalSummaryLine3, envir=nlmeEnv)
    UpdateProgressMessages()
    posthocTableNames=getTableNames("cols1.txt")
DEBUG_MASTER("collateTables")
    for ( name in unlist(strsplit(gsub("^\\s+","",posthocTableNames)," ")) )
        collateTables(jobList,unique_sorted_values,scenarioNames,name,name)

    GlobalSummaryLine1=sprintf("Summarizing sort column estimation %d runs",length(jobList))
    assign("GlobalSummaryLine1", GlobalSummaryLine1, envir=nlmeEnv)
    GlobalSummaryLine2=sprintf("Generating NonParam support summaries")
    assign("GlobalSummaryLine2", GlobalSummaryLine2, envir=nlmeEnv)
    GlobalSummaryLine3=""
    assign("GlobalSummaryLine3", GlobalSummaryLine3, envir=nlmeEnv)
    UpdateProgressMessages()
DEBUG_MASTER("generateNonparamSummary")
    generateNonparamSummary(jobList,unique_sorted_values,scenarioNames,jobsBaseDirectory)

    GlobalSummaryLine1=""
    assign("GlobalSummaryLine1", GlobalSummaryLine1, envir=nlmeEnv)
    GlobalSummaryLine1="Transfering data and loading results..."
    GlobalSummaryLine2=""
    assign("GlobalSummaryLine2", GlobalSummaryLine2, envir=nlmeEnv)
    GlobalSummaryLine3=""
    assign("GlobalSummaryLine3", GlobalSummaryLine3, envir=nlmeEnv)
    UpdateProgressMessages()
}


#
# Summarizes the results from estimation runs sorted by particular columns
# generates Overall.csv and StatusWindows.txt
#
summarizeSimpleEstimation <-function(localWorkingDir,jobList,control_lines){

require(reshape)

    workflow_name=get("workflow_name",envir=nlmeEnv)
    unique_sorted_values = NULL
    assign("unique_sorted_values", unique_sorted_values, envir=.GlobalEnv)
    num_sort_columns = 0
    assign("num_sort_columns", num_sort_columns, envir=.GlobalEnv)
    sort_column_names = c()
    assign("sort_column_names", sort_column_names, envir=.GlobalEnv)

    jobsDirectoryRoot=get("jobsDirectoryRoot",envir=nlmeEnv)
    jobsBaseDirectory=getBatchDirectoryLocation(jobsDirectoryRoot)
    UsingBatchTools=get("UsingBatchTools",envir=nlmeEnv)
    setwd(localWorkingDir)

    GlobalSummaryLine1=sprintf("Summarizing sort column estimation %d runs",length(jobList))
    assign("GlobalSummaryLine1", GlobalSummaryLine1, envir=nlmeEnv)
    UpdateProgressMessages()
    GlobalSummaryLine2=""
    assign("GlobalSummaryLine2", GlobalSummaryLine2, envir=nlmeEnv)
    GlobalSummaryLine3=""
    assign("GlobalSummaryLine3", GlobalSummaryLine3, envir=nlmeEnv)

    fileNames=c()
    scenarioNames=c()
    for ( job in jobList) {
        fileName= getRunSuccessFilename(control_lines[job])
        # expand the filename to point to original run directory
        if ( jobsDirectoryRoot != "" ) {
            jobBaseIndx =job %% 100
            fileName=sprintf("%s/jobs/%02d/%d/%s",jobsBaseDirectory,jobBaseIndx,job,fileName)
            fitFileName=sprintf("%s/jobs/%02d/%d/%s",jobsBaseDirectory,jobBaseIndx,job,"nlme7engine.log")
            progressFileName=sprintf("%s/jobs/%02d/%d/%s",jobsBaseDirectory,jobBaseIndx,job,"progress.txt")
        }
        else {
            fileName=sprintf("%s.Job%d",fileName,job)
        }
        fileNames=c(fileNames,fileName)
# Lets try to detect simulation jobs here and get out
if ( length(jobList) == 1 && !
     ( file.exists(fitFileName) || file.exists(progressFileName) )  )
    return()
        sName = getScenarioName(control_lines[job])
        if ( ! sName %in% scenarioNames ) 
            scenarioNames=c(scenarioNames,sName)
    }

    GlobalSummaryLine1=sprintf("Summarizing sort column estimation %d runs",length(jobList))
    assign("GlobalSummaryLine1", GlobalSummaryLine1, envir=nlmeEnv)
    GlobalSummaryLine2=sprintf("Generating Overall.csv")
    assign("GlobalSummaryLine2", GlobalSummaryLine2, envir=nlmeEnv)
    GlobalSummaryLine3=""
    assign("GlobalSummaryLine3", GlobalSummaryLine3, envir=nlmeEnv)
DEBUG_MASTER("generateOverallSummary")
    generateOverallSummary(fileNames,scenarioNames,FALSE)
    generateOmegaStderr(fileNames,scenarioNames,TRUE)
    UpdateProgressMessages()

    filenames=c()
    progressfilenames=c()
    scenarioNames=c()
    for ( job in jobList ) {
        statusFile=sprintf("S_%03d.status",job)
        fileName="progress.txt"
        statusFile="progress.txt"
        if ( jobsDirectoryRoot != "" ) {
            jobBaseIndx =job %% 100
            progressFile=sprintf("%s/jobs/%02d/%d/%s",jobsBaseDirectory,jobBaseIndx,job,fileName)
            statusFile=sprintf("%s/S_%03d.status",jobsDirectoryRoot,as.integer(job))
        }
        else
            progressFile=sprintf("%s.Job%0d",fileName,job)
        filenames=c(filenames,statusFile)
        progressfilenames=c(progressfilenames,progressFile)
        sName = getScenarioName(control_lines[job])
        if ( ! sName %in% scenarioNames ) 
            scenarioNames=c(scenarioNames,sName)
    }
    GlobalSummaryLine1=sprintf("Summarizing sort column estimation %d runs",length(jobList))
    assign("GlobalSummaryLine1", GlobalSummaryLine1, envir=nlmeEnv)
    GlobalSummaryLine2=sprintf("Generating StatusWindow.txt")
    assign("GlobalSummaryLine2", GlobalSummaryLine2, envir=nlmeEnv)
    GlobalSummaryLine3=""
    assign("GlobalSummaryLine3", GlobalSummaryLine3, envir=nlmeEnv)
    UpdateProgressMessages()
DEBUG_MASTER("generateStatusWindow")
    generateStatusWindow(filenames,progressfilenames,scenarioNames)

    GlobalSummaryLine1=sprintf("Summarizing sort column estimation %d runs",length(jobList))
    assign("GlobalSummaryLine1", GlobalSummaryLine1, envir=nlmeEnv)
    GlobalSummaryLine2=sprintf("Generating EtaEta.csv")
    assign("GlobalSummaryLine2", GlobalSummaryLine2, envir=nlmeEnv)
    GlobalSummaryLine3=""
    assign("GlobalSummaryLine3", GlobalSummaryLine3, envir=nlmeEnv)
    UpdateProgressMessages()
DEBUG_MASTER("generateEtaEtaTable")
    generateEtaEtaTable(jobList,unique_sorted_values,scenarioNames)

DEBUG_MASTER("renameDmpDotTxtFiles")
    renameDmpDotTxtFiles(jobList,unique_sorted_values,scenarioNames)

    GlobalSummaryLine1=sprintf("Summarizing sort column estimation %d runs",length(jobList))
    assign("GlobalSummaryLine1", GlobalSummaryLine1, envir=nlmeEnv)
    GlobalSummaryLine2=sprintf("Generating EtaCov.csv")
    assign("GlobalSummaryLine2", GlobalSummaryLine2, envir=nlmeEnv)
    GlobalSummaryLine3=""
    assign("GlobalSummaryLine3", GlobalSummaryLine3, envir=nlmeEnv)
    UpdateProgressMessages()
DEBUG_MASTER("readEtaCovTxtFile")
    readEtaCovTxtFile(jobList,unique_sorted_values,scenarioNames)

    fileNames=c()
    for ( job in jobList) {
        fileName=getRunSuccessFilename(control_lines[job])
        # expand the filename to point to original run directory
        if ( jobsDirectoryRoot != "" ) {
            jobBaseIndx =job %% 100
            fileName=sprintf("%s/jobs/%02d/%d/%s",jobsBaseDirectory,jobBaseIndx,job,fileName)
        }
        fileNames=c(fileNames,fileName)
    }
    GlobalSummaryLine1=sprintf("Summarizing sort column estimation %d runs",length(jobList))
    assign("GlobalSummaryLine1", GlobalSummaryLine1, envir=nlmeEnv)
    GlobalSummaryLine2=sprintf("Generating Secondary.csv")
    assign("GlobalSummaryLine2", GlobalSummaryLine2, envir=nlmeEnv)
    GlobalSummaryLine3=""
    assign("GlobalSummaryLine3", GlobalSummaryLine3, envir=nlmeEnv)
    UpdateProgressMessages()
DEBUG_MASTER("generateSecondary")
    generateSecondary(fileNames,unique_sorted_values,scenarioNames)

    GlobalSummaryLine1=sprintf("Summarizing sort column estimation %d runs",length(jobList))
    assign("GlobalSummaryLine1", GlobalSummaryLine1, envir=nlmeEnv)
    GlobalSummaryLine2=sprintf("Generating StrCovariate.csv")
    assign("GlobalSummaryLine2", GlobalSummaryLine2, envir=nlmeEnv)
    GlobalSummaryLine3=""
    assign("GlobalSummaryLine3", GlobalSummaryLine3, envir=nlmeEnv)
    UpdateProgressMessages()
DEBUG_MASTER("readStrCovTxtFile")
    readStrCovTxtFile(jobList,unique_sorted_values,scenarioNames)


    GlobalSummaryLine1=sprintf("Summarizing sort column estimation %d runs",length(jobList))
    assign("GlobalSummaryLine1", GlobalSummaryLine1, envir=nlmeEnv)
    GlobalSummaryLine2=sprintf("Generating ConvergenceData.csv")
    assign("GlobalSummaryLine2", GlobalSummaryLine2, envir=nlmeEnv)
    GlobalSummaryLine3=""
    assign("GlobalSummaryLine3", GlobalSummaryLine3, envir=nlmeEnv)
    UpdateProgressMessages()
DEBUG_MASTER("generateConvergencFile")
    generateConvergencFile(jobList,unique_sorted_values,progressfilenames,scenarioNames)

    GlobalSummaryLine1=sprintf("Summarizing sort column estimation %d runs",length(jobList))
    assign("GlobalSummaryLine1", GlobalSummaryLine1, envir=nlmeEnv)
    GlobalSummaryLine2=sprintf("Generating initest.csv")
    assign("GlobalSummaryLine2", GlobalSummaryLine2, envir=nlmeEnv)
    GlobalSummaryLine3=""
    assign("GlobalSummaryLine3", GlobalSummaryLine3, envir=nlmeEnv)
    UpdateProgressMessages()
DEBUG_MASTER("concatenateInitEstimates")
    concatenateInitEstimates(jobList,unique_sorted_values,scenarioNames)

    GlobalSummaryLine1=sprintf("Summarizing sort column estimation %d runs",length(jobList))
    assign("GlobalSummaryLine1", GlobalSummaryLine1, envir=nlmeEnv)
    GlobalSummaryLine2=sprintf("Generating dose.csv")
    assign("GlobalSummaryLine2", GlobalSummaryLine2, envir=nlmeEnv)
    GlobalSummaryLine3=""
    assign("GlobalSummaryLine3", GlobalSummaryLine3, envir=nlmeEnv)
    UpdateProgressMessages()
DEBUG_MASTER("generateDoseTable")
    generateDoseTable(jobList,unique_sorted_values,scenarioNames)

    GlobalSummaryLine1=sprintf("Summarizing sort column estimation %d runs",length(jobList))
    assign("GlobalSummaryLine1", GlobalSummaryLine1, envir=nlmeEnv)
    GlobalSummaryLine2=sprintf("Generating omega.csv")
    assign("GlobalSummaryLine2", GlobalSummaryLine2, envir=nlmeEnv)
    GlobalSummaryLine3=""
    assign("GlobalSummaryLine3", GlobalSummaryLine3, envir=nlmeEnv)
    UpdateProgressMessages()
DEBUG_MASTER("generateOmegaEtas")
    generateOmegaEtas(jobList,unique_sorted_values,scenarioNames)

    GlobalSummaryLine1=sprintf("Summarizing sort column estimation %d runs",length(jobList))
    assign("GlobalSummaryLine1", GlobalSummaryLine1, envir=nlmeEnv)
    GlobalSummaryLine2=sprintf("Generating thetas.csv")
    assign("GlobalSummaryLine2", GlobalSummaryLine2, envir=nlmeEnv)
    GlobalSummaryLine3=""
    assign("GlobalSummaryLine3", GlobalSummaryLine3, envir=nlmeEnv)
    UpdateProgressMessages()
DEBUG_MASTER("generateThetas")
    generateThetas(jobList,unique_sorted_values,scenarioNames)

    GlobalSummaryLine1=sprintf("Summarizing sort column estimation %d runs",length(jobList))
    assign("GlobalSummaryLine1", GlobalSummaryLine1, envir=nlmeEnv)
    GlobalSummaryLine2=sprintf("Generating Eta.csv and EtaStacked.csv")
    assign("GlobalSummaryLine2", GlobalSummaryLine2, envir=nlmeEnv)
    GlobalSummaryLine3=""
    assign("GlobalSummaryLine3", GlobalSummaryLine3, envir=nlmeEnv)
    UpdateProgressMessages()
DEBUG_MASTER("generateEtaSpreadsheet")
    generateEtaSpreadsheet(jobList,unique_sorted_values,scenarioNames)


    GlobalSummaryLine1=sprintf("Summarizing sort column estimation %d runs",length(jobList))
    assign("GlobalSummaryLine1", GlobalSummaryLine1, envir=nlmeEnv)
    GlobalSummaryLine2=sprintf("Generating Residuals.csv and Resid2.csv ")
    assign("GlobalSummaryLine2", GlobalSummaryLine2, envir=nlmeEnv)
    GlobalSummaryLine3=""
    assign("GlobalSummaryLine3", GlobalSummaryLine3, envir=nlmeEnv)
    UpdateProgressMessages()
DEBUG_MASTER("generateResiduals")
    generateResiduals(jobList,unique_sorted_values,scenarioNames,jobsBaseDirectory)



    GlobalSummaryLine1=sprintf("Summarizing sort column estimation %d runs",length(jobList))
    assign("GlobalSummaryLine1", GlobalSummaryLine1, envir=nlmeEnv)
    GlobalSummaryLine2=sprintf("Generating posthoc.csv and table0n.csv")
    assign("GlobalSummaryLine2", GlobalSummaryLine2, envir=nlmeEnv)
    GlobalSummaryLine3=""
    assign("GlobalSummaryLine3", GlobalSummaryLine3, envir=nlmeEnv)
    UpdateProgressMessages()
    posthocTableNames=getTableNames("cols1.txt")
DEBUG_MASTER("collateTables")
    for ( name in unlist(strsplit(gsub("^\\s+","",posthocTableNames)," ")) )
        collateTables(jobList,unique_sorted_values,scenarioNames,name,name)

    GlobalSummaryLine1=sprintf("Summarizing sort column estimation %d runs",length(jobList))
    assign("GlobalSummaryLine1", GlobalSummaryLine1, envir=nlmeEnv)
    GlobalSummaryLine2=sprintf("Generating NonParam support summaries")
    assign("GlobalSummaryLine2", GlobalSummaryLine2, envir=nlmeEnv)
    GlobalSummaryLine3=""
    assign("GlobalSummaryLine3", GlobalSummaryLine3, envir=nlmeEnv)
    UpdateProgressMessages()
DEBUG_MASTER("generateNonparamSummary")
    generateNonparamSummary(jobList,unique_sorted_values,scenarioNames,jobsBaseDirectory)

    GlobalSummaryLine1=""
    assign("GlobalSummaryLine1", GlobalSummaryLine1, envir=nlmeEnv)
    GlobalSummaryLine1="Transfering data and loading results..."
    GlobalSummaryLine2=""
    assign("GlobalSummaryLine2", GlobalSummaryLine2, envir=nlmeEnv)
    GlobalSummaryLine3=""
    assign("GlobalSummaryLine3", GlobalSummaryLine3, envir=nlmeEnv)
    UpdateProgressMessages()
}

#
# Summarizes the results from profile estimation runs 
# generates Overall.csv and StatusWindows.txt
#
summarizeProfileEstimation <-function(localWorkingDir,jobList,control_lines){

    workflow_name=get("workflow_name",envir=nlmeEnv)
    unique_sorted_values=get("unique_sorted_values",envir=nlmeEnv)
    jobsDirectoryRoot=get("jobsDirectoryRoot",envir=nlmeEnv)
    jobsBaseDirectory=getBatchDirectoryLocation(jobsDirectoryRoot)
    UsingBatchTools=get("UsingBatchTools",envir=nlmeEnv)
    setwd(localWorkingDir)

    GlobalSummaryLine1=sprintf("Summarizing profile estimation %d runs",length(jobList))
    assign("GlobalSummaryLine1", GlobalSummaryLine1, envir=nlmeEnv)
    UpdateProgressMessages()
    GlobalSummaryLine2=""
    assign("GlobalSummaryLine2", GlobalSummaryLine2, envir=nlmeEnv)
    GlobalSummaryLine3=""
    assign("GlobalSummaryLine3", GlobalSummaryLine3, envir=nlmeEnv)

    fileNames=c()
    scenarioNames=c()
    for ( job in jobList) {
        fileName= getRunSuccessFilename(control_lines[job])
        # expand the filename to point to original run directory
        if ( jobsDirectoryRoot != "" ) {
            jobBaseIndx =job %% 100
            fileName=sprintf("%s/jobs/%02d/%d/%s",jobsBaseDirectory,jobBaseIndx,job,fileName)
        }
        else
            fileName=sprintf("%s.Job%d",fileName,job)
        fileNames=c(fileNames,fileName)
        sName = getScenarioName(control_lines[job])
        if ( ! sName %in% scenarioNames ) 
            scenarioNames=c(scenarioNames,sName)
    }

    GlobalSummaryLine1=sprintf("Summarizing profile estimation %d runs",length(jobList))
    assign("GlobalSummaryLine1", GlobalSummaryLine1, envir=nlmeEnv)
    GlobalSummaryLine2=sprintf("Generating Overall.csv")
    assign("GlobalSummaryLine2", GlobalSummaryLine2, envir=nlmeEnv)
    GlobalSummaryLine3=""
    assign("GlobalSummaryLine3", GlobalSummaryLine3, envir=nlmeEnv)
DEBUG_MASTER("generateProfileSummary")
    generateProfileSummary(fileNames,scenarioNames,TRUE)
    UpdateProgressMessages()

    filenames=c()
    progressfilenames=c()
    scenarioNames=c()
    for ( job in jobList ) {
        statusFile=sprintf("S_%03d.status",job)
        fileName="progress.txt"
        statusFile="progress.txt"
        if ( jobsDirectoryRoot != "" ) {
            jobBaseIndx =job %% 100
            progressFile=sprintf("%s/jobs/%02d/%d/%s",jobsBaseDirectory,jobBaseIndx,job,fileName)
            statusFile=sprintf("%s/S_%03d.status",jobsDirectoryRoot,as.integer(job))
        }
        else
            progressFile=sprintf("%s.Job%0d",fileName,job)
        filenames=c(filenames,statusFile)
        progressfilenames=c(progressfilenames,progressFile)
        sName = getScenarioName(control_lines[job])
        if ( ! sName %in% scenarioNames ) 
            scenarioNames=c(scenarioNames,sName)
    }
    GlobalSummaryLine1=sprintf("Summarizing profile estimation %d runs",length(jobList))
    assign("GlobalSummaryLine1", GlobalSummaryLine1, envir=nlmeEnv)
    GlobalSummaryLine2=sprintf("Generating StatusWindow.txt")
    assign("GlobalSummaryLine2", GlobalSummaryLine2, envir=nlmeEnv)
    GlobalSummaryLine3=""
    assign("GlobalSummaryLine3", GlobalSummaryLine3, envir=nlmeEnv)
    UpdateProgressMessages()
DEBUG_MASTER("generateStatusWindow")
    generateStatusWindow(filenames,progressfilenames,scenarioNames)

    GlobalSummaryLine1=""
    assign("GlobalSummaryLine1", GlobalSummaryLine1, envir=nlmeEnv)
    GlobalSummaryLine1="Transfering data and loading results..."
    GlobalSummaryLine2=""
    assign("GlobalSummaryLine2", GlobalSummaryLine2, envir=nlmeEnv)
    GlobalSummaryLine3=""
    assign("GlobalSummaryLine3", GlobalSummaryLine3, envir=nlmeEnv)
    UpdateProgressMessages()
}




#
#
# Read progress.txt file and return values in a list
#
readProgressDotTxt <-function(fileToRead) {

#retVal=list(nSubj=0,nObs=0,iterations=list(iteration=0,names=list(),values=list()))
retVal=list(nSubj=0,nObs=0,iterations=list())


    if ( file.exists(fileToRead) ) {
        lines=readLines(fileToRead)
        if ( length(lines) > 2 ) {
            nSubj=gsub(")","",unlist(strsplit(lines[1]," = "))[2],fixed=TRUE)
            nObs=gsub(")","",unlist(strsplit(lines[2]," = "))[2],fixed=TRUE)
            retVal$nSubj=as.integer(nSubj)
            retVal$nObs=as.integer(nObs)
            lines=lines[3:length(lines)]
            numIterations=length(grep("Iteration",lines))
            numTokens=as.integer(length(lines)/numIterations) - 1 
            retVal$iterations=list()
            for ( it in 1:numIterations ) {
                firstLine=(it - 1 ) * (numTokens + 1 )  + 1 
                names=list()
                values=list()
                for ( indx in 1:numTokens ) {
                    l=lines[firstLine-1 + indx]
                    values[indx]=gsub(")","",unlist(strsplit(l," = "))[2],fixed=TRUE)
                    names[indx]=unlist(strsplit(unlist(strsplit(l," = "))[1],"(",fixed=TRUE))[2]
                    if ( names[indx] == "LL" ){
                        logLikeIndex=indx
                        names[indx] = "-2LL"
                        values[indx] = as.double(values[indx]) * -2 
                    }
                }
                itlist=list(iteration=it,names=names,values=values)
                retVal$iterations[[it]]=itlist
            }
        } else {
            print(paste0("WARNING: File ",fileToRead," Does not have information"))
        }
    } else {
        print(paste("Warning: Unable to find ",fileToRead))
    }
    return(retVal)
}


#
# Concatenate progress.txt files and generates ConvergenceData.csv
#
generateConvergencFile <-function(jobList,unique_sorted_values,outputFileNames,scenarioNames){

    num_sort_columns = get("num_sort_columns",envir=.GlobalEnv)
    sort_column_names = get("sort_column_names",envir=.GlobalEnv)

    nxtScenario=1
    nxtSortKey=1
    OutputFile="ConvergenceData.csv"
    sortNames=""
    if ( num_sort_columns > 0 ) 
    for ( c in 1:num_sort_columns ) {
        nam = sort_column_names[c]
        sortNames=sprintf("%s%s,",sortNames,nam)
    }
    cat(sprintf("%sScenario,Iter,Parameter,Value",sortNames),file=OutputFile,sep="\n",append=FALSE)

    for ( job in jobList ) {
        fileToRead=outputFileNames[job]
        pData=readProgressDotTxt(fileToRead)
        if ( length(pData$iterations) > 0 ) {
            for ( p in 1:length(pData$iterations)) {
                sortValues=""
                if ( num_sort_columns > 0 ) 
                for ( c in 1:num_sort_columns ) {
                    nam = sort_column_names[c]
                    val = unlist(unique_sorted_values[[c]])[nxtSortKey]
                    if ( is.na(val ) )
                        val = unlist(unique_sorted_values[[c]])[nxtSortKey+1]
                    val = sub("^\\s+","",val)
                    sortValues=sprintf("%s%s,",sortValues,val)
                }
                for ( n in 1:length(pData$iterations[[p]]$names) ) {
                    if ( pData$iterations[[p]]$names[[n]] == "Iteration" ) 
                        next
                    cat(sprintf("%s%s,%s,%s,%s",sortValues,scenarioNames[nxtScenario],pData$iterations[[p]]$iteration,pData$iterations[[p]]$names[[n]],pData$iterations[[p]]$values[[n]]), file=OutputFile,sep="\n",append=TRUE)
                }
            }
            nxtScenario=nxtScenario+1
            if ( nxtScenario > length(scenarioNames) ){
                nxtScenario = 1 
                nxtSortKey = nxtSortKey + 1 
            }
        }
    }
}


concatenateInitEstimates <-function(jobList,unique_sorted_values,scenarioNames){

    num_sort_columns = get("num_sort_columns",envir=.GlobalEnv)
    sort_column_names = get("sort_column_names",envir=.GlobalEnv)
    jobsDirectoryRoot=get("jobsDirectoryRoot",envir=nlmeEnv)
    UsingBatchTools=get("UsingBatchTools",envir=nlmeEnv)
    jobsBaseDirectory=getBatchDirectoryLocation(jobsDirectoryRoot)

    resultsDataframe=NULL
    first=TRUE
    nxtScenario=1
    nxtSortKey=1
    missingFiles=TRUE
    for ( job in jobList ) {
        files=c()
        if ( jobsDirectoryRoot != "" ) {
            jobBaseIndx =job %% 100
            fileToRead=sprintf("%s/jobs/%02d/%d/%s",jobsBaseDirectory,jobBaseIndx,job,"iniest.csv")
        }
        else
            fileToRead=sprintf("iniest.csv.Job%0d",job)
        if ( file.exists(fileToRead) && file.info(fileToRead)$size != 0 ) {
            missingFiles=FALSE
            dataf=read.csv(fileToRead,header=TRUE)
            cnames=colnames(dataf)
            # remove the 1st 5 id columns
            for ( c in 1:5 ) {
                dataf[cnames[c]] = NULL
            }
            colnames(dataf)=c("parameter","init","low","high")
            dataf = cbind( "Scenario"=scenarioNames[nxtScenario] , dataf )
            if ( num_sort_columns > 0 )
            for ( c in num_sort_columns:1 ) {
                nam = sort_column_names[c]
                val = unlist(unique_sorted_values[[c]])[nxtSortKey]
                if ( is.na(val ) )
                    val = unlist(unique_sorted_values[[c]])[nxtSortKey+1]
                dataf = cbind( nam=val , dataf )
            
                cn=colnames(dataf)
                cn[1]=nam
                colnames(dataf)=cn
            }
            if ( first==TRUE )
                resultsDataframe= dataf
            else
                resultsDataframe= rbind(resultsDataframe,dataf)
            first=FALSE
        }
        nxtScenario=nxtScenario+1
        if ( nxtScenario > length(scenarioNames) ){
            nxtScenario = 1
            nxtSortKey = nxtSortKey + 1
        }
    }
    if ( missingFiles == FALSE )
        write.csv(resultsDataframe,file="iniest.csv",row.names=FALSE,quote=FALSE,na="")
}

getCovariateNames<-function(modelFileName){

    covariates=c()    
    lines=readLines(modelFileName)
    indxs=grep("covariate",lines,fixed=TRUE)
    for ( indx in indxs ) {
        line=lines[indx]
        covar=regmatches(line,regexpr("\\(.+\\)$",line))
        if ( length(covar) == 1 ) {
            covar=sub("\\(","",covar)
            covar=sub("\\)$","",covar)
            isCategorical=FALSE
            if ( length(grep("\\(*\\)",covar)) > 0 ) {
                isCategorical=TRUE
                covar=sub("\\(*\\)","",covar)
            }
        }
        token=c()
        token[eval(covar)]=isCategorical
        covariates=c(covariates,token)
    }
    return(covariates)
}

#
# read EtaCov.txt file and generates :
#
#  EtaCovariateCat.csv
#  EtaCovariate.csv
#
readEtaCovTxtFile <-function(jobList,unique_sorted_values,scenarioNames){

    num_sort_columns = get("num_sort_columns",envir=.GlobalEnv)
    sort_column_names = get("sort_column_names",envir=.GlobalEnv)
    jobsDirectoryRoot=get("jobsDirectoryRoot",envir=nlmeEnv)
    jobsDirectoryRoot=get("jobsDirectoryRoot",envir=nlmeEnv)
    jobsBaseDirectory=getBatchDirectoryLocation(jobsDirectoryRoot)
    UsingBatchTools=get("UsingBatchTools",envir=nlmeEnv)
    #
    # Figure out categorical and noncategorical covariate names
    #
    covariateNames=getCovariateNames("test.mdl")
    catnames=c()
    noncatnames=c()
    indx=1
    for ( c in names(covariateNames) ) {
        if ( covariateNames[[c]] == TRUE )
            catnames=c(catnames,c)
        else
            noncatnames=c(noncatnames,c)
        indx=indx+1
    }

    etaCovTable=c()
    etaCovariateTable=c()
    etaCovariateCatTable=c()
    sc=1
    uIndex=1
    filesAreMissing=TRUE
    for ( job in jobList ) {
            rDumpFile="dmp"
        if ( jobsDirectoryRoot != "" ) {
            jobBaseIndx =job %% 100
            fileToRead=sprintf("%s/jobs/%02d/%d/%s",jobsBaseDirectory,jobBaseIndx,job,"EtaCov.txt")
            nonParamFile=sprintf("%s/jobs/%02d/%d/%s",jobsBaseDirectory,jobBaseIndx,job,"etameansnp.asc")
            rDumpFile=sprintf("%s/jobs/%02d/%d/%s",jobsBaseDirectory,jobBaseIndx,job,"dmp")
        }
        else {
            fileToRead=sprintf("EtaCov.txt.Job%0d",job)
            nonParamFile=sprintf("etameansnp.asc.Job%0d",job)
        }
        if ( file.exists(fileToRead) && file.info(fileToRead)$size != 0 ) {
            npDataPresent=FALSE
            if (file.exists(nonParamFile)&&file.info(nonParamFile)$size != 0){
                npData=read.table(nonParamFile)
                npDataPresent=TRUE
            }
            filesAreMissing=FALSE
            data = read.csv(fileToRead,header=FALSE)
            # get read of the 1st 4 columns
            data[1] = NULL
            data[1] = NULL
            data[1] = NULL
            data[1] = NULL
            # Columns to be added
            cnames=c()
            if ( num_sort_columns > 0 ) 
                for ( c in 1:num_sort_columns )  {
                    cnames=c(cnames,sort_column_names[c])
                    nam = sort_column_names[c]
                    val = unlist(unique_sorted_values[[c]])[uIndex]
                    if ( is.na(val ) )
                        val = unlist(unique_sorted_values[[c]])[uIndex+1]
                    val = sub("^\\s+","",val)
#                    if ( c == 1 )
#                        rDumpFile=sprintf("%s%s",rDumpFile,val)
#                    else
#                        rDumpFile=sprintf("%s-%s",rDumpFile,val)
                }
            rDumpFile=sprintf("%s.txt",rDumpFile)
            newFileToSource=shrinkDmpDotTxt(rDumpFile)
            source(newFileToSource)
            # Standard column names from phoenix
colnames(data)
            colnames(data)=c("ID","EtaName","CovrName","Eta","Covr")
            if ( npDataPresent ) {
                randomEffectNames=unlist(dimnames(dmp.txt$omega)[1])
                colnames(npData)=randomEffectNames
                uniqueIds=unique(data[1])
                npData$ID=uniqueIds[[1]]
                npData=melt(npData,id=(c("ID")))
#                npData$ID=seq.int(nrow(npData))
                colnames(npData)=c("ID","EtaName","NPEta")
                data=merge(data,npData,by=c("ID","EtaName"))
                data=data[order(data$ID),]
            }
            uniqueIds=unique(data[1])
            # handle multiple scenario jobs
            for ( id in unlist(uniqueIds) ) {
                if ( is.na(id ) )
                    next
                rowcol=c()
                # Add all the sort columns to the row
                if ( num_sort_columns > 0 ) 
                for ( c in 1:num_sort_columns ) {
                    nam = sort_column_names[c]
                    val = unlist(unique_sorted_values[[c]])[uIndex]
                    if ( is.na(val ) )
                        val = unlist(unique_sorted_values[[c]])[uIndex+1]
                    val = sub("^\\s+","",val)
                    rowcol[eval(nam)]= val
                }
                # Add the empty scenario and ID column
                rowcol["Scenario"]=scenarioNames[sc]
                rowcol["ID"]=id
                # Select rows for the current ID
                sub = subset(data, ID == id)
                # Set the values for sort columns
                for ( r in 1:nrow(sub)){
                    row=sub[r,]
                    ncols = (length(sub)-1)/2
                    for ( c in 2:(2 + ncols -1)) {
                        nam=as.character((unlist(sub[[c]])[r]))
                        val=as.character((unlist(sub[[c+ncols]])[r]))
                        rowcol[[eval(nam)]]=val
                    }
                }
                # Add the row to EtaCov.csv
                etaCovTable=rbind(etaCovTable,rowcol)
            }
            # Add empty scenario column to data table
            data=cbind("Scenario"=scenarioNames[sc],data)
            if ( num_sort_columns > 0 )
            for ( c in 1:num_sort_columns ) {
                nam = sort_column_names[c]
                val = unlist(unique_sorted_values[[c]])[uIndex]
                if ( is.na(val ) )
                    val = unlist(unique_sorted_values[[c]])[uIndex+1]
                val = sub("^\\s+","",val)
                data=cbind(nam=val,data)
                cn=colnames(data)
                cn[1]=nam
                colnames(data)=cn
            }
            etaCovariateTable=rbind(etaCovariateTable,subset(data, CovrName %in% noncatnames))
            etaCovariateCatTable=rbind(etaCovariateCatTable,subset(data, CovrName %in% catnames))
        }
        sc = sc + 1 
        if ( sc > length(scenarioNames) )  {
            sc = 1 
            uIndex = uIndex + 1 
        }
    }
    # 
    # Reorder the columns so sort columns appear 1st
    #
    if ( filesAreMissing == FALSE ) {
        ids=c()
        allids=c(1:length(colnames(etaCovTable)))
        lastid=as.integer(num_sort_columns) + 2
        begids=c(1:lastid)
        tempTable=etaCovTable[,c(1:lastid)]
        for ( cname in names(covariateNames) ) {
            covarPos = grep(cname,colnames(etaCovTable))
            ids=c(ids,covarPos)
            begids=c(begids,covarPos)
        }
        remainingIds=allids[-begids]
        tempTable=cbind(tempTable,etaCovTable[,c(ids,remainingIds)])
        etaCovTable=tempTable
#
# Write EtaCov.csv
#
        write.csv(etaCovTable,file="EtaCov.csv",row.names=FALSE,quote=FALSE)
#
# Reorder EtaCovariateTble so you have Covariate column 1st and sort by it
#
        covarPos = grep("CovrName",colnames(etaCovariateTable))
        if ( npDataPresent )
            etaCovariateTable=etaCovariateTable[,c(1:(covarPos-2),covarPos,(covarPos-1),covarPos+2,covarPos+1,covarPos+3)]
        else
            etaCovariateTable=etaCovariateTable[,c(1:(covarPos-2),covarPos,(covarPos-1),covarPos+2,covarPos+1)]
        etaCovariateTable=etaCovariateTable[order(etaCovariateTable$CovrName),]
        covarPos = grep("CovrName",colnames(etaCovariateCatTable))
        if ( npDataPresent )
            etaCovariateCatTable=etaCovariateCatTable[,c(1:(covarPos-2),covarPos,(covarPos-1),covarPos+2,covarPos+1,covarPos+3)]
        else
            etaCovariateCatTable=etaCovariateCatTable[,c(1:(covarPos-2),covarPos,(covarPos-1),covarPos+2,covarPos+1)]
        if ( num_sort_columns > 0 ) {
            nam=sort_column_names[1]
            etaCovariateCatTable=etaCovariateCatTable[order(etaCovariateCatTable[nam],etaCovariateCatTable$Scenario,etaCovariateCatTable$ID,etaCovariateCatTable$EtaName),]
        }
#
# Write EtaCovariate.csv
#
        write.csv(etaCovariateTable,file="EtaCovariate.csv",row.names=FALSE,quote=FALSE)
#
# Write EtaCovariateCat.csv
#
        write.csv(etaCovariateCatTable,file="EtaCovariateCat.csv",row.names=FALSE,quote=FALSE)
    }
}





#
# read StrCov.txt file and generates :
#
#  StrCovariateCat.csv
#  StrCovariate.csv
#
readStrCovTxtFile <-function(jobList,unique_sorted_values,scenarioNames){

    num_sort_columns = get("num_sort_columns",envir=.GlobalEnv)
    sort_column_names = get("sort_column_names",envir=.GlobalEnv)
    jobsDirectoryRoot=get("jobsDirectoryRoot",envir=nlmeEnv)
    jobsBaseDirectory=getBatchDirectoryLocation(jobsDirectoryRoot)
    UsingBatchTools=get("UsingBatchTools",envir=nlmeEnv)

    #
    # Figure out categorical and noncategorical covariate names
    #
    covariateNames=getCovariateNames("test.mdl")
    catnames=c()
    noncatnames=c()
    indx=1
    for ( c in names(covariateNames) ) {
        if ( covariateNames[[c]] == TRUE )
            catnames=c(catnames,c)
        else
            noncatnames=c(noncatnames,c)
        indx=indx+1
    }

    strCovTable=c()
    strCovariateTable=c()
    strCovariateCatTable=c()
    sc=1
    uIndex=1
    filesAreMissing=TRUE
    for ( job in jobList ) {
        if ( jobsDirectoryRoot != "" ) {
            jobBaseIndx =job %% 100
            fileToRead=sprintf("%s/jobs/%02d/%d/%s",jobsBaseDirectory,jobBaseIndx,job,"StrCov.txt")
        }
        else
            fileToRead=sprintf("StrCov.txt.Job%0d",job)
        if ( file.exists(fileToRead) && file.info(fileToRead)$size != 0 ) {
            filesAreMissing=FALSE
            data = read.csv(fileToRead,header=FALSE,strip.white=TRUE)
            # get read of the 1st 4 columns
            data[1] = NULL
            data[1] = NULL
            data[1] = NULL
            data[1] = NULL
            # Columns to be added
            cnames=c()
            if ( num_sort_columns > 0 ) 
            for ( c in 1:num_sort_columns ) 
                cnames=c(cnames,sort_column_names[c])
            # Standard column names from phoenix
            colnames(data)=c("ID","StrName","CovrName","Str","Covr")
    
            uniqueIds=s=unique(data[1])
            # handle multiple scenario jobs
            for ( id in unlist(uniqueIds) ) {
                if ( is.na(id ) )
                    next
                rowcol=c()
                # Add all the sort columns to the row
                if ( num_sort_columns > 0 ) 
                for ( c in 1:num_sort_columns ) {
                    nam = sort_column_names[c]
                    val = unlist(unique_sorted_values[[c]])[uIndex]
                    if ( is.na(val ) )
                        val = unlist(unique_sorted_values[[c]])[uIndex+1]
                    val = sub("^\\s+","",val)
                    rowcol[eval(nam)]= val
                }
                # Add the empty scenario and ID column
                rowcol["Scenario"]=scenarioNames[sc]
                rowcol["ID"]=id
                # Select rows for the current ID
                sub = subset(data, ID == id)
                # Set the values for sort columns
                for ( r in 1:nrow(sub)){
                    row=sub[r,]
                    ncols = (length(sub)-1)/2
                    for ( c in 2:(2 + ncols -1)) {
                        nam=as.character((unlist(sub[[c]])[r]))
                        val=as.character((unlist(sub[[c+ncols]])[r]))
                        rowcol[[eval(nam)]]=val
                    }
                }
                # Add the row to Cov.csv
                strCovTable=rbind(strCovTable,rowcol)
            }
            # Add empty scenario column to data table
            data=cbind("Scenario"=scenarioNames[sc],data)
            if ( num_sort_columns > 0 ) 
            for ( c in 1:num_sort_columns ) {
                    nam = sort_column_names[c]
                    val = unlist(unique_sorted_values[[c]])[uIndex]
                    if ( is.na(val ) )
                        val = unlist(unique_sorted_values[[c]])[uIndex+1]
                    val = sub("^\\s+","",val)
                    data=cbind(nam=val,data)
                    cn=colnames(data)
                    cn[1]=nam
                    colnames(data)=cn
            }
            strCovariateTable=rbind(strCovariateTable,subset(data, CovrName %in% noncatnames))
            strCovariateCatTable=rbind(strCovariateCatTable,subset(data, CovrName %in% catnames))
        }
        sc = sc + 1 
        if ( sc > length(scenarioNames) )  {
            sc = 1 
            uIndex = uIndex + 1 
        }
    }
    # 
    # Reorder the columns so sort columns appear 1st
    #
    if ( filesAreMissing == FALSE ) {
        ids=c()
        allids=c(1:length(colnames(strCovTable)))
        lastid=as.integer(num_sort_columns) + 2
        begids=c(1:lastid)
        tempTable=strCovTable[,c(1:lastid)]
        for ( cname in names(covariateNames) ) {
            covarPos = grep(cname,colnames(strCovTable))
            ids=c(ids,covarPos)
            begids=c(begids,covarPos)
        }
        remainingIds=allids[-begids]
        tempTable=cbind(tempTable,strCovTable[,c(ids,remainingIds)])
        strCovTable=tempTable
#
# Write StrCov.csv
#
#    write.csv(strCovTable,file="StrCov.csv",row.names=FALSE,quote=FALSE)
#
# Reorder CovariateTble so you have Covariate column 1st and sort by it
#

        covarPos = grep("CovrName",colnames(strCovariateTable))
        strCovariateTable=strCovariateTable[,c(1:(covarPos-2),covarPos,(covarPos-1),covarPos+2,covarPos+1)]
        strCovariateTable=strCovariateTable[order(strCovariateTable$CovrName),]

        covarPos = grep("CovrName",colnames(strCovariateCatTable))
        strCovariateCatTable=strCovariateCatTable[,c(1:(covarPos-2),covarPos,(covarPos-1),covarPos+2,covarPos+1)]
#    strCovariateCatTable=strCovariateCatTable[order(strCovariateCatTable$ID,strCovariateCatTable$StrName),]
        if ( num_sort_columns > 0 ) {
            nam=sort_column_names[1]
            strCovariateCatTable=strCovariateCatTable[order(strCovariateCatTable[nam],strCovariateCatTable$Scenario,strCovariateCatTable$ID,strCovariateCatTable$StrName),]
        }
#
# Write StrCovariate.csv
#
        write.csv(strCovariateTable,file="StrCovariate.csv",row.names=FALSE,quote=FALSE)
#
# Write StrCovariateCat.csv
#
        write.csv(strCovariateCatTable,file="StrCovariateCat.csv",row.names=FALSE,quote=FALSE)
    }
}



renameDmpDotTxtFiles <-function(jobList,unique_sorted_values,scenarioNames){

    unique_sorted_values = get("unique_sorted_values",envir=.GlobalEnv)
    num_sort_columns = get("num_sort_columns",envir=.GlobalEnv)
    sort_column_names = get("sort_column_names",envir=.GlobalEnv)
    jobsDirectoryRoot=get("jobsDirectoryRoot",envir=nlmeEnv)
    jobsBaseDirectory=getBatchDirectoryLocation(jobsDirectoryRoot)
    UsingBatchTools=get("UsingBatchTools",envir=nlmeEnv)

    sc=1
    uIndex=1
    filesAreMissing=FALSE
    for ( job in jobList ) {
        files=c()
        if ( jobsDirectoryRoot != "" ) {
            jobBaseIndx =job %% 100
            fromFile=sprintf("%s/jobs/%02d/%d/%s",jobsBaseDirectory,jobBaseIndx,job,"dmp.txt")
        }
        else
            fromFile=sprintf("dmp.txt.Job%0d",job)

        postfix=""
        first=TRUE
        if ( num_sort_columns > 0 ) 
        for ( c in 1:num_sort_columns ) {
            nam = sort_column_names[c]
            val = unlist(unique_sorted_values[[c]])[uIndex]
            if ( is.na(val ) )
                val = unlist(unique_sorted_values[[c]])[uIndex+1]
            val = sub("^\\s+","",val)
            if ( first == TRUE )
                postfix=val
            else
                postfix=sprintf("%s-%s",postfix,val)
            first=FALSE
        }
        toFile=sprintf("dmp%s.txt",postfix)
        if ( file.exists(fromFile) ) 
#            file.rename(fromFile,toFile)
            file.copy(fromFile,toFile)
        sc = sc + 1 
        if ( sc > length(scenarioNames) )  {
            sc = 1 
            uIndex = uIndex + 1 
        }
    }
    for ( job in jobList ) {
        files=c()
        fromFile=sprintf("dmp.txt.Job%0d",job)

        postfix=""
        first=TRUE
        if ( num_sort_columns > 0 ) 
        for ( c in 1:num_sort_columns ) {
            nam = sort_column_names[c]
            val = unlist(unique_sorted_values[[c]])[job]
            if ( is.na(val ) )
                val = unlist(unique_sorted_values[[c]])[job+1]
            val = sub("^\\s+","",val)
            if ( first == TRUE )
                postfix=val
            else
                postfix=sprintf("%s-%s",postfix,val)
            first=FALSE
        }
        toFile=sprintf("dmp%s.txt",postfix)
        if ( file.exists(fromFile) ) 
            file.copy(fromFile,toFile)

    }
}





generateGenericTable<-function(jobList,unique_sorted_values,scenarioNames, inputPrefix, outputName,colNames,optionalMergFile=""){

    unique_sorted_values = get("unique_sorted_values",envir=.GlobalEnv)
    num_sort_columns = get("num_sort_columns",envir=.GlobalEnv)
    sort_column_names = get("sort_column_names",envir=.GlobalEnv)
    jobsDirectoryRoot=get("jobsDirectoryRoot",envir=nlmeEnv)
    jobsBaseDirectory=getBatchDirectoryLocation(jobsDirectoryRoot)
    UsingBatchTools=get("UsingBatchTools",envir=nlmeEnv)
    resultsDataframe=NULL
    first=TRUE
    sc=1
    uIndex=1
    missingFile=TRUE
    for ( job in jobList ) {
        files=c()
        optionalFileToRead=""
        rDumpFile="dmp"
        if ( jobsDirectoryRoot != "" ) {
            jobBaseIndx =job %% 100
            fileToRead=sprintf("%s/jobs/%02d/%d/%s",jobsBaseDirectory,jobBaseIndx,job,inputPrefix)
            if ( optionalMergFile != "" )  {
                optionalFileToRead=sprintf("%s/jobs/%02d/%d/%s",jobsBaseDirectory,jobBaseIndx,job,optionalMergFile)
                rDumpFile=sprintf("%s/jobs/%02d/%d/%s",jobsBaseDirectory,jobBaseIndx,job,rDumpFile)
            }
        }
        else {
            fileToRead = sprintf("%s.Job%0d",inputPrefix,job)
            optionalFileToRead = sprintf("%s.Job%0d",optionalMergFile,job)
        }
        if ( file.exists(fileToRead) && file.info(fileToRead)$size != 0 ) {
            dataf=read.csv(fileToRead,header=TRUE)
            cnames=colnames(dataf)
            # remove the 1st 4 id columns
            for ( c in 1:4 ) {
                dataf[cnames[c]] = NULL
            }
            ncols=length(colnames(dataf))
            colnames(dataf)=colNames[1:ncols]

            if ( optionalMergFile != "" && file.exists(optionalFileToRead) && file.info(optionalFileToRead)$size != 0 ) {
#                if ( num_sort_columns > 0 ) 
#                    for ( c in 1:num_sort_columns )  {
#                        cnames=c(cnames,sort_column_names[c])
#                        nam = sort_column_names[c]
#                        val = unlist(unique_sorted_values[[c]])[uIndex]
#                        if ( is.na(val ) )
#                            val = unlist(unique_sorted_values[[c]])[uIndex+1]
#                        val = sub("^\\s+","",val)
#                        if ( c == 1 )
#                            rDumpFile=sprintf("%s%s",rDumpFile,val)
#                        else
#                            rDumpFile=sprintf("%s-%s",rDumpFile,val)
#                    }
                rDumpFile=sprintf("%s.txt",rDumpFile)
                newFileToSource=shrinkDmpDotTxt(rDumpFile)
                source(newFileToSource)
                randomEffectNames=unlist(dimnames(dmp.txt$omega)[1])

                optionalDataf=read.table(optionalFileToRead)
                colnames(optionalDataf)=randomEffectNames
                optionalDataf$ID=seq.int(nrow(optionalDataf))
                optionalDataf=melt(optionalDataf,id=(c("ID")))
                colnames(optionalDataf)=c("ID","Eta1Name","NPEta1")
                dataf=merge(dataf,optionalDataf,by=c("ID","Eta1Name"))
                colnames(optionalDataf)=c("ID","Eta2Name","NPEta2")
                dataf=merge(dataf,optionalDataf,by=c("ID","Eta2Name"))
                dataf=dataf[order(dataf$ID),]
            }
            ncols=length(colnames(dataf))
            colnames(dataf)=colNames[1:ncols]
            if ( nrow(dataf) > 0 ) {
                missingFile=FALSE
                # Add empty scenario column to data table
                dataf=cbind("Scenario"=scenarioNames[sc],dataf)
                if ( num_sort_columns > 0 ) 
                for ( c in 1:num_sort_columns ) {
                    nam = sort_column_names[c]
                    val = unlist(unique_sorted_values[[c]])[uIndex]
                    if ( is.na(val ) )
                        val = unlist(unique_sorted_values[[c]])[uIndex+1]
                    val = sub("^\\s+","",val)
                    dataf=cbind(nam=val,dataf)
                    cn=colnames(dataf)
                    cn[1]=nam
                    colnames(dataf)=cn
                }

                if ( first==TRUE )
                    resultsDataframe= dataf
                else
                    resultsDataframe= rbind(resultsDataframe,dataf)
                first=FALSE
                sc = sc + 1 
                if ( sc > length(scenarioNames) )  {
                    sc = 1 
                    uIndex = uIndex + 1 
                }
            }
        }
    }
    if ( missingFile == FALSE ) 
        write.csv(resultsDataframe,file=outputName,row.names=FALSE,quote=FALSE,na="")
}

generateEtaEtaTable <-function(jobList,unique_sorted_values,scenarioNames){

require(reshape)
    unique_sorted_values = get("unique_sorted_values",envir=.GlobalEnv)
    num_sort_columns = get("num_sort_columns",envir=.GlobalEnv)
    sort_column_names = get("sort_column_names",envir=.GlobalEnv)

    generateGenericTable(jobList,unique_sorted_values,scenarioNames, "EtaEta.txt", "EtaEta.csv",c("ID", "Eta1Name", "Eta2Name", "Eta1", "Eta2","NPEta1","NPEta2"),"etameansnp.asc")
}



collateTables<-function(jobList,unique_sorted_values,scenarioNames, inputPrefix, outputName){
    unique_sorted_values = get("unique_sorted_values",envir=.GlobalEnv)
    num_sort_columns = get("num_sort_columns",envir=.GlobalEnv)
    sort_column_names = get("sort_column_names",envir=.GlobalEnv)
    jobsDirectoryRoot=get("jobsDirectoryRoot",envir=nlmeEnv)
    jobsBaseDirectory=getBatchDirectoryLocation(jobsDirectoryRoot)
    UsingBatchTools=get("UsingBatchTools",envir=nlmeEnv)
    resultsDataframe=NULL
    first=TRUE
    sc=1
    uIndex=1
    missingFile=TRUE
    for ( job in jobList ) {
        files=c()
        if ( jobsDirectoryRoot != "" ) {
            jobBaseIndx =job %% 100
            fileToRead=sprintf("%s/jobs/%02d/%d/%s",jobsBaseDirectory,jobBaseIndx,job,inputPrefix)
        }
        else
            fileToRead=sprintf("%s.Job%0d",inputPrefix,job)
        if ( file.exists(fileToRead) && file.info(fileToRead)$size != 0 ) {
            dataf=read.csv(fileToRead,header=TRUE)
            cnames=colnames(dataf)
            cn=colnames(dataf)
            cn[1]="repl"
            colnames(dataf)=cn

            if ( nrow(dataf) > 0 ) {
            # Add empty scenario column to data table
                dataf=cbind("Scenario"=scenarioNames[sc],dataf)
                if ( num_sort_columns > 0 ) 
                for ( c in 1:num_sort_columns ) {
                    nam = sort_column_names[c]
                    val = unlist(unique_sorted_values[[c]])[uIndex]
                    if ( is.na(val ) )
                        val = unlist(unique_sorted_values[[c]])[uIndex+1]
                    val = sub("^\\s+","",val)
                    dataf=cbind(nam=val,dataf)
                    cn=colnames(dataf)
                    cn[1]=nam
                    colnames(dataf)=cn
                }
                if ( first==TRUE )
                    resultsDataframe= dataf
                else
                    resultsDataframe= rbind(resultsDataframe,dataf)
                first=FALSE
                missingFile=FALSE
            }
        }
        sc = sc + 1 
        if ( sc > length(scenarioNames) )  {
            sc = 1 
            uIndex = uIndex + 1 
        }
    }
    if ( missingFile == FALSE ) 
        write.csv(resultsDataframe,file=outputName,row.names=FALSE,quote=FALSE,na="")

}


parseResiduals <- function(fileName ) {
    data1 <- readLines(con <- file(fileName))
    cn=data1[1]
    close(con)

    data2=lapply(data1[-1],gsub, pattern="   \"\"   \"\"   \"\"   \"\" ",replacement ="\t\t\t\t",fixed=TRUE)
    data=lapply(data2,gsub, pattern="\"",replacement ="",fixed=TRUE)
    records <- sapply(data, strsplit, split="\t")
    dataFrame <- data.frame(t(sapply(records,c)))
    cn1=strsplit(cn,split="\t")
    cn2=sapply(cn1, sub, pattern="^\\s+",replacement="")
    colnames(dataFrame) = unlist(cn2)
    rownames(dataFrame) <- 1: nrow(dataFrame)
    rm(data)
    rm(data1)
    rm(data2)
    rm(records)
    rm(cn)
    rm(cn2)
    rm(cn1)
    gc()
    return(as.data.frame(dataFrame,stringsAsFactors = FALSE))
}


oldmyread <- function(fileName ) {
    data1 <- readLines(con <- file(fileName))
    cn=data1[1]
    close(con)
    data=lapply(data1[-1],gsub, pattern="   \"\"   \"\"   \"\"   \"\" ",replacement ="\t\t\t\t",fixed=TRUE)
    records <- sapply(data, strsplit, split="\t")
    dataFrame <- data.frame(t(sapply(records,c)))
    cn1=strsplit(cn,split="\t")
    cn2=sapply(cn1, sub, pattern="^\\s+",replacement="")
    colnames(dataFrame) = unlist(cn2)
    rownames(dataFrame) <- 1: nrow(dataFrame)
    return(as.data.frame(dataFrame,stringsAsFactors = FALSE))
}


figureOutDmpFileLocation <-function(num_sort_columns, sort_column_names, unique_sorted_values, scenarioIndex, job, jobsDirectoryRoot,jobsBaseDirectory)
{
    rDumpFile="dmp"
    if ( num_sort_columns > 0 )  {
        for ( c in 1:num_sort_columns ) {
            nam = sort_column_names[c]
            val = unlist(unique_sorted_values[[c]])[scenarioIndex]
            if ( is.na(val ) )
                val = unlist(unique_sorted_values[[c]])[scenarioIndex+1]
            val = sub("^\\s+","",val)
            if ( c == 1 ) 
                rDumpFile=sprintf("%s%s",rDumpFile,val)
            else
                rDumpFile=sprintf("%s-%s",rDumpFile,val)
        }
    } else 
    {
        if ( jobsDirectoryRoot != "" ) {
            jobBaseIndx =job %% 100
            rDumpFile=sprintf("%s/jobs/%02d/%d/%s",jobsBaseDirectory,jobBaseIndx,job,rDumpFile)
        }
        else
            rDumpFile=sprintf("%s.%0d.Job%0d",rDumpFile,job,job)
    }
    rDumpFile=sprintf("%s.txt",rDumpFile)
    return(rDumpFile)
}

generateResiduals<-function(jobList,unique_sorted_values,scenarioNames,jobsBaseDirectory){

    outputName="residuals.csv"
    resid2OutputName="resid2.csv"
    unique_sorted_values = get("unique_sorted_values",envir=.GlobalEnv)
    num_sort_columns = get("num_sort_columns",envir=.GlobalEnv)
    sort_column_names = get("sort_column_names",envir=.GlobalEnv)
    jobsDirectoryRoot=get("jobsDirectoryRoot",envir=nlmeEnv)
    resultsDataframe=NULL
    first=TRUE
    sc=1
    uIndex=1
    missingFile=TRUE
    control_lines=get("control_lines",envir=nlmeEnv)
    for ( job in jobList ) {
        files=c()
        rDumpFile = figureOutDmpFileLocation(num_sort_columns, sort_column_names, unique_sorted_values, uIndex, job, jobsDirectoryRoot,jobsBaseDirectory)
        if ( file.exists(rDumpFile) && file.info(rDumpFile)$size != 0 ) {
            jobBaseIndx =job %% 100
            fName=getRunSuccessFilename(control_lines[job])
            fileToRead=sprintf("%s/jobs/%02d/%d/%s",jobsBaseDirectory,jobBaseIndx,job,fName)
            newFileToRead=reformatResidualsFile(fileToRead)
            dataf=parseResiduals(newFileToRead)
            cnames=colnames(dataf)
            # remove the 1st 4 id columns
            for ( c in 1:4 ) {
                dataf[cnames[c]] = NULL
            }

            if ( nrow(dataf) > 0 ) {
            # Add the scenario name
                dataf=cbind("Scenario"=scenarioNames[sc],dataf)
                if ( num_sort_columns > 0 )  
                for ( c in 1:num_sort_columns ) {
                    nam = sort_column_names[c]
                    val = unlist(unique_sorted_values[[c]])[uIndex]
                    if ( is.na(val ) )
                        val = unlist(unique_sorted_values[[c]])[uIndex+1]
                    val = sub("^\\s+","",val)
                    dataf=cbind(nam=val,dataf)
                    cn=colnames(dataf)
                    cn[1]=nam
                    colnames(dataf)=cn
                }
                # Lets do resid2 logic
                resid=dataf
                resid$ID5=as.numeric(levels(resid$ID5))[resid$ID5]
                resid$CdfDV=NULL
                resid$CdfPCWRES=NULL
                resid$PCWRES=NULL
                resid$CWRES=NULL
                resid$IWRES=NULL
                resid$WRES=NULL
                resid$Weight=NULL
                resid$PREDSE=NULL
                agg=aggregate(cbind(PRED,DV)~ID5+IVAR, data=resid, paste, collapse="," )
                agg=agg[order(agg$ID5),]
                preds=do.call(rbind,strsplit(agg$PRED,","))
                dvs=do.call(rbind,strsplit(agg$DV,","))
                for ( c in 1:ncol(preds)) {
                    if ( c == 1 )
                        ds=cbind(preds[,c],dvs[,c])
                    else
                        ds=cbind(ds,preds[,c],dvs[,c])
                }
                agg$PRED=NULL
                agg$DV=NULL
                resid=resid[!duplicated(resid[,c("ID5","IVAR")]),]
                newDataf=cbind(agg,resid$TAD,ds,resid$WhichDose,resid$WhichReset)
                cn=colnames(newDataf)
                cn[1]="ID"
                cn[2]="IVAR"
                cn[3]="TAD"
                cn[4]="IPRED"
                cn[5]="DV"
                for ( n in seq(2,ncol(preds))) {
                    cn[(n-1)*2+4]=sprintf("IPRED%d",n)
                    cn[(n-1)*2+5]=sprintf("DV%d",n)
                }
                cn[ncol(preds)*2 +4]="TADSeq"
                cn[ncol(preds)*2 +5]="ResetSeq"
                colnames(newDataf)=cn

                cnames=colnames(dataf)
                for ( i in 1:length(cnames)){
                    if ( cnames[i] == "ID5" )
                        cnames[i] = "id"
                }
                colnames(dataf)=cnames

                if ( first==TRUE ) {
                    resultsDataframe= dataf
                    resid2Dataframe= resid
                }
                else {
                    resultsDataframe= rbind(resultsDataframe,dataf)
                    resid2Dataframe= rbind(resid2Dataframe,resid)
                }
                first=FALSE
                missingFile = FALSE 
            }
        }
        sc = sc + 1 
        if ( sc > length(scenarioNames) )  {
            sc = 1 
            uIndex = uIndex + 1 
        }
        rm(cn)
        rm(agg)
        rm(preds)
        rm(dvs)
        rm(newDataf)
        rm(dataf)
        rm(resid)
        rm(cnames)
        gc()
    }
    if ( missingFile == FALSE )  {
        resultsDataframe["PCWRES"]=NULL
        resultsDataframe["CdfPCWRES"]=NULL
        write.csv(resultsDataframe,file=outputName,row.names=FALSE,quote=FALSE,na="")
        write.csv(resid2Dataframe,file=resid2OutputName,row.names=FALSE,quote=FALSE,na="")
    }
    rm(resultsDataframe)
    rm(resid2Dataframe)
    gc()
}



generateNonparamSummary<-function(jobList,unique_sorted_values,scenarioNames,jobsBaseDirectory){

    outputName="nonParSupportResult.csv"
    outputStackedName="nonParStackedResult.csv"
    outputEtaName="nonParEtaResult.csv"
    outputOverallName="nonParOverallResult.csv"
    inputName="nparsupport.asc"
    inputEtaName="etameansnp.asc"

    unique_sorted_values = get("unique_sorted_values",envir=.GlobalEnv)
    num_sort_columns = get("num_sort_columns",envir=.GlobalEnv)
    sort_column_names = get("sort_column_names",envir=.GlobalEnv)
    jobsDirectoryRoot=get("jobsDirectoryRoot",envir=nlmeEnv)
    resultsDataframe=NULL
    resultsEtaDataframe=NULL
    resultsStackedDataframe=NULL
    resultsOverallDataframe=NULL
    first=TRUE
    sc=1
    uIndex=1
    missingFile=TRUE
    control_lines=get("control_lines",envir=nlmeEnv)
    for ( job in jobList ) {
        files=c()
        rDumpFile = figureOutDmpFileLocation(num_sort_columns, sort_column_names, unique_sorted_values, uIndex, job, jobsDirectoryRoot,jobsBaseDirectory)
        if ( file.exists(rDumpFile) && file.info(rDumpFile)$size != 0 ) {
            newFileToRead=shrinkDmpDotTxt(rDumpFile)
            source(newFileToRead)
            jobBaseIndx =job %% 100
            fileToRead=sprintf("%s/jobs/%02d/%d/%s",jobsBaseDirectory,jobBaseIndx,job,inputName)
            etafileToRead=sprintf("%s/jobs/%02d/%d/%s",jobsBaseDirectory,jobBaseIndx,job,inputEtaName)
            if ( file.exists(fileToRead) && file.info(fileToRead)$size != 0 ) {
                dataf=read.table(fileToRead)
                etaDataf=read.table(etafileToRead)

                if ( nrow(dataf) > 0 ) {
                    randomEffectNames=unlist(dimnames(dmp.txt$omega)[1])
                    colnames(etaDataf)=randomEffectNames
                    supportColnames=c("p",randomEffectNames)
                    etaDataf=cbind("Id"=1:nrow(etaDataf),etaDataf)
                    stacked=list()
                    nEffects=length(randomEffectNames)
                    meanDataf=dataf
                    means=c()
                    for ( ndx in 1:nEffects ) {
                        meanDataf = dataf
                        meanCol = paste0("Mean",ndx)
                        etaCol = paste0("V",(ndx+1))
                        meanDataf[meanCol]=dataf$V1 * dataf[etaCol]
                        mean = sum(meanDataf[meanCol])
                        means = c(means,mean)
                    }
                    covar=list()
                    for ( i in 1:(nEffects*nEffects) ) {
                        covar[[i]] = 0.0 
                    }
                    for ( row in 1:nrow(meanDataf) ){
                        prob = dataf[row,1]
                        for ( j in 1:nEffects ) {
                            mean1=means[j]
                            val1 = dataf[row,(j+1)]
                            for ( k in 1:nEffects) {
                                mean2=means[k]
                                val2 = dataf[row,(k+1)]
                                dcov = ( val1 - mean1 ) * ( val2 - mean2)
                                covar[[((j -1)* nEffects + k )]] = covar[[((j-1) * nEffects + k )]] + dcov *prob 
                            }
                        }
                    }
                    cnames=c("type")
                    col1=c("Mean")
                    for ( ndx in 1:nEffects ) {
                        col1=c( col1,"omega")
                        cnames=c(cnames,randomEffectNames[ndx])
                    }
                    overallDf = data.frame(col1)
                    for ( i in 1:nEffects ) {
                        coln=c(means[i])
                        for ( j in 1:nEffects ) {
                            coln=c(coln,covar[[(j-1)*nEffects + i]])
                        }
                        overallDf=cbind(overallDf,coln)
                    }
                    colnames(overallDf)=cnames

                    for ( ndx in 1:nEffects ) {
                        sorted=dataf[order(dataf[ndx+1]),]
                        prob=sorted[1]
                        res=sapply(1:(length(prob[,])),function(x){sum(prob[1:(x),1])})
                        stacked[[ndx]]=cbind(sorted[ndx+1],sorted[1])
                        stacked[[ndx]]=cbind(stacked[[ndx]],res)
                        n="eta"
                        stacked[[ndx]]=cbind(n=randomEffectNames[ndx],stacked[[ndx]])
                        cn=colnames(stacked[[ndx]])
                        cn[1]=n
                        cn[2]="value"
                        cn[3]="P"
                        cn[4]="CumP"
                        colnames(stacked[[ndx]])=cn
                        stacked[[ndx]]=cbind("Scenario"=scenarioNames[sc],stacked[[ndx]])
                        rm(sorted)
                        rm(prob)
                        rm(res)
                    }
                    colnames(dataf)=supportColnames
                    # Add the scenario name
                    dataf=cbind("Scenario"=scenarioNames[sc],dataf)
                    etaDataf=cbind("Scenario"=scenarioNames[sc],etaDataf)
                    overallDf=cbind("Scenario"=scenarioNames[sc],overallDf)
                    if ( num_sort_columns > 0 )  {
                        for ( c in 1:num_sort_columns ) {
                            nam = sort_column_names[c]
                            val = unlist(unique_sorted_values[[c]])[uIndex]
                            if ( is.na(val ) )
                                val = unlist(unique_sorted_values[[c]])[uIndex+1]
                            val = sub("^\\s+","",val)
                            dataf=cbind(nam=val,dataf)
                            cn=colnames(dataf)
                            cn[1]=nam
                            colnames(dataf)=cn
                            etaDataf=cbind(nam=val,etaDataf)
                            cn=colnames(etaDataf)
                            cn[1]=nam
                            colnames(etaDataf)=cn
                            overallDf=cbind(nam=val,overallDf)
                            cn=colnames(overallDf)
                            cn[1]=nam
                            colnames(overallDf)=cn
                            for ( ndx in 1:nEffects ) {
                                stacked[[ndx]]=cbind(nam=val,stacked[[ndx]])
                                cn=colnames(stacked[[ndx]])
                                cn[1]=nam
                                colnames(stacked[[ndx]])=cn
                            }
                        }
                    }
                    df = NULL
                    for ( ndx in 1:nEffects ) {
                        df=rbind(df,stacked[[ndx]])
                    }
                    if ( first==TRUE ) {
                        resultsDataframe= dataf
                        resultsStackedDataframe= df
                        resultsEtaDataframe= etaDataf
                        resultsOverallDataframe= overallDf
                    }
                    else {
                        resultsDataframe= rbind(resultsDataframe,dataf)
                        resultsStackedDataframe= rbind(resultsStackedDataframe,df)
                        resultsEtaDataframe= rbind(resultsEtaDataframe,etaDataf)
                        resultsOverallDataframe= rbind(resultsOverallDataframe,overallDf)
                    }
                    first=FALSE
                    missingFile = FALSE 
                }
            }
            sc = sc + 1 
            if ( sc > length(scenarioNames) )  {
                sc = 1 
                uIndex = uIndex + 1 
            }
        }
    }
    if ( missingFile == FALSE )  {
        write.csv(resultsDataframe,file=outputName,row.names=FALSE,quote=FALSE,na="")
        write.csv(resultsStackedDataframe,file=outputStackedName,row.names=FALSE,quote=FALSE,na="")
        write.csv(resultsEtaDataframe,file=outputEtaName,row.names=FALSE,quote=FALSE,na="")
        write.csv(resultsOverallDataframe,file=outputOverallName,row.names=FALSE,quote=FALSE,na="")
    }
}

generateOmegaEtas <-function( jobList,unique_sorted_values,scenarioNames){


    unique_sorted_values = get("unique_sorted_values",envir=.GlobalEnv)
    num_sort_columns = get("num_sort_columns",envir=.GlobalEnv)
    sort_column_names = get("sort_column_names",envir=.GlobalEnv)
    jobsDirectoryRoot=get("jobsDirectoryRoot",envir=nlmeEnv)
    jobsBaseDirectory=getBatchDirectoryLocation(jobsDirectoryRoot)
    UsingBatchTools=get("UsingBatchTools",envir=nlmeEnv)

    resultsDataframe=NULL
    resultsStackedDataframe=NULL
    first=TRUE
    sc=1
    uIndex=1
    appendFlag=FALSE
    control_lines=get("control_lines",envir=nlmeEnv)
    for ( job in jobList ) {
        rDumpFile = figureOutDmpFileLocation(num_sort_columns, sort_column_names, unique_sorted_values, uIndex, job, jobsDirectoryRoot,jobsBaseDirectory)
DEBUG_MASTER(paste("------",job,"------",rDumpFile))
        if ( file.exists(rDumpFile) && file.info(rDumpFile)$size != 0 ) {
            fileName=getRunSuccessFilename(control_lines[job])
            if ( jobsDirectoryRoot != "" ) {
                jobBaseIndx =job %% 100
                outputFile=sprintf("%s/jobs/%02d/%d/%s",jobsBaseDirectory,jobBaseIndx,job,fileName)
            }
            else
                outputFile=sprintf("%s.%0d.Job%0d",fileName,job,job)
            files=c()
            newFileToRead=shrinkDmpDotTxt(rDumpFile)
            source(newFileToRead)
            dat=readLines(outputFile)

            fixedEffectNames=names(dmp.txt$coefficients$fixed)
        
            secondaryEffectNames=names(dmp.txt$coefficients$secondary)
            randomEffectNames=unlist(dimnames(dmp.txt$omega)[1])
            numRandomEffects = length(randomEffectNames)
            omegas=dmp.txt$omega
            etas=dmp.txt$eta
            indx=1
            lowert=c()
            for ( r in 1:numRandomEffects ) {
                for ( c in 1:r ) {
                    ii=(r-1) * numRandomEffects +c
                    lowert[indx]=omegas[ii]
                    indx=indx+1
                }
            }
            numFixedEffects=length(fixedEffectNames)
            numParams = dmp.txt$nParm
            numObservations = dmp.txt$nObs
            numSubjects = dmp.txt$nSubj
            numFixedEffects = length(fixedEffectNames)
            numSecondaryEffects = length(secondaryEffectNames)

            if ( numSecondaryEffects > 0 ) {
            }

            headerLine="#"
            firstLine=""
            line=""
            if ( num_sort_columns > 0 ) 
            for ( c in 1:num_sort_columns ) {
                nam = sort_column_names[c]
                val = unlist(unique_sorted_values[[c]])[uIndex]
                if ( is.na(val ) )
                    val = unlist(unique_sorted_values[[c]])[uIndex+1]
                val = sub("^\\s+","",val)
                if ( c == 1 )  {
                    headerLine=sprintf("%s",nam)
                    line=sprintf("%s",val)
                    firstLine=sprintf("%s",val)
                }
                else {
                    headerLine=sprintf("%s,%s",headerLine,nam)
                    line=sprintf("%s,%s",line,val)
                    firstLine=sprintf("%s,%s",line,val)
                }
            }
            headerLine=sprintf("%s,Scenario,Label",headerLine)
            firstLine=sprintf("%s,%s,Omega",line,scenarioNames[sc])
            firstLineCorrelation=sprintf("%s,%s,Correlation",line,scenarioNames[sc])
            firstLineShrinkage=sprintf("%s,%s,Shrinkage",line,scenarioNames[sc])
            line=sprintf("%s,%s",line,scenarioNames[sc])
            for ( o in 1:numRandomEffects ) {
                headerLine=sprintf("%s,%s",headerLine,randomEffectNames[o])
            }
            OmegaOutputFile="omega.csv"
            if ( appendFlag == FALSE )  {
                cat(headerLine,file=OmegaOutputFile,sep="\n",append=appendFlag)
                appendFlag = TRUE
            }
            cat(firstLine,file=OmegaOutputFile,sep="\n",append=TRUE)
            start=line
            stdArray=c()
            for ( o in 1:numRandomEffects ) {
                start=sprintf("%s,%s",line,randomEffectNames[o])
                for ( o1 in 1:numRandomEffects ) {
                    val=omegas[(((o-1)*numRandomEffects ) + o1)]
                    if ( o == o1 ) {
                        stdArray[o] = sqrt(as.numeric(val))
                    }
#                if ( as.numeric(val) == 0 )
#                    val=""
                    if ( o1 <= o ) 
                        start=sprintf("%s,%s",start,val)
                    else
                        start=sprintf("%s,",start)
                }
                cat(start,file=OmegaOutputFile,sep="\n",append=TRUE)
            }
            cat(firstLineCorrelation,file=OmegaOutputFile,sep="\n",append=TRUE)
            ii=1
            for ( r in 1:numRandomEffects ) {
                start=sprintf("%s,%s",line,randomEffectNames[r])
                for ( c in 1:r ) {
                    stdO = stdArray[r]
                    stdO1 = stdArray[c]
                    omega = lowert[ii]
                    ii = ii + 1 
                    corellation = omega / ( stdO * stdO1 )
                    start=sprintf("%s,%s",start,corellation)
                }
                cat(start,file=OmegaOutputFile,sep="\n",append=TRUE)
            }
            start=firstLineShrinkage
            for ( r in 1:numRandomEffects ) {
                omega=stdArray[r]
                shrinkage= 1 -  ( etas[r,3]/omega)
                start=sprintf("%s,%s",start,shrinkage)
            }
            cat(start,file=OmegaOutputFile,sep="\n",append=TRUE)
        }
        sc = sc + 1 
        if ( sc > length(scenarioNames) )  {
            sc = 1 
            uIndex = uIndex + 1 
        }
    }
}

#'
#' @export
#'
generateThetas <-function( jobList,unique_sorted_values,scenarioNames){


    unique_sorted_values = get("unique_sorted_values",envir=.GlobalEnv)
    num_sort_columns = get("num_sort_columns",envir=.GlobalEnv)
    sort_column_names = get("sort_column_names",envir=.GlobalEnv)
    jobsDirectoryRoot=get("jobsDirectoryRoot",envir=nlmeEnv)
    jobsBaseDirectory=getBatchDirectoryLocation(jobsDirectoryRoot)
    UsingBatchTools=get("UsingBatchTools",envir=nlmeEnv)

    ThetaOutputFile="theta.csv"
    ThetaCorrelationOutputFile="thetaCorrelation.csv"
    ThetaCovarianceOutputFile="thetaCovariance.csv"
    ThetaVarCovarOutputFile="thetaVarCovar.csv"
    first=TRUE
    sc=1
    uIndex=1
    appendFlag=FALSE
    appendFlag2=FALSE
    appendFlag3=FALSE
    for ( job in jobList ) {
        rDumpFile = figureOutDmpFileLocation(num_sort_columns, sort_column_names, unique_sorted_values, uIndex, job, jobsDirectoryRoot,jobsBaseDirectory)
        files=c()
        if ( file.exists(rDumpFile) && file.info(rDumpFile)$size != 0 ) {
            newFileToRead=shrinkDmpDotTxt(rDumpFile)
            source(newFileToRead)
            #---------------
            nobs = dmp.txt$nObs
            nparam = dmp.txt$nParm
            degOfFreedom=as.numeric(nobs) - as.numeric(nparam)
            if ( degOfFreedom < 1 )
                depOfFreedom = 1 
            confidenceLevel=95
            frac=(100 - confidenceLevel)/200
            xcilow = TCDFInverse(degOfFreedom,frac)
            #---------------

            thetas=dmp.txt$coefficients$fixed
            fixedEffectNames=names(thetas)
            fixedEffectNames[length(fixedEffectNames)] = "stdev0"
            numFixedEffects=length(fixedEffectNames)

            varFix=dmp.txt$varFix
            headerLine="#"
            line=""
            line2=""
            if ( num_sort_columns > 0 )  {
              for ( c in 1:num_sort_columns ) {
                nam = sort_column_names[c]
                val = unlist(unique_sorted_values[[c]])[uIndex]
                if ( is.na(val ) )
                    val = unlist(unique_sorted_values[[c]])[uIndex+1]
                val = sub("^\\s+","",val)
                if ( c == 1 )  {
                    headerLine=sprintf("%s",nam)
                    line=sprintf("%s",val)
                }
                else {
                    headerLine=sprintf("%s,%s",headerLine,nam)
                    line=sprintf("%s,%s",line,val)
                }
              } 
              headerLine2=sprintf("%s,Scenario",headerLine)
              line2=sprintf("%s,%s",line,scenarioNames[sc])

              headerLine3=sprintf("%s,Scenario,Var Name",headerLine)
              line3=sprintf("%s,%s",line,scenarioNames[sc])
              headerLine=sprintf("%s,Scenario,Parameter,Estimate,Units,Stderr,CV%%,2.5%%CI,97.5%%CI,Var.Inf.factor",headerLine,scenarioNames[sc])
              line=sprintf("%s,%s",line,scenarioNames[sc])
            }else {
              headerLine2=sprintf("%sScenario",headerLine)
              line2=sprintf("%s%s",line,scenarioNames[sc])

              headerLine3=sprintf("%sScenario,Var Name",headerLine)
              line3=sprintf("%s%s",line,scenarioNames[sc])
              headerLine=sprintf("%sScenario,Parameter,Estimate,Units,Stderr,CV%%,2.5%%CI,97.5%%CI,Var.Inf.factor",headerLine,scenarioNames[sc])
              line=sprintf("%s%s",line,scenarioNames[sc])
            }

            for ( t in 1:numFixedEffects ) {
                headerLine2=sprintf("%s,%s",headerLine2,fixedEffectNames[t])
            }



            if ( appendFlag == FALSE )  {
                cat(headerLine,file=ThetaOutputFile,sep="\n",append=appendFlag)
                appendFlag = TRUE
            }
            start=line
            for ( t in 1:numFixedEffects ) {
#
# Figure out Stderr, CV%, CI%
#
                if ( length(varFix) > 0 ) {
                    dvar = varFix[t,t]
                    sigma = sqrt(dvar )
                    cv = (100 * as.double(sigma)) / thetas[t]
                    low = thetas[t] + xcilow * as.double(sigma)
                    hi = thetas[t] - xcilow * as.double(sigma)

                    start=sprintf("%s,%s,%s,,%s,%s,%s,%s,0",line,
                                  gsub("EEps","stdev",fixedEffectNames[t]),
                                  thetas[t],sigma,cv,low,hi)
                } else
                    start=sprintf("%s,%s,%s,,,,,,0",line,
                           gsub("EEps","stdev",fixedEffectNames[t]),thetas[t])
                cat(start,file=ThetaOutputFile,sep="\n",append=appendFlag)
            }
            if ( length(varFix)!= 0 ) {
                thetaCorrelation=cov2cor(varFix)
                thetaCovariance=varFix
                thetaVarCovar=dmp.txt$Covariance
                names=colnames(thetaVarCovar)
                for ( n in 1:length(names) ) {
                    headerLine3=sprintf("%s,%s",headerLine3,names[n])
                }
                for ( n in 1:length(names) ) {
                    varCovarLine=sprintf("%s,%s",line3,names[n])
                    for ( n1 in 1:length(names) ) {
                        varCovarLine=sprintf("%s,%s",varCovarLine,thetaVarCovar[n,n1])
                    }
                    if ( appendFlag3 == FALSE )  {
                        cat(headerLine3,file=ThetaVarCovarOutputFile,sep="\n",append=appendFlag3)
                        appendFlag3 = TRUE
                    }
                    cat(varCovarLine,file=ThetaVarCovarOutputFile,sep="\n",append=appendFlag3)
                }
                for ( t in 1:numFixedEffects ) {
                   correlationLine=line2
                   covarianceLine=line2
                    for ( t1 in 1:t ) {
                        correlationLine=sprintf("%s,%f",correlationLine,thetaCorrelation[t,t1])
                        covarianceLine=sprintf("%s,%f",covarianceLine,thetaCovariance[t,t1])
                    }
                    if ( t < numFixedEffects ) {
                    for ( t1 in t:(numFixedEffects-1) ) {
                        correlationLine=sprintf("%s,",correlationLine)
                        covarianceLine=sprintf("%s,",covarianceLine)
                    }
                    }
                    if ( appendFlag2 == FALSE )  {
                        cat(headerLine2,file=ThetaCorrelationOutputFile,sep="\n",append=appendFlag2)
                        cat(headerLine2,file=ThetaCovarianceOutputFile,sep="\n",append=appendFlag2)
                        appendFlag2 = TRUE
                    }
                    cat(correlationLine,file=ThetaCorrelationOutputFile,sep="\n",append=appendFlag2)
                    cat(covarianceLine,file=ThetaCovarianceOutputFile,sep="\n",append=appendFlag2)
                }
            }
        }
        sc = sc + 1 
        if ( sc > length(scenarioNames) )  {
            sc = 1 
            uIndex = uIndex + 1 
        }
    }
}




generateDoseTable <-function(jobList,unique_sorted_values,scenarioNames){

    unique_sorted_values = get("unique_sorted_values",envir=.GlobalEnv)
    num_sort_columns = get("num_sort_columns",envir=.GlobalEnv)
    sort_column_names = get("sort_column_names",envir=.GlobalEnv)

    generateGenericTable(jobList,unique_sorted_values,scenarioNames, "doses.csv", "doses.csv",c("id", "time", "dosepoint", "amt", "rate", "path","strip"))
}


generateEtaSpreadsheet <-function(jobList,unique_sorted_values,scenarioNames){

require(reshape)

DEBUG_MASTER("generateEtaSpreadsheet()")
    unique_sorted_values = get("unique_sorted_values",envir=.GlobalEnv)
    num_sort_columns = get("num_sort_columns",envir=.GlobalEnv)
    sort_column_names = get("sort_column_names",envir=.GlobalEnv)
    jobsDirectoryRoot=get("jobsDirectoryRoot",envir=nlmeEnv)
    jobsBaseDirectory=getBatchDirectoryLocation(jobsDirectoryRoot)
    UsingBatchTools=get("UsingBatchTools",envir=nlmeEnv)

    resultsDataframe=NULL
    resultsStackedDataframe=NULL
    first=TRUE
    sc=1
    uIndex=1
    missingFile=TRUE
    for ( job in jobList ) {
        files=c()
        if ( jobsDirectoryRoot != "" ) {
            jobBaseIndx =job %% 100
            fileToRead=sprintf("%s/jobs/%02d/%d/%s",jobsBaseDirectory,jobBaseIndx,job,"IdEta.txt")
            shrinkFileToRead=sprintf("%s/jobs/%02d/%d/%s",jobsBaseDirectory,jobBaseIndx,job,"EtaShrinkageBySubject.txt")
        }
        else {
            fileToRead=sprintf("IdEta.txt.Job%0d",job)
            shrinkFileToRead=sprintf("EtaShrinkageBySubject.txt.Job%0d",job)
       }
        if ( file.exists(fileToRead) && file.info(fileToRead)$size != 0 ) {
            dataf=read.csv(fileToRead,header=FALSE)
            # remove the 1st 4 id columns
            for ( c in 1:4 ) {
                dataf[1] = NULL
            }
            colnames(dataf)=c("id","Eta","Value")
            cdata=cast(dataf,id~Eta)

            names=colnames(cdata)
            shrink_names=c()
            for ( n in 2:length(names) ){
                shrink_names=c(shrink_names,sprintf("%s_shrinkage",names[n]))
            }


            shrink_data=read.table(shrinkFileToRead,sep="",header=FALSE)
            shrink_data[1]=NULL
            colnames(shrink_data)=shrink_names
            merged_data=cbind(cdata,shrink_data)

            if ( nrow(dataf) > 0 ) {
            # Add empty scenario column to data table
                merged_data=cbind("Scenario"=scenarioNames[sc],merged_data)
                dataf=cbind("Scenario"=scenarioNames[sc],dataf)
                if ( num_sort_columns > 0 ) 
                for ( c in 1:num_sort_columns ) {
                    nam = sort_column_names[c]
                    val = unlist(unique_sorted_values[[c]])[uIndex]
                    if ( is.na(val ) )
                        val = unlist(unique_sorted_values[[c]])[uIndex+1]
                    val = sub("^\\s+","",val)
                    merged_data=cbind(nam=val,merged_data)
                    cn=colnames(merged_data)
                    cn[1]=nam
                    colnames(merged_data)=cn

                    dataf=cbind(nam=val,dataf)
                    cn=colnames(dataf)
                    cn[1]=nam
                    colnames(dataf)=cn
                }
                if ( first==TRUE ) {
                    resultsDataframe= merged_data
                    resultsStackedDataframe= dataf
                }
                else {
                    resultsDataframe= rbind(resultsDataframe,merged_data)
                    resultsStackedDataframe= rbind(resultsStackedDataframe,dataf)
                }
                first=FALSE
                missingFile = FALSE
            }
        }
        sc = sc + 1 
        if ( sc > length(scenarioNames) )  {
            sc = 1 
            uIndex = uIndex + 1 
        } 
    }
    if ( missingFile == FALSE )  {
        write.csv(resultsDataframe,file="Eta.csv",row.names=FALSE,quote=FALSE,na="")
        write.csv(resultsStackedDataframe,file="EtaStacked.csv",row.names=FALSE,quote=FALSE,na="")
    }
}


TCDFInverse <-function(degOfFreedom, ptarget){
    if ( ptarget <=0 || ptarget >=1 )
        return -9999
    if ( ptarget >= 0.5 ) 
        x0 = 0
    else
        x0 = -1000
    if ( ptarget <= 0.5 ) 
        x1 = 0
    else
        x1 = 1000
    p0 = pt(x0,df=degOfFreedom)
    p1 = pt(x1,df=degOfFreedom)
    if ( p0 > ptarget ) return(x0)
    if ( p1 < ptarget ) return(x1)
    while ( 1 ) {
        x = (x0 + x1 ) / 2
        p= pt(x,df=degOfFreedom)
        err = p - ptarget 
        if ( abs(err) < 1e-6 ) {
DEBUG_MASTER(paste("degOfFreedom ",  degOfFreedom, " ptarget", ptarget, "x ",x))
            return(x)
        }
        if ( err > 0 ){
            x1= x 
            p1 = p 
        }
        else {
            x0= x 
            p0 = p 
        }
    }
}

generateSecondary <-function(outputFilenames,unique_sorted_values,scenarioNames){

    num_sort_columns = get("num_sort_columns",envir=.GlobalEnv)
    sort_column_names = get("sort_column_names",envir=.GlobalEnv)
    jobsDirectoryRoot=get("jobsDirectoryRoot",envir=nlmeEnv)
    jobsDirectoryRoot=get("jobsDirectoryRoot",envir=nlmeEnv)
    jobsBaseDirectory=getBatchDirectoryLocation(jobsDirectoryRoot)
    UsingBatchTools=get("UsingBatchTools",envir=nlmeEnv)

    secondaryTable=c()
    sc=1
    uIndex=1
    filesAreMissing=TRUE
    for ( fileToRead in outputFilenames ) {
DEBUG_MASTER(fileToRead)
        if ( file.exists(fileToRead) && file.info(fileToRead)$size != 0 ) {
            filesAreMissing=FALSE
            lines=readLines(fileToRead)
            secondaryIndex=grep("secondary",lines)
            if( length(secondaryIndex) ==  1 ) {
                pos=grep("NParm",lines)
                nparam=unlist(strsplit(lines[pos],split="="))[2]
                pos=grep("NObs",lines)
                nobs=unlist(strsplit(lines[pos],split="="))[2]
                degOfFreedom=as.numeric(nobs) - as.numeric(nparam)
                if ( degOfFreedom < 1 )
                    depOfFreedom = 1 
                secondaryStderrIndex=grep("stderrSecondary",lines)
                omega5Index=grep("omega5",lines)
                if ( length(secondaryStderrIndex) == 0 ) 
                    secondaryStderrIndex=omega5Index[1]
                # parse out secondary names/values and stderr
                numSecondaryVars = secondaryStderrIndex - secondaryIndex - 2 
                fileToRead="secondaryVariableNames.txt"
                units=c()
                if ( file.exists(fileToRead) && file.info(fileToRead)$size !=0){
                    names=readLines(fileToRead)
                    for ( n in names ) {
                        u = unlist(strsplit(n,split=" "))[2]
                        if ( is.na(u) ) 
                            u = ""
                        units=c(units,u)
                    }
                }
                for ( l in 1:numSecondaryVars ) {
                    secondaryVarLine=lines[secondaryIndex+l]
                    secondaryVarStderrLine=lines[secondaryStderrIndex+l]
                    vals=unlist(strsplit(secondaryVarLine,split="\t"))
                    secondaryVal=as.double(vals[2])
                    secondaryName=sub("#\\s+","",vals[3])
                    if ( secondaryStderrIndex  == omega5Index[1] ) 
                        secondaryStderr=""
                    else
                        secondaryStderr=as.double(unlist(strsplit(secondaryVarStderrLine,split="\t"))[2])
                    if ( l <= length(units) )
                        unit=units[l]
                    else
                        unit = "" 
                    rowcol=c()
                    # Add all the sort columns to the row
                    if ( num_sort_columns > 0 ) 
                        for ( c in 1:num_sort_columns ) {
                            nam = sort_column_names[c]
                            val = unlist(unique_sorted_values[[c]])[uIndex]
                            if ( is.na(val ) )
                                val =unlist(unique_sorted_values[[c]])[uIndex+1]
                            val = sub("^\\s+","",val)
                            rowcol[eval(nam)]= val
                        }
                    # Add the empty scenario 
                    rowcol["Scenario"]=scenarioNames[sc]
                    rowcol["Secondary"]=secondaryName
                    rowcol["Estimate"]=secondaryVal
                    rowcol["Units"]=unit
                    rowcol["Stderr"]=secondaryStderr
                    confidenceLevel=95
                    frac=(100 - confidenceLevel)/200
                    xcilow = TCDFInverse(degOfFreedom,frac)
DEBUG_MASTER(paste("xcilow",xcilow,"secondaryStderr",secondaryStderr))
                    if ( secondaryStderr == "" ) {
                        low =""
                        high =""
                        cv =""
                    }
                    else {
                        low = secondaryVal  + xcilow * as.double(secondaryStderr)
                        high = secondaryVal  - xcilow * as.double(secondaryStderr)
                        cv=(100*as.double(secondaryStderr))/secondaryVal
                    }
                    rowcol["CV%"]=cv
                    rowcol["2.5%CI"]=low
                    rowcol["97.5%CI"]=high
                    rowcol["Var.Inf.factor"]=""
DEBUG_MASTER(rowcol) 
                # Add the row to secondary table
                secondaryTable=rbind(secondaryTable,rowcol)
                }
            }

        }
        sc = sc + 1 
        if ( sc > length(scenarioNames) )  {
            sc = 1 
            uIndex = uIndex + 1 
        }
    }
    if ( filesAreMissing == FALSE ) 
        write.csv(secondaryTable,file="Secondary.csv",row.names=FALSE,quote=FALSE)
}


#
# Given a filename:recordno
# read the lines
readNlmeArgsFile <- function(filespec){

    filename=strsplit(filespec,":")[[1]][1]
    record=as.integer(strsplit(filespec,":")[[1]][2])
    allLines = readLines(filename)
    numRecords=length(grep("/o",allLines,fixed=TRUE))
    numArgsLines=length(allLines)/numRecords
    startRec=numArgsLines * ( record -1 ) + 1 
    lines=c()
    for ( l in startRec:(startRec+numArgsLines -1 )) {
        lines=c(lines,allLines[l])
    }

    return(lines)
}



#
# Generate a new control file from original by taking dataset and 
# splitting it by unique column keys
#
#
sortByColumnAndGenerateControlFile<-function(inputFilename,numColumns, columnNamesArray,argsFile,controlFilename) {

numRuns = 0
    # NLME combined arugments file
    combinedArgsFilename=sprintf("%s_combined_args.txt",strsplit(controlFilename,'[.]')[[1]][1])
    # Split the data set into multiple ones
#    data=read.csv(inputFilename)
#    cn[1]=gsub("X..","",cn[1],fixed=TRUE)
    data=read.csv(inputFilename,check.names=FALSE)
    cn=colnames(data)
    cn[1]=gsub("^##","",cn[1],fixed=FALSE)
    colnames(data)=cn

    fName2="data2.txt"
    if ( file.exists(fName2) ) {
        data2=read.csv(fName2,check.names=FALSE)
        cn=colnames(data2)
#        cn[1]=gsub("X..","",cn[1],fixed=TRUE)
        cn[1]=gsub("^##","",cn[1],fixed=FALSE)
        colnames(data2)=cn
    } else
        data2=NULL
    argsFileLines=readLines(argsFile)

    if ( numColumns == 0 ) { # No sort columns
        num_sort_columns=numColumns
        sort_column_names=""
        assign("num_sort_columns", num_sort_columns, envir=.GlobalEnv)
        assign("sort_column_names", sort_column_names, envir=.GlobalEnv)
        unique_sorted_values=NULL
        assign("unique_sorted_values", unique_sorted_values, envir=.GlobalEnv)
        numSortDatasets = 1 
    }
    else {
        colNames=c()
        for ( c in unlist(strsplit(columnNamesArray, split=" ") ))
            colNames=c(colNames,c)
        sorted=data[order(data[,colNames]),]
        if ( length(data2) != 0 )
            sorted2=data2[order(data2[,colNames]),]

#    unique_sorted_values=unique(sorted[colNames])
        num_sort_columns=numColumns
        sort_column_names=colNames
        assign("num_sort_columns", num_sort_columns, envir=.GlobalEnv)
        assign("sort_column_names", sort_column_names, envir=.GlobalEnv)

        tmpTable=split(sorted,f=sorted[,colNames],drop=TRUE)
        outTables=tmpTable[order(unlist(names(tmpTable)),decreasing=FALSE)]
        keyNames=names(outTables)
        unique_sorted_values=data.frame(matrix(ncol=length(colNames),nrow=length(keyNames)))
        colnames(unique_sorted_values)=colNames
        for ( r in 1:length(keyNames) ){
            row=c()
            key=keyNames[r]
            tokens=strsplit(key,".",fixed=TRUE)
            for ( t in unlist(tokens) ) {
                row=c(row,t)
            }
            unique_sorted_values[r,] = row
        }
        assign("unique_sorted_values", unique_sorted_values, envir=.GlobalEnv)

        if ( length(data2) != 0 ) {
            tmpTable=split(sorted2,f=sorted2[,colNames],drop=TRUE)
            outTables2=tmpTable[order(unlist(names(tmpTable)),decreasing=FALSE)]
        }
        numSortDatasets=length(outTables)
    }
    listOfProfileModels= generateProfileModels()
    numProfileVariables = length(listOfProfileModels)
    # How many scenarios do we have in the arguments file
    # We now know how many runs to create
    numScenarios=as.integer(argsFileLines[4])
    numSimulationRuns = numScenarios * numSortDatasets * numProfileVariables
    # 1st line
    cat(argsFileLines[1],file=controlFilename,sep="\n",append=FALSE)
    # second line needs to have all the new data files added to it
    cat(argsFileLines[2],file=controlFilename,sep=" ",append=TRUE)
    cat(" ",file=controlFilename,sep=" ",append=TRUE)
    cat(combinedArgsFilename,file=controlFilename,sep=" ",append=TRUE)
    tokens=unlist(strsplit(argsFileLines[5],","))
    if ( numSortDatasets == 1 ) {
    } else {
        for ( indx in 1:numSortDatasets ) {
            inputFile=sprintf(" %s.%d ",inputFilename,indx)
            cat(inputFile,file=controlFilename,sep=" ",append=TRUE)
            if ( length(data2) != 0 ) 
                cat(sprintf(" %s.%d ",fName2,indx),file=controlFilename,sep=" ",append=TRUE)
        }
    }
    if ( numProfileVariables> 0 ) {
        cat(" " ,file=controlFilename,sep=" ",append=TRUE)
        for ( indx in 1:numProfileVariables ) {
            cat(paste0(listOfProfileModels[[indx]]$modelName, " " ),file=controlFilename,sep=" ",append=TRUE)
        }
    }
    cat("",file=controlFilename,sep="\n",append=TRUE)
    # 3rd line stays the same
    cat(argsFileLines[3],file=controlFilename,sep="\n",append=TRUE)
    # 4th line is the number of runs
    cat(sprintf("%d",numSimulationRuns),file=controlFilename,sep="\n",append=TRUE)

    numRuns=numSortDatasets
    overwrite=FALSE
    nxtSeq = 1
    outfileSeq = 1
    flag=FALSE
    posthocTables=getTableNames("cols1.txt")
    for ( indx in 1:numSortDatasets ) {
        for ( scenario in 1:numScenarios ) {
            for ( prof in 1:numProfileVariables ) {
                originalLine=argsFileLines[4+scenario]
                tokens=unlist(strsplit(originalLine,","))
                scenarioName=tokens[1]
                argsFileName=tokens[2]
                outBaseName=tokens[4]
                outputFilename=sprintf("%s.%d",outBaseName,outfileSeq)
                generalFilesToRetrieve=tokens[5]
                jobFilesToRetrieve=tokens[6]
                # Write out a record in new nlmearguments file
                nlmearguments=readNlmeArgsFile(tokens[2])
                for ( i in 1:(length(nlmearguments)-1)){
                # Lets make sure that we replace data2.txt with data2.txt.i , etc
                    argsLine=nlmearguments[i]
                    for ( j in  2:2 ) {
                        from=sprintf("data%d.txt",j)
                        to=sprintf("data%d.txt.%d",j,indx)
                        argsLine=gsub(from,to,argsLine)
                    }
                    cat(argsLine,file=combinedArgsFilename,sep="\n",append=flag)
                    flag=TRUE
                }
#            cat(outputFilename,file=combinedArgsFilename,sep="\n",append=flag)
                #  Change the original file line into  new specs
                if ( listOfProfileModels[[prof]]$exePostfix=="")
                    postFix=""
                else
                    postFix=paste(",",listOfProfileModels[[prof]]$exePostfix)
#                line=sprintf("%s,%s:%d,,%s,%s,%s %s %s %s",scenarioName,combinedArgsFilename,nxtSeq,outputFilename,generalFilesToRetrieve, jobFilesToRetrieve,posthocTables, outputFilename,postFix)
                line=sprintf("%s,%s:%d,,%s,%s,%s %s %s %s",scenarioName,combinedArgsFilename,outfileSeq,outputFilename,generalFilesToRetrieve, jobFilesToRetrieve,posthocTables, outputFilename,postFix)
                cat(line ,file=controlFilename,sep="\n",append=TRUE)
                if ( numSortDatasets > 1 ) {
                    filename=sprintf("%s.%d",inputFilename,indx)
                    names=colnames(outTables[[indx]])
                    if ( length(grep("##",names[1])) == 0 ) 
                        names[1]=sprintf("##%s",names[1])
                    colnames(outTables[[indx]])=names
                    write.csv(outTables[[indx]],file=filename,row.names=FALSE,quote=FALSE)
                    if ( length(data2) != 0 ) {
                        filename2=sprintf("%s.%d",fName2,indx)
                        names=colnames(outTables2[[indx]])
                        if ( length(grep("##",names[1])) == 0 ) 
                            names[1]=sprintf("##%s",names[1])
                        colnames(outTables2[[indx]])=names
                        write.csv(outTables2[[indx]],file=filename2,row.names=FALSE,quote=FALSE)
                    }
                }
                else {
                    filename = inputFilename
                    filename2 = fName2
                }
                overwrite=TRUE

                cat(sprintf(" cols1.txt %s %s",filename,outputFilename),file=combinedArgsFilename,sep="\n",append=overwrite)
                outfileSeq = outfileSeq + 1 
            }
            nxtSeq = nxtSeq + 1 
        }
    }
    return(numSimulationRuns)
}






#
# Summarizes the results from a covariate search run.
# generates Overall.csv and StatusWindows.txt
#
summarizeCovarSearch <-function(localWorkingDir,jobList,control_lines){

    UsingBatchTools=get("UsingBatchTools",envir=nlmeEnv)
    workflow_name=get("workflow_name",envir=nlmeEnv)
    jobsDirectoryRoot=get("jobsDirectoryRoot",envir=nlmeEnv)
    jobsBaseDirectory=getBatchDirectoryLocation(jobsDirectoryRoot)
    setwd(localWorkingDir)
    GlobalSummaryLine1=sprintf("Summarizing shotgun covariate search results for %d scenarios",length(jobList))
    assign("GlobalSummaryLine1", GlobalSummaryLine1, envir=nlmeEnv)
    GlobalSummaryLine2=""
    assign("GlobalSummaryLine2", GlobalSummaryLine2, envir=nlmeEnv)
    GlobalSummaryLine3=""
    assign("GlobalSummaryLine3", GlobalSummaryLine3, envir=nlmeEnv)
    UpdateProgressMessages()

    fileNames=c()
    scenarioNames=c()
    for ( job in jobList) {
        fileName=getOutputFilenames(control_lines[job])
        # expand the filename to point to original run directory
        if ( jobsDirectoryRoot != "" ) {
            jobBaseIndx =job %% 100
            fileName=sprintf("%s/jobs/%02d/%d/%s",jobsBaseDirectory,jobBaseIndx,job,fileName)
        }
        fileNames=c(fileNames,fileName)
        sName = getScenarioName(control_lines[job])
        if ( ! sName %in% scenarioNames ) 
            scenarioNames=c(scenarioNames,sName)
    }
    GlobalSummaryLine2="Generating Overall.csv"
    assign("GlobalSummaryLine2", GlobalSummaryLine2, envir=nlmeEnv)
    generateOverallSummary(fileNames,scenarioNames,FALSE)
    generateOmegaStderr(fileNames,scenarioNames,TRUE)


    GlobalSummaryLine2=sprintf("Generating status file for shotgun covariate search results for %d scenarios",length(jobList))
    assign("GlobalSummaryLine2", GlobalSummaryLine2, envir=nlmeEnv)
    UpdateProgressMessages()
    progressfilenames=c()
    scenarioNames=c()
    fileNames=c()
    for ( job in jobList ) {
        fileName=sprintf("S_%03d.status",job)
        if ( jobsDirectoryRoot != "" ) {
            fileName=paste0(jobsDirectoryRoot,"/",fileName)
        }
        fileNames=c(fileNames,fileName)
        fileName=getOutputFilenames2(control_lines[job])
        if ( jobsDirectoryRoot != "" ) {
            jobBaseIndx =job %% 100
            fileName=sprintf("%s/jobs/%02d/%d/%s",jobsBaseDirectory,jobBaseIndx,job,fileName)
        } else {
            fileName=sprintf("%s.Job%0d",fileName,job)
        }
        progressfilenames=c(progressfilenames,fileName)
        scenarioNames=c(scenarioNames,getScenarioName(control_lines[job]))
    }
    GlobalSummaryLine2="Generating StatusWindow.txt"
    assign("GlobalSummaryLine2", GlobalSummaryLine2, envir=nlmeEnv)
    generateStatusWindow(fileNames,progressfilenames,scenarioNames)

    GlobalSummaryLine1="Finished summarizing shotgun covariate search results"
    assign("GlobalSummaryLine1", GlobalSummaryLine1, envir=nlmeEnv)
    GlobalSummaryLine1="Transfering data and loading results..."
    assign("GlobalSummaryLine2", GlobalSummaryLine2, envir=nlmeEnv)
    GlobalSummaryLine3=""
    assign("GlobalSummaryLine3", GlobalSummaryLine3, envir=nlmeEnv)
    UpdateProgressMessages()
}


#
# Summarizes the results from a stepwise covariate search run.
# generates Overall.csv and StatusWindows.txt
#
summarizeStepwiseCovarSearch <-function(localWorkingDir,scenarios){

    setwd(localWorkingDir)
DEBUG_MASTER("summarizeStepwiseCovarSearch()")
    OverallFilename="Overall.csv"
    cat("Scenario,RetCode,LogLik,-2LL,AIC,BIC,nParm,nObs,nSub,EpsShrinkage,Condition",file=OverallFilename,sep="\n",append=FALSE)
    workflow_name=get("workflow_name",envir=nlmeEnv)
    jobsDirectoryRoot=get("jobsDirectoryRoot",envir=nlmeEnv)

    GlobalSummaryLine1=sprintf("Summarizing stepwise covariate search results for %d scenarios",length(scenarios))
    assign("GlobalSummaryLine1", GlobalSummaryLine1, envir=nlmeEnv)
    GlobalSummaryLine2=""
    assign("GlobalSummaryLine2", GlobalSummaryLine2, envir=nlmeEnv)
    GlobalSummaryLine3=""
    assign("GlobalSummaryLine3", GlobalSummaryLine3, envir=nlmeEnv)
    UpdateProgressMessages()
    indx = 1 
    for ( scenario in scenarios) {
        indx=indx+1
        if ( scenario$status == "Completed" ){

            returnCode= as.integer(scenario$returnCode)
            logLik    = scenario$logLike
            twoLL    = scenario$logLikelihood
            shrinkage = scenario$shrinkage
            nParam    = as.integer(scenario$nParam)
            nObs      = as.integer(scenario$nObs)
            nSub      = as.integer(scenario$nSub)
            AIC       = scenario$AIC
            BIC       = scenario$BIC
DEBUG_MASTER(paste("nParam",nParam))
DEBUG_MASTER(paste("nObs",nObs))
DEBUG_MASTER(paste("nSub",nSub))
    GlobalSummaryLine2= sprintf("%s %s,%d,%f,%f,%f,%f,%d,%d,%d,%f,",scenario$scenarioDescription,scenario$scenarioName,returnCode,logLik,twoLL,AIC,BIC,nParam,nObs,nSub,shrinkage)
    assign("GlobalSummaryLine2", GlobalSummaryLine2, envir=nlmeEnv)
    UpdateProgressMessages()

            cat(sprintf("%s %s,%d,%f,%f,%f,%f,%d,%d,%d,%f,",scenario$scenarioDescription,scenario$scenarioName,returnCode,logLik,twoLL,AIC,BIC,nParam,nObs,nSub,shrinkage), file=OverallFilename,sep="\n",append=TRUE)
        } 
        else {
            cat(sprintf("%s,,,,,,,,,,",scenario$scenarioName), file=OverallFilename,sep="\n",append=TRUE)
        }
    } 
    filenames=c()
    progressfilenames=c()
    scenarioNames=c()
    for ( s in scenarios ) {
        if ( scenario$status == "Completed" ){
                filename=sprintf("status.txt.%0d",s$index)
            filenames=c(filenames,filename)
                fileToRead=sprintf("progress.txt.%0d",s$index)
            progressfilenames=c(progressfilenames,fileToRead)
            scenarioNames=c(scenarioNames,s$scenarioName)
        }
    }
    GlobalSummaryLine2= "Generating StatusWindow.txt"
    assign("GlobalSummaryLine2", GlobalSummaryLine2, envir=nlmeEnv)
    UpdateProgressMessages()
    generateStatusWindow(filenames,progressfilenames,scenarioNames)

}


#
# Reads outnnnnn.txt from multiple runs and generates Overall.csv
#
generateOverallSummary <-function(outputFileNames,scenarioNames,sorting=FALSE){
DEBUG_MASTER("generateOverallSummary")
    workflow_name=get("workflow_name",envir=nlmeEnv)

    if ( sorting == TRUE ) {
    unique_sorted_values=get("unique_sorted_values",envir=nlmeEnv)
    num_sort_columns = get("num_sort_columns",envir=.GlobalEnv)
    sort_column_names = get("sort_column_names",envir=.GlobalEnv)
    }
    else {
        unique_sorted_values= NULL

        num_sort_columns = 0 
        sort_column_names = NULL

    }

    nxtScenario=1
    nxtSortKey=1
    OverallFilename="Overall.csv"
    sortNames=""
    if ( num_sort_columns != 0 ) 
    for ( c in 1:num_sort_columns ) {
        nam = sort_column_names[c]
        sortNames=sprintf("%s%s,",sortNames,nam)
    }
    cat(sprintf("%sScenario,RetCode,LogLik,-2LL,AIC,BIC,nParm,nObs,nSub,EpsShrinkage,Condition",sortNames),file=OverallFilename,sep="\n",append=FALSE)
    workflow_name=get("workflow_name",envir=nlmeEnv)

    UpdateProgressMessages()
    for ( fileName in outputFileNames) {
        if ( file.exists(fileName) ) {
            lines=readLines(fileName)
            first=grep("ReturnCode",lines,fixed=TRUE)
            stopifnot(length(first) != 0 ) 

            tok=unlist(strsplit(lines[first],"="))[2]
            returnCode=as.integer(tok)

            tok=unlist(strsplit(lines[first+1],"="))[2]
            logLik=as.double(tok)

            tok=unlist(strsplit(lines[first+2],"="))[2]
            shrinkage=as.double(tok)

            tok=unlist(strsplit(lines[first+3],"="))[2]
            nParam=as.integer(tok)

            tok=unlist(strsplit(lines[first+4],"="))[2]
            nObs=as.integer(tok)

            tok=unlist(strsplit(lines[first+5],"="))[2]
            nSub=as.integer(tok)
            twoLL= -2 * logLik
            AIC = twoLL + nParam * 2 
            BIC = twoLL + nParam * log(nObs)
            sortValues=""
            if ( num_sort_columns != 0 ) 
            for ( c in 1:num_sort_columns ) {
                nam = sort_column_names[c]
                val = unlist(unique_sorted_values[[c]])[nxtSortKey]
                if ( is.na(val ) )
                    val = unlist(unique_sorted_values[[c]])[nxtSortKey+1]
                val = sub("^\\s+","",val)
                sortValues=sprintf("%s%s,",sortValues,val)
            }
            condition=""
            pos=grep("condition =",lines,fixed=TRUE)
            if ( length(pos) > 0 )  {
                condition = unlist(strsplit(lines[[pos[[1]]]],split="="))[[2]]
            }
            cat(sprintf("%s%s,%d,%f,%f,%f,%f,%d,%d,%d,%f,%s",sortValues,scenarioNames[nxtScenario],returnCode,logLik,twoLL,AIC,BIC,nParam,nObs,nSub,shrinkage,condition), file=OverallFilename,sep="\n",append=TRUE)
        } 
        else {
            cat(sprintf("%s,,,,,,,,,,",scenarioNames[nxtScenario]), file=OverallFilename,sep="\n",append=TRUE)
        }
        nxtScenario=nxtScenario+1
        if ( nxtScenario > length(scenarioNames) ){
            nxtScenario = 1 
            nxtSortKey = nxtSortKey + 1 
        }
        
    } 
} 


#
# Reads outnnnnn.txt from multiple runs and generates omega_stderr.csv
#
generateOmegaStderr<-function(outputFileNames,scenarioNames,sorting=FALSE){
DEBUG_MASTER("generateOmegaStderr")
    workflow_name=get("workflow_name",envir=nlmeEnv)

    if ( sorting == TRUE ) {
    unique_sorted_values=get("unique_sorted_values",envir=nlmeEnv)
    num_sort_columns = get("num_sort_columns",envir=.GlobalEnv)
    sort_column_names = get("sort_column_names",envir=.GlobalEnv)
    }
    else {
        unique_sorted_values= NULL

        num_sort_columns = 0 
        sort_column_names = NULL

    }

    nxtScenario=1
    nxtSortKey=1
    OverallFilename="omega_stderr.csv"
    sortNames=""
    if ( num_sort_columns != 0 ) 
    for ( c in 1:num_sort_columns ) {
        nam = sort_column_names[c]
        sortNames=sprintf("%s%s,",sortNames,nam)
    }
    header=sprintf("%sScenario,Label",sortNames)

    workflow_name=get("workflow_name",envir=nlmeEnv)

    UpdateProgressMessages()
    first=TRUE
    for ( fileName in outputFileNames) {
        if ( file.exists(fileName) ) {
            lines=readLines(fileName)
            pos=grep("stdOmega5 #",lines,fixed=TRUE)
            stopifnot(length(pos) != 0 ) 

            tok=unlist(strsplit(lines[pos],"#"))[2]
            thetaNames=unlist(strsplit(tok,"\\t"))
            thetaNames=trimws(thetaNames,"both")

            if ( first == TRUE ) {
                for ( t in thetaNames ) {
                    header=paste0(header,",",t)
                }
                first = FALSE 
                cat(header,file=OverallFilename,sep="\n",append=FALSE)
            }

            for ( indx in 1:length(thetaNames) ) {
                sortValues=""
                if ( num_sort_columns != 0 ) 
                    for ( c in 1:num_sort_columns ) {
                        nam = sort_column_names[c]
                        val = unlist(unique_sorted_values[[c]])[nxtSortKey]
                        if ( is.na(val ) )
                            val= unlist(unique_sorted_values[[c]])[nxtSortKey+1]
                        val = sub("^\\s+","",val)
                        sortValues=sprintf("%s%s,",sortValues,val)
                }
                line = lines[[pos[[1]]+indx]]
                tokens=unlist(strsplit(line,split="\\t"))
                tokens=trimws(tokens,"both")
                values=paste0(tokens,collapse=",")
                if ( length(tokens) < length(thetaNames )  )
                  for ( i in (length(tokens)+1):length(thetaNames) ) {
                      values=paste(values,"",sep=",")
                  }
                cat(sprintf("%s%s,%s,%s",sortValues,scenarioNames[nxtScenario],
                            thetaNames[[indx]],values), 
                            file=OverallFilename,sep="\n",append=TRUE)
            }
        } 
        else {
            cat(sprintf("%s,,,,,",scenarioNames[nxtScenario]), 
               file=OverallFilename,sep="\n",append=TRUE)
        }
        nxtScenario=nxtScenario+1
        if ( nxtScenario > length(scenarioNames) ){
            nxtScenario = 1 
            nxtSortKey = nxtSortKey + 1 
        }
        
    } 
} 

#
# Reads outnnnnn.txt from multiple runs and generates Profile.csv
#
generateProfileSummary <-function(outputFileNames,scenarioNames,sorting=FALSE){
    workflow_name=get("workflow_name",envir=nlmeEnv)

    profileModels=getListOfExesNeeded()
    if ( sorting == TRUE ) {
        unique_sorted_values=get("unique_sorted_values",envir=nlmeEnv)
        num_sort_columns = get("num_sort_columns",envir=.GlobalEnv)
        sort_column_names = get("sort_column_names",envir=.GlobalEnv)
    }
    else {
        unique_sorted_values= NULL

        num_sort_columns = 0 
        sort_column_names = NULL

    }

    nxtScenario=1
    nxtSortKey=1
    nxtProfileIndex=1
    ProfileFilename="Profile.csv"
    sortNames=""
    if ( num_sort_columns != 0 ) 
    for ( c in 1:num_sort_columns ) {
        nam = sort_column_names[c]
        sortNames=sprintf("%s%s,",sortNames,nam)
    }
    cat(sprintf("%sScenario,Theta,Estimate,LogLik,RetCode,Delta,Percent",sortNames),file=ProfileFilename,sep="\n",append=FALSE)
    workflow_name=get("workflow_name",envir=nlmeEnv)

    UpdateProgressMessages()
    for ( fileName in outputFileNames) {
        if ( file.exists(fileName) ) {
            lines=readLines(fileName)
            first=grep("ReturnCode",lines,fixed=TRUE)
            stopifnot(length(first) != 0 ) 

            tok=unlist(strsplit(lines[first],"="))[2]
            returnCode=as.integer(tok)

            tok=unlist(strsplit(lines[first+1],"="))[2]
            logLik=as.double(tok)

            tok=unlist(strsplit(lines[first+2],"="))[2]
            shrinkage=as.double(tok)

            tok=unlist(strsplit(lines[first+3],"="))[2]
            nParam=as.integer(tok)

            tok=unlist(strsplit(lines[first+4],"="))[2]
            nObs=as.integer(tok)

            tok=unlist(strsplit(lines[first+5],"="))[2]
            nSub=as.integer(tok)

            twoLL= -2 * logLik
            AIC = twoLL + nParam * 2 
            BIC = twoLL + nParam * log(nObs)
            sortValues=""
            if ( num_sort_columns != 0 ) 
            for ( c in 1:num_sort_columns ) {
                nam = sort_column_names[c]
                val = unlist(unique_sorted_values[[c]])[nxtSortKey]
                if ( is.na(val ) )
                    val = unlist(unique_sorted_values[[c]])[nxtSortKey+1]
                val = sub("^\\s+","",val)
                sortValues=sprintf("%s%s,",sortValues,val)
            }
cat(sprintf("%s%s,%s,%f,%f,%d,%s,%s",sortValues,scenarioNames[nxtScenario],profileModels[[nxtProfileIndex]]$theta,profileModels[[nxtProfileIndex]]$initialValue,logLik,returnCode,profileModels[[nxtProfileIndex]]$delta,profileModels[[nxtProfileIndex]]$percent),file=ProfileFilename,sep="\n",append=TRUE)
        } 
        else {
            cat(sprintf("%s,,,,,,",scenarioNames[nxtScenario]), file=ProfileFilename,sep="\n",append=TRUE)
        }
        nxtProfileIndex=nxtProfileIndex+1
        if ( nxtProfileIndex > length(profileModels) ) {
            nxtProfileIndex = 1 
            nxtScenario=nxtScenario+1
        }
        if ( nxtScenario > length(scenarioNames) ){
            nxtScenario = 1 
            nxtSortKey = nxtSortKey + 1 
        }
        
    } 
} 


#
# Takes a list of jobs and generates StatusWindow.txt
#

generateStatusWindow <-function(statusFiles,progressFiles,scenarioNames){
DEBUG_MASTER("generateStatusWindow()")
DEBUG_MASTER(paste(statusFiles))
DEBUG_MASTER(paste(progressFiles))
DEBUG_MASTER(paste(scenarioNames))

tryCatch (
{
    StatusFilename="StatusWindow.txt"
    appendFlag = FALSE
    indx=1
    nxt=1
    workflow_name=get("workflow_name",envir=nlmeEnv)
    for ( filename in statusFiles ) {
DEBUG_MASTER(filename)
            statusFileLines=readLines(filename)
            if ( length(grep("FAILED",statusFileLines)) > 0 ) {
                cat(sprintf("%s : %s FAILED",workflow_name,scenarioNames[nxt]),file=StatusFilename,sep="\n",append=appendFlag)
                appendFlag = TRUE
                next
            }
            startTime=strptime(statusFileLines[2],"%m/%d/%Y %H:%M:%S")
            stopTime=strptime(statusFileLines[3],"%m/%d/%Y %H:%M:%S")
            timeDiff=as.numeric(difftime(stopTime,startTime))
            fileToRead=progressFiles[nxt]
            indx=indx+1
            firstTime=TRUE
DEBUG_MASTER(fileToRead)
            if ( file.exists(fileToRead) && file.info(fileToRead)$size != 0 ) {
                lines=readLines(fileToRead)
                if ( length(lines) > 2 ) {
                    nSubj=gsub(")","",unlist(strsplit(lines[1]," = "))[2],fixed=TRUE)
                    nObs=gsub(")","",unlist(strsplit(lines[2]," = "))[2],fixed=TRUE)
                    lines=lines[3:length(lines)]
                    numIterations=length(grep("Iteration",lines))
                    numTokens=as.integer(length(lines)/numIterations) - 1 
                    for ( it in 1:numIterations ) {
                        firstLine=(it - 1 ) * (numTokens + 1 )  + 1 
                        names=list(numTokens)
                        values=list(numTokens)
                        for ( indx in 1:numTokens ) {
                            l=lines[firstLine-1 + indx]
                            values[indx]=gsub(")","",unlist(strsplit(l," = "))[2],fixed=TRUE)
                            names[indx]=unlist(strsplit(unlist(strsplit(l," = "))[1],"(",fixed=TRUE))[2]

                            if ( names[indx] == "LL" ){
                                logLikeIndex=indx
                                names[indx] = "-2LL"
                            }
                        }
                        if( firstTime ) {
                            firstTime=FALSE
                            cat(sprintf("%s : %s",workflow_name,scenarioNames[nxt]),file=StatusFilename,sep="\n",append=appendFlag)
                            appendFlag=TRUE
                            cat(sprintf("Start %s  Run : %s nSubj: %s nObs : %s",startTime,timeDiff,nSubj,nObs),file=StatusFilename,sep="\n",append=TRUE)
                            for ( i in 1:numTokens ) {
                                cat(paste0(names[[i]],"\t"),file=StatusFilename,sep="",append=TRUE)
                            }
                            cat("",file=StatusFilename,sep="\n",append=TRUE)
                        }
                        for ( i in 1:numTokens ) {
                            if ( i == 1 )
                                cat("\t",file=StatusFilename,sep="",append=TRUE)
                            if ( logLikeIndex == i ) 
                                cat(paste0(( as.double(values[[i]]) * -2 ),"\t"),file=StatusFilename,sep="",append=TRUE)
                            else
                                cat(paste0(values[[i]],"\t"),file=StatusFilename,sep="",append=TRUE)
                       }
                       cat("",file=StatusFilename,sep="\n",append=TRUE)
                    }
                }
                else {
                    print(paste0("WARNING: File ",fileToRead," Does not have information"))
                }
            } else {
                if ( firstTime == TRUE ) {
                   nSubj = nObs = 0 
                   firstTime=FALSE
                   cat(sprintf("%s : %s",workflow_name,scenarioNames[nxt]),file=StatusFilename,sep="\n",append=appendFlag)
                   appendFlag=TRUE
                   cat(sprintf("Start %s  Run : %s nSubj: %s nObs : %s",startTime,timeDiff,nSubj,nObs),file=StatusFilename,sep="\n",append=TRUE)
                }
            }
        nxt = nxt + 1 
    }
},
error=function(ex){
print(ex)
})
}


removeTempWorkingDirectory <- function(jobHome,localWorkingDir) {

    flag=Sys.getenv("NLME_KEEP_GRID_RESULTS")
DEBUG_MASTER(paste("flag is :",flag))
	if ( flag != "TRUE")
    for ( t in 1:3 ) {
        tryCatch (
        {
        setwd(localWorkingDir)
DEBUG_MASTER(paste("CWD", getwd()))
DEBUG_MASTER(paste("Unlink try ",t,jobHome))
                ret=unlink(jobHome,recursive=TRUE, force=TRUE)
DEBUG_MASTER(paste("Ret from unlink",ret))
                if ( !file.exists(jobHome) ) {
                    t = 9999
                    break
                }
    	        Sys.sleep(5)
         },
         error=function(ex){
             Sys.sleep(5)
DEBUG_MASTER(paste("Unlink Again ",jobHome))
             unlink(jobHome,recursive=TRUE, force=TRUE)
         })
    }
}

waitTillAllJobsAreFinished <-function() {
DEBUG_MASTER(paste("waitTillAllJobsAreFinished()"))
    gridRegistry=get("gridRegistry",envir=nlmeEnv)
    stat = waitForJobs(reg=gridRegistry)
    running = findRunning(ids=NULL, reg=gridRegistry)
    expired = findExpired(ids=NULL, reg=gridRegistry)
    errors = findErrors(ids=NULL, gridRegistry)
    if ( nrow(running) > 0  )
        running=running$job.id
    flag=Sys.getenv("NLME_NO_ZOMBIE_CLEANUP")
    if ( flag != "TRUE" ) {
        jobsStr=""
        if (exists("gridJobs", envir = nlmeEnv))  {
            gridJobs=get("gridJobs",envir=nlmeEnv)

            for ( j in gridJobs ) {
                jobsStr=paste(jobsStr,j)
            }
        }
        if (exists("gridRegistry2", envir = nlmeEnv))  {
            gridJobs2=get("gridJobs2",envir=nlmeEnv)
            for ( j in gridJobs2 ) {
                jobsStr=paste(jobsStr,j)
            }
        }
        if ( jobsStr != "" ) {
        fileName="clean_zombies.ksh"
        cat("#!/bin/ksh",file=fileName,sep="\n",append=FALSE)
        cat("",file=fileName,sep="\n",append=TRUE)
        cat(sprintf("for j in %s ",jobsStr),file=fileName,sep="\n",append=TRUE)
        cat("do",file=fileName,sep="\n",append=TRUE)
        cat("ret=`qstat -j ${j} 2>/dev/null`",file=fileName,sep="\n",append=TRUE)
        cat("if [ \"${ret}\" ]",file=fileName,sep="\n",append=TRUE)
        cat("then",file=fileName,sep="\n",append=TRUE)
        cat("qdel ${j} 1>/dev/null 2>/dev/null",file=fileName,sep="\n",append=TRUE)
        cat("fi",file=fileName,sep="\n",append=TRUE)
        cat("done",file=fileName,sep="\n",append=TRUE)

        system(sprintf("chmod 777 ./%s ",fileName ))
        system(sprintf("./%s  ",fileName ))
        }
    }
    if ( jobsStr != "" ) {
        command=sprintf("rm  %s",fileName)
        DEBUG_MASTER(command)
        system(command)
    }
DEBUG_MASTER(paste("Done with waitTillAllJobsAreFinished()"))
}

#' NLME Bootstrap Function
#'
#' This function runs an NLME bootstrap job in parallel and produces summaries
#' @param ParalleMethod    How to run, MPI, Multicore, Grid
#' @param Install_Directory Directory where NLME libraries, scripts are
#' @param Shared_Directory Directory accessible by all nodes
#' @param Local_Working_Directory Where input files are and results will be stored
#' @param Optimization_Engine Which Pharsight NLME engine to use
#' @param Num_Iterations      Maximum iterations
#' @param Num_Samples         Number of bootstrap samples to run
#' @param Max_Tries           How many times to try a sample that does not converge
#' @param Model_Filename      PML model
#' @param Column_Definition_Filename Mapping of columns
#' @param Data_File           NLME data file
#' @param Start_Seed          Initial seed
#' @param Extra_Arguments_File Other parameters to pass to engines
#' @param List_of_Files_to_copy_to_shared_directory List of files to copy to the remote systems
#' @param Number_of_Cores_to_use Number of cores to use for multicore runs
#' @param Confidence_Level       Confidence level to summarize for
#' @keywords NLME, Bootstrap
#' @export
#' @examples
#' performBootstrap(method,install_directory, shared_directory localWorkingDir engine num_iterations num_samples max_tries model_file column_def_file data_file start_seed extra_args_file files_to_copy NumProc ConfidenceLevel)
#'

performBootstrap<-function(args,allowIntermediateResults=TRUE,reportProgress=FALSE){
DEBUG_MASTER("func --> performBootstrap() <---")

    cleanupFromPreviousRun()
    localWorkingDir= gsub("\\","/",args[4],fixed=TRUE)
    assign("localWorkingDir", localWorkingDir, envir=nlmeEnv)
    updateInitialStatus("Bootstrap",args[1],localWorkingDir)
    if ( length(args) != 16 && length(args) != 17 ) {
    print("USAGE:performBootstrap parallelMethod install_dir shared_directory localWorkingDir engine num_iterations num_samples max_tries model_file column_def_file data_file start_seed extra_args_file files_to_copy NumProc ConfidenceLevel workflowName")
        print(args)
    }
    else {
    tryCatch (
    {
        submissionPlatform=.Platform$OS.type
        assign("submissionPlatform", submissionPlatform, envir=nlmeEnv)
        if ( submissionPlatform == "windows" ) {
            exeFileExtension=".bat"
        } else {
            exeFileExtension=".sh"
        }
        assign("exeFileExtension", exeFileExtension, envir=nlmeEnv)
        parallelMethod=args[1]
        assign("parallelMethod", parallelMethod, envir=nlmeEnv)
        jobHome=tempfile(pattern="NLME",tmpdir=gsub("\\","/",args[3],fixed=TRUE))
        jobHome=gsub("\\","/",jobHome,fixed=TRUE)
        
        assign("jobHome", jobHome, envir=nlmeEnv)
       
        SharedWorkingDir=gsub("\\","/",tempfile(pattern="NLME",tmpdir=jobHome),fixed=TRUE)

        assign("SharedWorkingDir",SharedWorkingDir,envir=nlmeEnv)
#        dir.create(jobHome)
#        dir.create(SharedWorkingDir)
        mkdir(jobHome)
        mkdir(SharedWorkingDir)

        assign("engine", as.integer(args[5]), envir=nlmeEnv)
        assign("num_iterations", as.integer(args[6]), envir=nlmeEnv)
        num_samples= as.integer(args[7])
        assign("num_samples", num_samples, envir=nlmeEnv)
        assign("max_tries", as.integer(args[8]), envir=nlmeEnv)
        assign("model_file", args[9], envir=nlmeEnv)
        assign("column_def_file", args[10], envir=nlmeEnv)
        assign("data_file", args[11], envir=nlmeEnv)
        assign("start_seed", as.integer(args[12]), envir=nlmeEnv)
        assign("extra_args_file", gsub("\\","/",args[13],fixed=TRUE), envir=nlmeEnv)

        assign("files_to_copy", gsub("\\","/",args[14],fixed=TRUE), envir=nlmeEnv)
        arglist=unlist(strsplit(args[15], split=","))  
        num_processes =  as.integer(arglist[1])
        assign("num_processes", num_processes , envir=nlmeEnv)
        if  ( length(arglist) == 2 ) {
            num_cores =  as.integer(arglist[2])
        } else {
            num_cores = figureOutMpiNumCores(num_samples)
            if ( tolower(parallelMethod) == "local_mpi" ) 
                num_cores=num_processes
            else
                num_cores = figureOutMpiNumCores(num_samples)
        }
        assign("confidence_level", as.double(args[16]), envir=nlmeEnv)
        workflow_name = args[17]
        assign("workflow_name", workflow_name, envir=nlmeEnv)
        progress=list(MachineName="LocalHost", ParallelProtocol="None",ModelName="",StartTime="", EndTime="", Status="InProgress", NumOfSamples=0, NumOfSamplesCompleted=0,NumOfSamplesFailed= 0 , NumOfSamplesExpired = 0 ,NumOfSamplesErrored=0)
        progress$NumOfSamples = as.integer(args[7])
        progress$Status = "InProgress"
        progress$ParallelProtocol = parallelMethod
        progress$StartTime= getLocalTimeUTC()

        assign("ProgressStatus",progress,envir=nlmeEnv)

        MpiExecutable="NLME7.exe"
        MpiArgument="MPINO"
        MpiNumCores=1
        MpiLocal="NO"
        assign("MpiExecutable", MpiExecutable , envir=.GlobalEnv)
        assign("MpiArgument", MpiArgument , envir=.GlobalEnv)
        assign("MpiLocal", MpiLocal , envir=.GlobalEnv)
        assign("MpiNumCores", MpiNumCores , envir=.GlobalEnv)

        ReportProgress()
        RunningOnGrid = FALSE 
        stat = FALSE
        if ( tolower(parallelMethod) == "none" ) {
            done=1:num_samples
            ret = multiCoreGeneric(parallelMethod,"BOOTSTRAP",num_processes,reportProgress=reportProgress)
            stat = ret$stat
            done = ret$done
        } else if ( tolower(parallelMethod) == "multicore" ) {
            ret = multiCoreGeneric(parallelMethod,"BOOTSTRAP",num_processes,reportProgress=reportProgress)
            stat = ret$stat
            done = ret$done
            if ( !IsEarlyTerminationRequested() ) 
                done=1:num_samples
        } else if ( tolower(parallelMethod) == "local_mpi" ) {
            MpiExecutable="mpiNLME7.exe"
            MpiArgument="MPIYES"
            MpiLocal="YES"
            MpiNumCores=num_cores
            assign("MpiExecutable", MpiExecutable , envir=.GlobalEnv)
            assign("MpiArgument", MpiArgument , envir=.GlobalEnv)
            assign("MpiLocal", MpiLocal , envir=.GlobalEnv)
            assign("MpiNumCores", MpiNumCores , envir=.GlobalEnv)
            ret = multiCoreGeneric(parallelMethod,"BOOTSTRAP",num_processes,allowIntermediateResults,progressStage=progressStage,reportProgress=reportProgress)
            done=1:num_samples
            stat = ret$stat
            done = ret$done
        } else if ( tolower(parallelMethod) == "mpi" ) {
            if ( isMPIEnabled() == FALSE ) {
                stop("MPI is not enabled on this machine!!")
            }
            setwd(SharedWorkingDir)
            IsMpiInitialized=get("IsMpiInitialized",envir=nlmeEnv)
            DEBUG_MASTER(paste("Value is ",IsMpiInitialized))
            if ( FALSE ){
                DEBUG_MASTER("MPI is already initialized!!")
            }
            else {
                initializeMPI(num_processes, args[3])
                IsMpiInitialized=TRUE
                assign("IsMpiInitialized", IsMpiInitialized , envir=nlmeEnv)
            }
            getUniqueHostNames()
            ret = multiCoreGeneric(parallelMethod,"BOOTSTRAP",num_processes,reportProgress=reportProgress)
            stat = ret$stat
            done = ret$done
            done=1:num_samples
        } else {
            RunningOnGrid = TRUE 
            if ( ( tolower(parallelMethod) == "torque_mpi" || tolower(parallelMethod) == "sge_mpi") || tolower(parallelMethod) == "lsf_mpi" ) {
                MpiExecutable="mpiNLME7.exe"
                MpiArgument="MPIYES"
                MpiLocal="NO"
                MpiNumCores=num_cores
                assign("MpiExecutable", MpiExecutable , envir=.GlobalEnv)
                assign("MpiArgument", MpiArgument , envir=.GlobalEnv)
                assign("MpiLocal", MpiLocal , envir=.GlobalEnv)
                assign("MpiNumCores", MpiNumCores , envir=.GlobalEnv)
            }
            else {
                MpiExecutable="NLME7.exe"
                MpiArgument="MPINO"
                MpiNumCores=1
                MpiLocal="NO"
                assign("MpiExecutable", MpiExecutable , envir=.GlobalEnv)
                assign("MpiArgument", MpiArgument , envir=.GlobalEnv)
                assign("MpiLocal", MpiLocal , envir=.GlobalEnv)
                assign("MpiNumCores", MpiNumCores , envir=.GlobalEnv)
            }
GlobalSummaryLine1=sprintf("")
assign("GlobalSummaryLine1", GlobalSummaryLine1, envir=nlmeEnv)
            ret = startGenericGridJob("BOOTSTRAP",allowIntermediateResults,progressStage="Processing Replicates" , reportProgress=reportProgress )
            gridRegistry=get("gridRegistry",envir=nlmeEnv)
            done=ret$done
            stat=ret$stat
        }
        if (  stat == TRUE && !IsJobCanceled() ) {
            generateBootstrapResults( localWorkingDir, done, confidence_level)

        }
        if ( stat == FALSE ) {
            done=1:num_samples
            collectJobErrors(localWorkingDir, "BOOTSTRAP", done, control_lines)
        }
        if ( !IsJobCanceled() ) {
            if ( stat == FALSE )
                FailProgress()
            else
                CompleteProgress()
        }
        if (  IsJobCanceled() || IsEarlyTerminationRequested() ) {
            if ( RunningOnGrid == TRUE )
                killAGridJob(gridRegistry)
        }
        progress = get("ProgressStatus",envir=nlmeEnv)
        flag=Sys.getenv("NLME_KEEP_GRID_RESULTS")
        if ( RunningOnGrid ) {

waitTillAllJobsAreFinished()
            if ( flag != "TRUE" ) {
                if ( progress$NumOfSamplesFailed == 0 && progress$NumOfSamplesExpired == 0 ) {
                    for ( t in 1:2 ) {
                        tryCatch (
                        {
                             DEBUG_MASTER("removeRegistry()")
                             removeRegistry(gridRegistry)
                             t=999
                             break
                        },
                        error=function(ex){
    	                    Sys.sleep(1)
                        })
                    }
                }
            }
        }

        if ( tolower(parallelMethod) == "mpi" )
        {
DEBUG_MASTER("mpi.exit()")
            mpi.exit()
            IsMpiInitialized=FALSE
            assign("IsMpiInitialized", IsMpiInitialized , envir=nlmeEnv)
        }
        if ( progress$NumOfSamplesFailed == 0 && progress$NumOfSamplesExpired == 0 && ( flag != "TRUE" ) ) {
            removeTempWorkingDirectory(jobHome,localWorkingDir)

        }
    },
        error=function(ex){
        DEBUG_MASTER("Failed to performBootstrap()")
        DEBUG_MASTER(paste("Error is : ",ex))
        print("Failed to performBootstrap()")
        print(paste("Error is : ",ex))
        FailProgress()
    })
    }
}

#'
#' @export parseControlFile
#'
parseControlFile <-function(controlFile)
{
    lines=readLines(controlFile)
    modelFile=lines[1]
    filesToCopy=lines[2]
    colFile=unlist(strsplit(filesToCopy," "))[2]
    dataFile=unlist(strsplit(filesToCopy," "))[3]
    controlLine=lines[5]
    argsFile=unlist(strsplit(controlLine,","))[2]
    argsFile=unlist(strsplit(argsFile,":"))[1]
    outputFile=unlist(strsplit(controlLine,","))[4]
    lines=readLines(argsFile)
    for ( l in lines ) {
        pos=unlist(strsplit(l,split="/m "))
        if ( length(pos) > 1 ) 
        {
            method=unlist(strsplit(pos[2],split=" "))[1]
        }
        pos=unlist(strsplit(l,split="/n "))
        if ( length(pos) > 1 ) 
        {
            iterations=unlist(strsplit(pos[2],split=" "))[1]
        }
    }
    return(c(modelFile,dataFile,colFile,argsFile,outputFile,filesToCopy,
             method,iterations))
}


#'
#' @export
#'
performBootstrap2<-function(args,allowIntermediateResults=TRUE,reportProgress=FALSE)
{
    cleanupFromPreviousRun()
    controlFile=args[[6]]
    stuff=parseControlFile(controlFile)
    modelFile=stuff[1]
    colDefFile=stuff[2]
    dataFile=stuff[3]
    extraArgsFile=stuff[4]
    outputFile=stuff[5]
    filesToCopy=stuff[6]
    engine=stuff[7]
    numIterations=stuff[8]
    numSamples=args[7]
    initialEstimates=args[8]
    maxRetries=args[9]
    startSeed=args[10]
    numProc=args[4]
    confidenceLevel=args[11]
    workflow_name = args[12]
    assign("workflow_name", workflow_name, envir=nlmeEnv)
    newArgs=c()
    newArgs=c(newArgs,args[1])
    newArgs=c(newArgs,args[2])
    newArgs=c(newArgs,args[3])
    newArgs=c(newArgs,args[5])
    newArgs=c(newArgs,engine)
    newArgs=c(newArgs,numIterations)
    newArgs=c(newArgs,numSamples)
    newArgs=c(newArgs,maxRetries)
    newArgs=c(newArgs,modelFile)
    newArgs=c(newArgs,colDefFile)
    newArgs=c(newArgs,dataFile)
    newArgs=c(newArgs,startSeed)
    newArgs=c(newArgs,extraArgsFile)
    newArgs=c(newArgs,filesToCopy)
    newArgs=c(newArgs,numProc)
    newArgs=c(newArgs,confidenceLevel)
    newArgs=c(newArgs,workflow_name)


    performBootstrap(newArgs,allowIntermediateResults,reportProgress)
}


#
# This method copies all files from grid/remote directory and summerizes the
# bootstrap run result
#
generateBootstrapResults <-function( localDir, jobList, confidence_level ) {
DEBUG_MASTER("generateBootstrapResults()")
    collectJobResults(jobList)

    results_file_list=c("out.bin.txt", "out.txt", "BootSubj.csv","out.csv" , "VarCoVar.csv","doses.csv", "iniest.csv", "err2.txt", "err.txt", "IniCovr.txt", "MultCovr.txt", "progress.txt", "IdEta.txt","dmp.txt","EtaEta.txt","EtaCov.txt","StrCov.txt","test_new.mdl","progress.xml","out_initialEstimates.txt","nlme7engine.log","EtaShrinkageBySubject","bluptable.dat","nparsupport.asc","etameansnp.asc")
        
    assign("results_file_list", results_file_list, envir=nlmeEnv)
    copyResults(localDir,TRUE)

    summarizeBootstrap(localDir,confidence_level)
}

#
# This method copies all error files from grid/remote directory 
# 
#
collectJobErrors<-function(localDir,jobType,jobList,control_lines,stepwiseSummary = FALSE ){

DEBUG_MASTER("collectJobErrors()")
DEBUG_MASTER(paste(jobList))

    setwd(localDir)

    copyFilesFlag = TRUE
    tryCatch (
    {
        collectJobErrorLogs(localDir,jobList,copyFilesFlag)

    },
        error=function(ex){
        DEBUG_MASTER("Failed to collectJobErrors()")
        DEBUG_MASTER(paste("Error is : ",ex))
    })
DEBUG_MASTER(" END collectJobErrors()")

}

#
# Looks for the presence of err2.txt and print it if it exist
#
listJobErrors<-function(localDir,jobType,num_samples,control_lines,stepwiseSummary = FALSE ){

DEBUG_MASTER("listJobErrors()")

    jobList=1:num_samples
    SharedWorkingDir=get("SharedWorkingDir",envir=nlmeEnv)
    baseDirectory = getBatchDirectoryLocation(SharedWorkingDir)
    for ( job in jobList ) {
        baseIndx=job %% 100
        wd=sprintf("%s/jobs/%02d/%d/",baseDirectory,baseIndx,job)
        #
        # Check to see if there are any error logs from executing NLME7
        # If there are errors, just dump them so they get recorded in the log
        # file to help user figure out why a replicate failed
        #
        fileToCheck = sprintf("%s/err2.txt",wd)
        if ( file.exists(fileToCheck) ) {
            lines=readLines(fileToCheck)
            if ( length(lines) != 0 ) {
                print(paste("ERRORS in replicate",job))
                for ( l in lines )
                    print(l)
            }
        }
    }


}


#
# This method copies all files from grid/remote directory and summerizes the
# generic run result
#
generateJobResults<-function(localDir,jobType,jobList,control_lines,stepwiseSummary = FALSE ){

DEBUG_MASTER("generateJobResults()")
DEBUG_MASTER(paste(jobList))

    setwd(localDir)


    copyFilesFlag = FALSE
    if ( jobType == "GENERIC" ) 
        copyFilesFlag = TRUE
    tryCatch (
    {
        collectJobResultsGeneric(jobList,copyFilesFlag)

        results_file_list=getGenericResultsList(control_lines) 
        assign("results_file_list", results_file_list, envir=nlmeEnv)
        copyResults(localDir,copyFilesFlag)

        if ( jobType == "GENERIC" ) {
            jobsList=c(1)
            summarizeSimpleEstimation(localDir,jobList,control_lines)
        }
        if ( jobType == "COVAR_SEARCH" ) {
            summarizeCovarSearch(localDir,jobList,control_lines)
        }
        if ( jobType == "ESTIMATION_RUN" ) {
            # grab out.txt and nlme7engine.log file for last job
            job=jobList[length(jobList)]
            collectJobStatusAndCoreFiles(job,c("nlme7engine.log",getRunSuccessFilename(control_lines[job])),localDir)
            summarizeSortByIdEstimation(localDir,jobList,control_lines)
        }
        if ( jobType == "PROFILE_RUN" ) {
            # grab out.txt and nlme7engine.log file for last job
            job=jobList[length(jobList)]
            collectJobStatusAndCoreFiles(job,c("nlme7engine.log",getRunSuccessFilename(control_lines[job])),localDir)
            summarizeProfileEstimation(localDir,jobList,control_lines)
        }
        if ( jobType == "STEPWISE_SEARCH" ) {
            if ( stepwiseSummary == TRUE ) {
                scenarios = get("scenarios",envir=nlmeEnv)
                summarizeStepwiseCovarSearch(localWorkingDir,scenarios)
            }
        }
    },
        error=function(ex){
        DEBUG_MASTER("Failed to generateJobResults()")
        DEBUG_MASTER(paste("Error is : ",ex))
    })
DEBUG_MASTER(" END generateJobResults()")

}

#
# Get the number of subjects in a dataset
#
getNumSubjects <- function(colDefFile,dataFile){

    lines=readLines(colDefFile)
    lineNo=grep("id",lines)
    numSubjects = 0
    if ( length( lineNo) > 0 ) {
        col=unlist(strsplit(lines[lineNo],"\""))[2]
        if ( col != "" ) {
            data=read.csv(dataFile)
            cols=colnames(data)
            cols[1]=gsub("^#+","",cols[1],fixed=FALSE)
            cols[1]=gsub("X..","",cols[1],fixed=FALSE)
            colnames(data) = cols
            numSubjects=length(unique(data[[col]]))
        }
    }
    return(numSubjects)
}

#
# Get the smallest population in a run
#
getMinimumNumSubjects<- function(controlFile){
    lines=readLines(controlFile)
    numReplicates=as.integer(lines[4])
    smallestPopulation=9999999
    for ( n in 1:numReplicates ) {
        extraArgsFile =getExtraArgumentFilename(lines[n+4])
        extraArgsFileIndx =getExtraArgumentFilenameIndex(lines[n+4])
        idx=as.integer(extraArgsFileIndx)
        extraLines=readLines(extraArgsFile)
        numLines=length(extraLines)
        numLinesPerRecord=numLines/length(grep("-anagrad",extraLines))
        colnameFile="cols1.txt"
        dataFilename="data1.txt"
        for ( l in 1:numLinesPerRecord ) {
             isThere=grep("data1",extraLines[(idx-1)*numLinesPerRecord + l ])
             if ( length(isThere) == 1  ){
                 tokens=unlist(strsplit(extraLines[(idx-1)*numLinesPerRecord + l ]," "))
                 for ( t in tokens ) {
                     if ( length(grep("data1",t)) != 0 )
                         dataFilename=t
                 }
             }
        }
        num=getNumSubjects(colnameFile,dataFilename)
        if ( num < smallestPopulation )
            smallestPopulation = num
    }
    return(smallestPopulation)
}

#
# Dummy entry to test
#
testPerformParallelNLMERun <-function(args){
performParallelNLMERun(args)
}


#' NLME Generic run Function
#'
#' This function runs a set of  NLME jobs in parallel 
#' @param JobType          COVAR_SEARCH, GENERIC, 
#' @param ParalleMethod    How to run, MPI, Multicore, Grid
#' @param Install_Directory Directory where NLME libraries, scripts are
#' @param Shared_Directory Directory accessible by all nodes
#' @param Local_Working_Directory Where input files are and results will be stored
#' @param Control file File containg instructions how to run
#' @param Number_of_Cores_to_use Number of cores to use for multicore runs
#' Control file format :
#' Line1 : name of pml model file
#' Line2 : Comma separated list of input files to copy to remote disk
#' Line3 : Comma separated list of outpu files to grab from NLME build processes
#' Line4 : Number of NLME jobs to run
#' Line5..LineN : comma separated parameters for each run
#'       JobName/ScenarioName
#'       Main NLME run control file(i.e. nlmeargs.txt)
#'       Extra arguments to pass to NLME7.exe ( optional ) 	
#'       Filename to check to declare the run a success(i.e. out.txt )
#'       Optional Space sepaated List of out fiels to return from each run
#'       Optional Space sepaated List of out files to return from each run
#'       ( These will be tagged with the jobname )
#'
#'
#' @keywords NLME, Bootstrap
#' @export
#' @examples
#' performParallelNLMERun(method,install_directory, shared_directory localWorkingDir ControlFile NumProc )
#'

performParallelNLMERun<-function(args,partialJob=FALSE,allowIntermediateResults=TRUE,progressStage="",func="",func_arg=NULL,reportProgress=FALSE){

DEBUG_MASTER("func --> performParallelNLMERun()")
DEBUG_MASTER(args[1])
DEBUG_MASTER(args[2])
DEBUG_MASTER(args[3])
DEBUG_MASTER(args[4])
DEBUG_MASTER(args[5])
DEBUG_MASTER(args[6])
DEBUG_MASTER(args[7])
DEBUG_MASTER(partialJob)
DEBUG_MASTER(allowIntermediateResults)
DEBUG_MASTER(progressStage)
DEBUG_MASTER(func)
DEBUG_MASTER(func_arg)

    if ( length(args) != 8 ) {
        print("USAGE:performParallelNLMERun.r jobType parallelMethod install_dir shared_directory localWorkingDir controlFile NumProc workflow_name")
    }
    else {
        generateProfileModels()
        reg.finalizer(nlmeEnv, nlmeEnvIsDone, onexit = TRUE ) 
        submissionPlatform=.Platform$OS.type
        assign("submissionPlatform", submissionPlatform, envir=nlmeEnv)
        if ( submissionPlatform == "windows" ) {
            exeFileExtension=".bat"
        } else {
            exeFileExtension=".sh"
        }
        assign("exeFileExtension", exeFileExtension, envir=nlmeEnv)
        jobType=args[1]
        parallelMethod=args[2]
        assign("parallelMethod", parallelMethod, envir=nlmeEnv)
        jobHome=tempfile(pattern="NLME",tmpdir=gsub("\\","/",args[4],fixed=TRUE))
        jobHome=gsub("\\","/",jobHome,fixed=TRUE)
        
        assign("jobHome", jobHome, envir=nlmeEnv)
       
        SharedWorkingDir=gsub("\\","/",tempfile(pattern="NLME",tmpdir=jobHome),fixed=TRUE)


        assign("SharedWorkingDir",SharedWorkingDir,envir=nlmeEnv)
#        dir.create(jobHome)
#        dir.create(SharedWorkingDir)
        mkdir(jobHome)
        mkdir(SharedWorkingDir)



        localWorkingDir= gsub("\\","/",args[5],fixed=TRUE)
        assign("localWorkingDir", localWorkingDir, envir=nlmeEnv)
        TrackingJobHistoryInitialization()

        control_file=args[6]
        assign("control_file", control_file , envir=nlmeEnv)
        lines = readLines(control_file)
        control_lines=lines[5:length(lines)]
        assign("control_lines", control_lines , envir=nlmeEnv)
        model_file=lines[1]
        assign("model_file", model_file , envir=nlmeEnv)
        files_to_copy=lines[2]
        assign("files_to_copy", files_to_copy , envir=nlmeEnv)
        files_to_return=lines[3]
        assign("files_to_return", files_to_return , envir=nlmeEnv)
        num_samples = as.integer(lines[4])
        assign("num_samples", num_samples , envir=nlmeEnv)

        extra_args_file=getExtraArgumentFilename(control_lines[1])
        assign("extra_args_file", gsub("\\","/",extra_args_file,fixed=TRUE), envir=nlmeEnv)
        arglist=unlist(strsplit(args[7], split=","))  

        num_processes =  as.integer(arglist[1])
        assign("num_processes", num_processes , envir=nlmeEnv)
        if  ( length(arglist) == 2 ) {
            num_cores =  as.integer(arglist[2])
        } else {
            if ( tolower(parallelMethod) == "local_mpi" ) 
                num_cores=num_processes
            else
                num_cores = figureOutMpiNumCoresForPop(num_samples,control_file)
        }

        assign("workflow_name",args[8],envir=nlmeEnv)
DEBUG_MASTER(paste("performParallel workflow_name",args[8]))
        progress=list(MachineName="LocalHost", ParallelProtocol="None",ModelName=args[8],StartTime="", EndTime="", Status="InProgress", NumOfSamples=0, NumOfSamplesCompleted=0,NumOfSamplesFailed= 0 , NumOfSamplesExpired = 0 , NumOfSamplesErrored=0)
        progress$NumOfSamples = num_samples
        progress$Status = "InProgress"
        progress$ParallelProtocol = "GRID"
        progress$ParallelProtocol = parallelMethod
        progress$StartTime= getLocalTimeUTC()

        assign("ProgressStatus",progress,envir=nlmeEnv)
        ReportProgress()
        RunningOnGrid = FALSE 
        stat = FALSE
        MpiExecutable="NLME7.exe"
        MpiArgument="MPINO"
        MpiNumCores=1
        MpiLocal="NO"
        assign("MpiExecutable", MpiExecutable , envir=.GlobalEnv)
        assign("MpiArgument", MpiArgument , envir=.GlobalEnv)
        assign("MpiLocal", MpiLocal , envir=.GlobalEnv)
        assign("MpiNumCores", MpiNumCores , envir=.GlobalEnv)
        if ( tolower(parallelMethod) == "none" ) {
            UsingBatchTools=FALSE
            assign("UsingBatchTools", UsingBatchTools, envir=nlmeEnv)
            done=1:num_samples
            ret = multiCoreGeneric(parallelMethod,jobType,num_processes,allowIntermediateResults,progressStage=progressStage,reportProgress=reportProgress)
            stat = ret$stat
            done = ret$done
        } else if ( tolower(parallelMethod) == "multicore" ) {
            ret = multiCoreGeneric(parallelMethod,jobType,num_processes,allowIntermediateResults,progressStage=progressStage,reportProgress=reportProgress)
            done=1:num_samples
            stat = ret$stat
            done = ret$done
        } else if ( tolower(parallelMethod) == "local_mpi" ) {
DEBUG_MASTER("*********************")
DEBUG_MASTER("Local MPI")
DEBUG_MASTER(parallelMethod)
            MpiExecutable="mpiNLME7.exe"
            MpiArgument="MPIYES"
            MpiLocal="YES"
            MpiNumCores=num_cores
            assign("MpiExecutable", MpiExecutable , envir=.GlobalEnv)
            assign("MpiArgument", MpiArgument , envir=.GlobalEnv)
            assign("MpiLocal", MpiLocal , envir=.GlobalEnv)
            assign("MpiNumCores", MpiNumCores , envir=.GlobalEnv)
            ret = multiCoreGeneric(parallelMethod,jobType,num_processes,allowIntermediateResults,progressStage=progressStage,reportProgress=reportProgress)
            done=1:num_samples
            stat = ret$stat
            done = ret$done
        } else if ( tolower(parallelMethod) == "mpi" ) {
            UsingBatchTools=FALSE
            assign("UsingBatchTools", UsingBatchTools, envir=nlmeEnv)
            if ( isMPIEnabled() == FALSE ) {
                stop("MPI is not enabled on this machine!!")
            }
            cwd=getwd()
            setwd(SharedWorkingDir)
            IsMpiInitialized=get("IsMpiInitialized",envir=nlmeEnv)
            DEBUG_MASTER(paste("Value is ",IsMpiInitialized))
            if ( FALSE ) {
                DEBUG_MASTER("MPI is already initialized!!")
            }
            else {
                initializeMPI(num_processes, args[4])
                IsMpiInitialized=TRUE
                assign("IsMpiInitialized", IsMpiInitialized , envir=nlmeEnv)
            }
            setwd(cwd)
            getUniqueHostNames()
            ret = multiCoreGeneric(parallelMethod,jobType,num_processes,allowIntermediateResults,reportProgress=reportProgress)
            done=1:num_samples
            stat = ret$stat
            done = ret$done
        } else {
            if ( ( tolower(parallelMethod) == "torque_mpi" || tolower(parallelMethod) == "sge_mpi") || tolower(parallelMethod) == "lsf_mpi" ) {
                MpiExecutable="mpiNLME7.exe"
                MpiArgument="MPIYES"
                MpiLocal="NO"
                MpiNumCores=num_cores
                assign("MpiExecutable", MpiExecutable , envir=.GlobalEnv)
                assign("MpiArgument", MpiArgument , envir=.GlobalEnv)
                assign("MpiLocal", MpiLocal , envir=.GlobalEnv)
                assign("MpiNumCores", MpiNumCores , envir=.GlobalEnv)
            }
            else {
                MpiExecutable="NLME7.exe"
                MpiArgument="MPINO"
                MpiNumCores=1
                MpiLocal="NO"
                assign("MpiExecutable", MpiExecutable , envir=.GlobalEnv)
                assign("MpiArgument", MpiArgument , envir=.GlobalEnv)
                assign("MpiLocal", MpiLocal , envir=.GlobalEnv)
                assign("MpiNumCores", MpiNumCores , envir=.GlobalEnv)
            }
            RunningOnGrid = TRUE 
            ret = startGenericGridJob(jobType,allowIntermediateResults,progressStage=progressStage,reportProgress=reportProgress)
            stat = ret$stat
            done = ret$done
DEBUG_MASTER(paste("Status",ret$stat))
DEBUG_MASTER(paste("done",ret$done))
waitTillAllJobsAreFinished()

        }
DEBUG_MASTER("Trying to generate job results...")
        if ( stat == TRUE && !IsJobCanceled() ) {
if ( length(done) == 0 ) 
    done=1:num_samples
            generateJobResults(localWorkingDir, jobType, done, control_lines)
            listJobErrors(localWorkingDir, jobType, num_samples, control_lines)
        }
        if ( stat == FALSE ) {
            done=1:num_samples
            collectJobErrors(localWorkingDir, jobType, done, control_lines)
        }
DEBUG_MASTER("Done generating job results...")

        if ( partialJob == FALSE && !IsJobCanceled() ) {
            if ( stat == FALSE ) 
                FailProgress()
            else
                CompleteProgress()
        }
     
        if (  IsJobCanceled() || IsEarlyTerminationRequested() ) {
            if ( RunningOnGrid == TRUE )
                killAGridJob(gridRegistry)
        }
        progress = get("ProgressStatus",envir=nlmeEnv)
        flag=Sys.getenv("NLME_KEEP_GRID_RESULTS")
        if ( func != "" ) {
            do.call(func,list(func_arg))
        }
        if ( RunningOnGrid ) {
            if ( flag != "TRUE" ) {
                if ( progress$NumOfSamplesFailed == 0 && progress$NumOfSamplesExpired == 0 ) {
                    for ( t in 1:1 ) {
                        tryCatch (
                        {
                             DEBUG_MASTER(paste("removeRegistry()",flag))
                             removeRegistry(reg=gridRegistry)
                             t=999
                             break
                        },
                        error=function(ex){
    	                    Sys.sleep(1)
                        })
                    }
                }
            }
        }
        if ( tolower(parallelMethod) == "mpi" && partialJob ==  FALSE ) 
        {
DEBUG_MASTER("mpi.exit()")
            mpi.exit()
            IsMpiInitialized=FALSE
            assign("IsMpiInitialized", IsMpiInitialized , envir=nlmeEnv)
        }
        if ( partialJob == FALSE ) {
        if ( progress$NumOfSamplesFailed == 0 && progress$NumOfSamplesExpired == 0 && ( flag != "TRUE" ) ) {
            removeTempWorkingDirectory(jobHome,localWorkingDir)

        }
        }

    }
DEBUG_MASTER("END OF performParallelNLMERun()")
    return(jobHome)
}

generateCovarSearchArgsFile <- function(controlFilename, nlmeArgsFile , modelFilename, nlmeArgsFilename, inputFileArray, numCovariates, covarNamesArray)
{
DEBUG_MASTER("generateCovarSearchArgsFile")
    appendFlag=FALSE
    nlmeArgLines=readLines(nlmeArgsFilename)
    numTodo = as.integer(numCovariates)
    submodels=c()
    for ( i in 1:numTodo ){
        submodels=c(submodels,FALSE)
    }
    cat(modelFilename,file=controlFilename,sep="\n",append=FALSE)
    for ( ifn in inputFileArray ) {
        cat(ifn,file=controlFilename,sep=" ",append=TRUE)
    }
    cat("",file=controlFilename,sep="\n",append=TRUE)
    cat("*.csv *.txt *.log *.LOG",file=controlFilename,sep="\n",append=TRUE)
    cat((numTodo * numTodo ) ,file=controlFilename,sep="\n",append=TRUE)
    nxtScenario=0
    scenarioBase="cshot"
    outputBase="out"
    done = FALSE
    while ( TRUE ) {
        nxtScenarioName=sprintf("%s%03d",scenarioBase,nxtScenario)
        suffix=""
        nxtScenarioDescription=""
        for ( i in 1:numTodo ) {
            if ( submodels[i] == TRUE ) 
                suffix = paste0(suffix , "1")
            else
                suffix = paste0(suffix , "0" )
        }
        argFlag=""
        na = unlist(strsplit(covarNamesArray, split=" "))

        for ( i in 1:numTodo ) {
            if ( submodels[i] == TRUE ) {
                nxtScenarioDescription=paste0(nxtScenarioDescription, " " ,na[i])
                argFlag=paste0(argFlag,"_",(i-1))
            }
        }
        argFlag=paste0(argFlag,"_")
        outputFile=paste0(outputBase,suffix,".txt")
        for ( i in 1:numTodo ) {
            submodels[i] = !submodels[i]
            if ( submodels[i] ) 
                break
        }
        cat(nxtScenarioName,file=controlFilename,sep="",append=TRUE)
        cat(nxtScenarioDescription,file=controlFilename,sep=",",append=TRUE)
        cat(sprintf(",%s:%d,",nlmeArgsFile,nxtScenario+1),file=controlFilename,sep=",",append=TRUE)
        cat(sprintf(",%s,%s,progress.txt",outputFile,outputFile),file=controlFilename,sep="\n",append=TRUE)
        
        cat(sprintf("/xe %s",argFlag), file=nlmeArgsFile, sep="\n",append=appendFlag)
        appendFlag=TRUE
        for ( l in nlmeArgLines ) {
            l=gsub("out.txt","",l,fixed=TRUE) # get read of out.txt if there is any
            cat(l, file=nlmeArgsFile, sep="\n",append=appendFlag)
        }
        cat(sprintf(" %s",outputFile), file=nlmeArgsFile, sep="\n",append=appendFlag)
        if ( done == TRUE ) 
            break
        nxtScenario = nxtScenario + 1 
        n = 0 
        for ( i in 1:numTodo ) {
            if ( submodels[i] ) 
                n=n+1
        }
        if ( n == numTodo  ) 
            done=TRUE
    }
}


#' NLME shotgun covariate search
#'
#' This function runs a shotgun covariate NLME job in parallel 
#' @param ParalleMethod    How to run, MPI, Multicore, Grid
#' @param Install_Directory Directory where NLME libraries, scripts are
#' @param Shared_Directory Directory accessible by all nodes
#' @param Local_Working_Directory Where input files are and results will be stored
#' @param ModelFile        Filename of the model
#' @param NlmeArgsFile     Arguments to NLME executable
#' @param InputFiles       List of input files
#' @param Num_Covariates   Number of covariates
#' @param Covariate_Names  Array of covariate names
#' @param Num_Of_Cores     Number of compute nodes to use
#' @param Workflow_Name    Name of workflow to use in summary report
#'
#' @keywords NLME, ShotgunCovariateSearch
#' @export
#' @examples
#' performShotgunCovarSearch(method, install_directory, shared_directory localWorkingDir modelFile nlmeArgsFile numCovariates CovariateNames NumProc )
#'
#'
performShotgunCovarSearch<-function(args,reportProgress=FALSE){
DEBUG_MASTER("func --> performShotgunCovarSearch()")

    localWorkingDir= gsub("\\","/",args[4],fixed=TRUE)
    assign("localWorkingDir", localWorkingDir, envir=nlmeEnv)
    updateInitialStatus("Shotgun Covariate Search",args[1],localWorkingDir)
    if ( length(args) != 11 ) {
        print("USAGE:method, install_directory, shared_directory localWorkingDir modelFile nlmeArgsFile numCovariates CovariateNames NumProc")
    }
    else {
    tryCatch (
    {
        controlFilename = "nlmeControlFile.txt"
        argsFileName = "nlmeargsCombined.txt" 
        generateCovarSearchArgsFile(controlFilename, argsFileName, args[5],args[6],args[7],args[8],args[9])
        argList=c()
        argList=c(argList,"COVAR_SEARCH")
        argList=c(argList,args[1])
        argList=c(argList,args[2])
        argList=c(argList,args[3])
        argList=c(argList,args[4])
        argList=c(argList,controlFilename)
        argList=c(argList,args[10])
        argList=c(argList,args[7])
        workflow_name=args[7]
        assign("workflow_name", workflow_name, envir=nlmeEnv)
DEBUG_MASTER(paste("workflowname",workflow_name))
        performParallelNLMERun(argList,progressStage="Processing Scenarios",reportProgress=reportProgress)

    },
    error=function(ex){
        print(ex)
        FailProgress()
    })
    }
}


#
#  Generate a line for the selected list of scenarios
#
# Returns : A list structure for this scenario
#           
#    Scenario data structure
#    line                : Line that will be used with NLME args file
#    key                 : 0000000  key for this scenario
#    outputFilename      : Name of the output file
#    scenarioDescription : 
#    scenarioName        :
#    status              : 
#    logLikelihood       :
#
generateSelCovarSearchArgsLine <- function(controlFilename, nlmeArgsFile , modelFilename, nlmeArgsFilename, inputFileArray, numCovariates, covarNamesArray,submodels,nxtScenario,appendFlag,argsIndex)
{
    nlmeArgLines=readLines(nlmeArgsFilename)
    numTodo = as.integer(numCovariates)
    scenarioBase="cstep"
    outputBase="out"
    scenarios = get("scenarios",envir=nlmeEnv)
    {
        nxtScenarioName=sprintf("%s%03d",scenarioBase,nxtScenario)
        suffix=""
        nxtScenarioDescription=""
        for ( i in 1:numTodo ) {
            if ( submodels[i] == TRUE ) 
                suffix = paste0(suffix , "1")
            else
                suffix = paste0(suffix , "0" )
        }
        if ( length(scenarios[[suffix]] ) != 0 ) {
            if ( scenarios[[suffix]]$status == "Completed" ) 
            return(scenarios[[suffix]])
        }
        argFlag=""
        na = unlist(strsplit(covarNamesArray, split=" "))

        for ( i in 1:numTodo ) {
            if ( submodels[i] == TRUE ) {
                nxtScenarioDescription=paste0(nxtScenarioDescription, " " ,na[i])
                argFlag=paste0(argFlag,"_",(i-1))
            }
        }
        argFlag=paste0(argFlag,"_")
        outputFile=paste0(outputBase,suffix,".txt")
        for ( i in 1:numTodo ) {
            submodels[i] = !submodels[i]
            if ( submodels[i] ) 
                break
        }
        line=nxtScenarioName
        line=paste(line,nxtScenarioDescription,sep=" ")
        line=paste(line,sprintf(",%s:%d,",nlmeArgsFile,argsIndex),sep=" ")
        line=paste(line,sprintf(",%s,%s,progress.txt",outputFile,outputFile),sep=" ")
#        cat(sprintf(",%s,%s,progress.txt",outputFile,outputFile),file=controlFilename,sep="\n",append=TRUE)

        cat(sprintf("/xe %s",argFlag), file=nlmeArgsFile, sep="\n",append=appendFlag)
        appendFlag=TRUE
        for ( l in nlmeArgLines ) {
            l=gsub("out.txt","",l,fixed=TRUE) # get read of out.txt if there is any
            cat(l, file=nlmeArgsFile, sep="\n",append=appendFlag)
        }
        cat(sprintf(" %s",outputFile), file=nlmeArgsFile, sep="\n",append=appendFlag)
    }
    return(list(line=line,
                index=nxtScenario+1,
                key= suffix,
                outputFilename=outputFile,
                scenarioDescription=nxtScenarioDescription,
                scenarioName=nxtScenarioName,
                status="Initialized",
                returnCode=0 ,
                logLike=0.0 ,
                logLikelihood=0.0 ,
                shrinkage=0.0 ,
                nParam=0,
                nObs=0 ,
                nSub=0 ,
                AIC=0.0 ,
                BIC=0.0 ))
}




#
# This function Generates a control file to run initial scenarios for
# stepwise covariate search
# 
# Returns : Name of the output files to generate
#
generateInitialScenarios <- function(controlFilename, nlmeArgsFile , modelFilename, nlmeArgsFilename, inputFileArray, numCovariates, covarNamesArray, nxtScenario)
{
    outputFilenames=c()
    indexes=c()
#    scenarios=c()
#    assign("scenarios", scenarios, envir=nlmeEnv)
    scenarios = get("scenarios",envir=nlmeEnv)
    numTodo=as.integer(numCovariates)
    submodels=c()
    for ( i in 1:numTodo ){
        submodels=c(submodels,FALSE)
    }
#
# Generate all possible results for 0-N covariates
#
    nxtScenario=0
    appendFlag = FALSE
    lines=c()

    cat(modelFilename,file=controlFilename,sep="\n")
    for ( ifn in inputFileArray ) {
        cat(ifn,file=controlFilename,sep=" ",append=TRUE)
    }
    cat("",file=controlFilename,sep="\n",append=TRUE)
    cat("*.csv *.txt *.log *.LOG",file=controlFilename,sep="\n",append=TRUE)

    cat((numTodo + 1 ) ,file=controlFilename,sep="\n",append=TRUE)

    indx=1
    for ( i in 0:numTodo ) {
        if ( i > 0 ) 
            submodels[i]=TRUE
        ret = generateSelCovarSearchArgsLine (controlFilename, nlmeArgsFile , modelFilename, nlmeArgsFilename, inputFileArray, numCovariates, covarNamesArray,submodels,nxtScenario,appendFlag,indx)
        if ( length(scenarios[[ret$key]] ) != 0 ) {
            if ( scenarios[[ret$key]]$status == "Completed" ) 
            return(scenarios[[ret$key]])
        }
        line= ret$line
        outputFilename = ret$outputFilename
        outputFilenames=c(outputFilenames, outputFilename)
        indexes=c(indexes, ret$index)
        scenarios[[ret$key]]= ret
        if ( i > 0 ) 
            submodels[i]=FALSE
        appendFlag= TRUE
        lines=c(lines,line)
        nxtScenario = nxtScenario + 1 
        cat(line,file=controlFilename,sep="\n",append=TRUE)
        indx=indx+1
    }
    assign("nxtScenario", nxtScenario, envir=nlmeEnv)
    assign("scenarios", scenarios, envir=nlmeEnv)
    return(indexes)
}


getScenarioKey <-function(outputFilename)
{
    first=unlist(strsplit(outputFilename,"\\."))[1]
    key=unlist(strsplit(first, split="out"))[2]
    return(key)
}

#
# Reads summary information from a list of results files from different
# scenarios .
#
# Return the index of best results in the list of output files
#
#
getBestResults <-function(localDir, scenarioIndexes,criteria,degreesOfFreedomString,addPValue,stepwiseFilename,currentBest,mode,firstTime)
{
DEBUG_MASTER("getBestResults()")
    scenarios = get("scenarios",envir=nlmeEnv)
    bestListIndex=-1
    bestScenario = ""
    if ( currentBest == -1 ) {
        bestScenarioIndex = -1
        bestLL = 99999999999999999
        initialLL=99999999999999999999
        degreesOfFreedomString=paste0("1,",degreesOfFreedomString)
    }
    else {
        bestScenarioIndex = -1
        bestLL = scenarios[[currentBest]]$logLikelihood
        if ( criteria == "AIC" )
            bestLL = scenarios[[currentBest]]$AIC
        if ( criteria == "BIC" )
            bestLL = scenarios[[currentBest]]$BIC
        initialLL=bestLL
        if ( criteria == "-2LL" )  {
            degOfFreedom=unlist(strsplit(degreesOfFreedomString,","))[1]
            chisq = qchisq(as.numeric(1.0 - addPValue), df=as.integer(degOfFreedom))	
            if ( is.nan(chisq) )
                chisq = 0.0 
            if ( mode == "add" ) 
                bestLL = bestLL - chisq
            else
                bestLL = bestLL + chisq
        }
        else {
            if ( mode == "add" ) 
                bestLL = bestLL - addPValue
            else
                bestLL = bestLL + addPValue
        }
    }
    first=TRUE
    listIndex = 1
    chisq=addPValue
DEBUG_MASTER(paste("degreesOfFreedomString:",degreesOfFreedomString))
    for ( si in scenarioIndexes ) {
        f = scenarios[[si]]$outputFilename
        key=getScenarioKey(f)
        if ( criteria == "-2LL" ){
            degOfFreedom=unlist(strsplit(degreesOfFreedomString,","))[listIndex]
            chisq = qchisq(as.numeric(1.0 - addPValue), df=as.integer(degOfFreedom))	
            if ( is.nan(chisq) )
                chisq = 0.0
DEBUG_MASTER(paste("degOfFreedom:",degOfFreedom))
DEBUG_MASTER(paste("addPValue:",addPValue))
        }
        if ( mode == "subtract" ) 
            chisq = chisq * -1
DEBUG_MASTER("-------------------------")
DEBUG_MASTER(paste("listIndex:",listIndex))
DEBUG_MASTER(paste("si:",si))
DEBUG_MASTER(paste("scenarioName:",scenarios[[si]]$scenarioName))
DEBUG_MASTER(paste("chisq:",chisq))
        if ( file.exists(f) ) {
        lines=readLines(f)
        a=grep("LogLikelihood ",lines)
        if ( length(a) != 0 ){
            line=lines[a]
            logLik=  as.numeric(unlist(strsplit(line," = "))[2]) 
            ll= -2 * logLik

            first=grep("ReturnCode",lines,fixed=TRUE)
            stopifnot(length(first) != 0 ) 

            scenarios[[si]]$returnCode= as.integer(unlist(strsplit(lines[first]," = "))[2])
            scenarios[[si]]$shrinkage= as.numeric(unlist(strsplit(lines[first+2]," = "))[2])
            nParam= as.integer(unlist(strsplit(lines[first+3]," = "))[2])
            scenarios[[si]]$nParam= nParam
            nObs= as.integer(unlist(strsplit(lines[first+4]," = "))[2])
            scenarios[[si]]$nObs= nObs
            scenarios[[si]]$nSub= as.integer(unlist(strsplit(lines[first+5]," = "))[2])
            scenarios[[si]]$logLikelihood =  ll
            scenarios[[si]]$logLike =  logLik
            scenarios[[si]]$AIC =  ll + nParam * 2
            scenarios[[si]]$BIC =  ll + nParam * log(nObs)

            valueToCompare = ll 
            if ( criteria == "AIC" )
                valueToCompare = scenarios[[si]]$AIC
            if ( criteria == "BIC" )
                valueToCompare = scenarios[[si]]$BIC
            if ( (firstTime == TRUE) && ( si == 1 )  ) {
                    initialLL=valueToCompare
                    cat(sprintf("%s %s %s, %s = %f ",scenarios[[si]]$scenarioName,scenarios[[si]]$scenarioDescription,scenarios[[si]]$key,criteria,valueToCompare,chisq ),file=stepwiseFilename,sep="\n",append=FALSE)
                    cat(paste0("Find effect to add that reduces ",criteria," the most"),file=stepwiseFilename,sep="\n",append=TRUE)
            }
            else {
                if ( first ) {
                        cat(" ",file=stepwiseFilename,sep="\n",append=TRUE)
                        if ( mode == "add" ) 
                            cat(paste0("Find effect to add that reduces ",criteria," the most"),file=stepwiseFilename,sep="\n",append=TRUE)
                        else
                            cat(paste0("Find effect to subtract that increases ",criteria," the least"),file=stepwiseFilename,sep="\n",append=TRUE)
                 }
                 if ( ( valueToCompare + chisq ) < ( initialLL ) ) {
                     cat(sprintf("%s %s %s     %f ( %f %+f ) < %f )",scenarios[[si]]$scenarioName,scenarios[[si]]$scenarioDescription,scenarios[[si]]$key,( valueToCompare + chisq ), valueToCompare , chisq , initialLL ),file=stepwiseFilename,sep="\n",append=TRUE)
                      if ( ( valueToCompare + chisq ) <  bestLL  ) {
                          bestScenario=key
                          bestScenarioIndex=si
                          bestNetValue = valueToCompare 
                          bestLL = valueToCompare + chisq 
                          bestListIndex=listIndex
                      }
                  }
                  else {
                     cat(sprintf("%s %s %s   X %f ( %f %+f ) > %f )",scenarios[[si]]$scenarioName,scenarios[[si]]$scenarioDescription,scenarios[[si]]$key,( valueToCompare + chisq ), valueToCompare , chisq , initialLL ),file=stepwiseFilename,sep="\n",append=TRUE)
                  }
            }
            first=FALSE
            scenarios[[si]]$status = 'Completed'

        }
        }
        else {
            cat(sprintf("ERROR : Unable to read %s",f),file=stepwiseFilename,sep="\n",append=TRUE)
        }
        listIndex = listIndex + 1
    }

    if ( bestScenarioIndex != -1 ) 
        cat(sprintf("%s %s %s chosen, %s = %f",scenarios[[bestScenarioIndex]]$scenarioName,scenarios[[bestScenarioIndex]]$scenarioDescription,scenarios[[bestScenarioIndex]]$key,criteria,bestNetValue),file=stepwiseFilename,sep="\n",append=TRUE)
    else {
        cat(sprintf("\nNo effect chosen to %s\n",mode),file=stepwiseFilename,sep="\n",append=TRUE)
        if ( mode == "subtract" && currentBest != -1 ) 
            cat(sprintf("\nScenario to use = %s %s %s\n",scenarios[[currentBest]]$scenarioName,scenarios[[currentBest]]$scenarioDescription,scenarios[[currentBest]]$key),file=stepwiseFilename,sep="\n",append=TRUE)
    }
    assign("scenarios", scenarios, envir=nlmeEnv)
    return(bestListIndex)
}




#
# Make copies of the file we need later, tag then with scnario ID
# These are basically : progress.txt S_00N.status
# outnnnn.txt just gets copied from run directory into current directory.
#
moveStepwiseOutputFiles<-function(scenarioIndexes){
DEBUG_MASTER(paste0("moveStepwiseOutputFiles()"))
DEBUG_MASTER(paste0(scenarioIndexes))

    if (exists("jobsDirectoryRoot", envir = nlmeEnv)) 
        jobsDirectoryRoot=get("jobsDirectoryRoot",  envir=nlmeEnv)
    else
        jobsDirectoryRoot = ""
DEBUG_MASTER("jobsDirectoryRoot")
DEBUG_MASTER(jobsDirectoryRoot)
DEBUG_MASTER(".....................")
    if ( jobsDirectoryRoot != "" ) {
        jobsBaseDirectory=getBatchDirectoryLocation(jobsDirectoryRoot)
        UsingBatchTools=get("UsingBatchTools",envir=nlmeEnv)
        scenarios = get("scenarios",envir=nlmeEnv)
        indx=1
        for ( s in scenarioIndexes ) {
        for ( t in 1:10 ) {
          tryCatch (
          {
            if ( scenarios[[s]]$status == "Completed" ){
            t=99
            break
            }
            if ( jobsDirectoryRoot != "" ) {
                jobBaseIndx = indx %% 100
                progressFile=sprintf("%s/jobs/%02d/%d/progress.txt",jobsBaseDirectory,jobBaseIndx,indx )
                statusFile=sprintf("%s/S_%03d.status",jobsDirectoryRoot,indx )
                newProgressFile=sprintf("progress.txt.%0d",s)
                newStatusFile=sprintf("status.txt.%0d",s)
                outputFile=sprintf("%s/jobs/%02d/%d/%s",jobsBaseDirectory,jobBaseIndx,indx , scenarios[[s]]$outputFilename)
                file.copy(outputFile, scenarios[[s]]$outputFilename,overwrite=TRUE)
            }
            else {
                progressFile=sprintf("progress.txt.Job%0d",indx)
                statusFile=sprintf("S_%03d.status",indx)
                newProgressFile=sprintf("progress.txt.%0d",s)
                newStatusFile=sprintf("status.txt.%0d",s)
            }
            file.copy(progressFile, newProgressFile,overwrite=TRUE)
            file.copy(statusFile, newStatusFile,overwrite=TRUE)
            indx = indx + 1 
            break
          },
            error=function(ex){
print(ex)
            Sys.sleep(1)
              })
            }
        }
    }
}



#' NLME stepwise covariate search
#'
#' This function runs a shotgun covariate NLME job in parallel 
#' @param ParalleMethod    How to run, MPI, Multicore, Grid
#' @param Install_Directory Directory where NLME libraries, scripts are
#' @param Shared_Directory Directory accessible by all nodes
#' @param Local_Working_Directory Where input files are and results will be stored
#' @param ModelFile       Filename of the model
#' @param NlmeArgsFile    Arguments to NLME executable
#' @param InputFiles      List of input files
#' @param NumCovariates   Number of covariates
#' @param CovariateNames  Array of covariate names
#' @param Criteria        What criteria to use for convergance
#' @param AddPValue       Threashold value for adding variables
#' @param RemovePValue    Threashold value for removing variables
#' @param NumOfCores      Number of compute nodes to use
#' @param WorkflowName    Name of workflow to use in summary report
#'
#' @keywords NLME, StepwiseCovariateSearch
#' @export
#' @examples
#' performStepwiseCovarSearch(method, install_directory, shared_directory localWorkingDir modelFile nlmeArgsFile numCovariates CovariateNames Criteria addPValue removePValue NumProc )
#'
#'
performStepwiseCovarSearch<-function(args,reportProgress=FALSE){
DEBUG_MASTER("func --> performStepwiseCovarSearch()")

    localWorkingDir= gsub("\\","/",args[4],fixed=TRUE)
    assign("localWorkingDir", localWorkingDir, envir=nlmeEnv)
    updateInitialStatus("Stepwise Covariate Search",args[1],localWorkingDir)
    jobHomeDirectories=c()
    if ( length(args) != 14 ) {
        print("USAGE:performStepwiseCovarSearch(method, install_directory, shared_directory localWorkingDir modelFile nlmeArgsFile listOfFilesToCopy numCovariates CovariateNames NCriteria addPValue removePValue NumProc workflowName)")
    }
    else {
        tryCatch (
        {
            controlFilename = "nlmeControlFile.txt"
            nlmeArgsFile = "nlmeargsCombined.txt" 
            parallelMethod=args[1]
            installDir=args[2]
            sharedDir=args[3]
            localDir=args[4]
            modelFilename=args[5]
            nlmeArgsFilename=args[6]
            inputFileArray=args[7]
            numCovariates=args[8]
            covarNamesArray=args[9]
            criteriaString=args[10]
            tokens=unlist(strsplit(criteriaString,split=":"))
            criteria=tokens[1]
            if ( length(tokens) == 2 )
                degreesOfFreedomString=tokens[2]
            else 
                degreesOfFreedomString=paste(as.character(rep(1, times=numCovariates)),collapse=",")
            addPValue=as.numeric(args[11])
            removePValue=as.numeric(args[12])
            numCores=args[13]
            workflow_name=args[14]
            nxtScenario = 0 
            assign("workflow_name", workflow_name, envir=nlmeEnv)

#
# Step 1 .  Run zero-N searches
#
            scenarios=list()
            assign("scenarios", scenarios, envir=nlmeEnv)
            scenarioIndexes=generateInitialScenarios(controlFilename, nlmeArgsFile , modelFilename, nlmeArgsFilename, inputFileArray, numCovariates, covarNamesArray, nxtScenario)

            scenarios = get("scenarios",envir=nlmeEnv)
            nxtScenario = get("nxtScenario",envir=nlmeEnv)
            selectedVariables=c()
            argList=c()
            argList=c(argList,"STEPWISE_SEARCH")
            argList=c(argList,parallelMethod)
            argList=c(argList,installDir)
            argList=c(argList,sharedDir)
            argList=c(argList,localDir)
            argList=c(argList,controlFilename)
            argList=c(argList,numCores)
            argList=c(argList,workflow_name)
            cwd=getwd()

            outputFilenames=c()
            scenarioNames=c()
            progressFilenames=c()
            statusFilenames=c()

            # Run the jobs
            jobHome=performParallelNLMERun(argList,partialJob=TRUE,allowIntermediateResults=FALSE,progressStage="Initial Effect To Add",func="moveStepwiseOutputFiles",func_arg=scenarioIndexes,reportProgress=reportProgress)

            if ( !IsJobCanceled() && !IsEarlyTerminationRequested() ) {
                jobHomeDirectories=c(jobHomeDirectories,jobHome)
                setwd(cwd)

                bestResultsSofar = 0.0
                currentBestIndex = -1 
                #
                # Pick the best run
                #
                stepwiseFilename="Stepwise.txt" 
                currentBestIndex=getBestResults(localDir,scenarioIndexes,criteria,degreesOfFreedomString,addPValue,stepwiseFilename,currentBestIndex,"add",TRUE)
                scenarios = get("scenarios",envir=nlmeEnv)
                s=scenarioIndexes[currentBestIndex]
DEBUG_MASTER(paste(".......",s))
                currentBestScenario = scenarios[[s]]$index
                currentBestIndex = currentBestIndex -1 
    
                selectedVariables = c(selectedVariables, currentBestIndex )

                #
                # Nothing todo, the best results is the one with no covariates
                #
                if ( currentBestIndex == 1 ) {
                }
                numTodo=as.integer(numCovariates)
                submodels=c()
                for ( i in 1:numTodo ){
                    submodels=c(submodels,FALSE)
                }
                submodels[currentBestIndex] = TRUE
                notDone = 1 

                #
                # Keep adding another variable and see if the results et improved
                #
                originaldegreesOfFreedomString = degreesOfFreedomString
                while ( notDone ) {
                    #
                    # Add all the remaining variables one at a time in this iteration
                    #
                    appendFlag = FALSE
                    lines=c()
                    outputFilenames=c()

                    cat(modelFilename,file=controlFilename,sep="\n")
                    for ( ifn in inputFileArray ) {
                        cat(ifn,file=controlFilename,sep=" ",append=TRUE)
                    }
                    cat("",file=controlFilename,sep="\n",append=TRUE)
                    cat("*.csv *.txt *.log *.LOG",file=controlFilename,sep="\n",append=TRUE)
                    scenarioIndexes=c()
                    indx=1
                    scenarioAlreadyRan = FALSE
                    newDegreesOfFreedomString = ""
                    firstTime = TRUE
                    for ( i in 1:numTodo ){
                    if ( submodels[i] == FALSE )  {
                        submodels[i] = TRUE
                        degOfFreedom=unlist(strsplit(degreesOfFreedomString,","))[i]
                        if ( firstTime == TRUE )
                            newDegreesOfFreedomString = degOfFreedom
                        else
                            newDegreesOfFreedomString = paste(newDegreesOfFreedomString,degOfFreedom,sep=",")
                        firstTime = FALSE
                        subArgs=submodels
                        ret = generateSelCovarSearchArgsLine (controlFilename, nlmeArgsFile , modelFilename, nlmeArgsFilename, inputFileArray, numCovariates, covarNamesArray,subArgs,nxtScenario,appendFlag,indx)
DEBUG_MASTER(paste(".......",ret$key))
                        if ( length(scenarios[[ret$key]] ) != 0 ) {
                           if ( scenarios[[ret$key]]$status == "Completed" )  {
                               scenarioAlreadyRan = TRUE
                           }
                        }
                        if ( scenarioAlreadyRan == FALSE ) {
                            indx=indx+1
                            scenarios[[ret$key]] = ret
                            appendFlag = TRUE
                            line= ret$line
                            outputFilename = ret$outputFilename
                            outputFilenames=c(outputFilenames, outputFilename)
                            assign("scenarios", scenarios, envir=nlmeEnv)
                            lines=c(lines,line)
                            nxtScenario = nxtScenario + 1 
                        }
                        submodels[i] = FALSE 
                        scenarioIndexes=c(scenarioIndexes,ret$index)
                    }
                    }
                    cat((length(lines)) ,file=controlFilename,sep="\n",append=TRUE)
                    for ( l in lines ) {
                        cat(l,file=controlFilename,sep="\n",append=TRUE)
                    }
                    if ( length(lines) > 0 ) {
                    cwd=getwd()
                    jobHome=performParallelNLMERun(argList,partialJob=TRUE,progressStage="Add Effects",func="moveStepwiseOutputFiles",func_arg=scenarioIndexes,reportProgress=reportProgress)
                    jobHomeDirectories=c(jobHomeDirectories,jobHome)
                    setwd(cwd)
                    }
                    if ( IsJobCanceled() || IsEarlyTerminationRequested() ) 
                        break
                    bestIndex=getBestResults(localDir,scenarioIndexes,criteria,newDegreesOfFreedomString,addPValue,stepwiseFilename,currentBestScenario,"add",FALSE)
                    scenarios = get("scenarios",envir=nlmeEnv)

                    if ( bestIndex == -1 ) {
                        notDone = 0 
                        break
                    }
                    bestScenario = scenarios[[scenarioIndexes[bestIndex]]]$index
                    if ( criteria == "-2LL" ) {
                        newBest = scenarios[[bestScenario]]$logLikelihood
                        oldBest = scenarios[[currentBestScenario]]$logLikelihood
                        degOfFreedom=unlist(strsplit(newDegreesOfFreedomString,","))[bestScenario]
                        chisq = qchisq(as.numeric(1.0 - addPValue), df=as.integer(degOfFreedom))	
                        if ( is.nan(chisq) )
                            chisq = 0.0
                    }
                    if ( criteria == "AIC" ) {
                        newBest = scenarios[[bestScenario]]$AIC
                        oldBest = scenarios[[currentBestScenario]]$AIC
                        chisq = addPValue
                    }
                    if ( criteria == "BIC" ) {
                        newBest = scenarios[[bestScenario]]$BIC
                        oldBest = scenarios[[currentBestScenario]]$BIC
                        chisq = addPValue
                    }
                    if (newBest < ( oldBest - addPValue )) {
                        currentBestIndex = bestIndex
                        currentBestScenario = bestScenario
                        mask = scenarios[[bestScenario]]$key
                        # Keep track of which variable we have used so far
                        selectedVariables = c(selectedVariables, bestIndex )
                        # Reset submodel flags based on what was picked
                        for ( i in 1:nchar(mask) ){
                            if ( unlist(strsplit(mask,split=""))[i] == '0' ) 
                                    submodels[i]=FALSE
                            else
                                    submodels[i]=TRUE
                        }
                    }
                    else {
                        print("Did not improve on LL")
                        print(scenarios[[bestIndex]]$logLikelihood)
                        print(scenarios[[currentBestScenario]]$logLikelihood )
                        notDone = FALSE 
                    }
                }
#
# Keep subtracting one variable and see if the results are improved
#
                notDone = 1
                while ( notDone ) {
                #
                # Remove the selected variables one at a time in this iteration
                #
                    appendFlag = FALSE
                    lines=c()
                    outputFilenames=c()

                    cat(modelFilename,file=controlFilename,sep="\n")
                    for ( ifn in inputFileArray ) {
                        cat(ifn,file=controlFilename,sep=" ",append=TRUE)
                    }
                    cat("",file=controlFilename,sep="\n",append=TRUE)
                    cat("*.csv *.txt *.log *.LOG",file=controlFilename,sep="\n",append=TRUE)
                    scenarioIndexes=c()
                    indx=1
                    for ( i in 1:numTodo ){
                        scenarioAlreadyRan = FALSE
                        {
                            if ( submodels[i] == TRUE )  {
                                submodels[i] = FALSE
                                subArgs=submodels
                                ret = generateSelCovarSearchArgsLine (controlFilename, nlmeArgsFile , modelFilename, nlmeArgsFilename, inputFileArray, numCovariates, covarNamesArray,subArgs,nxtScenario,appendFlag,indx)
                                if ( length(scenarios[[ret$key]] ) != 0 ) {
                                    if ( scenarios[[ret$key]]$status == "Completed" ) 
                                        scenarioAlreadyRan = TRUE
                                }

                                scenarioIndexes=c(scenarioIndexes,ret$index)
                                if ( scenarioAlreadyRan == FALSE ) {
                                    scenarios[[ret$key]] = ret
                                    appendFlag = TRUE
                                    line= ret$line
                                    outputFilename = ret$outputFilename
                                    outputFilenames=c(outputFilenames, outputFilename)
                                    assign("scenarios", scenarios, envir=nlmeEnv)
                                    indx=indx+1
                                    lines=c(lines,line)
                                    nxtScenario = nxtScenario + 1 
                                }
                                submodels[i] = TRUE 
                            }
                        }
                    }
                    cat((length(lines)) ,file=controlFilename,sep="\n",append=TRUE)
                    for ( l in lines ) {
                        cat(l,file=controlFilename,sep="\n",append=TRUE)
                    }
                    if ( length(lines) > 0 ) {
                        cwd=getwd()
                        jobHome=performParallelNLMERun(argList,partialJob=TRUE,progressStage="Remove Effects",func="moveStepwiseOutputFiles",func_arg=scenarioIndexes,reportProgress=reportProgress)
                        jobHomeDirectories=c(jobHomeDirectories,jobHome)
                        setwd(cwd)
                    }
                    if ( IsJobCanceled() || IsEarlyTerminationRequested() ) 
                        break
                    bestIndex=getBestResults(localDir,scenarioIndexes,criteria,degreesOfFreedomString,removePValue,stepwiseFilename,currentBestScenario,"subtract",FALSE)
                    scenarios = get("scenarios",envir=nlmeEnv)

                    if ( bestIndex == -1 ) {
                            notDone = 0 
                            break
                    }
                    bestScenario = scenarios[[scenarioIndexes[bestIndex]]]$index
                    newBest = scenarios[[bestScenario]]$logLikelihood
                    oldBest = scenarios[[currentBestScenario]]$logLikelihood
                    if ( criteria == "-2LL" ) {
                        newBest = scenarios[[bestScenario]]$logLikelihood
                        oldBest = scenarios[[currentBestScenario]]$logLikelihood
                        degOfFreedom=unlist(strsplit(newDegreesOfFreedomString,","))[bestScenario]
                        chisq = qchisq(as.numeric(1.0 - removePValue), df=as.integer(degOfFreedom))	
                        if ( is.nan(chisq) )
                            chisq = 0.0
                    }
                    if ( criteria == "AIC" ) {
                        newBest = scenarios[[bestScenario]]$AIC
                        oldBest = scenarios[[currentBestScenario]]$AIC
                        chisq = removePValue
                    }
                    if ( criteria == "BIC" ) {
                        newBest = scenarios[[bestScenario]]$BIC
                        oldBest = scenarios[[currentBestScenario]]$BIC
                        chisq = removePValue
                    }
                    chisq = chisq * -1 
                    if (newBest < ( oldBest - chisq )) {
                            currentBestIndex = bestIndex
                            currentBestScenario = bestScenario
                            mask = scenarios[[bestScenario]]$key
                            # Keep track of which variable we have used so far
                            selectedVariables = c(selectedVariables, bestIndex )
                            # Reset submodel flags based on what was picked
                            for ( i in 1:nchar(mask) ){
                                if ( unlist(strsplit(mask,split=""))[i] == '0' ) 
                                    submodels[i]=FALSE
                                else
                                    submodels[i]=TRUE
                            }
                    }
                    else  {
                            print("Did not improve on LL")
                            print(scenarios[[bestIndex]]$logLikelihood)
                            print(scenarios[[currentBestScenario]]$logLikelihood )
                            notDone = FALSE 
                    }
                }
#
# Report the best combination
#
                if ( tolower(parallelMethod) == "mpi " ) {
                    DEBUG_MASTER("mpi.exit()")
                    mpi.exit()
                    IsMpiInitialized=FALSE
                    assign("IsMpiInitialized", IsMpiInitialized , envir=nlmeEnv)
                }
            }
            },
            error=function(ex){
                print(ex)
                FailProgress()
            })

            if ( !IsJobCanceled() ) {
                summarizeStepwiseCovarSearch(localWorkingDir,scenarios)
                CompleteProgress()
            }
DEBUG_MASTER("End of  performStepwiseCovarSearch()")
DEBUG_MASTER(getwd())
DEBUG_MASTER(parallelMethod)
        if ( tolower(parallelMethod) == "mpi " ) {
            DEBUG_MASTER("mpi.exit()")
            mpi.quit()
            IsMpiInitialized=FALSE
            assign("IsMpiInitialized", IsMpiInitialized , envir=nlmeEnv)
        }
        for ( jh in jobHomeDirectories ) {
             removeTempWorkingDirectory(jh,getwd())
        }
        assign("jobHomeDirectories", jobHomeDirectories , envir=.GlobalEnv)
    }
}


getTableNames <- function(columnDefinitionFilename){

    names=""
    lines=readLines(columnDefinitionFilename)
    indxs=grep("table(file=",lines,fixed=TRUE)
    for ( i in indxs ) { # Parse te filename out
        line=lines[i]
        pos=gregexpr('\"',line)
        p=unlist(pos)
        if ( length(p) >= 2 ) {
            filename=substr(line,p[1]+1,p[2]-1)
            names=sprintf("%s %s",names,filename)
        }
    }
    return(names)
}




#' NLME estimation on sorted columns
#'
#' This function runs multiple estimations sorting the input dataset by 
#' requested columns and creating multiple data sets
#'
#' @param ParalleMethod    How to run, MPI, Multicore, Grid
#' @param Install_Directory Directory where NLME libraries, scripts are
#' @param Shared_Directory Directory accessible by all nodes
#' @param Local_Working_Directory Where input files are and results will be stored
#' @param NlmeArgsFile    Arguments to NLME executable
#' @param NumColmns       Number of sort columns
#' @param ColumnNames     Array of column names
#' @param NumOfCores      Number of compute nodes to use
#' @param WorkflowName    Name of workflow to use in summary report
#'
#' @keywords NLME, 
#' @export
#' @examples
#' performEstimationOnSortColumns(method, install_directory, shared_directory localWorkingDir modelFile nlmeArgsFile numColumns ColumnNames NumProc, workflowName )
#'
#'
performEstimationOnSortColumns<-function(args,reportProgress=FALSE){
DEBUG_MASTER("func --> performEstimationOnSortColumns()")
DEBUG_MASTER(paste(args))

    cleanupFromPreviousRun()
    localWorkingDir= gsub("\\","/",args[4],fixed=TRUE)
    assign("localWorkingDir", localWorkingDir, envir=nlmeEnv)
    updateInitialStatus("SortByColumn",args[1],localWorkingDir)
    if ( length(args) != 9 ) {
        print("USAGE:method, install_directory, shared_directory localWorkingDir nlmeArgsFile numColumns ColumnNames NumProc workflowName")
    }
    else {
    tryCatch (
    {
        controlFilename = "NewnlmeControlFile.txt"
        parallelMethod=args[1]
        installDir=args[2]
        sharedDir=args[3]
        localDir=args[4]
        nlmeArgsFilename=args[5]
        numColumns=args[6]
        columnNamesArray=args[7]
        numCores=args[8]
        workflow_name=args[9]
        nxtScenario = 0 
        assign("workflow_name", workflow_name, envir=nlmeEnv)

#
# Create multiple datasets based on sort column(s)
#

        numDatasets=sortByColumnAndGenerateControlFile("data1.txt",numColumns, columnNamesArray,nlmeArgsFilename,controlFilename) 

        argList=c()
        argList=c(argList,"ESTIMATION_RUN")
        argList=c(argList,parallelMethod)
        argList=c(argList,installDir)
        argList=c(argList,sharedDir)
        argList=c(argList,localDir)
        argList=c(argList,controlFilename)
        argList=c(argList,numCores)
        argList=c(argList,workflow_name)

        performParallelNLMERun(argList,partialJob=FALSE,allowIntermediateResults=TRUE,progressStage="Estimation",reportProgress=reportProgress)


    },
    error=function(ex){
        print(ex)
        FailProgress()
    })
    }
#    if ( !IsJobCanceled() )
#        CompleteProgress()
} 

cleanupFromPreviousRun <-function(){
    profileDescriptors=""
    assign("profileDescriptors",profileDescriptors,envir=nlmeEnv)
}

#' NLME a profile estimation run on list of fixed effects
#'
#' This function runs multiple estimations sorting the input dataset by 
#' requested columns and creating multiple data sets
#' Runs are also generated for all profiling variables
#'
#' @param ParalleMethod    How to run, MPI, Multicore, Grid
#' @param Install_Directory Directory where NLME libraries, scripts are
#' @param Shared_Directory Directory accessible by all nodes
#' @param Local_Working_Directory Where input files are and results will be stored
#' @param NlmeArgsFile    Arguments to NLME executable
#' @param NumColmns       Number of sort columns
#' @param ColumnNames     Array of column names
#' @param fixedEffectList Array of fixed effects to pertubate
#' @param NumOfCores      Number of compute nodes to use
#' @param WorkflowName    Name of workflow to use in summary report
#'
#' @keywords NLME, 
#' @export
#' @examples
#' performProfileEstimation(method, install_directory, shared_directory localWorkingDir modelFile nlmeArgsFile numColumns ColumnNames fixedEffectList NumProc, workflowName )
#'
#'
performProfileEstimation<-function(args,reportProgress=FALSE){
DEBUG_MASTER("func --> performProfileEstimation()")
DEBUG_MASTER(paste(args))

    localWorkingDir= gsub("\\","/",args[4],fixed=TRUE)
    assign("localWorkingDir", localWorkingDir, envir=nlmeEnv)
    updateInitialStatus("Profile",args[1],localWorkingDir)
    if ( length(args) != 11 ) {
        print("USAGE:method, install_directory, shared_directory localWorkingDir nlmeArgsFile numColumns ColumnNames profileDescriptions NumProc workflowName")
    }
    else {
    tryCatch (
    {
        controlFilename = "NewnlmeControlFile.txt"
        parallelMethod=args[1]
        installDir=args[2]
        sharedDir=args[3]
        localDir=args[4]
        nlmeArgsFilename=args[5]
        numColumns=args[6]
        columnNamesArray=args[7]
        profileArray=args[8]
        profilePercentFlag=args[9]
        numCores=args[10]
        workflow_name=args[11]
        nxtScenario = 0 
        assign("workflow_name", workflow_name, envir=nlmeEnv)

        profileDescriptors=profileArray
        assign("profileDescriptors",profileDescriptors,envir=nlmeEnv)
        assign("profilePercentFlag",profilePercentFlag,envir=nlmeEnv)
#
# Create multiple datasets based on sort column(s)
#

        numDatasets=sortByColumnAndGenerateControlFile("data1.txt",numColumns, columnNamesArray,nlmeArgsFilename,controlFilename) 

        argList=c()
        argList=c(argList,"PROFILE_RUN")
        argList=c(argList,parallelMethod)
        argList=c(argList,installDir)
        argList=c(argList,sharedDir)
        argList=c(argList,localDir)
        argList=c(argList,controlFilename)
        argList=c(argList,numCores)
        argList=c(argList,workflow_name)

        performParallelNLMERun(argList,partialJob=FALSE,allowIntermediateResults=TRUE,progressStage="Estimation",reportProgress=reportProgress)


    },
    error=function(ex){
print("----------------")
        print(ex)
print("----------------")
        FailProgress()
    })
    }
    cleanupFromPreviousRun()
#    if ( !IsJobCanceled() )
#    CompleteProgress()
} 




reconnectToGenericNLMERun<-function(args){

    loadBatchLibrary()

    if ( length(args) != 9 ) {
        print("USAGE:performParallelNLMERun.r jobType parallelMethod install_dir shared_directory localWorkingDir controlFile NumProc workflow_name gridRegistryDir")
    }
    else {
        reg.finalizer(nlmeEnv, nlmeEnvIsDone, onexit = TRUE ) 
        submissionPlatform=.Platform$OS.type
        assign("submissionPlatform", submissionPlatform, envir=nlmeEnv)
        if ( submissionPlatform == "windows" ) {
            exeFileExtension=".bat"
        } else {
            exeFileExtension=".sh"
        }
        assign("exeFileExtension", exeFileExtension, envir=nlmeEnv)
        jobType=args[1]
        parallelMethod=args[2]
        assign("parallelMethod", parallelMethod, envir=nlmeEnv)
        gridRegistryDir=args[9]
        jobHome=dirname(gridRegistryDir)
        jobHome=gsub("\\","/",jobHome,fixed=TRUE)
        
        assign("jobHome", jobHome, envir=nlmeEnv)
       
        SharedWorkingDir=gridRegistryDir

        assign("SharedWorkingDir",SharedWorkingDir,envir=nlmeEnv)

        localWorkingDir= gsub("\\","/",args[5],fixed=TRUE)
        assign("localWorkingDir", localWorkingDir, envir=nlmeEnv)

        control_file=args[6]
        assign("control_file", control_file , envir=nlmeEnv)
        lines = readLines(control_file)
        control_lines=lines[5:length(lines)]
        assign("control_lines", control_lines , envir=nlmeEnv)
        model_file=lines[1]
        assign("model_file", model_file , envir=nlmeEnv)
        files_to_copy=lines[2]
        assign("files_to_copy", files_to_copy , envir=nlmeEnv)
        files_to_return=lines[3]
        assign("files_to_return", files_to_return , envir=nlmeEnv)
        num_samples = as.integer(lines[4])
        assign("num_samples", num_samples , envir=nlmeEnv)

        extra_args_file=getExtraArgumentFilename(control_lines[1])
        assign("extra_args_file", gsub("\\","/",extra_args_file,fixed=TRUE), envir=nlmeEnv)

        num_processes =  as.integer(args[7])
        assign("num_processes", num_processes , envir=nlmeEnv)
        assign("workflow_name",args[8],envir=nlmeEnv)
        progress=list(MachineName="LocalHost", ParallelProtocol="None",ModelName="",StartTime="", EndTime="", Status="InProgress", NumOfSamples=0, NumOfSamplesCompleted=0,NumOfSamplesFailed= 0 , NumOfSamplesExpired = 0 , NumOfSamplesErrored=0)
        progress$NumOfSamples = num_samples
        progress$Status = "InProgress"
        progress$ParallelProtocol = "GRID"
        progress$ParallelProtocol = parallelMethod
        progress$StartTime= getLocalTimeUTC()

        assign("ProgressStatus",progress,envir=nlmeEnv)
        ReportProgress()

        RunningOnGrid = TRUE 
        gridRegistry=loadRegistry(gridRegistryDir)
        assign("gridRegistry", gridRegistry , envir=nlmeEnv)
print("-------------------------reconnectGenericGridJob()--------")
        stat = reconnectGenericGridJob(jobType)
print("----------------- After reconnectGenericGridJob()--------")
        done = findDone(gridRegistry)
        expired = findExpired(gridRegistry)
        done=c(done,expired)
        


        if ( stat == TRUE && !IsJobCanceled() ) {
if ( length(done) == 0 ) 
    done = 1:num_samples
            generateJobResults(localWorkingDir, jobType, done, control_lines)
            listJobErrors(localWorkingDir, jobType, num_samples, control_lines)
        }

        if ( partialJob == FALSE && !IsJobCanceled() ) 
            CompleteProgress()
     
        if (  IsJobCanceled() || IsEarlyTerminationRequested() ) {
            RemoveUserCommands()
            if ( RunningOnGrid == TRUE )
                killAGridJob(gridRegistry)
        }
        progress = get("ProgressStatus",envir=nlmeEnv)
        flag=Sys.getenv("NLME_KEEP_GRID_RESULTS")

        if ( RunningOnGrid ) {
            if ( progress$NumOfSamplesFailed == 0 && progress$NumOfSamplesExpired == 0 && ( flag != "TRUE" ) ) {
                tryCatch (
                {
                     if ( flag != TRUE ) {
                         print("removeRegistry()")
                         removeRegistry(reg=gridRegistry)
                     }
                },
                error=function(ex){
    	            Sys.sleep(1)
                })
            }
        }
        if ( tolower(parallelMethod) == "mpi" )  {
DEBUG_MASTER("mpi.exit()")
            mpi.exit()
            IsMpiInitialized=FALSE
            assign("IsMpiInitialized", IsMpiInitialized , envir=nlmeEnv)
        }
        if ( progress$NumOfSamplesFailed == 0 && progress$NumOfSamplesExpired == 0 && ( flag != "TRUE" ) ) {
             removeTempWorkingDirectory(jobHome,localWorkingDir)
        }
    }
}


#' NLME Bootstrap Function
#'
#' This function reconnects to a grid job
#' @param ParalleMethod    How to run, MPI, Multicore, Grid
#' @param Install_Directory Directory where NLME libraries, scripts are
#' @param Shared_Directory Directory accessible by all nodes
#' @param Local_Working_Directory Where input files are and results will be stored
#' @param Optimization_Engine Which Pharsight NLME engine to use
#' @param Num_Iterations      Maximum iterations
#' @param Num_Samples         Number of bootstrap samples to run
#' @param Max_Tries           How many times to try a sample that does not converge
#' @param Model_Filename      PML model
#' @param Column_Definition_Filename Mapping of columns
#' @param Data_File           NLME data file
#' @param Start_Seed          Initial seed
#' @param Extra_Arguments_File Other parameters to pass to engines
#' @param List_of_Files_to_copy_to_shared_directory List of files to copy to the remote systems
#' @param Number_of_Cores_to_use Number of cores to use for multicore runs
#' @param Confidence_Level       Confidence level to summarize for
#' @keywords NLME, Bootstrap
#' @export
#' @examples
#' reconnectToBootstrapNLMERun(method,install_directory, shared_directory localWorkingDir engine num_iterations num_samples max_tries model_file column_def_file data_file start_seed extra_args_file files_to_copy NumProc ConfidenceLevel gridDirerctory)
#'

reconnectToBootstrapNLMERun<-function(args){

    loadBatchLibrary()

    if ( length(args) != 17 ) {
    print("USAGE:bootstrap.r parallelMethod install_dir shared_directory localWorkingDir engine num_iterations num_samples max_tries model_file column_def_file data_file start_seed extra_args_file files_to_copy NumProc ConfidenceLevel tempdir")
    }
    else {
        jobType = "BOOTSTRAP" 
        submissionPlatform=.Platform$OS.type
        assign("submissionPlatform", submissionPlatform, envir=nlmeEnv)
        if ( submissionPlatform == "windows" ) {
            exeFileExtension=".bat"
        } else {
            exeFileExtension=".sh"
        }
        assign("exeFileExtension", exeFileExtension, envir=nlmeEnv)
        parallelMethod=args[1]
        assign("parallelMethod", parallelMethod, envir=nlmeEnv)

        gridRegistryDir=args[17]
        jobHome=dirname(gridRegistryDir)
        jobHome=gsub("\\","/",jobHome,fixed=TRUE)
        assign("jobHome", jobHome, envir=nlmeEnv)
        SharedWorkingDir=gridRegistryDir

        assign("SharedWorkingDir",SharedWorkingDir,envir=nlmeEnv)

        localWorkingDir= gsub("\\","/",args[4],fixed=TRUE)
        assign("localWorkingDir", localWorkingDir, envir=nlmeEnv)
        assign("engine", as.integer(args[5]), envir=nlmeEnv)
        assign("num_iterations", as.integer(args[6]), envir=nlmeEnv)
        assign("num_samples", as.integer(args[7]), envir=nlmeEnv)
        assign("max_tries", as.integer(args[8]), envir=nlmeEnv)
        assign("model_file", args[9], envir=nlmeEnv)
        assign("column_def_file", args[10], envir=nlmeEnv)
        assign("data_file", args[11], envir=nlmeEnv)
        assign("start_seed", as.integer(args[12]), envir=nlmeEnv)
        assign("extra_args_file", gsub("\\","/",args[13],fixed=TRUE), envir=nlmeEnv)
        assign("files_to_copy", gsub("\\","/",args[14],fixed=TRUE), envir=nlmeEnv)
        num_processes =  as.integer(args[15])
        assign("num_processes", num_processes , envir=nlmeEnv)
        assign("confidence_level", as.double(args[16]), envir=nlmeEnv)
        progress=list(MachineName="LocalHost", ParallelProtocol="None",ModelName="",StartTime="", EndTime="", Status="InProgress", NumOfSamples=0, NumOfSamplesCompleted=0,NumOfSamplesFailed= 0 , NumOfSamplesExpired = 0 ,NumOfSamplesErrored=0)
        progress$NumOfSamples = as.integer(args[7])
        progress$Status = "InProgress"
        progress$ParallelProtocol = parallelMethod
        progress$StartTime= getLocalTimeUTC()

        assign("ProgressStatus",progress,envir=nlmeEnv)

        ReportProgress()

        RunningOnGrid = TRUE 
        gridRegistry=loadRegistry(gridRegistryDir)

        assign("gridRegistry", gridRegistry , envir=nlmeEnv)
print("-------------------------reconnectGenericGridJob()--------")
        stat = reconnectGenericGridJob(jobType)
print("----------------- After reconnectGenericGridJob()--------")
        done = findDone(gridRegistry)
        expired = findExpired(gridRegistry)
        done=c(done,expired)
        


        if ( stat == TRUE && !IsJobCanceled() ) {
if ( length(done) == 0 ) 
    done=1:num_samples
            generateJobResults(localWorkingDir, jobType, done, control_lines)
            listJobErrors(localWorkingDir, jobType, num_samples, control_lines)
        }
        if ( !IsJobCanceled() )
        CompleteProgress()
     
        if (  IsJobCanceled() ) {
            killJobs(gridRegistry,findNotDone(gridRegistry))
        }
        progress = get("ProgressStatus",envir=nlmeEnv)
        flag=Sys.getenv("NLME_KEEP_GRID_RESULTS")

        if ( RunningOnGrid ) {
            if ( flag != "TRUE" ) {
                if ( progress$NumOfSamplesFailed == 0 && progress$NumOfSamplesExpired == 0 ) {
                    tryCatch (
                    {
                         print("removeRegistry()")
                         removeRegistry(reg=gridRegistry)
                    },
                    error=function(ex){
        	            Sys.sleep(1)
                    })
                }
            }
        }
        if ( tolower(parallelMethod) == "mpi" )  {
DEBUG_MASTER("mpi.exit()")
            mpi.exit()
            IsMpiInitialized=FALSE
            assign("IsMpiInitialized", IsMpiInitialized , envir=nlmeEnv)
        }
        if ( progress$NumOfSamplesFailed == 0 && progress$NumOfSamplesExpired == 0 && ( flag != "TRUE" ) )
            removeTempWorkingDirectory(jobHome,localWorkingDir)

    }
}

nlmeEnvIsDone <-function(e){
#       CompleteProgress()
    if (exists("jobHomeDirectories", envir = .GlobalEnv)) 
        jobHomeDirectories=get("jobHomeDirectories",  envir=.GlobalEnv)
    else
        jobHomeDirectories = NULL
    for ( jh in jobHomeDirectories ) {
         removeTempWorkingDirectory(jh,getwd())
    }
}


.Last <-function(){

}

.First <-function(){
}

.onAttach <-function(libname,pkgname){
# Check and give a warning if required packages are not there
# XML and reshape are required for all run modes.
# Rmpi is required for MPI modes
# batchtools is required for grids
#
    for ( lib in c("XML","reshape")  ) {
        stat = require(lib,character.only=TRUE,quietly=TRUE)
        if ( stat == FALSE ) {
            print(paste0("WARNNING : Library : ",lib," is required for using Certara.NLME8 package"))
        }
    }
    root=Sys.getenv("NLME_ROOT_DIRECTORY")
    if ( root != "" ) {
        root=gsub("\"","",root,fixed=TRUE)
        if ( Sys.info()["sysname"] == "Windows" )
            root=shortPathName(root)
        startup=paste0(root,"/InstallDirNLME/setup_env.r")
        if ( file.exists(startup) )  {
            print(paste("INFO: Sourcing ",startup))
            sys.source(startup,envir=.GlobalEnv)
        }
    }
}

.onLoad <-function(libname,pkgname){
}

getLocalTimeUTC <-function(){
        return(c(format(as.POSIXlt(Sys.time(), "UTC"),"%b %Y %d %X")))        
}

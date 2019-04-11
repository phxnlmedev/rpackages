
#'
#' @export NlmeParallelMethod
#'
NlmeParallelMethod=setClass("NlmeParallelMethod",representation(
                                      method="character"))

assign("NlmeParallelMethod",NlmeParallelMethod,env=.GlobalEnv)

setMethod("initialize","NlmeParallelMethod",
    function(.Object,
              method="None"){
        if ( !(toupper(method) == "NONE" || 
             toupper(method) == "MULTICORE" ||
             toupper(method) == "MPI" ||
             toupper(method) == "TORQUE" ||
             toupper(method) == "TORQUE_MPI" ||
             toupper(method) == "SGE" ||
             toupper(method) == "SGE_MPI" ||
             toupper(method) == "LSF" ||
             toupper(method) == "LSF_MPI" ||
             toupper(method) == "LOCAL_MPI" )) {
             warning(paste(method,"Is not supported! using NONE"))
             method = "NONE"
        }
        .Object@method=method
        .Object
    })

#'
#' NlmeUserAuthentication
#'
#' Authentication records
#'
#' @param userName             How the user is identified to the remote system
#' @param privateKeyFile       Ssh generated private key
#' @param userPassword         Password to login into remote system
#'
#'
#' @export NlmeUserAuthentication
#'
NlmeUserAuthentication=setClass("NlmeUserAuthentication",representation(
                                     userName="character",
                                     privateKeyFile="ANY",
                                     userPassword="ANY"))

assign("NlmeUserAuthentication",NlmeUserAuthentication,env=.GlobalEnv)

setMethod("initialize","NlmeUserAuthentication",
    function(.Object,
              userName="",
              privateKeyFile=NULL,
              userPassword=NULL){
        .Object@userName=userName
        .Object@privateKeyFile=privateKeyFile
        .Object@userPassword=userPassword
        .Object
    })

#'
#' NlmeRemoteExecutor
#'
#' Remote Executor 
#'
#' @param sharedDirectory        This is the directory where the run happens in
#' @param installationDirectory  Directory containing NLME libraries/scripts
#' @param hostName               IP or name of remote host
#' @param userAuthentication     credential for user to log into remote system
#'
#'
#' @export NlmeRemoteExecutor
#'
NlmeRemoteExecutor=setClass("NlmeRemoteExecutor",representation(
                                  sharedDirectory="character",
                                  installationDirectory="character",
                                  machineName="character",
                                  userAuthentication="ANY",
                                  session="ANY"
                                  ))

assign("NlmeRemoteExecutor",NlmeRemoteExecutor,env=.GlobalEnv)

setMethod("initialize","NlmeRemoteExecutor",
    function(.Object,
              sharedDirectory="",
              installationDirectory="",
              machineName="",
              userAuthentication=NULL){
        .Object@sharedDirectory=sharedDirectory
        .Object@installationDirectory=installationDirectory
        .Object@machineName=machineName
        .Object@userAuthentication=userAuthentication
        .Object@session=NULL
        .Object
    })

setGeneric(name="openSession",
           def=function(.Object)
           {
               standardGeneric("openSession")
           })


setMethod("openSession",
    signature="NlmeRemoteExecutor",
    definition = function(.Object){
    print(.Object@sharedDirectory)
    print(.Object@installationDirectory)
    print(.Object@machineName)
    print(.Object@userAuthentication@userName)
    print(.Object@userAuthentication@privateKeyFile)
    print(.Object@userAuthentication@userPassword)

    session = ssh_connect(host=paste0(.Object@userAuthentication@userName,
                                      "@",
                                      .Object@machineName),
                          keyfile=.Object@userAuthentication@privateKeyFile,
                          passwd=.Object@userAuthentication@userPassword,
                          verbose = 0 )
    .Object@session=session
    .Object
    })

#' 
#' @export execCommand
#'
setGeneric(name="execCommand",
           def=function(.Object,command)
           {
             standardGeneric("execCommand")
           })


setMethod("execCommand",
          signature="NlmeRemoteExecutor",
          definition = function(.Object,command){
print(paste0("Executing : ", command))     
          ret = ssh_exec_internal(.Object@session, command)
#print(paste0("Status    : ",ret$status))
#print(paste0("Results   : ",cat(rawToChar(ret$stdout))))
#print("-----------------------")
#Return 'ret' for NlmeJobStatus to get stdout of progress.xml
return(ret)
          })

#' 
#' @export mktempdir
#'
setGeneric(name="mktempdir",
           def=function(.Object)
           {
             standardGeneric("mktempdir")
           })


setMethod("mktempdir",
          signature="NlmeRemoteExecutor",
          definition = function(.Object){
            
            out = ssh_exec_internal(.Object@session,
                                paste0("mktemp -d ",paste0(.Object@sharedDirectory,"/DME_XXXXX")))
            
            dir=gsub("\n","",rawToChar(out$stdout))
            
            return(dir)
          })
#' 
#' @export uploadFiles
#'
setGeneric(name="uploadFiles",
           def=function(.Object,remoteDir,files)
           {
             standardGeneric("uploadFiles")
           })



setMethod("uploadFiles",
          signature="NlmeRemoteExecutor",
          definition = function(.Object,remoteDir,files){
            print(paste("Uploading",files))
            print(paste("remoteDir",remoteDir))
            print("------------------")
            ret = scp_upload(.Object@session, files, to=remoteDir)
            
          })
#' 
#' @export downloadFiles
#'
setGeneric(name="downloadFiles",
           def=function(.Object,localDir,files)
           {
             standardGeneric("downloadFiles")
           })



setMethod("downloadFiles",
          signature="NlmeRemoteExecutor",
          definition = function(.Object,localDir,files){
            
  print(paste0("downloadFiles:"))
  print(files)
  print(localDir)
  print("----------------")

  if (file.exists("lock")) {
      return("")
  }
  file.create("lock")
  
  #The new separate 'session' has been created just to perform the 'scp_download' call to ensure data are synchronized
  passwd = .Object@userAuthentication@userPassword
  privateKeyFile = .Object@userAuthentication@privateKeyFile
  host = .Object@machineName
  
  session = ssh_connect(host, privateKeyFile, passwd, verbose = 0)
#  print("+++++++++++ Start scp_download +++++++++++++")
  ret = scp_download(session, files, to=localDir)
  #ret = scp_download(.Object@session, files, to=localDir)
#  print(paste0("Return status is",ret))
#  print("+++++++++++ End scp_download +++++++++++++")  
  #Disconnect 'session'
  ssh_disconnect(session)
  
  file.remove("lock")  
            
          })

#'
#' NlmeParallelHost
#'
#' NLME Parallel Host object class
#'
#'
#'
#' Class represents an NLME parallel host
#' It can either be local or remote
#'
#' @param sharedDirectory        This is the directory where the run happens in
#' @param installationDirectory  Directory containing NLME libraries/scripts
#' @param hostName               visual name of the host(default local)
#' @param machineName            IP address or name of the host(default local)
#' @param hostType               Windows or Linux
#' @param numCores               Number of compute cores
#' @param parallelMethod         None|Multicore|SGE|SGE_MPI|TORQUE|TORQUE_MPI
#'                               MPI|LSF|LSF_MPI|LOCAL_MPI
#' @param userAuthentication	 user credential for remote system
#' @param scriptPath             Optional initialization remote script path
#' @param isLocal                Is this a local or remote host
#'
#' @keywords NLME, NlmeParallelHost 
#'
#' @export NlmeParallelHost
#'
#' @examples
#' NlmeParallelHost(...)
#'
NlmeParallelHost=setClass("NlmeParallelHost",representation(
                                   sharedDirectory="character",
                                   installationDirectory="character",
                                   hostName="character",
                                   machineName="character",
                                   hostType="character",
                                   numCores="numeric",
                                   isLocal="logical",
                                   rLocation="character",
                                   scriptPath="character",
                                   userAuthentication="NlmeUserAuthentication",
                                   remoteExecutor="ANY",
                                   parallelMethod="NlmeParallelMethod"))

assign("NlmeParallelHost",NlmeParallelHost,env=.GlobalEnv)

setMethod("initialize","NlmeParallelHost",
    function(.Object,
              sharedDirectory="",
              installationDirectory="",
              hostName="",
              machineName="",
              hostType="",
              numCores="",
              isLocal=TRUE,
              rLocation="",
              scriptPath="",
              remoteExecutor=NULL,
              userAuthentication=NlmeUserAuthentication(),
              parallelMethod=NlmeParallelMethod()){
        defaultSharedDirectory=Sys.getenv("NLME_ROOT_DIRECTORY")
        if ( sharedDirectory == "" )
            sharedDirectory = defaultSharedDirectory
        if ( installationDirectory == "" )
            installationDirectory = paste(sharedDirectory,"InstallDirNLME",
            sep="/")
        installationDirectory=gsub("\\","/",shortPathName(installationDirectory),fixed=TRUE)
        sharedDirectory=gsub("\\","/",shortPathName(sharedDirectory),fixed=TRUE)
        if ( machineName == "" )
            machineName=Sys.info()[["nodename"]]
        if ( hostType == "" )
            hostType=Sys.info()[["sysname"]]
        if ( numCores == "" )
            numCores = 1 
        if ( attr(parallelMethod,"method") == "" )
            parallelMethod = "None" 
        if ( isLocal == FALSE ) {
            remoteExecutor = NlmeRemoteExecutor(sharedDirectory,
                                                installationDirectory,
                                                machineName,
                                                userAuthentication)
            remoteExecutor=openSession(remoteExecutor)
        }
        .Object@sharedDirectory=sharedDirectory
        .Object@installationDirectory=installationDirectory
        .Object@hostName=hostName
        .Object@machineName=machineName
        .Object@hostType=hostType
        .Object@numCores=as.integer(numCores)
        .Object@parallelMethod=parallelMethod
        .Object@rLocation=rLocation
        .Object@scriptPath=scriptPath
        .Object@userAuthentication=userAuthentication
        .Object@remoteExecutor=remoteExecutor
        .Object@isLocal=isLocal
        .Object
    })

#'
#' @export
#'
print.NlmeParallelHost <-function(obj)
{
    name=attr(obj,"hostName")
    numCores=attr(obj,"numCores")
    method=attr(attr(obj,"parallelMethod"),"method")
    print(paste(name,numCores,method))
}



#' @export parseParallelHostLine

parseParallelHostLine <-function(line){

    tokens=unlist(strsplit(line,"\\|"))

    machName=tokens[[1]]
    mType=tokens[[2]]
    pMode=tokens[[3]]
    name=tokens[[4]]
    scriptPath=tokens[[5]]
    sharedDrive=tokens[[6]]
    rLocation=tokens[[7]]
    numberOfCores=tokens[[8]]
    installDir = paste(sharedDrive,"InstallDirNLME",sep="/")

    host=NlmeParallelHost(
                          sharedDirectory=sharedDrive,
                          installationDirectory=installDir,
                          hostName=name,
                          machineName=machName,
                          hostType=mType,
                          numCores=numberOfCores,
                          rLocation=rLocation,
                          scriptPath=scriptPath,
                          parallelMethod=NlmeParallelMethod(pMode))
    host
}

#assign("parseParallelHostLine",parseParallelHostLine,env=.GlobalEnv)

#' export readHostConfigurations

readHostConfigurations <-function(filename){

    lines=readLines(filename)
    hosts=c()
    for ( l in lines ) {
        if ( nchar(l)> 1 &&  substr(l,1,1) != '#' ) {
            host=parseParallelHostLine(l)
            hosts=c(hosts,host)
        }
        
    }
    return(hosts)
}

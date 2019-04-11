

HEARTBEAT_SLEEP_TIME=5

SLAVE_IS_READY_MSG = 1
SLAVE_HAS_RESULTS_MSG = 2
SLAVE_CLOSING_MSG = 3
SLAVE_HEARTBEAT_MSG = 4

MASTER_PROCESS_JOB_MSG =1
MASTER_CLOSE_MSG =2
MASTER_PING_MSG = 3
MASTER_SHUTDOWN_MSG =4
MASTER_CREATE_TEMP_MSG =5
MASTER_REMOVE_TEMP_MSG =6
MASTER_PROCESS_QUANTILES_JOB_MSG =7



#
# Checks to see if MPI protocol was requested and if smpd is indeed running.
#
isMPIEnabled<-function(){

    stat=shell("smpd -status")
    if ( stat == 0 ){
        library(Rmpi)
        return(TRUE)
    } else {
        return(FALSE)
    }

}



NumberOfSlavesAvailable = 0 
NumberOfSlaves = 4 
sharedDirectory = ""
runLocation=""
nodeRemoteWorkDir=""
SubmissionHost="SGE_Grid"
SubmissionPlatform="Windows"




CheckUserCommands <-function(){
tryCatch (
{
  fileName = "nlme.cmd"
  if ( file.exists(fileName) ) {
      lines=scan(fileName,what=character(),sep="\n",quiet=TRUE)
      a=grep("STOP",lines)
      if ( length(a) != 0 ){
print("------------------- We are Canceled------------------")
          return("Canceled")
      }
  }
  return("" )
},
error=function(ex){
return("" )
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





initializeMPI<-function(numCores,SharedWorkingDir){
    library(Rmpi)
    NumberOfSlaves = numCores
    NumberOfSlavesAvailable = mpi.universe.size()


    if ( NumberOfSlaves > NumberOfSlavesAvailable ){
        NumberOfSlaves = NumberOfSlavesAvailable
    }
#    if ( sharedDirectory == "" ) {
#
#        mpi.spawn.Rslaves(nslaves=NumberOfSlaves)
#
#    } else {
#        baseDir=basename(runLocation)
#        remoteDirectory = sharedDirectory ;
#
#        nodeRemoteWorkDir=paste(gsub("\\","/",remoteDirectory,fixed=TRUE),"/",baseDir, sep="")
#
#        if ( ! file.exists( nodeRemoteWorkDir ) ) {
#
#			 
#			 dir.create(nodeRemoteWorkDir)
#        }
#        setwd(nodeRemoteWorkDir)
#        mpi.spawn.Rslaves(nslaves=NumberOfSlaves,mapdrive=TRUE)
#        setwd(runLocation)
#    }
    cwd=getwd()
    DEBUG_MASTER(paste("CWD on master",getwd()))
    DEBUG_MASTER("mapdrive = TRUE")
    setwd(SharedWorkingDir)
    DEBUG_MASTER(paste("setwd to shared drive",SharedWorkingDir))
    mpi.spawn.Rslaves(nslaves=NumberOfSlaves,mapdrive=TRUE)
    DEBUG_MASTER(paste("SharedWorkingDir",SharedWorkingDir))
    DEBUG_SLAVE(paste("SharedWorkingDir",SharedWorkingDir))
    setwd(cwd)
    if ( ! file.exists( SharedWorkingDir ) ) {
        print(paste("Unable to see ",SharedWorkingDir))
        mpi.quit()
    }

    if (mpi.comm.size() < 2) {
        print("Two or more slave processes are required.")
        mpi.quit()
    }
}


getUniqueHostNames<-function(){
#
# Get a list of node slave names
#
    allNames=c()
	allTempDirectories=c()
    uniqueNameArray=c()
    hostNames = mpi.remote.exec(Sys.info()[['nodename']])
	tempDirectories=mpi.remote.exec(Sys.getenv("TEMP"))
    localHost = Sys.info()[['nodename']]
    indx = 1 
	i = 1 
    for ( name in hostNames ){
        allNames[indx] = name ;
		allTempDirectories[indx] = tempDirectories[i]
        indx = indx + 1 
		i = i + 1 
        if ( name %in% uniqueNameArray ){
        }else {
            uniqueNameArray[length(uniqueNameArray)+1] = name
        }
    }
    assign("AllHostNames",allNames,envir=.GlobalEnv)
	assign("AllHostTempDirectories",allTempDirectories,envir=.GlobalEnv)

    if ( length(uniqueNameArray) == 1 ){
        LocalRunOnly = TRUE ;
    }
    else {
        LocalRunOnly = FALSE ;
    }
    assign("LocalRunOnly",LocalRunOnly,envir=.GlobalEnv)
    return(uniqueNameArray)
}



DEBUG_SLAVE <- function(str) {
debug=Sys.getenv("MPI_DEBUG")
if ( debug == "TRUE" )
    cat(str,file="slave.log",sep="\n",append=TRUE)
#    cat(str,file="c:/users/fred/slave.log",sep="\n",append=TRUE)
}

DEBUG_MASTER <- function(str) {
debug=Sys.getenv("MPI_DEBUG")
if ( debug == "TRUE" )
    cat(str,file="master.log",sep="\n",append=TRUE)
#    cat(str,file="c:/users/fred/master.log",sep="\n",append=TRUE)
}



MPICloseSlaves <- function() {
DEBUG_MASTER("MPICloseSlaves")
    closed_slaves = 0 ;
    emptyTask = 0
#    n_slaves <- mpi.comm.size()-1
    n_slaves <- mpi.comm.size()
    for ( s in (1:n_slaves)){
        mpi.send.Robj(emptyTask , s , MASTER_PING_MSG  )
    }
    for ( s in (1:n_slaves)){
        mpi.send.Robj(emptyTask , s , MASTER_SHUTDOWN_MSG  )
    }

DEBUG_MASTER("Finished MPICloseSlaves")
}




#
# Slave function to perform one replicate calculation/analysis
#
#
#  Uses the following tags for sent messages :
#      Ready for task                : SLAVE_IS_READY_MSG
#      Finished with this task       : SLAVE_HAS_RESULTS_MSG
#      heartbeat                     : SLAVE_HEARTBEAT_MSG
#      Exiting(cleanup )             : SLAVE_CLOSING_MSG
#
#  Uses the following tags for received messages :
#
#      Process this task             : MASTER_PROCESS_JOB_MSG
#      Handshake                     : MASTER_PING_MSG
#      Create Local Remote Directory : MASTER_CREATE_TEMP_MSG
#      Delete Local Remote Directory : MASTER_REMOVE_TEMP_MSG
#      Finish this task(cleanup)     : MASTER_CLOSE_MSG
#      close connection to master    : MASTER_SHUTDOWN_MSG
#

MPIRunOneJob <- function() {

DEBUG_SLAVE(sprintf("MPIRunOneJob: %d",mpi.comm.rank()))
    emptyTask <- 0 
    library(XML)
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
                # 
                # Run the job and return the results
                #
DEBUG_SLAVE(paste("Started Job",Job))
                results <- list(Success=TRUE,Job=Job)
                Sys.sleep(10)
                mpi.send.Robj(results,0,SLAVE_HAS_RESULTS_MSG  )
            }
        } else if (tag == MASTER_PROCESS_QUANTILES_JOB_MSG ) {
DEBUG_SLAVE("Message : MASTER_PROCESS_QUANTILES_JOB_MSG")
            if ( mpi.comm.rank() == 1 ){
                # wait a couple of minutes and then signal ready again.
                Sys.sleep(HEARTBEAT_SLEEP_TIME)
                mpi.send.Robj(emptyTask,0,SLAVE_HEARTBEAT_MSG ) 
            } else {
                results = list(Status=TRUE, Job=3)
                Sys.sleep(10)
                   mpi.send.Robj(results,0,SLAVE_HAS_RESULTS_MSG  )
            }
        } else if (tag == MASTER_CLOSE_MSG ) {
DEBUG_SLAVE("Message : MASTER_CLOSE_MSG")
                results = 0 
                mpi.send.Robj(results,0,SLAVE_CLOSING_MSG  )
        } else if (tag == MASTER_SHUTDOWN_MSG ) {
DEBUG_SLAVE(paste("MASTER_SHUTDOWN_MSG Job",Job))
            # The server is done with us, we will break out of the infinite
            # loop and exit
            done <- 1
        } else if (tag == MASTER_PING_MSG ) {
DEBUG_SLAVE(paste("MASTER_PING_MSG Job",Job))

        }
        # We'll just ignore any unknown messages
    }
DEBUG_SLAVE(paste("SLAVE END"))

}



MPIRunAllJobs <- function(numJobs){
    library(Rmpi)

print(paste("There are this many jobs" , numJobs))

    machinesProcessed=c()
    mpi.bcast.Robj2slave(MPIRunOneJob)
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

    emptyTask <- 0 
    closed_slaves = 0 
    CurrentJob <- 1
    numJobsFinished = 0
    numJobsFailed = 0

    n_slaves <- mpi.comm.size()-1
    slaveCloseList=list(dim=n_slaves)
    for ( s in (1:n_slaves)){
        slaveCloseList[s] = FALSE
    }
    # Call the function in all the slaves to get them ready to
    # undertake task
    if ( TRUE ){
        mpi.bcast.cmd(MPIRunOneJob())
    } else {
        for ( s in (1:n_slaves)){
                mpi.send.Robj(emptyTask , s , MASTER_PING_MSG  )
            }
    }


    while (closed_slaves < n_slaves) { 

print(sprintf("closed_slaves %d n_slaves %d",closed_slaves,n_slaves))

        # While there are more slaves that we started alive, keep waiting
        # for a message from them.
print("     Wait for a message")
        message <- mpi.recv.Robj(mpi.any.source(),mpi.any.tag()) 
        message_info <- mpi.get.sourcetag() 
        slave_id <- message_info[1] 
        tag <- message_info[2] 
print(sprintf("  RECEIVE slave_id %d tag %d",slave_id,tag))
#	if ( IsJobCanceled() ) {
#            for ( s in (1:n_slaves) ){
#                if ( slaveCloseList[s] != TRUE ){
#                    mpi.send.Robj(task , s, MASTER_CLOSE_MSG )
#                    slaveCloseList[s] = TRUE
#                }
#            }
#            res=list(Status='Canceled')
#            return(res)
#        }
        if (tag == SLAVE_IS_READY_MSG ) { 
            # slave is ready for a task. Give it the next task, or tell it tasks
            # are done if there are none. 
print(sprintf("CurrentJob %d numJobs %d",CurrentJob,numJobs))
            if ( CurrentJob <= numJobs ) {
                if ( slave_id == 1 ) {
                    # Reserve the 1st slave for the heartbeat
print(sprintf("               Sending MASTER_PROCESS_JOB_MSG to %d",slave_id))
                    mpi.send.Robj(emptyTask , slave_id , MASTER_PROCESS_JOB_MSG)
                }
                else {
    
 
                if ( slave_id == -1 ){
                    machineName = "Local"
                } else {
                    machineName = AllHostNames[slave_id]
#                #
                task <- list(Job= CurrentJob)
                
                CurrentJob = CurrentJob + 1 
print(sprintf("               Sending MASTER_PROCESS_JOB_MSG to %d job %d",slave_id,CurrentJob))
                mpi.send.Robj(task , slave_id, MASTER_PROCESS_JOB_MSG )
                }
                }
            }
	    else {
                if ( slaveCloseList[slave_id] != TRUE ){
print(sprintf("               Sending MASTER_CLOSE_MSG to %d",slave_id))
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
print(sprintf("      Received SLAVE_HAS_RESULTS_MSG from %d",slave_id))
            rep = message$Job
            if ( message$Success == TRUE ){         
		    numJobsFinished = numJobsFinished + 1 
            } else {
			    numJobsFailed = numJobsFailed + 1 
	   }
        } 
        else if (tag == SLAVE_CLOSING_MSG ) { 
            # A slave has closed down. 
print(sprintf("      Received SLAVE_CLOSING_MSG from %d",slave_id))
            closed_slaves <- closed_slaves + 1 
        } 
        else if (tag == SLAVE_HEARTBEAT_MSG ) { 
print(sprintf("      Received SLAVE_HEARTBEAT_MSG from %d",slave_id))
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
print(sprintf("END END"))
MPICloseSlaves()
}














#main<-function(){
#    args <- commandArgs(TRUE)
#    print(args[1])
#    if ( isMPIEnabled() ) {
#        initializeMPI()
#print("Done initializeMPI()")
#        getUniqueHostNames()
#print("Done getUniqueHostNames()")
#   
#        MPIRunAllJobs(as.integer(args[1]))
#
#print("mpi.close.Rslaves()")
#        mpi.close.Rslaves()
#print("Done mpi.close.Rslaves()")
#        print("mpi.quit()")
#        mpi.quit(save="no")
#        print("DONE mpi.quit()")
#    }
#    quit()
#}
#
#
#main()

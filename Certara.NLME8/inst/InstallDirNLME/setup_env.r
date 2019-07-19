
library(parallel)
library(XML)

Sys.setenv("PhoenixLicenseFile"="C:\\Program Files (x86)\\Pharsight\\Phoenix\\application\\Services\\Licensing\\lservrc")
Sys.setenv("shared_directory"=getwd())
#
# Check environment variables 
#
rootDirectory=Sys.getenv("NLME_ROOT_DIRECTORY")
if ( rootDirectory == "" )
{
    warning("environment variable NLME_ROOT_DIRECTORY is not defined, skipping i
nitialization")
    stop()
}
#
# Windows long path fix#
#
if ( Sys.info()["sysname"] == "Windows" )
     rootDirectory=shortPathName(rootDirectory)

#
# Setup environment variables
#
Sys.setenv("INSTALLDIR"=paste0(rootDirectory,"/InstallDirNLME"))

# 
# Example of a parallel host for Multicore execution
#
host1 = NlmeParallelHost(sharedDirectory=getwd(),
                         installationDirectory=rootDirectory,
                        parallelMethod=NlmeParallelMethod("MULTICORE"),
                        hostName="Multicore",
                        numCores=4)

#
# Example of  a TORQUE_MPI parallel host
#
host2 = NlmeParallelHost(sharedDirectory=rootDirectory,
                        parallelMethod=NlmeParallelMethod("TORQUE_MPI"),
                        hostName="TorqueGrid",
                        numCores=32)
#
# Example of an SGE_MPI grid
#					
host3 = NlmeParallelHost(sharedDirectory=rootDirectory,
                        parallelMethod=NlmeParallelMethod("SGE_MPI"),
                        hostName="SgeGrid",
                        numCores=8)	
						
host4 = NlmeParallelHost(sharedDirectory=getwd(),
                         installationDirectory=rootDirectory,
                        parallelMethod=NlmeParallelMethod("LOCAL_MPI"),
                        hostName="LOCAL_MPI",
                        numCores=4)							
#
# Make example hosts available
#
hosts=c(host1,host2,host3,host4)

#
# Some defaults
#
defaultParams=NlmeEngineExtraParams()
#defaultDataset=NlmeDataset()
defaultHost=hosts[[1]]



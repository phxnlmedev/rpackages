##############################################################################################################

##############       Setup environment variables and loading necessary packages                        ######

##############################################################################################################

# ==========================================================================================================
# setup environment
# ==========================================================================================================
# Disk location where RsNlme was installed
Sys.setenv("NLME_ROOT_DIRECTORY"="c:/Work/RsNlmeWorkDirectory")
# Directory containing scripts and libraries
Sys.setenv("INSTALLDIR"="C:/PROGRA~1/R/R-3.5.1/library/Certara.NLME8/InstallDirNLME")
# Workspace where RsNlme projects are created
Sys.setenv("NLME_WORKSPACE" = "c:/Work/NlmeInstall_07_10_19/Examples")


#
# goto your working directory where you copied the demo files
#
#setwd("XXXX")

#
# setup environment
#


# License
Sys.setenv("PhoenixLicenseFile"="C:\\Program Files (x86)\\Certara\\Phoenix\\application\\Services\\Licensing\\lservrc")
#Sys.setenv("NLME_HASH"="xxxx")





# ==========================================================================================================
# Load library
# ==========================================================================================================
# Phoenix NLME engine libraries
library(Certara.NLME8)

# library used to create PK/PD model in R 
library(RsNlme)

# package used to run the model in the linux grid 
library(ssh)

# packages using to create common diagnostic plots 
library(ggplot2)
library(xpose)
library(Xpose.Nlme)

# package used to do VPC plot
library(vpc)

# data processing library 
library(data.table)

# host setup: run locally with MPI enabled
host = NlmeParallelHost(sharedDirectory = Sys.getenv("NLME_ROOT_DIRECTORY")
                        ,installationDirectory = Sys.getenv("INSTALLDIR")
                        , parallelMethod = NlmeParallelMethod("LOCAL_MPI")
                        , hostName = "MPI"
                        , numCores = 4
)

# engine setup 
engineParams = NlmeEngineExtraParams(PARAMS_METHOD = METHOD_FOCE_ELS
                                     , PARAMS_NUM_ITERATIONS = 1000
                                     , PARAMS_SAND="TRUE"
)


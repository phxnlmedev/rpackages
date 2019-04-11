##############################################################################################################

##############       Setup environment variables and loading necessary packages                        ######

##############################################################################################################

# ==========================================================================================================
# setup environment
# ==========================================================================================================
# Disk location where RsNlme was installed
Sys.setenv("NLME_ROOT_DIRECTORY" = "C:/RsNLME")
# Directory containing scripts and libraries
Sys.setenv("INSTALLDIR" = paste0(NLME_ROOT_DIRECTORY,"/InstallDirNLME")
# Workspace where RsNlme projects are created
Sys.setenv("NLME_WORKSPACE" = NLME_ROOT_DIRECTORY)


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

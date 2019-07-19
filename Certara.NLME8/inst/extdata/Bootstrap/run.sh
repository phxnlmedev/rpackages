#!/bin/ksh
#
# Source in enviroment necessary to access the grid commands
#
if [ -e /opt/openmpi/setup_ompi.sh ]
then
. /opt/openmpi/setup_ompi.sh
fi

if [ "${NLME_ROOT_DIRECTORY}"X == "X" ]
then
    export NLME_ROOT_DIRECTORY=/vgrid/NLME_GRID/Next/Fred//CommandlineExamples
fi


#
# Path definition
#

#
# Where is the license server running
export PhoenixLicenseServer=s01rdus-vgrid00.certara.com
#
# Optional NLME_HASH code if there is no license server
#export NLME_HASH=1456414426

#
# Where Phoenix/NLME libraries/scripts are
#
export INSTALLDIR=${NLME_ROOT_DIRECTORY}/InstallDirNLME

#
# Where should temporary work directory/files be created
#
export shared_directory=${PWD}

# 
# Command to run bootstrap
#
${INSTALLDIR}/phx_bootstrap.sh TORQUE_MPI ${INSTALLDIR} ${shared_directory} `pwd`  3 1000 10 3 test.mdl cols1.txt data1.txt 11938 nlmeargs.txt "nlmeargs.txt cols1.txt data1.txt test.mdl" 32 95 TRUE

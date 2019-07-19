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
# Command to run stepwise covariate search
#
${INSTALLDIR}/phx_stepwise_covarsrch.sh TORQUE_MPI ${INSTALLDIR} ${shared_directory} `pwd` test.mdl nlmeargs.txt "nlmeargs.txt test.mdl cols1.txt data1.txt" 7 "Cl-age3 Cl-age32 Cl-sex V-age V-age32 V-sex V-age3"  -2LL:2,3,1,1,3,1,2 0.05 0.001 32 "Grid"   TRUE

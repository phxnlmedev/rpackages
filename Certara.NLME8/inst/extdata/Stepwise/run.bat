echo off
rem


if DEFINED NLME_ROOT_DIRECTORY goto L0
    set NLME_ROOT_DIRECTORY=C:\Work\TestInstallNLME

:L0
for %%I in ( %NLME_ROOT_DIRECTORY% ) DO set NLME_ROOT_DIRECTORY=%%~sI


rem
rem  Path definition
rem

rem
rem  Where is the license server running
rem set PhoenixLicenseServer=s01rdus-vgrid00.certara.com

rem
rem  Optional NLME_HASH code if there is no license server
rme set NLME_HASH=1450292831

rem
rem  Where Phoenix/NLME libraries/scripts are
rem
set INSTALLDIR=%NLME_ROOT_DIRECTORY%\InstallDirNLME

rem
rem  Where should temporary work directory/files be created
rem
set shared_directory=%CD%
for %%I in ( "%CD%" ) DO set workDir=%%~sI

echo %INSTALLDIR%\stepwise_covarsrch.bat MULTICORE %INSTALLDIR% %shared_directory% %workDir%  test.mdl nlmeargs.txt "nlmeargs.txt test.mdl cols1.txt data1.txt" 7 "Cl-ge3 Cl-age32 Cl-sex V-age V-age32 V-sex V-age3"  "-2LL:2,3,1,1,3,1,2" 0.05 0.001 2 "Stepwise"    TRUE
%INSTALLDIR%\phx_stepwise_covarsrch.bat MULTICORE %INSTALLDIR% %shared_directory% %workDir%  test.mdl nlmeargs.txt "nlmeargs.txt test.mdl cols1.txt data1.txt" 7 "Cl-ge3 Cl-age32 Cl-sex V-age V-age32 V-sex V-age3"  "-2LL:2,3,1,1,3,1,2" 0.05 0.001 2 "Stepwise"    TRUE

echo off
rem 
rem source in enviroment necessary to access the grid commands
rem


if DEFINED NLME_ROOT_DIRECTORY goto L0
    set NLME_ROOT_DIRECTORY="C:/PROGRA~1/R/R-3.5.1/library/Certara.NLME8"

:L0
for %%I in ( %NLME_ROOT_DIRECTORY% ) DO set NLME_ROOT_DIRECTORY=%%~sI

rem
rem  Path definition
rem

rem
rem  Where is the license server running
rem set PhoenixLicenseServer="C:\\Program Files (x86)\\Pharsight\\Phoenix\\application\\Services\\Licensing\\lservrc"

rem
rem  Optional NLME_HASH code if there is no license server
rem set NLME_HASH=1450292831

rem
rem  Where Phoenix/NLME libraries/scripts are
rem
set INSTALLDIR=%NLME_ROOT_DIRECTORY%\InstallDirNLME

rem
rem  Where should temporary work directory/files be created
rem

rem set shared_directory=%NLME_ROOT_DIRECTORY%
set shared_directory="%CD%

for %%I in ( "%CD%" ) DO set workDir=%%~sI


rem
rem  Command to run bootstrap
rem

%INSTALLDIR%\generic_run.bat LOCAL_MPI %INSTALLDIR% %shared_directory% %workDir% nlmeControlFile.txt 4 SingleNlme TRUE

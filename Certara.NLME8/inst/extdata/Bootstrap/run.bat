rem
rem
rem  Source in enviroment necessary to access the grid commands
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
rem set NLME_HASH=1430741471

rem
rem  Where Phoenix/NLME libraries/scripts are
rem
set INSTALLDIR=%NLME_ROOT_DIRECTORY%\InstallDirNLME

rem
rem  Where should temporary work directory/files be created
rem
set shared_directory=%CD%

for %%I in ( "%CD%" ) DO set workDir=%%~sI

rem  
rem  Command to run bootstrap
rem 
echo %INSTALLDIR%\phx_bootstrap.bat MULTICORE %INSTALLDIR% %shared_directory% %workDir%  3 1000 10 3 test.mdl cols1.txt data1.txt 11938 nlmeargs.txt "nlmeargs.txt cols1.txt data1.txt test.mdl" 4 95  TRUE
%INSTALLDIR%\phx_bootstrap.bat MULTICORE %INSTALLDIR% %shared_directory% %workDir%  3 1000 10 3 test.mdl cols1.txt data1.txt 11938 nlmeargs.txt "nlmeargs.txt cols1.txt data1.txt test.mdl" 4 95  TRUE

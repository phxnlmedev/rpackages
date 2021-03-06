echo on
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
rem set NLME_HASH=1450292831

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
rem  Command to run shotgun covariate search
rem
echo %INSTALLDIR%\phx_shotgun_covarsrch.bat MULTICORE %INSTALLDIR% %shared_directory% %workDir% test.mdl nlmeargs.txt "test.mdl nlmeargs.txt cols1.txt data1.txt" 4 "V-wt Cl-wt Cl-age V-sex" 4 shotgun  TRUE
%INSTALLDIR%\phx_shotgun_covarsrch.bat MULTICORE %INSTALLDIR% %shared_directory% %workDir% test.mdl nlmeargs.txt "test.mdl nlmeargs.txt cols1.txt data1.txt" 4 "V-wt Cl-wt Cl-age V-sex" 4 shotgun  TRUE

echo off
setlocal
rem
rem
rem Batch script to run NLME Shotgun covariate search on windows
rem
rem Args : Parallel method  to run the job on
rem        SERIAL | MultiCore | MPI
rem
rem

set script_dir=%~dp0
set help_cmd=%script_dir%\help.cmd
set win_common=%script_dir%\windows_common.bat
call %help_cmd% :countArgs ret_value %*
if %ret_value% == 11  goto Valid
if %ret_value% == 12 goto Valid

call %help_cmd% shotgun_usage
goto :eof

:Valid


set PARALLEL_MECHANISM=%1%
shift
set INSTALLATION_DIRECTORY=%1%
shift
set SHARED_DIRECTORY=%1%
shift
set LOCAL_DIRECTORY=%1%
shift
set MODEL_FILE=%1%
shift
set ARGS_FILE=%1%
shift
set FILES_TO_COPY=%1%
shift
set NUM_COVARIATES=%1%
shift
set COVARIATE_NAMES=%1%
shift
set NUM_PROCESSES=%1%
shift
set WORKFLOW_NAME=%1%
shift
set OPTIONAL_WORK_DIR=%1%

call %win_common% %PARALLEL_MECHANISM% %OPTIONAL_WORK_DIR% %FILES_TO_COPY% 


rem
rem See if we want to use our own installation directory
rem
if "%INSTALLATION_DIRECTORY%X" == "X"  goto L2
    set INSTALLDIR=%INSTALLATION_DIRECTORY%
:L2








echo Rscript %INSTALLDIR%\\phx_shotgun_covarsrch.r %PARALLEL_MECHANISM% %INSTALLATION_DIRECTORY% %SHARED_DIRECTORY% %LOCAL_DIRECTORY% %MODEL_FILE% %ARGS_FILE% %FILES_TO_COPY% %NUM_COVARIATES% %COVARIATE_NAMES% %NUM_PROCESSES% %WORKFLOW_NAME% 
set tstart=%time%
Rscript %INSTALLDIR%\\phx_shotgun_covarsrch.r %PARALLEL_MECHANISM% %INSTALLATION_DIRECTORY% %SHARED_DIRECTORY% %LOCAL_DIRECTORY% %MODEL_FILE% %ARGS_FILE% %FILES_TO_COPY% %NUM_COVARIATES% %COVARIATE_NAMES% %NUM_PROCESSES% %WORKFLOW_NAME% 
set tstop=%time%

@echo ----------------------------
@echo Start Time : %tstart%
@echo Stop Time  : %tstop%
@echo ----------------------------
if "%OPTIONAL_WORK_DIR%X" == "X"  goto eof
    @echo off
    xcopy /F /Q /Y Overall.csv .. > NUL
    xcopy /F /Q /Y StatusWindow.txt .. > NUL
    xcopy /F /Q /Y progress.xml .. > NUL
   cd ..
   DEL /Q /S TempWorkDirectory\* > NUL
   RMDIR /Q /S TempWorkDirectory > NUL

goto :eof

:eof

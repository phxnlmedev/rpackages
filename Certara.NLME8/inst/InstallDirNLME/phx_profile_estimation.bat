echo off
setlocal
rem
rem
rem Batch script to run NLME estimation splitting the input dataset by unique
rem columns and parallelizing resulting runs
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

call %help_cmd% profile_usage
goto eof
:Valid

set PARALLEL_MECHANISM=%1%
shift
set INSTALLATION_DIRECTORY=%1%
shift
set SHARED_DIRECTORY=%1%
shift
set LOCAL_DIRECTORY=%1%
shift
set CONTROL_FILE=%1%
shift
set NUM_COLUMNS=%1%
shift
set COLUMN_NAMES=%1%
shift
set PROFILE_SPECS=%1%
shift
set PROFILE_FLAG=%1%
shift
set NUM_PROCESSES=%1%
shift
set WORKFLOW_NAME=%1%
shift
set OPTIONAL_WORK_DIR=%1%

set FILES_TO_COPY=


call %win_common% %PARALLEL_MECHANISM% %OPTIONAL_WORK_DIR% "%FILES_TO_COPY%" %CONTROL_FILE%



rem
rem See if we want to use our own installation directory
rem
if "%INSTALLATION_DIRECTORY%X" == "X"  goto L2
    set INSTALLDIR=%INSTALLATION_DIRECTORY%
:L2





rem echo Rscript %INSTALLDIR%\\phx_profile_estimation.r %PARALLEL_MECHANISM% %INSTALLATION_DIRECTORY% %SHARED_DIRECTORY% %LOCAL_DIRECTORY% %CONTROL_FILE% %NUM_COLUMNS% %COLUMN_NAMES% %PROFILE_SPECS% %PROFILE_FLAG% %NUM_PROCESSES% %WORKFLOW_NAME% 
set tstart=%time%
Rscript %INSTALLDIR%\\phx_profile_estimation.r %PARALLEL_MECHANISM% %INSTALLATION_DIRECTORY% %SHARED_DIRECTORY% %LOCAL_DIRECTORY% %CONTROL_FILE% %NUM_COLUMNS% %COLUMN_NAMES% %PROFILE_SPECS% %PROFILE_FLAG% %NUM_PROCESSES% %WORKFLOW_NAME%
set tstop=%time%

@echo ----------------------------
@echo Start Time : %tstart%
@echo Stop Time  : %tstop%
@echo ----------------------------
if "%OPTIONAL_WORK_DIR%X" == "X"  goto eof
    @echo off
    xcopy /F /Q /Y Profile.csv .. > NUL
    xcopy /F /Q /Y out*.txt .. > NUL
    xcopy /F /Q /Y StatusWindow.txt .. > NUL
    cd ..
    DEL /Q /S TempWorkDirectory\* > NUL
    RMDIR /Q /S TempWorkDirectory > NUL

goto :eof

:eof

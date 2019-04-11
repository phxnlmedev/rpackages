echo off
setlocal
rem
rem
rem Batch script to run NLME bootstrap on windows
rem
rem Args : Parallel method  to run the job on
rem        SERIAL | MultiCore | MPI
rem
rem
set script_dir=%~dp0
set help_cmd=%script_dir%\help.cmd
set win_common=%script_dir%\windows_common.bat
call %help_cmd% :countArgs ret_value %*
if %ret_value% == 16  goto Valid
if %ret_value% == 17 goto Valid

call %help_cmd% bootstrap_usage
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
set METHOD=%1%
shift
set ITERATIONS=%1%
shift
set NUM_SAMPLES=%1%
shift
set NUM_RETRIES=%1%
shift
set MODEL_FILE=%1%
shift
set COL_DEF_FILE=%1%
shift
set DATA_FILE=%1%
shift
set START_SEED=%1%
shift
set EXTRA_ARGS_FILE=%1%
shift
set FILES_TO_COPY=%1%
shift
set NUM_PROCESSES=%1%
shift
set CONFIDENCE_LEVEL=%1%
shift
set OPTIONAL_WORK_DIR=%1%

call %win_common% %PARALLEL_MECHANISM% %OPTIONAL_WORK_DIR% %FILES_TO_COPY% 


echo Rscript  %INSTALLATION_DIRECTORY%\bootstrap.r %PARALLEL_MECHANISM% %INSTALLATION_DIRECTORY% %SHARED_DIRECTORY% %LOCAL_DIRECTORY% %METHOD% %ITERATIONS% %NUM_SAMPLES% %NUM_RETRIES% %MODEL_FILE% %COL_DEF_FILE% %DATA_FILE% %START_SEED%   %EXTRA_ARGS_FILE%   %FILES_TO_COPY% %NUM_PROCESSES% %CONFIDENCE_LEVEL%

set tstart=%time%
Rscript  %INSTALLATION_DIRECTORY%\bootstrap.r %PARALLEL_MECHANISM% %INSTALLATION_DIRECTORY% %SHARED_DIRECTORY% %LOCAL_DIRECTORY% %METHOD% %ITERATIONS% %NUM_SAMPLES% %NUM_RETRIES% %MODEL_FILE% %COL_DEF_FILE% %DATA_FILE% %START_SEED%   %EXTRA_ARGS_FILE%   %FILES_TO_COPY% %NUM_PROCESSES% %CONFIDENCE_LEVEL%
set tstop=%time%

@echo ----------------------------
@echo Start Time : %tstart%
@echo Stop Time  : %tstop%
@echo ----------------------------

if "%OPTIONAL_WORK_DIR%X" == "X"  goto eof
    @echo off
    xcopy /F /Q /Y Boot*.csv ..
    xcopy /F /Q /Y *.txt ..
    cd ..
    DEL /Q /S TempWorkDirectory\* > NUL
    RMDIR /Q /S TempWorkDirectory > NUL

goto :eof

:eof

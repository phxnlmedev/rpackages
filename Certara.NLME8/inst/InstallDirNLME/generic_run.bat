echo off
setlocal
rem
rem
rem Batch script to run NLME estimation
rem
rem Args : Parallel method  to run the job on
rem        SERIAL | MultiCore | MPI
rem
rem

set script_dir=%~dp0
set help_cmd=%script_dir%\help.cmd
set win_common=%script_dir%\windows_common.bat
call %help_cmd% :countArgs ret_value %*
if %ret_value% == 7  goto Valid
if %ret_value% == 8 goto Valid

call %help_cmd% simple_usage
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
set NUM_PROCESSES=%1%
shift
set WORKFLOW_NAME=%1%
shift
set OPTIONAL_WORK_DIR=%1%
set FILES_TO_COPY=

call %win_common% %PARALLEL_MECHANISM% %OPTIONAL_WORK_DIR% "%FILES_TO_COPY%" %CONTROL_FILE%

echo Rscript  %INSTALLDIR%\generic_run.r GENERIC %PARALLEL_MECHANISM% %INSTALLATION_DIRECTORY% %SHARED_DIRECTORY% %LOCAL_DIRECTORY%   %CONTROL_FILE% %NUM_PROCESSES% %WORKFLOW_NAME% 
set tstart=%time%
Rscript  %INSTALLDIR%\generic_run.r GENERIC %PARALLEL_MECHANISM% %INSTALLATION_DIRECTORY% %SHARED_DIRECTORY% %LOCAL_DIRECTORY%   %CONTROL_FILE% %NUM_PROCESSES% %WORKFLOW_NAME% 
set tstop=%time%

@echo ----------------------------
@echo Start Time : %tstart%
@echo Stop Time  : %tstop%
@echo ----------------------------
if "%OPTIONAL_WORK_DIR%X" == "X"  goto L2
    @echo off
    copy *.csv .. > NUL
    copy *.txt .. > NUL
    copy *.log .. > NUL
    copy *.asc .. > NUL
    copy *.dat .. > NUL
    cd ..
    DEL /Q /S TempWorkDirectory\* > NUL
    RMDIR /Q /S TempWorkDirectory > NUL
:L2


goto :eof

:eof

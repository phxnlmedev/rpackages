echo off
rem
rem
rem Batch script to run NLME bootstrap on windows
rem
rem Args : Parallel method  to run the job on
rem        SERIAL | MultiCore | MPI
rem


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


if "%PARALLEL_MECHANISM%" == "LSF" (
    copy %INSTALLDIR%\simple.tmpl.lsf .
    copy %INSTALLDIR%\BatchJobs.R.lsf .BatchJobs.R
	copy %INSTALLDIR%\batchtools.lsf.tmpl .
)


rem
rem See if we want to use our own installation directory
rem
if %INSTALLATION_DIRECTORY%X == "X"  goto L2
    set INSTALLDIR=%INSTALLATION_DIRECTORY%
:L2




rem echo  %INSTALLDIR%\\covar_search.r COVAR_SEARCH %PARALLEL_MECHANISM% %INSTALLATION_DIRECTORY% %SHARED_DIRECTORY% %LOCAL_DIRECTORY% %CONTROL_FILE% %NUM_PROCESSES% %WORKFLOW_NAME%
set tstart=%time%
Rscript  %INSTALLDIR%\\covar_search.r COVAR_SEARCH %PARALLEL_MECHANISM% %INSTALLATION_DIRECTORY% %SHARED_DIRECTORY% %LOCAL_DIRECTORY% %CONTROL_FILE% %NUM_PROCESSES% %WORKFLOW_NAME%
set tstop=%time%

@echo ----------------------------
@echo Start Time : %tstart%
@echo Stop Time  : %tstop%
@echo ----------------------------

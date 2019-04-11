
rem
rem See if we want to use our own installation directory
rem
if %INSTALLATION_DIRECTORY%X == "X"  goto L2
    set INSTALLDIR=%INSTALLATION_DIRECTORY%
:L2

if NOT %INSTALLDIR%X == "X"  goto L3
    set INSTALLDIR=C:\Progra~2\Certara\Phoenix\application\lib\NLME\Executables
:L3


if "%PARALLEL_MECHANISM%" == "LSF" (
    copy %INSTALLDIR%\simple.tmpl.lsf .
    copy %INSTALLDIR%\BatchJobs.R.lsf .BatchJobs.R
        copy %INSTALLDIR%\batchtools.lsf.tmpl .
)

set PATH=%INSTALLDIR%;%PATH%

echo on

if "%OPTIONAL_WORK_DIR%X" == "X"  goto L4
    set TEMP_WORK_DIRECTORY=TempWorkDirectory
    set LOCAL_DIRECTORY=%LOCAL_DIRECTORY%\%TEMP_WORK_DIRECTORY%
    if NOT EXIST %TEMP_WORK_DIRECTORY% (
        MKDIR %TEMP_WORK_DIRECTORY% 
    ) else (
        DEL /Q /S TempWorkDirectory\* > NUL
rem         RMDIR /Q /S TempWorkDirectory > NUL
    )
    set list=%FILES_TO_COPY%
    if  DEFINED %FILES_TO_COPY (
        set list=%FILES_TO_COPY:"=%
        set argsFile=
        set CONTROL_FILE
    ) else (
        for /F "skip=1 tokens=* delims=" %%L in ( %CONTROL_FILE% ) DO ( if not defined list set list=%%L  )

        set argsFile=
        for /F "skip=4 tokens=2 delims=,:" %%F in ( %CONTROL_FILE% ) DO if not defined argsFile set argsFile=%%F

    )

:L5
    for %%a in ( %list% %CONTROL_FILE% %argsFile% ) DO (
       XCOPY  /Q /Y %%a %TEMP_WORK_DIRECTORY%
    )

    cd %TEMP_WORK_DIRECTORY% 

:L4

exit /b

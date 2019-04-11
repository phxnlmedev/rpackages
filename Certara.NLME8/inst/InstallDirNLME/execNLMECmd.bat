echo off

rem
rem execNLMECmd.bat : Runs NLME on windows from command line.
rem
rem Args :
rem        %1 RUN_MODE   = COMPILE_AND_RUN COMPILE RUN
rem        %2 MODELFILE  = PML file to use for the model
rem        %3 LCWD       = Full path Working directory to start on local host
rem        %4 MPIFLAG    = MPIYES | MPINO
rem        %5 LOCAL_HOST =  YES | NO
rem        %6 NUM_NODES  = Number of mpi nodes
rem        %7 SHARED_DRIVE = Location of shared drive for remote MPI
rem        %8 RCWD         = Name of the working directory on remote/shared drive
rem        
rem
rem                          This directory will be created on either :
rem                          --  Shared Drive OR
rem                          --  %USERPROFILE%
rem                              on remote nodes and will be cleaned after the
rem                              run is finished.
rem 
rem        %9 FILES_TO_COPY= List of files to copy to remote node or shared directory
rem        %10 NLME_ARGS    = Arguments passed on to xxx.exe
rem 

set RUN_MODE=%1
set MODELFILE=%2
set WORKING_DIR=%3
set MPIFLAG=%4
set LOCAL_HOST=%5
set NUM_NODES=%6
set SHARED_DRIVE=%7
set RCWD=%8_%RANDOM%
set FILES=%9
shift
set NLME_ARGS=%9
shift
set CMD_HASHCODE=%9
shift
set NLME_EXE_POSTFIX=%9

pushd %WORKING_DIR%


echo WORKING_DIR=%WORKING_DIR%,MPIFLAG=%MPIFLAG%, LOCAL_HOST=%LOCAL_HOST%,NUM_NODES=%NUM_NODES%,SHARED_DRIVE=%SHARED_DRIVE%

set EXECUTION_DIR=

set HASH_TO_USE=

if  DEFINED CMD_HASHCODE goto L00000
set CMD_HASHCODE=""
:L00000
IF %CMD_HASHCODE% NEQ "" goto L0
   set HASH=%NLME_HASH%
   if DEFINED HASH goto L000
       set HASH_TO_USE=
       goto L0000
:L000
   set HASH_TO_USE=/hash %HASH%
:L0000
   goto L00
:L0
    set HASH_TO_USE=/hash %CMD_HASHCODE%
:L00

rem
rem First thing is to translate the model file and create NLME.exe
rem 


if DEFINED INSTALLDIR goto L1
   set INSTALLDIR=%PhoenixDir%\application\lib\NLME\Executables
rem   set INSTALLDIR=C:\Program Files (x86)\Pharsight\Phoenix\application\lib\NLME\Executables
:L1


IF DEFINED PhoenixGCCDir64 goto L021
        SET PhoenixGCCDir64=C:\PHSTMinGW64\
:L021

IF DEFINED PhoenixMPIDir64 goto L022
        SET PhoenixMPIDir64=C:\Program Files (x86)\Pharsight\MPICH2-1.4.1\
:L022


if "%NLME_BUILD_DEBUG%" == "" goto L031
    set DEBUG_FLAG=-g
    goto L032
:L031
    set DEBUG_FLAG=-O2
:L032

if "%NLME_PLATFORM%" == "INTEL" goto L033
    setlocal
    set path=%PhoenixGCCDir64%\bin;%PhoenixGCCDir64%\libexec\gcc\x86_64-w64-mingw32\4.9.2;%path%

    set CCOMPILER=%PhoenixGCCDir64%bin/gcc
    set COMPILER=%PhoenixGCCDir64%bin/gfortran
    set PLAT=g
    goto L034
:L033
    set CCOMPILER=icc
    set COMPILER=f90
    set PLAT=i
:L034

    set ARCH=
    set BUILD_FLAGS=-m64  -I.. -malign-double -fverbose-asm -c



REM Set environment variables
set GPP="%CCOMPILER%" %BUILD_FLAGS% %DEBUG_FLAG%
set G77="%COMPILER%" %DEBUG_FLAG%


REM Define any undefined args - note if OUTERRESULTFILE is not defined (normally the case in the examples)
REM it is hard coded to out.txt below.


if DEFINED OUTERRESULTFILE goto L01
        set OUTERRESULTFILE=out.txt
:L01

if DEFINED SUBMODELS goto L02
        set SUBMODELS=0

:L02


if %RUN_MODE% == RUN goto DoneCompileAndLink


echo      model=%MODELFILE%, nlmeDir=%INSTALLDIR%
REM ------------------------------------------------------
echo      Deleting files

REM following label is not used
:L03

if NOT EXIST .\Work goto L04
        rmdir /S /Q .\Work

:L04
md .\Work

REM ------------------------------------------------------
echo      Translating
echo on
echo "%INSTALLDIR%\\TDL5.exe" %HASH_TO_USE% /L .\%MODELFILE% .\Work > log.txt
echo off
"%INSTALLDIR%\\TDL5.exe" %HASH_TO_USE% /L .\%MODELFILE% .\Work > log.txt

echo      Done

if NOT ERRORLEVEL 1 goto L05
        echo      ERROR in model translation
        type log.txt
        goto err
:L05

if EXIST .\Work\Model.cpp goto L06
        echo      ERROR in generating Model.cpp
        goto err
:L06

        copy /y "%INSTALLDIR%\ModelAPI.h" .\Work
        xcopy /e /i "%INSTALLDIR%\ModelAPI" .\Work\ModelAPI
        xcopy /e /i "%INSTALLDIR%\mutil" .\Work\mutil
        copy /y "%INSTALLDIR%\Mutil.h" .\Work

REM ------------------------------------------------------
echo      Compiling *.cpp
echo %GPP%
        %GPP% .\Work\Model.cpp
        move /y Model.o .\Work

REM ------------------------------------------------------
echo      Linking

if %MPIFLAG%==MPINO goto LNoMPI
        %G77% --enable-stdcall-fixup -static .\Work\Model.o "%INSTALLDIR%\mpiNLME7%PLAT%%ARCH%.a" "%INSTALLDIR%\lapack%PLAT%%ARCH%.a" "%INSTALLDIR%\blas%PLAT%%ARCH%.a" "%PhoenixMPIDir64%lib\libfmpich2g.a" -lstdc++ -o mpiNLME7%NLME_EXE_POSTFIX%.exe

goto DoneCompiling
:LNoMPI
echo ---------------------
echo %G77% --enable-stdcall-fixup -static .\Work\Model.o "%INSTALLDIR%\NLME7%PLAT%%ARCH%.a" "%INSTALLDIR%\lapack%PLAT%%ARCH%.a" "%INSTALLDIR%\blas%PLAT%%ARCH%.a" -lstdc++ -o NLME7%NLME_EXE_POSTFIX%.exe
echo ---------------------

        %G77% --enable-stdcall-fixup -static .\Work\Model.o "%INSTALLDIR%\NLME7%PLAT%%ARCH%.a" "%INSTALLDIR%\lapack%PLAT%%ARCH%.a" "%INSTALLDIR%\blas%PLAT%%ARCH%.a" -lstdc++ -o NLME7%NLME_EXE_POSTFIX%.exe
goto DoneCompiling

:err
echo --------------- Model build failed ----------------
goto exit

:DoneCompiling
        set MODELFILE=
        set HASH=
        set GPP=
        set INSTALLDIR=


REM ------------------------------------------------------

:DoneCompileAndLink

if %RUN_MODE% == COMPILE  goto exit


rem
rem Are we doing MPI
rem

if %MPIFLAG%==MPINO goto LNoMPI


rem 
rem Do the logic for running on remote nodes here
rem 
if %LOCAL_HOST%==YES goto LLocalHost

rem
rem Figure out what hosts we will be running on
rem

setlocal EnableDelayedExpansion

"%PhoenixMPIDir64%\bin\smpd" -hosts > hostNames
set /a num_hosts=0
for /F %%i IN ( hostNames ) do (
    set /a num_hosts+=1
    set host_names_array[!num_hosts!]=%%i
)

for /L %%i in (1,1,%num_hosts%) do (
    echo !host_names_array[%%i]!
)

rem
rem If we are running on a remote node, we need to copy the needed files over to
rem 
rem Either ) Shared drive
rem OR     ) TEMP directory on the remote hosts
rem

set MAPALLFLAG=-mapall
set EXECUTABLE=.\mpiNLME7%NLME_EXE_POSTFIX%.exe
set LOCALONLY=
rem set FILES_TO_COPY=mpiNLME7.exe nlmeargs.txt cols1.txt data1.txt
set FILES_TO_COPY=mpiNLME7%NLME_EXE_POSTFIX%.exe %FILES:"=%


if %SHARED_DRIVE%=="" goto LNoSharedDriveCopy

    set EXECUTION_DIR=%SHARED_DRIVE:"=%\%RCWD%
    mkdir %EXECUTION_DIR%
    set EXECUTABLE=%EXECUTION_DIR%\mpiNLME7%NLME_EXE_POSTFIX%.exe
    set WD=-wdir %EXECUTION_DIR%     
	

    for %%x in (%FILES_TO_COPY%) do (
        copy  %%x %EXECUTION_DIR%
    )



goto LRunMPI

:LNoSharedDriveCopy
rem
rem There is no shared drive, we need to copy the files over to a local
rem directory on all remote nodes and then start the run.
rem
set EXECUTION_DIR=%USERPROFILE%\%RCWD%
set EXECUTABLE=%EXECUTION_DIR%\mpiNLME7%NLME_EXE_POSTFIX%.exe
set WD=-wdir %EXECUTION_DIR%


    for /L %%i in (1,1,%num_hosts%) do (
rem
rem Check to see if we are handling a  remote node, and do mpicp
rem

        if  /I NOT %COMPUTERNAME% == !host_names_array[%%i]! (
             "%PhoenixMPIDir64%\bin\mpiexec" -n 1 -host !host_names_array[%%i]! cmd /c mkdir %EXECUTION_DIR%
             for %%x in (%FILES_TO_COPY%) do (
                  "%PhoenixMPIDir64%\bin\mpiexec" -n 1 -host %COMPUTERNAME% mpicp.exe send %%x : -n 1 -host !host_names_array[%%i]! mpicp receive %EXECUTION_DIR%\%%x
             )
        )

rem
rem We are on the local hostnode, so just do a local copy
rem
        if  /I %COMPUTERNAME% == !host_names_array[%%i]! (
             mkdir %EXECUTION_DIR%
             for %%x in (%FILES_TO_COPY%) do (
                 copy  %%x %EXECUTION_DIR%
             )
        )

    )

goto LRunMPI


rem
rem local host logic
rem 
:LLocalHost

set WD=
set MAPALLFLAG=
set EXECUTABLE=.\mpiNLME7%NLME_EXE_POSTFIX%.exe
set LOCALONLY=-localonly
set EXECUTION_DIR=.\

rem 
rem Run the executable
rem 
:LRunMPI

if %EXECUTION_DIR%==.\ goto LRemovePrefix
SET NLME_ARGS=!NLME_ARGS:EXECUTION_DIR=%EXECUTION_DIR%!
goto LDoneRemovingPrefix
:LRemovePrefix
SET NLME_ARGS=%NLME_ARGS:EXECUTION_DIR\=%
:LDoneRemovingPrefix
"%PhoenixMPIDir64%\bin\mpiexec" -n %NUM_NODES% %WD% %MAPALLFLAG% %LOCALONLY%  %EXECUTABLE% %NLME_ARGS:"=% 1> err1.txt 2> err2.txt



rem
rem If we created a directory to work in, then clean it up.
rem

if %EXECUTION_DIR%==.\ goto LNoCleanup

rem
rem Copy all files from temporary directory to the run directoy
rem
echo %EXECUTION_DIR%
copy %EXECUTION_DIR%\* .



if NOT %SHARED_DRIVE%=="" goto LLocalCleanup
rem
rem Cleanup all directories on remote nodes.
rem
    for /L %%i in (1,1,%num_hosts%) do (
rem
rem Check to see if we are handling a  remote node, and do mpicp
rem
        if  /I NOT %COMPUTERNAME% == !host_names_array[%%i]! (
             "%PhoenixMPIDir64%\bin\mpiexec" -n 1 -host !host_names_array[%%i]! cmd /c del /Q %EXECUTION_DIR%
             "%PhoenixMPIDir64%\bin\mpiexec" -n 1 -host !host_names_array[%%i]! cmd /c rmdir  %EXECUTION_DIR%
        )

        if  /I %COMPUTERNAME% == !host_names_array[%%i]! (
            del /Q %EXECUTION_DIR% 
            rmdir %EXECUTION_DIR%
        )

    )

goto LNoCleanup

rem
rem remove the directory we have created on shared_directory drive
rem
:LLocalCleanup
del /Q %EXECUTION_DIR% 
rmdir %EXECUTION_DIR%


:LNoCleanup

rem

goto LDoneRunning

:LNoMPI
SET NLME_ARGS=%NLME_ARGS:EXECUTION_DIR\=%
".\NLME7%NLME_EXE_POSTFIX%.exe"  %NLME_ARGS:"=% 1> err1.txt 2> err2.txt

:LDoneRunning

:exit

EXIT /B

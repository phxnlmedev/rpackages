@echo off
REM The following 5-argument script is adequate to run all of the command line examples provide in the Phoenix distribution
REM in multiple processor/process mode using MPI.  The script is set up to use 8 processes, although that can easily be
REM changed my editing the "mpiexec" command below

REM Typical usage is    RunNLMEMPI METHOD ITERATIONS MODELFILE COLDEFFILE DATAFILE
REM  e.g. to run the Model 1 example with MODELFILE=lyon04.mdl, coldeffile=COLS04.TXT, anD DATAFILE=EMAX02.csv
REM with METHOD=5 (ELS FOCE) and maximum ITERATIONS=100, the command issued in a command prompt window is
REM "RunNLMEMPI 5 100 lyon04.mdl COLS04.TXT EMAX02.csv"
REM Note that addtional run control parameters may be used in conjunction with this script by use of an
REM appropriate file 'nlmeflags.asc' and/or 'qrpemflags.asc' in the working directory.  If these files are not
REM present, default run control parameters will be used.


del /q /f mpinlme7.exe
del /q /f fort.77
del /q /f out.txt

REM Set all the args in order so we know we got each one
REM and didn't duplicate any
set METHOD=%1
set ITERATIONS=%2
set MODELFILE=%3
set COLDEFFILE=%4
set DATAFILE=%5


set NLME_ARGS=-m %METHOD% -n %ITERATIONS% EXECUTION_DIR\%COLDEFFILE% EXECUTION_DIR\%DATAFILE% %RESULTFILE%
echo %NLME_ARGS%


call execNLMECmd.bat %MODELFILE% "%CD%" MPIYES NO 4 "\\s01-storage.certara.com\lsfdev\STL-LSF-DEV\work\FredTemp" NLME_TEMP_DIR "%COLDEFFILE% %DATAFILE%" "%NLME_ARGS%"
rem call execNLMECmd.bat %MODELFILE% "%CD%" MPIYES NO 4 "" NLME_TEMP_DIR "%COLDEFFILE% %DATAFILE%" "%NLME_ARGS%"

set METHOD=
set ITERATIONS=
set MODELFILE=
set COLDEFFILE=
set DATAFILE=

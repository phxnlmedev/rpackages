rem
rem Methods and help text for NLME windows scripts
rem 
rem
rem
rem

set func=%~1
@echo off
set _args=%*
call set _args=%%_args:*%1=%%

call:%func% %_args%
goto exit


:common_usage
    echo "   Arg1  =   PARALLEL_MECHANISM "
    echo "                NONE|MULTICORE|SGE|SGE_MPI|TORQUE|TORQUE_MPI|LSF|LSF_MPI"
    echo "   Arg2  =   INSTALLATION_DIRECTORY "
    echo "                Contains NLME libraries and shell scripts for platform ."
    echo "   Arg3  =   SHARED_DIRECTORY "
    echo "                Use this location to create temporary working dirs"
    echo "   Arg4  =   LOCAL_DIRECTORY "
    echo "                Input and results files are expected/placed here"
goto:eof

:method_usage
set /A argno=%~1
    echo "   Arg%argno%   =   METHOD "
    echo "                1 = QRPEM"
    echo "                2 = IT2S-EM"
    echo "                3 = FOCE L-B"
    echo "                4 = First Order"
    echo "                5 = FOCE ELS"
    echo "                5 = Laplacian"
    echo "                6 = Naive pooled"
goto:eof

:controlfile_usage
    set /A indx=%~1
    echo "   Arg%indx%  =   CONTROL_FILE "
    echo "    Nlme Job Control file of the form :"
    echo "    Line1 : name of pml model file"
    echo "    Line2 : Comma separated list of input files to copy to remote disk"
    echo "    Line3 : Comma separated list of output files to grab from NLME build processes"
    echo "    Line4 : Number of NLME scenarios to run"
    echo "    Line5..LineN : comma separated parameters for each scenario"
    echo "    JobName/ScenarioName"
    echo "    Main NLME run control file(i.e. nlmeargs.txt)"
    echo "    Extra arguments to pass to NLME7.exe ( optional )"
    echo "    Filename to check to declare the run a success(i.e. out.txt )"
    echo "    Optional Space separated List of out files to return from each run"
    echo "    Optional Space separated List of out files to return from each run"
    echo "    ( These will be tagged with the jobname )"
    echo "    Example : "
    echo "       test.mdl"
    echo "       test.mdl cols1.txt data1.txt"
    echo "       *.csv *.txt *.log *.LOG"
    echo "       1,nlmeargsCombined.txt:1,,out0001.txt,,dmp.txt iniest.csv IdEta.txt EtaEta.txt EtaCov.txt StrCov.txt EtaShrinkageBySubject.txt progress.txt doses.csv"
    echo "       ..."
goto:eof

:inputfile_usage
    set /A argno=%~1
    set /A argno1=argno+1
    set /A argno2=argno1+1
    echo "   Arg%argno%  =   MODEL_FILE "
    echo "                Text file containing PML model"
    echo "   Arg%argno1% =   COL_DEF_FILE "
    echo "                Mapping between model and data file column names"
    echo "   Arg%argno2% =   DATA_FILE "
    echo "                text file containing the observations"
goto:eof



:argsfile_usage
    set /A indx=%~1
    set /A indx1=indx+1
    set /A indx2=indx1+1
    echo "   Arg%indx%  =   MODEL_FILE "
    echo "                Text file containing PML model"
    echo "   Arg%indx1%" =   ARGS_FILE "
    echo "                Text file containing NLME options "
    echo "   Arg%indx2%" =   INPUT_FILES "
    echo "                Space separated list of files to copy to remote dir"
    echo "                e.g. test.mdl data1.txt cols1.txt nlmeargs.txt"
goto:eof

:profilearg_usage
    set /A indx=%~1
    set /A indx1=indx+1
    echo "   Arg%indx%  =   PROFILE_SPECS "
    echo "                   Space separated list of effects to pertubate"
    echo "                      EffectName,InitialValue,delta1,delta2,..."
    echo "   Arg%indx1% =   PROFILE_FLAG "
    echo "                      USE_DELTA|USE_PERCENTAGE"
    echo "                      USE_DELTA : Add deltas to initial value"
    echo "                      USE_PERCENTAGE : Add %delta to initial value"
goto:eof

:covariates_usage
    set /A indx=%~1
    set /A indx1=indx+1
    echo "   Arg%indx%  =   NUM_COVARIATES "
    echo "                Number of covariates in the model"
    echo "   Arg%indx1% =   COVARIATE_NAMES "
    echo "                Space separated list of covariate names"
goto:eof

:sortcolarg_usage
    set /A indx=%~1
    set /A indx1=indx+1
    echo "   Arg%indx%  =   NUM_SORT_COLUMNS "
    echo "                Number of columns to sort on "
    echo "   Arg%indx1%" =   SORT_COLUMN_NAMES "
    echo "                Space separated list of sort column names"
goto:eof


:criteria_usage
    set /A indx=%~1
    set /A indx1=indx+1
    set /A indx2=indx1+1
    echo "   Arg%indx% =   CRITERIA "
    echo "                Criteria to be used for convergance and degrees of freedom"
    echo "                specified as Criteria:n1,n2,n3,..."
    echo "                Criteria is one of :"
    echo "                    AIC | BIC | -2LL"
    echo "                comma separted list of degress of freedom for each covariate"
    echo "                Where degree of freedom is :"
    echo "                    1 for Noncategorical covariates"
    echo "                    Number of categories - 1 for Categorical covarites"
    echo "   Arg%indx1% =   ADD_P_VALUE "
    echo "                Threashold value for adding variables"
    echo "   Arg%indx2% =   REMOVE_P_VALUE "
    echo "                Threashold value for remove variables"
goto:eof

:numprocesses_usage
    set /A indx=%~1
    echo "   Arg%indx% =   NUM_PROCESSES "
    echo "                Number of processes to use for parallelization"
    echo "                e.g.Num cores on remote host or compute nodes on grid."
goto:eof

:profile_usage
    echo "Usage : profile_estimation.bat "
    call:common_usage
    call:controlfile_usage 5
    call:sortcolarg_usage 6
    call:profilearg_usage 8 
    call:numprocesses_usage 10
    echo "   Arg11 =   WORKFLOW_NAME "
    echo "                Name of the workflow"
goto:eof

:sortcol_usage
    echo "Usage : sortcol_estimation.bat "
    call:common_usage
    call:controlfile_usage 5
    call:sortcolarg_usage 6
    call:numprocesses_usage 8
    echo "   Arg9  =   WORKFLOW_NAME "
    echo "                Name of the workflow"
goto:eof


rem :simple_usage
rem     echo "Usage : /generic_run.bat "
rem     call:common_usage
rem     call:controlfile_usage 5
rem     call:numprocesses_usage 6
rem     echo "   Arg7  =   WORKFLOW_NAME "
rem     echo "                Name of the workflow"
rem goto:eof


:stepwise_usage
    echo "Usage : stepwise_covarsrch.bat "
    call:common_usage
    call:argsfile_usage 5
    call:covariates_usage 8
    call:criteria_usage 10
    call:numprocesses_usage 13
    echo "   Arg14 =   WORKFLOW_NAME "
    echo "                Name of the workflow"
goto:eof

:shotgun_usage
    echo "Usage : shotgun_covarsrch.bat "
    call:common_usage
    call:argsfile_usage 5
    call:covariates_usage 8
    call:numprocesses_usage 10
    echo "   Arg11 =   WORKFLOW_NAME "
    echo "                Name of the workflow"
goto:eof


:bootstrap_usage
    echo "Usage : bootstrap.bat "
    call:common_usage
    call:method_usage 5
    echo "   Arg6  =   ITERATIONS "
    echo "                Maximum number of iteration to use"
    echo "   Arg7  =   NUM_SAMPLES "
    echo "                Number of bootstrap replicates to run"
    echo "   Arg8  =   NUM_RETRIES "
    echo "                Maximum number of retries for each replicate"
    call:inputfile_usage 9
    echo "   Arg12 =   START_SEED "
    echo "                Random number generator seed"
    echo "   Arg13 =   EXTRA_ARGS_FILE "
    echo "                Text file containing NLME engine options"
    echo "   Arg14 =   FILES_TO_COPY "
    echo "                Space separated list of files to copy to remote dir"
    echo "                e.g. test.mdl data1.txt cols1.txt nlmeargs.txt"
    call:numprocesses_usage 15
    echo "   Arg16 =   CONFIDENCE_LEVEL "
    echo "                Confidence level for summarization.i.e 95"
goto:eof

:exit
exit /b



:countArgs - Counts arguments of a script

set args=%*

SET /A ARGS_COUNT=0
rem for %%A in (%*) DO SET /A ARGS_COUNT+=1
for %%A in (%args%) DO SET /A ARGS_COUNT+=1
set /A ARGS_COUNT=ARGS_COUNT-1
set %1=%ARGS_COUNT%
exit /b

:simple_usage
echo "Usage : /generic_run.bat "
call:common_usage
call:controlfile_usage 5
call:numprocesses_usage 6
echo "   Arg7  =   WORKFLOW_NAME "
echo "                Name of the workflow"
goto:eof

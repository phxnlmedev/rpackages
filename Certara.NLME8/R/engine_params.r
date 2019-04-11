

#' @export
PARAMS_IS_POPULATION="isPopulation"
#' @export
PARAMS_IS_PCWRES="isPCWRES"
#' @export
PARAMS_IS_QRPEM="isQRPEMStyleMethod"
#' @export
PARAMS_METHOD="method"
#' @export
PARAMS_NUM_ITERATIONS="numIterations"
#' @export
PARAMS_ODE="odeToUse"
#' @export
PARAMS_SCENARIOS="scenarios"
#' @export
PARAMS_NON_PARAMETRIC="xnp"
#' @export
PARAMS_ALLOW_ANAL_GRAD="anagrad"
#' @export
PARAMS_AUTO_LOG_TRANSFORM="logtran"
#' @export
PARAMS_RESTART="xrestart"
#' @export
PARAMS_NORDERAGQ="xnorderagq"
#' @export
PARAMS_FOCEHESE="xfocehess"
#' @export
PARAMS_VERBOSE="xverbose"
#' @export
PARAMS_PRESAMPLE="xpresample"
#' @export
PARAMS_MAPNP="xmapnp" 
#' @export
PARAMS_STDERR="xstderr"
#' @export
PARAMS_LAMETH="xlameth"
#' @export
PARAMS_LAGL_PRECISION="xlandig"
#' @export
PARAMS_LA_STDERR="xlatol"
#' @export
PARAMS_BLMETH="xblmeth"
#' @export
PARAMS_BLUP_PREC="xblndig"
#' @export
PARAMS_BLUP_STDERR="xbltol"
#' @export
PARAMS_REL_TOL="rtol"
#' @export
PARAMS_ABS_TOL="atol"
#' @export
PARAMS_ODE_MAX_STEPS="nmxstep"
#' @export
PARAMS_SORT="sort"
#' @export
PARAMS_CSV="csv"
#' @export
PARAMS_SAND="sand"
#' @export
PARAMS_ISAMPLE ="xisample"
#' @export
PARAMS_MAPASSIST="xmapassist"
#' @export
PARAMS_IMPSAMPDOF="ximpsampdof"
#' @export
PARAMS_MCPEM="xmcpem"
#' @export
PARAMS_PEM_RUNALL="xpemrunall"
#' @export
PARAMS_SIR_SAMP="xsirsamp"
#' @export
PARAMS_BURNIN="xburnin"
#' @export
PARAMS_NONOMEGA_BURN="xnonomegaburn"
#' @export
PARAMS_NO_BURN="xnoburn"
#' @export
PARAMS_START_FROM_POST="xstartfromsavedposteriors"
#' @export
PARAMS_ACC_RATIO="xaccratio"
#' @export
PARAMS_SCRAMBLE="xscramble"
#' @export
PARAMS_PCWRES_NREP="xpcwresnrep"
#' @export
PARAMS_PAR_DERIV_NUM_STEPS="pardern"
#' @export
PARAMS_PAR_DERIV_DELTA="parderd"

#' @export
PARAMS_IMP_SAMPLE_NORMAL=0
#' @export
PARAMS_IMP_SAMPLE_DBL_EXP=1
#' @export
PARAMS_IMP_SAMPLE_DIRECT=2
#' @export
PARAMS_IMP_SAMPLE_MIXTURE_2=-2
#' @export
PARAMS_IMP_SAMPLE_MIXTURE_3=-3

#'  QRPEM
#' @export
#'
METHOD_QRPEM=1

#'  ITSEM
#' @export
#'
METHOD_IT2S_EM=2

#'  FOCE_LB
#' @export
#'
METHOD_FOCE_LB=3

#'  FO
#' @export
#'
METHOD_FIRST_ORDER=4
#'  FOCE_ELS
#' @export
#'
METHOD_FOCE_ELS=5
#'  LAPLACIAN  
#' @export
#'
METHOD_LAPLACIAN=5

#'  NAIVE_POOLED
#' @export
#'
METHOD_NAIVE_POOLED=6

#'  
#' @export
#'
ODE_LSODE=1

#' 
#' @export
#'
ODE_LSODE_ANA_JAC=2
#' 
#' @export
#'
ODE_STIFF=2

#'
#' @export
#'
ODE_RK=3

#'
#' @export
#'
ODE_NON_STIFF=2

#' 
#' @export
#'
ODE_LSODA=4

#'
#' @export
#'
ODE_LSODA_ANA_JAC=5

#'
#' @export
#'
ODE_AUTO_DETECT=5

#' Matrix Exponent
#' @export
#'
MATRIX_EXP=6

#' No error
#' @export
#'
ERR_NONE=0

#' central  
#' @export
#'
ERR_CENTRAL_DIFF=1

#' forward diff
#' @export
#'
ERR_FORWARD_DIFF=2

#'
#' NlmeEngineExtraParams : Defines all extra engine parameters
#'
#' @param PARAMS_IS_POPULATION is this a population model
#' @param PARAMS_METHOD what engine to use
#'        METHOD_QRPEM|METHOD_IT2S_EM|METHOD_FOCE_LB|METHOD_FIRST_ORDER|
#'        METHOD_LAPLACIAN|METHOD_NAIVE_POOLED
#' @param PARAMS_NUM_ITERATIONS maximum number of iterations
#' @param PARAMS_ODE What ODE to use
#'        common : ODE_STIFF=2|ODE_NON_STIFF=3|ODE_AUTO_DETECT=5|MATRIX_EXP=6
#'        others : ODE_LSODE=1|ODE_LSODE_ANA_JAC=2|ODE_RK=3|ODE_LSODA=4|
#'                 ODE_LSODA_ANA_JAC=5|MATRIX_EXP=6
#' @param PARAMS_ALLOW_ANAL_GRAD n  0, or 1 to allow analytic gradients (default 0) 
#' @param PARAMS_NON_PARAMETRIC  n  number of nonparametric generations (or 0) 
#' @param PARAMS_RESTART n     0, or 1 to restart 
#' @param PARAMS_NORDERAGQ n   number of AGQ points per axis (or 0) 
#' @param PARAMS_FOCEHESE n    0 for numerical hessian, 1 for foce 
#' @param PARAMS_VERBOSE n     0 or 1 
#' @param PARAMS_STDERR n    0=ERR_NONE, 1=ERR_CENTRAL_DIFF, 2=ERR_FORWARD_DIFF
#' @param PARAMS_LAMETH n    LAGL Method 1, 2, or 3 
#' @param PARAMS_LAGL_PRECISION n      LAGL nDigit 
#' @param PARAMS_LA_STDERR n       LAGL tolerance 
#' @param PARAMS_BLUP_PREC n      BLUP Method 1, 2, or 3 
#' @param PARAMS_BLUP_PREC n      BLUP nDigit 
#' @param PARAMS_BLUP_STDERR n       BLUP tolerance 
#' @param PARAMS_PCWRES_NREP n  number of reps for PCWRES simulation 
#' @param PARAMS_ISAMPLE n     number of samples for QRPEM 
#' @param PARAMS_MAPASSIST n   0 (default) or >0, to enable map assist and specify periodicity 
#' @param PARAMS_MAPNP n       map naive-pool control 
#' @param PARAMS_IMPSAMPDOF n  importance sampling control (-3 to 30) 
#'        PARAMS_IMP_SAMPLE_NORMAL=0
#'        PARAMS_IMP_SAMPLE_DBL_EXP=1
#'        PARAMS_IMP_SAMPLE_DIRECT=2
#'        PARAMS_IMP_SAMPLE_MIXTURE_2=-2
#'        PARAMS_IMP_SAMPLE_MIXTURE_3=-3
#' @param PARAMS_MCPEM n       0 for QRPEM, 1 for MCPEM
#' @param PARAMS_PRESAMPLE n   0 (default) for no presampling
#' @param PARAMS_SIR_SAMP n     number of SIR samples
#' @param PARAMS_BURNIN n      number of burn-in samples
#' @param PARAMS_NONOMEGA_BURN n   0 (default) or 1, to use non-omega burn-in 
#' @param PARAMS_NO_BURN n   0 (default) or 1, to suppress burn-in 
#' @param PARAMS_START_FROM_POST n   0 (default) or 1, to start from saved posteriors 
#' @param PARAMS_ACC_RATIO n    acceptance ratio
#' @param PARAMS_SCRAMBLE n    0 for None, 1 for Owen, 2 for Faure-Tezuka
#' @param PARAMS_PAR_DERIV_NUM_STEPS n      partial derivative # steps
#' @param PARAMS_PAR_DERIV_DELTA n      partial derivative delta
#' @param PARAMS_AUTO_LOG_TRANSFORM n      0, or 1(default) to enable log-translate of data 
#' 
#' @export NlmeEngineExtraParams
#'
#' @examples  
#'            param=NlmeEngineExtraParams(PARAMS_METHOD=METHOD_FOCE_LB,
#'                                        PARAMS_NUM_ITERATIONS=1000)
#'            param=NlmeEngineExtraParams(PARAMS_METHOD=METHOD_QRPEM,
#'                                        PARAMS_NUM_ITERATIONS=300)
#'            param=NlmeEngineExtraParams(PARAMS_METHOD=METHOD_QRPEM,
#'                                        PARAMS_NUM_ITERATIONS=300,
#'                                        PARAMS_IS_POPULATION=TRUE,
#'                                        PARAM_ODE=ODE_STIFF)
#'
NlmeEngineExtraParams= setClass("NlmeEngineExtraParams",representation(
                                      isPopulation="logical",
                                      isPCWRES="numeric",
                                      isQRPEMStyleMethod="numeric",
                                      method="numeric",
                                      numIterations="numeric",
                                      odeToUse="numeric",
                                      scenarios="character",
                                      xnp="numeric",
                                      anagrad="numeric",
                                      logtran="numeric",
                                      xrestart="numeric",
                                      xnorderagq="numeric",
                                      xfocehess="numeric",
                                      xverbose="numeric",
                                      xpresample="numeric",
                                      xmapnp="numeric",
                                      xpcwresnrep="numeric",
                                      xstderr="numeric",
                                      xlameth="numeric",
                                      xlandig="numeric",
                                      xlatol="numeric",
                                      xblmeth="numeric",
                                      xblndig="numeric",
                                      xbltol="numeric",
                                      rtol="numeric",
                                      atol="numeric",
                                      xisample="numeric",
                                      xmapassist="numeric",
                                      ximpsampdof="numeric",
                                      xmcpem="numeric",
                                      xpemrunall="numeric",
                                      xsirsamp="numeric",
                                      xburnin="numeric",
                                      xnonomegaburn="numeric",
                                      xnoburn="numeric",
                                      nmxstep="numeric",
                                      sort="character",
                                      csv="character",
                                      sand="character",
                                      fisher="character",
                                      xstartfromsavedposteriors="numeric",
                                      xaccratio="numeric",
                                      xscramble="numeric"))
#                                      existingParamsFile="character") )

assign("NlmeEngineExtraParams",NlmeEngineExtraParams,env=.GlobalEnv)

setMethod("initialize","NlmeEngineExtraParams",
    function(.Object,
                      PARAMS_IS_POPULATION=TRUE,
                      PARAMS_IS_PCWRES=0,
                      PARAMS_METHOD=3,
                      PARAMS_NUM_ITERATIONS=1000,
                      PARAMS_ODE=6,
                      PARAMS_SCENARIOS="",
                      PARAMS_NON_PARAMETRIC=0,
                      PARAMS_ALLOW_ANAL_GRAD=0,
                      PARAMS_AUTO_LOG_TRANSFORM=1,
                      PARAMS_RESTART=0,
                      PARAMS_NORDERAGQ=1,
                      PARAMS_FOCEHESE=1,
                      PARAMS_VERBOSE=1,
                      PARAMS_PRESAMPLE=0,
                      PARAMS_MAPNP=0,
                      PARAMS_PCWRES_NREP=1,
                      PARAMS_STDERR=1,
                      PARAMS_LAMETH=1,
                      PARAMS_LAGL_PRECISION=7,
                      PARAMS_LA_STDERR=0.01,
                      PARAMS_BLMETH=1,
                      PARAMS_BLUP_PREC=13,
                      PARAMS_BLUP_STDERR=0.002,
                      PARAMS_REL_TOL=1E-06,
                      PARAMS_ABS_TOL=1E-06,
                      PARAMS_ISAMPLE=300,
                      PARAMS_MAPASSIST=0,
                      PARAMS_IMPSAMPDOF=0,
                      PARAMS_MCPEM=0,
                      PARAMS_PEM_RUNALL=0,
                      PARAMS_SIR_SAMP=10,
                      PARAMS_BURNIN=0,
                      PARAMS_NONOMEGA_BURN=0,
                      PARAMS_NO_BURN=0,
                      PARAMS_ODE_MAX_STEPS=5000,
                      PARAMS_SORT=" -sort ",
                      PARAMS_CSV=" -csv ",
                      PARAMS_SAND="",
                      PARAMS_FISHER="",
                      PARAMS_START_FROM_POST=0,
                      PARAMS_ACC_RATIO=0.1,
                      PARAMS_SCRAMBLE=1){
#                      existingParamsFile="") {
    if ( PARAMS_METHOD == METHOD_QRPEM || PARAMS_METHOD == METHOD_IT2S_EM )
        PARAMS_IS_QRPEM=1
    else
        PARAMS_IS_QRPEM=0
    if ( PARAMS_SORT != "" ) 
        PARAMS_SORT = "  -sort "
    if ( PARAMS_CSV != "" ) 
        PARAMS_CSV = "  -csv "
    if ( PARAMS_SAND != "" ) 
        PARAMS_SAND = "  -sand "
    if ( PARAMS_FISHER != "" ) 
        PARAMS_FISHER = "  -fscore "
                .Object@method=PARAMS_METHOD
                .Object@isPopulation=PARAMS_IS_POPULATION
                .Object@isQRPEMStyleMethod=PARAMS_IS_QRPEM
                .Object@isPCWRES=PARAMS_IS_PCWRES
                .Object@numIterations=PARAMS_NUM_ITERATIONS
                .Object@odeToUse=PARAMS_ODE
                .Object@scenarios=PARAMS_SCENARIOS
                .Object@xnp=PARAMS_NON_PARAMETRIC 
                .Object@anagrad=PARAMS_ALLOW_ANAL_GRAD
                .Object@logtran=PARAMS_AUTO_LOG_TRANSFORM
                .Object@xrestart=PARAMS_RESTART
                .Object@xnorderagq=PARAMS_NORDERAGQ
                .Object@xfocehess=PARAMS_FOCEHESE
                .Object@xverbose=PARAMS_VERBOSE
                .Object@xpresample=PARAMS_PRESAMPLE
                .Object@xmapnp=PARAMS_MAPNP
                .Object@xpcwresnrep=PARAMS_PCWRES_NREP
                .Object@xstderr=PARAMS_STDERR
                .Object@xlameth=PARAMS_LAMETH
                .Object@xlandig=PARAMS_LAGL_PRECISION
                .Object@xlatol=PARAMS_LA_STDERR
                .Object@xblmeth=PARAMS_BLMETH
                .Object@xblndig=PARAMS_BLUP_PREC 
                .Object@xbltol=PARAMS_BLUP_STDERR
                .Object@rtol=PARAMS_REL_TOL 
                .Object@atol=PARAMS_ABS_TOL
                .Object@xisample=PARAMS_ISAMPLE
                .Object@xmapassist=PARAMS_MAPASSIST
                .Object@ximpsampdof=PARAMS_IMPSAMPDOF
                .Object@xmcpem=PARAMS_MCPEM
                .Object@xpemrunall=PARAMS_PEM_RUNALL
                .Object@xsirsamp=PARAMS_SIR_SAMP
                .Object@xburnin=PARAMS_BURNIN
                .Object@xnonomegaburn=PARAMS_NONOMEGA_BURN
                .Object@xnoburn=PARAMS_NO_BURN
                .Object@xstartfromsavedposteriors=PARAMS_START_FROM_POST
                .Object@xaccratio=PARAMS_ACC_RATIO
                .Object@xscramble=PARAMS_SCRAMBLE
                .Object@nmxstep=PARAMS_ODE_MAX_STEPS
                .Object@sort=PARAMS_SORT
                .Object@csv=PARAMS_CSV 
                .Object@sand=PARAMS_SAND 
                .Object@fisher=PARAMS_FISHER 
#                .Object@existingParamsFile=existingParamsFile 
        .Object
    })



#'
#' @export
#'
generateScenarioArg <-function(scenario)
{
    argFlag=" /xe "
    submodels=c()
    for ( i in 1:50 ) {
        submodels=c(submodels,FALSE)
    }
    covariatesList=attr(scenario,"covariatesList")
    if ( length(covariatesList) == 0 || covariatesList == "" ) {
        argFlag=" /xe _ "
    } 
    else {
        covarsUsed = unlist(strsplit(covariatesList,split=","))
        for ( i in covarsUsed ) {
            argFlag=paste0(argFlag,"_",(as.integer(i)-1))
        }
        argFlag=paste0(argFlag,"_ ")
    }
    return(argFlag)
}


#'
#' @export
#'
GenerateParamsfile <-function(argsFilename, dataset,params,bootStratify="",
                              vpcOption=c(),
                              simOption=c(),
                               scenarios=c())
{
appendFlag=FALSE 

    numTodo = length(scenarios)
    done = FALSE
    current = 1 
    while ( !done ) 
    {
        if ( is.null(params)  || dataset@phoenixSourceDir != "" ) {
            lines=readLines(paste0(dataset@phoenixSourceDir,"/",
                                   dataset@engineParamsFile))
            for ( l in lines ) {
                cat(sprintf(" %s ",l),
               file=argsFilename,sep="\n",append=appendFlag)
                appendFlag=TRUE
            }
        }
        else {
        if ( numTodo != 0 ) 
        {
            arg = generateScenarioArg(scenarios[[current]])
            cat(sprintf(" %s ",arg),
               file=argsFilename,sep="\n",append=appendFlag)
            appendFlag=TRUE
        }
        else
        {
            cat(sprintf(" /e -1 "),
               file=argsFilename,sep="\n",append=appendFlag)
            appendFlag=TRUE
        }
    if ( length(vpcOption) != 0 ) {
        cat(sprintf(" -predn %d  ",
               attr(vpcOption,"numReplicates")),
               file=argsFilename,sep="\n",append=appendFlag)
        appendFlag=TRUE
        cat(sprintf(" -predout %s  ",
               attr(dataset,"outputFilename")),
               file=argsFilename,sep="\n",append=appendFlag)
        cat(sprintf(" -pcseed %d  ",
               attr(vpcOption,"seed")),
               file=argsFilename,sep="\n",append=appendFlag)

        obsVars = attr(vpcOption,"observationVars")

        numObsVars=length(obsVars)
        if ( numObsVars == 0 ) {
        # if none specified, then take defaults for all observation params
            obsVars = GetObservationVariables(NlmeDataset())
            numObsVars=length(obsVars)
        }
        predxValues=""
        quantilesValues =""
        bql =""
        secondaryValues =""
        binningValues =""
        binningOptions =""
        predCorrections =""
        predVarCorrs =""
        useNewFormat = TRUE
        if ( useNewFormat == FALSE ) 
            numObsVars = 1 
        for ( indx in 1:numObsVars ) {
            sep = ";"
            obs = obsVars[[indx]]
            xaxis=XAxisNames[attr(obs,"xaxis")]
            predxValues = paste0(predxValues,xaxis,sep) 
            qv=attr(obs,"quantilesValues")
            quantilesValues=paste0(quantilesValues,qv,sep)
            sv=attr(obs,"quantilesSecondaryValues")
            isBql=attr(obs,"isBql")
            if ( sv != "" )  
                secondaryValues=paste0(secondaryValues,sv,sep)
            else
                secondaryValues=paste0(secondaryValues,"-",sep)
            if ( isBql == TRUE )  
                bql=paste0(bql,"1",sep)
            else
                bql=paste0(bql,"0",sep)
            binningMethod=attr(obs,"binningMethod")
            binningOption=attr(obs,"binningOption")
            extraBinningOptions=""
            if ( binningMethod == VPC_BIN_NONE )
               binningValues = paste0(binningValues,"prednobin",sep) 
            if ( binningMethod == VPC_BIN_KMEANS )
               binningValues = paste0(binningValues,"predkmeans",sep) 
            if ( binningMethod == VPC_BIN_EXP_CENTERS ) {
               binningValues = paste0(binningValues,"predcenters",sep) 
               extraBinningOptions=attr(obs,"binningOption")
            }
            if (  binningMethod == VPC_BIN_EXP_BOUNDARIES ) {
               binningValues = paste0(binningValues,"predboundaries",sep) 
               extraBinningOptions=attr(obs,"binningOption")
            }
        }
        if ( predxValues != "" ) 
            cat(sprintf(" -predx %s  ",
               predxValues),
               file=argsFilename,sep="\n",append=appendFlag)
        if ( quantilesValues != "" ) 
            cat(sprintf(" -pcpi \"%s\"  ",
               quantilesValues),
               file=argsFilename,sep="\n",append=appendFlag)

        if ( secondaryValues != "" ) 
            cat(sprintf(" -pcpe \"%s\"  ",
               secondaryValues),
               file=argsFilename,sep="\n",append=appendFlag)
        if ( bql != "" ) 
            cat(sprintf(" -bql %s ",
               bql),
               file=argsFilename,sep="\n",append=appendFlag)
        if ( binningValues != "" ) {
            if ( useNewFormat ) {
                    cat(sprintf(" -predbin \"%s\" ",
                       binningValues),
                       file=argsFilename,sep="\n",append=appendFlag)
            } else {
                if ( extraBinningOptions == ""  )
                    cat(sprintf(" -%s ",
                       binningValues),
                       file=argsFilename,sep="\n",append=appendFlag)
                else
                    cat(sprintf(" -%s \"%s\"  ",
                       binningValues,
                       extraBinningOptions),
                       file=argsFilename,sep="\n",append=appendFlag)
            }
        }

        predCorrection=attr(vpcOption,"predCorrection")
        predVarCorr=attr(vpcOption,"predVarCorr")
        if ( predVarCorr == FALSE )  {
            if ( predCorrection == VPC_PRED_ADDITIVE )
                cat(sprintf(" -predpc -predpcadd "),
                   file=argsFilename,sep="\n",append=appendFlag)
            else if ( predCorrection == VPC_PRED_PROPOTIONAL )
                cat(sprintf(" -predpc "),
                   file=argsFilename,sep="\n",append=appendFlag)
        }
        else {
            if ( predCorrection == VPC_PRED_ADDITIVE )
                cat(sprintf(" -predvpc -predpcadd "),
                   file=argsFilename,sep="\n",append=appendFlag)
            else if ( predCorrection == VPC_PRED_PROPOTIONAL )
                cat(sprintf(" -predvpc "),
                   file=argsFilename,sep="\n",append=appendFlag)
        }
        pStratify=attr(vpcOption,"stratifyColumns")
        if ( pStratify  != "" ) {
            tokens=unlist(strsplit(pStratify,","))
            for ( i in 1:length(tokens))
                cat(sprintf("/pstrat%0d \"%s\"",i,tokens[[i]]),file=argsFilename,sep="\n",append=TRUE)
        }
    }
    if ( length(simOption) != 0 ) {
        if ( simOption@isPopulation == TRUE ) {
        cat(sprintf(" -predn %d  ",
               attr(simOption,"numReplicates")),
               file=argsFilename,sep="\n",append=appendFlag)
        appendFlag=TRUE
        cat(sprintf(" -predout %s  ",
               attr(dataset,"outputFilename")),
               file=argsFilename,sep="\n",append=appendFlag)
        cat(sprintf(" -pcseed %d  ",
               attr(simOption,"seed")),
               file=argsFilename,sep="\n",append=appendFlag)
        } else {
            cat(sprintf(" -simn %d  ",
                   attr(simOption,"numPoints")),
                   file=argsFilename,sep="\n",append=appendFlag)
            cat(sprintf(" -simmax %d  ",
                   attr(simOption,"maxXRange")),
                   file=argsFilename,sep="\n",append=appendFlag)
            cat(sprintf(" -simvary \"%s\"  ",
                   attr(simOption,"yVariables")),
                   file=argsFilename,sep="\n",append=appendFlag)
            cat(sprintf(" -simout %s",dataset@outputFilename),
                   file=argsFilename,sep="\n",append=appendFlag)
            if ( simOption@simAtObs == TRUE )
                cat(sprintf(" -simobs "),
                   file=argsFilename,sep="\n",append=appendFlag)
        }

    }
    cat(sprintf("/m %d /n %d /o %d %s",
           attr(params,PARAMS_METHOD),
           attr(params,PARAMS_NUM_ITERATIONS),
           attr(params,PARAMS_ODE),
           attr(params,PARAMS_SCENARIOS)),
           file=argsFilename,sep="\n",append=appendFlag)
    appendFlag=TRUE

    if ( bootStratify  != "" ) {
        tokens=unlist(strsplit(bootStratify,","))
        for ( i in 1:length(tokens))
            cat(sprintf("/bstrat%0d \"%s\"",i,tokens[[i]]),file=argsFilename,sep="\n",append=TRUE)
    }
    cat(sprintf("-xnp %d -anagrad %d -logtran %d -xrestart %d -xnorderagq %d -xfocehess %d -xverbose %d  -xstderr %d -rtol %f -atol %f ",
           attr(params,PARAMS_NON_PARAMETRIC),
           attr(params,PARAMS_ALLOW_ANAL_GRAD),
           attr(params,PARAMS_AUTO_LOG_TRANSFORM),
           attr(params,PARAMS_RESTART),
           attr(params,PARAMS_NORDERAGQ),
           attr(params,PARAMS_FOCEHESE),
           attr(params,PARAMS_VERBOSE),
           attr(params,PARAMS_STDERR),
           attr(params,PARAMS_REL_TOL),
           attr(params,PARAMS_ABS_TOL)),
           file=argsFilename,sep="\n",append=TRUE)

    cat(sprintf(" -xlameth %d  ",
           attr(params,PARAMS_LAMETH)),
           file=argsFilename,sep="\n",append=TRUE)
 cat(sprintf(" -xlandig %d ",
           attr(params,PARAMS_LAGL_PRECISION)),
           file=argsFilename,sep="\n",append=TRUE)
 cat(sprintf(" -xlatol %f ",
           attr(params,PARAMS_LA_STDERR)),
           file=argsFilename,sep="\n",append=TRUE)
 cat(sprintf(" -xblmeth %d ",
           attr(params,PARAMS_BLMETH)),
           file=argsFilename,sep="\n",append=TRUE)
 cat(sprintf(" -xblndig %d ",
           attr(params,PARAMS_BLUP_PREC)),
           file=argsFilename,sep="\n",append=TRUE)
 cat(sprintf(" -xbltol %f ",
           attr(params,PARAMS_BLUP_STDERR)),
           file=argsFilename,sep="\n",append=TRUE)


    if ( attr(params,PARAMS_IS_PCWRES) == 1 ) 
    {
        cat(sprintf("-xpresample %d -xmapnp %d -xpcwresnrep %d",
               attr(params,PARAMS_PRESAMPLE),
               attr(params,PARAMS_MAPNP),
               attr(params,PARAMS_PCWRES_NREP)),
               file=argsFilename,sep="\n",append=TRUE)
    }
    if ( attr(params,PARAMS_IS_QRPEM) == 1 ) 
    {
        cat(sprintf("-xisample %d -xmapassist %d -ximpsampdof %d -xmcpem %d -xpemrunall %d -xsirsamp %d -xburnin %d -xnonomegaburn %d -xstartfromsavedposteriors %d -xaccratio %f -xscramble %d",
               attr(params,PARAMS_ISAMPLE),
               attr(params,PARAMS_MAPASSIST),
               attr(params,PARAMS_IMPSAMPDOF),
               attr(params,PARAMS_MCPEM),
               attr(params,PARAMS_PEM_RUNALL),
               attr(params,PARAMS_SIR_SAMP),
               attr(params,PARAMS_BURNIN),
               attr(params,PARAMS_NONOMEGA_BURN),
               attr(params,PARAMS_START_FROM_POST),
               attr(params,PARAMS_ACC_RATIO),
               attr(params,PARAMS_SCRAMBLE)),
               file=argsFilename,sep="\n",append=TRUE)
    }


    cat(sprintf("-nmxstep %d %s %s %s",
           attr(params,PARAMS_ODE_MAX_STEPS),
           attr(params,PARAMS_SORT),
           attr(params,PARAMS_CSV),attr(params,PARAMS_SAND)),
           file=argsFilename,sep="\n",append=TRUE)
    }
    doseDef= attr(dataset,"doseDefFile")
    if ( doseDef != "" ) 
    {
        cat(sprintf("/d2 %s %s",
               doseDef,
               attr(dataset,"doseDataFile")),
               file=argsFilename,sep="\n",append=TRUE)
    }
    estDef= attr(dataset,"estimatesDefFile")
    if ( estDef != "" ) 
    {
        cat(sprintf("/d3 %s %s",
               estDef,
               attr(dataset,"estimatesDataFile")),
               file=argsFilename,sep="\n",append=TRUE)
    }
    ranEffDef= attr(dataset,"ranEffectDefFile")
    if ( ranEffDef != "" ) 
    {
        cat(sprintf("/d4 %s %s",
               ranEffDef,
               attr(dataset,"ranEffectDataFile")),
               file=argsFilename,sep="\n",append=TRUE)
    }
    cat(sprintf("%s %s %s",
               attr(dataset,"colDefFile"),
               attr(dataset,"dataFile"),
               attr(dataset,"outputFilename")),
               file=argsFilename,sep="\n",append=TRUE)
        if ( numTodo == 0 ) 
            done = TRUE
        else 
        {
            current = current + 1 
            if ( current > numTodo )
                done = TRUE
        }
    }
}

#'
#' @export
#'
GenerateControlfile <-function(dataset,params,workFlow,bootStratify="",
                               vpcOption=c(),
                               simOptions=c(),
                               scenarios=c())
{
    controlFilename = "jobControlFile.txt"
    argsFilename = "jobArgsCombined.txt"
    GenerateParamsfile(argsFilename, dataset,params,bootStratify,vpcOption,simOptions,scenarios=scenarios)
    cat(attr(dataset,"modelFile"),file=controlFilename,sep="\n",append=FALSE)
    cat(argsFilename,file=controlFilename,sep="",append=TRUE)
    cat(sprintf(" %s",attr(dataset,"dataFile")),file=controlFilename,sep="  ",append=TRUE)
    cat(sprintf(" %s",attr(dataset,"colDefFile")),file=controlFilename,sep="  ",append=TRUE)
    cat(sprintf(" %s",attr(dataset,"modelFile")),file=controlFilename,sep="  ",append=TRUE)
    if ( attr(dataset,"estimatesDefFile") != "" ) 
    {
        cat(sprintf(" %s",attr(dataset,"estimatesDefFile")),file=controlFilename,sep=" ",
            append=TRUE)
        cat(sprintf(" %s",attr(dataset,"estimatesDataFile")),file=controlFilename,sep=" ",
            append=TRUE)
    }
    if ( attr(dataset,"doseDefFile") != "" ) 
    {
        cat(sprintf(" %s",attr(dataset,"doseDefFile")),file=controlFilename,sep=" ",
            append=TRUE)
        cat(sprintf(" %s",attr(dataset,"doseDataFile")),file=controlFilename,sep=" ",
            append=TRUE)
    }
    if ( attr(dataset,"ranEffectDefFile") != "" ) 
    {
        cat(sprintf(" %s",attr(dataset,"ranEffectDefFile")),file=controlFilename,sep=" ",
            append=TRUE)
        cat(sprintf(" %s",attr(dataset,"ranEffectDataFile")),file=controlFilename,sep=" ",
            append=TRUE)
    }
    cat("",file=controlFilename,sep="\n",append=TRUE)
    cat("*.csv *.txt *.log *.asc *.dat *.LOG",file=controlFilename,sep="\n",append=TRUE)
    numTodo = length(scenarios)
    done = FALSE
    current = 1 
    if ( numTodo == 0 )
        cat("1",file=controlFilename,sep="\n",append=TRUE)
    else
        cat(sprintf("%d",numTodo),file=controlFilename,sep="\n",append=TRUE)
    while ( !done ) 
    {
        if ( numTodo != 0 ) {
            workFlow=attr(scenarios[[current]],"scenarioName")
        }
        cat(sprintf("%s,%s:%d,,%s,*.csv *.txt *.log *.asc *.dat *.LOG,progress.txt",
                workFlow,argsFilename,current,attr(dataset,"outputFilename")),
                file=controlFilename,sep="\n",append=TRUE)
        if ( numTodo == 0 )
            done = TRUE
        else
        {
            current = current + 1
            if ( current > numTodo )
                done = TRUE
        }
    }
    return(controlFilename)
}



test_that("class running a VPC simulation  ", {
    source("env.r")
    cwd=getwd()
    tmpdir = tempdir()
    dir.create(tmpdir)
    setwd(tmpdir)
    for ( f in unlist(strsplit(
"test.mdl data1.txt cols1.txt predout.csv.original",split=" " ) )) {
        location=system.file("extdata/Vpc",f,package="Certara.NLME8")
        file.copy(location,".")
    }

    dataset = NlmeDataset(outputFilename="predout.csv")

    host = NlmeParallelHost( parallelMethod=NlmeParallelMethod("MULTICORE"),
                        hostName="Local",
                        numCores=4)
    param = NlmeEngineExtraParams(PARAMS_METHOD=METHOD_NAIVE_POOLED,
                             PARAMS_NUM_ITERATIONS=0)

    obsVars = GetObservationVariables(dataset)

    observationParameters(obsVars[[1]])=c(xaxis=VPC_XAXIS_TAD,
                                      binningMethod=VPC_BIN_NONE,
                                      quantilesValues ="5,50,95")

    vpc = NlmeVpcSimParams(numReplicates=10,
                       seed=29423,
                       stratifyColumns="sex,race,dosing",
                       observationVars=obsVars)

    job=RunVpcSimulation(host,dataset,param,vpc)
print(job)
    results=readLines("predout.csv")
    oldResults=readLines("predout.csv.original")
    expect_that(results,equals(oldResults))
    setwd(cwd)
    unlink(tmpdir,recursive=TRUE)
})





test_that("class running a bootstrap ", {
    source("env.r")
    cwd=getwd()
    tmpdir = tempdir()
    dir.create(tmpdir)
    setwd(tmpdir)
    for ( f in unlist(strsplit(
"test.mdl data1.txt cols1.txt nlmeargs.txt BootOverall.csv.original",split=" " ) )) {
        location=system.file("extdata/Bootstrap",f,package="Certara.NLME8")
        file.copy(location,".")
    }
    host = NlmeParallelHost( parallelMethod=NlmeParallelMethod("MULTICORE"),
                        hostName="Local",
                        numCores=4)
    dataset=NlmeDataset()
    param = NlmeEngineExtraParams(PARAMS_METHOD=METHOD_FOCE_LB,
                             PARAMS_NUM_ITERATIONS=1000)

    boot = NlmeBootstrapParams(numReplicates=50,
                           randomNumSeed=1234)
    job=RunBootstrap(host,dataset,param,boot)

    results=readLines("BootOverall.csv")
    oldResults=readLines("BootOverall.csv.original")
    expect_that(results,equals(oldResults))
    setwd(cwd)
    unlink(tmpdir,recursive=TRUE)
})



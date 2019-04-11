

test_that("class running a shotgun covariate search ", {
    source("env.r")
    cwd=getwd()
    tmpdir = tempdir()
    dir.create(tmpdir)
    setwd(tmpdir)
    for ( f in unlist(strsplit(
"test.mdl data1.txt cols1.txt nlmeargs.txt covariates.txt Overall.csv.original"
   ,split=" " ) )) {
        location=system.file("extdata/Shotgun",f,package="Certara.NLME8")
        file.copy(location,".")
    }
    host = NlmeParallelHost( parallelMethod=NlmeParallelMethod("MULTICORE"),
                        hostName="Local",
                        numCores=4)

    param = NlmeEngineExtraParams(PARAMS_METHOD=METHOD_FOCE_ELS,
                             PARAMS_NUM_ITERATIONS=1000)

    dataset = NlmeDataset()

    cm=ReadNlmeCovariateEffectModel("covariates.txt")

    job=RunShotgunSearch(host,dataset,param,cm)

    results=readLines("Overall.csv")
    oldResults=readLines("Overall.csv.original")
    expect_that(results,equals(oldResults))
    setwd(cwd)

    unlink(tmpdir,recursive=TRUE)
})



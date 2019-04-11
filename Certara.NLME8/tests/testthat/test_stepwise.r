

test_that("class validation test", {
    expect_is(NlmeStepwiseParams(0.01, 0.001, "-2LL"),
         "NlmeStepwiseParams")
})

test_that("test setting values", {
    params = NlmeStepwiseParams(0.01, 0.001, "-2LL")
    expect_that(attr(params,"method"), equals("-2LL"))
    expect_that(attr(params,"addPValue"), equals(0.01))
    expect_that(attr(params,"removePValue"), equals(0.001))
})

test_that("class running a stepwise covariate search ", {
    source("env.r")
    cwd=getwd()
    tmpdir = tempdir()
    dir.create(tmpdir)
    setwd(tmpdir)
    for ( f in unlist(strsplit(
"test.mdl data1.txt cols1.txt nlmeargs.txt covariates.txt Overall.csv.original"
   ,split=" " ) )) {
        location=system.file("extdata/Stepwise",f,package="Certara.NLME8")
        file.copy(location,".")
    }
    host = NlmeParallelHost( parallelMethod=NlmeParallelMethod("MULTICORE"),
                        hostName="Local",
                        numCores=4)

    param = NlmeEngineExtraParams(PARAMS_METHOD=METHOD_FOCE_ELS,
                             PARAMS_NUM_ITERATIONS=1000)

    dataset = NlmeDataset()

    cm=ReadNlmeCovariateEffectModel("covariates.txt")

    sp = NlmeStepwiseParams(0.05, 0.001, "-2LL")

    job=RunStepwiseSearch(host,dataset,param,cm,sp)


    results=readLines("Overall.csv")
    oldResults=readLines("Overall.csv.original")
    expect_that(results,equals(oldResults))
    
    setwd(cwd)
    unlink(tmpdir,recursive=TRUE)
})



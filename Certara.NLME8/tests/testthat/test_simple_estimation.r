

test_that("class running a simple estimation", {
    source("env.r")
    cwd=getwd()
    tmpdir = tempdir()
    dir.create(tmpdir)
    setwd(tmpdir)
    for ( f in unlist(strsplit(
                "test.mdl data1.txt cols1.txt nlmeargs.txt out.txt.original",
                split=" " ) )) {
        location=system.file("extdata/Simple",f,package="Certara.NLME8")
        file.copy(location,".")
    }
    host=NlmeParallelHost()
    dataset=NlmeDataset()
    params=NlmeEngineExtraParams(PARAMS_METHOD=METHOD_QRPEM,
                                PARAMS_NUM_ITERATIONS=100)

    job=RunSimpleEstimation(host,dataset,params)

    results=readLines("out.txt")
    oldResults=readLines("out.txt.original")
    results[1]=""
    oldResults[1]=""
    expect_that(results,equals(oldResults))
    
    setwd(cwd)
    unlink(tmpdir,recursive=TRUE)
    
} )



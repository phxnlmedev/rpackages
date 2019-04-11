

test_that("class running a profile pertubation", {
    source("env.r")
    cwd=getwd()
    tmpdir = tempdir()
    dir.create(tmpdir)
    setwd(tmpdir)
    for ( f in unlist(strsplit(
"test.mdl data1.txt cols1.txt nlmeargs.txt Profile.csv.original"
   ,split=" " ) )) {
        location=system.file("extdata/Profile",f,package="Certara.NLME8")
        file.copy(location,".")
    }
    host = NlmeParallelHost( parallelMethod=NlmeParallelMethod("MULTICORE"),
                        hostName="Local",
                        numCores=4)

    param = NlmeEngineExtraParams(PARAMS_METHOD=METHOD_FOCE_ELS,
                             PARAMS_NUM_ITERATIONS=1000)

    dataset = NlmeDataset()

    scenarios=c()
    sortColumns=NlmeSortColumns("")

    profile1 = NlmeProfileVar("tvV",9.95482,"-2,-1,0,1,2")
    profiles= NlmeProfileParameters("USE_DELTA",c(profile1))

    job=RunProfilePertubation(host,
                    dataset,
                    param,
                    profiles,
                    sortColumns,
                    scenarios)


    results=readLines("Profile.csv")
    oldResults=readLines("Profile.csv.original")
    expect_that(results,equals(oldResults))
    setwd(cwd)
    unlink(tmpdir,recursive=TRUE)
})



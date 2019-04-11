

test_that("class running an estimation with sort columns ", {
    source("env.r")
    cwd=getwd()
    tmpdir = tempdir()
    dir.create(tmpdir)
    setwd(tmpdir)
    for ( f in unlist(strsplit(
"test.mdl data1.txt cols1.txt cols2.txt data2.txt cols3.txt data3.txt nlmeargs.txt Overall.csv.original"
   ,split=" " ) )) {
        location=system.file("extdata/SortByColumn",f,package="Certara.NLME8")
        file.copy(location,".")
    }
    host = NlmeParallelHost( parallelMethod=NlmeParallelMethod("MULTICORE"),
                        hostName="Local",
                        numCores=4)
    param = NlmeEngineExtraParams(PARAMS_METHOD=METHOD_NAIVE_POOLED,
                             PARAMS_NUM_ITERATIONS=1000)

    dataset = NlmeDataset(estimatesDefFile="cols3.txt",
                      estimatesDataFile="data3.txt",
                      doseDefFile="cols2.txt",
                      doseDataFile="data2.txt")

    scenario1=NlmeScenario("sc0001","1")
    scenario2=NlmeScenario("sc0002","")
    scenario3=NlmeScenario("sc0003","2")
    scenario4=NlmeScenario("sc0004","1,2")
    scenarios=c(scenario1,scenario2,scenario3,scenario4)

    sortColumns=NlmeSortColumns("group,sex")

    job=RunSortByEstimation(host,
                    dataset,
                    param,
                    sortColumns,
                    scenarios)


    results=readLines("Overall.csv")
    oldResults=readLines("Overall.csv.original")
    expect_that(results,equals(oldResults))
    setwd(cwd)
    unlink(tmpdir,recursive=TRUE)
})



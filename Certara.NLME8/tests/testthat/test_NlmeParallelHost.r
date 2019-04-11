
test_that("Parallel host class validation test", {
    expect_is(NlmeParallelHost(), "NlmeParallelHost")
})

test_that("test parallel host default values",  {

    Sys.setenv("NLME_ROOT_DIRECTORY"="C:/Test")
    host = NlmeParallelHost()
    expect_that(attr(host,'sharedDirectory'), equals("C:/Test"))
    expect_that(attr(host,'machineName'), equals(Sys.info()[["nodename"]]))
    expect_that(attr(host,'hostType'), equals(Sys.info()[["sysname"]]))
    expect_that(attr(attr(host,'parallelMethod'),'method'), equals("None"))
    expect_that(attr(host,'numCores'), equals(1))
})

test_that("Parallel method class validation test", {
    expect_is(NlmeParallelMethod(), "NlmeParallelMethod")
})

test_that("invalid parallel method", {
    expect_warning(NlmeParallelMethod("junk"))
})

test_that("test parallel method default value", {
    expect_that(attr(NlmeParallelMethod(),'method'), equals("None"))
})

test_that("test parallel host print method ", {
    expect_that(print(NlmeParallelHost(parallelMethod=NlmeParallelMethod("MULTICORE"),numCores=4)),equals(" 4 MULTICORE"))
})


test_that("test parallel host parse method ", {

    host1=parseParallelHostLine("LocalHost|Windows|None|MyMachine||/home/nlmeuser/NLME_SHARED_FRED/|C:\\Program Files\\R\\R-3.1.1\\bin\\x64|1|")

    expect_that(print(host1), equals("MyMachine 1 None"))

    host2=parseParallelHostLine("34.232.192.95|Linux|SGE_MPI|AWS_360||/shared/|/usr/bin|360|")
    expect_that(print(host2), equals("AWS_360 360 SGE_MPI"))
    expect_that(attr(host2,"installationDirectory"), 
                equals("/shared//InstallDirNLME"))

})


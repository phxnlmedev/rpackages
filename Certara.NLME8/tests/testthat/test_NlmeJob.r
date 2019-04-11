

test_that("class validation test", {
    expect_is(NlmeJob(host=NlmeParallelHost()),"NlmeJob")
})

test_that("test missing host argument", {
    expect_error(NlmeJob(),'argument "host" is missing, with no default')
})


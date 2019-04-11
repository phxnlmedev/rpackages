

test_that("class validation test", {
    expect_is(NlmeEngineExtraParams(),"NlmeEngineExtraParams")
})

 
test_that("test default values", {
    params = NlmeEngineExtraParams()
    expect_that(attr(params,"isPopulation"), equals(TRUE))
    expect_that(attr(params,"method"), equals(METHOD_FOCE_LB))
    expect_that(attr(params,"odeToUse"), equals(MATRIX_EXP))
    expect_that(attr(params,"isQRPEMStyleMethod"), equals(0))
    expect_that(attr(params,"numIterations"), equals(1000))
    expect_that(attr(params,"xfocehess"), equals(1))
    expect_that(attr(params,"sort"), equals("  -sort "))
    expect_that(attr(params,"sand"), equals(""))
    expect_that(attr(params,"csv"), equals("  -csv "))
    expect_that(attr(params,"fisher"), equals(""))
})


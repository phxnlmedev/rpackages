context("Expression Class Validation")

test_that("class validation test", {
    expect_is(NlmeExpression(blockName="testBlock"),"NlmeExpression")
})


test_that("parameter parsing test", {
    expr=NlmeExpression(blockName="testBlock")
    expect_equal(getNumInPorts(expr),0)
})


test_that("parameter parsing test 1", {
    expr=NlmeExpression(blockName="testBlock",structuralParams=c("Cl","V"))
    expect_equal(getNumInPorts(expr),2)
})

test_that("Generate statement test", {
    expr=NlmeExpression(blockName="testBlock",
                        structuralParams=c("Cl","V"),
                        codeLine="Cl/V")
    expect_equal(genStatements(expr),"testBlock = Cl/V")
})


test_that("test missing blockName argument", {
expect_error(NlmeExpression(),'argument "blockName" is missing, with no default')
})


test_that("class validation test", {
    comp=NlmeCompartment(inputName="A1",
                            aName="",
                            cName="C",
                            vName="V",
                            type=Central,
						    numDosePoints=2)
    expect_is(comp,"NlmeCompartment")
})

test_that("parameter parsing test", {
    comp=NlmeCompartment(inputName="A1",
                            aName="",
                            cName="C",
                            vName="V",
                            type=Central,
						    numDosePoints=2)
    expect_equal(getNumInPorts(comp),1)
})

test_that("parameter parsing test 1", {
    comp=NlmeCompartment(inputName="A1",
                            aName="",
                            cName="C",
                            vName="V",
                            type=Central,
						    numDosePoints=2)
    sps=genStructuralParameters(comp)
	strucParam = NlmeStructuralParameter("V","tvV","nV",TRUE,FALSE,1,"1")
    expect_equal(sps[[1]],strucParam)
})

test_that("Generate statement test", {
    comp=NlmeCompartment(inputName="A1",
                            aName="",
                            cName="C",
                            vName="V",
                            type=Central,
						    numDosePoints=2)
    stmts=genStatements(comp)
    expect_equal(gsub(" ","",stmts[[1]]),"C=/V")
})

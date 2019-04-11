context("Error Model Parameter Validation")

test_that("Test error model", {

    resEff1 = NlmeResidualEffect(ERR_ADDITIVE,"C")
	resEff2 = NlmeResidualEffect(ERR_MULTIPLICATIVE,"E")
	effectsList = c(resEff1,resEff2)
    errorModel = NlmeErrorModel(effectsList)
	
    model = pkmodel(numComp=1,
                isPopulation=TRUE,
                absorption = Intravenous,
                parameterization = Clearance,
                modelName="PK01Model",
                isTlag = FALSE,
                hasEliminationComp = FALSE,
                isClosedForm = TRUE)

	newModel = addToErrorModel(model,effectsList)
    newModelEffectsList = newModel@errorModel@effectsList
	
    startIdx = length(newModelEffectsList) - length(errorModel@effectsList) + 1
	endIdx = length(newModelEffectsList)
	expect_that(errorModel@effectsList,equals(c(newModelEffectsList[(startIdx:endIdx)])))
})

test_that("Test ObservationCategorical getNumInPorts", {

    obs = ObservationCategorical(
                                name="C",
                                observationName="CatObs",
                                blockName="",
                                linkFunction=InvLogit,
                                isInhibitory=FALSE,
                                slope="0",
                                offsetArray=c(),
                                dobefore="",
                                doafter="",
                                customCodeLines=c())

    expect_equal(getNumInPorts(obs),1)
})

test_that("Test ObservationCategorical getInPortName", {

    obs = ObservationCategorical(
                                name="C",
                                observationName="CatObs",
                                blockName="",
                                linkFunction=InvLogit,
                                isInhibitory=FALSE,
                                slope="0",
                                offsetArray=c(),
                                dobefore="",
                                doafter="",
                                customCodeLines=c())

    expect_equal(getInPortName(obs),"C")
})

test_that("Test ObservationCategorical genObserveError", {

    obs = ObservationCategorical(
                                name="C",
                                observationName="CatObs",
                                blockName="",
                                linkFunction=InvLogit,
                                isInhibitory=FALSE,
                                slope="0",
                                offsetArray=c(),
                                dobefore="",
                                doafter="",
                                customCodeLines=c())

	stmts = genObserveError(obs)

    expect_equal(gsub("\\s+","",stmts),"multi(CatObs,ilogit)")
})

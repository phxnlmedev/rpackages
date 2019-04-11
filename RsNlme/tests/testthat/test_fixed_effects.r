context("Fixed Effect Initialization")

test_that("Test fixed effects initialization", {

    source("env.r")

    location=system.file("extdata/FixedEffects","set_fixed_effect_1.txt",package="RsNlme")
    referenceModel = gsub("\\s+","",readLines(location))

    model = pkmodel(numComp=1,
                absorption = Intravenous,
                modelName="InitialModel")

    initFixedEffects(model)=c(tvV=16,tvCl=41)
	
    newModel = gsub("\\s+","",as.character(model@statements))

    expect_that(referenceModel,equals(newModel))
})

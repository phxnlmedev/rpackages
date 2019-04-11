test_that("Test fixed effects initialization", {

    source("env.r")

    location=system.file("extdata/FixedEffects","set_fixed_effect_1.txt",package="RsNlme")
print("-----------------------")
print(location)
    referenceModel = readLines(location)

    model = pkmodel(numComp=1,
                absorption = Intravenous,
                modelName="InitialModel")

    initFixedEffects(model)=c(tvV=16,tvCl=41)
	
    newModel = as.character(model@statements)

    expect_that(referenceModel,equals(newModel))
})

context("Test random effects initialization")
test_that("Test random effects initialization", {

    source("env.r")


    location=system.file("extdata/RandomEffects","test_rand_effect_1.txt",package="RsNlme")
    referenceModel = readLines(location)

    model = pkmodel(isPopulation = TRUE,
                numCompartments = 2,
                absorption = Intravenous,
                modelName="PK",
                isClosedForm = FALSE)

    initRandomEffects(model)=c(Diagonal,FALSE,"nV,nCl,nV2,nCl2","0.2,0.09,0.1,0.01")
    newModel = as.character(model@statements)

    expect_that(referenceModel,equals(newModel))
})
    

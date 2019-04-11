context("Test residual effects initialization")
test_that("Test residual effects initialization I", {

    source("env.r")


    location=system.file("extdata/ResidualEffects","set_residual_effect_1.txt",package="RsNlme")
    referenceModel = readLines(location)

    model = pkmodel(numComp=1,
                isPopulation=TRUE,
                absorption = Intravenous,
                parameterization = Clearance,
                modelName="PK01Model",
                isTlag = FALSE,
                hasEliminationComp = FALSE,
                isClosedForm = TRUE)



    residualEffect(model,"C")=c(errorType=Multiplicative, SD="0.094356",frozen=TRUE)
    newModel = as.character(model@statements)

    expect_that(referenceModel,equals(newModel))
})


test_that("Test residual effects initialization II ", {

    source("env.r")


    location=system.file("extdata/ResidualEffects","set_residual_effect_2.txt",package="RsNlme")
    referenceModel = readLines(location)

    model = pkmodel(numComp=1,
                isPopulation=TRUE,
                absorption = Intravenous,
                parameterization = Clearance,
                modelName="PK01Model",
                isTlag = FALSE,
                hasEliminationComp = FALSE,
                isClosedForm = TRUE)



    residualEffect(model,"C")=c(errorType=Power, SD="0.16",definition="3")

    newModel = as.character(model@statements)

    expect_that(referenceModel,equals(newModel))
})


test_that("Test residual effects initialization III ", {

    source("env.r")


    location=system.file("extdata/ResidualEffects","set_residual_effect_3.txt",package="RsNlme")
    referenceModel = readLines(location)

    model = pkmodel(numComp=1,
                isPopulation=TRUE,
                absorption = Intravenous,
                parameterization = Clearance,
                modelName="PK01Model",
                isTlag = FALSE,
                hasEliminationComp = FALSE,
                isClosedForm = TRUE)



    residualEffect(model,"C")=c(errorType=MixRatio, SD="0.16", definition="MR", frozen = TRUE)

    newModel = as.character(model@statements)

    expect_that(referenceModel,equals(newModel))
})
    

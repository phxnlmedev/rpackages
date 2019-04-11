context("1 Comp Extravascular PML generation")
test_that("PML generation", {

    source("env.r")


    location=system.file("extdata/PkModel","pk_1c_extravascular_ode.txt",package="RsNlme")
    referenceModel = gsub("\\s+","",readLines(location))

    model = pkmodel(isPopulation = TRUE,
                numCompartments = 1,
                absorption = Extravascular,
                modelName="Categorical",
                isClosedForm = FALSE)

    newModel = gsub("\\s+","",as.character(model@statements))

    expect_that(referenceModel,equals(newModel))
})
    


test_that("PML generation", {

junk

    source("env.r")


    location=system.file("extdata/PkModel","pk_1c_extravascular_ode.txt",package="RsNlme")
    referenceModel = readLines(location)

    model = pkmodel(isPopulation = TRUE,
                numCompartments = 1,
                absorption = Extravascular,
                modelName="Categorical",
                isClosedForm = FALSE)

    newModel = as.character(model@statements)

    expect_that(referenceModel,equals(newModel))
})
    

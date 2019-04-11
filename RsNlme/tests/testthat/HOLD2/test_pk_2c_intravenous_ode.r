context("2Comp Intra PML generation")
test_that("PML generation", {

    source("env.r")


    location=system.file("extdata/PkModel","pk_2c_intravenous_ode.txt",package="RsNlme")
    referenceModel = gsub("\\s+","",readLines(location))

    model = pkmodel(isPopulation = TRUE,
                numCompartments = 2,
                absorption = Intravenous,
                modelName="PK",
                isClosedForm = FALSE)

    newModel = gsub("\\s+","",as.character(model@statements))

    expect_that(referenceModel,equals(newModel))
})
    

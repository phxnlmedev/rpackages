context("Test random effects initialization")
test_that("Test random effects initialization", {

    source("env.r")


    location=system.file("extdata/RandomEffects","test_rand_effect_1.txt",package="RsNlme")
    referenceModel = gsub("\\s+","",readLines(location))

    model = pkmodel(isPopulation = TRUE,
                numCompartments = 2,
                absorption = Intravenous,
                modelName="PK",
                isClosedForm = FALSE)

    initRandomEffects(model)=c(Diagonal,FALSE,"nV,nCl,nV2,nCl2","0.2,0.09,0.1,0.01")
    newModel = gsub("\\s+","",as.character(model@statements))

    expect_that(referenceModel,equals(newModel))
})

test_that("Test multiple block effects initialization", {

    source("env.r")


    model = pkmodel(isPopulation = TRUE,
                numCompartments = 2,
                absorption = Intravenous,
                modelName="PK",
                isClosedForm = FALSE)

    initRandomEffects(model)=c(Diagonal,FALSE,"nV,nCl","0.2,0.09",
                               Block,TRUE,"nV2,nCl2","0.1,0,0.01")
    pos=grep("ranef",model@statements)
    randomEffectLine=gsub("\\s+","",model@statements[[pos[[1]]]])
 
    expect_that(randomEffectLine,equals("ranef(diag(nV,nCl)=c(0.2,0.09),block(nV2,nCl2)(freeze)=c(0.1,0,0.01))"))
})


test_that("Test specify initialization with missing variable ", {

    source("env.r")


    model = pkmodel(isPopulation = TRUE,
                numCompartments = 2,
                absorption = Intravenous,
                modelName="PK",
                isClosedForm = FALSE)

    initRandomEffects(model)=c(Diagonal,FALSE,"nV,nCl","0.2,0.09")
    pos=grep("ranef",model@statements)
    randomEffectLine=gsub("\\s+","",model@statements[[pos[[1]]]])
 
    expect_that(randomEffectLine,equals("ranef(diag(nV,nCl)=c(0.2,0.09),diag(nV2,nCl2)(freeze)=c(1,1))"))
})
    

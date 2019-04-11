context("Occasion Covariate PML generation")
test_that("Coccasion Covariate PML generation", {

    source("env.r")


    location=system.file("extdata/Covariates","test_occasion_covar_1.txt",package="RsNlme")
    referenceModel = gsub("\\s+","",readLines(location))

    model = pkmodel(isPopulation = TRUE,
                numCompartments = 2,
                absorption = Intravenous,
                modelName="PK",
                isClosedForm = FALSE)

    structuralParam(model,"Cl")=c(styl=LogNormal)
    structuralParam(model,"Cl2")=c(styl=Normal)
    structuralParam(model,"V")=c(styl=Combination)
    structuralParam(model,"V2")=c(styl=Log)
    structuralParam(model,"V3")=c(styl=Logit)

    Occasion = occasionCovariate("Occasion",
                             occasions = c(0,1,2),
                             occasionNames=c("0","1","2"))
    model=addCovariates(model,
                         c(Occasion),
                         c("V"="Occasion",
                           "V2"="Occasion",
                           "V3"="Occasion",
                           "Cl"="Occasion",
                           "Cl2"="Occasion"))

    newModel = gsub("\\s+","",as.character(model@statements))

    expect_that(referenceModel,equals(newModel))
})
    
test_that("Coccasion Covariate combination PML generation", {

    source("env.r")


    location=system.file("extdata/Covariates","test_occasion_covar_2.txt",package="RsNlme")
    referenceModel = gsub("\\s+","",readLines(location))


    model = pkmodel(isPopulation = TRUE,
                numCompartments = 3,
                absorption = Intravenous,
                modelName="PK",
                isClosedForm = FALSE)

    structuralParam(model,"Cl")=c(style=LogNormal)
    structuralParam(model,"Cl2")=c(style=Normal)
    structuralParam(model,"V")=c(style=Combination)
    structuralParam(model,"V2")=c(style=Log)
    structuralParam(model,"V3")=c(style=Logit)

    Occasion = occasionCovariate("Occasion",
                             occasions = c(0,1,2),
                             occasionNames=c("0","1","2"))

    sex=categoricalCovariate("sex",c(1,2,3),c("female","male"))

    model=addCovariates(model,
                    c(Occasion,sex),
                    c("V"="Occasion,sex",
                      "V2"="Occasion,sex",
                      "V3"="Occasion,sex",
                      "Cl"="Occasion,sex",
                      "Cl2"="Occasion,sex"))

    newModel = gsub("\\s+","",as.character(model@statements))

    expect_that(referenceModel,equals(newModel))
})



test_that("Coccasion Covariate random effect initialization ", {

    source("env.r")


    location=system.file("extdata/Covariates","test_occasion_covar_3.txt",package="RsNlme")
    referenceModel = gsub("\\s+","",readLines(location))


    model = pkmodel(isPopulation = TRUE,
                numCompartments = 3,
                absorption = Intravenous,
                modelName="PK",
                isClosedForm = FALSE)

    structuralParam(model,"Cl")=c(style=LogNormal)
    structuralParam(model,"Cl2")=c(style=Normal)
    structuralParam(model,"V")=c(style=Combination)
    structuralParam(model,"V2")=c(style=Log)
    structuralParam(model,"V3")=c(style=Logit)

    Occasion = occasionCovariate("Occasion",
                             occasions = c(0,1,2),
                             occasionNames=c("0","1","2"))

    sex=categoricalCovariate("sex",c(1,2,3),c("female","male"))

    model=addCovariates(model,
                    c(Occasion,sex),
                    c("V"="Occasion,sex",
                      "V2"="Occasion,sex",
                      "V3"="Occasion,sex",
                      "Cl"="Occasion,sex",
                      "Cl2"="Occasion,sex"))
    initOccasionRandomEffect(model,"Occasion")=c(0.1,0.2,0.3,0.4,0.5)
    newModel = gsub("\\s+","",as.character(model@statements))

    expect_that(referenceModel,equals(newModel))
})

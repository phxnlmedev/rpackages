

test_that("class validation test", {
    expect_is(NlmeDataset(),"NlmeDataset")
})

test_that("test default arguments", {
    expect_that(NlmeDatasetToString(NlmeDataset()), 
                equals(" cols1.txt data1.txt out.txt"))
})

test_that("test setting arguments", {
    expect_that(NlmeDatasetToString(NlmeDataset(outputFilename="results.txt")), 
                equals(" cols1.txt data1.txt results.txt"))

    expect_that(NlmeDatasetToString(NlmeDataset(doseDefFile="cols2.txt",doseDataFile="doses.csv")),
                equals(" /d2 cols2.txt doses.csv cols1.txt data1.txt out.txt"))

    expect_that(NlmeDatasetToString(NlmeDataset(estimatesDefFile="cols3.txt",estimatesDataFile="data3.txt")),
                equals(" /d3 cols3.txt data3.txt cols1.txt data1.txt out.txt"))

    expect_that(NlmeDatasetToString(NlmeDataset(ranEffectDefFile="cols4.txt",ranEffectDataFile="data4.txt")),
            equals(" /d4 cols4.txt data4.txt cols1.txt data1.txt out.txt"))

    expect_that(NlmeDatasetToString(NlmeDataset(
                 estimatesDefFile="cols3.txt", estimatesDataFile="data3.txt",
                 ranEffectDefFile="cols4.txt",ranEffectDataFile="data4.txt")),
            equals(" /d3 cols3.txt data3.txt /d4 cols4.txt data4.txt cols1.txt data1.txt out.txt"))

})


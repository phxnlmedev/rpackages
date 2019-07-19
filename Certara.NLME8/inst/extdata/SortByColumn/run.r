
Sys.setenv("INSTALLDIR"="C:/PROGRA~1/R/R-3.5.1/library/Certara.NLME8/InstallDirNLME")

library(Certara.NLME8)

host=hosts[[1]]


param = NlmeEngineExtraParams(PARAMS_METHOD=METHOD_NAIVE_POOLED,
                             PARAMS_NUM_ITERATIONS=1000)

dataset = NlmeDataset(estimatesDefFile="cols3.txt",
                      estimatesDataFile="data2.txt",
                      doseDefFile="cols2.txt",
                      doseDataFile="data2.txt",
                      workingDir = getwd() )


scenario1=NlmeScenario("Scenrio001","1")
scenario2=NlmeScenario("Scenrio002","")
scenario3=NlmeScenario("Scenrio003","2")
scenario4=NlmeScenario("Scenrio004","1,2")
scenarios=c(scenario1,scenario2,scenario3,scenario4)

sortColumns=NlmeSortColumns("group,sex")

file.remove("progress.xml")

job=RunSortByEstimation(host,
                    dataset,
                    param,
                    sortColumns,
                    scenarios,
                    workingDir=getwd())


while (!(NlmeJobStatus(job) == "Finished" || NlmeJobStatus(job) == "Failed") )
{
    print(NlmeJobStatus(job))
    print(job)
    Sys.sleep(5)
}
print(job)


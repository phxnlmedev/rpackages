Sys.setenv("INSTALLDIR"="C:/PROGRA~1/R/R-3.5.1/library/Certara.NLME8/InstallDirNLME")

library(Certara.NLME8)

host=hosts[[1]]

dataset = NlmeDataset(workingDir=getwd())

param = NlmeEngineExtraParams(PARAMS_METHOD=METHOD_FOCE_ELS,
                             PARAMS_NUM_ITERATIONS=1000)


scenarios=list()
sortColumns=NlmeSortColumns("")


profile1 = NlmeProfileVar("tvV",9.95482,"-2,-1,0,1,2")
#profile2 = NlmeProfileVar("tvCl",0.919,"-0.5,0,0.5")
profiles= NlmeProfileParameters("USE_DELTA",c(profile1))

file.remove("progress.xml")

job=RunProfilePertubation(host,
                    dataset,
                    param,
                    profiles,
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


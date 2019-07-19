Sys.setenv("INSTALLDIR"="C:/PROGRA~1/R/R-3.5.1/library/Certara.NLME8/InstallDirNLME")
library(Certara.NLME8)

host=hosts[[1]]

dataset = NlmeDataset(workingDir=getwd())

param = NlmeEngineExtraParams(PARAMS_METHOD=METHOD_FOCE_ELS,
                             PARAMS_NUM_ITERATIONS=1000)


cm=ReadNlmeCovariateEffectModel("covariates.txt")

file.remove("progress.xml")

job=RunShotgunSearch(host,dataset,param,cm,workingDir=getwd())

while (!(NlmeJobStatus(job) == "Finished" || NlmeJobStatus(job) == "Failed") )
{
    print(NlmeJobStatus(job))
    print(job)
    Sys.sleep(5)
}
print(job)


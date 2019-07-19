Sys.setenv("INSTALLDIR"="C:/PROGRA~1/R/R-3.5.1/library/Certara.NLME8/InstallDirNLME")
library(Certara.NLME8)

host=hosts[[1]]

dataset = NlmeDataset(workingDir=getwd())

param = NlmeEngineExtraParams(PARAMS_METHOD=METHOD_FOCE_ELS,
                             PARAMS_NUM_ITERATIONS=1000)

cm=ReadNlmeCovariateEffectModel("covariates.txt")

sp = NlmeStepwiseParams(0.05, 0.001, "-2LL")

file.remove("progress.xml")

job=RunStepwiseSearch(host,dataset,param,cm,sp,workingDir=getwd())


while (!(NlmeJobStatus(job) == "Finished" || NlmeJobStatus(job) == "Failed") )
{
    print(NlmeJobStatus(job))
    print(job)
    Sys.sleep(5)
}
print(job)


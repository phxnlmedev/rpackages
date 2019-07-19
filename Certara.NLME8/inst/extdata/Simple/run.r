
#Sys.setenv("INSTALLDIR"="C:/PROGRA~1/R/R-3.5.1/library/Certara.NLME8/InstallDirNLME")

library(Certara.NLME8)

host=hosts[[4]]

dataset = NlmeDataset(workingDir=getwd())

param = NlmeEngineExtraParams(PARAMS_METHOD=METHOD_QRPEM,
                             PARAMS_NUM_ITERATIONS=100,
							 PARAMS_MAPASSIST=0,
                             PARAMS_LA_STDERR=0.01,
                             PARAMS_BLUP_STDERR=0.002)
file.remove("progress.xml")

job=RunSimpleEstimation(host,dataset,param,workingDir=getwd())

print(job)
while (!(NlmeJobStatus(job) == "Finished" || NlmeJobStatus(job) == "Failed") )
{
    print(NlmeJobStatus(job))
    print(job)
    Sys.sleep(5)
}
print(job)



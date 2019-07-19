#
# Load Certara NLME library
#
library(Certara.NLME8)
#
# Default setup_env.r creates a Multicore parallel host with 4 cores
#
host=hosts[[1]]
#
# default NLME Dataset consists of Phoenix generated files
# test.mdl cols1.txt data1.txt
#
dataset = NlmeDataset(workingDir=getwd())
#
# Engine parameters with 1000 iterations and FOCE_LB engine
#
param = NlmeEngineExtraParams(PARAMS_METHOD=METHOD_FOCE_LB,
                             PARAMS_NUM_ITERATIONS=1000)
#
# 50 boot replicates with seed = 1234
#
boot = NlmeBootstrapParams(numReplicates=50,
                           randomNumSeed=1234)
#
# Run a bootstrap job, by default on Linux it will run in the background. 
# On windows it waits till the job is finished
#
file.remove("progress.xml")
job=RunBootstrap(host,dataset,param,boot,workingDir=getwd())
#
# Report Progress on the job
#
while (!(NlmeJobStatus(job) == "Finished" || NlmeJobStatus(job) == "Failed") )
{
    print(NlmeJobStatus(job))
    print(job)
    Sys.sleep(5)
}
print(job)


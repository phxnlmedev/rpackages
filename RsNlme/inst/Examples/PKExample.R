
# Setup environment variables and loading necessary packages 
source("c:/Work/NlmeInstall_07_10_19/Examples/SetUpEnv_LoadRPackages.R")
setwd("c:/Work/NlmeInstall_07_10_19/Examples/")
#
# Look at the input data
#
input=read.csv("16subjects.csv")
View(input)
#
# Graph time vs. Conc
#
df=data.frame(time=input$Act_Time,conc=input$Conc,subject=input$Subject)
ggplot(data=df,aes(x=time,y=conc))+scale_y_log10()+geom_point(colour=df$subject)+
       geom_line(aes(x=df$time,y=df$conc,group=df$subject,colour=df$subject))

#
# Format the input file and add a gender_code for the model
#
colnames(input)
input$Gender_Code=as.numeric(input$Gender)
#
# Statistics on possible covariates
#
mean(input$Age)
mean(input$BodyWeight)
#
# 2 Compartment population PK model with intravaneous observation
# Help is avaialble by typing ?pkmodel
#
model = pkmodel(numComp=2,
                isPopulation=TRUE,
                absorption = Intravenous,
                modelName="InitialModel")
#
# Looks at model variables and map them to data columns
#
initColMapping(model)=input

#modelColumnMapping(model)
#colnames(input)
#print(modelVariableNames(model))

modelColumnMapping(model)=c(id="Subject", CObs="Conc",A1="Amount")
#
# Change error model
#
residualEffect(model,"C")=c(errorType=Multiplicative,SD="0.16")

#
# Set initial estimates on model variables
#
#initFixedEffects(model)=c(tvV=16,tvCl=7,tvV2=41,tvCl2=14)

#
# Where to execute fitting
#
host = NlmeParallelHost(sharedDirectory=Sys.getenv("NLME_ROOT_DIRECTORY"),
                        installationDirectory = Sys.getenv("INSTALLDIR"),
                        parallelMethod=NlmeParallelMethod("LOCAL_MPI"),
                        hostName="MPI",
                        numCores=4)
#
# Evaluate the model and pick reaso
estimatesUI(model,unique(input$Subject),host)

# Accept initial estimates picked from shiny app
effects=getInitialEstimates(model)
initFixedEffects(model) = effects
print(model)
#
# If you need to edit any of the generated files
#
# model2=RsNlme::editModel(model,dataset)
#
#

#
# NLME engine to use
#
engineParams = NlmeEngineExtraParams(PARAMS_METHOD=METHOD_FOCE_ELS,
                                     PARAMS_NUM_ITERATIONS=1000,
                                     PARAMS_SAND="TRUE")

#
# Do the model fitting
#
job=fitmodel(host,engineParams,model=model)


#
# Look at the results
#
library(xpose)
library(Xpose.Nlme)

xp=xposeNlme(dir=model@modelInfo@workingDir,modelName="Initial Model")
list_vars(xp)

doxpose(xp)


dv_vs_pred(xp)
#dv_vs_ipred(xp)
res_vs_pred(xp,res="CWRES",type="ps")
res_vs_idv(xp,res="CWRES",type="ps")
ind_plots(xp)
#res_vs_idv(xp,res="WRES")
eta_distrib(xp)
#res_distrib(xp,res="IWRES")
#cov_distrib(xp, type = 'd')
#cov_distrib(xp, type = 'h')
#cov_distrib(xp, type = 'dh')
#prm_distrib(xp, type = 'h')
#res_distrib(xp, type = 'hr', res = c('IWRES', 'WRES'))
#cov_qq(xp,log="xy")
#cov_qq(xp)
eta_qq(xp)
#res_qq(xp, res = c('IWRES', 'WRES'))

#
# Accept fixed effects estimates from fitting run
#
model= acceptAllEffects(model)

#
# Make a copy of the model
#
covarModel = copyModel(model,modelName="CovarModel")

#
# Examine Some covariates
#
sex=categoricalCovariate("sex",c(1,2),c("female","male"))

weight=NlmeCovariateParameter("weight",
                              centerValue="70",
                              continuousType  =CovarNumber,
                              direction=Forward)

age=NlmeCovariateParameter("age")

covarModel=addCovariates(covarModel,
                         c(sex,weight,age),
                         c("V"="weight,age",
                           "Cl"="sex,weight"))


#
# initialize etas
#
initRandomEffects(covarModel)=c(Diagonal,FALSE,"nCl,nV,nCl2,nV2", "0.1,0.1,0.1,0.1")
#
# Map newly created model variables
initColMapping(covarModel)=input
#print(RsNlme::modelColumnMapping(covarModel))
#print(columnNames(dataset))
#print(modelVariableNames(covarModel))

modelColumnMapping(covarModel)=c(id="Subject", CObs="Conc",A1="Amount","sex"="Gender_Code")


#
# Run a stepwise covariate search
#
sp = NlmeStepwiseParams(0.01, 0.001, "-2LL")
mhost = NlmeParallelHost(sharedDirectory=Sys.getenv("NLME_ROOT_DIRECTORY"),
                        installationDirectory = Sys.getenv("INSTALLDIR"),
                        parallelMethod=NlmeParallelMethod("MULTICORE"),
                        hostName="MULTICORE",
                        numCores=4)
#
# remote execution on the grid
#
job=stepwiseSearch(mhost,
                      engineParams,
                      covariateModel(covarModel),
                      sp,
                      covarModel,
                      runInBackground = FALSE )
print(job)

#
# view results from covariate search
#
overall=read.csv(paste0(covarModel@modelInfo@workingDir,"/Overall.csv"))
View(overall)

stepwiseLines=readLines(paste0(covarModel@modelInfo@workingDir,"/Stepwise.txt"))
View(stepwiseLines)

#
# Reset the covariates to list suggested by covariate search
#
covarModel=resetCovariateEffects(covarModel)
covariateEffect(covarModel,"sex","Cl")=COVAR_EFF_YES
covariateEffect(covarModel,"age","V")=COVAR_EFF_YES

#
# Fit the model
#
covarModel=generatePMLModel(covarModel)

job=fitmodel(host,engineParams,covarModel,runInBackground = FALSE)

#
# Analyze results
xp=xposeNlme(dir=covarModel@modelInfo@workingDir,modelName="Covariate Model")
list_vars(xp)

dv_vs_pred(xp)

cov_distrib(xp, type = 'd')
cov_distrib(xp, type = 'h')

eta_distrib(xp, type = 'h')

cov_qq(xp)


#
# Validate the model with bootstrap
#
#
# Copy the model
#
bootModel = copyModel(covarModel,modelName="BootModel")


#
# 5 boot replicates with seed = 1234
#
bootParam = NlmeBootstrapParams(numReplicates=15,
                           randomNumSeed=1234,
                           stratifyColumns="Weight")
#
# Run a bootstrap job
#
job=bootstrap(mhost,engineParams,bootParam,runInBackground = FALSE,model=bootModel)

print(job)

out=read.csv(paste0(bootModel@modelInfo@workingDir,"/out.csv"))
View(out)

overall=read.csv(paste0(bootModel@modelInfo@workingDir,"/BootOverall.csv"))
View(overall)

theta=read.csv(paste0(bootModel@modelInfo@workingDir,"/BootTheta.csv"))
View(theta)

varCovar=read.csv(paste0(bootModel@modelInfo@workingDir,"/BootVarCoVar.csv"))
View(varCovar)

omega=read.csv(paste0(bootModel@modelInfo@workingDir,"/BootOmega.csv"))
View(omega)



#----------------------------
# VPC
#
vpcModel= copyModel(covarModel,modelName="VpcModel")


observationNames(vpcModel)

obsVars = GetObservationVariables(vpcModel@dataset)

observationParameters(obsVars[[1]])=c(xaxis=VPC_XAXIS_T,
                                      binningMethod=VPC_BIN_KMEANS,
                                      quantilesValues ="5,50,95",
                                      quantilesSecondaryValues="4,50,96")

simTable1 = NlmeSimTableDef("Simtable01.csv","0,0.5,1,2,4,8,24","C,CObs,sex,age",FALSE)

vpc = NlmeVpcParams(numReplicates=1000,
                       seed=1957,
                       stratifyColumns="sex",
                       observationVars=obsVars,
                       simulationTables=c(simTable1))


job=vpcmodel(host,vpc,vpcModel,runInBackground = FALSE)

library(vpc)
simData=getSimData(input,stratifyColumns = "sex",simFile="out.txt",modelDir=vpcModel@modelInfo@workingDir)
obsData=getObsData(input,modelDir=vpcModel@modelInfo@workingDir)

vpc1 <- vpc(sim = simData, obs = obsData, vpcdb = TRUE)

plot_vpc(
  vpc1,
  show = list(obs_dv = TRUE, obs_ci = FALSE),
  xlab = "Time (hours)", ylab = "Concentration", title="VPC!")


vpc::vpc(simData,obsData,
         bins = c(0, 2, 4, 6, 8, 10, 16, 25),
         stratify=c(mapCovariate(input,"sex")),
         facet="columns",
         pi = c(0.10,0.90),
         ci = c(0.20,0.80))

vpc::vpc(sim=simData,obs=obsData,stratify=c(mapCovariate(input,"sex",workingDir=vpcModel@modelInfo@workingDir)),show=c(pi_ci=TRUE,pi_as_area=TRUE,obs_dv=TRUE))+theme_classic()
st2=read.csv("simTable01.csv")
View(st2)



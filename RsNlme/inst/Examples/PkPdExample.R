
# Setup environment variables and loading necessary packages 
source("c:/Work/NlmeInstall_07_10_19/Examples/SetUpEnv_LoadRPackages.R")
setwd("c:/Work/NlmeInstall_07_10_19/Examples/")
#
# Look at the data
#
input=read.csv("PKPD_Data.csv")

df=data.frame(time=input$Time,conc=input$Conc,subject=input$ID)
ggplot(data=df,aes(x=time,y=conc))+scale_y_log10()+geom_point(colour=df$subject)+
  geom_line(aes(x=df$time,y=df$conc,group=df$subject,colour=df$subject))

#
# PK/Emax extravascular model
#
pkpdmodel = pkemaxmodel(numComp=1,
                     absorption = Extravascular,
                     isSequential = FALSE  ,
                     hasEliminationComp = FALSE,
                     checkBaseline = TRUE,
                     hasEffectsCompartment = TRUE,
                     modelName="PkPdModel")

dataset=defaultDataset(pkpdmodel,input)

#
# Map data columns to model variables
#
initColMapping(pkpdmodel)= input

#print(modelColumnMapping(pkpdmodel))
#print(columnNames(dataset))
#print(modelVariableNames(pkpdmodel))

modelColumnMapping(pkpdmodel)=c(id="ID", Aa="Dose","EObs"="Effect","CObs"="Conc")

#
# Error model shape
#
residualEffect(pkpdmodel,"C")=c(errorType=Multiplicative,SD="01")
residualEffect(pkpdmodel,"E")=c(errorType=Multiplicative,SD="01")

#initRandomEffects(pkpdmodel) = c(Diagonal,FALSE,"nEC50,nEmax,nV,nCl,nKe0","0.1,0.1,0.1,0.1,01")
initRandomEffects(pkpdmodel) = c(Diagonal,FALSE,"nEC50,nEmax,nV","0.1,0.01,0.1",
                                 Block, FALSE,"nCl,nKe0","0.1,0,0.1")

initFixedEffects(pkpdmodel)=c(tvKe0=10,tvE0=50,tvEmax=100)



#
# Where to execute fitting
#
host = NlmeParallelHost(sharedDirectory=Sys.getenv("NLME_ROOT_DIRECTORY"),
                        installationDirectory = Sys.getenv("INSTALLDIR"),
                        parallelMethod=NlmeParallelMethod("LOCAL_MPI"),
                        hostName="MPI",
                        numCores=4)
#
# estimate initial values for model variables
#
estimatesUI(pkpdmodel,unique(input$ID), host)
#
# Accept model initial values
#
effects=getInitialEstimates()
initFixedEffects(model) = effects
#
# NLME engine to use
#
engineParams = NlmeEngineExtraParams(PARAMS_METHOD=METHOD_FOCE_ELS,
                                     PARAMS_NUM_ITERATIONS=1000,
                                     PARAMS_SAND="TRUE")
#
# fit the model
#
job=fitmodel(host,engineParams,pkpdmodel)

#
# Look at the results
#

runStatus = read.csv(paste0(pkpdmodel@modelInfo@workingDir,"/Overall.csv"))
print(runStatus)


library(xpose)
library(Xpose.Nlme)

xp=xposeNlme(dir=pkpdmodel@modelInfo@workingDir,modelName="Initial Model")
list_vars(xp)

doxpose(xp)

dv_vs_pred(xp)
#dv_vs_ipred(xp)
#res_vs_pred(xp,res="IWRES")
ind_plots(xp, res="IWRES")
#res_vs_idv(xp,res="WRES")
eta_distrib(xp)
#res_distrib(xp,res="IWRES")
#prm_distrib(xp, type = 'h')
#res_distrib(xp, type = 'hr', res = c('IWRES', 'WRES'))
eta_qq(xp)
#res_qq(xp, res = c('IWRES', 'WRES'))

#
# Accept all estimates
#
pkpdmodel=acceptAllEffects(pkpdmodel)



#
# Load libraries and setup paths and hosts
#
library(ggplot2)
library(xpose)
library(Certara.NLME8)
library(RsNlme)
library(shiny)

#
# goto your working directory where you copied the demo files
#
setwd("c:/Work/RsNlmeWorkDirectory")

#
# setup environment
#
Sys.setenv("NLME_ROOT_DIRECTORY"="c:/Work/RsNlmeWorkDirectory")
Sys.setenv("INSTALLDIR"="C:/Program Files/R/R-3.5.1/library/Certara.NLME8/InstallDirNLME")
# License
#Sys.setenv("PhoenixLicenseFile"="C:\\Program Files (x86)\\Pharsight\\Phoenix\\application\\Services\\Licensing\\lservrc")
#Sys.setenv("NLME_HASH"="xxxx")


#
# Look at the data
#
input=read.csv("PKPD_Data.csv")
#View(input)
#
# Graph time vs. Conc
#
df=data.frame(time=input$Time,conc=input$Conc,subject=input$ID)
ggplot(data=df,aes(x=time,y=conc))+scale_y_log10()+geom_point(colour=df$subject)+
  geom_line(aes(x=df$time,y=df$conc,group=df$subject,colour=df$subject))


input=read.csv("PKPD_Data.csv")

df=data.frame(time=input$Time,conc=input$Conc,subject=input$ID)
ggplot(data=df,aes(x=time,y=conc))+scale_y_log10()+geom_point(colour=df$subject)+
  geom_line(aes(x=df$time,y=df$conc,group=df$subject,colour=df$subject))

pkpdmodel = pkemaxmodel(numComp=1,
                     absorption = Extravascular,
                     isSequential = FALSE  ,
                     hasEliminationComp = FALSE,
                     checkBaseline = TRUE,
                     hasEffectsCompartment = TRUE,
                     modelName="PkPdModel")

dataset=defaultDataset(pkpdmodel,input)
initColMapping(pkpdmodel)= input

print(modelColumnMapping(pkpdmodel))
print(columnNames(dataset))
print(modelVariableNames(pkpdmodel))

modelColumnMapping(pkpdmodel)=c(id="ID", Aa="Dose","EObs"="Effect","CObs"="Conc")

residualEffect(pkpdmodel,"C")=c(errorType=Multiplicative,SD="01")
residualEffect(pkpdmodel,"E")=c(errorType=Multiplicative,SD="01")

#initRandomEffects(pkpdmodel) = c(Diagonal,FALSE,"nEC50,nEmax,nV,nCl,nKe0","0.1,0.1,0.1,0.1,01")
initRandomEffects(pkpdmodel) = c(Diagonal,FALSE,"nEC50,nEmax,nV","0.1,0.01,0.1",
                                 Block, FALSE,"nCl,nKe0","0.1,0,0.1")

initFixedEffects(pkpdmodel)=c(tvKe0=10,tvE0=50,tvEmax=100)

pkpdmodel=generatePMLModel(pkpdmodel)
print(pkpdmodel)
writeDefaultFiles(pkpdmodel,dataset)

estimatesUI(pkpdmodel,unique(input$ID), host)

job=fitmodel(host,dataset,engineParams)

model=acceptAllEffects(model)

xp=xposeNlme(dir="./",modelName="Initial Model")
list_vars(xp)

dv_vs_pred(xp)


#--------------------------------------------------------

setwd("D:/SDCard/NlmeInstall_04_30_18/Pml")
input=read.csv("PKPD_Data.csv")


pdmodel = emaxmodel(
                        checkBaseline = TRUE,

                        modelName="PdModel")

dataset=defaultDataset(pkpdmodel,input)
initColMapping(pdmodel)= input

print(modelColumnMapping(pdmodel))
print(columnNames(dataset))
print(modelVariableNames(pdmodel))

modelColumnMapping(pdmodel)=c(id="ID", Aa="Dose","EObs"="Effect","CObs"="Conc")

initRandomEffects(pdmodel)=c(Diagonal,FALSE,"nEC50,nE0,nEmax","0.1,0.1,0.1")

residualEffect(pdmodel,"E")=c(errorType=Multiplicative,SD="01")

pkpdmodel=generatePMLModel(pdmodel)
print(pdmodel)
writeDefaultFiles(pdmodel,dataset)

estimatesUI(pdmodel,unique(input$ID), host)





#--------------------------------------------------------

doseInput=read.csv("PKPD_Data.csv")
initDoseColMapping(pkpdmodel)=doseInput


print(modelDoseMapping(pkpdmodel))
print(doseNames(pkpdmodel))
modelDoseMapping(pkpdmodel)=c(Aa="Dose")

params=read.csv("params.csv")
initParamsMapping(pkpdmodel) = params


#
# Accept initial estimates from previous run
#
initialEstimates(pkpdmodel)=dmp.txt$coefficients$fixed

random=as.data.frame(dmp.txt$coefficients$random[[1]])
initRandParamsMapping(pkpdmodel)=random

updateDataset(dataset)=pkpdmodel

pkpdmodel=generatePMLModel(pkpdmodel)
print(pkpdmodel)

writeDefaultFiles(pkpdmodel,dataset)

job=fitmodel(host,dataset,engineParams)

source("dmp.txt")


#
# Look at the results
#
source("dmp.txt")
View(dmp.txt$posthoc)

library(xpose)
library(Xpose.Nlme)

xp=xposeNlme(dir="./",modelName="Initial Model")
list_vars(xp)

dv_vs_pred(xp)
#dv_vs_ipred(xp)
#res_vs_pred(xp,res="IWRES")
ind_plots(xp, res="IWRES")
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
# Boot this model
#

bootModel = copyModel(model,modelName="BootModel")
writeDefaultFiles(bootModel,dataset)

boot=NlmeBootstrapParams(numReplicates = 10)

#setwd("E:/Work/NlmeInstall_04_30_18/Pml/InitialModel")

bjob=RunBootstrap(hostPlatform = torqueHost,
                  dataset=dataset,
                  params = engineParams,
                  bootParams = boot,
                  runInBackground = TRUE )

#
# Run in background, gets control back to R session.  check job progress and return results
#
stat=NlmeJobStatus(bjob)

while ( stat != "Finished" ) {
  print(bjob)
  Sys.sleep(3)

  stat=NlmeJobStatus(bjob)
}
retrieveJobResults(bjob)
cleanupRemoteDirectory(bjob)

#
# Covariate Search
#

#
# Copy the covarModel
#
covarModel = copyModel(model,modelName="CovarModel")

# Some covariates
#
sex=categoricalCovariate("sex",c(1,2),c("female","male"))

weight=NlmeCovariateParameter("weight",
                              centerValue="70",
                              continiousType=CovarNumber,
                              direction=Forward)

age=NlmeCovariateParameter("age")

covarModel=addCovariates(covarModel,
                         c(sex,weight,age),
                         c("V"="weight,age",
                           "Cl"="sex,weight"))

covarModel=initColMapping(covarModel,dataset)
print(RsNlme::modelColumnMapping(covarModel))
print(columnNames(dataset))
print(modelVariableNames(covarModel))

modelColumnMapping(covarModel)=c(id="Subject", CObs="Conc",A1="Amount","sex"="Gender_Code")
print(covarModel)

#
# Change error model to multiplicative
#
residualEffect(covarModel,"C")=c(errorType=Multiplicative,SD="0.16")
#?residualEffect
#?NlmeResidualEffect

covarModel=generatePMLModel(covarModel)
print(covarModel)


writeDefaultFiles(covarModel,dataset)
# Run a stepwise covariate search
#
sp = NlmeStepwiseParams(0.01, 0.001, "-2LL")

job=stepwiseSearch(host,
                   dataset,
                   engineParams,
                   covariateModel(covarModel),
                   sp)
print(job)

overall=read.csv("Overall.csv")
View(overall)

stepwiseLines=readLines("Stepwise.txt")
View(stepwiseLines)



covarModel=resetCovariateEffects(covarModel)
covariateEffect(covarModel,"sex","Cl")=COVAR_EFF_YES
covariateEffect(covarModel,"weight","V")=COVAR_EFF_YES
covariateEffect(covarModel,"age","V")=COVAR_EFF_YES


covarModel=generatePMLModel(covarModel)
print(covarModel)

writeDefaultFiles(covarModel,dataset)

job=fitmodel(host,dataset,engineParams)


xp=xposeNlme(dir="./",modelName="Covariate Model")
list_vars(xp)

#dv_vs_pred(xp)

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
writeDefaultFiles(bootModel,dataset)

#
# 5 boot replicates with seed = 1234
#
boot = NlmeBootstrapParams(numReplicates=5,
                           randomNumSeed=1234,
                           stratifyColumns="Weight")
#
# Run a bootstrap job
#

job=bootstrap(host,dataset,engineParams,boot)

print(job)

out=read.csv("out.csv")
View(out)

overall=read.csv("BootOverall.csv")
View(overall)

theta=read.csv("BootTheta.csv")
View(theta)

varCovar=read.csv("BootVarCoVar.csv")
View(varCovar)

omega=read.csv("BootOmega.csv")
View(omega)



#----------------------------
# VPC
#
vpcModel= copyModel(covarModel,modelName="VpcModel")

observationNames(vpcModel)

obsVars = GetObservationVariables(dataset)

observationParameters(obsVars[[1]])=c(xaxis=VPC_XAXIS_T,
                                      binningMethod=VPC_BIN_KMEANS,
                                      quantilesValues ="5,50,95",
                                      quantilesSecondaryValues="4,50,96")

simTable1 = NlmeVpcSimTableDef("Simtable01.csv","0,0.5,1,2,4,8,24","C,CObs,sex,age",FALSE)

vpc = NlmeVpcSimParams(numReplicates=300,
                       seed=1957,
                       stratifyColumns="sex",
                       observationVars=obsVars,
                       simulationTables=c(simTable1))

print(vpcModel)
writeDefaultFiles(vpcModel,dataset,vpc)

job=RunVpcSimulation(hostmpi,dataset,engineParams,vpc)

dir(pattern="pred*")
st2=read.csv("simTable01.csv")
View(st2)




setwd("E:/Work/NlmeInstall_04_30_18/Pml/PhoenixVpcFiles")

eParam = NlmeEngineExtraParams(existingParamsFile = "predckargs.txt")
job=RunVpcSimulation(hostmpi,dataset,eParam)






#pkpd=copyModel(covarModel)

#pkpd=addEmaxModel(pkpd,NlmeEmaxParameters(checkBaseline = TRUE,checkSigmoid = TRUE),isPkFrozen=TRUE)

#print(pkpd)

#writeDefaultFiles(pkpd,dataset)

#job=fitmodel(host,dataset,engineParams)







createInitialParamsMapping <-function(model,paramsInput){
  map=list()
  ids=c("param","init","high","low")
  pn = fixedParameterNames(model)
  names=ids
  for ( p in pn )
    names=c(names,p)
  colNames=colnames(paramsInput)
  for ( c in paramsInput$Parameter )
    colNames=c(colNames,c)

  for ( n in names ) {
    # check for exact match
    for ( c in colNames ) {
      if ( toupper(c) == toupper(n) ) {
        m = NlmeColumnMap(n,c)
        map[[n]] = m
      }
    }
  }
  for ( n in names ) {
    # check for partial match
    if ( length( map[[n]]) == 0 ) {
      for ( c in colNames ) {
        if ( length(grep(toupper(n),toupper(c))) != 0 ) {
          m = NlmeColumnMap(n,c)
          map[[n]] = m
        }
      }
    }
  }
  for ( n in names ) {
    #  Mark as unassigned
    if ( length( map[[n]]) == 0 ) {
      if ( n == "param" )
        m = NlmeColumnMap(n,"Parameter")
      else if ( n == "init" )
        m = NlmeColumnMap(n,"Initial")
      else if ( n == "low" )
        m = NlmeColumnMap(n,"Lower")
      else if ( n == "high" )
        m = NlmeColumnMap(n,"Upper")
      else
        m = NlmeColumnMap(n,"?")
      map[[n]] = m
    }
  }
  return(map)
}



createInitialParamsMapping <-function(model,paramsInput){
  map=list()
  ids=c("param","init","high","low")
  pn = fixedParameterNames(model)
  names=ids
  for ( p in pn )
    names=c(names,p)
  colNames=colnames(paramsInput)
  for ( c in paramsInput$Parameter )
    colNames=c(colNames,c)

  for ( n in names ) {
    # check for exact match
    for ( c in colNames ) {
      if ( toupper(c) == toupper(n) ) {
        m = NlmeColumnMap(n,c)
        map[[n]] = m
      }
    }
  }
  for ( n in names ) {
    # check for partial match
    if ( length( map[[n]]) == 0 ) {
      for ( c in colNames ) {
        if ( length(grep(toupper(n),toupper(c))) != 0 ) {
          m = NlmeColumnMap(n,c)
          map[[n]] = m
        }
      }
    }
  }
  for ( n in names ) {
    #  Mark as unassigned
    if ( length( map[[n]]) == 0 ) {
      if ( n == "param" )
        m = NlmeColumnMap(n,"Parameter")
      else if ( n == "init" )
        m = NlmeColumnMap(n,"Initial")
      else if ( n == "low" )
        m = NlmeColumnMap(n,"Lower")
      else if ( n == "high" )
        m = NlmeColumnMap(n,"Upper")
      else
        m = NlmeColumnMap(n,"?")
      map[[n]] = m
    }
  }
  return(map)
}



writeRandParamsMaping <-function(model,dataset){

  colMap = model@randParamsMapping@mapping
  data = model@randParamData

  subjectId=model@randParamsMapping@mapping$id@columnName

  lines=c()
  line = paste0("id(\"",subjectId,"\")")
  lines=c(lines,line)

  for ( c in colMap ) {
    browser()
    varName=attr(c,"variableName")
    colName=attr(c,"columnName")
    if ( colName != "?" && colName != "" )  {
      line=paste0("covr(",varName,"<-\"",colName,"\")")
      lines=c(lines,line)
    }
  }

  filename=dataset@ranEffectDefFile
  dataFilename=dataset@ranEffectDataFile
  appendFlag=FALSE
  for (  l in  lines ) {
    cat(l,file=filename , sep ="\n" ,append=appendFlag)
    appendFlag=TRUE
  }

  header=paste0("##",paste0(cols, collapse=","))

  cat(header,file=dataFilename,sep="\n",append=FALSE)
  write.table(data,dataFilename,row.names=FALSE,col.names=FALSE,sep=",",
              quote=FALSE,append=TRUE)

}

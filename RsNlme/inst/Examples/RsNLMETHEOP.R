
#
# Load libraries and setup paths and hosts
#
library(ggplot2)
library(xpose)
library(Certara.NLME8)
library(RsNlme)
library(shiny)
library(xpose)
library(Xpose.Nlme)

# setup environment
#
Sys.setenv("INSTALLDIR"="C:/Program Files/R/R-3.6.0/library/Certara.NLME8/InstallDirNLME")
Sys.setenv("PhoenixLicenseFile"="C:/Users/smouksas/Desktop/ChrisTSperrigo/CUcourse/RsNLME/lservrc")
Sys.setenv("NLME_ROOT_DIRECTORY"="C:/Users/smouksas/Desktop/ChrisTSperrigo/CUcourse/RsNLME/NLMETEMPRUNDIR")
#
# Look at the input data
#
nlmedatainput <-read.csv("THEOPP.csv")
#
# Graph time vs. Conc
#
nlmedatainput$DV <- as.numeric(as.character(nlmedatainput$DV))
nlmedatainput$TIME <- as.numeric(as.character(nlmedatainput$TIME))


ggplot(data= nlmedatainput, aes(x=TIME    ,y=DV   )) +
       geom_point(aes(colour=DV   ))+
  facet_grid(~SEX)+
       geom_line(aes(x=TIME,y=DV,group=ID,colour=DV))+
                   scale_y_log10()

#
# Format the input file and add a gender_code for the model
#
nlmedatainput$Gender_Code =as.numeric(nlmedatainput$SEX)

# 2 Compartment population PK model with intravaneous observation
# Help is avaialble by typing ?pkmodel
#
theopmodel <-  pkmodel(numComp=1,
                isPopulation=TRUE,
                isTlag = FALSE,
                absorption = Extravascular,
                modelName="theopbase")
#
# Looks at model variables and map them to data columns
#
datasettheop=NlmeDataset()#list of files used for the engine running
initColMapping(theopmodel)=nlmedatainput

modelColumnMapping(theopmodel)# not all were guessed we need to specify the mappings
print(modelVariableNames(theopmodel))

modelColumnMapping(theopmodel)=c( CObs="DV",Aa="AMT")
modelColumnMapping(theopmodel)#
#
# Change error model

residualEffect(theopmodel,"C") <- c(errorType=Multiplicative,SD="0.16")

#
# Set initial estimates on model variables
#
#
# Write out default model and mapping files
#
writeDefaultFiles(theopmodel,datasettheop)
#
# Where to execute fitting
#
host = NlmeParallelHost(sharedDirectory=Sys.getenv("NLME_ROOT_DIRECTORY"),
                        installationDirectory = Sys.getenv("INSTALLDIR"),
                        parallelMethod=NlmeParallelMethod("LOCAL_MPI"),
                        hostName="MPI",
                        numCores=4)
#
# Evaluate the model and pick reasonable initial values for fixed effects
#
estimatesUI(theopmodel,unique(nlmedatainput$ID),host)
# Accept initial estimates picked from shiny app
effects=getInitialEstimates()
initFixedEffects(theopmodel) = effects
print(theopmodel)
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
theopfit <- fitmodel(host,datasettheop,engineParams, model = theopmodel)


#
# Look at the results
#


xp=xposeNlme(dir="./",modelName="theopbase")
list_vars(xp)
doxpose(xp)
xp<- set_var_types(xp,  contcov = 'WT')
xp<- set_var_types(xp,  catcov = 'SEX')


dv_vs_pred(xp)
dv_vs_ipred(xp)
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
res_distrib(xp, type = 'hr', res = c('IWRES', 'WRES'))
#cov_qq(xp,log="xy")
#cov_qq(xp)
eta_qq(xp)
res_qq(xp, res = c('IWRES', 'WRES'))

theopmodel= acceptAllEffects(theopmodel)

#
# Make a copy of the model
#
theopcovarmodel = copyModel(theopmodel,modelName="theopcovarmodel")

#
# Examine Some covariates
#
sex <-  categoricalCovariate("SEX",c(0,1),c("female","male"))

weight <-  NlmeCovariateParameter("WT",
                              centerValue="70",
                              continuousType  =CovarNumber,
                              direction=Backward)

theopcovarmodel=addCovariates(theopcovarmodel,
                         c(sex,weight),
                         c("V"="sex,weight",
                           "Cl"="sex,weight"))

initRandomEffects(theopcovarmodel) = c(Diagonal, TRUE, "nKa"    ,"0.1",
                                       Block   , FALSE, "nV,nCl","0.2, 0.01, 0.2")

initRandomEffects(theopcovarmodel)=c(Block,
                           FALSE,
                           "nKa,nV,nCl",
                           "0.2,
                           0.01, 0.2,
                           0.01, 0.01, 0.2")


# Map newly created model variables
initColMapping(theopcovarmodel)=nlmedatainput
modelColumnMapping(theopcovarmodel)#  need to set the mappings for amt and dv
print(modelVariableNames(theopcovarmodel))
modelColumnMapping(theopcovarmodel)=c( CObs="DV",Aa="AMT")
modelColumnMapping(theopcovarmodel)#

#
# Run a stepwise covariate search
#
sptheop = NlmeStepwiseParams(0.01, 0.001, "-2LL")

#
# remote execution on the grid
#
jobtheopcov=stepwiseSearch(host,
                      datasettheop,
                      engineParams,
                      covariateModel(theopcovarmodel),
                      sptheop,
                      runInBackground = FALSE )
print(jobtheopcov)

#
# view results from covariate search
#
overall=read.csv("Overall.csv")
View(overall)

stepwiseLines=readLines("Stepwise.txt")
View(stepwiseLines)



#----------------------------
# VPC
#
vpctheop= copyModel(theopmodel,modelName="vpctheop")
observationNames(vpctheop)
obsVars = GetObservationVariables(datasettheop)

observationParameters(obsVars[[1]])=c(xaxis=VPC_XAXIS_T,
                                      binningMethod=VPC_BIN_KMEANS,
                                      quantilesValues ="10,50,90",
                                      quantilesSecondaryValues="2.5,50,97.5")
vpc = NlmeVpcParams(numReplicates=1000,
                       seed=444,
                       stratifyColumns="SEX",
                       observationVars=obsVars)

print(vpctheop)
writeDefaultFiles(vpctheop,datasettheop,vpc)

jobvpc <- vpcmodel(host,dataset,vpc,runInBackground = FALSE)

library(vpc)
simData=getSimData(nlmedatainput,stratifyColumns = "SEX",simFile="out.txt")
obsData=getObsData(nlmedatainput)

vpc1 <- vpc(sim = simData, obs = obsData, vpcdb = TRUE)

plot_vpc(
  vpc1,
  show = list(obs_dv = TRUE, obs_ci = FALSE),
  xlab = "Time (hours)", ylab = "Concentration", title="VPC!")


vpc::vpc(simData,obsData,
         bins = c(0, 2, 4, 6, 8, 10, 16, 25),
         stratify=c(mapCovariate(input,"sex")),
         facet="rows",
         pi = c(0.10,0.90),
         ci = c(0.20,0.80))

vpc::vpc(sim=simData,obs=obsData,stratify=c(mapCovariate(input,"sex")),show=c(pi_ci=TRUE,pi_as_area=TRUE,obs_dv=TRUE))+theme_classic()+
  scale_y_log10()
st2=read.csv("simTable01.csv")
View(st2)


#devtools::install_github("olivierbarriere/vpcstats")
library(vpc)
library(magrittr)
library(rlang)
library(dplyr)
library(ggplot2)
library(data.table)
library(vpcstats)

exampleobs <- obsData
examplesim <- simData

exampleobs$ID
examplesim <- examplesim %>% 
  arrange(REPLICATE,ID)

exampleobs$REPLICATE <- 0
VPCDATA<- vpcstats(
  obsdata = data.frame(exampleobs),
  simdata = data.frame(examplesim),
  NBINS = NULL,REPL = REPLICATE,stratify = ~Gender_Code)



ggplot(VPCDATA$PI) +
  facet_grid( ~ Gender_Code) +
  geom_ribbon(
    aes(
      XMED,
      ymin = (`SIM2.5%CI`),
      ymax = (`SIM97.5%CI`),
      fill = QNAME,
      col = QNAME,
      group = QNAME
    ),
    alpha = 0.1,
    col = NA
  ) +
  geom_line(aes(
    XMED,
    y = `SIM50%CI`,
    col = QNAME,
    group = QNAME
  )) +
  geom_line(aes(
    x = XMED,
    y = RAWOBS,
    group = QNAME,
    linetype = QNAME
  ), size = 1) +
    scale_colour_manual(
    name = "Simulated Percentiles\nMedian (lines) 95% CI (areas)",
    breaks = c("5%PI", "50%PI", "95%PI", "Percent BLQ"),
    values = c("red", "blue", "red", "black"),
    labels = c("5%", "50%", "95%", "Percent BLQ")
  ) +
  scale_fill_manual(
    name = "Simulated Percentiles\nMedian (lines) 95% CI (areas)",
    breaks = c("5%PI", "50%PI", "95%PI", "Percent BLQ"),
    values = c("red", "blue", "red", "black"),
    labels = c("5%", "50%", "95%", "Percent BLQ")
  ) +
  scale_linetype_manual(
    name = "Observed Percentiles\n(black lines)",
    breaks = c("5%PI", "50%PI", "95%PI"),
    values = c("dotted", "solid", "dashed"),
    labels = c("5%", "50%", "95%")
  ) +
  guides(
    fill = guide_legend(order = 2),
    colour = guide_legend(order = 2),
    linetype = guide_legend(order = 1)
  ) +
  theme(
    legend.position = "top",
    legend.key.width = grid::unit(2, "cm"),
    axis.text.x = element_text(angle = 30),
    axis.title.x = element_blank()
  )+
  scale_y_log10()




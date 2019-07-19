##############################################################################################################
##                              Description
##
## The purpose of this example is to demonstrate how to 
##
##    - Construct the base model (described by a two-compartment model with IV bolus)
## 
##        * Load the input dataset and explore the data
##        * Define the base model 
##        * Map model variables to input dataset columns
##        * Using the provided initial estimates shiny app, estimatesUI, to visually determine 
##          a set of reasonable initial values for fixed effects
##        * Fit the base model with initial estimates picked from shiny app
##
##    - Identify covariates through a stepwise covariate search. 
##
##    - Bootstrapping analysis for the model selected by the covariate search procedure.
##
##
##############################################################################################################

# Setup environment variables and loading necessary packages 
source("c:/Work/NlmeInstall_07_10_19/Examples/SetUpEnv_LoadRPackages.R")
setwd("c:/Work/NlmeInstall_07_10_19/Examples/")


# ==========================================================================================================
# Load the input dataset and explore the data 
# ==========================================================================================================
#
# load and view the input dataset
#
dt_InputDataSet = fread("16subjects.csv")
View(dt_InputDataSet)

#
# Format the dt_InputDataSet file and add a gender_code for the model
#
colnames(dt_InputDataSet)
dt_InputDataSet$Gender_Code = as.numeric(as.factor(dt_InputDataSet$Gender))

#
# Graph time vs. Conc
#
dt_Conc = data.table(Time = dt_InputDataSet$Act_Time, conc = dt_InputDataSet$Conc, Subject = dt_InputDataSet$Subject)
dt_Conc$Subject = as.factor(dt_Conc$Subject)
plot_Conc = ggplot(dt_Conc, aes(x = Time, y = conc, group = Subject, color = Subject)) + 
  scale_y_log10() +
  geom_line() + 
  geom_point() +
  ylab("Drug Concentration \n at the central compartment")

ggsave(filename = "Plot_16Subjects_ConcTime.pdf", plot_Conc)

      
#
# Statistics on possible covariates
#
mean(dt_InputDataSet$Age)
mean(dt_InputDataSet$BodyWeight)


# ==========================================================================================================
#                           Define the PK/PD model through RsNlme
# ==========================================================================================================
#
# 2 Compartment population PK model with intravaneous observation
# Help is avaialble by typing ?pkmodel
#
ModelName = "TwCpt_IVBolus_FOCE-ELS"
model = pkmodel(numComp = 2, modelName = ModelName)

#
# Change error model
#
residualEffect(model,"C") = c(errorType = Multiplicative, SD = "0.16")


# ==========================================================================================================
#                     Map model variables to input dataset columns
# ==========================================================================================================
# initialize model mapping and automatically mapping some of the model variables to the data columns 
initColMapping(model) = dt_InputDataSet

# manually set up the mapping for the rest of variables
modelColumnMapping(model) = c(id = "Subject", CObs = "Conc", A1 = "Amount")


# ==========================================================================================================
# Using the provided initial estimates shiny app, estimatesUI, to visually determine 
# a set of reasonable initial values for fixed effects
# ==========================================================================================================


# envoke the initial estimates GUI
#
# Evaluate the model and pick reasonable initial values for fixed effects
#
estimatesUI(model, unique(dt_InputDataSet$Subject), host)

# Accept initial estimates picked from shiny app
initFixedEffects(model) = getInitialEstimates()
print(model)


#
# If you need to edit any of the generated files
#
# model2=RsNlme::editModel(model,dataset)
#
#

# ==========================================================================================================
#           Run the base model with the initial estimates picked from the shiny app
# ==========================================================================================================
#
# NLME engine to use
#
engineParams = NlmeEngineExtraParams(PARAMS_METHOD=METHOD_FOCE_ELS,
                                     PARAMS_NUM_ITERATIONS=1000,
                                     PARAMS_SAND="TRUE")

#
# Do the model fitting
#
job = fitmodel(host, engineParams, model)


# ==========================================================================================================
#                      Diagnostic plots
# ==========================================================================================================
# imports results of an NLME run into xpose database to create commonly used diagnostic plots
xp=xposeNlme(dir=model@modelInfo@workingDir,modelName = ModelName)

# list all available variables in an xpdb object
list_vars(xp)


# ==========================================================================================================
#            Observations against population or individual predications 
# ==========================================================================================================
dv_vs_pred(xp)
dv_vs_ipred(xp)

# ==========================================================================================================
#           Residuals against population predications or independent variable   
# ==========================================================================================================
plot_CWRES_PRED = res_vs_pred(xp,res="CWRES",type="ps")
ggsave(filename = "TwoCptIVBolus_CWRES_PRED.pdf", plot_CWRES_PRED)

plot_CWRES_IDV = res_vs_idv(xp,res="CWRES",type="ps")
ggsave(filename = "TwoCptIVBolus_CWRES_idv.pdf", plot_CWRES_IDV)

# ==========================================================================================================
# Observations, individual predictions and population predictions plotted against 
# the independent variable for every individual   
# ==========================================================================================================
ind_plots(xp)



# ==========================================================================================================
#          Distribution plots of ETA  
# ==========================================================================================================
eta_distrib(xp)


##############################################################################################################

###################                      Covariate search                            ########################

##############################################################################################################
#
# Accept fixed effects estimates from fitting run
#
covarModel = acceptAllEffects(model)

# Copy the model into a new object, and then create a new working directory and copied all the files to it
covarModel = copyModel(covarModel, modelName = paste0(ModelName, "_CovariateSearch"))

#=====================================================================================================
# Examine Some covariates
#=====================================================================================================
# define covariates 
sex = categoricalCovariate("sex",c(1,2), c("female","male"))

weight = NlmeCovariateParameter("weight",
                              centerValue = "70",
                              continuousType = CovarNumber,
                              direction = Forward)

age = NlmeCovariateParameter("age")

# automatically incorporates the covariates into the base model 
covarModel = addCovariates(covarModel,
                         c(sex,weight,age),
                         c("V" = "weight,age",
                           "Cl" = "sex,weight"))

#=====================================================================================================
# Mapping model variables to input dataset columns
#=====================================================================================================
# Map newly created model variables
initColMapping(covarModel) = dt_InputDataSet
#print(RsNlme::modelColumnMapping(covarModel))
#print(columnNames(dataset))
#print(modelVariableNames(covarModel))

modelColumnMapping(covarModel) = c(id = "Subject", CObs = "Conc",A1 = "Amount","sex" = "Gender_Code")


#=====================================================================================================
# Run the stepwise covariate search
#=====================================================================================================

# set up for the stepwise covariate search
stepwiseSearchSetup = NlmeStepwiseParams(0.01, 0.001, "-2LL")

# run the stepwise covariate search
job = stepwiseSearch(host,
                     engineParams,
                     covariateModel(covarModel),
                     stepwiseSearchSetup,
                     covarModel)
print(job)


##=====================================================================================================
## Load and view results from covariate search
##=====================================================================================================
# load and view the reports for model fit diagnostic 
overall = fread(paste0(model@modelInfo@workingDir,"/Overall.csv"))
View(overall)

# load and view the model selected by the stepwise covariate search
stepwiseLines = readLines(paste0(model@modelInfo@workingDir,"/Stepwise.txt"))
View(stepwiseLines)



##############################################################################################################

###################                      Bootstrapping                           ########################

##############################################################################################################


##=====================================================================================================
# Reset the covariates to list suggested by covariate search
##=====================================================================================================
# return a new model with all covariate effects cleared
selectedCovarModel = resetCovariateEffects(covarModel)

## enable the covariates selected by the covariate search
covariateEffect(selectedCovarModel, "sex", "Cl") =  EnableEffect
covariateEffect(selectedCovarModel, "age", "V") =  EnableEffect

# update the PML statements
selectedCovarModel = generatePMLModel(selectedCovarModel)



##=====================================================================================================
## Run the bootstrap
##=====================================================================================================
## Copy the model into a new object, and then create a new working directory and copied all the files to it
bootModel = copyModel(selectedCovarModel, modelName = paste0(ModelName, "_SelectedCovarModel_Bootrapping"))

# bootstrap setup
bootSetup = NlmeBootstrapParams(numReplicates = 10,
                           randomNumSeed = 1234,
                           stratifyColumns = "sex")


# run the boostrap for the model selected by the covariate search 
job = bootstrap(host, engineParams, bootSetup, bootModel)

print(job)


##=====================================================================================================
## Load and view results
##=====================================================================================================
# load and view the estimation results for all boostrap runs
dt_out = fread(paste0(model@modelInfo@workingDir,"/out.csv"))

# load and view the return code and LL for all the bootstrap runs
dt_overall = fread(paste0(model@modelInfo@workingDir,"/BootOverall.csv"))

# load and view reports for fixed effects over all bootstrap runs
dt_theta = fread(paste0(model@modelInfo@workingDir,"/BootTheta.csv"))

# load and view reports for Omega over all bootstrap runs
dt_omega = fread(paste0(model@modelInfo@workingDir,"/BootOmega.csv"))





##############################################################################################################
##                              Description
##
## The purpose of this example is to demonstrate how to 
##
##    - create a model involving BQL and covariates through RsNLME
##    - fit the model 
##    - use the "xpose.Nlme" package to create commonly used diagonistic plots
##    - use the "vpc" package to create the VPC plot 
##
## The model demonstrated is a one-compartment model with IV bolus, where both V and Cl depend on some 
## continuous covariates as shown below
##
##    - V = tvV * (BW/30)^dVdBW * exp(nV)
##    - Cl = tvCl * (BW/30)^dCldBW * (PMA^Gam/(PMA^Gam + PMA50^Gam)) * exp(nCl)
##
## where BW and PMA are covariates. 
##
##############################################################################################################

# Setup environment variables and loading necessary packages 
source("c:/Work/NlmeInstall_07_10_19/Examples/SetUpEnv_LoadRPackages.R")
setwd("c:/Work/NlmeInstall_07_10_19/Examples/")

##############################################################################################################

###############      Load the input dataset, and create the model and column mapping files      ###############

#   - Load the input dataset
#   - Define the model through RsNlme
#   - Mapping the data to the model 
##############################################################################################################


# ==========================================================================================================
# Load the input dataset
# ==========================================================================================================
InputDataSetName = "OneCpt_IVBolus_ContCovariatesOnClV_BQL"
dt_InputDataSet = fread(paste0(InputDataSetName, ".csv"))

# ==========================================================================================================
#                           Define the PK/PD model through RsNlme
# ==========================================================================================================
# model name 
ModelName = paste0(InputDataSetName, "_Laplacian")

# define the basic PK model (a one-compartment model with IV bolus)
model = pkmodel(numCompartments = 1
                , modelName = ModelName
                )

# --------------------------------------------------------------------------------------------------------
# Incorporate the covariates into the basic model 
# --------------------------------------------------------------------------------------------------------
# define covariate BW
BW = NlmeCovariateParameter(name = "BW"
                            , type = Continuous
                            , continuousType = CovarNumber
                            , centerValue = "30"
                            )

# define covariate PMA
PMA = NlmeCovariateParameter(name = "PMA")

# automatically incorporate covariate BW into the basic model 
model = addCovariates(model
                      , covariates = c(BW, PMA)
                      , effects = c("V" = "BW", "Cl" = "BW")
                      )

# Maually incorporate covariate PMA into the basic model (as the formula is not standard)
# This is done through function "structuralParam" by maunally redefining the structural parameter "Cl" 
# and then using its "extraCode" option to incorporating the newly introduced fixed effects.
structuralParam(model, "Cl") = c(style = Custom
                                 , code = "stparm(Cl = tvCl * (BW/30)^dCldBW * (PMA^Gam/(PMA^Gam + PMA50^Gam)) * exp(nCl))"
                                 , extraCode = c("fixef(PMA50 = c(, 5, ))"
                                                 , "fixef(Gam = c(, 1, ))"
                                                 )
                                 )

# --------------------------------------------------------------------------------------------------------
# Reset initial values for Theta and Omega
# --------------------------------------------------------------------------------------------------------
# reset initial values for fixed effects 
# (default value for the general fixed effect is 1, and the default value for the one related to covariates is 0)
initFixedEffects(model) = c(tvV = 20, tvCl = 20
                            , dVdBW = 1, dCldBW = 1
                            )  

# reset initial values for random effects (the default value is 1)
initRandomEffects(model) = c(Diagonal
                             , FALSE
                             , "nV, nCl"
                             , "0.1, 0.2"
                             )

# --------------------------------------------------------------------------------------------------------
# Reset the residual error model 
# --------------------------------------------------------------------------------------------------------
residualEffect(model, "C") = c(errorType = Multiplicative
                             , SD = "0.1"
                             , isBQL = TRUE
                             )

# ==========================================================================================================
#                     Mapping the input dataset to the created model 
# ==========================================================================================================
# initialize model mapping and automatically mapping some of the model variables to the data columns 
initColMapping(model) = dt_InputDataSet

# output the mapping 
modelColumnMapping(model) 

# manually set up the mapping for the rest of variables
modelColumnMapping(model) = c(A1 = "Dose")
                              
##############################################################################################################

###################                    Model Fitting                                      ################### 

##############################################################################################################



# host setup: run locally with MPI enabled
host = NlmeParallelHost(sharedDirectory = Sys.getenv("NLME_ROOT_DIRECTORY")
                        , parallelMethod = NlmeParallelMethod("LOCAL_MPI")
                        , hostName = "MPI"
                        , numCores = 4
                        )

# engine setup 
engineParams = NlmeEngineExtraParams(PARAMS_METHOD = METHOD_LAPLACIAN
                                     , PARAMS_NUM_ITERATIONS = 1000
                                     , PARAMS_SAND="TRUE"
                                     )


# run the model 
job = fitmodel(host, engineParams, model)


##############################################################################################################

###################                      Diagnostic plots                            ########################

##############################################################################################################
xp = xposeNlme(dir=model@modelInfo@workingDir, modelName = ModelName)


# ==========================================================================================================
#            Observations against population or individual predications 
# ==========================================================================================================
# observations against population predications 
dv_vs_pred(xp
           , type = "p"
           )

# observations against individual predications 
dv_vs_ipred(xp
            , type = "p"
            )


# ==========================================================================================================
#           Residuals against population predications or independent variable   
# ==========================================================================================================
# CWRES against population predications 
res_vs_pred(xp
            , res = "CWRES"
            , type = "ps"
            )

# CWRES against the independent variable
res_vs_idv(xp
           , res = "CWRES"
           , type = "ps"
           )


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

###################                    VPC                                   ################### 

##############################################################################################################

# Accept all the estimates for fixed effects, random effects and sigma (to be used for VPC simulation)
modelVPC = acceptAllEffects(model)

# Copy the model into a new object, and then create a new working directory and copied all the files to it
modelVPC = copyModel(modelVPC, modelName = paste0(ModelName, "_VPC"))

# ==========================================================================================================
#          - Set up default name for model, input dataset, and mapping files
#          - Set up host platform 
#          - set up simulation parameters (numReplicates, seed, output tables)
# ==========================================================================================================
# Define the file name for VPC simulation results (the default name is out.txt).
vpcOutputFileName = "predout.csv"

# Create the default name for the model, input dataset and mapping files 
modelVPC@dataset@outputFilename= vpcOutputFileName


# Host setup 
host = NlmeParallelHost(sharedDirectory = Sys.getenv("NLME_ROOT_DIRECTORY")
                        , parallelMethod = NlmeParallelMethod("LOCAL_MPI")
                        , hostName = "MPI"
                        , numCores = 1
                        )

# VPC setup
VPCSetup = NlmeVpcParams(numReplicates = 100
                         , seed = 1
                         )

# ==========================================================================================================
#                                   Run the model 
# ==========================================================================================================
job = vpcmodel(host, VPCSetup, model = modelVPC)

# ==========================================================================================================
#                             Using vpc library to do the VPC plots 
# ==========================================================================================================
# load observed data
dt_ObsData = getObsData(dt_InputDataSet,modelDir=modelVPC@modelInfo@workingDir)


# load simulated data 
dt_SimData = getSimData(input = dt_InputDataSet, simFile = vpcOutputFileName,modelDir=modelVPC@modelInfo@workingDir)

#----------------------------------------------------------------------------------------------------------
# VPC plots
#   - Create a VPC plot for un-censored data that shows the censor limit (LLOQ) as a horizontal line 
#   - Create a VPC for the probability of left-censored data 
# Note: need to set the value of lloq to be above the LLOQ so that observation equal to LLOQ 
# will not be treated as actual observations, see http://vpc.ronkeizer.com/censored-data.html for details 
#----------------------------------------------------------------------------------------------------------
lloq_value = 0.01 + 1e-8

# Create a VPC plot for un-censored data that shows the censor limit (LLOQ) as a horizontal line 
plot_VPC = vpc(sim = dt_SimData, obs = dt_ObsData, lloq = lloq_value
               , log_y = TRUE, log_y_min = 1e-9
               , xlab = "Time", ylab = "Drug concentration at the central compartment"
               )

# Create a VPC for the probability of left-censored data 
plot_VPC_cens = vpc_cens(sim = dt_SimData,  obs = dt_ObsData, lloq = lloq_value
                         , xlab = "Time"
                         )

# put these two VPC plots in a page
egg::ggarrange(plot_VPC, plot_VPC_cens)

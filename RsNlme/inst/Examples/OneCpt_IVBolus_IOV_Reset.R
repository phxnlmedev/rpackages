##############################################################################################################
##                              Description
##
## The purpose of this example is to demonstrate how to create a model involving inter-occasion variability, 
## Reset through RsNlme, and then simulate it, where the model is one compartment with an IV bolus.
##
##############################################################################################################



##############################################################################################################

##############        Setup Environment Variables and Load Necessary Packages               ##############

##############################################################################################################

# ==========================================================================================================
# Setup environment
# ==========================================================================================================
source("c:/Work/NlmeInstall_07_10_19/Examples/SetUpEnv_LoadRPackages.R")
setwd("c:/Work/NlmeInstall_07_10_19/Examples/")



##############################################################################################################

###############      Create the Model, Simulation Input Dataset, and Column Mapping files      ###############
#
#   - Create the model through RsNlme
#   - Create the simulation input dataset
#   - Map the input dataset to the model 
##############################################################################################################



# ==========================================================================================================
#                           Create the PK/PD model through RsNlme
# ==========================================================================================================

# Model name
ModelName = "OneCpt_IVBolus_IOV_Reset"

# Basic one-compartment PK model with 1st-order absorption 
model = pkmodel(numCompartments = 1
                , modelName = ModelName
                )

#----------------------------------------------------------------------------------------------------
# Incorporate covariates inside the basic model 
#----------------------------------------------------------------------------------------------------
# Define Occasion covariate, which involves 3 occasions (0, 1, 2)
Occasion = occasionCovariate("Occasion"
                             , occasions = c(0, 1, 2)
                             , occasionNames = c("0", "1", "2")
                             )

# Update the model with covariates built inside
model = addCovariates(model, covariates = c(Occasion), effects = c("V" = "Occasion", "Cl" = "Occasion"))

#----------------------------------------------------------------------------------------------------
# Reset structural model parameters (including initial values for fixed and random effects)
#----------------------------------------------------------------------------------------------------
# Set up initial values for fixed effects  (the default value is 1)
initFixedEffects(model) = c(tvV = 1, tvCl = 0.3) 


# Set up initial values for random effects (the default value is 1) 
initRandomEffects(model) = c(Diagonal
                             , FALSE
                             , "nV, nCl"
                             , "0.02, 0.02"
                             )

# Set up initial values for inter-occasion variability 
initOccasionRandomEffect(model, "Occasion") = c(0.01, 0.01)

#----------------------------------------------------------------------------------------------------
# Reset residual error model
#----------------------------------------------------------------------------------------------------
residualEffect(model,"C") = c(errorType = Multiplicative, SD = "0.1")

# ==========================================================================================================
#                          Create the simulation input dataset 
# ==========================================================================================================

dt_SimInputData = data.table(ID = 1
                             , Time = 0
                             , Dose = 100
                             , Occasion = c("0", "1", "2")  
                             , Reset = c(0, 4, 4)
                             )

# ==========================================================================================================
#                     Map the input dataset to the created model 
# ==========================================================================================================

#----------------------------------------------------------------------------------------------------
# Enable reset 
#----------------------------------------------------------------------------------------------------
model = addReset(model
                 , low = 4
                 , hi = 4
                 )

#----------------------------------------------------------------------------------------------------
# Map input data columns to model variables
#----------------------------------------------------------------------------------------------------
# Initialize model mapping (link the input dataset to model@inputData)
# and automatically map some of the model variables to the data columns 
initColMapping(model) = dt_SimInputData
modelColumnMapping(model) # output the mapping 

# Manually set up the mapping for the rest of variables
modelColumnMapping(model) = c(A1 = "Dose")


##############################################################################################################

###################                    Model Simulation                                    ################### 

##############################################################################################################
# ==========================================================================================================
#          - Set up default name for model, input dataset and mapping file
#          - Set up host platform 
#          - Set up simulation parameters (numReplicates, seed, output tables)
# ==========================================================================================================



# Host setup: run locally
host = NlmeParallelHost(sharedDirectory = Sys.getenv("NLME_ROOT_DIRECTORY")
                        , parallelMethod = NlmeParallelMethod("LOCAL_MPI")
                        , hostName = "MPI"
                        , numCores = 1
                        )


# --------------------------------------------------------------------------
# Simulation setup
# --------------------------------------------------------------------------
# Simulation table for structural model parameters 
SimTableStructuralModelParams = NlmeSimTableDef(name = "SimTableStructuralModelParams.csv"
                                                , timesList = "0"
                                                , variablesList = "Occasion, V, Cl"
                                                , timeAfterDose = FALSE
                                                )

# Simulation table for observations
SimTableObs = NlmeSimTableDef(name = "SimTableObs.csv"
                              , timesList = "0, 0.5, 1, 2, 4, 8, 12, 16, 20, 24"
                              , variablesList = "Occasion, C, CObs"
                              , timeAfterDose = FALSE
                              )

# Simulation setup
SimSetup = NlmeSimulationParams(numReplicates = 50
                                , seed = 1
                                , simulationTables = c(SimTableStructuralModelParams, SimTableObs)
                                )

# ==========================================================================================================
#                                   Run the model 
# ==========================================================================================================
job = simmodel(host, SimSetup, model)








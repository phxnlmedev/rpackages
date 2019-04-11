##############################################################################################################
##                              Description
##
## The purpose of this example is to demonstrate how to create a model involving steady state through RsNlme.
## The model demonstrated is a one-compartment model with first-order absorption having no time lag.
##
##############################################################################################################

##############################################################################################################

##############        Setup Environment Variables and Load Necessary Packages               ##############

##############################################################################################################

# ==========================================================================================================
# Setup environment
# ==========================================================================================================
source("C:/RsNLME/SetUpEnv_LoadRPackages.R")
# ==========================================================================================================
# Load library
# ==========================================================================================================
# Phoenix NLME engine libraries
library(Certara.NLME8)

# Library used to create PK/PD model in R 
library(RsNlme)

# Package used to run the model in the linux grid 
library(ssh)

# Graphic package
library(ggplot2)


# Data processing library 
library(data.table)



##############################################################################################################

###############      Create the Model, Simulation Input Dataset, and Column Mapping files      ###############
#
#   - Create the model through RsNlme
#   - Create the simulation input dataset
#   - Map the input dataset to the model 
#
##############################################################################################################



# ==========================================================================================================
#                           Create the PK/PD model through RsNlme
# ==========================================================================================================

# Model name
ModelName = "OneCpt_1stOrderAbsorpNoTlag_SteadyState"

# Basic one-compartment PK model with 1st-order absorption 
model = pkmodel(numCompartments = 1
                , absorption = Extravascular
                , modelName = ModelName
                )

#----------------------------------------------------------------------------------------------------
# Reset structural model parameters (including initial values for fixed and random effects)
#----------------------------------------------------------------------------------------------------
# Fixed effects (the default value is 1)
initFixedEffects(model) # output initial values for fixed effects 
initFixedEffects(model) = c(tvKa = 0.8, tvV = 5, tvCl = 1) # set up initial values for fixed effects 


# Random effects (the default value is 1) 
initRandomEffects(model) = c(Diagonal
                             , FALSE
                             , "nKa, nV, nCl"
                             , "0.01, 0.01, 0.01"
                             )

#----------------------------------------------------------------------------------------------------
# Reset residual error model
#----------------------------------------------------------------------------------------------------
residualEffect(model,"C") = c(errorType = Multiplicative, SD = "0.1")


# ==========================================================================================================
#                          Create the simulation input dataset 
# ==========================================================================================================

dt_SimInputData = data.table(ID = 1
                             , Time = 0
                             , Dose = 10
                             , SteadyState = 1
                             , II = 12 
                             )

# ==========================================================================================================
#                     Map the input dataset to the created model 
# ==========================================================================================================

#----------------------------------------------------------------------------------------------------
# Add steady state dose 
#----------------------------------------------------------------------------------------------------
mapExtraDose = ExtraDoseItem(type = BolusDose
                             , amountColumn = "Dose"
                             , deltaTimeColumn = "II"
                             )


model = addExtraDose(model
                     , name = "Aa" 
                     , doseType = SteadyStateDose
                     , doses = c(mapExtraDose)
                     )

#----------------------------------------------------------------------------------------------------
# Map the data columns to model variables  
#----------------------------------------------------------------------------------------------------
# Initialize model mapping (link the input dataset to model@inputData)
# and automatically map some of the model variables to the data columns
initColMapping(model) = dt_SimInputData
modelColumnMapping(model) 
# Output the mapping 


# Manually set up the mapping for the rest of variables
modelColumnMapping(model) = c(Aa = "Dose")


##############################################################################################################

###################                    Model Simulation                                    ################### 

##############################################################################################################
# ==========================================================================================================
#          - Set up default name for model, input dataset and mapping file
#          - Set up host platform 
#          - Set up simulation parameters (numReplicates, seed, output tables)
# ==========================================================================================================

# Create the default name for the model, input dataset and mapping files 
NlmeFileNames = NlmeDataset()

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
                                                , variablesList = "Ka,V,Cl"
                                                , timeAfterDose = FALSE
                                                )

# Simulation table for observations
SimTableObs = NlmeSimTableDef(name = "SimTableObs.csv"
                              , timesList = "0, 0.5, 1, 2, 4, 8, 12, 16, 20, 24"
                              , variablesList = "C, CObs"
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
job = simmodel(host, NlmeFileNames, SimSetup, model)








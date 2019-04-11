##############################################################################################################
##                              Description
##
## The purpose of this example is to demonstrate how to create a model involving both plasma and urine data
## with the urine compartment reset to zero right after each observation through RsNlme, and then simulate it.
## The model demonstrated is a two-compartment model with an IV bolus. 
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
##############################################################################################################

# ==========================================================================================================
#                           Create the PK/PD model through RsNlme
# ==========================================================================================================

# Model name
ModelName = "TwoCpt_IVBolus_1stOrderElim_PlasmaUrineObs"

# Basic one-compartment PK model 
model = pkmodel(numCompartments = 2
                , hasEliminationComp = TRUE
                , modelName = ModelName
                )


#----------------------------------------------------------------------------------------------------
# Reset structural model parameters (including initial values for fixed and random effects)
#----------------------------------------------------------------------------------------------------
# Fixed effects (the default value is 1)
initFixedEffects(model) # output initial values for fixed effects 
initFixedEffects(model) = c(tvV = 5, tvCl = 1, tvV2 = 3, tvCl2 = 0.5) # set up initial values for fixed effects 


# Random effects (the default value is 1) 
initRandomEffects(model) = c(Diagonal
                             , FALSE
                             , "nV, nCl, nV2, nCl2"
                             , "0.01, 0.01, 0.01, 0.01"
                             )

#----------------------------------------------------------------------------------------------------
# Reset residual error model
#----------------------------------------------------------------------------------------------------
# For plasma observation 
residualEffect(model,"C") = c(errorType = Multiplicative
                              , SD = "0.1"
                              )

# For urine observation (reset the urine compartment to be zero right after each observation)
residualEffect(model, "A0") = c(errorType = Multiplicative
                                , SD = "0.2"
                                , doafter = "{A0 = 0}"
                                )


# ==========================================================================================================
#                          Create the simulation input dataset 
# ==========================================================================================================
# Create the simulation input data
dt_SimInputData = data.table(ID = seq(1, 2)
                             , Time = 0
                             , Dose = c(10, 20)
                             )

# ==========================================================================================================
#                     Map the input dataset to the created model 
# ==========================================================================================================
# Initialize model mapping (link the input dataset to model@inputData)
# and automatically map some of the model variables to the data columns 
initColMapping(model) = dt_SimInputData
modelColumnMapping(model) # Output the mapping 

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

# Create the default name for the model, input dataset and mapping files 
NlmeFileNames = NlmeDataset()

# Host setup: run locally 
host = NlmeParallelHost(sharedDirectory = Sys.getenv("NLME_ROOT_DIRECTORY")
                        , parallelMethod = NlmeParallelMethod("LOCAL_MPI")
                        , hostName = "local"
                        , numCores = 1
                        )

# -----------------------------------------------------------------------------------
# Simulation setup
# -----------------------------------------------------------------------------------
# Define simulation tables for plasma observations
SimTableCObs = NlmeSimTableDef(name = "SimTableCObs.csv"
                                 , timesList = "0, 0.5, 1, 2, 4, 8, 12, 16, 20, 24"
                                 , variablesList = "C, CObs"
                                 , timeAfterDose = FALSE
                                 )

# Define simulation tables for urine observations
SimTableA0Obs = NlmeSimTableDef(name = "SimTableA0Obs.csv"
                                  , timesList = "12, 24"
                                  , variablesList = "A0Obs"
                                  , timeAfterDose = FALSE
                                  )

# Simulation setup 
SimSetup = NlmeSimulationParams(numReplicates = 50
                            , seed = 1
                            , simulationTables = c(SimTableCObs, SimTableA0Obs)
                            )

# ==========================================================================================================
#                                   Run the model 
# ==========================================================================================================
job = simmodel(host, NlmeFileNames, SimSetup, model)








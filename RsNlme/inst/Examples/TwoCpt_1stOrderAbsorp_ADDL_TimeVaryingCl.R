##############################################################################################################
##                              Description
##
## The purpose of this example is to demonstrate how to create a model involving time-varying parameters 
## in RsNLME, and then simulate it. The model demonstrated is a two-compartment model with first-order absorption,
## where the central clearance, Cl, is described by 
##    Cl = baseCl * (1 - Imax * t^Gam /(t^Gam + T50^Gam)
## 
##############################################################################################################

# Setup environment variables and loading necessary packages 
source("C:/RsNLME/SetUpEnv_LoadRPackages.R")

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
ModelName = "TwoCpt_1stOrderAbsorp_ADDL_TimeVaryingCl"

# Create a basic two-compartment PK model with 1st-order absorption 
model = pkmodel(numCompartments = 2
                , absorption = Extravascular
                , modelName = ModelName
                )

# redefine the central clearance rate, Cl
model = addExpression(model
                      , blockName = "Cl"
                      , codeLine = "baseCl * (1 - Imax * t^Gam /(t^Gam + T50^Gam))"
                      , structuralParams = c("baseCl", "Imax", "T50", "Gam")
                      , override = TRUE
                      )

#----------------------------------------------------------------------------------------------------
# Reset structural model parameters (including initial values for fixed and random effects)
#----------------------------------------------------------------------------------------------------
# reset "Imax" to be logit-normally distributed with the value of corresponding fixed effect to be 1.5
structuralParam(model, "Imax") = c(style = Logit
                                   , initialValue = "1.5"
                                   )

# Fixed effects (the default value is 1)
initFixedEffects(model) = c(tvT50 = 2, tvGam = 3, tvV = 5, tvKa = 0.6, tvV2 = 3, tvCl2 = 0.5) 


# Random effects (the default value is 1) 
initRandomEffects(model) = c(Diagonal
                             , FALSE
                             , "nV, nbaseCl, nV2, nCl2, nKa, nImax, nT50, nGam"
                             , "0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01"
                             )

#-------------------------------------------------------------------------------------------------------------
# Reset residual error model 
# (default: additive residual error model with the standard deviation of residual errors being 1)
#-------------------------------------------------------------------------------------------------------------
# set residual error model to be multiplicative with the standard deviation of residual errors being 0.1
residualEffect(model,"C") = c(errorType = Multiplicative
                              , SD = "0.1"
                              )

# ==========================================================================================================
#                          Create the simulation input dataset 
# ==========================================================================================================
# Create the simulation input data
dt_SimInputData = data.table(ID = seq(1, 2)
                             , Time = 0
                             , Dose = c(10, 20)
                             , ADDL = c(24, 12)
                             , II = c(7, 14)
                             )

# ==========================================================================================================
#                     Map the input dataset to the created model 
# ==========================================================================================================
# Initialize model mapping (link the input dataset to model@inputData)
# and automatically map some of the model variables to the data columns 
initColMapping(model) = dt_SimInputData
modelColumnMapping(model) # Output the mapping 

# Manually set up the mapping for the rest of variables
modelColumnMapping(model) = c(Aa = "Dose")


# Add user-defined extra column definitions (to map the ADDL and II columns)
userDefinedExtraDefinitions(model) = c("addlcol(ADDL)"
                                       , "iicol(II)"
                                       )


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
# Simulation table for structural model parameters 
SimTableStructuralModelParams = NlmeSimTableDef(name = "SimTableStructuralModelParams.csv"
                                                , timesList = "0"
                                                , variablesList = "V,baseCl,V2,Cl2,Ka,Imax,T50,Gam"
                                                , timeAfterDose = FALSE
                                                )

# Define simulation tables for Clearance and plasma observations
SimTableObs = NlmeSimTableDef(name = "SimTableObs.csv"
                                 , timesList = "seq(0, 168, 1)"
                                 , variablesList = "Cl, C, CObs"
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








##############################################################################################################
##                              Description
##
## The purpose of this example is to demonstrate how to create a model involving both continuous and event 
## observations through RsNlme, and then simulate it. The model demonstrated is a 
##   - PK: a one-compartment model with first-order absorption having no time lag, 
##   - Event model: hazard function is described by an Imax model.
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
ModelName = "OneCpt1stOrderAbsorpNoTlag_EventHazardImax"

# Basic one-compartment PK model with 1st-order absorption 
model = pkmodel(numCompartments = 1
                , absorption = Extravascular
                , modelName = ModelName
                )

# Add an event model with  hazard described by an Imax model  
model = addEventObservation(model
                            , observationName = "EventObs"
                            , expression = "1 - C/(C + IC50)"
                            , structuralParameters = c("IC50")
                            )


#----------------------------------------------------------------------------------------------------
# Reset structural model parameters (including initial values for fixed and random effects)
#----------------------------------------------------------------------------------------------------
# Fixed effects (the default value is 1)
initFixedEffects(model) # output initial values for fixed effects 
initFixedEffects(model) = c(tvV = 1, tvCl = 1, tvKa = 1, tvIC50 = 250) # set up initial values for fixed effects 


# Random effects (the default value is 1) 
initRandomEffects(model) = c(Diagonal
                             , FALSE
                             , "nV, nCl, nKa, nIC50"
                             , "0.01, 0.01, 0.01, 0.01"
                             )


#----------------------------------------------------------------------------------------------------
# Reset residual error model
#----------------------------------------------------------------------------------------------------
residualEffect(model,"C") = c(errorType = Multiplicative, SD = "0.1")

# ==========================================================================================================
#                          Create the simulation input dataset 
# ==========================================================================================================

dt_SimInputData = data.table(ID = seq(1, 3, 1)
                             , Time = 0
                             , Dose = c(100, 1000, 10000)
                             )

# ==========================================================================================================
#                     Map the input dataset columns to the created model variables
# ==========================================================================================================

# Initialize model mapping (link the input dataset to model@inputData)
# and automatically map some of the model variables to the data columns 
initColMapping(model) = dt_SimInputData
modelColumnMapping(model) # Output the mapping 

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

# Host setup 
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
                                 , variablesList = "V,Cl,Ka,IC50"
                                 , timeAfterDose = FALSE
                                 )

# Simulation table for observations
SimTableObs = NlmeSimTableDef(name = "SimTableObs.csv"
                                 , timesList = "seq(0, 10, 0.1)"
                                 , variablesList = "Aa, C, CObs, EventObs"
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








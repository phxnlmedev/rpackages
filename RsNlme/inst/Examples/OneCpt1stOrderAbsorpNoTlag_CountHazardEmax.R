##############################################################################################################
##                              Description
##
## The purpose of this example is to demonstrate how to create a model involving both continuous and count
## observations through RsNlme, and then simulate it. The model demonstrated is a 
##   - PK: one-compartment model with first-order absorption having no time lag, 
##   - Count model: hazard function is described by an Emax model.
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
ModelName = "OneCpt1stOrderAbsorpNoTlag_CountHazardEmax"

# Basic one-compartment PK model with 1st-order absorption 
model = pkmodel(numCompartments = 1
                , absorption = Extravascular
                , modelName = ModelName
                )

# Add a count model with hazard being an Emax model 
model = addCountObservation(model
                            , observationName = "CountObs"
                            , expression = "Emax * C / (EC50 + C)"
                            , structuralParameters = c("Emax", "EC50")
                            )


#----------------------------------------------------------------------------------------------------
# Reset structural model parameters (including initial values for fixed and random effects)
#----------------------------------------------------------------------------------------------------
# Fixed effects (the default value is 1)
initFixedEffects(model) # output initial values for fixed effects 
initFixedEffects(model) = c(tvV = 1, tvCl = 1, tvKa = 1, tvEmax = 10, tvEC50 = 500) # set up initial values for fixed effects 


# Random effects (the default value is 1) 
initRandomEffects(model) = c(Diagonal
                             , FALSE
                             , "nV, nCl, nKa, nEmax, nEC50"
                             , "0.01, 0.01, 0.01, 0.01, 0.01"
                             )
                             
  
#----------------------------------------------------------------------------------------------------
# Reset residual error model for CObs
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
#                     Map the input dataset to the created model 
# ==========================================================================================================

# Initialize model mapping (link the input dataset to model@inputData)
# and automatically map some of the model variables to the data columns 
initColMapping(model) = dt_SimInputData
modelColumnMapping(model) # output the mapping 

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
#  Simulation setup
# --------------------------------------------------------------------------
# Simulation table for structural model parameters 
SimTableStructuralModelParams = NlmeSimTableDef(name = "SimTableStructuralModelParams.csv"
                                                , timesList = "0"
                                                , variablesList = "V,Cl,Ka,Emax,EC50"
                                                , timeAfterDose = FALSE
                                                )

# Simulation table for observations
SimTableObs = NlmeSimTableDef(name = "SimTableObs.csv"
                              , timesList = "seq(0, 5, 1)"
                              , variablesList = "Aa, C, CObs, CountObs"
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








##############################################################################################################
##                              Description
##
## The purpose of this example is to demonstrate how to create a model involving only count observations 
## through RsNlme, and then simulate it. The model demonstrated is a count model with hazard being count. 
##
##############################################################################################################

# Setup environment variables and loading necessary packages 
# Setup environment variables and loading necessary packages 
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
ModelName = "Count_ConstantHazard"

# Create a blank model 
model = blankmodel(modelName = ModelName)
        

# Add a count model with constant hazard
model = addCountObservation(model
                            , observationName = "CountObs"
                            , expression = "Hazard"
                            , structuralParameters = c("Hazard")
                            )


#----------------------------------------------------------------------------------------------------
# Reset structural model parameters (including initial values for fixed and random effects)
#----------------------------------------------------------------------------------------------------

# Random effects (the default value is 1) 
initRandomEffects(model) = c(Diagonal
                             , FALSE
                             , "nHazard" 
                             , "0.01"
                             )

# ==========================================================================================================
#                          Create the simulation input dataset 
# ==========================================================================================================

dt_SimInputData = data.table(ID = 1
                             , Time = 0
                             )

# ==========================================================================================================
#                     Map the input dataset to the created model 
# ==========================================================================================================

initColMapping(model) = dt_SimInputData
modelColumnMapping(model) # output the mapping 


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
                                 , variablesList = "Hazard"
                                 , timeAfterDose = FALSE
                                 )

# Simulation table for observations
SimTableObs = NlmeSimTableDef(name = "SimTableObs.csv"
                                 , timesList = "seq(0, 10, 0.1)"
                                 , variablesList = "CountObs"
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








##############################################################################################################
##                              Description
##
## The purpose of this example is to demonstrate how to create a model involving zero-order absorption
## through RsNlme, and then simulate it. The model demonstrated is a one-compartment model
## with zero-order absorption having no time lag, and the clearance is described by a Michaelis-Menten model.
##
##############################################################################################################


##############################################################################################################

##############        Setup Environment Variables and Load Necessary Packages               ##############

##############################################################################################################

# ==========================================================================================================
# Setup environment
# ==========================================================================================================
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
ModelName = "OneCpt_0OrderAbsorpNoTlag_MMElim"

# Basic one-compartment PK model with Michaelis-Menten elimination
model = pkmodel(numCompartments = 1
                , isSaturating = TRUE
                , modelName = ModelName
                )

# Dosing information for the zero-order absorption
DoseInfo = DosePoint(isZeroOrderAbsorption = DurationDose
                 , isTlag = FALSE
                 , isBioavail = FALSE
                 , durationExpression = "D"
                 )

# Update the model with zero-order absorption included
model = zeroOrderAbsorption(model, DoseInfo)


#----------------------------------------------------------------------------------------------------
# Reset structural model parameters (including initial values for fixed and random effects)
#----------------------------------------------------------------------------------------------------
# Fixed effects (the default value is 1)
initFixedEffects(model) = c(tvV = 5, tvVMax = 1, tvKm = 2, tvD = 6)


# Random effects (the default value is 1)
initRandomEffects(model) = c(Diagonal
                             , FALSE
                             , "nV, nD, nVMax, nKm"
                             , "0.01, 0.01, 0.01, 0.01"
                             )

#----------------------------------------------------------------------------------------------------
# Reset residual error model
#----------------------------------------------------------------------------------------------------
residualEffect(model,"C") = c(errorType = Multiplicative, SD = "0.1")

# ==========================================================================================================
#                          Create the simulation input dataset
# ==========================================================================================================
# Create the simulation input data
dt_SimInputData = data.table(ID = seq(1, 2)
                             , Time = 0
                             , Dose = c(10, 20)
                             )

# ==========================================================================================================
#                     Mapping the input dataset to the created model
# ==========================================================================================================
# Initialize model mapping (link the input dataset to model@inputData)
# and automatically map some of the model variables to the data columns
initColMapping(model) = dt_SimInputData
modelColumnMapping(model) # output the mapping


# Manually map the other model variables to input dataset columns
modelColumnMapping(model) = c(A1 = "Dose")



##############################################################################################################

###################                    Model Simulation                                    ###################

##############################################################################################################
# ==========================================================================================================
#          - Set up default name for model, input dataset and mapping file
#          - Set up host platform
#          - Set up simulation parameters (numReplicates, seed, output tables)
# ==========================================================================================================



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
                                                , variablesList = "V,VMax,Km,D"
                                                )
# Simulation table for model variables
SimTableObs = NlmeSimTableDef(name = "SimTableObs.csv"
                              , timesList = "0, 0.5, 1, 2, 4, 8, 12, 16, 20, 24"
                              , variablesList = "C, CObs"
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


tab=read.csv(paste0(model@modelInfo@workingDir,"/SimTableObs.csv"))
View(tab)

tab2=read.csv(paste0(model@modelInfo@workingDir,"/SimTableStructuralModelParams.csv"))
View(tab2)

#library(ggquickeda)
#run_ggquickeda()





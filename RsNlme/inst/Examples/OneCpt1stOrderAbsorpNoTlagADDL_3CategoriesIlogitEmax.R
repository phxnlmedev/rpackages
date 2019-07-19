##############################################################################################################
##                              Description
##
## The purpose of this example is to demonstrate how to create a model involving both continuous and categorical
## observations through RsNlme, and then simulate it. The model demonstrated is a 
##   - PK: one-compartment model with first-order absorption having no time lag for an ADDL case, 
##   - Categorical model: 3 categories with linkFunction being logit.
##
##############################################################################################################

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
ModelName = "OneCpt1stOrderAbsorpNoTlagADDL_3CategoriesIlogitEmax"

# Basic one-compartment PK model with 1st-order absorption 
model = pkmodel(numCompartments = 1
                , absorption = Extravascular
                , modelName = ModelName
                )

# Add a categorical model with 3 categories with linkFunction being logit 
model = addCategoricalObservation(model
                                  , observationName = "CategoricalObs"
                                  , offsetArray = c("E0 + Emax*C/(C+EC50) + Cat1Constant"
                                                    , "E0 + Emax*C/(C+EC50)"
                                                    )
                                  , structuralParameters = c("E0", "Emax", "EC50", "Cat1Constant")
                                  , hasRandomEffect = c(FALSE, FALSE, FALSE, FALSE)
                                  )


#----------------------------------------------------------------------------------------------------
# Reset structural model parameters (including initial values for fixed and random effects)
#----------------------------------------------------------------------------------------------------
# Fixed effects (the default value is 1)
initFixedEffects(model) # output initial values for fixed effects 
initFixedEffects(model) = c(tvV = 1, tvCl = 0.3, tvKa = 1, tvE0 = 0.1, tvEmax = 5, tvEC50 = 500, tvCat1Constant = 1) # set up initial values for fixed effects 


# Random effects (the default value is 1) 
initRandomEffects(model) = c(Diagonal
                             , FALSE
                             , "nV, nCl, nKa"
                             , "0.01, 0.01, 0.01"
                             )

#----------------------------------------------------------------------------------------------------
# Reset residual error model
#----------------------------------------------------------------------------------------------------
residualEffect(model,"C") = c(errorType = Multiplicative, SD = "0.1")

# ==========================================================================================================
#                          Create the simulation input dataset 
# ==========================================================================================================
# Create the simulation input data (ADDL)
dt_SimInputData = data.table(ID = c(1, 2)
                             , Time = 0
                             , Dose = c(100, 1000)
                             , ADDL = c(5, 5)
                             , II = c(12, 12)
                             )

# ==========================================================================================================
#                     Map the input dataset to the created model 
# ==========================================================================================================
#----------------------------------------------------------------------------------------------------
# Add ADDL dose 
#----------------------------------------------------------------------------------------------------
mapExtraDose = ExtraDoseItem(type = BolusDose
                             , amountColumn = "Dose"
                             , deltaTimeColumn = "II"
                             )


model = addExtraDose(model
                     , name = "Aa" 
                     , doseType = AddlDose
                     , doses = c(mapExtraDose)
                     )

#----------------------------------------------------------------------------------------------------
# Map the input dataset columns to the created model variables
#----------------------------------------------------------------------------------------------------
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
                                                   , variablesList = "V,Cl,Ka,E0,Emax,EC50,Cat1Constant"
                                                   , timeAfterDose = FALSE
                                                   )

# Simulation table for observations
SimTableObs = NlmeSimTableDef(name = "SimTableObs.csv"
                                 , timesList = "0, 1, 2, 3, 4, 8, 12, 24, 36, 48, 60, 84, 85, 86, 87, 89, 92, 96 "
                                 , variablesList = "Aa, C, CObs, CategoricalObs"
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








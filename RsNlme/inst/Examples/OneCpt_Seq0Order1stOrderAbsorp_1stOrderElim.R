##############################################################################################################
##                              Description
##
## The purpose of this example is to demonstrate how to create a model involving mixed first-order and zero-order
## absorption through RsNlme, and then simulate it. The model demonstrated is a one-compartment model with  
## first-order elimination, where the zero-order absorption is followed by a first-order absorption. 
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

ModelName = "OneCpt_Seq0Order1stOrderAbsorp_1stOrderElim"

# ==========================================================================================================
#                           Create the PK/PD model through RsNlme
# ==========================================================================================================
#-----------------------------------------------------------------------------------------------------
# Basic one-compartment PK model 
model = pkmodel(numCompartments = 1
                , modelName = ModelName
                )

# Dosing information for the first-order absorption pathway 
DoseInfo_AbsCpt = DosePoint(isZeroOrderAbsorption = No
                            , isTlag = TRUE
                            , tlagExpression = "Tlag1"
                            , isBioavail = TRUE
                            , bioavailExpression = "logitF1; ilogit(logitF1)"
                            )

# Dosing information for the zero-order absorption pathway  
DoseInfo_CentralCpt = DosePoint(isZeroOrderAbsorption = DurationDose
                                , isTlag = FALSE
                                , isBioavail = TRUE
                                , bioavailExpression = "logitF1; 1 - ilogit(logitF1)"
                                , durationExpression = "D2"
                                )


# Update the model with the mixed first-order and zero-order absorption included 
model = mixedFirstOrderZeroOrderAbsorption(model, DoseInfo_CentralCpt, DoseInfo_AbsCpt)

#----------------------------------------------------------------------------------------------------
# Reset structural model parameters (including initial values for fixed and random effects)
#----------------------------------------------------------------------------------------------------
# Reset the distribution form for logitF1
structuralParam(model, "logitF1") = c(style = Normal)

# Fixed effects (the default value is 1)
initFixedEffects(model) # output initial values for fixed effects 
initFixedEffects(model) = c(tvV = 5, tvCl = 1, tvKa1 = 2.5, tvlogitF1 = 0.1, tvD2 = 6, tvTlag1 = 2) # set up initial values for fixed effects 

# Random effects (the default value is 1) 
initRandomEffects(model) = c(Diagonal
                             , FALSE
                             , "nV, nD2, nlogitF1, nCl, nTlag1, nKa1"
                             , "0.01, 0.01, 0.01, 0.01, 0.01, 0.01"
                             )
 

#----------------------------------------------------------------------------------------------------
# Reset residual error model
#----------------------------------------------------------------------------------------------------
residualEffect(model,"C") = c(errorType = Multiplicative, SD = "0.1")

#----------------------------------------------------------------------------------------------------
# Update the model based on the above changes on structural model parameters and residual error model
#----------------------------------------------------------------------------------------------------
# Incorporate the above changes on fixed effects into the model 
model = generatePML(model)

# Output the model 
print(model)

# ==========================================================================================================
#                          Create the simulation input dataset 
# ==========================================================================================================
# Create the simulation input data
dt_SimInputData = data.table(ID = seq(1, 2)
                             , Time = 0
                             , Dose = c(10, 20)
                             )

dt_SimInputData$RepDose = dt_SimInputData$Dose

# ==========================================================================================================
#                     Map the input dataset to the created model 
# ==========================================================================================================
# Initialize model mapping (link the input dataset to model@inputData)
# and automatically map some of the model variables to the data columns 
initColMapping(model) = dt_SimInputData
modelColumnMapping(model) # output the mapping 


# Manually set up the mapping for the rest of variables
modelColumnMapping(model) = c(Aa1 = "Dose", A1 = "RepDose")


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
                                                   , variablesList = "V,Cl,Ka1,logitF1,D2,Tlag1"
                                                   , timeAfterDose = FALSE
                                                   ) 

# Simulation table for simulations 
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
job = simmodel(host, SimSetup, model)


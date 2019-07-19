##############################################################################################################
##                              Description
##
## The purpose of this example is to demonstrate how to create a model involving user-defined extra column  
## definition through RsNlme, and then simulate it. The model demonstrated is a one-compartment model 
## with parallel first-order absorption in an ADDL case. 
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

ModelName = "OneCpt_Parallel1stOrderAbsorp_ADDL_UserDefinedColDef"

# ==========================================================================================================
#                           Create the PK/PD model through RsNlme
# ==========================================================================================================
#-----------------------------------------------------------------------------------------------------
# Basic one-compartment PK model 
model = pkmodel(numCompartments = 1
                , modelName = ModelName
                )

# Dosing information for the first pathway 
DoseInfo_AbsCpt1 = DosePoint(isZeroOrderAbsorption = No
                            , isTlag = FALSE
                            , isBioavail = TRUE
                            , bioavailExpression = "logitF1; ilogit(logitF1)"
                            )

# Dosing information for the second pathway 
DoseInfo_AbsCpt2 = DosePoint(isZeroOrderAbsorption = No
                             , isTlag = TRUE
                             , tlagExpression = "Tlag2"
                             , isBioavail = TRUE
                             , bioavailExpression = "logitF1; 1 - ilogit(logitF1)"
                             )

# Update the model with the parallel first-order absorption included 
model = parallelFirstOrderAbsorption(model, DoseInfo_AbsCpt1, DoseInfo_AbsCpt2)

#----------------------------------------------------------------------------------------------------
# Reset structural model parameters (including initial values for fixed and random effects)
#----------------------------------------------------------------------------------------------------
# Reset the distibution form for logitF1
structuralParam(model, "logitF1") = c(style = Normal)

# Fixed effects (the default value is 1)
initFixedEffects(model) = c(tvV = 5, tvCl = 1, tvKa1 = 0.5, tvKa2 = 1.5, tvTlag2 = 1, tvlogitF1 = 0) # set up initial values for fixed effects 

# Random effects (the default value is 1) 
initRandomEffects(model) = c(Diagonal
                             , FALSE
                             , "nV, nCl, nKa1, nKa2, nTlag2, nlogitF1"
                             , "0.01, 0.01, 0.01, 0.01, 0.01, 0.01"
                             )
                             

#----------------------------------------------------------------------------------------------------
# Reset residual error model
#----------------------------------------------------------------------------------------------------
residualEffect(model, "C") = c(errorType = Multiplicative, SD = "0.1")


# ==========================================================================================================
#                          Create the simulation input dataset 
# ==========================================================================================================

# Create the simulation input data
dt_SimInputData = data.table(ID = seq(1, 2)
                             , Time = 0
                             , Dose = c(10, 20)
                             , RepDose = c(10, 20)
                             , ADDL = c(4, 2)
                             , II = c(12, 24)
                             )


# ==========================================================================================================
#                     Map the input dataset to the created model 
# ==========================================================================================================

#----------------------------------------------------------------------------------------------------
# Initialize model mapping (link the input dataset to model@inputData)
# and automatically map some of the model variables to the data columns 
#----------------------------------------------------------------------------------------------------
initColMapping(model) = dt_SimInputData
modelColumnMapping(model) # Output the mapping 


#----------------------------------------------------------------------------------------------------
# Manually map the other model variables to input dataset columns 
#----------------------------------------------------------------------------------------------------
modelColumnMapping(model) = c(Aa1 = "Dose", Aa2 = "RepDose")

#----------------------------------------------------------------------------------------------------
# Add user-defined extra column definitions (to map the ADDL and II columns)
#----------------------------------------------------------------------------------------------------
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
                                                , variablesList = "V,Cl,Ka1,Ka2,Tlag2,logitF1"
                                                , timeAfterDose = FALSE
                                                ) 

# Simulation table for simulations 
SimTableObs = NlmeSimTableDef(name = "SimTableObs.csv"
                              , timesList = "seq(0, 60, 2)"
                              , variablesList = "Aa1, Aa2, C, CObs"
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


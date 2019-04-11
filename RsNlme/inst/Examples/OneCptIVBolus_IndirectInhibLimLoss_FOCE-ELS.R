##############################################################################################################
##                              Description
##
## This example involves both PK and PD, where PK is described by a one-compartment model with IV bolus, and
## PD is described by an indirect model with the loss of a mediator inhibited. The model is fitted, and then 
## some commonly used diagnostic plots are created. 
##
##############################################################################################################

# Setup environment variables and loading necessary packages 
source("C:/RsNLME/SetUpEnv_LoadRPackages.R")

##############################################################################################################

###############      Load the input dataset, and create the model and column mapping files      ###############

#   - Load the input dataset
#   - define the model through RsNlme
#   - mapping the data to the model 

##############################################################################################################


# ==========================================================================================================
# Load and explore the input dataset
# ==========================================================================================================
InputDataSetName = "OneCptIVBolus_IndirectInhibLimLoss"
dt_InputDataSet = fread(paste0(InputDataSetName, ".csv"))


# ==========================================================================================================
#                           Define the PK/PD model through RsNlme
# ==========================================================================================================
# model name 
ModelName = paste0(InputDataSetName, "_FOCE-ELS")

# define the basic PK/Indirect model 
model = pkindirectmodel(indirectType = LimitedInhibition
                        , isBuildup = FALSE
                        , modelName = ModelName
                        )

# --------------------------------------------------------------------------------------------------------
# Reset the forms of structural model parameters as well as initial values for Theta and Omega
# --------------------------------------------------------------------------------------------------------
# Set Imax = ilogit(tvlogitImax) with ilogit used to make sure it is between 0 and 1
structuralParam(model, "Imax") = c(style = Logit
                                   , fixedEffName = "tvlogitImax"
                                   , hasRandomEffect = FALSE
                                   )

# disable random effect for IC50
structuralParam(model, "IC50") = c(hasRandomEffect = FALSE)


# reset initial values for fixed effects  (the default value is 1)
initFixedEffects(model) = c(tvCl = 0.5, tvKin = 10, tvKout = 0.5)  

# reset initial values for random effects (the default value is 1)
# Note: Even when one only wants to reset the value some of random effects, 
#       one has to do this for all the random effects, 
#       and can only use "initRandomEffects" once. 
initRandomEffects(model) = c(Diagonal
                             , FALSE
                             , "nV, nCl, nKin, nKout"
                             , "0.01, 0.01, 0.01, 0.01"
                             )

# --------------------------------------------------------------------------------------------------------
# Reset the residual error models 
# --------------------------------------------------------------------------------------------------------
# reset the residual error model for CObs
residualEffect(model, "C") = c(errorType = Multiplicative
                             , SD = "0.1"
                             )

# reset the residual error model for EObs
residualEffect(model, "E") = c(errorType = Multiplicative
                               , SD = "0.1"
                               )

# ==========================================================================================================
#                     Mapping the input dataset to the created model 
# ==========================================================================================================
# initialize model mapping and automatically mapping some of the model variables to the data columns 
initColMapping(model) = dt_InputDataSet
modelColumnMapping(model) # output the mapping 

# manually mapping the rest of the variables
modelColumnMapping(model) = c(A1 = "Dose")


##############################################################################################################

###################                    Model Fitting                                      ################### 

##############################################################################################################
# create the default name for the model, input dataset and mapping files 
NlmeFileNames = NlmeDataset()


# host setup: run locally with MPI enabled
host = NlmeParallelHost(sharedDirectory = Sys.getenv("NLME_ROOT_DIRECTORY")
                        , parallelMethod = NlmeParallelMethod("LOCAL_MPI")
                        , hostName = "MPI"
                        , numCores = 4
						)

# engine setup 
engineParams = NlmeEngineExtraParams(PARAMS_METHOD = METHOD_FOCE_ELS
                                     , PARAMS_NUM_ITERATIONS = 1000
                                     , PARAMS_SAND="TRUE"
                                     )


# run the model 
job = fitmodel(host, NlmeFileNames, engineParams, model)

##############################################################################################################

###################                      Diagnostic plots                            ########################

##############################################################################################################

xp = xposeNlme(dir="./", modelName = ModelName)

# ==========================================================================================================
#      Observations against population or individual predications for each observed variable
# ==========================================================================================================
# observations against population predications 
dv_vs_pred(xp
           , type = "p"
           , facets = "ObsName"
           )

# observations against individual predications 
dv_vs_ipred(xp
            , type = "p"
            , facets = "ObsName"
            )

# ==========================================================================================================
#    Residuals against population predications or independent variable for each observed variable  
# ==========================================================================================================

# CWRES against population predications 
res_vs_pred(xp
            , res = "CWRES"
            , type = "ps"
            , facets = "ObsName"
            )

# CWRES against the independent variable
res_vs_idv(xp
           , res = "CWRES"
           , type = "ps"
           , facets = "ObsName"
           )


# ==========================================================================================================
#          Distribution plots of ETA  
# ==========================================================================================================
eta_distrib(xp)



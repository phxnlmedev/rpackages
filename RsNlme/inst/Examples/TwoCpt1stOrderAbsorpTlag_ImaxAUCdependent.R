##############################################################################################################
##                              Description
##
## The purpose of this example is to demonstrate how to create a customized model in RsNLME. 
##
## The model demonstrated is a standard PK model with a customized PD model. Specifically, PK is described by 
## a standard two-compartment model with first-order absorption, and PD is descibed by 
##
##        E = E0 * (1 - Imax * AUC/(IC50 + AUC))
##
## where AUC denotes the area under the curve.
##
##############################################################################################################

# Setup environment variables and loading necessary packages 
source("C:/RsNLME/SetUpEnv_LoadRPackages.R")

##############################################################################################################

###############      Create the Model, Simulation Input DataSet, and Column Mapping files      ###############

#   - Create the model through RsNlme
#   - Create the simulation input dataset
#   - Mapping the input dataset to the model 
##############################################################################################################

# ==========================================================================================================
#                           Define the PK/PD model through RsNlme
# ==========================================================================================================
# Model name 
ModelName = "TwoCpt1stOrderAbsorpTlag_ImaxAUCdependent"

# Create a standard two-compartment model with 1st order absorption 
model = pkmodel(numCompartments = 2
                , absorption = Extravascular
                , isTlag = TRUE
                , modelName = ModelName
                )

# --------------------------------------------------------------------------------------------------------
# Add a customized Imax model 
# --------------------------------------------------------------------------------------------------------
# add a customized Imax model 
# Note: the newly introduce structural parameter, Imax, will be defined through "addParameter"
#       to customize the style including the distribution form, the names of fixed and random effects.
model = addProcedure(model
                     , blockName = "EmaxAUC"
                     , codeLines = list("deriv(AUC = C)"
                                        , "E = E0 * (1 - Imax * AUC/(IC50 + AUC))"
                                        )
                     , structuralParams = c("E0", "IC50")
                     )


# Imax: logit-normally distributed with form given by 
# Imax = ilogit(tvlogitImax + nlogitImax)
# This form is used to ensure that Imax is between 0 and 1
model = addParameter(model
                     , name = "Imax"
                     , style = Logit
                     , fixedEffName = "tvlogitImax"
                     , randomEffName = "nlogitImax"
                     , initialValue = "0.2"
                     )

# --------------------------------------------------------------------------------------------------------
# Reset the residual error model for CObs, and add residual error model for EObs
# --------------------------------------------------------------------------------------------------------

# Reset the residual error model for CObs
residualEffect(model, "C") = c(errorType = Multiplicative
                               , SD = "0.1"
                               )

# Residual error setup for EObs
ResidualErrorSetUp = NlmeResidualEffect(epsilonName = "EEps"
                                        , effectName = "E"
                                        , errorType = Additive
                                        , SD = 5
                                        )

# Add residual error model for EObs
model = addContinuousObservation(model
                                 , observationName = "EObs"
                                 , effect = ResidualErrorSetUp
                                 )

# --------------------------------------------------------------------------------------------------------
# Reset initial values for Theta and Omega
# Note: the functions "initRandomEffects" and "initFixedEffects" should be used only after all the components
#       of model (including the residual error model added by using "addContinuousObservation") are generated.
# --------------------------------------------------------------------------------------------------------

# reset initial values for fixed effects 
# (default value for the general fixed effect is 1, and the default value for the one related to covariates is 0)
initFixedEffects(model) = c(tvV = 314, tvCl = 30, tvTlag = 0.2, tvKa = 1.3, tvV2 = 272, tvCl2 = 83
                            , tvIC50 = 296, tvE0 = 85
                            )  

# reset initial values for random effects (the default value is 1)
initRandomEffects(model) = c(Diagonal
                             , FALSE
                             , "nV, nCl, nTlag, nKa, nV2, nCl2, nIC50, nE0, nlogitImax"
                             , "0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01"
                             )



# ==========================================================================================================
#                          create the simulation input dataset 
# ==========================================================================================================
# create the simulation input data
dt_SimInputData = data.table(ID = 1
                             , Time = 0
                             , Dose = 25
                             , ADDL = 336
                             , II = 12
                             )


# ==========================================================================================================
#                     Mapping the input dataset to the created model 
# ==========================================================================================================
# initialize model mapping and automatically mapping some of the model variables to the data columns 
initColMapping(model) = dt_SimInputData

# output the mapping 
modelColumnMapping(model) 

# manually mapping the other model variables to input dataset columns 
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
#          - set up simulation parameters (numReplicates, seed, output tables)
# ==========================================================================================================

# Create the default name for the model, input dataset and mapping files 
NlmeFileNames = NlmeDataset()

# Host setup 
host = NlmeParallelHost(sharedDirectory = Sys.getenv("NLME_ROOT_DIRECTORY")
                        , parallelMethod = NlmeParallelMethod("LOCAL_MPI")
                        , hostName = "MPI"
                        , numCores = 1
                        )

# ----------------------------------------------------------------------------------------------------
# Simulation setup
# ---------------------------------------------------------------------------------------------------
# Simulation table for structural model parameters 
SimTableStructuralModelParams = NlmeSimTableDef(name = "SimTableStructuralModelParams.csv"
                                                , timesList = "0"
                                                , variablesList = "V,Cl,Tlag,Ka,V2,Cl2,IC50,E0,Imax"
                                                , timeAfterDose = FALSE
                                                )

# Simulation table for model variables 
SimTableObs = NlmeSimTableDef(name = "SimTableObs.csv"
                              , timesList = "0, 0.5, 1, 2,  4, 8,  seq(12, 4032, 12)"
                              , variablesList = "CObs, EObs"
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

# ==========================================================================================================
#                                   plot the results 
# ==========================================================================================================
# read the simulated observations 
dt_SimObs = fread("SimTableObs.csv")
colnames(dt_SimObs)[1] = 'repl'

# plot for CObs
plot_CObs = ggplot(dt_SimObs, aes(x = time, y = CObs, group = repl, color = repl)) +
  geom_line() + 
  geom_point()

# plot for EObs
plot_EObs = ggplot(dt_SimObs, aes(x = time, y = EObs, group = repl, color = repl)) +
  geom_line() + 
  geom_point()

# put the two plots side-by-side
egg::ggarrange(plot_CObs, plot_EObs)
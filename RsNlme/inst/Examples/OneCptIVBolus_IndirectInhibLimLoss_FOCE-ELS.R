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
# Set the forms of structural model parameters as well as initial values for Theta and Omega
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

# set initial values for random effects (the default value is 1)
# Note: Even when one only wants to reset the value some of random effects, 
#       one has to do this for all the random effects, 
#       and can only use "initRandomEffects" once. 
initRandomEffects(model) = c(Diagonal
                             , FALSE
                             , "nV, nCl, nKin, nKout"
                             , "0.01, 0.01, 0.01, 0.01"
                             )

# --------------------------------------------------------------------------------------------------------
# Set the residual error models 
# --------------------------------------------------------------------------------------------------------
# set the residual error model for CObs
residualEffect(model, "C") = c(errorType = Multiplicative
                             , SD = "0.1"
                             )

# set the residual error model for EObs
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


##############################################################################################################

###################                    VPC                                   ################### 

##############################################################################################################

# Accept all the estimates for fixed effects, random effects and sigma (to be used for VPC simulation)
modelVPC = acceptAllEffects(model)

# Copy the model into a new object, and then create a new working directory and copied all the files to it
modelVPC = copyModel(modelVPC, modelName = paste0(ModelName, "_VPC"))

# ==========================================================================================================
#          - Set up default name for model, input dataset, and mapping files
#          - Set up host platform 
#          - set up simulation parameters (numReplicates, seed, output tables)
# ==========================================================================================================
# Define the file name for VPC simulation results (the default name is out.txt).
vpcOutputFileName = "predout.csv"

# Create the default name for the model, input dataset and mapping files 
NlmeFileNames = NlmeDataset(outputFilename = vpcOutputFileName)


# Host setup 
host = NlmeParallelHost(sharedDirectory = Sys.getenv("NLME_ROOT_DIRECTORY")
                        , parallelMethod = NlmeParallelMethod("LOCAL_MPI")
                        , hostName = "MPI"
                        , numCores = 1
                        )

# VPC setup
VPCSetup = NlmeVpcParams(numReplicates = 100
                         , seed = 1
                         )

# ==========================================================================================================
#                                   Run the model 
# ==========================================================================================================
job = vpcmodel(host, NlmeFileNames, VPCSetup, modelVPC)

# ==========================================================================================================
#                             Using vpc library to do the VPC plots 
# ==========================================================================================================

# Load simulation input dataset (the generated predcheck0.csv put all the observations in one column)
dt_ObsData = fread("predcheck0.csv")
setnames(dt_ObsData, c("IVAR", "ID5"), c("TIME", "ID"))
dt_ObsData_CObs = dt_ObsData[ObsName == "CObs"]
dt_ObsData_EObs = dt_ObsData[ObsName == "EObs"]


# load simulated data 
dt_SimData = fread(vpcOutputFileName)
setnames(dt_SimData, c("ID5", "IVAR"), c("ID", "TIME"))
dt_SimData_CObs = dt_SimData[OBSNAME == "CObs"]
dt_SimData_EObs = dt_SimData[OBSNAME == "EObs"]

# VPC plots 
plot_CObsVPC = vpc(sim = dt_SimData_CObs, obs = dt_ObsData_CObs, ylab = "CObs")
plot_EObsVPC = vpc(sim = dt_SimData_EObs, obs = dt_ObsData_EObs, ylab = "EObs")
egg::ggarrange(plot_CObsVPC, plot_EObsVPC)

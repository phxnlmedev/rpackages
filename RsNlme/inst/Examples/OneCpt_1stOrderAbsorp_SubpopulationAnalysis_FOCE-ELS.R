##############################################################################################################
##                              Description
##
##
## The purpose of this example is to demonstrate how to do a subpopulation type of analysis. Specifically,
## the input dataset, OneCpt_1stOrderAbsorp_SubpopulationAnalysis.csv, contains a column "Source". For each  
## value of "Source", we want to fit the same model but have the results seperated by the Source value. 
## We also demonstrate how to the use the xpose.Nlme package to create commonly used diagnostic plots 
## for each analysis.
##
##
##############################################################################################################

# Setup environment variables and loading necessary packages 
source("c:/Work/NlmeInstall_07_10_19/Examples/SetUpEnv_LoadRPackages.R")
setwd("c:/Work/NlmeInstall_07_10_19/Examples/")

##############################################################################################################

###############      Load the input dataset, and create the model and column mapping files      ###############

#   - Load and explore the input dataset
#   - define the model through RsNlme
#   - mapping the data to the model 
##############################################################################################################


# ==========================================================================================================
# Load the input dataset
# ==========================================================================================================
InputDataSetName = "OneCpt_1stOrderAbsorp_SubpopulationAnalysis"
dt_InputDataSet = fread(paste0(InputDataSetName, ".csv"))

# ==========================================================================================================
#                           Define the PK/PD model through RsNlme
# ==========================================================================================================
# model name 
ModelName = paste0(InputDataSetName, "_FOCE-ELS")

# define the basic PK model (a one-compartment model with 1st-order absorption)
model = pkmodel(numCompartments = 1
                , absorption = Extravascular
                , modelName = ModelName
                )

# --------------------------------------------------------------------------------------------------------
# Reset initial values for Theta and Omega
# --------------------------------------------------------------------------------------------------------
# reset initial values for fixed effects 
# (default value for the general fixed effect is 1, and the default value for the one related to covariates is 0)
initFixedEffects(model) = c(tvV = 5)  

# reset initial values for random effects (the default value is 1)
initRandomEffects(model) = c(Diagonal
                             , FALSE
                             , "nKa, nV, nCl"
                             , "0.01, 0.01, 0.01"
                             )

# --------------------------------------------------------------------------------------------------------
# Reset the residual error model 
# --------------------------------------------------------------------------------------------------------
residualEffect(model, "C") = c(errorType = Multiplicative
                             , SD = "0.1"
                             )

# ==========================================================================================================
#                     Mapping the input dataset to the created model 
# ==========================================================================================================
# initialize model mapping and automatically mapping some of the model variables to the data columns 
initColMapping(model) = dt_InputDataSet

# output the mapping 
modelColumnMapping(model) 

# manually set up the mapping for the rest of variables
modelColumnMapping(model) = c(Aa = "Dose")

                              
##############################################################################################################

###################                    Model Fitting                                      ################### 

##############################################################################################################



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

# list of sort variables
sortColumns = NlmeSortColumns("Source")



# run the model 
job = sortfit(host, engineParams, sortColumns,model=model)


##############################################################################################################

###################                      Diagnostic plots                            ########################

##############################################################################################################

# Create a xpose database object for the results obtained for Source = FM 
xp_FM = xposeNlme(dir=model@modelInfo@workingDir, modelName = ModelName
                  , dmpFile = "dmpFM.txt"
                  , dataFile = "data1.txt.1"
                  )

# Create a xpose database object for the results obtained for Source = FM 
xp_SM = xposeNlme(dir=model@modelInfo@workingDir, modelName = ModelName
                  , dmpFile = "dmpSM.txt"
                  , dataFile = "data1.txt.2"
                  )


# Specify which anaysis one wants to see the diagnostic plots
xp = xp_SM


# ==========================================================================================================
#            Observations against population or individual predications 
# ==========================================================================================================
# observations against population predications 
dv_vs_pred(xp
           , type = "p"
           )

# observations against individual predications 
dv_vs_ipred(xp
            , type = "p"
            )


# ==========================================================================================================
#           Residuals against population predications or independent variable   
# ==========================================================================================================
# CWRES against population predications 
res_vs_pred(xp
            , res = "CWRES"
            , type = "ps"
            )

# CWRES against the independent variable
res_vs_idv(xp
           , res = "CWRES"
           , type = "ps"
           )

# ==========================================================================================================
# Observations, individual predictions and population predictions plotted against 
# the independent variable for every individual   
# ==========================================================================================================
ind_plots(xp)

# ==========================================================================================================
#          Distribution plots of ETA  
# ==========================================================================================================
eta_distrib(xp)




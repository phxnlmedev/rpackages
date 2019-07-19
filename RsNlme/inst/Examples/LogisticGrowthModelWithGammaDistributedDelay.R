##############################################################################################################
##                              Description
##
## The model considered here is a logistic growth model with gamma distributed delay. 
##
##            dS(t)/dt = r * S(t) * (1 - 1/K * int_0^infty g(tau)S(t-tau)dtau)
##            S(t) = 0.5, t <= 0
##
##  Here 
##    - r: instrinsic growth rate
##    - K: Carrying capacity
##    - g: probability density function of a gamma distribution with shape parameter being 2.5 and mean being
##          MeanDelayTime. 
## 
## It is simulated with different values for the MeanDelayTime to explore the effect of the MeanDelayTime on 
## the system dynamics. 
##
## This model as well as how to implement the gamma distributed delay in Phoenix Modeling Language (PML)
## can be found in the reference below.  
##
## Hu, S., Dunlavey, M., Guzy, S. Teuscher, N.,  A distributed delay approach for modeling delayed outcomes 
## in pharmacokinetics and pharmacodynamics studies, J Pharmacokinet Pharmacodyn (2018) 45: 285. 
## https://doi.org/10.1007/s10928-018-9570-4
##
##############################################################################################################


# Setup environment variables and loading necessary packages 
source("c:/Work/NlmeInstall_07_10_19/Examples/SetUpEnv_LoadRPackages.R")
setwd("c:/Work/NlmeInstall_07_10_19/Examples/")


# model name 
ModelName = "LogisticGrowthModelWithGammaDistributedDelay"

# MeanDelayTime considered
MeanDelayTimeValues = c(1, 2, 4)

# num cases considered
numMeanDelayTimeValues = length(MeanDelayTimeValues)

##############################################################################################################

###############      Simulate Logistic Growth Model with Gamma Distributed delay 
###############         Mean Delay Time being the first value considered                       ###############

##############################################################################################################

# ==========================================================================================================
#     Create the Model, Simulation Input DataSet, and Column Mapping files   
#
#   - Create the model through RsNlme
#   - Create the simulation input dataset
#   - Mapping the input dataset to the model 
# ==========================================================================================================


# ---------------------------------------------------------------------------------------------------------
#                           Define the PK/PD model through RsNlme
#----------------------------------------------------------------------------------------------------------
# define an empty model 
model = blankmodel(modelName = paste0(ModelName, "_DelayTime", MeanDelayTimeValues[1])
                   , isPopulation = FALSE
                   )

# define the logistic growth model with gamma distributed delay through the "addProcedure" function 
model = addProcedure(model
                     , blockName = "LogisticGrowthModel_GammaDistributedDelay"
                     , codeLines = list("delayedS = delay(S, MeanDelayTime, shape = 2.5, hist = 0.5)"
                                        , "deriv(S = r * S * (1 - delayedS/K))"
                                        , "sequence {S = 0.5} "
                                        )
                     , structuralParams = list("r", "K", "MeanDelayTime")
                     )

# reset initial values for fixed effects
initFixedEffects(model) = c(tvr = 0.8, tvK = 1, tvMeanDelayTime = MeanDelayTimeValues[1]) 

# ----------------------------------------------------------------------------------------------------
#                          create the simulation input dataset 
#  ----------------------------------------------------------------------------------------------------
# create the simulation input data
dt_SimInputData = data.table(Time = 0)

# ----------------------------------------------------------------------------------------------------
#                     Mapping the input dataset to the created model 
# ----------------------------------------------------------------------------------------------------
# initialize model mapping (link the input dataset to model@inputData)
# and automatically mapping some of the model variables to the data columns 
initColMapping(model) = dt_SimInputData
modelColumnMapping(model) # output the mapping 


# ==========================================================================================================
#
#                   Model Simulation                                    
#
#          - Set up default name for model, input dataset and mapping file
#          - Set up host platform 
#          - set up simulation parameters (numReplicates, seed, output tables)
#          - Run the model and then load the simulation results 
# ==========================================================================================================



# Host setup 
host = NlmeParallelHost(sharedDirectory = Sys.getenv("NLME_ROOT_DIRECTORY")
                        , installationDirectory = Sys.getenv("INSTALLDIR")
                        , parallelMethod = NlmeParallelMethod("LOCAL_MPI")
                        , hostName = "MPI"
                        , numCores = 1
                        )

# ----------------------------------------------------------------------------------------------------
# Simulation setup
# ----------------------------------------------------------------------------------------------------

# Simulation table for model variables 
SimTableObs = NlmeSimTableDef(name = "SimTableObs.csv"
                              , timesList = "seq(0,50,0.5)"
                              , variablesList = "S"
                              )

# Simulation setup 
SimSetup = NlmeSimulationParams(simulationTables = c(SimTableObs))

# ----------------------------------------------------------------------------------------------------
#                     Run the model and then load the simulation results 
# ----------------------------------------------------------------------------------------------------
# Run the model 
job = simmodel(host, SimSetup, model)

# Load the results 
dt_temp = fread(paste0(model@modelInfo@workingDir,"/SimTableObs.csv"))

# Add a column to indicate the value of MeanDelayTime
dt_temp$MeanDelayTime = MeanDelayTimeValues[1]

# Extract the necessary columns 
dt_SimOutputData = dt_temp[, c("time", "S", "MeanDelayTime"), with = FALSE]


##############################################################################################################


###############      Simulate Logistic Growth Model with Gamma Distributed delay 
###############            MeanDelayTime being other values considered                         ###############  

##############################################################################################################

if (numMeanDelayTimeValues > 1) {
  
  for (i in 2:numMeanDelayTimeValues){
    
    # Set the mean delay time to the next considered value
    initFixedEffects(model) = c(tvMeanDelayTime = MeanDelayTimeValues[i]) 
    
    # Copy the model to a new object and then create a new working directory and copied all the files to it
    modelTemp = copyModel(model, modelName = paste0(ModelName, "_DelayTime", MeanDelayTimeValues[i]))
    
    # Simulate the new model 
    job = simmodel(host, SimSetup, modelTemp)
    
    # Load the results 
    dt_temp = fread(paste0(model@modelInfo@workingDir,"/SimTableObs.csv"))
    
    # Add a column to indicate the value of MeanDelayTime
    dt_temp$MeanDelayTime = MeanDelayTimeValues[i]
    
    # Extract the necessary columns 
    dt_SimOutputData = rbind(dt_SimOutputData, dt_temp[, c("time", "S", "MeanDelayTime"), with = FALSE])
    
  }
  
}


##############################################################################################################

###############                    Plot the simulation results                                ###############

##############################################################################################################

# Change the format of MeanDelayTime to factor 
dt_SimOutputData$MeanDelayTime = as.factor(dt_SimOutputData$MeanDelayTime)

# plot the results 
ggplot(dt_SimOutputData, aes(x = time, y = S
                             , group = MeanDelayTime
                             , color = MeanDelayTime
                             , linetype = MeanDelayTime
                             )
       ) + 
  geom_line(size = 1) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")
        , legend.position = c(0.8, 0.9)
        ) +
  xlab("t (Time)") + 
  ylab("S(t)")



##############################################################################################################
#                         Description 
#
# The purpose of this file is to
#
#   - demonstrate how to fit a model defined by PML codes (see "test.mdl" for details) to the dataset defined
#     in the csv file in this directory
#   - demonstrate how to create commonly used diagonistic plots
#   - demonstrate how to do VPC
#
# =============================================================================================================
#
# Note: to run a model in NLME, the following three files are required
#
#   - model file: the default name is "test.mdl"
#   - Input dataset: the default name is "data1.txt" with "##" prepended in the header
#   - column mapping file: the default name is "cols1.txt"
#
# Hence, we will demonstrate how to create the required input dataset using the provided csv file and to
# generate the required column definition file in R.
#
##############################################################################################################

# Setup environment variables and loading necessary packages 
source("C:/RsNLME/SetUpEnv_LoadRPackages.R")


##############################################################################################################

###################                    Model Fitting                                    ################### 
#
# - Read the csv file and then write it to a text file with default name "data1.txt".
# - Create a colname definition file with default name "cols1.txt"
# - Setup the running mode
# - Run the model 
##############################################################################################################
# ==========================================================================================================
#    Load the input dataset and then write it to text file with default name "data1.txt"
# ==========================================================================================================

# read the csv file 
dt_InputData = fread("OneCpt_GammaDistributedAbsorptionDelay.csv")

# prepend the "##" to the first column names 
colnames(dt_InputData)[1] = paste0("##", colnames(dt_InputData)[1])

# write the data to a text file with its name being the default one "data1.txt" 
# quote = FALSE is used to avoid the quotation marks on the column names 
write.table(dt_InputData 
            , file = "data1.txt"
            , sep = ","
            , row.names = FALSE
            , quote = FALSE
            )

# ==========================================================================================================
#                          Create colname definition file
# ==========================================================================================================
# Define column definition text
ColDefText = c('id("ID")'
               , 'time("time")'
               , 'dose(A1 <- "Dose")'
               , 'obs(CObs <- "CObs")'
               , 'table(file="posthoc.csv",time(0),V,Cl,MeanDelayTime,ShapeParamMinusOne)'
               )

# Write the column definition text to a text file with its name being the default one "cols1.txt"
FileConnection = file("cols1.txt")
writeLines(ColDefText, FileConnection)
close(FileConnection)

# ==========================================================================================================
#                          Setup the running mode
# ==========================================================================================================
#  create the default name for the model, input dataset and mapping files 
NlmeFileNames = NlmeDataset()


# host setup: run locally 
host = NlmeParallelHost(sharedDirectory = Sys.getenv("NLME_ROOT_DIRECTORY")
                        , parallelMethod = NlmeParallelMethod("LOCAL_MPI")
                        , hostName = "MPI"
                        , numCores = 4
                        )

# engine setup 
engineParams = NlmeEngineExtraParams(PARAMS_METHOD = METHOD_QRPEM
                                     , PARAMS_NUM_ITERATIONS = 1000
                                     , PARAMS_SAND="TRUE"
                                     )


# ==========================================================================================================
#                                   Run the model 
# ==========================================================================================================

job = fitmodel(host, NlmeFileNames, engineParams)


##############################################################################################################

###################                      Diagnostic plots                            ########################

##############################################################################################################
xp = xposeNlme(dir="./")


# ==========================================================================================================
#            Observations against population or individual predications 
# ==========================================================================================================
# observations against population predications 
dv_vs_pred(xp, type = "p")

# observations against individual predications 
dv_vs_ipred(xp, type = "p")


# ==========================================================================================================
#           Residuals against population predications or independent variable   
# ==========================================================================================================
# CWRES against population predications 
res_vs_pred(xp, res = "CWRES", type = "ps")

# CWRES against the independent variable
res_vs_idv(xp, res = "CWRES", type = "ps")


# ==========================================================================================================
# Observations, individual predictions and population predictions plotted against 
# the independent variable for every individual   
# ==========================================================================================================
ind_plots(xp)


# ==========================================================================================================
#          Distribution plots of ETA  
# ==========================================================================================================
eta_distrib(xp)


##############################################################################################################

###################                    VPC                                   ################### 

##############################################################################################################

# Specify the directory where the source files are located 
# Note: if the directory is relative, then it should be relative to the directory of the newly created folder 
# that will be automatically generated by the "phxmodel" (see the code followed by below one). 
PMLSourceDir = "../"
NlmeFileNames = NlmeDataset(phoenixSourceDir = PMLSourceDir)

# Create a model object based on PML file, generate a new folder in the current directory using the name 
# specified by the "modelName" argument, and then copy all the files to this new directory
# Note: nlmeargs.txt is used for the engine/simulation setup in Phoenix. Since this will be defined,
# there is no need to worry about the generated warning message.
model = phxmodel(modelName = "VPC", dataset = NlmeFileNames)

# Copy the dmp.txt file (containing estimation results) to this new folder 
file.copy(from = paste0(PMLSourceDir, "dmp.txt"), to = "dmp.txt")

# Accept all the estimates for fixed effects, random effects and sigma based on the results provided by 
# dmp.txt (to be used for VPC simulation)
modelVPC = acceptAllEffects(model)

# Update the test.mdl with the initial estimates for fixed and random effects as well as the residual error 
# to their corresponding estimated values.
sink("test.mdl")
writeLines(unlist(modelVPC@statements))
sink()

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
job = vpcmodel(host, NlmeFileNames, VPCSetup)

# ==========================================================================================================
#                             Using vpc library to do the VPC plots 
# ==========================================================================================================
# load observed data
dt_ObsData = getObsData(dt_InputData)


# load simulated data 
dt_SimData = getSimData(input = dt_InputData, simFile = vpcOutputFileName)


# Create a VPC plot 
vpc(sim = dt_SimData, obs = dt_ObsData
    , xlab = "Time", ylab = "Drug concentration at the central compartment"
    )





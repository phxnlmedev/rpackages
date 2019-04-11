##############################################################################################################
#                         Description 
#
# The purpose of this file is to demonstrate how to simulate a model defined by PML codes 
# (see "test.mdl" for details).
#
# =============================================================================================================
#
# Note: to run a model in NLME, the following three files are required
#
#   - model file: the default name is "test.mdl"
#   - Input dataset: the default name is "data1.txt" with "##" prepended in the header
#   - column mapping file: the default name is "cols1.txt"
# 
# Hence, we will demontrate how to create the required input dataset and column definition file in R.
#  
##############################################################################################################

# Setup environment variables and loading necessary packages 
source("C:/RsNLME/SetUpEnv_LoadRPackages.R")


##############################################################################################################

###################                    Model Simulation                                    ################### 
#
# - Create simulation input dataset  (multiple identical oral bolus)
# - Create colname definition file
# - Setup simulation running mode
# - Run the model 
##############################################################################################################
# ==========================================================================================================
#                  Create simulation input dataset  (multiple identical oral bolus)
# ==========================================================================================================
# create the simulation input dataset
dt_SimInputData = data.table("##ID" = seq(1, 2)
                             , Time = 0
                             , Dose = c(100, 200)
                             , ADDL = c(10, 5)
                             , II = c(12, 24)
                             )

# this column is used in the sequence statement to reset the Ka value 
dt_SimInputData$DosingInterval = dt_SimInputData$II 

# write the data to a text file with its name being the default one "data1.txt" 
# quote = FALSE is used to avoid the quotation marks on the column names 
write.table(dt_SimInputData
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
               , 'time("Time")'
               , 'dose(Aa <- "Dose")'
               , 'covr(DosingInterval <- "DosingInterval")'
               , 'addlcol(ADDL)'
               , 'iicol(II)'               
               , 'simtbl(file = "SimTable_StructuralParam.csv", time(0), V, Cl, Ka_bChangePt, Ka_aChangePt, TimePt_KaChange)'
               , 'simtbl(file = "SimTable_Obs.csv", time(seq(0, 120, 1)), Ka, C, CObs)'
               )

# Write the column definition text to a text file with its name being the default one "cols1.txt"
FileConnection = file("cols1.txt")
writeLines(ColDefText, FileConnection)
close(FileConnection)

# ==========================================================================================================
#                          Setup simulation running mode
# ==========================================================================================================
#  create the default name for the model, input dataset and mapping files 
NlmeFileNames = NlmeDataset()


# host setup: run locally 
host = NlmeParallelHost(sharedDirectory = Sys.getenv("NLME_ROOT_DIRECTORY")
                        , parallelMethod = NlmeParallelMethod("LOCAL_MPI")
                        , hostName = "MPI"
                        , numCores = 1
                        )


# simulation setup
SimSetup = NlmeSimulationParams(numReplicates = 50
                                , seed = 1
                                )

# ==========================================================================================================
#                                   Run the model 
# ==========================================================================================================
job = simmodel(host, NlmeFileNames, SimSetup)






library("XML")
library("Certara.NLME8")

main<-function(){
    args <- commandArgs(TRUE)
    
    if ( length(args) == 11 ) {
        performProfileEstimation(args)
    }
    else {
        print("Usage:profile_estimation.r parallel_mechanism install_directory shared_directory local_working_dir control_file num_columns column_names profile_values use_percentage_flag num_proc scenarioName")
    }
}

print(main())
 

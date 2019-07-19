

library("XML")
library("Certara.NLME8")

main<-function(){
    args <- commandArgs(TRUE)
    
    if ( length(args) == 14 ) {
        performStepwiseCovarSearch(args)
    }
    else {
    print("Usage:stepwise_covarsrch.r parallel_mechanism install_directory shared_directory local_working_dir model_file args_file num_covariates covar_names criteria addPValue removePValue num_proc scenarioName")
    }
}

print(main())
 

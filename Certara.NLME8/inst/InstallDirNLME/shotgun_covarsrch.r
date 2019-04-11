

library("XML")
library("Certara.NLME8")

main<-function(){
    args <- commandArgs(TRUE)
    
    if ( length(args) == 11 ) {
        performShotgunCovarSearch(args)
    }
    else {
    print("Usage:shotgun_covarsrch.r parallel_mechanism install_directory shared_directory local_working_dir model_file args_file num_covariates covar_names num_proc ")
    }
}

print(main())
 

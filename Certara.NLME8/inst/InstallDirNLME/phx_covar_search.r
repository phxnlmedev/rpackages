

library("XML")
library("Certara.NLME8")

main<-function(){
    args <- commandArgs(TRUE)
    
    if ( length(args) == 8 ) {
        performParallelNLMERun(args)
    }
    else if ( length(args) == 9 ) {
        reconnectGenericGridJob(args)
    }
    else {
    print("Usage:covar_search.r job_type parallel_mechanism shared_directory local_working_dir control_file num_proc workflow_name [gridDirectory]")
    }
}

print(main())
 


library("XML")
library("Certara.NLME8")

main<-function(){
    args <- commandArgs(TRUE)
    
    if ( length(args) == 16 ) {
        performBootstrap(args,reportProgress=TRUE)
    }
    else if ( length(args) == 17 ) {
        performBootstrap(args,reportProgress=TRUE)
    }
    else if ( length(args) == 17 ) {
        reconnectToBootstrapNLMERun(args)
    }
    else {
        print("Usage:bootstrap.r parallel_mechanism shared_directory localWorkingDir engine num_iterations num_samples max_tries model_file column_def_file data_file start_seed extra_args_file files_to_copy NumProc ConfidenceLevel [gridDirectory]")
    }
}

print(main())
 

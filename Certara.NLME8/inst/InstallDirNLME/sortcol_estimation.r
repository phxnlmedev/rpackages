

library("XML")
library("Certara.NLME8")

main<-function(){
    args <- commandArgs(TRUE)
    
    if ( length(args) == 9 ) {
        performEstimationOnSortColumns(args)
    }
    else {
        print("Usage:sortcol_estimation.r parallel_mechanism install_directory shared_directory local_working_dir control_file num_columns column_names num_proc scenarioName")
    }
}

print(main())
 

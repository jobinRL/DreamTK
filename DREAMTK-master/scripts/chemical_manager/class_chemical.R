
# Data structure: Chemical -------------------------------------------

# v0.7

#General parameters we want:

#   chem_info: casn, name, target, cytotoxic concentration, toxicity flag

#
#   assay_info: aeid, name, component name, component endpoints, organism, tissue,
# cell, biological process target, intended target family, ac50, ac cutoff, ac top asymptote
#
#   model_data: list with some data objects for each model keyed by model name
#
#   model_results: list with some data objects for each model results keyed by model name
#
#   analysis_results: list with some data objects for each analysis operation keyed by analysis name
 

Class.Chemical <- R6Class(
  "Class.Chemical",
  
  lock_class = TRUE,
  
  #self variables and functions
  private = list(),
  
  #public variables and functions
  public = list(
    classname = NULL, #we want to use this to id class instead of typeof(object) which returns "environment" for all R6 classes
    
    chem_info = NULL,
    assay_info = NULL,
    model_data = NULL,
    model_results = NULL,
    analysis_results = NULL,


    #constructor
    initialize = function(){
      self$classname = "Class.Chemical";
      
      #not initializing tables to default values saves a whole lot of computation time for long chem lists!
      #self$assay_info = tibble(aeid = 0, assay_name = "", assay_component_name = "", assay_component_endpoint_name ="",
      #                         organism = "", tissue = "", cell_short_name = "", biological_process_target = "", 
      #                         intended_target_family = "", ac50 = 0, ac_top = 0, ac_cutoff = 0);
      self$chem_info = list(casn = "", name = "", cytotoxicity_um = NA, cytotoxic = NA);
      self$assay_info = tibble();
      self$model_data = list();
      self$model_results = list();
      self$analysis_results = list();

    },
    
    #finalizer
    finalize = function() {},

    #calculates minimum Ac50 for a Chemical object, saves analysis results to Chemical object
    calculateMinAc50 = function(){
      if (!is.null(self$assay_info) && length(self$assay_info$aeid) > 0){
        withCallingHandlers( #catch warnings that clutter up the console
          {
			#bug fix here
			#intended_target_family filter Gabriel to check with Andy
            valid_ac50 <- self$assay_info;
            valid_ac50$ac50 <- 10^(valid_ac50$ac50); #ac50 are log10 values, convert to regular value in uM units
            min_ac50 <- min(valid_ac50$ac50, na.rm = TRUE)[[1]]; #grab minimum ac50 values
            
            #find aeid for this value and set it to self
            valid_ac50 <- filter(valid_ac50, ac50 == min_ac50);
            if(length(valid_ac50$aeid)>0){
              min_aeid <- valid_ac50$aeid[[1]];
            } else {
              min_aeid <- NA;
            }
          },
          warning=function(w) { 
            logdebug(str_c("calculateMinAc50() warning:", w));
            invokeRestart("muffleWarning");
          }
        )
      } else {
        min_ac50 <- NA;
        min_aeid <- NA;
      }
      if( !is.finite(min_ac50) ){
        min_ac50 <- NA;
        min_aeid <- NA;
      }
      if(!is.na(min_ac50)){
        min_ac50_units <- "uM";
      } else {
        min_ac50_units <- "";
      }
      
      self$analysis_results$min_ac50 <- list(value = min_ac50, aeid = min_aeid, units = min_ac50_units);
      invisible (min_ac50);
    },
	
    #calculate minimum OED for a Chemical object given a certain name for the OED model,saves analysis results to Chemical object
    calculateMinOED = function (oed_model_name ){
      if (!is.null(self$model_results[[oed_model_name]]) && length(self$model_results[[oed_model_name]]$oed) > 0){
        withCallingHandlers(
          {
		  #thinking Gabriel
            valid_oed <- filter(self$model_results[[oed_model_name]], background != "Y")$oed;
            min_oed <- min(valid_oed, na.rm = TRUE)[[1]]; #grab first minimum oed value 
          },
          warning=function(w) { 
            logdebug(str_c("Chemical list selection warning:", w));
            invokeRestart("muffleWarning");
          }
        )
      } else {
        min_oed <- NA;
      }
      if(!is.finite(min_oed)){
        min_oed <- NA;
      }
      if(!is.na(min_oed)){
        min_oed_units <- self$model_results[[oed_model_name]]$oed_units[[ which(self$model_results[[oed_model_name]]$oed == min_oed) ]];
      } else {
        min_oed_units <- "";
      }
      
      self$analysis_results$min_oed <- list(value = min_oed, units = min_oed_units);
      invisible (min_oed);
    }

  )
  
)

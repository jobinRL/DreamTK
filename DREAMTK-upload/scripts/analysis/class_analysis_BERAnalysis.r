# Basic statistical data  --------------------------------------------


#v0.7

#class contains basic statistical data and data calculating functions
# - minac50, minoed, target family counts, min and avg ac50 per target family, scalar top

#the design goal with this is that this is part of a composition where the basicstats data is all managed by another class and all the plotting is done by this class.
#The reason I am doing this is that it allows for easy expension. Since all you need is to include the data. 
Class.Analysis.BERAnalysis <- R6Class("Class.Analysis.BERAnalysis",
  #private variables and functions
  private = list(
  
	#BER plot variables
	pltBER = NULL,
	tblBER = NULL,
	tblMeanBER = NULL,#
	pltBERvsAC50 = NULL
	
),

#public variables and functions
  public = list(
	BERData =  NULL,
	basicData = NULL,
    #constructor
    initialize = function(  ) {
		self$BERData = Class.Analysis.BERData$new(); #Done here because it will be shared across all instances of the object otherwise.
		self$basicData <- Class.Analysis.Data$new(  ); #Done here because it will be shared across all instances of the object otherwise.
	},
    getBERData = function(){
		return (self$BERData);
	},
    #finalizer
    finalize = function() {
    },
	
	
	
	#Basic Analysis Region
	

    plotBER = function(){
      if (self$BERData$calcBERStatsDataExists()) {
		private$tblMeanBER = tribble(~casn,~name,~average_of_oral_BER, ~min_ac50_over_oral_BER, ~mean_ac50_over_oral_BER);
		#getting our private data
        calc_BER_stat_tbl <- self$BERData$getCalcBERStatsTable();
		chemical_casn_list <- unique(calc_BER_stat_tbl[["casn"]]);
		AC50_stat_tbl <- self$basicData$getBasicStatsTable();
		
		for(chemical_casn in chemical_casn_list){
			casn_tbl <- filter(calc_BER_stat_tbl, casn == chemical_casn);
			ac50_tbl <- filter(AC50_stat_tbl, casn == chemical_casn);
			avg_mean = mean(casn_tbl$direct_ingestion + casn_tbl$direct_vapor + casn_tbl$direct_aerosol);
			oral_ber = (casn_tbl$direct_ingestion + casn_tbl$direct_vapor + casn_tbl$direct_aerosol);
			avg_ac50 = mean(10^ac50_tbl$ac50);
			min_ac50 = min(10^ac50_tbl$ac50);
			
			
			theorical_95th_percentile = quantile(oral_ber,0.95);
			closest_to_95th = absolute.min(oral_ber - theorical_95th_percentile) + theorical_95th_percentile ; #gets the closest value to the 95th percentile.
			
			
			private$tblMeanBER <- add_row(private$tblMeanBER, casn = chemical_casn, 
						name = as.character(filter(calc_BER_stat_tbl, casn == chemical_casn) %>% select(name) %>% distinct(name)),
						average_of_oral_BER = avg_mean,
						min_ac50_over_oral_BER = closest_to_95th / min_ac50,
						mean_ac50_over_oral_BER = closest_to_95th / avg_ac50
						);
				
			
		}

		invisible(private$tblMeanBER);
     }   
    },
	
	plotBERvsAC50 = function(){
      if (self$BERData$calcBERStatsDataExists() && self$basicData$basicStatsDataExists()) {
		BER_data = self$BERData$getCalcBERStatsTable();
		basic_data = self$basicData$getBasicStatsTable();
		
	
				
		
		invisible(private$tblMeanBER);
     }   
    },

	getMeanBERTable = function (){
      return (private$tblMeanBER);
    }
 
 
	)
)